#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <array>
#include <bitset>
#include <ctime>
#include <random>
#include <cstdlib>

using namespace std;

int maxWeight = 0;

void getWeightsFromInput(string input, vector<short> & weights) {
    short weight;
    input.erase(0, 1);
    stringstream s(input);
    while (s >> weight) {
        if (0 != weight) {
            weights.push_back(weight);
        }
    }
}

vector<short> getFormulaFromInputLine(string input, int maxVariables) {
    istringstream s(input);
    string tmp;
    short index;
    vector<short> formula;
    for (int i = 0; i < maxVariables; i++) {
        formula.push_back(0);
    }
    while (s >> index) {
        if (index == 0) {
            break;
        }
        
        short absIndex = index < 0 ? -index : index;
        formula[absIndex-1] = index < 0 ? -1 : 1;
    }

    return formula;
}

void printFormulas(vector<vector<short>> & formulas) {
    for (auto formula: formulas) {
        for (auto variable: formula) {
            cout << variable << " ";
        }
        cout << endl;
    }
}

void increaseConfiguration(vector<short> & configuration) {
    for (auto & it : configuration) {
        if (it == 0) {
            it = 1;
            break;
        }

        if (it == 1) {
            it = 0;
        }
    }
}

bool isConfigurationMax(vector<short> & configuration) {
    for (auto it : configuration) {
        if (it == 0) {
            return false;
        }
    }

    return true;
}

void printConfiguration(vector<short> & configuration) {
    for (size_t i = 0; i < configuration.size(); i++) {
        cout << (configuration[i] == 0 ? "-" : "") << i+1 << " ";
    }
}

void printVector(vector<short> & vec) {
    for (auto it : vec) {
        cout << it << " ";
    }
    cout << endl;
}

void findSolutionBruteForce(vector<short> & configuration, vector<vector<short>> formulas, vector<short> weights) {
    bool breakLoop = false;
    int max = 0;
    while (true) {
        bool isSolution = false;
        for (auto formula : formulas) {
            isSolution = false;

            for (size_t i = 0; i < formula.size(); i++) {
                if (formula[i] == 0) continue;

                if ((formula[i] == 1 && configuration[i] == 1) || (formula[i] == -1 && configuration[i] == 0)) {
                    isSolution = true;
                    break;
                }
            }

            if (!isSolution) {
                increaseConfiguration(configuration);
                break;
            }
        }
        
        if (isSolution) {
            int sum = 0;
            for (size_t i = 0; i < configuration.size(); i++) {
                sum += configuration[i] * weights[i];
            }

            if (sum > max) {
                max = sum;
                cout << sum << " ";
                printConfiguration(configuration);
                cout << endl;
            }

            increaseConfiguration(configuration);
            continue;
        }

        if (breakLoop) {
            break;
        }

        if (isConfigurationMax(configuration)) {
            breakLoop = true;
        }
    }

}

void setRandomConfiguration(vector<short> & configuration) {
    for (size_t i = 0; i < configuration.size(); i++) {
        configuration[i] = rand() % 2;
    }
}

int calculateWeightSum(vector<short> & configuration, vector<short> & weights) {
    int sum = 0;
    for (size_t i = 0; i < configuration.size(); i++) {
        sum += configuration[i] * weights[i];
    }

    return sum;
}

int calculateMaxWeightSum(vector<short> & weights) {
    if (maxWeight > 0) return maxWeight;

    int sum = 0;
    for (size_t i = 0; i < weights.size(); i++) {
        sum += weights[i];
    }

    maxWeight = sum;

    return sum;
}

double calculateFitness(vector<short> & configuration, vector<vector<short>> & formulas, vector<short> & weights, double COST_COEFFICIENT) {
    size_t satisfiedClauses = 0;

    for (auto formula : formulas) {
        bool isSolution = false;
        for (size_t i = 0; i < formula.size(); i++) {
            if (formula[i] == 0) continue;

            if ((formula[i] == 1 && configuration[i] == 1) || (formula[i] == -1 && configuration[i] == 0)) {
                isSolution = true;
                break;
            }
        }
        if (isSolution) {
            satisfiedClauses++;
        }
    }

    int weightSum = calculateWeightSum(configuration, weights);

    return (weightSum * COST_COEFFICIENT) + satisfiedClauses;
    
}

void findSolution(
    vector<short> & configuration, 
    vector<vector<short>> & formulas, 
    vector<short> & weights, 
    double randomDouble,
    double START_TEMPERATURE,
    double COOLING_SPEED,
    double FROZEN_TEMPERATURE,
    int EQUILIBRIUM,
    double COST_COEFFICIENT
) {
    double temperature = START_TEMPERATURE;

    setRandomConfiguration(configuration);

    vector<short> bestConfiguration(configuration);

    while (temperature > FROZEN_TEMPERATURE) {
        for (int e = 0; e < EQUILIBRIUM; e++) {
            double currentCost = calculateFitness(configuration, formulas, weights, COST_COEFFICIENT);
            double bestCost = calculateFitness(bestConfiguration, formulas, weights, COST_COEFFICIENT);

            if (currentCost > bestCost) {
                bestConfiguration = configuration;
            }

            //cout << currentCost << endl;
            
            vector<short> newConfiguration(configuration);
            int idx = rand() % newConfiguration.size();
            newConfiguration[idx] = newConfiguration[idx] == 0 ? 1 : 0;
            //setRandomConfiguration(newConfiguration);
            double newCost = calculateFitness(newConfiguration, formulas, weights, COST_COEFFICIENT);

            if (newCost > currentCost) {
                configuration = newConfiguration;
                continue;
            }

            double delta = currentCost - newCost;

            if (randomDouble < exp((delta * -1) / temperature)) {
                configuration = newConfiguration;
            }
        }

        temperature = temperature * COOLING_SPEED;
    }

    cout << calculateWeightSum(bestConfiguration, weights) << " ";
    printConfiguration(bestConfiguration);
    cout << 0 << endl;
}

int main(int argc, char** argv)
{

    if (argc != 6) {
        cout << "[START_TEMPERATURE] [COOLING_SPEED] [FROZEN_TEMPERATURE] [EQUILIBRIUM] [COST_COEFFICIENT]" << endl;
        exit(1);
    }

    std::srand(std::time(nullptr));    
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> rand_prob(0, 1.0);

    int maxVariables, maxFormulas;
    vector<
        vector<short> // terms
    > formulas;
    vector<short> weights;

    int formulaIdx = 0;
    string tmp;
    while (getline(cin, tmp)) {
        if (tmp[0] == 'c') {
            continue;
        }

        if (tmp[0] == 'p') {
            tmp.erase(0, 1);
            istringstream s(tmp);
            string trash;
            s >> trash; // throw out "mwcnf"

            s >> maxVariables;
            s >> maxFormulas;

            for (int i = 0; i < maxFormulas; i++) {
                formulas.push_back(vector<short>(maxVariables));
            }
            continue;
        }

        if (tmp[0] == 'w') {
            getWeightsFromInput(tmp, weights);
            continue;
        }

        formulas[formulaIdx++] = getFormulaFromInputLine(tmp, maxVariables);
    }

    vector<short> configuration;
    configuration.reserve(maxVariables);
    for (int i = 0; i < maxVariables; i++) {
        configuration.push_back(0);
    }

    double START_TEMPERATURE = stod(argv[1]) * maxVariables;
    double COOLING_SPEED = stod(argv[2]);
    double FROZEN_TEMPERATURE = stod(argv[3]);
    int START_EQUILIBRIUM = stoi(argv[4]);
    double COST_COEFFICIENT = stod(argv[5]);

    findSolution(configuration, formulas, weights, rand_prob(gen), START_TEMPERATURE, COOLING_SPEED, FROZEN_TEMPERATURE, START_EQUILIBRIUM, COST_COEFFICIENT);
    //findSolutionBruteForce(configuration, formulas, weights);
}
