# Assignment Problem

A **Mixed Integer Linear Programming (MILP)** model in **R** for the **Assignment Problem**, built with the [`ompr`](https://dirkschumacher.github.io/ompr/) modeling framework and solved via the **SYMPHONY** solver (through `ROI`).

## Overview

The Assignment Problem is a fundamental combinatorial optimization problem in Operations Research. A set of workers must be assigned to a set of tasks — each task performed by exactly one worker and each worker performing at most one task — at **minimum total cost**.

This implementation solves a **rectangular** variant where the number of workers ($n$) exceeds the number of tasks ($m$). Since not all workers can receive a task, the model selects the best subset of workers to assign. When $n = m$ (balanced case), every worker is assigned exactly one task.

The Assignment Problem is a special case of the Transportation Problem and can be solved in polynomial time via the Hungarian algorithm. It is widely applicable to workforce scheduling, resource allocation, and task distribution.

## Repository Contents

| File | Description |
|---|---|
| `Assignment Problem.R` | R script implementing and solving the Assignment Problem instance |
| `Assignment_Problem.pdf` | Mathematical formulation of the problem |

## Mathematical Formulation

### Parameters

- $n$ = number of workers (index $i = 1, \dots, n$)
- $m$ = number of tasks (index $j = 1, \dots, m$)
- $c_{ij}$ = cost of assigning worker $i$ to task $j$; $\forall\, i = 1, \dots, n,\ j = 1, \dots, m$

### Variable

- $x_{ij}$ = binary assignment variable:

$$
x_{ij} = \begin{cases} 1 & \text{if worker } i \text{ is assigned to task } j \\ 0 & \text{otherwise} \end{cases}
$$

### Objective Function

**(1)** — Minimize total assignment cost

$$
\displaystyle \min \sum_{i=1}^{n} \sum_{j=1}^{m} c_{ij} \cdot x_{ij}
$$

### Constraints

**(2)** — Each task is performed by exactly one worker

$$
\displaystyle \sum_{i=1}^{n} x_{ij} = 1 \qquad \forall\, j = 1, \dots, m
$$

**(3)** — Each worker is assigned to at most one task

$$
\displaystyle \sum_{j=1}^{m} x_{ij} \le 1 \qquad \forall\, i = 1, \dots, n
$$

**(4)** — Binary assignment variables

$$
x_{ij} \in \{0, 1\} \qquad \forall\, i = 1, \dots, n,\ j = 1, \dots, m
$$

> **Note on the rectangular case:** Constraint (2) uses $= 1$ (every task must be covered), while constraint (3) uses $\le 1$ (a worker may be left unassigned). This asymmetry handles the case $n > m$: exactly $m$ workers are selected from the $n$ available. In the balanced case $n = m$, constraint (3) automatically becomes an equality and every worker is assigned.

> **Relation to the Transportation Problem:** The Assignment Problem is a special case of the Transportation Problem where all supplies and demands equal 1. Its constraint matrix is totally unimodular, guaranteeing integer optimal solutions from the LP relaxation. Binary variables are declared explicitly for clarity.

A copy of this formulation is also available as a standalone PDF in this repository.

## Example Instance

The script uses a hardcoded instance with **5 workers** and **4 tasks** ($n = 5,\ m = 4$). Since workers outnumber tasks, exactly one worker will be left unassigned in the optimal solution.

**Cost matrix $C$** (rows = workers, columns = tasks):

$$
C = \begin{pmatrix}
90 & 80 & 75 & 70 \\
35 & 85 & 55 & 65 \\
125 & 95 & 90 & 95 \\
45 & 110 & 95 & 115 \\
50 & 100 & 90 & 100
\end{pmatrix}
$$

| | Task 1 | Task 2 | Task 3 | Task 4 |
|---|---:|---:|---:|---:|
| **Worker 1** | 90 | 80 | 75 | 70 |
| **Worker 2** | 35 | 85 | 55 | 65 |
| **Worker 3** | 125 | 95 | 90 | 95 |
| **Worker 4** | 45 | 110 | 95 | 115 |
| **Worker 5** | 50 | 100 | 90 | 100 |

## Requirements

```r
install.packages(c("lpSolve", "dplyr", "ROI", "ROI.plugin.symphony", "ompr", "ompr.roi"))
```

## Usage

1. Clone or download this repository.
2. Open `Assignment Problem.R` in R or RStudio.
3. Update the `setwd()` path at the top of the script to match your local directory.
4. Run the script. It will:
   - Build and solve the MILP model using `ompr` and SYMPHONY
   - Print the optimal total assignment cost (objective value)
   - Print each worker-to-task assignment in the optimal solution with its individual cost

## Output

The script prints:

- **Objective value** — the minimum total assignment cost
- **Assignments** — for each active $x[i, j] = 1$: the worker index, the task index, and the cost of that assignment
