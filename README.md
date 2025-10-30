# 🧮 Type Checking and CEK Machines

## 📘 Archivo principal
**TypeChecking_and_CEK_machines.rkt**

## 🧠 Lenguaje
Racket — dialecto **play** (`#lang play`)

---

## 📖 Descripción general
Este programa implementa un **intérprete con tipado estático** y una **máquina CEK** (Control, Environment, Kontinuation) para un lenguaje funcional de primer orden.  
Integra dos componentes principales:

### 🧩 Sistema de Tipos (Type Checking)
Encargado de inferir el tipo de cada expresión del lenguaje utilizando un ambiente de tipos.

**Tipos soportados:**  
- `Number`  
- `Boolean`  
- Funciones (`->`)

**Operaciones aritméticas y lógicas:** `+`, `-`, `*`, `<=`  
**Estructuras de control:** `if`, `fun`, `app`

---

### ⚙️ Máquina CEK (Evaluación Semántica)
Implementa la **semántica operacional** del lenguaje mediante transiciones de estado entre:

- **Control (Expr):** expresión actual a evaluar  
- **Environment (Env):** ambiente de valores  
- **Kontinuation (Kont):** pila de contexto de ejecución  

Permite modelar la evaluación paso a paso (*step semantics*) hasta obtener el valor final (*final state*).

---

## 🧱 Estructura del código

### 🔹 Definiciones de tipos de datos:
- `Expr` → Expresiones abstractas del lenguaje  
- `Type` → Sistema de tipos estático  
- `Env`, `TypeEnv`, `Kont`, `State` → Estructuras auxiliares de ejecución

### 🔹 Funciones principales:
- `parse-type` → Traduce tipos concretos a tipos abstractos  
- `parse` → Convierte expresiones concretas a sintaxis abstracta  
- `infer-type` → Infere el tipo de una expresión dado un ambiente  
- `eval` → Evalúa una expresión con la máquina CEK hasta su valor final  
- `run` → Ejecuta una expresión concreta y devuelve el valor y su tipo

---

## 🧠 Ejecución
1. Abre **DrRacket** o un entorno compatible con `#lang play`.  
2. Carga el archivo `TypeChecking_and_CEK_machines.rkt`.  
3. Ejecuta pruebas mediante el archivo de tests o comandos interactivos, por ejemplo:

```racket
(run '(+ 1 2))
(run '((fun (x : Number) (fun (y : Number) (+ x y))) 2))

🧪 Archivo de pruebas asociado

test.rkt

Este archivo valida la correcta inferencia de tipos, parseo, semántica de evaluación y funcionamiento de la máquina CEK.

🧱 División del set de tests
Parte I – Sistema de Tipos y Parsing (TypeChecking Tests)

Valida el correcto funcionamiento del parser y del sistema de inferencia de tipos.

Parte II – Máquina CEK y Evaluación (CEK Machine Tests)

Verifica la evaluación paso a paso, continuidad del entorno, funciones y resultados de ejecución final.
