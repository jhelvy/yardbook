# Yardbook Project Overview

## What We're Building

**Yet Another R Dataviz Book (YARDBOOK)** - An open-source book using Quarto that converts course content from the "Exploratory Data Analysis" class at GWU into a more accessible, book-formatted resource.

## Purpose

This book provides a book-version of the course content from https://eda.seas.gwu.edu/2025-Fall/schedule.html, making it easier for people to find and follow the material in a cohesive narrative structure rather than individual lecture slides.

## Source Material

- **Course Website**: https://eda.seas.gwu.edu/2025-Fall/schedule.html
- **Course Repository**: https://github.com/emse-eda-gwu/2025-Fall
- All content from the class slides is available in the course repository and serves as source material for the book

## Technical Stack

- **Build Tool**: Quarto
- **Language**: R
- **Main Package**: {ggplot2} for data visualization
- **Data Manipulation**: Tidyverse
- **Output**: Web-based book (HTML)

## Book Structure

The book is organized into three main parts:

### Part 1: Getting Started
- Quarto basics
- ggplot2 introduction

### Part 2: Exploring Data
- Tidy data
- Cleaning data
- Good practices
- Summarizing data (planned)

### Part 3: Data Visualization (planned)
- Effective data visualization
- Visualizing trends
- Visualizing amounts
- Visualizing proportions
- Visualizing comparisons
- Maps
- Scales
- Aesthetics
- Animation
- Interactive visualizations

## Current Status

The book is **in active development**. Currently implemented:
- Part 1 (Getting Started) - Complete
- Part 2 (Exploring Data) - Partially complete (tidy-data, cleaning-data, good-practices done)
- Part 3 (Data Visualization) - Chapters exist but are commented out in `_quarto.yml`

## Repository Information

- **Live Site**: https://yardbook.jhelvy.com/
- **Repository**: https://github.com/jhelvy/yardbook/
- **Authors**: John Paul Helveston & Lola Nurullaeva
- **License**: Open source

## Book Philosophy

From the preface:
> "Throughout the book, we keep the text to a minimum and focus on communicating key concepts as succinctly as possible. The general approach is to explain an idea in one or two sentences max, then provide an example with code so that you can see how to implement it."

The book emphasizes:
- Minimal explanatory text
- Maximum code examples
- Quick, practical implementation guidance
- Clear, concise concept communication

## File Organization

- `*.qmd` files: Individual chapter content
- `_quarto.yml`: Book configuration and chapter ordering
- `_common.R`: Shared R code and setup
- `data/`: Data files used in examples
- `images/`: Image assets
- `figs/`: Generated figures
- `cover.png`: Book cover image
- `styles.css`: Custom styling

## Development Workflow

1. Content is based on lecture materials from the course repository
2. Slides are converted to book chapters in Quarto markdown (`.qmd`)
3. Chapters are added to `_quarto.yml` when ready
4. Book is rendered to `_book/` directory
5. Published to https://yardbook.jhelvy.com/
