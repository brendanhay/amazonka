{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.CodeCoverage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.CodeCoverage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains code coverage report information.
--
-- Line coverage measures how many statements your tests cover. A statement
-- is a single instruction, not including comments, conditionals, etc.
--
-- Branch coverage determines if your tests cover every possible branch of
-- a control structure, such as an @if@ or @case@ statement.
--
-- /See:/ 'newCodeCoverage' smart constructor.
data CodeCoverage = CodeCoverage'
  { -- | The number of conditional branches that are not covered by your tests.
    branchesMissed :: Prelude.Maybe Prelude.Natural,
    -- | The number of lines that are covered by your tests.
    linesCovered :: Prelude.Maybe Prelude.Natural,
    -- | The number of conditional branches that are covered by your tests.
    branchesCovered :: Prelude.Maybe Prelude.Natural,
    -- | The path of the test report file.
    filePath :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the report.
    reportARN :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the code coverage report.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the tests were run.
    expired :: Prelude.Maybe Prelude.POSIX,
    -- | The percentage of lines that are covered by your tests.
    lineCoveragePercentage :: Prelude.Maybe Prelude.Double,
    -- | The number of lines that are not covered by your tests.
    linesMissed :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of branches that are covered by your tests.
    branchCoveragePercentage :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CodeCoverage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchesMissed', 'codeCoverage_branchesMissed' - The number of conditional branches that are not covered by your tests.
--
-- 'linesCovered', 'codeCoverage_linesCovered' - The number of lines that are covered by your tests.
--
-- 'branchesCovered', 'codeCoverage_branchesCovered' - The number of conditional branches that are covered by your tests.
--
-- 'filePath', 'codeCoverage_filePath' - The path of the test report file.
--
-- 'reportARN', 'codeCoverage_reportARN' - The ARN of the report.
--
-- 'id', 'codeCoverage_id' - The identifier of the code coverage report.
--
-- 'expired', 'codeCoverage_expired' - The date and time that the tests were run.
--
-- 'lineCoveragePercentage', 'codeCoverage_lineCoveragePercentage' - The percentage of lines that are covered by your tests.
--
-- 'linesMissed', 'codeCoverage_linesMissed' - The number of lines that are not covered by your tests.
--
-- 'branchCoveragePercentage', 'codeCoverage_branchCoveragePercentage' - The percentage of branches that are covered by your tests.
newCodeCoverage ::
  CodeCoverage
newCodeCoverage =
  CodeCoverage'
    { branchesMissed = Prelude.Nothing,
      linesCovered = Prelude.Nothing,
      branchesCovered = Prelude.Nothing,
      filePath = Prelude.Nothing,
      reportARN = Prelude.Nothing,
      id = Prelude.Nothing,
      expired = Prelude.Nothing,
      lineCoveragePercentage = Prelude.Nothing,
      linesMissed = Prelude.Nothing,
      branchCoveragePercentage = Prelude.Nothing
    }

-- | The number of conditional branches that are not covered by your tests.
codeCoverage_branchesMissed :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.Natural)
codeCoverage_branchesMissed = Lens.lens (\CodeCoverage' {branchesMissed} -> branchesMissed) (\s@CodeCoverage' {} a -> s {branchesMissed = a} :: CodeCoverage)

-- | The number of lines that are covered by your tests.
codeCoverage_linesCovered :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.Natural)
codeCoverage_linesCovered = Lens.lens (\CodeCoverage' {linesCovered} -> linesCovered) (\s@CodeCoverage' {} a -> s {linesCovered = a} :: CodeCoverage)

-- | The number of conditional branches that are covered by your tests.
codeCoverage_branchesCovered :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.Natural)
codeCoverage_branchesCovered = Lens.lens (\CodeCoverage' {branchesCovered} -> branchesCovered) (\s@CodeCoverage' {} a -> s {branchesCovered = a} :: CodeCoverage)

-- | The path of the test report file.
codeCoverage_filePath :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.Text)
codeCoverage_filePath = Lens.lens (\CodeCoverage' {filePath} -> filePath) (\s@CodeCoverage' {} a -> s {filePath = a} :: CodeCoverage)

-- | The ARN of the report.
codeCoverage_reportARN :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.Text)
codeCoverage_reportARN = Lens.lens (\CodeCoverage' {reportARN} -> reportARN) (\s@CodeCoverage' {} a -> s {reportARN = a} :: CodeCoverage)

-- | The identifier of the code coverage report.
codeCoverage_id :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.Text)
codeCoverage_id = Lens.lens (\CodeCoverage' {id} -> id) (\s@CodeCoverage' {} a -> s {id = a} :: CodeCoverage)

-- | The date and time that the tests were run.
codeCoverage_expired :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.UTCTime)
codeCoverage_expired = Lens.lens (\CodeCoverage' {expired} -> expired) (\s@CodeCoverage' {} a -> s {expired = a} :: CodeCoverage) Prelude.. Lens.mapping Prelude._Time

-- | The percentage of lines that are covered by your tests.
codeCoverage_lineCoveragePercentage :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.Double)
codeCoverage_lineCoveragePercentage = Lens.lens (\CodeCoverage' {lineCoveragePercentage} -> lineCoveragePercentage) (\s@CodeCoverage' {} a -> s {lineCoveragePercentage = a} :: CodeCoverage)

-- | The number of lines that are not covered by your tests.
codeCoverage_linesMissed :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.Natural)
codeCoverage_linesMissed = Lens.lens (\CodeCoverage' {linesMissed} -> linesMissed) (\s@CodeCoverage' {} a -> s {linesMissed = a} :: CodeCoverage)

-- | The percentage of branches that are covered by your tests.
codeCoverage_branchCoveragePercentage :: Lens.Lens' CodeCoverage (Prelude.Maybe Prelude.Double)
codeCoverage_branchCoveragePercentage = Lens.lens (\CodeCoverage' {branchCoveragePercentage} -> branchCoveragePercentage) (\s@CodeCoverage' {} a -> s {branchCoveragePercentage = a} :: CodeCoverage)

instance Prelude.FromJSON CodeCoverage where
  parseJSON =
    Prelude.withObject
      "CodeCoverage"
      ( \x ->
          CodeCoverage'
            Prelude.<$> (x Prelude..:? "branchesMissed")
            Prelude.<*> (x Prelude..:? "linesCovered")
            Prelude.<*> (x Prelude..:? "branchesCovered")
            Prelude.<*> (x Prelude..:? "filePath")
            Prelude.<*> (x Prelude..:? "reportARN")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "expired")
            Prelude.<*> (x Prelude..:? "lineCoveragePercentage")
            Prelude.<*> (x Prelude..:? "linesMissed")
            Prelude.<*> (x Prelude..:? "branchCoveragePercentage")
      )

instance Prelude.Hashable CodeCoverage

instance Prelude.NFData CodeCoverage
