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
-- Module      : Amazonka.MigrationHubStrategy.Types.AnalyzerNameUnion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AnalyzerNameUnion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.BinaryAnalyzerName
import Amazonka.MigrationHubStrategy.Types.RunTimeAnalyzerName
import Amazonka.MigrationHubStrategy.Types.SourceCodeAnalyzerName
import qualified Amazonka.Prelude as Prelude

-- | The combination of the existing analyzers.
--
-- /See:/ 'newAnalyzerNameUnion' smart constructor.
data AnalyzerNameUnion = AnalyzerNameUnion'
  { -- | The binary analyzer names.
    binaryAnalyzerName :: Prelude.Maybe BinaryAnalyzerName,
    -- | The assessment analyzer names.
    runTimeAnalyzerName :: Prelude.Maybe RunTimeAnalyzerName,
    -- | The source code analyzer names.
    sourceCodeAnalyzerName :: Prelude.Maybe SourceCodeAnalyzerName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzerNameUnion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'binaryAnalyzerName', 'analyzerNameUnion_binaryAnalyzerName' - The binary analyzer names.
--
-- 'runTimeAnalyzerName', 'analyzerNameUnion_runTimeAnalyzerName' - The assessment analyzer names.
--
-- 'sourceCodeAnalyzerName', 'analyzerNameUnion_sourceCodeAnalyzerName' - The source code analyzer names.
newAnalyzerNameUnion ::
  AnalyzerNameUnion
newAnalyzerNameUnion =
  AnalyzerNameUnion'
    { binaryAnalyzerName =
        Prelude.Nothing,
      runTimeAnalyzerName = Prelude.Nothing,
      sourceCodeAnalyzerName = Prelude.Nothing
    }

-- | The binary analyzer names.
analyzerNameUnion_binaryAnalyzerName :: Lens.Lens' AnalyzerNameUnion (Prelude.Maybe BinaryAnalyzerName)
analyzerNameUnion_binaryAnalyzerName = Lens.lens (\AnalyzerNameUnion' {binaryAnalyzerName} -> binaryAnalyzerName) (\s@AnalyzerNameUnion' {} a -> s {binaryAnalyzerName = a} :: AnalyzerNameUnion)

-- | The assessment analyzer names.
analyzerNameUnion_runTimeAnalyzerName :: Lens.Lens' AnalyzerNameUnion (Prelude.Maybe RunTimeAnalyzerName)
analyzerNameUnion_runTimeAnalyzerName = Lens.lens (\AnalyzerNameUnion' {runTimeAnalyzerName} -> runTimeAnalyzerName) (\s@AnalyzerNameUnion' {} a -> s {runTimeAnalyzerName = a} :: AnalyzerNameUnion)

-- | The source code analyzer names.
analyzerNameUnion_sourceCodeAnalyzerName :: Lens.Lens' AnalyzerNameUnion (Prelude.Maybe SourceCodeAnalyzerName)
analyzerNameUnion_sourceCodeAnalyzerName = Lens.lens (\AnalyzerNameUnion' {sourceCodeAnalyzerName} -> sourceCodeAnalyzerName) (\s@AnalyzerNameUnion' {} a -> s {sourceCodeAnalyzerName = a} :: AnalyzerNameUnion)

instance Data.FromJSON AnalyzerNameUnion where
  parseJSON =
    Data.withObject
      "AnalyzerNameUnion"
      ( \x ->
          AnalyzerNameUnion'
            Prelude.<$> (x Data..:? "binaryAnalyzerName")
            Prelude.<*> (x Data..:? "runTimeAnalyzerName")
            Prelude.<*> (x Data..:? "sourceCodeAnalyzerName")
      )

instance Prelude.Hashable AnalyzerNameUnion where
  hashWithSalt _salt AnalyzerNameUnion' {..} =
    _salt
      `Prelude.hashWithSalt` binaryAnalyzerName
      `Prelude.hashWithSalt` runTimeAnalyzerName
      `Prelude.hashWithSalt` sourceCodeAnalyzerName

instance Prelude.NFData AnalyzerNameUnion where
  rnf AnalyzerNameUnion' {..} =
    Prelude.rnf binaryAnalyzerName
      `Prelude.seq` Prelude.rnf runTimeAnalyzerName
      `Prelude.seq` Prelude.rnf sourceCodeAnalyzerName
