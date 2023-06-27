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
-- Module      : Amazonka.MigrationHubStrategy.Types.AnalysisStatusUnion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AnalysisStatusUnion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.RuntimeAnalysisStatus
import Amazonka.MigrationHubStrategy.Types.SrcCodeOrDbAnalysisStatus
import qualified Amazonka.Prelude as Prelude

-- | A combination of existing analysis statuses.
--
-- /See:/ 'newAnalysisStatusUnion' smart constructor.
data AnalysisStatusUnion = AnalysisStatusUnion'
  { -- | The status of the analysis.
    runtimeAnalysisStatus :: Prelude.Maybe RuntimeAnalysisStatus,
    -- | The status of the source code or database analysis.
    srcCodeOrDbAnalysisStatus :: Prelude.Maybe SrcCodeOrDbAnalysisStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisStatusUnion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runtimeAnalysisStatus', 'analysisStatusUnion_runtimeAnalysisStatus' - The status of the analysis.
--
-- 'srcCodeOrDbAnalysisStatus', 'analysisStatusUnion_srcCodeOrDbAnalysisStatus' - The status of the source code or database analysis.
newAnalysisStatusUnion ::
  AnalysisStatusUnion
newAnalysisStatusUnion =
  AnalysisStatusUnion'
    { runtimeAnalysisStatus =
        Prelude.Nothing,
      srcCodeOrDbAnalysisStatus = Prelude.Nothing
    }

-- | The status of the analysis.
analysisStatusUnion_runtimeAnalysisStatus :: Lens.Lens' AnalysisStatusUnion (Prelude.Maybe RuntimeAnalysisStatus)
analysisStatusUnion_runtimeAnalysisStatus = Lens.lens (\AnalysisStatusUnion' {runtimeAnalysisStatus} -> runtimeAnalysisStatus) (\s@AnalysisStatusUnion' {} a -> s {runtimeAnalysisStatus = a} :: AnalysisStatusUnion)

-- | The status of the source code or database analysis.
analysisStatusUnion_srcCodeOrDbAnalysisStatus :: Lens.Lens' AnalysisStatusUnion (Prelude.Maybe SrcCodeOrDbAnalysisStatus)
analysisStatusUnion_srcCodeOrDbAnalysisStatus = Lens.lens (\AnalysisStatusUnion' {srcCodeOrDbAnalysisStatus} -> srcCodeOrDbAnalysisStatus) (\s@AnalysisStatusUnion' {} a -> s {srcCodeOrDbAnalysisStatus = a} :: AnalysisStatusUnion)

instance Data.FromJSON AnalysisStatusUnion where
  parseJSON =
    Data.withObject
      "AnalysisStatusUnion"
      ( \x ->
          AnalysisStatusUnion'
            Prelude.<$> (x Data..:? "runtimeAnalysisStatus")
            Prelude.<*> (x Data..:? "srcCodeOrDbAnalysisStatus")
      )

instance Prelude.Hashable AnalysisStatusUnion where
  hashWithSalt _salt AnalysisStatusUnion' {..} =
    _salt
      `Prelude.hashWithSalt` runtimeAnalysisStatus
      `Prelude.hashWithSalt` srcCodeOrDbAnalysisStatus

instance Prelude.NFData AnalysisStatusUnion where
  rnf AnalysisStatusUnion' {..} =
    Prelude.rnf runtimeAnalysisStatus
      `Prelude.seq` Prelude.rnf srcCodeOrDbAnalysisStatus
