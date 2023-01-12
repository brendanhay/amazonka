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
-- Module      : Amazonka.FraudDetector.Types.VariableImportanceMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.VariableImportanceMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.LogOddsMetric
import qualified Amazonka.Prelude as Prelude

-- | The variable importance metrics details.
--
-- /See:/ 'newVariableImportanceMetrics' smart constructor.
data VariableImportanceMetrics = VariableImportanceMetrics'
  { -- | List of variable metrics.
    logOddsMetrics :: Prelude.Maybe [LogOddsMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariableImportanceMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logOddsMetrics', 'variableImportanceMetrics_logOddsMetrics' - List of variable metrics.
newVariableImportanceMetrics ::
  VariableImportanceMetrics
newVariableImportanceMetrics =
  VariableImportanceMetrics'
    { logOddsMetrics =
        Prelude.Nothing
    }

-- | List of variable metrics.
variableImportanceMetrics_logOddsMetrics :: Lens.Lens' VariableImportanceMetrics (Prelude.Maybe [LogOddsMetric])
variableImportanceMetrics_logOddsMetrics = Lens.lens (\VariableImportanceMetrics' {logOddsMetrics} -> logOddsMetrics) (\s@VariableImportanceMetrics' {} a -> s {logOddsMetrics = a} :: VariableImportanceMetrics) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON VariableImportanceMetrics where
  parseJSON =
    Data.withObject
      "VariableImportanceMetrics"
      ( \x ->
          VariableImportanceMetrics'
            Prelude.<$> ( x Data..:? "logOddsMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VariableImportanceMetrics where
  hashWithSalt _salt VariableImportanceMetrics' {..} =
    _salt `Prelude.hashWithSalt` logOddsMetrics

instance Prelude.NFData VariableImportanceMetrics where
  rnf VariableImportanceMetrics' {..} =
    Prelude.rnf logOddsMetrics
