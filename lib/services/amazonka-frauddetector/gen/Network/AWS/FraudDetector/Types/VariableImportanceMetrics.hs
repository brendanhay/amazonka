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
-- Module      : Network.AWS.FraudDetector.Types.VariableImportanceMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FraudDetector.Types.VariableImportanceMetrics where

import qualified Network.AWS.Core as Core
import Network.AWS.FraudDetector.Types.LogOddsMetric
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON VariableImportanceMetrics where
  parseJSON =
    Core.withObject
      "VariableImportanceMetrics"
      ( \x ->
          VariableImportanceMetrics'
            Prelude.<$> ( x Core..:? "logOddsMetrics"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VariableImportanceMetrics

instance Prelude.NFData VariableImportanceMetrics
