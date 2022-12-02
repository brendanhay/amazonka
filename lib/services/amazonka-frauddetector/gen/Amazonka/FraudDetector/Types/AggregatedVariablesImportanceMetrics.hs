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
-- Module      : Amazonka.FraudDetector.Types.AggregatedVariablesImportanceMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.AggregatedVariablesImportanceMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.AggregatedLogOddsMetric
import qualified Amazonka.Prelude as Prelude

-- | The details of the relative importance of the aggregated variables.
--
-- Account Takeover Insights (ATI) model uses event variables from the
-- login data you provide to continuously calculate a set of variables
-- (aggregated variables) based on historical events. For example, your ATI
-- model might calculate the number of times an user has logged in using
-- the same IP address. In this case, event variables used to derive the
-- aggregated variables are @IP address@ and @user@.
--
-- /See:/ 'newAggregatedVariablesImportanceMetrics' smart constructor.
data AggregatedVariablesImportanceMetrics = AggregatedVariablesImportanceMetrics'
  { -- | List of variables\' metrics.
    logOddsMetrics :: Prelude.Maybe [AggregatedLogOddsMetric]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregatedVariablesImportanceMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logOddsMetrics', 'aggregatedVariablesImportanceMetrics_logOddsMetrics' - List of variables\' metrics.
newAggregatedVariablesImportanceMetrics ::
  AggregatedVariablesImportanceMetrics
newAggregatedVariablesImportanceMetrics =
  AggregatedVariablesImportanceMetrics'
    { logOddsMetrics =
        Prelude.Nothing
    }

-- | List of variables\' metrics.
aggregatedVariablesImportanceMetrics_logOddsMetrics :: Lens.Lens' AggregatedVariablesImportanceMetrics (Prelude.Maybe [AggregatedLogOddsMetric])
aggregatedVariablesImportanceMetrics_logOddsMetrics = Lens.lens (\AggregatedVariablesImportanceMetrics' {logOddsMetrics} -> logOddsMetrics) (\s@AggregatedVariablesImportanceMetrics' {} a -> s {logOddsMetrics = a} :: AggregatedVariablesImportanceMetrics) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AggregatedVariablesImportanceMetrics
  where
  parseJSON =
    Data.withObject
      "AggregatedVariablesImportanceMetrics"
      ( \x ->
          AggregatedVariablesImportanceMetrics'
            Prelude.<$> ( x Data..:? "logOddsMetrics"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AggregatedVariablesImportanceMetrics
  where
  hashWithSalt
    _salt
    AggregatedVariablesImportanceMetrics' {..} =
      _salt `Prelude.hashWithSalt` logOddsMetrics

instance
  Prelude.NFData
    AggregatedVariablesImportanceMetrics
  where
  rnf AggregatedVariablesImportanceMetrics' {..} =
    Prelude.rnf logOddsMetrics
