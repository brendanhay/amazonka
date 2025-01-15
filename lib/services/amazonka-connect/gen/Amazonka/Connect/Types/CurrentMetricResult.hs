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
-- Module      : Amazonka.Connect.Types.CurrentMetricResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.CurrentMetricResult where

import Amazonka.Connect.Types.CurrentMetricData
import Amazonka.Connect.Types.Dimensions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a set of real-time metrics.
--
-- /See:/ 'newCurrentMetricResult' smart constructor.
data CurrentMetricResult = CurrentMetricResult'
  { -- | The set of metrics.
    collections :: Prelude.Maybe [CurrentMetricData],
    -- | The dimensions for the metrics.
    dimensions :: Prelude.Maybe Dimensions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CurrentMetricResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collections', 'currentMetricResult_collections' - The set of metrics.
--
-- 'dimensions', 'currentMetricResult_dimensions' - The dimensions for the metrics.
newCurrentMetricResult ::
  CurrentMetricResult
newCurrentMetricResult =
  CurrentMetricResult'
    { collections = Prelude.Nothing,
      dimensions = Prelude.Nothing
    }

-- | The set of metrics.
currentMetricResult_collections :: Lens.Lens' CurrentMetricResult (Prelude.Maybe [CurrentMetricData])
currentMetricResult_collections = Lens.lens (\CurrentMetricResult' {collections} -> collections) (\s@CurrentMetricResult' {} a -> s {collections = a} :: CurrentMetricResult) Prelude.. Lens.mapping Lens.coerced

-- | The dimensions for the metrics.
currentMetricResult_dimensions :: Lens.Lens' CurrentMetricResult (Prelude.Maybe Dimensions)
currentMetricResult_dimensions = Lens.lens (\CurrentMetricResult' {dimensions} -> dimensions) (\s@CurrentMetricResult' {} a -> s {dimensions = a} :: CurrentMetricResult)

instance Data.FromJSON CurrentMetricResult where
  parseJSON =
    Data.withObject
      "CurrentMetricResult"
      ( \x ->
          CurrentMetricResult'
            Prelude.<$> (x Data..:? "Collections" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Dimensions")
      )

instance Prelude.Hashable CurrentMetricResult where
  hashWithSalt _salt CurrentMetricResult' {..} =
    _salt
      `Prelude.hashWithSalt` collections
      `Prelude.hashWithSalt` dimensions

instance Prelude.NFData CurrentMetricResult where
  rnf CurrentMetricResult' {..} =
    Prelude.rnf collections `Prelude.seq`
      Prelude.rnf dimensions
