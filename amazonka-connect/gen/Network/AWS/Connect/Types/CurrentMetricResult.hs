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
-- Module      : Network.AWS.Connect.Types.CurrentMetricResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricResult where

import Network.AWS.Connect.Types.CurrentMetricData
import Network.AWS.Connect.Types.Dimensions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a set of real-time metrics.
--
-- /See:/ 'newCurrentMetricResult' smart constructor.
data CurrentMetricResult = CurrentMetricResult'
  { -- | The set of metrics.
    collections :: Prelude.Maybe [CurrentMetricData],
    -- | The dimensions for the metrics.
    dimensions :: Prelude.Maybe Dimensions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
currentMetricResult_collections = Lens.lens (\CurrentMetricResult' {collections} -> collections) (\s@CurrentMetricResult' {} a -> s {collections = a} :: CurrentMetricResult) Prelude.. Lens.mapping Prelude._Coerce

-- | The dimensions for the metrics.
currentMetricResult_dimensions :: Lens.Lens' CurrentMetricResult (Prelude.Maybe Dimensions)
currentMetricResult_dimensions = Lens.lens (\CurrentMetricResult' {dimensions} -> dimensions) (\s@CurrentMetricResult' {} a -> s {dimensions = a} :: CurrentMetricResult)

instance Prelude.FromJSON CurrentMetricResult where
  parseJSON =
    Prelude.withObject
      "CurrentMetricResult"
      ( \x ->
          CurrentMetricResult'
            Prelude.<$> ( x Prelude..:? "Collections"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Dimensions")
      )

instance Prelude.Hashable CurrentMetricResult

instance Prelude.NFData CurrentMetricResult
