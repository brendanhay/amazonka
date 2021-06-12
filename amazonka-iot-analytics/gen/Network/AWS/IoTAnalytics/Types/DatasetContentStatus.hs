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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.DatasetContentState
import qualified Network.AWS.Lens as Lens

-- | The state of the data set contents and the reason they are in this
-- state.
--
-- /See:/ 'newDatasetContentStatus' smart constructor.
data DatasetContentStatus = DatasetContentStatus'
  { -- | The state of the data set contents. Can be one of READY, CREATING,
    -- SUCCEEDED, or FAILED.
    state :: Core.Maybe DatasetContentState,
    -- | The reason the data set contents are in this state.
    reason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DatasetContentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'datasetContentStatus_state' - The state of the data set contents. Can be one of READY, CREATING,
-- SUCCEEDED, or FAILED.
--
-- 'reason', 'datasetContentStatus_reason' - The reason the data set contents are in this state.
newDatasetContentStatus ::
  DatasetContentStatus
newDatasetContentStatus =
  DatasetContentStatus'
    { state = Core.Nothing,
      reason = Core.Nothing
    }

-- | The state of the data set contents. Can be one of READY, CREATING,
-- SUCCEEDED, or FAILED.
datasetContentStatus_state :: Lens.Lens' DatasetContentStatus (Core.Maybe DatasetContentState)
datasetContentStatus_state = Lens.lens (\DatasetContentStatus' {state} -> state) (\s@DatasetContentStatus' {} a -> s {state = a} :: DatasetContentStatus)

-- | The reason the data set contents are in this state.
datasetContentStatus_reason :: Lens.Lens' DatasetContentStatus (Core.Maybe Core.Text)
datasetContentStatus_reason = Lens.lens (\DatasetContentStatus' {reason} -> reason) (\s@DatasetContentStatus' {} a -> s {reason = a} :: DatasetContentStatus)

instance Core.FromJSON DatasetContentStatus where
  parseJSON =
    Core.withObject
      "DatasetContentStatus"
      ( \x ->
          DatasetContentStatus'
            Core.<$> (x Core..:? "state") Core.<*> (x Core..:? "reason")
      )

instance Core.Hashable DatasetContentStatus

instance Core.NFData DatasetContentStatus
