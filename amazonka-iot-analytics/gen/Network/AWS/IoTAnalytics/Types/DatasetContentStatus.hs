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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentStatus where

import Network.AWS.IoTAnalytics.Types.DatasetContentState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The state of the data set contents and the reason they are in this
-- state.
--
-- /See:/ 'newDatasetContentStatus' smart constructor.
data DatasetContentStatus = DatasetContentStatus'
  { -- | The state of the data set contents. Can be one of READY, CREATING,
    -- SUCCEEDED, or FAILED.
    state :: Prelude.Maybe DatasetContentState,
    -- | The reason the data set contents are in this state.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { state = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The state of the data set contents. Can be one of READY, CREATING,
-- SUCCEEDED, or FAILED.
datasetContentStatus_state :: Lens.Lens' DatasetContentStatus (Prelude.Maybe DatasetContentState)
datasetContentStatus_state = Lens.lens (\DatasetContentStatus' {state} -> state) (\s@DatasetContentStatus' {} a -> s {state = a} :: DatasetContentStatus)

-- | The reason the data set contents are in this state.
datasetContentStatus_reason :: Lens.Lens' DatasetContentStatus (Prelude.Maybe Prelude.Text)
datasetContentStatus_reason = Lens.lens (\DatasetContentStatus' {reason} -> reason) (\s@DatasetContentStatus' {} a -> s {reason = a} :: DatasetContentStatus)

instance Prelude.FromJSON DatasetContentStatus where
  parseJSON =
    Prelude.withObject
      "DatasetContentStatus"
      ( \x ->
          DatasetContentStatus'
            Prelude.<$> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "reason")
      )

instance Prelude.Hashable DatasetContentStatus

instance Prelude.NFData DatasetContentStatus
