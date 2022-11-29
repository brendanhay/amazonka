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
-- Module      : Amazonka.IoTAnalytics.Types.DatasetContentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetContentStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTAnalytics.Types.DatasetContentState
import qualified Amazonka.Prelude as Prelude

-- | The state of the dataset contents and the reason they are in this state.
--
-- /See:/ 'newDatasetContentStatus' smart constructor.
data DatasetContentStatus = DatasetContentStatus'
  { -- | The state of the dataset contents. Can be one of READY, CREATING,
    -- SUCCEEDED, or FAILED.
    state :: Prelude.Maybe DatasetContentState,
    -- | The reason the dataset contents are in this state.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetContentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'datasetContentStatus_state' - The state of the dataset contents. Can be one of READY, CREATING,
-- SUCCEEDED, or FAILED.
--
-- 'reason', 'datasetContentStatus_reason' - The reason the dataset contents are in this state.
newDatasetContentStatus ::
  DatasetContentStatus
newDatasetContentStatus =
  DatasetContentStatus'
    { state = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The state of the dataset contents. Can be one of READY, CREATING,
-- SUCCEEDED, or FAILED.
datasetContentStatus_state :: Lens.Lens' DatasetContentStatus (Prelude.Maybe DatasetContentState)
datasetContentStatus_state = Lens.lens (\DatasetContentStatus' {state} -> state) (\s@DatasetContentStatus' {} a -> s {state = a} :: DatasetContentStatus)

-- | The reason the dataset contents are in this state.
datasetContentStatus_reason :: Lens.Lens' DatasetContentStatus (Prelude.Maybe Prelude.Text)
datasetContentStatus_reason = Lens.lens (\DatasetContentStatus' {reason} -> reason) (\s@DatasetContentStatus' {} a -> s {reason = a} :: DatasetContentStatus)

instance Core.FromJSON DatasetContentStatus where
  parseJSON =
    Core.withObject
      "DatasetContentStatus"
      ( \x ->
          DatasetContentStatus'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "reason")
      )

instance Prelude.Hashable DatasetContentStatus where
  hashWithSalt _salt DatasetContentStatus' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` reason

instance Prelude.NFData DatasetContentStatus where
  rnf DatasetContentStatus' {..} =
    Prelude.rnf state `Prelude.seq` Prelude.rnf reason
