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
-- Module      : Amazonka.EC2.Types.CancelSpotFleetRequestsSuccessItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CancelSpotFleetRequestsSuccessItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.BatchState
import qualified Amazonka.Prelude as Prelude

-- | Describes a Spot Fleet request that was successfully canceled.
--
-- /See:/ 'newCancelSpotFleetRequestsSuccessItem' smart constructor.
data CancelSpotFleetRequestsSuccessItem = CancelSpotFleetRequestsSuccessItem'
  { -- | The current state of the Spot Fleet request.
    currentSpotFleetRequestState :: Prelude.Maybe BatchState,
    -- | The previous state of the Spot Fleet request.
    previousSpotFleetRequestState :: Prelude.Maybe BatchState,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSpotFleetRequestsSuccessItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentSpotFleetRequestState', 'cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState' - The current state of the Spot Fleet request.
--
-- 'previousSpotFleetRequestState', 'cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState' - The previous state of the Spot Fleet request.
--
-- 'spotFleetRequestId', 'cancelSpotFleetRequestsSuccessItem_spotFleetRequestId' - The ID of the Spot Fleet request.
newCancelSpotFleetRequestsSuccessItem ::
  CancelSpotFleetRequestsSuccessItem
newCancelSpotFleetRequestsSuccessItem =
  CancelSpotFleetRequestsSuccessItem'
    { currentSpotFleetRequestState =
        Prelude.Nothing,
      previousSpotFleetRequestState =
        Prelude.Nothing,
      spotFleetRequestId = Prelude.Nothing
    }

-- | The current state of the Spot Fleet request.
cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Prelude.Maybe BatchState)
cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState = Lens.lens (\CancelSpotFleetRequestsSuccessItem' {currentSpotFleetRequestState} -> currentSpotFleetRequestState) (\s@CancelSpotFleetRequestsSuccessItem' {} a -> s {currentSpotFleetRequestState = a} :: CancelSpotFleetRequestsSuccessItem)

-- | The previous state of the Spot Fleet request.
cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Prelude.Maybe BatchState)
cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState = Lens.lens (\CancelSpotFleetRequestsSuccessItem' {previousSpotFleetRequestState} -> previousSpotFleetRequestState) (\s@CancelSpotFleetRequestsSuccessItem' {} a -> s {previousSpotFleetRequestState = a} :: CancelSpotFleetRequestsSuccessItem)

-- | The ID of the Spot Fleet request.
cancelSpotFleetRequestsSuccessItem_spotFleetRequestId :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Prelude.Maybe Prelude.Text)
cancelSpotFleetRequestsSuccessItem_spotFleetRequestId = Lens.lens (\CancelSpotFleetRequestsSuccessItem' {spotFleetRequestId} -> spotFleetRequestId) (\s@CancelSpotFleetRequestsSuccessItem' {} a -> s {spotFleetRequestId = a} :: CancelSpotFleetRequestsSuccessItem)

instance
  Data.FromXML
    CancelSpotFleetRequestsSuccessItem
  where
  parseXML x =
    CancelSpotFleetRequestsSuccessItem'
      Prelude.<$> (x Data..@? "currentSpotFleetRequestState")
      Prelude.<*> (x Data..@? "previousSpotFleetRequestState")
      Prelude.<*> (x Data..@? "spotFleetRequestId")

instance
  Prelude.Hashable
    CancelSpotFleetRequestsSuccessItem
  where
  hashWithSalt
    _salt
    CancelSpotFleetRequestsSuccessItem' {..} =
      _salt
        `Prelude.hashWithSalt` currentSpotFleetRequestState
        `Prelude.hashWithSalt` previousSpotFleetRequestState
        `Prelude.hashWithSalt` spotFleetRequestId

instance
  Prelude.NFData
    CancelSpotFleetRequestsSuccessItem
  where
  rnf CancelSpotFleetRequestsSuccessItem' {..} =
    Prelude.rnf currentSpotFleetRequestState `Prelude.seq`
      Prelude.rnf previousSpotFleetRequestState `Prelude.seq`
        Prelude.rnf spotFleetRequestId
