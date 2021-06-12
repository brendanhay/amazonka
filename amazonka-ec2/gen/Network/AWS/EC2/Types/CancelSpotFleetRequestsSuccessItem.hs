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
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BatchState
import qualified Network.AWS.Lens as Lens

-- | Describes a Spot Fleet request that was successfully canceled.
--
-- /See:/ 'newCancelSpotFleetRequestsSuccessItem' smart constructor.
data CancelSpotFleetRequestsSuccessItem = CancelSpotFleetRequestsSuccessItem'
  { -- | The current state of the Spot Fleet request.
    currentSpotFleetRequestState :: Core.Maybe BatchState,
    -- | The previous state of the Spot Fleet request.
    previousSpotFleetRequestState :: Core.Maybe BatchState,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      previousSpotFleetRequestState =
        Core.Nothing,
      spotFleetRequestId = Core.Nothing
    }

-- | The current state of the Spot Fleet request.
cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Core.Maybe BatchState)
cancelSpotFleetRequestsSuccessItem_currentSpotFleetRequestState = Lens.lens (\CancelSpotFleetRequestsSuccessItem' {currentSpotFleetRequestState} -> currentSpotFleetRequestState) (\s@CancelSpotFleetRequestsSuccessItem' {} a -> s {currentSpotFleetRequestState = a} :: CancelSpotFleetRequestsSuccessItem)

-- | The previous state of the Spot Fleet request.
cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Core.Maybe BatchState)
cancelSpotFleetRequestsSuccessItem_previousSpotFleetRequestState = Lens.lens (\CancelSpotFleetRequestsSuccessItem' {previousSpotFleetRequestState} -> previousSpotFleetRequestState) (\s@CancelSpotFleetRequestsSuccessItem' {} a -> s {previousSpotFleetRequestState = a} :: CancelSpotFleetRequestsSuccessItem)

-- | The ID of the Spot Fleet request.
cancelSpotFleetRequestsSuccessItem_spotFleetRequestId :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Core.Maybe Core.Text)
cancelSpotFleetRequestsSuccessItem_spotFleetRequestId = Lens.lens (\CancelSpotFleetRequestsSuccessItem' {spotFleetRequestId} -> spotFleetRequestId) (\s@CancelSpotFleetRequestsSuccessItem' {} a -> s {spotFleetRequestId = a} :: CancelSpotFleetRequestsSuccessItem)

instance
  Core.FromXML
    CancelSpotFleetRequestsSuccessItem
  where
  parseXML x =
    CancelSpotFleetRequestsSuccessItem'
      Core.<$> (x Core..@? "currentSpotFleetRequestState")
      Core.<*> (x Core..@? "previousSpotFleetRequestState")
      Core.<*> (x Core..@? "spotFleetRequestId")

instance
  Core.Hashable
    CancelSpotFleetRequestsSuccessItem

instance
  Core.NFData
    CancelSpotFleetRequestsSuccessItem
