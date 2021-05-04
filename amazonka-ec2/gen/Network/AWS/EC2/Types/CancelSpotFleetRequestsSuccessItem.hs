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
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.BatchState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromXML
    CancelSpotFleetRequestsSuccessItem
  where
  parseXML x =
    CancelSpotFleetRequestsSuccessItem'
      Prelude.<$> (x Prelude..@? "currentSpotFleetRequestState")
      Prelude.<*> (x Prelude..@? "previousSpotFleetRequestState")
      Prelude.<*> (x Prelude..@? "spotFleetRequestId")

instance
  Prelude.Hashable
    CancelSpotFleetRequestsSuccessItem

instance
  Prelude.NFData
    CancelSpotFleetRequestsSuccessItem
