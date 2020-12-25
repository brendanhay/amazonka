{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetSuccessItem
  ( DeleteFleetSuccessItem (..),

    -- * Smart constructor
    mkDeleteFleetSuccessItem,

    -- * Lenses
    dfsiCurrentFleetState,
    dfsiFleetId,
    dfsiPreviousFleetState,
  )
where

import qualified Network.AWS.EC2.Types.FleetId as Types
import qualified Network.AWS.EC2.Types.FleetStateCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an EC2 Fleet that was successfully deleted.
--
-- /See:/ 'mkDeleteFleetSuccessItem' smart constructor.
data DeleteFleetSuccessItem = DeleteFleetSuccessItem'
  { -- | The current state of the EC2 Fleet.
    currentFleetState :: Core.Maybe Types.FleetStateCode,
    -- | The ID of the EC2 Fleet.
    fleetId :: Core.Maybe Types.FleetId,
    -- | The previous state of the EC2 Fleet.
    previousFleetState :: Core.Maybe Types.FleetStateCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFleetSuccessItem' value with any optional fields omitted.
mkDeleteFleetSuccessItem ::
  DeleteFleetSuccessItem
mkDeleteFleetSuccessItem =
  DeleteFleetSuccessItem'
    { currentFleetState = Core.Nothing,
      fleetId = Core.Nothing,
      previousFleetState = Core.Nothing
    }

-- | The current state of the EC2 Fleet.
--
-- /Note:/ Consider using 'currentFleetState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsiCurrentFleetState :: Lens.Lens' DeleteFleetSuccessItem (Core.Maybe Types.FleetStateCode)
dfsiCurrentFleetState = Lens.field @"currentFleetState"
{-# DEPRECATED dfsiCurrentFleetState "Use generic-lens or generic-optics with 'currentFleetState' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsiFleetId :: Lens.Lens' DeleteFleetSuccessItem (Core.Maybe Types.FleetId)
dfsiFleetId = Lens.field @"fleetId"
{-# DEPRECATED dfsiFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The previous state of the EC2 Fleet.
--
-- /Note:/ Consider using 'previousFleetState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsiPreviousFleetState :: Lens.Lens' DeleteFleetSuccessItem (Core.Maybe Types.FleetStateCode)
dfsiPreviousFleetState = Lens.field @"previousFleetState"
{-# DEPRECATED dfsiPreviousFleetState "Use generic-lens or generic-optics with 'previousFleetState' instead." #-}

instance Core.FromXML DeleteFleetSuccessItem where
  parseXML x =
    DeleteFleetSuccessItem'
      Core.<$> (x Core..@? "currentFleetState")
      Core.<*> (x Core..@? "fleetId")
      Core.<*> (x Core..@? "previousFleetState")
