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
    dfsiPreviousFleetState,
    dfsiFleetId,
  )
where

import Network.AWS.EC2.Types.FleetStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an EC2 Fleet that was successfully deleted.
--
-- /See:/ 'mkDeleteFleetSuccessItem' smart constructor.
data DeleteFleetSuccessItem = DeleteFleetSuccessItem'
  { currentFleetState ::
      Lude.Maybe FleetStateCode,
    previousFleetState ::
      Lude.Maybe FleetStateCode,
    fleetId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFleetSuccessItem' with the minimum fields required to make a request.
--
-- * 'currentFleetState' - The current state of the EC2 Fleet.
-- * 'fleetId' - The ID of the EC2 Fleet.
-- * 'previousFleetState' - The previous state of the EC2 Fleet.
mkDeleteFleetSuccessItem ::
  DeleteFleetSuccessItem
mkDeleteFleetSuccessItem =
  DeleteFleetSuccessItem'
    { currentFleetState = Lude.Nothing,
      previousFleetState = Lude.Nothing,
      fleetId = Lude.Nothing
    }

-- | The current state of the EC2 Fleet.
--
-- /Note:/ Consider using 'currentFleetState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsiCurrentFleetState :: Lens.Lens' DeleteFleetSuccessItem (Lude.Maybe FleetStateCode)
dfsiCurrentFleetState = Lens.lens (currentFleetState :: DeleteFleetSuccessItem -> Lude.Maybe FleetStateCode) (\s a -> s {currentFleetState = a} :: DeleteFleetSuccessItem)
{-# DEPRECATED dfsiCurrentFleetState "Use generic-lens or generic-optics with 'currentFleetState' instead." #-}

-- | The previous state of the EC2 Fleet.
--
-- /Note:/ Consider using 'previousFleetState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsiPreviousFleetState :: Lens.Lens' DeleteFleetSuccessItem (Lude.Maybe FleetStateCode)
dfsiPreviousFleetState = Lens.lens (previousFleetState :: DeleteFleetSuccessItem -> Lude.Maybe FleetStateCode) (\s a -> s {previousFleetState = a} :: DeleteFleetSuccessItem)
{-# DEPRECATED dfsiPreviousFleetState "Use generic-lens or generic-optics with 'previousFleetState' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsiFleetId :: Lens.Lens' DeleteFleetSuccessItem (Lude.Maybe Lude.Text)
dfsiFleetId = Lens.lens (fleetId :: DeleteFleetSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: DeleteFleetSuccessItem)
{-# DEPRECATED dfsiFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.FromXML DeleteFleetSuccessItem where
  parseXML x =
    DeleteFleetSuccessItem'
      Lude.<$> (x Lude..@? "currentFleetState")
      Lude.<*> (x Lude..@? "previousFleetState")
      Lude.<*> (x Lude..@? "fleetId")
