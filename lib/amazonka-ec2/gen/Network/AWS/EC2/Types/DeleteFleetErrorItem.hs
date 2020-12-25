{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetErrorItem
  ( DeleteFleetErrorItem (..),

    -- * Smart constructor
    mkDeleteFleetErrorItem,

    -- * Lenses
    dfeiError,
    dfeiFleetId,
  )
where

import qualified Network.AWS.EC2.Types.DeleteFleetError as Types
import qualified Network.AWS.EC2.Types.FleetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an EC2 Fleet that was not successfully deleted.
--
-- /See:/ 'mkDeleteFleetErrorItem' smart constructor.
data DeleteFleetErrorItem = DeleteFleetErrorItem'
  { -- | The error.
    error :: Core.Maybe Types.DeleteFleetError,
    -- | The ID of the EC2 Fleet.
    fleetId :: Core.Maybe Types.FleetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFleetErrorItem' value with any optional fields omitted.
mkDeleteFleetErrorItem ::
  DeleteFleetErrorItem
mkDeleteFleetErrorItem =
  DeleteFleetErrorItem'
    { error = Core.Nothing,
      fleetId = Core.Nothing
    }

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeiError :: Lens.Lens' DeleteFleetErrorItem (Core.Maybe Types.DeleteFleetError)
dfeiError = Lens.field @"error"
{-# DEPRECATED dfeiError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeiFleetId :: Lens.Lens' DeleteFleetErrorItem (Core.Maybe Types.FleetId)
dfeiFleetId = Lens.field @"fleetId"
{-# DEPRECATED dfeiFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Core.FromXML DeleteFleetErrorItem where
  parseXML x =
    DeleteFleetErrorItem'
      Core.<$> (x Core..@? "error") Core.<*> (x Core..@? "fleetId")
