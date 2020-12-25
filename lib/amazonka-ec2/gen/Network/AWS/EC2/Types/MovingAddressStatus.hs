{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MovingAddressStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MovingAddressStatus
  ( MovingAddressStatus (..),

    -- * Smart constructor
    mkMovingAddressStatus,

    -- * Lenses
    masMoveStatus,
    masPublicIp,
  )
where

import qualified Network.AWS.EC2.Types.MoveStatus as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the status of a moving Elastic IP address.
--
-- /See:/ 'mkMovingAddressStatus' smart constructor.
data MovingAddressStatus = MovingAddressStatus'
  { -- | The status of the Elastic IP address that's being moved to the EC2-VPC platform, or restored to the EC2-Classic platform.
    moveStatus :: Core.Maybe Types.MoveStatus,
    -- | The Elastic IP address.
    publicIp :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MovingAddressStatus' value with any optional fields omitted.
mkMovingAddressStatus ::
  MovingAddressStatus
mkMovingAddressStatus =
  MovingAddressStatus'
    { moveStatus = Core.Nothing,
      publicIp = Core.Nothing
    }

-- | The status of the Elastic IP address that's being moved to the EC2-VPC platform, or restored to the EC2-Classic platform.
--
-- /Note:/ Consider using 'moveStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masMoveStatus :: Lens.Lens' MovingAddressStatus (Core.Maybe Types.MoveStatus)
masMoveStatus = Lens.field @"moveStatus"
{-# DEPRECATED masMoveStatus "Use generic-lens or generic-optics with 'moveStatus' instead." #-}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masPublicIp :: Lens.Lens' MovingAddressStatus (Core.Maybe Types.String)
masPublicIp = Lens.field @"publicIp"
{-# DEPRECATED masPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

instance Core.FromXML MovingAddressStatus where
  parseXML x =
    MovingAddressStatus'
      Core.<$> (x Core..@? "moveStatus") Core.<*> (x Core..@? "publicIp")
