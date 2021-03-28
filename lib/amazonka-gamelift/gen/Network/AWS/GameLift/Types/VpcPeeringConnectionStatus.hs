{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.VpcPeeringConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.VpcPeeringConnectionStatus
  ( VpcPeeringConnectionStatus (..)
  -- * Smart constructor
  , mkVpcPeeringConnectionStatus
  -- * Lenses
  , vpcsCode
  , vpcsMessage
  ) where

import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents status information for a VPC peering connection. Status is associated with a 'VpcPeeringConnection' object. Status codes and messages are provided from EC2 (see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpcPeeringConnectionStateReason.html VpcPeeringConnectionStateReason> ). Connection status information is also communicated as a fleet 'Event' .
--
-- /See:/ 'mkVpcPeeringConnectionStatus' smart constructor.
data VpcPeeringConnectionStatus = VpcPeeringConnectionStatus'
  { code :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Code indicating the status of a VPC peering connection.
  , message :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Additional messaging associated with the connection status. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcPeeringConnectionStatus' value with any optional fields omitted.
mkVpcPeeringConnectionStatus
    :: VpcPeeringConnectionStatus
mkVpcPeeringConnectionStatus
  = VpcPeeringConnectionStatus'{code = Core.Nothing,
                                message = Core.Nothing}

-- | Code indicating the status of a VPC peering connection.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcsCode :: Lens.Lens' VpcPeeringConnectionStatus (Core.Maybe Types.NonZeroAndMaxString)
vpcsCode = Lens.field @"code"
{-# INLINEABLE vpcsCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | Additional messaging associated with the connection status. 
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcsMessage :: Lens.Lens' VpcPeeringConnectionStatus (Core.Maybe Types.NonZeroAndMaxString)
vpcsMessage = Lens.field @"message"
{-# INLINEABLE vpcsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON VpcPeeringConnectionStatus where
        parseJSON
          = Core.withObject "VpcPeeringConnectionStatus" Core.$
              \ x ->
                VpcPeeringConnectionStatus' Core.<$>
                  (x Core..:? "Code") Core.<*> x Core..:? "Message"
