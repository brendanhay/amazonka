{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnectionStateReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcPeeringConnectionStateReason
  ( VpcPeeringConnectionStateReason (..)
  -- * Smart constructor
  , mkVpcPeeringConnectionStateReason
  -- * Lenses
  , vpcsrCode
  , vpcsrMessage
  ) where

import qualified Network.AWS.EC2.Types.VpcPeeringConnectionStateReasonCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the status of a VPC peering connection.
--
-- /See:/ 'mkVpcPeeringConnectionStateReason' smart constructor.
data VpcPeeringConnectionStateReason = VpcPeeringConnectionStateReason'
  { code :: Core.Maybe Types.VpcPeeringConnectionStateReasonCode
    -- ^ The status of the VPC peering connection.
  , message :: Core.Maybe Core.Text
    -- ^ A message that provides more information about the status, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcPeeringConnectionStateReason' value with any optional fields omitted.
mkVpcPeeringConnectionStateReason
    :: VpcPeeringConnectionStateReason
mkVpcPeeringConnectionStateReason
  = VpcPeeringConnectionStateReason'{code = Core.Nothing,
                                     message = Core.Nothing}

-- | The status of the VPC peering connection.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcsrCode :: Lens.Lens' VpcPeeringConnectionStateReason (Core.Maybe Types.VpcPeeringConnectionStateReasonCode)
vpcsrCode = Lens.field @"code"
{-# INLINEABLE vpcsrCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A message that provides more information about the status, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcsrMessage :: Lens.Lens' VpcPeeringConnectionStateReason (Core.Maybe Core.Text)
vpcsrMessage = Lens.field @"message"
{-# INLINEABLE vpcsrMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML VpcPeeringConnectionStateReason where
        parseXML x
          = VpcPeeringConnectionStateReason' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
