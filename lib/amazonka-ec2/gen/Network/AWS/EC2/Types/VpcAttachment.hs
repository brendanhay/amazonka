{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcAttachment
  ( VpcAttachment (..)
  -- * Smart constructor
  , mkVpcAttachment
  -- * Lenses
  , vafState
  , vafVpcId
  ) where

import qualified Network.AWS.EC2.Types.AttachmentStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an attachment between a virtual private gateway and a VPC.
--
-- /See:/ 'mkVpcAttachment' smart constructor.
data VpcAttachment = VpcAttachment'
  { state :: Core.Maybe Types.AttachmentStatus
    -- ^ The current state of the attachment.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcAttachment' value with any optional fields omitted.
mkVpcAttachment
    :: VpcAttachment
mkVpcAttachment
  = VpcAttachment'{state = Core.Nothing, vpcId = Core.Nothing}

-- | The current state of the attachment.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vafState :: Lens.Lens' VpcAttachment (Core.Maybe Types.AttachmentStatus)
vafState = Lens.field @"state"
{-# INLINEABLE vafState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vafVpcId :: Lens.Lens' VpcAttachment (Core.Maybe Core.Text)
vafVpcId = Lens.field @"vpcId"
{-# INLINEABLE vafVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML VpcAttachment where
        parseXML x
          = VpcAttachment' Core.<$>
              (x Core..@? "state") Core.<*> x Core..@? "vpcId"
