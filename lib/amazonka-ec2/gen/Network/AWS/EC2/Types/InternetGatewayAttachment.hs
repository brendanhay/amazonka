{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InternetGatewayAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InternetGatewayAttachment
  ( InternetGatewayAttachment (..)
  -- * Smart constructor
  , mkInternetGatewayAttachment
  -- * Lenses
  , igaState
  , igaVpcId
  ) where

import qualified Network.AWS.EC2.Types.AttachmentStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the attachment of a VPC to an internet gateway or an egress-only internet gateway.
--
-- /See:/ 'mkInternetGatewayAttachment' smart constructor.
data InternetGatewayAttachment = InternetGatewayAttachment'
  { state :: Types.AttachmentStatus
    -- ^ The current state of the attachment. For an internet gateway, the state is @available@ when attached to a VPC; otherwise, this value is not returned.
  , vpcId :: Core.Text
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InternetGatewayAttachment' value with any optional fields omitted.
mkInternetGatewayAttachment
    :: Types.AttachmentStatus -- ^ 'state'
    -> Core.Text -- ^ 'vpcId'
    -> InternetGatewayAttachment
mkInternetGatewayAttachment state vpcId
  = InternetGatewayAttachment'{state, vpcId}

-- | The current state of the attachment. For an internet gateway, the state is @available@ when attached to a VPC; otherwise, this value is not returned.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igaState :: Lens.Lens' InternetGatewayAttachment Types.AttachmentStatus
igaState = Lens.field @"state"
{-# INLINEABLE igaState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igaVpcId :: Lens.Lens' InternetGatewayAttachment Core.Text
igaVpcId = Lens.field @"vpcId"
{-# INLINEABLE igaVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML InternetGatewayAttachment where
        parseXML x
          = InternetGatewayAttachment' Core.<$>
              (x Core..@ "state") Core.<*> x Core..@ "vpcId"
