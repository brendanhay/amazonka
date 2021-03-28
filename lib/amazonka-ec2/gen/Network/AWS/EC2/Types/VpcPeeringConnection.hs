{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcPeeringConnection
  ( VpcPeeringConnection (..)
  -- * Smart constructor
  , mkVpcPeeringConnection
  -- * Lenses
  , vpcAccepterVpcInfo
  , vpcExpirationTime
  , vpcRequesterVpcInfo
  , vpcStatus
  , vpcTags
  , vpcVpcPeeringConnectionId
  ) where

import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VpcPeeringConnectionStateReason as Types
import qualified Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPC peering connection.
--
-- /See:/ 'mkVpcPeeringConnection' smart constructor.
data VpcPeeringConnection = VpcPeeringConnection'
  { accepterVpcInfo :: Core.Maybe Types.VpcPeeringConnectionVpcInfo
    -- ^ Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
  , expirationTime :: Core.Maybe Core.UTCTime
    -- ^ The time that an unaccepted VPC peering connection will expire.
  , requesterVpcInfo :: Core.Maybe Types.VpcPeeringConnectionVpcInfo
    -- ^ Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
  , status :: Core.Maybe Types.VpcPeeringConnectionStateReason
    -- ^ The status of the VPC peering connection.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the resource.
  , vpcPeeringConnectionId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC peering connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VpcPeeringConnection' value with any optional fields omitted.
mkVpcPeeringConnection
    :: VpcPeeringConnection
mkVpcPeeringConnection
  = VpcPeeringConnection'{accepterVpcInfo = Core.Nothing,
                          expirationTime = Core.Nothing, requesterVpcInfo = Core.Nothing,
                          status = Core.Nothing, tags = Core.Nothing,
                          vpcPeeringConnectionId = Core.Nothing}

-- | Information about the accepter VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- /Note:/ Consider using 'accepterVpcInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcAccepterVpcInfo :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.VpcPeeringConnectionVpcInfo)
vpcAccepterVpcInfo = Lens.field @"accepterVpcInfo"
{-# INLINEABLE vpcAccepterVpcInfo #-}
{-# DEPRECATED accepterVpcInfo "Use generic-lens or generic-optics with 'accepterVpcInfo' instead"  #-}

-- | The time that an unaccepted VPC peering connection will expire.
--
-- /Note:/ Consider using 'expirationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcExpirationTime :: Lens.Lens' VpcPeeringConnection (Core.Maybe Core.UTCTime)
vpcExpirationTime = Lens.field @"expirationTime"
{-# INLINEABLE vpcExpirationTime #-}
{-# DEPRECATED expirationTime "Use generic-lens or generic-optics with 'expirationTime' instead"  #-}

-- | Information about the requester VPC. CIDR block information is only returned when describing an active VPC peering connection.
--
-- /Note:/ Consider using 'requesterVpcInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcRequesterVpcInfo :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.VpcPeeringConnectionVpcInfo)
vpcRequesterVpcInfo = Lens.field @"requesterVpcInfo"
{-# INLINEABLE vpcRequesterVpcInfo #-}
{-# DEPRECATED requesterVpcInfo "Use generic-lens or generic-optics with 'requesterVpcInfo' instead"  #-}

-- | The status of the VPC peering connection.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcStatus :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.VpcPeeringConnectionStateReason)
vpcStatus = Lens.field @"status"
{-# INLINEABLE vpcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcTags :: Lens.Lens' VpcPeeringConnection (Core.Maybe [Types.Tag])
vpcTags = Lens.field @"tags"
{-# INLINEABLE vpcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcVpcPeeringConnectionId :: Lens.Lens' VpcPeeringConnection (Core.Maybe Core.Text)
vpcVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE vpcVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

instance Core.FromXML VpcPeeringConnection where
        parseXML x
          = VpcPeeringConnection' Core.<$>
              (x Core..@? "accepterVpcInfo") Core.<*> x Core..@? "expirationTime"
                Core.<*> x Core..@? "requesterVpcInfo"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vpcPeeringConnectionId"
