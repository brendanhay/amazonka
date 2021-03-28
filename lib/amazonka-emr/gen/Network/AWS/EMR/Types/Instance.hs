{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Instance
  ( Instance (..)
  -- * Smart constructor
  , mkInstance
  -- * Lenses
  , iEbsVolumes
  , iEc2InstanceId
  , iId
  , iInstanceFleetId
  , iInstanceGroupId
  , iInstanceType
  , iMarket
  , iPrivateDnsName
  , iPrivateIpAddress
  , iPublicDnsName
  , iPublicIpAddress
  , iStatus
  ) where

import qualified Network.AWS.EMR.Types.EbsVolume as Types
import qualified Network.AWS.EMR.Types.InstanceFleetId as Types
import qualified Network.AWS.EMR.Types.InstanceId as Types
import qualified Network.AWS.EMR.Types.InstanceStatus as Types
import qualified Network.AWS.EMR.Types.InstanceType as Types
import qualified Network.AWS.EMR.Types.MarketType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an EC2 instance provisioned as part of cluster.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { ebsVolumes :: Core.Maybe [Types.EbsVolume]
    -- ^ The list of EBS volumes that are attached to this instance.
  , ec2InstanceId :: Core.Maybe Types.InstanceId
    -- ^ The unique identifier of the instance in Amazon EC2.
  , id :: Core.Maybe Types.InstanceId
    -- ^ The unique identifier for the instance in Amazon EMR.
  , instanceFleetId :: Core.Maybe Types.InstanceFleetId
    -- ^ The unique identifier of the instance fleet to which an EC2 instance belongs.
  , instanceGroupId :: Core.Maybe Core.Text
    -- ^ The identifier of the instance group to which this instance belongs.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The EC2 instance type, for example @m3.xlarge@ .
  , market :: Core.Maybe Types.MarketType
    -- ^ The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ . 
  , privateDnsName :: Core.Maybe Core.Text
    -- ^ The private DNS name of the instance.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The private IP address of the instance.
  , publicDnsName :: Core.Maybe Core.Text
    -- ^ The public DNS name of the instance.
  , publicIpAddress :: Core.Maybe Core.Text
    -- ^ The public IP address of the instance.
  , status :: Core.Maybe Types.InstanceStatus
    -- ^ The current status of the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance
    :: Instance
mkInstance
  = Instance'{ebsVolumes = Core.Nothing,
              ec2InstanceId = Core.Nothing, id = Core.Nothing,
              instanceFleetId = Core.Nothing, instanceGroupId = Core.Nothing,
              instanceType = Core.Nothing, market = Core.Nothing,
              privateDnsName = Core.Nothing, privateIpAddress = Core.Nothing,
              publicDnsName = Core.Nothing, publicIpAddress = Core.Nothing,
              status = Core.Nothing}

-- | The list of EBS volumes that are attached to this instance.
--
-- /Note:/ Consider using 'ebsVolumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEbsVolumes :: Lens.Lens' Instance (Core.Maybe [Types.EbsVolume])
iEbsVolumes = Lens.field @"ebsVolumes"
{-# INLINEABLE iEbsVolumes #-}
{-# DEPRECATED ebsVolumes "Use generic-lens or generic-optics with 'ebsVolumes' instead"  #-}

-- | The unique identifier of the instance in Amazon EC2.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEc2InstanceId :: Lens.Lens' Instance (Core.Maybe Types.InstanceId)
iEc2InstanceId = Lens.field @"ec2InstanceId"
{-# INLINEABLE iEc2InstanceId #-}
{-# DEPRECATED ec2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead"  #-}

-- | The unique identifier for the instance in Amazon EMR.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Instance (Core.Maybe Types.InstanceId)
iId = Lens.field @"id"
{-# INLINEABLE iId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The unique identifier of the instance fleet to which an EC2 instance belongs.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceFleetId :: Lens.Lens' Instance (Core.Maybe Types.InstanceFleetId)
iInstanceFleetId = Lens.field @"instanceFleetId"
{-# INLINEABLE iInstanceFleetId #-}
{-# DEPRECATED instanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead"  #-}

-- | The identifier of the instance group to which this instance belongs.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceGroupId :: Lens.Lens' Instance (Core.Maybe Core.Text)
iInstanceGroupId = Lens.field @"instanceGroupId"
{-# INLINEABLE iInstanceGroupId #-}
{-# DEPRECATED instanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead"  #-}

-- | The EC2 instance type, for example @m3.xlarge@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceType :: Lens.Lens' Instance (Core.Maybe Types.InstanceType)
iInstanceType = Lens.field @"instanceType"
{-# INLINEABLE iInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ . 
--
-- /Note:/ Consider using 'market' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMarket :: Lens.Lens' Instance (Core.Maybe Types.MarketType)
iMarket = Lens.field @"market"
{-# INLINEABLE iMarket #-}
{-# DEPRECATED market "Use generic-lens or generic-optics with 'market' instead"  #-}

-- | The private DNS name of the instance.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateDnsName :: Lens.Lens' Instance (Core.Maybe Core.Text)
iPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE iPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The private IP address of the instance.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateIpAddress :: Lens.Lens' Instance (Core.Maybe Core.Text)
iPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE iPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | The public DNS name of the instance.
--
-- /Note:/ Consider using 'publicDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicDnsName :: Lens.Lens' Instance (Core.Maybe Core.Text)
iPublicDnsName = Lens.field @"publicDnsName"
{-# INLINEABLE iPublicDnsName #-}
{-# DEPRECATED publicDnsName "Use generic-lens or generic-optics with 'publicDnsName' instead"  #-}

-- | The public IP address of the instance.
--
-- /Note:/ Consider using 'publicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicIpAddress :: Lens.Lens' Instance (Core.Maybe Core.Text)
iPublicIpAddress = Lens.field @"publicIpAddress"
{-# INLINEABLE iPublicIpAddress #-}
{-# DEPRECATED publicIpAddress "Use generic-lens or generic-optics with 'publicIpAddress' instead"  #-}

-- | The current status of the instance.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatus :: Lens.Lens' Instance (Core.Maybe Types.InstanceStatus)
iStatus = Lens.field @"status"
{-# INLINEABLE iStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON Instance where
        parseJSON
          = Core.withObject "Instance" Core.$
              \ x ->
                Instance' Core.<$>
                  (x Core..:? "EbsVolumes") Core.<*> x Core..:? "Ec2InstanceId"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "InstanceFleetId"
                    Core.<*> x Core..:? "InstanceGroupId"
                    Core.<*> x Core..:? "InstanceType"
                    Core.<*> x Core..:? "Market"
                    Core.<*> x Core..:? "PrivateDnsName"
                    Core.<*> x Core..:? "PrivateIpAddress"
                    Core.<*> x Core..:? "PublicDnsName"
                    Core.<*> x Core..:? "PublicIpAddress"
                    Core.<*> x Core..:? "Status"
