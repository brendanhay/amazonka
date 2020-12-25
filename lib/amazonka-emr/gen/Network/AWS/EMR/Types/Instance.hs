{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iEbsVolumes,
    iEc2InstanceId,
    iId,
    iInstanceFleetId,
    iInstanceGroupId,
    iInstanceType,
    iMarket,
    iPrivateDnsName,
    iPrivateIpAddress,
    iPublicDnsName,
    iPublicIpAddress,
    iStatus,
  )
where

import qualified Network.AWS.EMR.Types.EbsVolume as Types
import qualified Network.AWS.EMR.Types.InstanceFleetId as Types
import qualified Network.AWS.EMR.Types.InstanceId as Types
import qualified Network.AWS.EMR.Types.InstanceStatus as Types
import qualified Network.AWS.EMR.Types.InstanceType as Types
import qualified Network.AWS.EMR.Types.MarketType as Types
import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an EC2 instance provisioned as part of cluster.
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { -- | The list of EBS volumes that are attached to this instance.
    ebsVolumes :: Core.Maybe [Types.EbsVolume],
    -- | The unique identifier of the instance in Amazon EC2.
    ec2InstanceId :: Core.Maybe Types.InstanceId,
    -- | The unique identifier for the instance in Amazon EMR.
    id :: Core.Maybe Types.InstanceId,
    -- | The unique identifier of the instance fleet to which an EC2 instance belongs.
    instanceFleetId :: Core.Maybe Types.InstanceFleetId,
    -- | The identifier of the instance group to which this instance belongs.
    instanceGroupId :: Core.Maybe Types.String,
    -- | The EC2 instance type, for example @m3.xlarge@ .
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ .
    market :: Core.Maybe Types.MarketType,
    -- | The private DNS name of the instance.
    privateDnsName :: Core.Maybe Types.String,
    -- | The private IP address of the instance.
    privateIpAddress :: Core.Maybe Types.String,
    -- | The public DNS name of the instance.
    publicDnsName :: Core.Maybe Types.String,
    -- | The public IP address of the instance.
    publicIpAddress :: Core.Maybe Types.String,
    -- | The current status of the instance.
    status :: Core.Maybe Types.InstanceStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance ::
  Instance
mkInstance =
  Instance'
    { ebsVolumes = Core.Nothing,
      ec2InstanceId = Core.Nothing,
      id = Core.Nothing,
      instanceFleetId = Core.Nothing,
      instanceGroupId = Core.Nothing,
      instanceType = Core.Nothing,
      market = Core.Nothing,
      privateDnsName = Core.Nothing,
      privateIpAddress = Core.Nothing,
      publicDnsName = Core.Nothing,
      publicIpAddress = Core.Nothing,
      status = Core.Nothing
    }

-- | The list of EBS volumes that are attached to this instance.
--
-- /Note:/ Consider using 'ebsVolumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEbsVolumes :: Lens.Lens' Instance (Core.Maybe [Types.EbsVolume])
iEbsVolumes = Lens.field @"ebsVolumes"
{-# DEPRECATED iEbsVolumes "Use generic-lens or generic-optics with 'ebsVolumes' instead." #-}

-- | The unique identifier of the instance in Amazon EC2.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iEc2InstanceId :: Lens.Lens' Instance (Core.Maybe Types.InstanceId)
iEc2InstanceId = Lens.field @"ec2InstanceId"
{-# DEPRECATED iEc2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The unique identifier for the instance in Amazon EMR.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Instance (Core.Maybe Types.InstanceId)
iId = Lens.field @"id"
{-# DEPRECATED iId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The unique identifier of the instance fleet to which an EC2 instance belongs.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceFleetId :: Lens.Lens' Instance (Core.Maybe Types.InstanceFleetId)
iInstanceFleetId = Lens.field @"instanceFleetId"
{-# DEPRECATED iInstanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead." #-}

-- | The identifier of the instance group to which this instance belongs.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceGroupId :: Lens.Lens' Instance (Core.Maybe Types.String)
iInstanceGroupId = Lens.field @"instanceGroupId"
{-# DEPRECATED iInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

-- | The EC2 instance type, for example @m3.xlarge@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInstanceType :: Lens.Lens' Instance (Core.Maybe Types.InstanceType)
iInstanceType = Lens.field @"instanceType"
{-# DEPRECATED iInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The instance purchasing option. Valid values are @ON_DEMAND@ or @SPOT@ .
--
-- /Note:/ Consider using 'market' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMarket :: Lens.Lens' Instance (Core.Maybe Types.MarketType)
iMarket = Lens.field @"market"
{-# DEPRECATED iMarket "Use generic-lens or generic-optics with 'market' instead." #-}

-- | The private DNS name of the instance.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateDnsName :: Lens.Lens' Instance (Core.Maybe Types.String)
iPrivateDnsName = Lens.field @"privateDnsName"
{-# DEPRECATED iPrivateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead." #-}

-- | The private IP address of the instance.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateIpAddress :: Lens.Lens' Instance (Core.Maybe Types.String)
iPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED iPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | The public DNS name of the instance.
--
-- /Note:/ Consider using 'publicDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicDnsName :: Lens.Lens' Instance (Core.Maybe Types.String)
iPublicDnsName = Lens.field @"publicDnsName"
{-# DEPRECATED iPublicDnsName "Use generic-lens or generic-optics with 'publicDnsName' instead." #-}

-- | The public IP address of the instance.
--
-- /Note:/ Consider using 'publicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicIpAddress :: Lens.Lens' Instance (Core.Maybe Types.String)
iPublicIpAddress = Lens.field @"publicIpAddress"
{-# DEPRECATED iPublicIpAddress "Use generic-lens or generic-optics with 'publicIpAddress' instead." #-}

-- | The current status of the instance.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatus :: Lens.Lens' Instance (Core.Maybe Types.InstanceStatus)
iStatus = Lens.field @"status"
{-# DEPRECATED iStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject "Instance" Core.$
      \x ->
        Instance'
          Core.<$> (x Core..:? "EbsVolumes")
          Core.<*> (x Core..:? "Ec2InstanceId")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "InstanceFleetId")
          Core.<*> (x Core..:? "InstanceGroupId")
          Core.<*> (x Core..:? "InstanceType")
          Core.<*> (x Core..:? "Market")
          Core.<*> (x Core..:? "PrivateDnsName")
          Core.<*> (x Core..:? "PrivateIpAddress")
          Core.<*> (x Core..:? "PublicDnsName")
          Core.<*> (x Core..:? "PublicIpAddress")
          Core.<*> (x Core..:? "Status")
