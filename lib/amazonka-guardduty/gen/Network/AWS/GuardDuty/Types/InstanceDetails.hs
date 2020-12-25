{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.InstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.InstanceDetails
  ( InstanceDetails (..),

    -- * Smart constructor
    mkInstanceDetails,

    -- * Lenses
    idAvailabilityZone,
    idIamInstanceProfile,
    idImageDescription,
    idImageId,
    idInstanceId,
    idInstanceState,
    idInstanceType,
    idLaunchTime,
    idNetworkInterfaces,
    idOutpostArn,
    idPlatform,
    idProductCodes,
    idTags,
  )
where

import qualified Network.AWS.GuardDuty.Types.IamInstanceProfile as Types
import qualified Network.AWS.GuardDuty.Types.NetworkInterface as Types
import qualified Network.AWS.GuardDuty.Types.ProductCode as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.GuardDuty.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the details of an instance.
--
-- /See:/ 'mkInstanceDetails' smart constructor.
data InstanceDetails = InstanceDetails'
  { -- | The Availability Zone of the EC2 instance.
    availabilityZone :: Core.Maybe Types.String,
    -- | The profile information of the EC2 instance.
    iamInstanceProfile :: Core.Maybe Types.IamInstanceProfile,
    -- | The image description of the EC2 instance.
    imageDescription :: Core.Maybe Types.String,
    -- | The image ID of the EC2 instance.
    imageId :: Core.Maybe Types.String,
    -- | The ID of the EC2 instance.
    instanceId :: Core.Maybe Types.String,
    -- | The state of the EC2 instance.
    instanceState :: Core.Maybe Types.String,
    -- | The type of the EC2 instance.
    instanceType :: Core.Maybe Types.String,
    -- | The launch time of the EC2 instance.
    launchTime :: Core.Maybe Types.String,
    -- | The elastic network interface information of the EC2 instance.
    networkInterfaces :: Core.Maybe [Types.NetworkInterface],
    -- | The Amazon Resource Name (ARN) of the AWS Outpost. Only applicable to AWS Outposts instances.
    outpostArn :: Core.Maybe Types.String,
    -- | The platform of the EC2 instance.
    platform :: Core.Maybe Types.String,
    -- | The product code of the EC2 instance.
    productCodes :: Core.Maybe [Types.ProductCode],
    -- | The tags of the EC2 instance.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceDetails' value with any optional fields omitted.
mkInstanceDetails ::
  InstanceDetails
mkInstanceDetails =
  InstanceDetails'
    { availabilityZone = Core.Nothing,
      iamInstanceProfile = Core.Nothing,
      imageDescription = Core.Nothing,
      imageId = Core.Nothing,
      instanceId = Core.Nothing,
      instanceState = Core.Nothing,
      instanceType = Core.Nothing,
      launchTime = Core.Nothing,
      networkInterfaces = Core.Nothing,
      outpostArn = Core.Nothing,
      platform = Core.Nothing,
      productCodes = Core.Nothing,
      tags = Core.Nothing
    }

-- | The Availability Zone of the EC2 instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idAvailabilityZone :: Lens.Lens' InstanceDetails (Core.Maybe Types.String)
idAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED idAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The profile information of the EC2 instance.
--
-- /Note:/ Consider using 'iamInstanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idIamInstanceProfile :: Lens.Lens' InstanceDetails (Core.Maybe Types.IamInstanceProfile)
idIamInstanceProfile = Lens.field @"iamInstanceProfile"
{-# DEPRECATED idIamInstanceProfile "Use generic-lens or generic-optics with 'iamInstanceProfile' instead." #-}

-- | The image description of the EC2 instance.
--
-- /Note:/ Consider using 'imageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageDescription :: Lens.Lens' InstanceDetails (Core.Maybe Types.String)
idImageDescription = Lens.field @"imageDescription"
{-# DEPRECATED idImageDescription "Use generic-lens or generic-optics with 'imageDescription' instead." #-}

-- | The image ID of the EC2 instance.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idImageId :: Lens.Lens' InstanceDetails (Core.Maybe Types.String)
idImageId = Lens.field @"imageId"
{-# DEPRECATED idImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The ID of the EC2 instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInstanceId :: Lens.Lens' InstanceDetails (Core.Maybe Types.String)
idInstanceId = Lens.field @"instanceId"
{-# DEPRECATED idInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The state of the EC2 instance.
--
-- /Note:/ Consider using 'instanceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInstanceState :: Lens.Lens' InstanceDetails (Core.Maybe Types.String)
idInstanceState = Lens.field @"instanceState"
{-# DEPRECATED idInstanceState "Use generic-lens or generic-optics with 'instanceState' instead." #-}

-- | The type of the EC2 instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idInstanceType :: Lens.Lens' InstanceDetails (Core.Maybe Types.String)
idInstanceType = Lens.field @"instanceType"
{-# DEPRECATED idInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The launch time of the EC2 instance.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idLaunchTime :: Lens.Lens' InstanceDetails (Core.Maybe Types.String)
idLaunchTime = Lens.field @"launchTime"
{-# DEPRECATED idLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | The elastic network interface information of the EC2 instance.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idNetworkInterfaces :: Lens.Lens' InstanceDetails (Core.Maybe [Types.NetworkInterface])
idNetworkInterfaces = Lens.field @"networkInterfaces"
{-# DEPRECATED idNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Outpost. Only applicable to AWS Outposts instances.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idOutpostArn :: Lens.Lens' InstanceDetails (Core.Maybe Types.String)
idOutpostArn = Lens.field @"outpostArn"
{-# DEPRECATED idOutpostArn "Use generic-lens or generic-optics with 'outpostArn' instead." #-}

-- | The platform of the EC2 instance.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idPlatform :: Lens.Lens' InstanceDetails (Core.Maybe Types.String)
idPlatform = Lens.field @"platform"
{-# DEPRECATED idPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The product code of the EC2 instance.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idProductCodes :: Lens.Lens' InstanceDetails (Core.Maybe [Types.ProductCode])
idProductCodes = Lens.field @"productCodes"
{-# DEPRECATED idProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The tags of the EC2 instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idTags :: Lens.Lens' InstanceDetails (Core.Maybe [Types.Tag])
idTags = Lens.field @"tags"
{-# DEPRECATED idTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON InstanceDetails where
  parseJSON =
    Core.withObject "InstanceDetails" Core.$
      \x ->
        InstanceDetails'
          Core.<$> (x Core..:? "availabilityZone")
          Core.<*> (x Core..:? "iamInstanceProfile")
          Core.<*> (x Core..:? "imageDescription")
          Core.<*> (x Core..:? "imageId")
          Core.<*> (x Core..:? "instanceId")
          Core.<*> (x Core..:? "instanceState")
          Core.<*> (x Core..:? "instanceType")
          Core.<*> (x Core..:? "launchTime")
          Core.<*> (x Core..:? "networkInterfaces")
          Core.<*> (x Core..:? "outpostArn")
          Core.<*> (x Core..:? "platform")
          Core.<*> (x Core..:? "productCodes")
          Core.<*> (x Core..:? "tags")
