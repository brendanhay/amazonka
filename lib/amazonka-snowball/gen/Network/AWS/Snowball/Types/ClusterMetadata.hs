{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.ClusterMetadata
  ( ClusterMetadata (..)
  -- * Smart constructor
  , mkClusterMetadata
  -- * Lenses
  , cmAddressId
  , cmClusterId
  , cmClusterState
  , cmCreationDate
  , cmDescription
  , cmForwardingAddressId
  , cmJobType
  , cmKmsKeyARN
  , cmNotification
  , cmResources
  , cmRoleARN
  , cmShippingOption
  , cmSnowballType
  , cmTaxDocuments
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.AddressId as Types
import qualified Network.AWS.Snowball.Types.ClusterState as Types
import qualified Network.AWS.Snowball.Types.JobResource as Types
import qualified Network.AWS.Snowball.Types.JobType as Types
import qualified Network.AWS.Snowball.Types.KmsKeyARN as Types
import qualified Network.AWS.Snowball.Types.Notification as Types
import qualified Network.AWS.Snowball.Types.RoleARN as Types
import qualified Network.AWS.Snowball.Types.ShippingOption as Types
import qualified Network.AWS.Snowball.Types.SnowballType as Types
import qualified Network.AWS.Snowball.Types.TaxDocuments as Types

-- | Contains metadata about a specific cluster.
--
-- /See:/ 'mkClusterMetadata' smart constructor.
data ClusterMetadata = ClusterMetadata'
  { addressId :: Core.Maybe Types.AddressId
    -- ^ The automatically generated ID for a specific address.
  , clusterId :: Core.Maybe Core.Text
    -- ^ The automatically generated ID for a cluster.
  , clusterState :: Core.Maybe Types.ClusterState
    -- ^ The current status of the cluster.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date for this cluster.
  , description :: Core.Maybe Core.Text
    -- ^ The optional description of the cluster.
  , forwardingAddressId :: Core.Maybe Types.AddressId
    -- ^ The ID of the address that you want a cluster shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
  , jobType :: Core.Maybe Types.JobType
    -- ^ The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
  , kmsKeyARN :: Core.Maybe Types.KmsKeyARN
    -- ^ The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
  , notification :: Core.Maybe Types.Notification
    -- ^ The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
  , resources :: Core.Maybe Types.JobResource
    -- ^ The arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ The role ARN associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
  , shippingOption :: Core.Maybe Types.ShippingOption
    -- ^ The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows:
--
--
--     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow devices are delivered in one to seven days.
--
--
--     * In the US, you have access to one-day shipping and two-day shipping.
--
--
  , snowballType :: Core.Maybe Types.SnowballType
    -- ^ The type of AWS Snow device to use for this cluster. 
  , taxDocuments :: Core.Maybe Types.TaxDocuments
    -- ^ The tax documents required in your AWS Region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ClusterMetadata' value with any optional fields omitted.
mkClusterMetadata
    :: ClusterMetadata
mkClusterMetadata
  = ClusterMetadata'{addressId = Core.Nothing,
                     clusterId = Core.Nothing, clusterState = Core.Nothing,
                     creationDate = Core.Nothing, description = Core.Nothing,
                     forwardingAddressId = Core.Nothing, jobType = Core.Nothing,
                     kmsKeyARN = Core.Nothing, notification = Core.Nothing,
                     resources = Core.Nothing, roleARN = Core.Nothing,
                     shippingOption = Core.Nothing, snowballType = Core.Nothing,
                     taxDocuments = Core.Nothing}

-- | The automatically generated ID for a specific address.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmAddressId :: Lens.Lens' ClusterMetadata (Core.Maybe Types.AddressId)
cmAddressId = Lens.field @"addressId"
{-# INLINEABLE cmAddressId #-}
{-# DEPRECATED addressId "Use generic-lens or generic-optics with 'addressId' instead"  #-}

-- | The automatically generated ID for a cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmClusterId :: Lens.Lens' ClusterMetadata (Core.Maybe Core.Text)
cmClusterId = Lens.field @"clusterId"
{-# INLINEABLE cmClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The current status of the cluster.
--
-- /Note:/ Consider using 'clusterState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmClusterState :: Lens.Lens' ClusterMetadata (Core.Maybe Types.ClusterState)
cmClusterState = Lens.field @"clusterState"
{-# INLINEABLE cmClusterState #-}
{-# DEPRECATED clusterState "Use generic-lens or generic-optics with 'clusterState' instead"  #-}

-- | The creation date for this cluster.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmCreationDate :: Lens.Lens' ClusterMetadata (Core.Maybe Core.NominalDiffTime)
cmCreationDate = Lens.field @"creationDate"
{-# INLINEABLE cmCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The optional description of the cluster.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmDescription :: Lens.Lens' ClusterMetadata (Core.Maybe Core.Text)
cmDescription = Lens.field @"description"
{-# INLINEABLE cmDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the address that you want a cluster shipped to, after it will be shipped to its primary address. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmForwardingAddressId :: Lens.Lens' ClusterMetadata (Core.Maybe Types.AddressId)
cmForwardingAddressId = Lens.field @"forwardingAddressId"
{-# INLINEABLE cmForwardingAddressId #-}
{-# DEPRECATED forwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead"  #-}

-- | The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmJobType :: Lens.Lens' ClusterMetadata (Core.Maybe Types.JobType)
cmJobType = Lens.field @"jobType"
{-# INLINEABLE cmJobType #-}
{-# DEPRECATED jobType "Use generic-lens or generic-optics with 'jobType' instead"  #-}

-- | The @KmsKeyARN@ Amazon Resource Name (ARN) associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS).
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmKmsKeyARN :: Lens.Lens' ClusterMetadata (Core.Maybe Types.KmsKeyARN)
cmKmsKeyARN = Lens.field @"kmsKeyARN"
{-# INLINEABLE cmKmsKeyARN #-}
{-# DEPRECATED kmsKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead"  #-}

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNotification :: Lens.Lens' ClusterMetadata (Core.Maybe Types.Notification)
cmNotification = Lens.field @"notification"
{-# INLINEABLE cmNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | The arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmResources :: Lens.Lens' ClusterMetadata (Core.Maybe Types.JobResource)
cmResources = Lens.field @"resources"
{-# INLINEABLE cmResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | The role ARN associated with this cluster. This ARN was created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRoleARN :: Lens.Lens' ClusterMetadata (Core.Maybe Types.RoleARN)
cmRoleARN = Lens.field @"roleARN"
{-# INLINEABLE cmRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows:
--
--
--     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow devices are delivered in one to seven days.
--
--
--     * In the US, you have access to one-day shipping and two-day shipping.
--
--
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmShippingOption :: Lens.Lens' ClusterMetadata (Core.Maybe Types.ShippingOption)
cmShippingOption = Lens.field @"shippingOption"
{-# INLINEABLE cmShippingOption #-}
{-# DEPRECATED shippingOption "Use generic-lens or generic-optics with 'shippingOption' instead"  #-}

-- | The type of AWS Snow device to use for this cluster. 
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmSnowballType :: Lens.Lens' ClusterMetadata (Core.Maybe Types.SnowballType)
cmSnowballType = Lens.field @"snowballType"
{-# INLINEABLE cmSnowballType #-}
{-# DEPRECATED snowballType "Use generic-lens or generic-optics with 'snowballType' instead"  #-}

-- | The tax documents required in your AWS Region.
--
-- /Note:/ Consider using 'taxDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmTaxDocuments :: Lens.Lens' ClusterMetadata (Core.Maybe Types.TaxDocuments)
cmTaxDocuments = Lens.field @"taxDocuments"
{-# INLINEABLE cmTaxDocuments #-}
{-# DEPRECATED taxDocuments "Use generic-lens or generic-optics with 'taxDocuments' instead"  #-}

instance Core.FromJSON ClusterMetadata where
        parseJSON
          = Core.withObject "ClusterMetadata" Core.$
              \ x ->
                ClusterMetadata' Core.<$>
                  (x Core..:? "AddressId") Core.<*> x Core..:? "ClusterId" Core.<*>
                    x Core..:? "ClusterState"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "ForwardingAddressId"
                    Core.<*> x Core..:? "JobType"
                    Core.<*> x Core..:? "KmsKeyARN"
                    Core.<*> x Core..:? "Notification"
                    Core.<*> x Core..:? "Resources"
                    Core.<*> x Core..:? "RoleARN"
                    Core.<*> x Core..:? "ShippingOption"
                    Core.<*> x Core..:? "SnowballType"
                    Core.<*> x Core..:? "TaxDocuments"
