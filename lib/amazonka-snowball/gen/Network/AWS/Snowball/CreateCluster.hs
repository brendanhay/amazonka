{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty cluster. Each cluster supports five nodes. You use the 'CreateJob' action separately to create the jobs for each of these nodes. The cluster does not ship until these five node jobs have been created.
module Network.AWS.Snowball.CreateCluster
    (
    -- * Creating a request
      CreateCluster (..)
    , mkCreateCluster
    -- ** Request lenses
    , ccJobType
    , ccResources
    , ccAddressId
    , ccRoleARN
    , ccShippingOption
    , ccDescription
    , ccForwardingAddressId
    , ccKmsKeyARN
    , ccNotification
    , ccSnowballType
    , ccTaxDocuments

    -- * Destructuring the response
    , CreateClusterResponse (..)
    , mkCreateClusterResponse
    -- ** Response lenses
    , crsClusterId
    , crsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkCreateCluster' smart constructor.
data CreateCluster = CreateCluster'
  { jobType :: Types.JobType
    -- ^ The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
  , resources :: Types.JobResource
    -- ^ The resources associated with the cluster job. These resources include Amazon S3 buckets and optional AWS Lambda functions written in the Python language. 
  , addressId :: Types.AddressId
    -- ^ The ID for the address that you want the cluster shipped to.
  , roleARN :: Types.RoleARN
    -- ^ The @RoleARN@ that you want to associate with this cluster. @RoleArn@ values are created by using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
  , shippingOption :: Types.ShippingOption
    -- ^ The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each Snowball Edge device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows: 
--
--
--     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow device are delivered in one to seven days.
--
--
--     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
--
--
--
--     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow device are delivered in one to seven days.
--
--
--     * In the US, you have access to one-day shipping and two-day shipping.
--
--
  , description :: Core.Maybe Core.Text
    -- ^ An optional description of this specific cluster, for example @Environmental Data Cluster-01@ .
  , forwardingAddressId :: Core.Maybe Types.ForwardingAddressId
    -- ^ The forwarding address ID for a cluster. This field is not supported in most regions.
  , kmsKeyARN :: Core.Maybe Types.KmsKeyARN
    -- ^ The @KmsKeyARN@ value that you want to associate with this cluster. @KmsKeyARN@ values are created by using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS). 
  , notification :: Core.Maybe Types.Notification
    -- ^ The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
  , snowballType :: Core.Maybe Types.SnowballType
    -- ^ The type of AWS Snow Family device to use for this cluster. 
  , taxDocuments :: Core.Maybe Types.TaxDocuments
    -- ^ The tax documents required in your AWS Region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCluster' value with any optional fields omitted.
mkCreateCluster
    :: Types.JobType -- ^ 'jobType'
    -> Types.JobResource -- ^ 'resources'
    -> Types.AddressId -- ^ 'addressId'
    -> Types.RoleARN -- ^ 'roleARN'
    -> Types.ShippingOption -- ^ 'shippingOption'
    -> CreateCluster
mkCreateCluster jobType resources addressId roleARN shippingOption
  = CreateCluster'{jobType, resources, addressId, roleARN,
                   shippingOption, description = Core.Nothing,
                   forwardingAddressId = Core.Nothing, kmsKeyARN = Core.Nothing,
                   notification = Core.Nothing, snowballType = Core.Nothing,
                   taxDocuments = Core.Nothing}

-- | The type of job for this cluster. Currently, the only job type supported for clusters is @LOCAL_USE@ .
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccJobType :: Lens.Lens' CreateCluster Types.JobType
ccJobType = Lens.field @"jobType"
{-# INLINEABLE ccJobType #-}
{-# DEPRECATED jobType "Use generic-lens or generic-optics with 'jobType' instead"  #-}

-- | The resources associated with the cluster job. These resources include Amazon S3 buckets and optional AWS Lambda functions written in the Python language. 
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccResources :: Lens.Lens' CreateCluster Types.JobResource
ccResources = Lens.field @"resources"
{-# INLINEABLE ccResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | The ID for the address that you want the cluster shipped to.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAddressId :: Lens.Lens' CreateCluster Types.AddressId
ccAddressId = Lens.field @"addressId"
{-# INLINEABLE ccAddressId #-}
{-# DEPRECATED addressId "Use generic-lens or generic-optics with 'addressId' instead"  #-}

-- | The @RoleARN@ that you want to associate with this cluster. @RoleArn@ values are created by using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRoleARN :: Lens.Lens' CreateCluster Types.RoleARN
ccRoleARN = Lens.field @"roleARN"
{-# INLINEABLE ccRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The shipping speed for each node in this cluster. This speed doesn't dictate how soon you'll get each Snowball Edge device, rather it represents how quickly each device moves to its destination while in transit. Regional shipping speeds are as follows: 
--
--
--     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow device are delivered in one to seven days.
--
--
--     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
--
--
--
--     * In Australia, you have access to express shipping. Typically, devices shipped express are delivered in about a day.
--
--
--     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.
--
--
--     * In India, Snow device are delivered in one to seven days.
--
--
--     * In the US, you have access to one-day shipping and two-day shipping.
--
--
--
-- /Note:/ Consider using 'shippingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccShippingOption :: Lens.Lens' CreateCluster Types.ShippingOption
ccShippingOption = Lens.field @"shippingOption"
{-# INLINEABLE ccShippingOption #-}
{-# DEPRECATED shippingOption "Use generic-lens or generic-optics with 'shippingOption' instead"  #-}

-- | An optional description of this specific cluster, for example @Environmental Data Cluster-01@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccDescription :: Lens.Lens' CreateCluster (Core.Maybe Core.Text)
ccDescription = Lens.field @"description"
{-# INLINEABLE ccDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The forwarding address ID for a cluster. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccForwardingAddressId :: Lens.Lens' CreateCluster (Core.Maybe Types.ForwardingAddressId)
ccForwardingAddressId = Lens.field @"forwardingAddressId"
{-# INLINEABLE ccForwardingAddressId #-}
{-# DEPRECATED forwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead"  #-}

-- | The @KmsKeyARN@ value that you want to associate with this cluster. @KmsKeyARN@ values are created by using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> API action in AWS Key Management Service (AWS KMS). 
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccKmsKeyARN :: Lens.Lens' CreateCluster (Core.Maybe Types.KmsKeyARN)
ccKmsKeyARN = Lens.field @"kmsKeyARN"
{-# INLINEABLE ccKmsKeyARN #-}
{-# DEPRECATED kmsKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead"  #-}

-- | The Amazon Simple Notification Service (Amazon SNS) notification settings for this cluster.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNotification :: Lens.Lens' CreateCluster (Core.Maybe Types.Notification)
ccNotification = Lens.field @"notification"
{-# INLINEABLE ccNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | The type of AWS Snow Family device to use for this cluster. 
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSnowballType :: Lens.Lens' CreateCluster (Core.Maybe Types.SnowballType)
ccSnowballType = Lens.field @"snowballType"
{-# INLINEABLE ccSnowballType #-}
{-# DEPRECATED snowballType "Use generic-lens or generic-optics with 'snowballType' instead"  #-}

-- | The tax documents required in your AWS Region.
--
-- /Note:/ Consider using 'taxDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTaxDocuments :: Lens.Lens' CreateCluster (Core.Maybe Types.TaxDocuments)
ccTaxDocuments = Lens.field @"taxDocuments"
{-# INLINEABLE ccTaxDocuments #-}
{-# DEPRECATED taxDocuments "Use generic-lens or generic-optics with 'taxDocuments' instead"  #-}

instance Core.ToQuery CreateCluster where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateCluster where
        toHeaders CreateCluster{..}
          = Core.pure
              ("X-Amz-Target", "AWSIESnowballJobManagementService.CreateCluster")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateCluster where
        toJSON CreateCluster{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobType" Core..= jobType),
                  Core.Just ("Resources" Core..= resources),
                  Core.Just ("AddressId" Core..= addressId),
                  Core.Just ("RoleARN" Core..= roleARN),
                  Core.Just ("ShippingOption" Core..= shippingOption),
                  ("Description" Core..=) Core.<$> description,
                  ("ForwardingAddressId" Core..=) Core.<$> forwardingAddressId,
                  ("KmsKeyARN" Core..=) Core.<$> kmsKeyARN,
                  ("Notification" Core..=) Core.<$> notification,
                  ("SnowballType" Core..=) Core.<$> snowballType,
                  ("TaxDocuments" Core..=) Core.<$> taxDocuments])

instance Core.AWSRequest CreateCluster where
        type Rs CreateCluster = CreateClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateClusterResponse' Core.<$>
                   (x Core..:? "ClusterId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { clusterId :: Core.Maybe Types.ClusterId
    -- ^ The automatically generated ID for a cluster.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateClusterResponse' value with any optional fields omitted.
mkCreateClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateClusterResponse
mkCreateClusterResponse responseStatus
  = CreateClusterResponse'{clusterId = Core.Nothing, responseStatus}

-- | The automatically generated ID for a cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsClusterId :: Lens.Lens' CreateClusterResponse (Core.Maybe Types.ClusterId)
crsClusterId = Lens.field @"clusterId"
{-# INLINEABLE crsClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateClusterResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
