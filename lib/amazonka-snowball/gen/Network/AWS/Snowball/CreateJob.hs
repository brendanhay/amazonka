{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job to import or export data between Amazon S3 and your on-premises data center. Your AWS account must have the right trust policies and permissions in place to create a job for a Snow device. If you're creating a job for a node in a cluster, you only need to provide the @clusterId@ value; the other job attributes are inherited from the cluster. 
module Network.AWS.Snowball.CreateJob
    (
    -- * Creating a request
      CreateJob (..)
    , mkCreateJob
    -- ** Request lenses
    , cjAddressId
    , cjClusterId
    , cjDescription
    , cjDeviceConfiguration
    , cjForwardingAddressId
    , cjJobType
    , cjKmsKeyARN
    , cjNotification
    , cjResources
    , cjRoleARN
    , cjShippingOption
    , cjSnowballCapacityPreference
    , cjSnowballType
    , cjTaxDocuments

    -- * Destructuring the response
    , CreateJobResponse (..)
    , mkCreateJobResponse
    -- ** Response lenses
    , cjrrsJobId
    , cjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { addressId :: Core.Maybe Types.AddressId
    -- ^ The ID for the address that you want the Snow device shipped to.
  , clusterId :: Core.Maybe Types.ClusterId
    -- ^ The ID of a cluster. If you're creating a job for a node in a cluster, you need to provide only this @clusterId@ value. The other job attributes are inherited from the cluster.
  , description :: Core.Maybe Core.Text
    -- ^ Defines an optional description of this specific job, for example @Important Photos 2016-08-11@ .
  , deviceConfiguration :: Core.Maybe Types.DeviceConfiguration
    -- ^ Defines the device configuration for an AWS Snowcone job.
  , forwardingAddressId :: Core.Maybe Types.ForwardingAddressId
    -- ^ The forwarding address ID for a job. This field is not supported in most regions.
  , jobType :: Core.Maybe Types.JobType
    -- ^ Defines the type of job that you're creating. 
  , kmsKeyARN :: Core.Maybe Types.KmsKeyARN
    -- ^ The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@ s are created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> AWS Key Management Service (KMS) API action.
  , notification :: Core.Maybe Types.Notification
    -- ^ Defines the Amazon Simple Notification Service (Amazon SNS) notification settings for this job.
  , resources :: Core.Maybe Types.JobResource
    -- ^ Defines the Amazon S3 buckets associated with this job.
--
-- With @IMPORT@ jobs, you specify the bucket or buckets that your transferred data will be imported into.
-- With @EXPORT@ jobs, you specify the bucket or buckets that your transferred data will be exported from. Optionally, you can also specify a @KeyRange@ value. If you choose to export a range, you define the length of the range by providing either an inclusive @BeginMarker@ value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary sorted.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ The @RoleARN@ that you want to associate with this job. @RoleArn@ s are created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
  , shippingOption :: Core.Maybe Types.ShippingOption
    -- ^ The shipping speed for this job. This speed doesn't dictate how soon you'll get the Snow device, rather it represents how quickly the Snow device moves to its destination while in transit. Regional shipping speeds are as follows:
--
--
--     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.
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
  , snowballCapacityPreference :: Core.Maybe Types.SnowballCapacity
    -- ^ If your job is being created in one of the US regions, you have the option of specifying what size Snow device you'd like for this job. In all other regions, Snowballs come with 80 TB in storage capacity.
  , snowballType :: Core.Maybe Types.SnowballType
    -- ^ The type of AWS Snow Family device to use for this job. 
--
-- The type of AWS Snow device to use for this job. Currently, the only supported device type for cluster jobs is @EDGE@ .
-- For more information, see <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options> in the Snowball Edge Developer Guide.
  , taxDocuments :: Core.Maybe Types.TaxDocuments
    -- ^ The tax documents required in your AWS Region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJob' value with any optional fields omitted.
mkCreateJob
    :: CreateJob
mkCreateJob
  = CreateJob'{addressId = Core.Nothing, clusterId = Core.Nothing,
               description = Core.Nothing, deviceConfiguration = Core.Nothing,
               forwardingAddressId = Core.Nothing, jobType = Core.Nothing,
               kmsKeyARN = Core.Nothing, notification = Core.Nothing,
               resources = Core.Nothing, roleARN = Core.Nothing,
               shippingOption = Core.Nothing,
               snowballCapacityPreference = Core.Nothing,
               snowballType = Core.Nothing, taxDocuments = Core.Nothing}

-- | The ID for the address that you want the Snow device shipped to.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAddressId :: Lens.Lens' CreateJob (Core.Maybe Types.AddressId)
cjAddressId = Lens.field @"addressId"
{-# INLINEABLE cjAddressId #-}
{-# DEPRECATED addressId "Use generic-lens or generic-optics with 'addressId' instead"  #-}

-- | The ID of a cluster. If you're creating a job for a node in a cluster, you need to provide only this @clusterId@ value. The other job attributes are inherited from the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjClusterId :: Lens.Lens' CreateJob (Core.Maybe Types.ClusterId)
cjClusterId = Lens.field @"clusterId"
{-# INLINEABLE cjClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | Defines an optional description of this specific job, for example @Important Photos 2016-08-11@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDescription :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
cjDescription = Lens.field @"description"
{-# INLINEABLE cjDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Defines the device configuration for an AWS Snowcone job.
--
-- /Note:/ Consider using 'deviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDeviceConfiguration :: Lens.Lens' CreateJob (Core.Maybe Types.DeviceConfiguration)
cjDeviceConfiguration = Lens.field @"deviceConfiguration"
{-# INLINEABLE cjDeviceConfiguration #-}
{-# DEPRECATED deviceConfiguration "Use generic-lens or generic-optics with 'deviceConfiguration' instead"  #-}

-- | The forwarding address ID for a job. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjForwardingAddressId :: Lens.Lens' CreateJob (Core.Maybe Types.ForwardingAddressId)
cjForwardingAddressId = Lens.field @"forwardingAddressId"
{-# INLINEABLE cjForwardingAddressId #-}
{-# DEPRECATED forwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead"  #-}

-- | Defines the type of job that you're creating. 
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobType :: Lens.Lens' CreateJob (Core.Maybe Types.JobType)
cjJobType = Lens.field @"jobType"
{-# INLINEABLE cjJobType #-}
{-# DEPRECATED jobType "Use generic-lens or generic-optics with 'jobType' instead"  #-}

-- | The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@ s are created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> AWS Key Management Service (KMS) API action.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjKmsKeyARN :: Lens.Lens' CreateJob (Core.Maybe Types.KmsKeyARN)
cjKmsKeyARN = Lens.field @"kmsKeyARN"
{-# INLINEABLE cjKmsKeyARN #-}
{-# DEPRECATED kmsKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead"  #-}

-- | Defines the Amazon Simple Notification Service (Amazon SNS) notification settings for this job.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjNotification :: Lens.Lens' CreateJob (Core.Maybe Types.Notification)
cjNotification = Lens.field @"notification"
{-# INLINEABLE cjNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | Defines the Amazon S3 buckets associated with this job.
--
-- With @IMPORT@ jobs, you specify the bucket or buckets that your transferred data will be imported into.
-- With @EXPORT@ jobs, you specify the bucket or buckets that your transferred data will be exported from. Optionally, you can also specify a @KeyRange@ value. If you choose to export a range, you define the length of the range by providing either an inclusive @BeginMarker@ value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary sorted.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjResources :: Lens.Lens' CreateJob (Core.Maybe Types.JobResource)
cjResources = Lens.field @"resources"
{-# INLINEABLE cjResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | The @RoleARN@ that you want to associate with this job. @RoleArn@ s are created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjRoleARN :: Lens.Lens' CreateJob (Core.Maybe Types.RoleARN)
cjRoleARN = Lens.field @"roleARN"
{-# INLINEABLE cjRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | The shipping speed for this job. This speed doesn't dictate how soon you'll get the Snow device, rather it represents how quickly the Snow device moves to its destination while in transit. Regional shipping speeds are as follows:
--
--
--     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.
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
cjShippingOption :: Lens.Lens' CreateJob (Core.Maybe Types.ShippingOption)
cjShippingOption = Lens.field @"shippingOption"
{-# INLINEABLE cjShippingOption #-}
{-# DEPRECATED shippingOption "Use generic-lens or generic-optics with 'shippingOption' instead"  #-}

-- | If your job is being created in one of the US regions, you have the option of specifying what size Snow device you'd like for this job. In all other regions, Snowballs come with 80 TB in storage capacity.
--
-- /Note:/ Consider using 'snowballCapacityPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjSnowballCapacityPreference :: Lens.Lens' CreateJob (Core.Maybe Types.SnowballCapacity)
cjSnowballCapacityPreference = Lens.field @"snowballCapacityPreference"
{-# INLINEABLE cjSnowballCapacityPreference #-}
{-# DEPRECATED snowballCapacityPreference "Use generic-lens or generic-optics with 'snowballCapacityPreference' instead"  #-}

-- | The type of AWS Snow Family device to use for this job. 
--
-- The type of AWS Snow device to use for this job. Currently, the only supported device type for cluster jobs is @EDGE@ .
-- For more information, see <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options> in the Snowball Edge Developer Guide.
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjSnowballType :: Lens.Lens' CreateJob (Core.Maybe Types.SnowballType)
cjSnowballType = Lens.field @"snowballType"
{-# INLINEABLE cjSnowballType #-}
{-# DEPRECATED snowballType "Use generic-lens or generic-optics with 'snowballType' instead"  #-}

-- | The tax documents required in your AWS Region.
--
-- /Note:/ Consider using 'taxDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTaxDocuments :: Lens.Lens' CreateJob (Core.Maybe Types.TaxDocuments)
cjTaxDocuments = Lens.field @"taxDocuments"
{-# INLINEABLE cjTaxDocuments #-}
{-# DEPRECATED taxDocuments "Use generic-lens or generic-optics with 'taxDocuments' instead"  #-}

instance Core.ToQuery CreateJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateJob where
        toHeaders CreateJob{..}
          = Core.pure
              ("X-Amz-Target", "AWSIESnowballJobManagementService.CreateJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateJob where
        toJSON CreateJob{..}
          = Core.object
              (Core.catMaybes
                 [("AddressId" Core..=) Core.<$> addressId,
                  ("ClusterId" Core..=) Core.<$> clusterId,
                  ("Description" Core..=) Core.<$> description,
                  ("DeviceConfiguration" Core..=) Core.<$> deviceConfiguration,
                  ("ForwardingAddressId" Core..=) Core.<$> forwardingAddressId,
                  ("JobType" Core..=) Core.<$> jobType,
                  ("KmsKeyARN" Core..=) Core.<$> kmsKeyARN,
                  ("Notification" Core..=) Core.<$> notification,
                  ("Resources" Core..=) Core.<$> resources,
                  ("RoleARN" Core..=) Core.<$> roleARN,
                  ("ShippingOption" Core..=) Core.<$> shippingOption,
                  ("SnowballCapacityPreference" Core..=) Core.<$>
                    snowballCapacityPreference,
                  ("SnowballType" Core..=) Core.<$> snowballType,
                  ("TaxDocuments" Core..=) Core.<$> taxDocuments])

instance Core.AWSRequest CreateJob where
        type Rs CreateJob = CreateJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateJobResponse' Core.<$>
                   (x Core..:? "JobId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { jobId :: Core.Maybe Types.JobId
    -- ^ The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobResponse' value with any optional fields omitted.
mkCreateJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateJobResponse
mkCreateJobResponse responseStatus
  = CreateJobResponse'{jobId = Core.Nothing, responseStatus}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsJobId :: Lens.Lens' CreateJobResponse (Core.Maybe Types.JobId)
cjrrsJobId = Lens.field @"jobId"
{-# INLINEABLE cjrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsResponseStatus :: Lens.Lens' CreateJobResponse Core.Int
cjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
