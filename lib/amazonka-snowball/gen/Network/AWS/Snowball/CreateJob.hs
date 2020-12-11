{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateJob (..),
    mkCreateJob,

    -- ** Request lenses
    cjJobType,
    cjKMSKeyARN,
    cjNotification,
    cjForwardingAddressId,
    cjAddressId,
    cjSnowballType,
    cjShippingOption,
    cjResources,
    cjClusterId,
    cjDeviceConfiguration,
    cjDescription,
    cjTaxDocuments,
    cjRoleARN,
    cjSnowballCapacityPreference,

    -- * Destructuring the response
    CreateJobResponse (..),
    mkCreateJobResponse,

    -- ** Response lenses
    cjrsJobId,
    cjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { jobType :: Lude.Maybe JobType,
    kmsKeyARN :: Lude.Maybe Lude.Text,
    notification :: Lude.Maybe Notification,
    forwardingAddressId :: Lude.Maybe Lude.Text,
    addressId :: Lude.Maybe Lude.Text,
    snowballType :: Lude.Maybe SnowballType,
    shippingOption :: Lude.Maybe ShippingOption,
    resources :: Lude.Maybe JobResource,
    clusterId :: Lude.Maybe Lude.Text,
    deviceConfiguration :: Lude.Maybe DeviceConfiguration,
    description :: Lude.Maybe Lude.Text,
    taxDocuments :: Lude.Maybe TaxDocuments,
    roleARN :: Lude.Maybe Lude.Text,
    snowballCapacityPreference :: Lude.Maybe SnowballCapacity
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- * 'addressId' - The ID for the address that you want the Snow device shipped to.
-- * 'clusterId' - The ID of a cluster. If you're creating a job for a node in a cluster, you need to provide only this @clusterId@ value. The other job attributes are inherited from the cluster.
-- * 'description' - Defines an optional description of this specific job, for example @Important Photos 2016-08-11@ .
-- * 'deviceConfiguration' - Defines the device configuration for an AWS Snowcone job.
-- * 'forwardingAddressId' - The forwarding address ID for a job. This field is not supported in most regions.
-- * 'jobType' - Defines the type of job that you're creating.
-- * 'kmsKeyARN' - The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@ s are created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> AWS Key Management Service (KMS) API action.
-- * 'notification' - Defines the Amazon Simple Notification Service (Amazon SNS) notification settings for this job.
-- * 'resources' - Defines the Amazon S3 buckets associated with this job.
--
-- With @IMPORT@ jobs, you specify the bucket or buckets that your transferred data will be imported into.
-- With @EXPORT@ jobs, you specify the bucket or buckets that your transferred data will be exported from. Optionally, you can also specify a @KeyRange@ value. If you choose to export a range, you define the length of the range by providing either an inclusive @BeginMarker@ value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary sorted.
-- * 'roleARN' - The @RoleARN@ that you want to associate with this job. @RoleArn@ s are created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
-- * 'shippingOption' - The shipping speed for this job. This speed doesn't dictate how soon you'll get the Snow device, rather it represents how quickly the Snow device moves to its destination while in transit. Regional shipping speeds are as follows:
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
-- * 'snowballCapacityPreference' - If your job is being created in one of the US regions, you have the option of specifying what size Snow device you'd like for this job. In all other regions, Snowballs come with 80 TB in storage capacity.
-- * 'snowballType' - The type of AWS Snow Family device to use for this job.
--
-- The type of AWS Snow device to use for this job. Currently, the only supported device type for cluster jobs is @EDGE@ .
-- For more information, see <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options> in the Snowball Edge Developer Guide.
-- * 'taxDocuments' - The tax documents required in your AWS Region.
mkCreateJob ::
  CreateJob
mkCreateJob =
  CreateJob'
    { jobType = Lude.Nothing,
      kmsKeyARN = Lude.Nothing,
      notification = Lude.Nothing,
      forwardingAddressId = Lude.Nothing,
      addressId = Lude.Nothing,
      snowballType = Lude.Nothing,
      shippingOption = Lude.Nothing,
      resources = Lude.Nothing,
      clusterId = Lude.Nothing,
      deviceConfiguration = Lude.Nothing,
      description = Lude.Nothing,
      taxDocuments = Lude.Nothing,
      roleARN = Lude.Nothing,
      snowballCapacityPreference = Lude.Nothing
    }

-- | Defines the type of job that you're creating.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobType :: Lens.Lens' CreateJob (Lude.Maybe JobType)
cjJobType = Lens.lens (jobType :: CreateJob -> Lude.Maybe JobType) (\s a -> s {jobType = a} :: CreateJob)
{-# DEPRECATED cjJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@ s are created using the <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey> AWS Key Management Service (KMS) API action.
--
-- /Note:/ Consider using 'kmsKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjKMSKeyARN :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjKMSKeyARN = Lens.lens (kmsKeyARN :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyARN = a} :: CreateJob)
{-# DEPRECATED cjKMSKeyARN "Use generic-lens or generic-optics with 'kmsKeyARN' instead." #-}

-- | Defines the Amazon Simple Notification Service (Amazon SNS) notification settings for this job.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjNotification :: Lens.Lens' CreateJob (Lude.Maybe Notification)
cjNotification = Lens.lens (notification :: CreateJob -> Lude.Maybe Notification) (\s a -> s {notification = a} :: CreateJob)
{-# DEPRECATED cjNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | The forwarding address ID for a job. This field is not supported in most regions.
--
-- /Note:/ Consider using 'forwardingAddressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjForwardingAddressId :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjForwardingAddressId = Lens.lens (forwardingAddressId :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {forwardingAddressId = a} :: CreateJob)
{-# DEPRECATED cjForwardingAddressId "Use generic-lens or generic-optics with 'forwardingAddressId' instead." #-}

-- | The ID for the address that you want the Snow device shipped to.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAddressId :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjAddressId = Lens.lens (addressId :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {addressId = a} :: CreateJob)
{-# DEPRECATED cjAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The type of AWS Snow Family device to use for this job.
--
-- The type of AWS Snow device to use for this job. Currently, the only supported device type for cluster jobs is @EDGE@ .
-- For more information, see <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options> in the Snowball Edge Developer Guide.
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjSnowballType :: Lens.Lens' CreateJob (Lude.Maybe SnowballType)
cjSnowballType = Lens.lens (snowballType :: CreateJob -> Lude.Maybe SnowballType) (\s a -> s {snowballType = a} :: CreateJob)
{-# DEPRECATED cjSnowballType "Use generic-lens or generic-optics with 'snowballType' instead." #-}

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
cjShippingOption :: Lens.Lens' CreateJob (Lude.Maybe ShippingOption)
cjShippingOption = Lens.lens (shippingOption :: CreateJob -> Lude.Maybe ShippingOption) (\s a -> s {shippingOption = a} :: CreateJob)
{-# DEPRECATED cjShippingOption "Use generic-lens or generic-optics with 'shippingOption' instead." #-}

-- | Defines the Amazon S3 buckets associated with this job.
--
-- With @IMPORT@ jobs, you specify the bucket or buckets that your transferred data will be imported into.
-- With @EXPORT@ jobs, you specify the bucket or buckets that your transferred data will be exported from. Optionally, you can also specify a @KeyRange@ value. If you choose to export a range, you define the length of the range by providing either an inclusive @BeginMarker@ value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary sorted.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjResources :: Lens.Lens' CreateJob (Lude.Maybe JobResource)
cjResources = Lens.lens (resources :: CreateJob -> Lude.Maybe JobResource) (\s a -> s {resources = a} :: CreateJob)
{-# DEPRECATED cjResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The ID of a cluster. If you're creating a job for a node in a cluster, you need to provide only this @clusterId@ value. The other job attributes are inherited from the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjClusterId :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjClusterId = Lens.lens (clusterId :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: CreateJob)
{-# DEPRECATED cjClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | Defines the device configuration for an AWS Snowcone job.
--
-- /Note:/ Consider using 'deviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDeviceConfiguration :: Lens.Lens' CreateJob (Lude.Maybe DeviceConfiguration)
cjDeviceConfiguration = Lens.lens (deviceConfiguration :: CreateJob -> Lude.Maybe DeviceConfiguration) (\s a -> s {deviceConfiguration = a} :: CreateJob)
{-# DEPRECATED cjDeviceConfiguration "Use generic-lens or generic-optics with 'deviceConfiguration' instead." #-}

-- | Defines an optional description of this specific job, for example @Important Photos 2016-08-11@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjDescription :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjDescription = Lens.lens (description :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateJob)
{-# DEPRECATED cjDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tax documents required in your AWS Region.
--
-- /Note:/ Consider using 'taxDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjTaxDocuments :: Lens.Lens' CreateJob (Lude.Maybe TaxDocuments)
cjTaxDocuments = Lens.lens (taxDocuments :: CreateJob -> Lude.Maybe TaxDocuments) (\s a -> s {taxDocuments = a} :: CreateJob)
{-# DEPRECATED cjTaxDocuments "Use generic-lens or generic-optics with 'taxDocuments' instead." #-}

-- | The @RoleARN@ that you want to associate with this job. @RoleArn@ s are created using the <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> AWS Identity and Access Management (IAM) API action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjRoleARN :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjRoleARN = Lens.lens (roleARN :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CreateJob)
{-# DEPRECATED cjRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | If your job is being created in one of the US regions, you have the option of specifying what size Snow device you'd like for this job. In all other regions, Snowballs come with 80 TB in storage capacity.
--
-- /Note:/ Consider using 'snowballCapacityPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjSnowballCapacityPreference :: Lens.Lens' CreateJob (Lude.Maybe SnowballCapacity)
cjSnowballCapacityPreference = Lens.lens (snowballCapacityPreference :: CreateJob -> Lude.Maybe SnowballCapacity) (\s a -> s {snowballCapacityPreference = a} :: CreateJob)
{-# DEPRECATED cjSnowballCapacityPreference "Use generic-lens or generic-optics with 'snowballCapacityPreference' instead." #-}

instance Lude.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Lude.<$> (x Lude..?> "JobId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSIESnowballJobManagementService.CreateJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobType" Lude..=) Lude.<$> jobType,
            ("KmsKeyARN" Lude..=) Lude.<$> kmsKeyARN,
            ("Notification" Lude..=) Lude.<$> notification,
            ("ForwardingAddressId" Lude..=) Lude.<$> forwardingAddressId,
            ("AddressId" Lude..=) Lude.<$> addressId,
            ("SnowballType" Lude..=) Lude.<$> snowballType,
            ("ShippingOption" Lude..=) Lude.<$> shippingOption,
            ("Resources" Lude..=) Lude.<$> resources,
            ("ClusterId" Lude..=) Lude.<$> clusterId,
            ("DeviceConfiguration" Lude..=) Lude.<$> deviceConfiguration,
            ("Description" Lude..=) Lude.<$> description,
            ("TaxDocuments" Lude..=) Lude.<$> taxDocuments,
            ("RoleARN" Lude..=) Lude.<$> roleARN,
            ("SnowballCapacityPreference" Lude..=)
              Lude.<$> snowballCapacityPreference
          ]
      )

instance Lude.ToPath CreateJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'responseStatus' - The response status code.
mkCreateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateJobResponse
mkCreateJobResponse pResponseStatus_ =
  CreateJobResponse'
    { jobId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsJobId :: Lens.Lens' CreateJobResponse (Lude.Maybe Lude.Text)
cjrsJobId = Lens.lens (jobId :: CreateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: CreateJobResponse)
{-# DEPRECATED cjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsResponseStatus :: Lens.Lens' CreateJobResponse Lude.Int
cjrsResponseStatus = Lens.lens (responseStatus :: CreateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateJobResponse)
{-# DEPRECATED cjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
