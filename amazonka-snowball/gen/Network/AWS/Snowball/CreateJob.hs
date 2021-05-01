{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.CreateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job to import or export data between Amazon S3 and your
-- on-premises data center. Your AWS account must have the right trust
-- policies and permissions in place to create a job for a Snow device. If
-- you\'re creating a job for a node in a cluster, you only need to provide
-- the @clusterId@ value; the other job attributes are inherited from the
-- cluster.
module Network.AWS.Snowball.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_clusterId,
    createJob_roleARN,
    createJob_shippingOption,
    createJob_deviceConfiguration,
    createJob_kmsKeyARN,
    createJob_jobType,
    createJob_resources,
    createJob_taxDocuments,
    createJob_snowballCapacityPreference,
    createJob_snowballType,
    createJob_description,
    createJob_addressId,
    createJob_forwardingAddressId,
    createJob_notification,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_jobId,
    createJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | The ID of a cluster. If you\'re creating a job for a node in a cluster,
    -- you need to provide only this @clusterId@ value. The other job
    -- attributes are inherited from the cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The @RoleARN@ that you want to associate with this job. @RoleArn@s are
    -- created using the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
    -- AWS Identity and Access Management (IAM) API action.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The shipping speed for this job. This speed doesn\'t dictate how soon
    -- you\'ll get the Snow device, rather it represents how quickly the Snow
    -- device moves to its destination while in transit. Regional shipping
    -- speeds are as follows:
    --
    -- -   In Australia, you have access to express shipping. Typically, Snow
    --     devices shipped express are delivered in about a day.
    --
    -- -   In the European Union (EU), you have access to express shipping.
    --     Typically, Snow devices shipped express are delivered in about a
    --     day. In addition, most countries in the EU have access to standard
    --     shipping, which typically takes less than a week, one way.
    --
    -- -   In India, Snow devices are delivered in one to seven days.
    --
    -- -   In the US, you have access to one-day shipping and two-day shipping.
    shippingOption :: Prelude.Maybe ShippingOption,
    -- | Defines the device configuration for an AWS Snowcone job.
    deviceConfiguration :: Prelude.Maybe DeviceConfiguration,
    -- | The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@s
    -- are created using the
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
    -- AWS Key Management Service (KMS) API action.
    kmsKeyARN :: Prelude.Maybe Prelude.Text,
    -- | Defines the type of job that you\'re creating.
    jobType :: Prelude.Maybe JobType,
    -- | Defines the Amazon S3 buckets associated with this job.
    --
    -- With @IMPORT@ jobs, you specify the bucket or buckets that your
    -- transferred data will be imported into.
    --
    -- With @EXPORT@ jobs, you specify the bucket or buckets that your
    -- transferred data will be exported from. Optionally, you can also specify
    -- a @KeyRange@ value. If you choose to export a range, you define the
    -- length of the range by providing either an inclusive @BeginMarker@
    -- value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary
    -- sorted.
    resources :: Prelude.Maybe JobResource,
    -- | The tax documents required in your AWS Region.
    taxDocuments :: Prelude.Maybe TaxDocuments,
    -- | If your job is being created in one of the US regions, you have the
    -- option of specifying what size Snow device you\'d like for this job. In
    -- all other regions, Snowballs come with 80 TB in storage capacity.
    snowballCapacityPreference :: Prelude.Maybe SnowballCapacity,
    -- | The type of AWS Snow Family device to use for this job.
    --
    -- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
    -- device type.
    --
    -- The type of AWS Snow device to use for this job. Currently, the only
    -- supported device type for cluster jobs is @EDGE@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options>
    -- in the Snowball Edge Developer Guide.
    snowballType :: Prelude.Maybe SnowballType,
    -- | Defines an optional description of this specific job, for example
    -- @Important Photos 2016-08-11@.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID for the address that you want the Snow device shipped to.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The forwarding address ID for a job. This field is not supported in most
    -- regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | Defines the Amazon Simple Notification Service (Amazon SNS) notification
    -- settings for this job.
    notification :: Prelude.Maybe Notification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'createJob_clusterId' - The ID of a cluster. If you\'re creating a job for a node in a cluster,
-- you need to provide only this @clusterId@ value. The other job
-- attributes are inherited from the cluster.
--
-- 'roleARN', 'createJob_roleARN' - The @RoleARN@ that you want to associate with this job. @RoleArn@s are
-- created using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- AWS Identity and Access Management (IAM) API action.
--
-- 'shippingOption', 'createJob_shippingOption' - The shipping speed for this job. This speed doesn\'t dictate how soon
-- you\'ll get the Snow device, rather it represents how quickly the Snow
-- device moves to its destination while in transit. Regional shipping
-- speeds are as follows:
--
-- -   In Australia, you have access to express shipping. Typically, Snow
--     devices shipped express are delivered in about a day.
--
-- -   In the European Union (EU), you have access to express shipping.
--     Typically, Snow devices shipped express are delivered in about a
--     day. In addition, most countries in the EU have access to standard
--     shipping, which typically takes less than a week, one way.
--
-- -   In India, Snow devices are delivered in one to seven days.
--
-- -   In the US, you have access to one-day shipping and two-day shipping.
--
-- 'deviceConfiguration', 'createJob_deviceConfiguration' - Defines the device configuration for an AWS Snowcone job.
--
-- 'kmsKeyARN', 'createJob_kmsKeyARN' - The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@s
-- are created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- AWS Key Management Service (KMS) API action.
--
-- 'jobType', 'createJob_jobType' - Defines the type of job that you\'re creating.
--
-- 'resources', 'createJob_resources' - Defines the Amazon S3 buckets associated with this job.
--
-- With @IMPORT@ jobs, you specify the bucket or buckets that your
-- transferred data will be imported into.
--
-- With @EXPORT@ jobs, you specify the bucket or buckets that your
-- transferred data will be exported from. Optionally, you can also specify
-- a @KeyRange@ value. If you choose to export a range, you define the
-- length of the range by providing either an inclusive @BeginMarker@
-- value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary
-- sorted.
--
-- 'taxDocuments', 'createJob_taxDocuments' - The tax documents required in your AWS Region.
--
-- 'snowballCapacityPreference', 'createJob_snowballCapacityPreference' - If your job is being created in one of the US regions, you have the
-- option of specifying what size Snow device you\'d like for this job. In
-- all other regions, Snowballs come with 80 TB in storage capacity.
--
-- 'snowballType', 'createJob_snowballType' - The type of AWS Snow Family device to use for this job.
--
-- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
-- device type.
--
-- The type of AWS Snow device to use for this job. Currently, the only
-- supported device type for cluster jobs is @EDGE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options>
-- in the Snowball Edge Developer Guide.
--
-- 'description', 'createJob_description' - Defines an optional description of this specific job, for example
-- @Important Photos 2016-08-11@.
--
-- 'addressId', 'createJob_addressId' - The ID for the address that you want the Snow device shipped to.
--
-- 'forwardingAddressId', 'createJob_forwardingAddressId' - The forwarding address ID for a job. This field is not supported in most
-- regions.
--
-- 'notification', 'createJob_notification' - Defines the Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this job.
newCreateJob ::
  CreateJob
newCreateJob =
  CreateJob'
    { clusterId = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      shippingOption = Prelude.Nothing,
      deviceConfiguration = Prelude.Nothing,
      kmsKeyARN = Prelude.Nothing,
      jobType = Prelude.Nothing,
      resources = Prelude.Nothing,
      taxDocuments = Prelude.Nothing,
      snowballCapacityPreference = Prelude.Nothing,
      snowballType = Prelude.Nothing,
      description = Prelude.Nothing,
      addressId = Prelude.Nothing,
      forwardingAddressId = Prelude.Nothing,
      notification = Prelude.Nothing
    }

-- | The ID of a cluster. If you\'re creating a job for a node in a cluster,
-- you need to provide only this @clusterId@ value. The other job
-- attributes are inherited from the cluster.
createJob_clusterId :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_clusterId = Lens.lens (\CreateJob' {clusterId} -> clusterId) (\s@CreateJob' {} a -> s {clusterId = a} :: CreateJob)

-- | The @RoleARN@ that you want to associate with this job. @RoleArn@s are
-- created using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>
-- AWS Identity and Access Management (IAM) API action.
createJob_roleARN :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_roleARN = Lens.lens (\CreateJob' {roleARN} -> roleARN) (\s@CreateJob' {} a -> s {roleARN = a} :: CreateJob)

-- | The shipping speed for this job. This speed doesn\'t dictate how soon
-- you\'ll get the Snow device, rather it represents how quickly the Snow
-- device moves to its destination while in transit. Regional shipping
-- speeds are as follows:
--
-- -   In Australia, you have access to express shipping. Typically, Snow
--     devices shipped express are delivered in about a day.
--
-- -   In the European Union (EU), you have access to express shipping.
--     Typically, Snow devices shipped express are delivered in about a
--     day. In addition, most countries in the EU have access to standard
--     shipping, which typically takes less than a week, one way.
--
-- -   In India, Snow devices are delivered in one to seven days.
--
-- -   In the US, you have access to one-day shipping and two-day shipping.
createJob_shippingOption :: Lens.Lens' CreateJob (Prelude.Maybe ShippingOption)
createJob_shippingOption = Lens.lens (\CreateJob' {shippingOption} -> shippingOption) (\s@CreateJob' {} a -> s {shippingOption = a} :: CreateJob)

-- | Defines the device configuration for an AWS Snowcone job.
createJob_deviceConfiguration :: Lens.Lens' CreateJob (Prelude.Maybe DeviceConfiguration)
createJob_deviceConfiguration = Lens.lens (\CreateJob' {deviceConfiguration} -> deviceConfiguration) (\s@CreateJob' {} a -> s {deviceConfiguration = a} :: CreateJob)

-- | The @KmsKeyARN@ that you want to associate with this job. @KmsKeyARN@s
-- are created using the
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateKey.html CreateKey>
-- AWS Key Management Service (KMS) API action.
createJob_kmsKeyARN :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_kmsKeyARN = Lens.lens (\CreateJob' {kmsKeyARN} -> kmsKeyARN) (\s@CreateJob' {} a -> s {kmsKeyARN = a} :: CreateJob)

-- | Defines the type of job that you\'re creating.
createJob_jobType :: Lens.Lens' CreateJob (Prelude.Maybe JobType)
createJob_jobType = Lens.lens (\CreateJob' {jobType} -> jobType) (\s@CreateJob' {} a -> s {jobType = a} :: CreateJob)

-- | Defines the Amazon S3 buckets associated with this job.
--
-- With @IMPORT@ jobs, you specify the bucket or buckets that your
-- transferred data will be imported into.
--
-- With @EXPORT@ jobs, you specify the bucket or buckets that your
-- transferred data will be exported from. Optionally, you can also specify
-- a @KeyRange@ value. If you choose to export a range, you define the
-- length of the range by providing either an inclusive @BeginMarker@
-- value, an inclusive @EndMarker@ value, or both. Ranges are UTF-8 binary
-- sorted.
createJob_resources :: Lens.Lens' CreateJob (Prelude.Maybe JobResource)
createJob_resources = Lens.lens (\CreateJob' {resources} -> resources) (\s@CreateJob' {} a -> s {resources = a} :: CreateJob)

-- | The tax documents required in your AWS Region.
createJob_taxDocuments :: Lens.Lens' CreateJob (Prelude.Maybe TaxDocuments)
createJob_taxDocuments = Lens.lens (\CreateJob' {taxDocuments} -> taxDocuments) (\s@CreateJob' {} a -> s {taxDocuments = a} :: CreateJob)

-- | If your job is being created in one of the US regions, you have the
-- option of specifying what size Snow device you\'d like for this job. In
-- all other regions, Snowballs come with 80 TB in storage capacity.
createJob_snowballCapacityPreference :: Lens.Lens' CreateJob (Prelude.Maybe SnowballCapacity)
createJob_snowballCapacityPreference = Lens.lens (\CreateJob' {snowballCapacityPreference} -> snowballCapacityPreference) (\s@CreateJob' {} a -> s {snowballCapacityPreference = a} :: CreateJob)

-- | The type of AWS Snow Family device to use for this job.
--
-- For cluster jobs, AWS Snow Family currently supports only the @EDGE@
-- device type.
--
-- The type of AWS Snow device to use for this job. Currently, the only
-- supported device type for cluster jobs is @EDGE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/snowball/latest/developer-guide/device-differences.html Snowball Edge Device Options>
-- in the Snowball Edge Developer Guide.
createJob_snowballType :: Lens.Lens' CreateJob (Prelude.Maybe SnowballType)
createJob_snowballType = Lens.lens (\CreateJob' {snowballType} -> snowballType) (\s@CreateJob' {} a -> s {snowballType = a} :: CreateJob)

-- | Defines an optional description of this specific job, for example
-- @Important Photos 2016-08-11@.
createJob_description :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_description = Lens.lens (\CreateJob' {description} -> description) (\s@CreateJob' {} a -> s {description = a} :: CreateJob)

-- | The ID for the address that you want the Snow device shipped to.
createJob_addressId :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_addressId = Lens.lens (\CreateJob' {addressId} -> addressId) (\s@CreateJob' {} a -> s {addressId = a} :: CreateJob)

-- | The forwarding address ID for a job. This field is not supported in most
-- regions.
createJob_forwardingAddressId :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_forwardingAddressId = Lens.lens (\CreateJob' {forwardingAddressId} -> forwardingAddressId) (\s@CreateJob' {} a -> s {forwardingAddressId = a} :: CreateJob)

-- | Defines the Amazon Simple Notification Service (Amazon SNS) notification
-- settings for this job.
createJob_notification :: Lens.Lens' CreateJob (Prelude.Maybe Notification)
createJob_notification = Lens.lens (\CreateJob' {notification} -> notification) (\s@CreateJob' {} a -> s {notification = a} :: CreateJob)

instance Prelude.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Prelude.<$> (x Prelude..?> "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJob

instance Prelude.NFData CreateJob

instance Prelude.ToHeaders CreateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.CreateJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ClusterId" Prelude..=) Prelude.<$> clusterId,
            ("RoleARN" Prelude..=) Prelude.<$> roleARN,
            ("ShippingOption" Prelude..=)
              Prelude.<$> shippingOption,
            ("DeviceConfiguration" Prelude..=)
              Prelude.<$> deviceConfiguration,
            ("KmsKeyARN" Prelude..=) Prelude.<$> kmsKeyARN,
            ("JobType" Prelude..=) Prelude.<$> jobType,
            ("Resources" Prelude..=) Prelude.<$> resources,
            ("TaxDocuments" Prelude..=) Prelude.<$> taxDocuments,
            ("SnowballCapacityPreference" Prelude..=)
              Prelude.<$> snowballCapacityPreference,
            ("SnowballType" Prelude..=) Prelude.<$> snowballType,
            ("Description" Prelude..=) Prelude.<$> description,
            ("AddressId" Prelude..=) Prelude.<$> addressId,
            ("ForwardingAddressId" Prelude..=)
              Prelude.<$> forwardingAddressId,
            ("Notification" Prelude..=)
              Prelude.<$> notification
          ]
      )

instance Prelude.ToPath CreateJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'createJobResponse_jobId' - The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
newCreateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
createJobResponse_jobId :: Lens.Lens' CreateJobResponse (Prelude.Maybe Prelude.Text)
createJobResponse_jobId = Lens.lens (\CreateJobResponse' {jobId} -> jobId) (\s@CreateJobResponse' {} a -> s {jobId = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse
