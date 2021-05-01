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
-- Module      : Network.AWS.Snowball.UpdateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- While a job\'s @JobState@ value is @New@, you can update some of the
-- information associated with a job. Once the job changes to a different
-- job state, usually within 60 minutes of the job being created, this
-- action is no longer available.
module Network.AWS.Snowball.UpdateJob
  ( -- * Creating a Request
    UpdateJob (..),
    newUpdateJob,

    -- * Request Lenses
    updateJob_roleARN,
    updateJob_shippingOption,
    updateJob_resources,
    updateJob_snowballCapacityPreference,
    updateJob_description,
    updateJob_addressId,
    updateJob_forwardingAddressId,
    updateJob_notification,
    updateJob_jobId,

    -- * Destructuring the Response
    UpdateJobResponse (..),
    newUpdateJobResponse,

    -- * Response Lenses
    updateJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { -- | The new role Amazon Resource Name (ARN) that you want to associate with
    -- this job. To create a role ARN, use the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>AWS
    -- Identity and Access Management (IAM) API action.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The updated shipping option value of this job\'s ShippingDetails object.
    shippingOption :: Prelude.Maybe ShippingOption,
    -- | The updated @JobResource@ object, or the updated JobResource object.
    resources :: Prelude.Maybe JobResource,
    -- | The updated @SnowballCapacityPreference@ of this job\'s JobMetadata
    -- object. The 50 TB Snowballs are only available in the US regions.
    snowballCapacityPreference :: Prelude.Maybe SnowballCapacity,
    -- | The updated description of this job\'s JobMetadata object.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the updated Address object.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The updated ID for the forwarding address for a job. This field is not
    -- supported in most regions.
    forwardingAddressId :: Prelude.Maybe Prelude.Text,
    -- | The new or updated Notification object.
    notification :: Prelude.Maybe Notification,
    -- | The job ID of the job that you want to update, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'updateJob_roleARN' - The new role Amazon Resource Name (ARN) that you want to associate with
-- this job. To create a role ARN, use the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>AWS
-- Identity and Access Management (IAM) API action.
--
-- 'shippingOption', 'updateJob_shippingOption' - The updated shipping option value of this job\'s ShippingDetails object.
--
-- 'resources', 'updateJob_resources' - The updated @JobResource@ object, or the updated JobResource object.
--
-- 'snowballCapacityPreference', 'updateJob_snowballCapacityPreference' - The updated @SnowballCapacityPreference@ of this job\'s JobMetadata
-- object. The 50 TB Snowballs are only available in the US regions.
--
-- 'description', 'updateJob_description' - The updated description of this job\'s JobMetadata object.
--
-- 'addressId', 'updateJob_addressId' - The ID of the updated Address object.
--
-- 'forwardingAddressId', 'updateJob_forwardingAddressId' - The updated ID for the forwarding address for a job. This field is not
-- supported in most regions.
--
-- 'notification', 'updateJob_notification' - The new or updated Notification object.
--
-- 'jobId', 'updateJob_jobId' - The job ID of the job that you want to update, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
newUpdateJob ::
  -- | 'jobId'
  Prelude.Text ->
  UpdateJob
newUpdateJob pJobId_ =
  UpdateJob'
    { roleARN = Prelude.Nothing,
      shippingOption = Prelude.Nothing,
      resources = Prelude.Nothing,
      snowballCapacityPreference = Prelude.Nothing,
      description = Prelude.Nothing,
      addressId = Prelude.Nothing,
      forwardingAddressId = Prelude.Nothing,
      notification = Prelude.Nothing,
      jobId = pJobId_
    }

-- | The new role Amazon Resource Name (ARN) that you want to associate with
-- this job. To create a role ARN, use the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole>AWS
-- Identity and Access Management (IAM) API action.
updateJob_roleARN :: Lens.Lens' UpdateJob (Prelude.Maybe Prelude.Text)
updateJob_roleARN = Lens.lens (\UpdateJob' {roleARN} -> roleARN) (\s@UpdateJob' {} a -> s {roleARN = a} :: UpdateJob)

-- | The updated shipping option value of this job\'s ShippingDetails object.
updateJob_shippingOption :: Lens.Lens' UpdateJob (Prelude.Maybe ShippingOption)
updateJob_shippingOption = Lens.lens (\UpdateJob' {shippingOption} -> shippingOption) (\s@UpdateJob' {} a -> s {shippingOption = a} :: UpdateJob)

-- | The updated @JobResource@ object, or the updated JobResource object.
updateJob_resources :: Lens.Lens' UpdateJob (Prelude.Maybe JobResource)
updateJob_resources = Lens.lens (\UpdateJob' {resources} -> resources) (\s@UpdateJob' {} a -> s {resources = a} :: UpdateJob)

-- | The updated @SnowballCapacityPreference@ of this job\'s JobMetadata
-- object. The 50 TB Snowballs are only available in the US regions.
updateJob_snowballCapacityPreference :: Lens.Lens' UpdateJob (Prelude.Maybe SnowballCapacity)
updateJob_snowballCapacityPreference = Lens.lens (\UpdateJob' {snowballCapacityPreference} -> snowballCapacityPreference) (\s@UpdateJob' {} a -> s {snowballCapacityPreference = a} :: UpdateJob)

-- | The updated description of this job\'s JobMetadata object.
updateJob_description :: Lens.Lens' UpdateJob (Prelude.Maybe Prelude.Text)
updateJob_description = Lens.lens (\UpdateJob' {description} -> description) (\s@UpdateJob' {} a -> s {description = a} :: UpdateJob)

-- | The ID of the updated Address object.
updateJob_addressId :: Lens.Lens' UpdateJob (Prelude.Maybe Prelude.Text)
updateJob_addressId = Lens.lens (\UpdateJob' {addressId} -> addressId) (\s@UpdateJob' {} a -> s {addressId = a} :: UpdateJob)

-- | The updated ID for the forwarding address for a job. This field is not
-- supported in most regions.
updateJob_forwardingAddressId :: Lens.Lens' UpdateJob (Prelude.Maybe Prelude.Text)
updateJob_forwardingAddressId = Lens.lens (\UpdateJob' {forwardingAddressId} -> forwardingAddressId) (\s@UpdateJob' {} a -> s {forwardingAddressId = a} :: UpdateJob)

-- | The new or updated Notification object.
updateJob_notification :: Lens.Lens' UpdateJob (Prelude.Maybe Notification)
updateJob_notification = Lens.lens (\UpdateJob' {notification} -> notification) (\s@UpdateJob' {} a -> s {notification = a} :: UpdateJob)

-- | The job ID of the job that you want to update, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
updateJob_jobId :: Lens.Lens' UpdateJob Prelude.Text
updateJob_jobId = Lens.lens (\UpdateJob' {jobId} -> jobId) (\s@UpdateJob' {} a -> s {jobId = a} :: UpdateJob)

instance Prelude.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJob

instance Prelude.NFData UpdateJob

instance Prelude.ToHeaders UpdateJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.UpdateJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateJob where
  toJSON UpdateJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoleARN" Prelude..=) Prelude.<$> roleARN,
            ("ShippingOption" Prelude..=)
              Prelude.<$> shippingOption,
            ("Resources" Prelude..=) Prelude.<$> resources,
            ("SnowballCapacityPreference" Prelude..=)
              Prelude.<$> snowballCapacityPreference,
            ("Description" Prelude..=) Prelude.<$> description,
            ("AddressId" Prelude..=) Prelude.<$> addressId,
            ("ForwardingAddressId" Prelude..=)
              Prelude.<$> forwardingAddressId,
            ("Notification" Prelude..=) Prelude.<$> notification,
            Prelude.Just ("JobId" Prelude..= jobId)
          ]
      )

instance Prelude.ToPath UpdateJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateJobResponse_httpStatus' - The response's http status code.
newUpdateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateJobResponse
newUpdateJobResponse pHttpStatus_ =
  UpdateJobResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateJobResponse_httpStatus :: Lens.Lens' UpdateJobResponse Prelude.Int
updateJobResponse_httpStatus = Lens.lens (\UpdateJobResponse' {httpStatus} -> httpStatus) (\s@UpdateJobResponse' {} a -> s {httpStatus = a} :: UpdateJobResponse)

instance Prelude.NFData UpdateJobResponse
