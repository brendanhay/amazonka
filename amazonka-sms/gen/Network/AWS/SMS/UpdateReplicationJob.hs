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
-- Module      : Network.AWS.SMS.UpdateReplicationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified settings for the specified replication job.
module Network.AWS.SMS.UpdateReplicationJob
  ( -- * Creating a Request
    UpdateReplicationJob (..),
    newUpdateReplicationJob,

    -- * Request Lenses
    updateReplicationJob_nextReplicationRunStartTime,
    updateReplicationJob_numberOfRecentAmisToKeep,
    updateReplicationJob_encrypted,
    updateReplicationJob_roleName,
    updateReplicationJob_kmsKeyId,
    updateReplicationJob_frequency,
    updateReplicationJob_description,
    updateReplicationJob_licenseType,
    updateReplicationJob_replicationJobId,

    -- * Destructuring the Response
    UpdateReplicationJobResponse (..),
    newUpdateReplicationJobResponse,

    -- * Response Lenses
    updateReplicationJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newUpdateReplicationJob' smart constructor.
data UpdateReplicationJob = UpdateReplicationJob'
  { -- | The start time of the next replication run.
    nextReplicationRunStartTime :: Core.Maybe Core.POSIX,
    -- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
    -- after the maximum number is reached and a new AMI is created.
    numberOfRecentAmisToKeep :: Core.Maybe Core.Int,
    -- | When true, the replication job produces encrypted AMIs. For more
    -- information, @KmsKeyId@.
    encrypted :: Core.Maybe Core.Bool,
    -- | The name of the IAM role to be used by AWS SMS.
    roleName :: Core.Maybe Core.Text,
    -- | The ID of the KMS key for replication jobs that produce encrypted AMIs.
    -- This value can be any of the following:
    --
    -- -   KMS key ID
    --
    -- -   KMS key alias
    --
    -- -   ARN referring to the KMS key ID
    --
    -- -   ARN referring to the KMS key alias
    --
    -- If encrypted is enabled but a KMS key ID is not specified, the
    -- customer\'s default KMS key for Amazon EBS is used.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Core.Maybe Core.Int,
    -- | The description of the replication job.
    description :: Core.Maybe Core.Text,
    -- | The license type to be used for the AMI created by a successful
    -- replication run.
    licenseType :: Core.Maybe LicenseType,
    -- | The ID of the replication job.
    replicationJobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateReplicationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextReplicationRunStartTime', 'updateReplicationJob_nextReplicationRunStartTime' - The start time of the next replication run.
--
-- 'numberOfRecentAmisToKeep', 'updateReplicationJob_numberOfRecentAmisToKeep' - The maximum number of SMS-created AMIs to retain. The oldest is deleted
-- after the maximum number is reached and a new AMI is created.
--
-- 'encrypted', 'updateReplicationJob_encrypted' - When true, the replication job produces encrypted AMIs. For more
-- information, @KmsKeyId@.
--
-- 'roleName', 'updateReplicationJob_roleName' - The name of the IAM role to be used by AWS SMS.
--
-- 'kmsKeyId', 'updateReplicationJob_kmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs.
-- This value can be any of the following:
--
-- -   KMS key ID
--
-- -   KMS key alias
--
-- -   ARN referring to the KMS key ID
--
-- -   ARN referring to the KMS key alias
--
-- If encrypted is enabled but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
--
-- 'frequency', 'updateReplicationJob_frequency' - The time between consecutive replication runs, in hours.
--
-- 'description', 'updateReplicationJob_description' - The description of the replication job.
--
-- 'licenseType', 'updateReplicationJob_licenseType' - The license type to be used for the AMI created by a successful
-- replication run.
--
-- 'replicationJobId', 'updateReplicationJob_replicationJobId' - The ID of the replication job.
newUpdateReplicationJob ::
  -- | 'replicationJobId'
  Core.Text ->
  UpdateReplicationJob
newUpdateReplicationJob pReplicationJobId_ =
  UpdateReplicationJob'
    { nextReplicationRunStartTime =
        Core.Nothing,
      numberOfRecentAmisToKeep = Core.Nothing,
      encrypted = Core.Nothing,
      roleName = Core.Nothing,
      kmsKeyId = Core.Nothing,
      frequency = Core.Nothing,
      description = Core.Nothing,
      licenseType = Core.Nothing,
      replicationJobId = pReplicationJobId_
    }

-- | The start time of the next replication run.
updateReplicationJob_nextReplicationRunStartTime :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.UTCTime)
updateReplicationJob_nextReplicationRunStartTime = Lens.lens (\UpdateReplicationJob' {nextReplicationRunStartTime} -> nextReplicationRunStartTime) (\s@UpdateReplicationJob' {} a -> s {nextReplicationRunStartTime = a} :: UpdateReplicationJob) Core.. Lens.mapping Core._Time

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
-- after the maximum number is reached and a new AMI is created.
updateReplicationJob_numberOfRecentAmisToKeep :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.Int)
updateReplicationJob_numberOfRecentAmisToKeep = Lens.lens (\UpdateReplicationJob' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@UpdateReplicationJob' {} a -> s {numberOfRecentAmisToKeep = a} :: UpdateReplicationJob)

-- | When true, the replication job produces encrypted AMIs. For more
-- information, @KmsKeyId@.
updateReplicationJob_encrypted :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.Bool)
updateReplicationJob_encrypted = Lens.lens (\UpdateReplicationJob' {encrypted} -> encrypted) (\s@UpdateReplicationJob' {} a -> s {encrypted = a} :: UpdateReplicationJob)

-- | The name of the IAM role to be used by AWS SMS.
updateReplicationJob_roleName :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.Text)
updateReplicationJob_roleName = Lens.lens (\UpdateReplicationJob' {roleName} -> roleName) (\s@UpdateReplicationJob' {} a -> s {roleName = a} :: UpdateReplicationJob)

-- | The ID of the KMS key for replication jobs that produce encrypted AMIs.
-- This value can be any of the following:
--
-- -   KMS key ID
--
-- -   KMS key alias
--
-- -   ARN referring to the KMS key ID
--
-- -   ARN referring to the KMS key alias
--
-- If encrypted is enabled but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
updateReplicationJob_kmsKeyId :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.Text)
updateReplicationJob_kmsKeyId = Lens.lens (\UpdateReplicationJob' {kmsKeyId} -> kmsKeyId) (\s@UpdateReplicationJob' {} a -> s {kmsKeyId = a} :: UpdateReplicationJob)

-- | The time between consecutive replication runs, in hours.
updateReplicationJob_frequency :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.Int)
updateReplicationJob_frequency = Lens.lens (\UpdateReplicationJob' {frequency} -> frequency) (\s@UpdateReplicationJob' {} a -> s {frequency = a} :: UpdateReplicationJob)

-- | The description of the replication job.
updateReplicationJob_description :: Lens.Lens' UpdateReplicationJob (Core.Maybe Core.Text)
updateReplicationJob_description = Lens.lens (\UpdateReplicationJob' {description} -> description) (\s@UpdateReplicationJob' {} a -> s {description = a} :: UpdateReplicationJob)

-- | The license type to be used for the AMI created by a successful
-- replication run.
updateReplicationJob_licenseType :: Lens.Lens' UpdateReplicationJob (Core.Maybe LicenseType)
updateReplicationJob_licenseType = Lens.lens (\UpdateReplicationJob' {licenseType} -> licenseType) (\s@UpdateReplicationJob' {} a -> s {licenseType = a} :: UpdateReplicationJob)

-- | The ID of the replication job.
updateReplicationJob_replicationJobId :: Lens.Lens' UpdateReplicationJob Core.Text
updateReplicationJob_replicationJobId = Lens.lens (\UpdateReplicationJob' {replicationJobId} -> replicationJobId) (\s@UpdateReplicationJob' {} a -> s {replicationJobId = a} :: UpdateReplicationJob)

instance Core.AWSRequest UpdateReplicationJob where
  type
    AWSResponse UpdateReplicationJob =
      UpdateReplicationJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateReplicationJobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateReplicationJob

instance Core.NFData UpdateReplicationJob

instance Core.ToHeaders UpdateReplicationJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.UpdateReplicationJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateReplicationJob where
  toJSON UpdateReplicationJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextReplicationRunStartTime" Core..=)
              Core.<$> nextReplicationRunStartTime,
            ("numberOfRecentAmisToKeep" Core..=)
              Core.<$> numberOfRecentAmisToKeep,
            ("encrypted" Core..=) Core.<$> encrypted,
            ("roleName" Core..=) Core.<$> roleName,
            ("kmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("frequency" Core..=) Core.<$> frequency,
            ("description" Core..=) Core.<$> description,
            ("licenseType" Core..=) Core.<$> licenseType,
            Core.Just
              ("replicationJobId" Core..= replicationJobId)
          ]
      )

instance Core.ToPath UpdateReplicationJob where
  toPath = Core.const "/"

instance Core.ToQuery UpdateReplicationJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateReplicationJobResponse' smart constructor.
data UpdateReplicationJobResponse = UpdateReplicationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateReplicationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateReplicationJobResponse_httpStatus' - The response's http status code.
newUpdateReplicationJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateReplicationJobResponse
newUpdateReplicationJobResponse pHttpStatus_ =
  UpdateReplicationJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateReplicationJobResponse_httpStatus :: Lens.Lens' UpdateReplicationJobResponse Core.Int
updateReplicationJobResponse_httpStatus = Lens.lens (\UpdateReplicationJobResponse' {httpStatus} -> httpStatus) (\s@UpdateReplicationJobResponse' {} a -> s {httpStatus = a} :: UpdateReplicationJobResponse)

instance Core.NFData UpdateReplicationJobResponse
