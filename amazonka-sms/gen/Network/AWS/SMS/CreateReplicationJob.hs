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
-- Module      : Network.AWS.SMS.CreateReplicationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication job. The replication job schedules periodic
-- replication runs to replicate your server to AWS. Each replication run
-- creates an Amazon Machine Image (AMI).
module Network.AWS.SMS.CreateReplicationJob
  ( -- * Creating a Request
    CreateReplicationJob (..),
    newCreateReplicationJob,

    -- * Request Lenses
    createReplicationJob_numberOfRecentAmisToKeep,
    createReplicationJob_encrypted,
    createReplicationJob_roleName,
    createReplicationJob_kmsKeyId,
    createReplicationJob_frequency,
    createReplicationJob_runOnce,
    createReplicationJob_description,
    createReplicationJob_licenseType,
    createReplicationJob_serverId,
    createReplicationJob_seedReplicationTime,

    -- * Destructuring the Response
    CreateReplicationJobResponse (..),
    newCreateReplicationJobResponse,

    -- * Response Lenses
    createReplicationJobResponse_replicationJobId,
    createReplicationJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newCreateReplicationJob' smart constructor.
data CreateReplicationJob = CreateReplicationJob'
  { -- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
    -- after the maximum number is reached and a new AMI is created.
    numberOfRecentAmisToKeep :: Core.Maybe Core.Int,
    -- | Indicates whether the replication job produces encrypted AMIs.
    encrypted :: Core.Maybe Core.Bool,
    -- | The name of the IAM role to be used by the AWS SMS.
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
    -- If encrypted is /true/ but a KMS key ID is not specified, the
    -- customer\'s default KMS key for Amazon EBS is used.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Core.Maybe Core.Int,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Core.Maybe Core.Bool,
    -- | The description of the replication job.
    description :: Core.Maybe Core.Text,
    -- | The license type to be used for the AMI created by a successful
    -- replication run.
    licenseType :: Core.Maybe LicenseType,
    -- | The ID of the server.
    serverId :: Core.Text,
    -- | The seed replication time.
    seedReplicationTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateReplicationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfRecentAmisToKeep', 'createReplicationJob_numberOfRecentAmisToKeep' - The maximum number of SMS-created AMIs to retain. The oldest is deleted
-- after the maximum number is reached and a new AMI is created.
--
-- 'encrypted', 'createReplicationJob_encrypted' - Indicates whether the replication job produces encrypted AMIs.
--
-- 'roleName', 'createReplicationJob_roleName' - The name of the IAM role to be used by the AWS SMS.
--
-- 'kmsKeyId', 'createReplicationJob_kmsKeyId' - The ID of the KMS key for replication jobs that produce encrypted AMIs.
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
-- If encrypted is /true/ but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
--
-- 'frequency', 'createReplicationJob_frequency' - The time between consecutive replication runs, in hours.
--
-- 'runOnce', 'createReplicationJob_runOnce' - Indicates whether to run the replication job one time.
--
-- 'description', 'createReplicationJob_description' - The description of the replication job.
--
-- 'licenseType', 'createReplicationJob_licenseType' - The license type to be used for the AMI created by a successful
-- replication run.
--
-- 'serverId', 'createReplicationJob_serverId' - The ID of the server.
--
-- 'seedReplicationTime', 'createReplicationJob_seedReplicationTime' - The seed replication time.
newCreateReplicationJob ::
  -- | 'serverId'
  Core.Text ->
  -- | 'seedReplicationTime'
  Core.UTCTime ->
  CreateReplicationJob
newCreateReplicationJob
  pServerId_
  pSeedReplicationTime_ =
    CreateReplicationJob'
      { numberOfRecentAmisToKeep =
          Core.Nothing,
        encrypted = Core.Nothing,
        roleName = Core.Nothing,
        kmsKeyId = Core.Nothing,
        frequency = Core.Nothing,
        runOnce = Core.Nothing,
        description = Core.Nothing,
        licenseType = Core.Nothing,
        serverId = pServerId_,
        seedReplicationTime =
          Core._Time Lens.# pSeedReplicationTime_
      }

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
-- after the maximum number is reached and a new AMI is created.
createReplicationJob_numberOfRecentAmisToKeep :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Int)
createReplicationJob_numberOfRecentAmisToKeep = Lens.lens (\CreateReplicationJob' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@CreateReplicationJob' {} a -> s {numberOfRecentAmisToKeep = a} :: CreateReplicationJob)

-- | Indicates whether the replication job produces encrypted AMIs.
createReplicationJob_encrypted :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Bool)
createReplicationJob_encrypted = Lens.lens (\CreateReplicationJob' {encrypted} -> encrypted) (\s@CreateReplicationJob' {} a -> s {encrypted = a} :: CreateReplicationJob)

-- | The name of the IAM role to be used by the AWS SMS.
createReplicationJob_roleName :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Text)
createReplicationJob_roleName = Lens.lens (\CreateReplicationJob' {roleName} -> roleName) (\s@CreateReplicationJob' {} a -> s {roleName = a} :: CreateReplicationJob)

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
-- If encrypted is /true/ but a KMS key ID is not specified, the
-- customer\'s default KMS key for Amazon EBS is used.
createReplicationJob_kmsKeyId :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Text)
createReplicationJob_kmsKeyId = Lens.lens (\CreateReplicationJob' {kmsKeyId} -> kmsKeyId) (\s@CreateReplicationJob' {} a -> s {kmsKeyId = a} :: CreateReplicationJob)

-- | The time between consecutive replication runs, in hours.
createReplicationJob_frequency :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Int)
createReplicationJob_frequency = Lens.lens (\CreateReplicationJob' {frequency} -> frequency) (\s@CreateReplicationJob' {} a -> s {frequency = a} :: CreateReplicationJob)

-- | Indicates whether to run the replication job one time.
createReplicationJob_runOnce :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Bool)
createReplicationJob_runOnce = Lens.lens (\CreateReplicationJob' {runOnce} -> runOnce) (\s@CreateReplicationJob' {} a -> s {runOnce = a} :: CreateReplicationJob)

-- | The description of the replication job.
createReplicationJob_description :: Lens.Lens' CreateReplicationJob (Core.Maybe Core.Text)
createReplicationJob_description = Lens.lens (\CreateReplicationJob' {description} -> description) (\s@CreateReplicationJob' {} a -> s {description = a} :: CreateReplicationJob)

-- | The license type to be used for the AMI created by a successful
-- replication run.
createReplicationJob_licenseType :: Lens.Lens' CreateReplicationJob (Core.Maybe LicenseType)
createReplicationJob_licenseType = Lens.lens (\CreateReplicationJob' {licenseType} -> licenseType) (\s@CreateReplicationJob' {} a -> s {licenseType = a} :: CreateReplicationJob)

-- | The ID of the server.
createReplicationJob_serverId :: Lens.Lens' CreateReplicationJob Core.Text
createReplicationJob_serverId = Lens.lens (\CreateReplicationJob' {serverId} -> serverId) (\s@CreateReplicationJob' {} a -> s {serverId = a} :: CreateReplicationJob)

-- | The seed replication time.
createReplicationJob_seedReplicationTime :: Lens.Lens' CreateReplicationJob Core.UTCTime
createReplicationJob_seedReplicationTime = Lens.lens (\CreateReplicationJob' {seedReplicationTime} -> seedReplicationTime) (\s@CreateReplicationJob' {} a -> s {seedReplicationTime = a} :: CreateReplicationJob) Core.. Core._Time

instance Core.AWSRequest CreateReplicationJob where
  type
    AWSResponse CreateReplicationJob =
      CreateReplicationJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationJobResponse'
            Core.<$> (x Core..?> "replicationJobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateReplicationJob

instance Core.NFData CreateReplicationJob

instance Core.ToHeaders CreateReplicationJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.CreateReplicationJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateReplicationJob where
  toJSON CreateReplicationJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("numberOfRecentAmisToKeep" Core..=)
              Core.<$> numberOfRecentAmisToKeep,
            ("encrypted" Core..=) Core.<$> encrypted,
            ("roleName" Core..=) Core.<$> roleName,
            ("kmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("frequency" Core..=) Core.<$> frequency,
            ("runOnce" Core..=) Core.<$> runOnce,
            ("description" Core..=) Core.<$> description,
            ("licenseType" Core..=) Core.<$> licenseType,
            Core.Just ("serverId" Core..= serverId),
            Core.Just
              ("seedReplicationTime" Core..= seedReplicationTime)
          ]
      )

instance Core.ToPath CreateReplicationJob where
  toPath = Core.const "/"

instance Core.ToQuery CreateReplicationJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateReplicationJobResponse' smart constructor.
data CreateReplicationJobResponse = CreateReplicationJobResponse'
  { -- | The unique identifier of the replication job.
    replicationJobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateReplicationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationJobId', 'createReplicationJobResponse_replicationJobId' - The unique identifier of the replication job.
--
-- 'httpStatus', 'createReplicationJobResponse_httpStatus' - The response's http status code.
newCreateReplicationJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateReplicationJobResponse
newCreateReplicationJobResponse pHttpStatus_ =
  CreateReplicationJobResponse'
    { replicationJobId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the replication job.
createReplicationJobResponse_replicationJobId :: Lens.Lens' CreateReplicationJobResponse (Core.Maybe Core.Text)
createReplicationJobResponse_replicationJobId = Lens.lens (\CreateReplicationJobResponse' {replicationJobId} -> replicationJobId) (\s@CreateReplicationJobResponse' {} a -> s {replicationJobId = a} :: CreateReplicationJobResponse)

-- | The response's http status code.
createReplicationJobResponse_httpStatus :: Lens.Lens' CreateReplicationJobResponse Core.Int
createReplicationJobResponse_httpStatus = Lens.lens (\CreateReplicationJobResponse' {httpStatus} -> httpStatus) (\s@CreateReplicationJobResponse' {} a -> s {httpStatus = a} :: CreateReplicationJobResponse)

instance Core.NFData CreateReplicationJobResponse
