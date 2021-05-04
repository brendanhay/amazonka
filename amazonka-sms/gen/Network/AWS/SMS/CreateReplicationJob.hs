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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newCreateReplicationJob' smart constructor.
data CreateReplicationJob = CreateReplicationJob'
  { -- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
    -- after the maximum number is reached and a new AMI is created.
    numberOfRecentAmisToKeep :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the replication job produces encrypted AMIs.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The name of the IAM role to be used by the AWS SMS.
    roleName :: Prelude.Maybe Prelude.Text,
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
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Prelude.Maybe Prelude.Bool,
    -- | The description of the replication job.
    description :: Prelude.Maybe Prelude.Text,
    -- | The license type to be used for the AMI created by a successful
    -- replication run.
    licenseType :: Prelude.Maybe LicenseType,
    -- | The ID of the server.
    serverId :: Prelude.Text,
    -- | The seed replication time.
    seedReplicationTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'seedReplicationTime'
  Prelude.UTCTime ->
  CreateReplicationJob
newCreateReplicationJob
  pServerId_
  pSeedReplicationTime_ =
    CreateReplicationJob'
      { numberOfRecentAmisToKeep =
          Prelude.Nothing,
        encrypted = Prelude.Nothing,
        roleName = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        frequency = Prelude.Nothing,
        runOnce = Prelude.Nothing,
        description = Prelude.Nothing,
        licenseType = Prelude.Nothing,
        serverId = pServerId_,
        seedReplicationTime =
          Prelude._Time Lens.# pSeedReplicationTime_
      }

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
-- after the maximum number is reached and a new AMI is created.
createReplicationJob_numberOfRecentAmisToKeep :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Int)
createReplicationJob_numberOfRecentAmisToKeep = Lens.lens (\CreateReplicationJob' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@CreateReplicationJob' {} a -> s {numberOfRecentAmisToKeep = a} :: CreateReplicationJob)

-- | Indicates whether the replication job produces encrypted AMIs.
createReplicationJob_encrypted :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Bool)
createReplicationJob_encrypted = Lens.lens (\CreateReplicationJob' {encrypted} -> encrypted) (\s@CreateReplicationJob' {} a -> s {encrypted = a} :: CreateReplicationJob)

-- | The name of the IAM role to be used by the AWS SMS.
createReplicationJob_roleName :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Text)
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
createReplicationJob_kmsKeyId :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Text)
createReplicationJob_kmsKeyId = Lens.lens (\CreateReplicationJob' {kmsKeyId} -> kmsKeyId) (\s@CreateReplicationJob' {} a -> s {kmsKeyId = a} :: CreateReplicationJob)

-- | The time between consecutive replication runs, in hours.
createReplicationJob_frequency :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Int)
createReplicationJob_frequency = Lens.lens (\CreateReplicationJob' {frequency} -> frequency) (\s@CreateReplicationJob' {} a -> s {frequency = a} :: CreateReplicationJob)

-- | Indicates whether to run the replication job one time.
createReplicationJob_runOnce :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Bool)
createReplicationJob_runOnce = Lens.lens (\CreateReplicationJob' {runOnce} -> runOnce) (\s@CreateReplicationJob' {} a -> s {runOnce = a} :: CreateReplicationJob)

-- | The description of the replication job.
createReplicationJob_description :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Text)
createReplicationJob_description = Lens.lens (\CreateReplicationJob' {description} -> description) (\s@CreateReplicationJob' {} a -> s {description = a} :: CreateReplicationJob)

-- | The license type to be used for the AMI created by a successful
-- replication run.
createReplicationJob_licenseType :: Lens.Lens' CreateReplicationJob (Prelude.Maybe LicenseType)
createReplicationJob_licenseType = Lens.lens (\CreateReplicationJob' {licenseType} -> licenseType) (\s@CreateReplicationJob' {} a -> s {licenseType = a} :: CreateReplicationJob)

-- | The ID of the server.
createReplicationJob_serverId :: Lens.Lens' CreateReplicationJob Prelude.Text
createReplicationJob_serverId = Lens.lens (\CreateReplicationJob' {serverId} -> serverId) (\s@CreateReplicationJob' {} a -> s {serverId = a} :: CreateReplicationJob)

-- | The seed replication time.
createReplicationJob_seedReplicationTime :: Lens.Lens' CreateReplicationJob Prelude.UTCTime
createReplicationJob_seedReplicationTime = Lens.lens (\CreateReplicationJob' {seedReplicationTime} -> seedReplicationTime) (\s@CreateReplicationJob' {} a -> s {seedReplicationTime = a} :: CreateReplicationJob) Prelude.. Prelude._Time

instance Prelude.AWSRequest CreateReplicationJob where
  type
    Rs CreateReplicationJob =
      CreateReplicationJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationJobResponse'
            Prelude.<$> (x Prelude..?> "replicationJobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReplicationJob

instance Prelude.NFData CreateReplicationJob

instance Prelude.ToHeaders CreateReplicationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.CreateReplicationJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateReplicationJob where
  toJSON CreateReplicationJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("numberOfRecentAmisToKeep" Prelude..=)
              Prelude.<$> numberOfRecentAmisToKeep,
            ("encrypted" Prelude..=) Prelude.<$> encrypted,
            ("roleName" Prelude..=) Prelude.<$> roleName,
            ("kmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("frequency" Prelude..=) Prelude.<$> frequency,
            ("runOnce" Prelude..=) Prelude.<$> runOnce,
            ("description" Prelude..=) Prelude.<$> description,
            ("licenseType" Prelude..=) Prelude.<$> licenseType,
            Prelude.Just ("serverId" Prelude..= serverId),
            Prelude.Just
              ( "seedReplicationTime"
                  Prelude..= seedReplicationTime
              )
          ]
      )

instance Prelude.ToPath CreateReplicationJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateReplicationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateReplicationJobResponse' smart constructor.
data CreateReplicationJobResponse = CreateReplicationJobResponse'
  { -- | The unique identifier of the replication job.
    replicationJobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateReplicationJobResponse
newCreateReplicationJobResponse pHttpStatus_ =
  CreateReplicationJobResponse'
    { replicationJobId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the replication job.
createReplicationJobResponse_replicationJobId :: Lens.Lens' CreateReplicationJobResponse (Prelude.Maybe Prelude.Text)
createReplicationJobResponse_replicationJobId = Lens.lens (\CreateReplicationJobResponse' {replicationJobId} -> replicationJobId) (\s@CreateReplicationJobResponse' {} a -> s {replicationJobId = a} :: CreateReplicationJobResponse)

-- | The response's http status code.
createReplicationJobResponse_httpStatus :: Lens.Lens' CreateReplicationJobResponse Prelude.Int
createReplicationJobResponse_httpStatus = Lens.lens (\CreateReplicationJobResponse' {httpStatus} -> httpStatus) (\s@CreateReplicationJobResponse' {} a -> s {httpStatus = a} :: CreateReplicationJobResponse)

instance Prelude.NFData CreateReplicationJobResponse
