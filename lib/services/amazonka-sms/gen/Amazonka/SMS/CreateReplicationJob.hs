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
-- Module      : Amazonka.SMS.CreateReplicationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication job. The replication job schedules periodic
-- replication runs to replicate your server to Amazon Web Services. Each
-- replication run creates an Amazon Machine Image (AMI).
module Amazonka.SMS.CreateReplicationJob
  ( -- * Creating a Request
    CreateReplicationJob (..),
    newCreateReplicationJob,

    -- * Request Lenses
    createReplicationJob_description,
    createReplicationJob_encrypted,
    createReplicationJob_frequency,
    createReplicationJob_kmsKeyId,
    createReplicationJob_licenseType,
    createReplicationJob_numberOfRecentAmisToKeep,
    createReplicationJob_roleName,
    createReplicationJob_runOnce,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newCreateReplicationJob' smart constructor.
data CreateReplicationJob = CreateReplicationJob'
  { -- | The description of the replication job.
    description :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the replication job produces encrypted AMIs.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Prelude.Maybe Prelude.Int,
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
    -- | The license type to be used for the AMI created by a successful
    -- replication run.
    licenseType :: Prelude.Maybe LicenseType,
    -- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
    -- after the maximum number is reached and a new AMI is created.
    numberOfRecentAmisToKeep :: Prelude.Maybe Prelude.Int,
    -- | The name of the IAM role to be used by the Server Migration Service.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to run the replication job one time.
    runOnce :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the server.
    serverId :: Prelude.Text,
    -- | The seed replication time.
    seedReplicationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createReplicationJob_description' - The description of the replication job.
--
-- 'encrypted', 'createReplicationJob_encrypted' - Indicates whether the replication job produces encrypted AMIs.
--
-- 'frequency', 'createReplicationJob_frequency' - The time between consecutive replication runs, in hours.
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
-- 'licenseType', 'createReplicationJob_licenseType' - The license type to be used for the AMI created by a successful
-- replication run.
--
-- 'numberOfRecentAmisToKeep', 'createReplicationJob_numberOfRecentAmisToKeep' - The maximum number of SMS-created AMIs to retain. The oldest is deleted
-- after the maximum number is reached and a new AMI is created.
--
-- 'roleName', 'createReplicationJob_roleName' - The name of the IAM role to be used by the Server Migration Service.
--
-- 'runOnce', 'createReplicationJob_runOnce' - Indicates whether to run the replication job one time.
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
      { description =
          Prelude.Nothing,
        encrypted = Prelude.Nothing,
        frequency = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        licenseType = Prelude.Nothing,
        numberOfRecentAmisToKeep = Prelude.Nothing,
        roleName = Prelude.Nothing,
        runOnce = Prelude.Nothing,
        serverId = pServerId_,
        seedReplicationTime =
          Data._Time Lens.# pSeedReplicationTime_
      }

-- | The description of the replication job.
createReplicationJob_description :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Text)
createReplicationJob_description = Lens.lens (\CreateReplicationJob' {description} -> description) (\s@CreateReplicationJob' {} a -> s {description = a} :: CreateReplicationJob)

-- | Indicates whether the replication job produces encrypted AMIs.
createReplicationJob_encrypted :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Bool)
createReplicationJob_encrypted = Lens.lens (\CreateReplicationJob' {encrypted} -> encrypted) (\s@CreateReplicationJob' {} a -> s {encrypted = a} :: CreateReplicationJob)

-- | The time between consecutive replication runs, in hours.
createReplicationJob_frequency :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Int)
createReplicationJob_frequency = Lens.lens (\CreateReplicationJob' {frequency} -> frequency) (\s@CreateReplicationJob' {} a -> s {frequency = a} :: CreateReplicationJob)

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

-- | The license type to be used for the AMI created by a successful
-- replication run.
createReplicationJob_licenseType :: Lens.Lens' CreateReplicationJob (Prelude.Maybe LicenseType)
createReplicationJob_licenseType = Lens.lens (\CreateReplicationJob' {licenseType} -> licenseType) (\s@CreateReplicationJob' {} a -> s {licenseType = a} :: CreateReplicationJob)

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
-- after the maximum number is reached and a new AMI is created.
createReplicationJob_numberOfRecentAmisToKeep :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Int)
createReplicationJob_numberOfRecentAmisToKeep = Lens.lens (\CreateReplicationJob' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@CreateReplicationJob' {} a -> s {numberOfRecentAmisToKeep = a} :: CreateReplicationJob)

-- | The name of the IAM role to be used by the Server Migration Service.
createReplicationJob_roleName :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Text)
createReplicationJob_roleName = Lens.lens (\CreateReplicationJob' {roleName} -> roleName) (\s@CreateReplicationJob' {} a -> s {roleName = a} :: CreateReplicationJob)

-- | Indicates whether to run the replication job one time.
createReplicationJob_runOnce :: Lens.Lens' CreateReplicationJob (Prelude.Maybe Prelude.Bool)
createReplicationJob_runOnce = Lens.lens (\CreateReplicationJob' {runOnce} -> runOnce) (\s@CreateReplicationJob' {} a -> s {runOnce = a} :: CreateReplicationJob)

-- | The ID of the server.
createReplicationJob_serverId :: Lens.Lens' CreateReplicationJob Prelude.Text
createReplicationJob_serverId = Lens.lens (\CreateReplicationJob' {serverId} -> serverId) (\s@CreateReplicationJob' {} a -> s {serverId = a} :: CreateReplicationJob)

-- | The seed replication time.
createReplicationJob_seedReplicationTime :: Lens.Lens' CreateReplicationJob Prelude.UTCTime
createReplicationJob_seedReplicationTime = Lens.lens (\CreateReplicationJob' {seedReplicationTime} -> seedReplicationTime) (\s@CreateReplicationJob' {} a -> s {seedReplicationTime = a} :: CreateReplicationJob) Prelude.. Data._Time

instance Core.AWSRequest CreateReplicationJob where
  type
    AWSResponse CreateReplicationJob =
      CreateReplicationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationJobResponse'
            Prelude.<$> (x Data..?> "replicationJobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReplicationJob where
  hashWithSalt _salt CreateReplicationJob' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` numberOfRecentAmisToKeep
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` runOnce
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` seedReplicationTime

instance Prelude.NFData CreateReplicationJob where
  rnf CreateReplicationJob' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf numberOfRecentAmisToKeep
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf runOnce
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf seedReplicationTime

instance Data.ToHeaders CreateReplicationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.CreateReplicationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateReplicationJob where
  toJSON CreateReplicationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("encrypted" Data..=) Prelude.<$> encrypted,
            ("frequency" Data..=) Prelude.<$> frequency,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("licenseType" Data..=) Prelude.<$> licenseType,
            ("numberOfRecentAmisToKeep" Data..=)
              Prelude.<$> numberOfRecentAmisToKeep,
            ("roleName" Data..=) Prelude.<$> roleName,
            ("runOnce" Data..=) Prelude.<$> runOnce,
            Prelude.Just ("serverId" Data..= serverId),
            Prelude.Just
              ("seedReplicationTime" Data..= seedReplicationTime)
          ]
      )

instance Data.ToPath CreateReplicationJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateReplicationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateReplicationJobResponse' smart constructor.
data CreateReplicationJobResponse = CreateReplicationJobResponse'
  { -- | The unique identifier of the replication job.
    replicationJobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateReplicationJobResponse where
  rnf CreateReplicationJobResponse' {..} =
    Prelude.rnf replicationJobId
      `Prelude.seq` Prelude.rnf httpStatus
