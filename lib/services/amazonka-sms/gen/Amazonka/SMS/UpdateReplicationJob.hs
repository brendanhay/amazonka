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
-- Module      : Amazonka.SMS.UpdateReplicationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified settings for the specified replication job.
module Amazonka.SMS.UpdateReplicationJob
  ( -- * Creating a Request
    UpdateReplicationJob (..),
    newUpdateReplicationJob,

    -- * Request Lenses
    updateReplicationJob_roleName,
    updateReplicationJob_licenseType,
    updateReplicationJob_frequency,
    updateReplicationJob_description,
    updateReplicationJob_encrypted,
    updateReplicationJob_kmsKeyId,
    updateReplicationJob_nextReplicationRunStartTime,
    updateReplicationJob_numberOfRecentAmisToKeep,
    updateReplicationJob_replicationJobId,

    -- * Destructuring the Response
    UpdateReplicationJobResponse (..),
    newUpdateReplicationJobResponse,

    -- * Response Lenses
    updateReplicationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newUpdateReplicationJob' smart constructor.
data UpdateReplicationJob = UpdateReplicationJob'
  { -- | The name of the IAM role to be used by Server Migration Service.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The license type to be used for the AMI created by a successful
    -- replication run.
    licenseType :: Prelude.Maybe LicenseType,
    -- | The time between consecutive replication runs, in hours.
    frequency :: Prelude.Maybe Prelude.Int,
    -- | The description of the replication job.
    description :: Prelude.Maybe Prelude.Text,
    -- | When true, the replication job produces encrypted AMIs. For more
    -- information, @KmsKeyId@.
    encrypted :: Prelude.Maybe Prelude.Bool,
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
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The start time of the next replication run.
    nextReplicationRunStartTime :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
    -- after the maximum number is reached and a new AMI is created.
    numberOfRecentAmisToKeep :: Prelude.Maybe Prelude.Int,
    -- | The ID of the replication job.
    replicationJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReplicationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'updateReplicationJob_roleName' - The name of the IAM role to be used by Server Migration Service.
--
-- 'licenseType', 'updateReplicationJob_licenseType' - The license type to be used for the AMI created by a successful
-- replication run.
--
-- 'frequency', 'updateReplicationJob_frequency' - The time between consecutive replication runs, in hours.
--
-- 'description', 'updateReplicationJob_description' - The description of the replication job.
--
-- 'encrypted', 'updateReplicationJob_encrypted' - When true, the replication job produces encrypted AMIs. For more
-- information, @KmsKeyId@.
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
-- 'nextReplicationRunStartTime', 'updateReplicationJob_nextReplicationRunStartTime' - The start time of the next replication run.
--
-- 'numberOfRecentAmisToKeep', 'updateReplicationJob_numberOfRecentAmisToKeep' - The maximum number of SMS-created AMIs to retain. The oldest is deleted
-- after the maximum number is reached and a new AMI is created.
--
-- 'replicationJobId', 'updateReplicationJob_replicationJobId' - The ID of the replication job.
newUpdateReplicationJob ::
  -- | 'replicationJobId'
  Prelude.Text ->
  UpdateReplicationJob
newUpdateReplicationJob pReplicationJobId_ =
  UpdateReplicationJob'
    { roleName = Prelude.Nothing,
      licenseType = Prelude.Nothing,
      frequency = Prelude.Nothing,
      description = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      nextReplicationRunStartTime = Prelude.Nothing,
      numberOfRecentAmisToKeep = Prelude.Nothing,
      replicationJobId = pReplicationJobId_
    }

-- | The name of the IAM role to be used by Server Migration Service.
updateReplicationJob_roleName :: Lens.Lens' UpdateReplicationJob (Prelude.Maybe Prelude.Text)
updateReplicationJob_roleName = Lens.lens (\UpdateReplicationJob' {roleName} -> roleName) (\s@UpdateReplicationJob' {} a -> s {roleName = a} :: UpdateReplicationJob)

-- | The license type to be used for the AMI created by a successful
-- replication run.
updateReplicationJob_licenseType :: Lens.Lens' UpdateReplicationJob (Prelude.Maybe LicenseType)
updateReplicationJob_licenseType = Lens.lens (\UpdateReplicationJob' {licenseType} -> licenseType) (\s@UpdateReplicationJob' {} a -> s {licenseType = a} :: UpdateReplicationJob)

-- | The time between consecutive replication runs, in hours.
updateReplicationJob_frequency :: Lens.Lens' UpdateReplicationJob (Prelude.Maybe Prelude.Int)
updateReplicationJob_frequency = Lens.lens (\UpdateReplicationJob' {frequency} -> frequency) (\s@UpdateReplicationJob' {} a -> s {frequency = a} :: UpdateReplicationJob)

-- | The description of the replication job.
updateReplicationJob_description :: Lens.Lens' UpdateReplicationJob (Prelude.Maybe Prelude.Text)
updateReplicationJob_description = Lens.lens (\UpdateReplicationJob' {description} -> description) (\s@UpdateReplicationJob' {} a -> s {description = a} :: UpdateReplicationJob)

-- | When true, the replication job produces encrypted AMIs. For more
-- information, @KmsKeyId@.
updateReplicationJob_encrypted :: Lens.Lens' UpdateReplicationJob (Prelude.Maybe Prelude.Bool)
updateReplicationJob_encrypted = Lens.lens (\UpdateReplicationJob' {encrypted} -> encrypted) (\s@UpdateReplicationJob' {} a -> s {encrypted = a} :: UpdateReplicationJob)

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
updateReplicationJob_kmsKeyId :: Lens.Lens' UpdateReplicationJob (Prelude.Maybe Prelude.Text)
updateReplicationJob_kmsKeyId = Lens.lens (\UpdateReplicationJob' {kmsKeyId} -> kmsKeyId) (\s@UpdateReplicationJob' {} a -> s {kmsKeyId = a} :: UpdateReplicationJob)

-- | The start time of the next replication run.
updateReplicationJob_nextReplicationRunStartTime :: Lens.Lens' UpdateReplicationJob (Prelude.Maybe Prelude.UTCTime)
updateReplicationJob_nextReplicationRunStartTime = Lens.lens (\UpdateReplicationJob' {nextReplicationRunStartTime} -> nextReplicationRunStartTime) (\s@UpdateReplicationJob' {} a -> s {nextReplicationRunStartTime = a} :: UpdateReplicationJob) Prelude.. Lens.mapping Core._Time

-- | The maximum number of SMS-created AMIs to retain. The oldest is deleted
-- after the maximum number is reached and a new AMI is created.
updateReplicationJob_numberOfRecentAmisToKeep :: Lens.Lens' UpdateReplicationJob (Prelude.Maybe Prelude.Int)
updateReplicationJob_numberOfRecentAmisToKeep = Lens.lens (\UpdateReplicationJob' {numberOfRecentAmisToKeep} -> numberOfRecentAmisToKeep) (\s@UpdateReplicationJob' {} a -> s {numberOfRecentAmisToKeep = a} :: UpdateReplicationJob)

-- | The ID of the replication job.
updateReplicationJob_replicationJobId :: Lens.Lens' UpdateReplicationJob Prelude.Text
updateReplicationJob_replicationJobId = Lens.lens (\UpdateReplicationJob' {replicationJobId} -> replicationJobId) (\s@UpdateReplicationJob' {} a -> s {replicationJobId = a} :: UpdateReplicationJob)

instance Core.AWSRequest UpdateReplicationJob where
  type
    AWSResponse UpdateReplicationJob =
      UpdateReplicationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateReplicationJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateReplicationJob where
  hashWithSalt _salt UpdateReplicationJob' {..} =
    _salt `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` nextReplicationRunStartTime
      `Prelude.hashWithSalt` numberOfRecentAmisToKeep
      `Prelude.hashWithSalt` replicationJobId

instance Prelude.NFData UpdateReplicationJob where
  rnf UpdateReplicationJob' {..} =
    Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf nextReplicationRunStartTime
      `Prelude.seq` Prelude.rnf numberOfRecentAmisToKeep
      `Prelude.seq` Prelude.rnf replicationJobId

instance Core.ToHeaders UpdateReplicationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.UpdateReplicationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateReplicationJob where
  toJSON UpdateReplicationJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("roleName" Core..=) Prelude.<$> roleName,
            ("licenseType" Core..=) Prelude.<$> licenseType,
            ("frequency" Core..=) Prelude.<$> frequency,
            ("description" Core..=) Prelude.<$> description,
            ("encrypted" Core..=) Prelude.<$> encrypted,
            ("kmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("nextReplicationRunStartTime" Core..=)
              Prelude.<$> nextReplicationRunStartTime,
            ("numberOfRecentAmisToKeep" Core..=)
              Prelude.<$> numberOfRecentAmisToKeep,
            Prelude.Just
              ("replicationJobId" Core..= replicationJobId)
          ]
      )

instance Core.ToPath UpdateReplicationJob where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateReplicationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateReplicationJobResponse' smart constructor.
data UpdateReplicationJobResponse = UpdateReplicationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateReplicationJobResponse
newUpdateReplicationJobResponse pHttpStatus_ =
  UpdateReplicationJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateReplicationJobResponse_httpStatus :: Lens.Lens' UpdateReplicationJobResponse Prelude.Int
updateReplicationJobResponse_httpStatus = Lens.lens (\UpdateReplicationJobResponse' {httpStatus} -> httpStatus) (\s@UpdateReplicationJobResponse' {} a -> s {httpStatus = a} :: UpdateReplicationJobResponse)

instance Prelude.NFData UpdateReplicationJobResponse where
  rnf UpdateReplicationJobResponse' {..} =
    Prelude.rnf httpStatus
