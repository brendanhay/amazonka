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
-- Module      : Network.AWS.Backup.GetBackupVaultAccessPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access policy document that is associated with the named
-- backup vault.
module Network.AWS.Backup.GetBackupVaultAccessPolicy
  ( -- * Creating a Request
    GetBackupVaultAccessPolicy (..),
    newGetBackupVaultAccessPolicy,

    -- * Request Lenses
    getBackupVaultAccessPolicy_backupVaultName,

    -- * Destructuring the Response
    GetBackupVaultAccessPolicyResponse (..),
    newGetBackupVaultAccessPolicyResponse,

    -- * Response Lenses
    getBackupVaultAccessPolicyResponse_backupVaultArn,
    getBackupVaultAccessPolicyResponse_policy,
    getBackupVaultAccessPolicyResponse_backupVaultName,
    getBackupVaultAccessPolicyResponse_httpStatus,
  )
where

import Network.AWS.Backup.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBackupVaultAccessPolicy' smart constructor.
data GetBackupVaultAccessPolicy = GetBackupVaultAccessPolicy'
  { -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackupVaultAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'getBackupVaultAccessPolicy_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
newGetBackupVaultAccessPolicy ::
  -- | 'backupVaultName'
  Prelude.Text ->
  GetBackupVaultAccessPolicy
newGetBackupVaultAccessPolicy pBackupVaultName_ =
  GetBackupVaultAccessPolicy'
    { backupVaultName =
        pBackupVaultName_
    }

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
getBackupVaultAccessPolicy_backupVaultName :: Lens.Lens' GetBackupVaultAccessPolicy Prelude.Text
getBackupVaultAccessPolicy_backupVaultName = Lens.lens (\GetBackupVaultAccessPolicy' {backupVaultName} -> backupVaultName) (\s@GetBackupVaultAccessPolicy' {} a -> s {backupVaultName = a} :: GetBackupVaultAccessPolicy)

instance Core.AWSRequest GetBackupVaultAccessPolicy where
  type
    AWSResponse GetBackupVaultAccessPolicy =
      GetBackupVaultAccessPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackupVaultAccessPolicyResponse'
            Prelude.<$> (x Core..?> "BackupVaultArn")
            Prelude.<*> (x Core..?> "Policy")
            Prelude.<*> (x Core..?> "BackupVaultName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackupVaultAccessPolicy

instance Prelude.NFData GetBackupVaultAccessPolicy

instance Core.ToHeaders GetBackupVaultAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetBackupVaultAccessPolicy where
  toPath GetBackupVaultAccessPolicy' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Core.toBS backupVaultName,
        "/access-policy"
      ]

instance Core.ToQuery GetBackupVaultAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackupVaultAccessPolicyResponse' smart constructor.
data GetBackupVaultAccessPolicyResponse = GetBackupVaultAccessPolicyResponse'
  { -- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
    -- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The backup vault access policy document in JSON format.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Region where they are created. They consist of lowercase
    -- letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackupVaultAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultArn', 'getBackupVaultAccessPolicyResponse_backupVaultArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
--
-- 'policy', 'getBackupVaultAccessPolicyResponse_policy' - The backup vault access policy document in JSON format.
--
-- 'backupVaultName', 'getBackupVaultAccessPolicyResponse_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
--
-- 'httpStatus', 'getBackupVaultAccessPolicyResponse_httpStatus' - The response's http status code.
newGetBackupVaultAccessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackupVaultAccessPolicyResponse
newGetBackupVaultAccessPolicyResponse pHttpStatus_ =
  GetBackupVaultAccessPolicyResponse'
    { backupVaultArn =
        Prelude.Nothing,
      policy = Prelude.Nothing,
      backupVaultName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
getBackupVaultAccessPolicyResponse_backupVaultArn :: Lens.Lens' GetBackupVaultAccessPolicyResponse (Prelude.Maybe Prelude.Text)
getBackupVaultAccessPolicyResponse_backupVaultArn = Lens.lens (\GetBackupVaultAccessPolicyResponse' {backupVaultArn} -> backupVaultArn) (\s@GetBackupVaultAccessPolicyResponse' {} a -> s {backupVaultArn = a} :: GetBackupVaultAccessPolicyResponse)

-- | The backup vault access policy document in JSON format.
getBackupVaultAccessPolicyResponse_policy :: Lens.Lens' GetBackupVaultAccessPolicyResponse (Prelude.Maybe Prelude.Text)
getBackupVaultAccessPolicyResponse_policy = Lens.lens (\GetBackupVaultAccessPolicyResponse' {policy} -> policy) (\s@GetBackupVaultAccessPolicyResponse' {} a -> s {policy = a} :: GetBackupVaultAccessPolicyResponse)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
getBackupVaultAccessPolicyResponse_backupVaultName :: Lens.Lens' GetBackupVaultAccessPolicyResponse (Prelude.Maybe Prelude.Text)
getBackupVaultAccessPolicyResponse_backupVaultName = Lens.lens (\GetBackupVaultAccessPolicyResponse' {backupVaultName} -> backupVaultName) (\s@GetBackupVaultAccessPolicyResponse' {} a -> s {backupVaultName = a} :: GetBackupVaultAccessPolicyResponse)

-- | The response's http status code.
getBackupVaultAccessPolicyResponse_httpStatus :: Lens.Lens' GetBackupVaultAccessPolicyResponse Prelude.Int
getBackupVaultAccessPolicyResponse_httpStatus = Lens.lens (\GetBackupVaultAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@GetBackupVaultAccessPolicyResponse' {} a -> s {httpStatus = a} :: GetBackupVaultAccessPolicyResponse)

instance
  Prelude.NFData
    GetBackupVaultAccessPolicyResponse
