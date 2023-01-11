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
-- Module      : Amazonka.Backup.GetBackupVaultAccessPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access policy document that is associated with the named
-- backup vault.
module Amazonka.Backup.GetBackupVaultAccessPolicy
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
    getBackupVaultAccessPolicyResponse_backupVaultName,
    getBackupVaultAccessPolicyResponse_policy,
    getBackupVaultAccessPolicyResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackupVaultAccessPolicyResponse'
            Prelude.<$> (x Data..?> "BackupVaultArn")
            Prelude.<*> (x Data..?> "BackupVaultName")
            Prelude.<*> (x Data..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackupVaultAccessPolicy where
  hashWithSalt _salt GetBackupVaultAccessPolicy' {..} =
    _salt `Prelude.hashWithSalt` backupVaultName

instance Prelude.NFData GetBackupVaultAccessPolicy where
  rnf GetBackupVaultAccessPolicy' {..} =
    Prelude.rnf backupVaultName

instance Data.ToHeaders GetBackupVaultAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBackupVaultAccessPolicy where
  toPath GetBackupVaultAccessPolicy' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/access-policy"
      ]

instance Data.ToQuery GetBackupVaultAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackupVaultAccessPolicyResponse' smart constructor.
data GetBackupVaultAccessPolicyResponse = GetBackupVaultAccessPolicyResponse'
  { -- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
    -- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
    backupVaultArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Region where they are created. They consist of lowercase
    -- letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text,
    -- | The backup vault access policy document in JSON format.
    policy :: Prelude.Maybe Prelude.Text,
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
-- 'backupVaultName', 'getBackupVaultAccessPolicyResponse_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
--
-- 'policy', 'getBackupVaultAccessPolicyResponse_policy' - The backup vault access policy document in JSON format.
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
      backupVaultName = Prelude.Nothing,
      policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup vault;
-- for example, @arn:aws:backup:us-east-1:123456789012:vault:aBackupVault@.
getBackupVaultAccessPolicyResponse_backupVaultArn :: Lens.Lens' GetBackupVaultAccessPolicyResponse (Prelude.Maybe Prelude.Text)
getBackupVaultAccessPolicyResponse_backupVaultArn = Lens.lens (\GetBackupVaultAccessPolicyResponse' {backupVaultArn} -> backupVaultArn) (\s@GetBackupVaultAccessPolicyResponse' {} a -> s {backupVaultArn = a} :: GetBackupVaultAccessPolicyResponse)

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
getBackupVaultAccessPolicyResponse_backupVaultName :: Lens.Lens' GetBackupVaultAccessPolicyResponse (Prelude.Maybe Prelude.Text)
getBackupVaultAccessPolicyResponse_backupVaultName = Lens.lens (\GetBackupVaultAccessPolicyResponse' {backupVaultName} -> backupVaultName) (\s@GetBackupVaultAccessPolicyResponse' {} a -> s {backupVaultName = a} :: GetBackupVaultAccessPolicyResponse)

-- | The backup vault access policy document in JSON format.
getBackupVaultAccessPolicyResponse_policy :: Lens.Lens' GetBackupVaultAccessPolicyResponse (Prelude.Maybe Prelude.Text)
getBackupVaultAccessPolicyResponse_policy = Lens.lens (\GetBackupVaultAccessPolicyResponse' {policy} -> policy) (\s@GetBackupVaultAccessPolicyResponse' {} a -> s {policy = a} :: GetBackupVaultAccessPolicyResponse)

-- | The response's http status code.
getBackupVaultAccessPolicyResponse_httpStatus :: Lens.Lens' GetBackupVaultAccessPolicyResponse Prelude.Int
getBackupVaultAccessPolicyResponse_httpStatus = Lens.lens (\GetBackupVaultAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@GetBackupVaultAccessPolicyResponse' {} a -> s {httpStatus = a} :: GetBackupVaultAccessPolicyResponse)

instance
  Prelude.NFData
    GetBackupVaultAccessPolicyResponse
  where
  rnf GetBackupVaultAccessPolicyResponse' {..} =
    Prelude.rnf backupVaultArn
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
