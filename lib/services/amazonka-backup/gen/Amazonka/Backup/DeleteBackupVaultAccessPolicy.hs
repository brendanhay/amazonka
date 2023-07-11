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
-- Module      : Amazonka.Backup.DeleteBackupVaultAccessPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the policy document that manages permissions on a backup vault.
module Amazonka.Backup.DeleteBackupVaultAccessPolicy
  ( -- * Creating a Request
    DeleteBackupVaultAccessPolicy (..),
    newDeleteBackupVaultAccessPolicy,

    -- * Request Lenses
    deleteBackupVaultAccessPolicy_backupVaultName,

    -- * Destructuring the Response
    DeleteBackupVaultAccessPolicyResponse (..),
    newDeleteBackupVaultAccessPolicyResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBackupVaultAccessPolicy' smart constructor.
data DeleteBackupVaultAccessPolicy = DeleteBackupVaultAccessPolicy'
  { -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupVaultAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'deleteBackupVaultAccessPolicy_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
newDeleteBackupVaultAccessPolicy ::
  -- | 'backupVaultName'
  Prelude.Text ->
  DeleteBackupVaultAccessPolicy
newDeleteBackupVaultAccessPolicy pBackupVaultName_ =
  DeleteBackupVaultAccessPolicy'
    { backupVaultName =
        pBackupVaultName_
    }

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
deleteBackupVaultAccessPolicy_backupVaultName :: Lens.Lens' DeleteBackupVaultAccessPolicy Prelude.Text
deleteBackupVaultAccessPolicy_backupVaultName = Lens.lens (\DeleteBackupVaultAccessPolicy' {backupVaultName} -> backupVaultName) (\s@DeleteBackupVaultAccessPolicy' {} a -> s {backupVaultName = a} :: DeleteBackupVaultAccessPolicy)

instance
  Core.AWSRequest
    DeleteBackupVaultAccessPolicy
  where
  type
    AWSResponse DeleteBackupVaultAccessPolicy =
      DeleteBackupVaultAccessPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteBackupVaultAccessPolicyResponse'

instance
  Prelude.Hashable
    DeleteBackupVaultAccessPolicy
  where
  hashWithSalt _salt DeleteBackupVaultAccessPolicy' {..} =
    _salt `Prelude.hashWithSalt` backupVaultName

instance Prelude.NFData DeleteBackupVaultAccessPolicy where
  rnf DeleteBackupVaultAccessPolicy' {..} =
    Prelude.rnf backupVaultName

instance Data.ToHeaders DeleteBackupVaultAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBackupVaultAccessPolicy where
  toPath DeleteBackupVaultAccessPolicy' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/access-policy"
      ]

instance Data.ToQuery DeleteBackupVaultAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackupVaultAccessPolicyResponse' smart constructor.
data DeleteBackupVaultAccessPolicyResponse = DeleteBackupVaultAccessPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupVaultAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBackupVaultAccessPolicyResponse ::
  DeleteBackupVaultAccessPolicyResponse
newDeleteBackupVaultAccessPolicyResponse =
  DeleteBackupVaultAccessPolicyResponse'

instance
  Prelude.NFData
    DeleteBackupVaultAccessPolicyResponse
  where
  rnf _ = ()
