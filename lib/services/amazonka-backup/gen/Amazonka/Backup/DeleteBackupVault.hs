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
-- Module      : Amazonka.Backup.DeleteBackupVault
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the backup vault identified by its name. A vault can be deleted
-- only if it is empty.
module Amazonka.Backup.DeleteBackupVault
  ( -- * Creating a Request
    DeleteBackupVault (..),
    newDeleteBackupVault,

    -- * Request Lenses
    deleteBackupVault_backupVaultName,

    -- * Destructuring the Response
    DeleteBackupVaultResponse (..),
    newDeleteBackupVaultResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBackupVault' smart constructor.
data DeleteBackupVault = DeleteBackupVault'
  { -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupVault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'deleteBackupVault_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
newDeleteBackupVault ::
  -- | 'backupVaultName'
  Prelude.Text ->
  DeleteBackupVault
newDeleteBackupVault pBackupVaultName_ =
  DeleteBackupVault'
    { backupVaultName =
        pBackupVaultName_
    }

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
deleteBackupVault_backupVaultName :: Lens.Lens' DeleteBackupVault Prelude.Text
deleteBackupVault_backupVaultName = Lens.lens (\DeleteBackupVault' {backupVaultName} -> backupVaultName) (\s@DeleteBackupVault' {} a -> s {backupVaultName = a} :: DeleteBackupVault)

instance Core.AWSRequest DeleteBackupVault where
  type
    AWSResponse DeleteBackupVault =
      DeleteBackupVaultResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteBackupVaultResponse'

instance Prelude.Hashable DeleteBackupVault where
  hashWithSalt _salt DeleteBackupVault' {..} =
    _salt `Prelude.hashWithSalt` backupVaultName

instance Prelude.NFData DeleteBackupVault where
  rnf DeleteBackupVault' {..} =
    Prelude.rnf backupVaultName

instance Data.ToHeaders DeleteBackupVault where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBackupVault where
  toPath DeleteBackupVault' {..} =
    Prelude.mconcat
      ["/backup-vaults/", Data.toBS backupVaultName]

instance Data.ToQuery DeleteBackupVault where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackupVaultResponse' smart constructor.
data DeleteBackupVaultResponse = DeleteBackupVaultResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupVaultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBackupVaultResponse ::
  DeleteBackupVaultResponse
newDeleteBackupVaultResponse =
  DeleteBackupVaultResponse'

instance Prelude.NFData DeleteBackupVaultResponse where
  rnf _ = ()
