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
-- Module      : Amazonka.Backup.DeleteBackupVaultNotifications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes event notifications for the specified backup vault.
module Amazonka.Backup.DeleteBackupVaultNotifications
  ( -- * Creating a Request
    DeleteBackupVaultNotifications (..),
    newDeleteBackupVaultNotifications,

    -- * Request Lenses
    deleteBackupVaultNotifications_backupVaultName,

    -- * Destructuring the Response
    DeleteBackupVaultNotificationsResponse (..),
    newDeleteBackupVaultNotificationsResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBackupVaultNotifications' smart constructor.
data DeleteBackupVaultNotifications = DeleteBackupVaultNotifications'
  { -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Region where they are created. They consist of lowercase
    -- letters, numbers, and hyphens.
    backupVaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupVaultNotifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupVaultName', 'deleteBackupVaultNotifications_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
newDeleteBackupVaultNotifications ::
  -- | 'backupVaultName'
  Prelude.Text ->
  DeleteBackupVaultNotifications
newDeleteBackupVaultNotifications pBackupVaultName_ =
  DeleteBackupVaultNotifications'
    { backupVaultName =
        pBackupVaultName_
    }

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Region where they are created. They consist of lowercase
-- letters, numbers, and hyphens.
deleteBackupVaultNotifications_backupVaultName :: Lens.Lens' DeleteBackupVaultNotifications Prelude.Text
deleteBackupVaultNotifications_backupVaultName = Lens.lens (\DeleteBackupVaultNotifications' {backupVaultName} -> backupVaultName) (\s@DeleteBackupVaultNotifications' {} a -> s {backupVaultName = a} :: DeleteBackupVaultNotifications)

instance
  Core.AWSRequest
    DeleteBackupVaultNotifications
  where
  type
    AWSResponse DeleteBackupVaultNotifications =
      DeleteBackupVaultNotificationsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteBackupVaultNotificationsResponse'

instance
  Prelude.Hashable
    DeleteBackupVaultNotifications
  where
  hashWithSalt
    _salt
    DeleteBackupVaultNotifications' {..} =
      _salt `Prelude.hashWithSalt` backupVaultName

instance
  Prelude.NFData
    DeleteBackupVaultNotifications
  where
  rnf DeleteBackupVaultNotifications' {..} =
    Prelude.rnf backupVaultName

instance
  Data.ToHeaders
    DeleteBackupVaultNotifications
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBackupVaultNotifications where
  toPath DeleteBackupVaultNotifications' {..} =
    Prelude.mconcat
      [ "/backup-vaults/",
        Data.toBS backupVaultName,
        "/notification-configuration"
      ]

instance Data.ToQuery DeleteBackupVaultNotifications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackupVaultNotificationsResponse' smart constructor.
data DeleteBackupVaultNotificationsResponse = DeleteBackupVaultNotificationsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupVaultNotificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBackupVaultNotificationsResponse ::
  DeleteBackupVaultNotificationsResponse
newDeleteBackupVaultNotificationsResponse =
  DeleteBackupVaultNotificationsResponse'

instance
  Prelude.NFData
    DeleteBackupVaultNotificationsResponse
  where
  rnf _ = ()
