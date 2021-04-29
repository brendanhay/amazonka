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
-- Module      : Network.AWS.Glacier.DeleteVaultNotifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the notification configuration set for a vault.
-- The operation is eventually consistent; that is, it might take some time
-- for Amazon S3 Glacier to completely disable the notifications and you
-- might still receive some notifications for a short time after you send
-- the delete request.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon S3 Glacier>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-delete.html Delete Vault Notification Configuration>
-- in the Amazon S3 Glacier Developer Guide.
module Network.AWS.Glacier.DeleteVaultNotifications
  ( -- * Creating a Request
    DeleteVaultNotifications (..),
    newDeleteVaultNotifications,

    -- * Request Lenses
    deleteVaultNotifications_accountId,
    deleteVaultNotifications_vaultName,

    -- * Destructuring the Response
    DeleteVaultNotificationsResponse (..),
    newDeleteVaultNotificationsResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for deleting a vault notification configuration from an
-- Amazon Glacier vault.
--
-- /See:/ 'newDeleteVaultNotifications' smart constructor.
data DeleteVaultNotifications = DeleteVaultNotifications'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVaultNotifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteVaultNotifications_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'deleteVaultNotifications_vaultName' - The name of the vault.
newDeleteVaultNotifications ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  DeleteVaultNotifications
newDeleteVaultNotifications pAccountId_ pVaultName_ =
  DeleteVaultNotifications'
    { accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
deleteVaultNotifications_accountId :: Lens.Lens' DeleteVaultNotifications Prelude.Text
deleteVaultNotifications_accountId = Lens.lens (\DeleteVaultNotifications' {accountId} -> accountId) (\s@DeleteVaultNotifications' {} a -> s {accountId = a} :: DeleteVaultNotifications)

-- | The name of the vault.
deleteVaultNotifications_vaultName :: Lens.Lens' DeleteVaultNotifications Prelude.Text
deleteVaultNotifications_vaultName = Lens.lens (\DeleteVaultNotifications' {vaultName} -> vaultName) (\s@DeleteVaultNotifications' {} a -> s {vaultName = a} :: DeleteVaultNotifications)

instance Prelude.AWSRequest DeleteVaultNotifications where
  type
    Rs DeleteVaultNotifications =
      DeleteVaultNotificationsResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteVaultNotificationsResponse'

instance Prelude.Hashable DeleteVaultNotifications

instance Prelude.NFData DeleteVaultNotifications

instance Prelude.ToHeaders DeleteVaultNotifications where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteVaultNotifications where
  toPath DeleteVaultNotifications' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/notification-configuration"
      ]

instance Prelude.ToQuery DeleteVaultNotifications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVaultNotificationsResponse' smart constructor.
data DeleteVaultNotificationsResponse = DeleteVaultNotificationsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVaultNotificationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVaultNotificationsResponse ::
  DeleteVaultNotificationsResponse
newDeleteVaultNotificationsResponse =
  DeleteVaultNotificationsResponse'

instance
  Prelude.NFData
    DeleteVaultNotificationsResponse
