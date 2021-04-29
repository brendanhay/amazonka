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
-- Module      : Network.AWS.Glacier.DeleteArchive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes an archive from a vault. Subsequent requests to
-- initiate a retrieval of this archive will fail. Archive retrievals that
-- are in progress for this archive ID may or may not succeed according to
-- the following scenarios:
--
-- -   If the archive retrieval job is actively preparing the data for
--     download when Amazon S3 Glacier receives the delete archive request,
--     the archival retrieval operation might fail.
--
-- -   If the archive retrieval job has successfully prepared the archive
--     for download when Amazon S3 Glacier receives the delete archive
--     request, you will be able to download the output.
--
-- This operation is idempotent. Attempting to delete an already-deleted
-- archive does not result in an error.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-an-archive.html Deleting an Archive in Amazon Glacier>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html Delete Archive>
-- in the /Amazon Glacier Developer Guide/.
module Network.AWS.Glacier.DeleteArchive
  ( -- * Creating a Request
    DeleteArchive (..),
    newDeleteArchive,

    -- * Request Lenses
    deleteArchive_accountId,
    deleteArchive_vaultName,
    deleteArchive_archiveId,

    -- * Destructuring the Response
    DeleteArchiveResponse (..),
    newDeleteArchiveResponse,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Provides options for deleting an archive from an Amazon S3 Glacier
-- vault.
--
-- /See:/ 'newDeleteArchive' smart constructor.
data DeleteArchive = DeleteArchive'
  { -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text,
    -- | The ID of the archive to delete.
    archiveId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteArchive_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'vaultName', 'deleteArchive_vaultName' - The name of the vault.
--
-- 'archiveId', 'deleteArchive_archiveId' - The ID of the archive to delete.
newDeleteArchive ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'vaultName'
  Prelude.Text ->
  -- | 'archiveId'
  Prelude.Text ->
  DeleteArchive
newDeleteArchive pAccountId_ pVaultName_ pArchiveId_ =
  DeleteArchive'
    { accountId = pAccountId_,
      vaultName = pVaultName_,
      archiveId = pArchiveId_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
deleteArchive_accountId :: Lens.Lens' DeleteArchive Prelude.Text
deleteArchive_accountId = Lens.lens (\DeleteArchive' {accountId} -> accountId) (\s@DeleteArchive' {} a -> s {accountId = a} :: DeleteArchive)

-- | The name of the vault.
deleteArchive_vaultName :: Lens.Lens' DeleteArchive Prelude.Text
deleteArchive_vaultName = Lens.lens (\DeleteArchive' {vaultName} -> vaultName) (\s@DeleteArchive' {} a -> s {vaultName = a} :: DeleteArchive)

-- | The ID of the archive to delete.
deleteArchive_archiveId :: Lens.Lens' DeleteArchive Prelude.Text
deleteArchive_archiveId = Lens.lens (\DeleteArchive' {archiveId} -> archiveId) (\s@DeleteArchive' {} a -> s {archiveId = a} :: DeleteArchive)

instance Prelude.AWSRequest DeleteArchive where
  type Rs DeleteArchive = DeleteArchiveResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteArchiveResponse'

instance Prelude.Hashable DeleteArchive

instance Prelude.NFData DeleteArchive

instance Prelude.ToHeaders DeleteArchive where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteArchive where
  toPath DeleteArchive' {..} =
    Prelude.mconcat
      [ "/",
        Prelude.toBS accountId,
        "/vaults/",
        Prelude.toBS vaultName,
        "/archives/",
        Prelude.toBS archiveId
      ]

instance Prelude.ToQuery DeleteArchive where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteArchiveResponse' smart constructor.
data DeleteArchiveResponse = DeleteArchiveResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteArchiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteArchiveResponse ::
  DeleteArchiveResponse
newDeleteArchiveResponse = DeleteArchiveResponse'

instance Prelude.NFData DeleteArchiveResponse
