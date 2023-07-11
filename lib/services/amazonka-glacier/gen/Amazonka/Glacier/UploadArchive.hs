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
-- Module      : Amazonka.Glacier.UploadArchive
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds an archive to a vault. This is a synchronous
-- operation, and for a successful upload, your data is durably persisted.
-- Amazon S3 Glacier returns the archive ID in the @x-amz-archive-id@
-- header of the response.
--
-- You must use the archive ID to access your data in Amazon S3 Glacier.
-- After you upload an archive, you should save the archive ID returned so
-- that you can retrieve or delete the archive later. Besides saving the
-- archive ID, you can also index it and give it a friendly name to allow
-- for better searching. You can also use the optional archive description
-- field to specify how the archive is referred to in an external index of
-- archives, such as you might create in Amazon DynamoDB. You can also get
-- the vault inventory to obtain a list of archive IDs in a vault. For more
-- information, see InitiateJob.
--
-- You must provide a SHA256 tree hash of the data you are uploading. For
-- information about computing a SHA256 tree hash, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums>.
--
-- You can optionally specify an archive description of up to 1,024
-- printable ASCII characters. You can get the archive description when you
-- either retrieve the archive or get the vault inventory. For more
-- information, see InitiateJob. Amazon Glacier does not interpret the
-- description in any way. An archive description does not need to be
-- unique. You cannot use the description to retrieve or sort the archive
-- list.
--
-- Archives are immutable. After you upload an archive, you cannot edit the
-- archive or its description.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-an-archive.html Uploading an Archive in Amazon Glacier>
-- and
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive>
-- in the /Amazon Glacier Developer Guide/.
module Amazonka.Glacier.UploadArchive
  ( -- * Creating a Request
    UploadArchive (..),
    newUploadArchive,

    -- * Request Lenses
    uploadArchive_archiveDescription,
    uploadArchive_checksum,
    uploadArchive_vaultName,
    uploadArchive_accountId,
    uploadArchive_body,

    -- * Destructuring the Response
    ArchiveCreationOutput (..),
    newArchiveCreationOutput,

    -- * Response Lenses
    archiveCreationOutput_archiveId,
    archiveCreationOutput_checksum,
    archiveCreationOutput_location,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glacier.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Provides options to add an archive to a vault.
--
-- /See:/ 'newUploadArchive' smart constructor.
data UploadArchive = UploadArchive'
  { -- | The optional description of the archive you are uploading.
    archiveDescription :: Prelude.Maybe Prelude.Text,
    -- | The SHA256 tree hash of the data being uploaded.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The name of the vault.
    vaultName :: Prelude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the
    -- vault. You can either specify an AWS account ID or optionally a single
    -- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
    -- ID associated with the credentials used to sign the request. If you use
    -- an account ID, do not include any hyphens (\'-\') in the ID.
    accountId :: Prelude.Text,
    -- | The data to upload.
    body :: Data.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveDescription', 'uploadArchive_archiveDescription' - The optional description of the archive you are uploading.
--
-- 'checksum', 'uploadArchive_checksum' - The SHA256 tree hash of the data being uploaded.
--
-- 'vaultName', 'uploadArchive_vaultName' - The name of the vault.
--
-- 'accountId', 'uploadArchive_accountId' - The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
--
-- 'body', 'uploadArchive_body' - The data to upload.
newUploadArchive ::
  -- | 'vaultName'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'body'
  Data.HashedBody ->
  UploadArchive
newUploadArchive pVaultName_ pAccountId_ pBody_ =
  UploadArchive'
    { archiveDescription =
        Prelude.Nothing,
      checksum = Prelude.Nothing,
      vaultName = pVaultName_,
      accountId = pAccountId_,
      body = pBody_
    }

-- | The optional description of the archive you are uploading.
uploadArchive_archiveDescription :: Lens.Lens' UploadArchive (Prelude.Maybe Prelude.Text)
uploadArchive_archiveDescription = Lens.lens (\UploadArchive' {archiveDescription} -> archiveDescription) (\s@UploadArchive' {} a -> s {archiveDescription = a} :: UploadArchive)

-- | The SHA256 tree hash of the data being uploaded.
uploadArchive_checksum :: Lens.Lens' UploadArchive (Prelude.Maybe Prelude.Text)
uploadArchive_checksum = Lens.lens (\UploadArchive' {checksum} -> checksum) (\s@UploadArchive' {} a -> s {checksum = a} :: UploadArchive)

-- | The name of the vault.
uploadArchive_vaultName :: Lens.Lens' UploadArchive Prelude.Text
uploadArchive_vaultName = Lens.lens (\UploadArchive' {vaultName} -> vaultName) (\s@UploadArchive' {} a -> s {vaultName = a} :: UploadArchive)

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- \'@-@\' (hyphen), in which case Amazon S3 Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (\'-\') in the ID.
uploadArchive_accountId :: Lens.Lens' UploadArchive Prelude.Text
uploadArchive_accountId = Lens.lens (\UploadArchive' {accountId} -> accountId) (\s@UploadArchive' {} a -> s {accountId = a} :: UploadArchive)

-- | The data to upload.
uploadArchive_body :: Lens.Lens' UploadArchive Data.HashedBody
uploadArchive_body = Lens.lens (\UploadArchive' {body} -> body) (\s@UploadArchive' {} a -> s {body = a} :: UploadArchive)

instance Core.AWSRequest UploadArchive where
  type
    AWSResponse UploadArchive =
      ArchiveCreationOutput
  request overrides =
    Request.glacierVersionHeader (Core.version defaultService)
      Prelude.. Request.postBody (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ArchiveCreationOutput'
            Prelude.<$> (h Data..#? "x-amz-archive-id")
            Prelude.<*> (h Data..#? "x-amz-sha256-tree-hash")
            Prelude.<*> (h Data..#? "Location")
      )

instance Data.ToBody UploadArchive where
  toBody UploadArchive' {..} = Data.toBody body

instance Data.ToHeaders UploadArchive where
  toHeaders UploadArchive' {..} =
    Prelude.mconcat
      [ "x-amz-archive-description"
          Data.=# archiveDescription,
        "x-amz-sha256-tree-hash" Data.=# checksum
      ]

instance Data.ToPath UploadArchive where
  toPath UploadArchive' {..} =
    Prelude.mconcat
      [ "/",
        Data.toBS accountId,
        "/vaults/",
        Data.toBS vaultName,
        "/archives"
      ]

instance Data.ToQuery UploadArchive where
  toQuery = Prelude.const Prelude.mempty
