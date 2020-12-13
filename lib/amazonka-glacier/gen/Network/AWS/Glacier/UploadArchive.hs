{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.UploadArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds an archive to a vault. This is a synchronous operation, and for a successful upload, your data is durably persisted. Amazon S3 Glacier returns the archive ID in the @x-amz-archive-id@ header of the response.
--
-- You must use the archive ID to access your data in Amazon S3 Glacier. After you upload an archive, you should save the archive ID returned so that you can retrieve or delete the archive later. Besides saving the archive ID, you can also index it and give it a friendly name to allow for better searching. You can also use the optional archive description field to specify how the archive is referred to in an external index of archives, such as you might create in Amazon DynamoDB. You can also get the vault inventory to obtain a list of archive IDs in a vault. For more information, see 'InitiateJob' .
-- You must provide a SHA256 tree hash of the data you are uploading. For information about computing a SHA256 tree hash, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums> .
-- You can optionally specify an archive description of up to 1,024 printable ASCII characters. You can get the archive description when you either retrieve the archive or get the vault inventory. For more information, see 'InitiateJob' . Amazon Glacier does not interpret the description in any way. An archive description does not need to be unique. You cannot use the description to retrieve or sort the archive list.
-- Archives are immutable. After you upload an archive, you cannot edit the archive or its description.
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
-- For conceptual information and underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-an-archive.html Uploading an Archive in Amazon Glacier> and <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive> in the /Amazon Glacier Developer Guide/ .
module Network.AWS.Glacier.UploadArchive
  ( -- * Creating a request
    UploadArchive (..),
    mkUploadArchive,

    -- ** Request lenses
    uaChecksum,
    uaVaultName,
    uaBody,
    uaArchiveDescription,
    uaAccountId,

    -- * Destructuring the response
    ArchiveCreationOutput (..),
    mkArchiveCreationOutput,

    -- ** Response lenses
    acoArchiveId,
    acoChecksum,
    acoLocation,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options to add an archive to a vault.
--
-- /See:/ 'mkUploadArchive' smart constructor.
data UploadArchive = UploadArchive'
  { -- | The SHA256 tree hash of the data being uploaded.
    checksum :: Lude.Maybe Lude.Text,
    -- | The name of the vault.
    vaultName :: Lude.Text,
    -- | The data to upload.
    body :: Lude.HashedBody,
    -- | The optional description of the archive you are uploading.
    archiveDescription :: Lude.Maybe Lude.Text,
    -- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'UploadArchive' with the minimum fields required to make a request.
--
-- * 'checksum' - The SHA256 tree hash of the data being uploaded.
-- * 'vaultName' - The name of the vault.
-- * 'body' - The data to upload.
-- * 'archiveDescription' - The optional description of the archive you are uploading.
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
mkUploadArchive ::
  -- | 'vaultName'
  Lude.Text ->
  -- | 'body'
  Lude.HashedBody ->
  -- | 'accountId'
  Lude.Text ->
  UploadArchive
mkUploadArchive pVaultName_ pBody_ pAccountId_ =
  UploadArchive'
    { checksum = Lude.Nothing,
      vaultName = pVaultName_,
      body = pBody_,
      archiveDescription = Lude.Nothing,
      accountId = pAccountId_
    }

-- | The SHA256 tree hash of the data being uploaded.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaChecksum :: Lens.Lens' UploadArchive (Lude.Maybe Lude.Text)
uaChecksum = Lens.lens (checksum :: UploadArchive -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: UploadArchive)
{-# DEPRECATED uaChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaVaultName :: Lens.Lens' UploadArchive Lude.Text
uaVaultName = Lens.lens (vaultName :: UploadArchive -> Lude.Text) (\s a -> s {vaultName = a} :: UploadArchive)
{-# DEPRECATED uaVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

-- | The data to upload.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaBody :: Lens.Lens' UploadArchive Lude.HashedBody
uaBody = Lens.lens (body :: UploadArchive -> Lude.HashedBody) (\s a -> s {body = a} :: UploadArchive)
{-# DEPRECATED uaBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The optional description of the archive you are uploading.
--
-- /Note:/ Consider using 'archiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaArchiveDescription :: Lens.Lens' UploadArchive (Lude.Maybe Lude.Text)
uaArchiveDescription = Lens.lens (archiveDescription :: UploadArchive -> Lude.Maybe Lude.Text) (\s a -> s {archiveDescription = a} :: UploadArchive)
{-# DEPRECATED uaArchiveDescription "Use generic-lens or generic-optics with 'archiveDescription' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAccountId :: Lens.Lens' UploadArchive Lude.Text
uaAccountId = Lens.lens (accountId :: UploadArchive -> Lude.Text) (\s a -> s {accountId = a} :: UploadArchive)
{-# DEPRECATED uaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest UploadArchive where
  type Rs UploadArchive = ArchiveCreationOutput
  request = Req.postBody glacierService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ArchiveCreationOutput'
            Lude.<$> (h Lude..#? "x-amz-archive-id")
            Lude.<*> (h Lude..#? "x-amz-sha256-tree-hash")
            Lude.<*> (h Lude..#? "Location")
      )

instance Lude.ToBody UploadArchive where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders UploadArchive where
  toHeaders UploadArchive' {..} =
    Lude.mconcat
      [ "x-amz-sha256-tree-hash" Lude.=# checksum,
        "x-amz-archive-description" Lude.=# archiveDescription
      ]

instance Lude.ToPath UploadArchive where
  toPath UploadArchive' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/archives"
      ]

instance Lude.ToQuery UploadArchive where
  toQuery = Lude.const Lude.mempty
