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
-- Module      : Network.AWS.StorageGateway.CreateNFSFileShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Network File System (NFS) file share on an existing file
-- gateway. In Storage Gateway, a file share is a file system mount point
-- backed by Amazon S3 cloud storage. Storage Gateway exposes file shares
-- using an NFS interface. This operation is only supported for file
-- gateways.
--
-- File gateway requires AWS Security Token Service (AWS STS) to be
-- activated to enable you to create a file share. Make sure AWS STS is
-- activated in the AWS Region you are creating your file gateway in. If
-- AWS STS is not activated in the AWS Region, activate it. For information
-- about how to activate AWS STS, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating AWS STS in an AWS Region>
-- in the /AWS Identity and Access Management User Guide/.
--
-- File gateway does not support creating hard or symbolic links on a file
-- share.
module Network.AWS.StorageGateway.CreateNFSFileShare
  ( -- * Creating a Request
    CreateNFSFileShare (..),
    newCreateNFSFileShare,

    -- * Request Lenses
    createNFSFileShare_defaultStorageClass,
    createNFSFileShare_fileShareName,
    createNFSFileShare_guessMIMETypeEnabled,
    createNFSFileShare_readOnly,
    createNFSFileShare_kmsEncrypted,
    createNFSFileShare_squash,
    createNFSFileShare_notificationPolicy,
    createNFSFileShare_kmsKey,
    createNFSFileShare_tags,
    createNFSFileShare_cacheAttributes,
    createNFSFileShare_clientList,
    createNFSFileShare_objectACL,
    createNFSFileShare_nFSFileShareDefaults,
    createNFSFileShare_requesterPays,
    createNFSFileShare_clientToken,
    createNFSFileShare_gatewayARN,
    createNFSFileShare_role,
    createNFSFileShare_locationARN,

    -- * Destructuring the Response
    CreateNFSFileShareResponse (..),
    newCreateNFSFileShareResponse,

    -- * Response Lenses
    createNFSFileShareResponse_fileShareARN,
    createNFSFileShareResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | CreateNFSFileShareInput
--
-- /See:/ 'newCreateNFSFileShare' smart constructor.
data CreateNFSFileShare = CreateNFSFileShare'
  { -- | The default storage class for objects put into an Amazon S3 bucket by
    -- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
    -- Optional.
    --
    -- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
    -- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
    defaultStorageClass :: Core.Maybe Core.Text,
    -- | The name of the file share. Optional.
    --
    -- @FileShareName@ must be set if an S3 prefix name is set in
    -- @LocationARN@.
    fileShareName :: Core.Maybe Core.Text,
    -- | A value that enables guessing of the MIME type for uploaded objects
    -- based on file extensions. Set this value to @true@ to enable MIME type
    -- guessing, otherwise set to @false@. The default value is @true@.
    --
    -- Valid Values: @true@ | @false@
    guessMIMETypeEnabled :: Core.Maybe Core.Bool,
    -- | A value that sets the write status of a file share. Set this value to
    -- @true@ to set the write status to read-only, otherwise set to @false@.
    --
    -- Valid Values: @true@ | @false@
    readOnly :: Core.Maybe Core.Bool,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
    -- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Core.Maybe Core.Bool,
    -- | A value that maps a user to anonymous user.
    --
    -- Valid values are the following:
    --
    -- -   @RootSquash@: Only root is mapped to anonymous user.
    --
    -- -   @NoSquash@: No one is mapped to anonymous user.
    --
    -- -   @AllSquash@: Everyone is mapped to anonymous user.
    squash :: Core.Maybe Core.Text,
    -- | The notification policy of the file share.
    notificationPolicy :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Core.Maybe Core.Text,
    -- | A list of up to 50 tags that can be assigned to the NFS file share. Each
    -- tag is a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Core.Maybe [Tag],
    -- | Refresh cache information.
    cacheAttributes :: Core.Maybe CacheAttributes,
    -- | The list of clients that are allowed to access the file gateway. The
    -- list must contain either valid IP addresses or valid CIDR blocks.
    clientList :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A value that sets the access control list (ACL) permission for objects
    -- in the S3 bucket that a file gateway puts objects into. The default
    -- value is @private@.
    objectACL :: Core.Maybe ObjectACL,
    -- | File share default values. Optional.
    nFSFileShareDefaults :: Core.Maybe NFSFileShareDefaults,
    -- | A value that sets who pays the cost of the request and the cost
    -- associated with data download from the S3 bucket. If this value is set
    -- to @true@, the requester pays the costs; otherwise, the S3 bucket owner
    -- pays. However, the S3 bucket owner always pays the cost of storing data.
    --
    -- @RequesterPays@ is a configuration for the S3 bucket that backs the file
    -- share, so make sure that the configuration on the file share is the same
    -- as the S3 bucket configuration.
    --
    -- Valid Values: @true@ | @false@
    requesterPays :: Core.Maybe Core.Bool,
    -- | A unique string value that you supply that is used by file gateway to
    -- ensure idempotent file share creation.
    clientToken :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the file gateway on which you want to
    -- create a file share.
    gatewayARN :: Core.Text,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that a file
    -- gateway assumes when it accesses the underlying storage.
    role' :: Core.Text,
    -- | The ARN of the backend storage used for storing file data. A prefix name
    -- can be added to the S3 bucket name. It must end with a \"\/\".
    locationARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNFSFileShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultStorageClass', 'createNFSFileShare_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'fileShareName', 'createNFSFileShare_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
--
-- 'guessMIMETypeEnabled', 'createNFSFileShare_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'readOnly', 'createNFSFileShare_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsEncrypted', 'createNFSFileShare_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'squash', 'createNFSFileShare_squash' - A value that maps a user to anonymous user.
--
-- Valid values are the following:
--
-- -   @RootSquash@: Only root is mapped to anonymous user.
--
-- -   @NoSquash@: No one is mapped to anonymous user.
--
-- -   @AllSquash@: Everyone is mapped to anonymous user.
--
-- 'notificationPolicy', 'createNFSFileShare_notificationPolicy' - The notification policy of the file share.
--
-- 'kmsKey', 'createNFSFileShare_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'tags', 'createNFSFileShare_tags' - A list of up to 50 tags that can be assigned to the NFS file share. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'cacheAttributes', 'createNFSFileShare_cacheAttributes' - Refresh cache information.
--
-- 'clientList', 'createNFSFileShare_clientList' - The list of clients that are allowed to access the file gateway. The
-- list must contain either valid IP addresses or valid CIDR blocks.
--
-- 'objectACL', 'createNFSFileShare_objectACL' - A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a file gateway puts objects into. The default
-- value is @private@.
--
-- 'nFSFileShareDefaults', 'createNFSFileShare_nFSFileShareDefaults' - File share default values. Optional.
--
-- 'requesterPays', 'createNFSFileShare_requesterPays' - A value that sets who pays the cost of the request and the cost
-- associated with data download from the S3 bucket. If this value is set
-- to @true@, the requester pays the costs; otherwise, the S3 bucket owner
-- pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- @RequesterPays@ is a configuration for the S3 bucket that backs the file
-- share, so make sure that the configuration on the file share is the same
-- as the S3 bucket configuration.
--
-- Valid Values: @true@ | @false@
--
-- 'clientToken', 'createNFSFileShare_clientToken' - A unique string value that you supply that is used by file gateway to
-- ensure idempotent file share creation.
--
-- 'gatewayARN', 'createNFSFileShare_gatewayARN' - The Amazon Resource Name (ARN) of the file gateway on which you want to
-- create a file share.
--
-- 'role'', 'createNFSFileShare_role' - The ARN of the AWS Identity and Access Management (IAM) role that a file
-- gateway assumes when it accesses the underlying storage.
--
-- 'locationARN', 'createNFSFileShare_locationARN' - The ARN of the backend storage used for storing file data. A prefix name
-- can be added to the S3 bucket name. It must end with a \"\/\".
newCreateNFSFileShare ::
  -- | 'clientToken'
  Core.Text ->
  -- | 'gatewayARN'
  Core.Text ->
  -- | 'role''
  Core.Text ->
  -- | 'locationARN'
  Core.Text ->
  CreateNFSFileShare
newCreateNFSFileShare
  pClientToken_
  pGatewayARN_
  pRole_
  pLocationARN_ =
    CreateNFSFileShare'
      { defaultStorageClass =
          Core.Nothing,
        fileShareName = Core.Nothing,
        guessMIMETypeEnabled = Core.Nothing,
        readOnly = Core.Nothing,
        kmsEncrypted = Core.Nothing,
        squash = Core.Nothing,
        notificationPolicy = Core.Nothing,
        kmsKey = Core.Nothing,
        tags = Core.Nothing,
        cacheAttributes = Core.Nothing,
        clientList = Core.Nothing,
        objectACL = Core.Nothing,
        nFSFileShareDefaults = Core.Nothing,
        requesterPays = Core.Nothing,
        clientToken = pClientToken_,
        gatewayARN = pGatewayARN_,
        role' = pRole_,
        locationARN = pLocationARN_
      }

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
createNFSFileShare_defaultStorageClass :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Text)
createNFSFileShare_defaultStorageClass = Lens.lens (\CreateNFSFileShare' {defaultStorageClass} -> defaultStorageClass) (\s@CreateNFSFileShare' {} a -> s {defaultStorageClass = a} :: CreateNFSFileShare)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
createNFSFileShare_fileShareName :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Text)
createNFSFileShare_fileShareName = Lens.lens (\CreateNFSFileShare' {fileShareName} -> fileShareName) (\s@CreateNFSFileShare' {} a -> s {fileShareName = a} :: CreateNFSFileShare)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
createNFSFileShare_guessMIMETypeEnabled :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Bool)
createNFSFileShare_guessMIMETypeEnabled = Lens.lens (\CreateNFSFileShare' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@CreateNFSFileShare' {} a -> s {guessMIMETypeEnabled = a} :: CreateNFSFileShare)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
createNFSFileShare_readOnly :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Bool)
createNFSFileShare_readOnly = Lens.lens (\CreateNFSFileShare' {readOnly} -> readOnly) (\s@CreateNFSFileShare' {} a -> s {readOnly = a} :: CreateNFSFileShare)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createNFSFileShare_kmsEncrypted :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Bool)
createNFSFileShare_kmsEncrypted = Lens.lens (\CreateNFSFileShare' {kmsEncrypted} -> kmsEncrypted) (\s@CreateNFSFileShare' {} a -> s {kmsEncrypted = a} :: CreateNFSFileShare)

-- | A value that maps a user to anonymous user.
--
-- Valid values are the following:
--
-- -   @RootSquash@: Only root is mapped to anonymous user.
--
-- -   @NoSquash@: No one is mapped to anonymous user.
--
-- -   @AllSquash@: Everyone is mapped to anonymous user.
createNFSFileShare_squash :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Text)
createNFSFileShare_squash = Lens.lens (\CreateNFSFileShare' {squash} -> squash) (\s@CreateNFSFileShare' {} a -> s {squash = a} :: CreateNFSFileShare)

-- | The notification policy of the file share.
createNFSFileShare_notificationPolicy :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Text)
createNFSFileShare_notificationPolicy = Lens.lens (\CreateNFSFileShare' {notificationPolicy} -> notificationPolicy) (\s@CreateNFSFileShare' {} a -> s {notificationPolicy = a} :: CreateNFSFileShare)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createNFSFileShare_kmsKey :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Text)
createNFSFileShare_kmsKey = Lens.lens (\CreateNFSFileShare' {kmsKey} -> kmsKey) (\s@CreateNFSFileShare' {} a -> s {kmsKey = a} :: CreateNFSFileShare)

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createNFSFileShare_tags :: Lens.Lens' CreateNFSFileShare (Core.Maybe [Tag])
createNFSFileShare_tags = Lens.lens (\CreateNFSFileShare' {tags} -> tags) (\s@CreateNFSFileShare' {} a -> s {tags = a} :: CreateNFSFileShare) Core.. Lens.mapping Lens._Coerce

-- | Refresh cache information.
createNFSFileShare_cacheAttributes :: Lens.Lens' CreateNFSFileShare (Core.Maybe CacheAttributes)
createNFSFileShare_cacheAttributes = Lens.lens (\CreateNFSFileShare' {cacheAttributes} -> cacheAttributes) (\s@CreateNFSFileShare' {} a -> s {cacheAttributes = a} :: CreateNFSFileShare)

-- | The list of clients that are allowed to access the file gateway. The
-- list must contain either valid IP addresses or valid CIDR blocks.
createNFSFileShare_clientList :: Lens.Lens' CreateNFSFileShare (Core.Maybe (Core.NonEmpty Core.Text))
createNFSFileShare_clientList = Lens.lens (\CreateNFSFileShare' {clientList} -> clientList) (\s@CreateNFSFileShare' {} a -> s {clientList = a} :: CreateNFSFileShare) Core.. Lens.mapping Lens._Coerce

-- | A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a file gateway puts objects into. The default
-- value is @private@.
createNFSFileShare_objectACL :: Lens.Lens' CreateNFSFileShare (Core.Maybe ObjectACL)
createNFSFileShare_objectACL = Lens.lens (\CreateNFSFileShare' {objectACL} -> objectACL) (\s@CreateNFSFileShare' {} a -> s {objectACL = a} :: CreateNFSFileShare)

-- | File share default values. Optional.
createNFSFileShare_nFSFileShareDefaults :: Lens.Lens' CreateNFSFileShare (Core.Maybe NFSFileShareDefaults)
createNFSFileShare_nFSFileShareDefaults = Lens.lens (\CreateNFSFileShare' {nFSFileShareDefaults} -> nFSFileShareDefaults) (\s@CreateNFSFileShare' {} a -> s {nFSFileShareDefaults = a} :: CreateNFSFileShare)

-- | A value that sets who pays the cost of the request and the cost
-- associated with data download from the S3 bucket. If this value is set
-- to @true@, the requester pays the costs; otherwise, the S3 bucket owner
-- pays. However, the S3 bucket owner always pays the cost of storing data.
--
-- @RequesterPays@ is a configuration for the S3 bucket that backs the file
-- share, so make sure that the configuration on the file share is the same
-- as the S3 bucket configuration.
--
-- Valid Values: @true@ | @false@
createNFSFileShare_requesterPays :: Lens.Lens' CreateNFSFileShare (Core.Maybe Core.Bool)
createNFSFileShare_requesterPays = Lens.lens (\CreateNFSFileShare' {requesterPays} -> requesterPays) (\s@CreateNFSFileShare' {} a -> s {requesterPays = a} :: CreateNFSFileShare)

-- | A unique string value that you supply that is used by file gateway to
-- ensure idempotent file share creation.
createNFSFileShare_clientToken :: Lens.Lens' CreateNFSFileShare Core.Text
createNFSFileShare_clientToken = Lens.lens (\CreateNFSFileShare' {clientToken} -> clientToken) (\s@CreateNFSFileShare' {} a -> s {clientToken = a} :: CreateNFSFileShare)

-- | The Amazon Resource Name (ARN) of the file gateway on which you want to
-- create a file share.
createNFSFileShare_gatewayARN :: Lens.Lens' CreateNFSFileShare Core.Text
createNFSFileShare_gatewayARN = Lens.lens (\CreateNFSFileShare' {gatewayARN} -> gatewayARN) (\s@CreateNFSFileShare' {} a -> s {gatewayARN = a} :: CreateNFSFileShare)

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file
-- gateway assumes when it accesses the underlying storage.
createNFSFileShare_role :: Lens.Lens' CreateNFSFileShare Core.Text
createNFSFileShare_role = Lens.lens (\CreateNFSFileShare' {role'} -> role') (\s@CreateNFSFileShare' {} a -> s {role' = a} :: CreateNFSFileShare)

-- | The ARN of the backend storage used for storing file data. A prefix name
-- can be added to the S3 bucket name. It must end with a \"\/\".
createNFSFileShare_locationARN :: Lens.Lens' CreateNFSFileShare Core.Text
createNFSFileShare_locationARN = Lens.lens (\CreateNFSFileShare' {locationARN} -> locationARN) (\s@CreateNFSFileShare' {} a -> s {locationARN = a} :: CreateNFSFileShare)

instance Core.AWSRequest CreateNFSFileShare where
  type
    AWSResponse CreateNFSFileShare =
      CreateNFSFileShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNFSFileShareResponse'
            Core.<$> (x Core..?> "FileShareARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateNFSFileShare

instance Core.NFData CreateNFSFileShare

instance Core.ToHeaders CreateNFSFileShare where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CreateNFSFileShare" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateNFSFileShare where
  toJSON CreateNFSFileShare' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultStorageClass" Core..=)
              Core.<$> defaultStorageClass,
            ("FileShareName" Core..=) Core.<$> fileShareName,
            ("GuessMIMETypeEnabled" Core..=)
              Core.<$> guessMIMETypeEnabled,
            ("ReadOnly" Core..=) Core.<$> readOnly,
            ("KMSEncrypted" Core..=) Core.<$> kmsEncrypted,
            ("Squash" Core..=) Core.<$> squash,
            ("NotificationPolicy" Core..=)
              Core.<$> notificationPolicy,
            ("KMSKey" Core..=) Core.<$> kmsKey,
            ("Tags" Core..=) Core.<$> tags,
            ("CacheAttributes" Core..=) Core.<$> cacheAttributes,
            ("ClientList" Core..=) Core.<$> clientList,
            ("ObjectACL" Core..=) Core.<$> objectACL,
            ("NFSFileShareDefaults" Core..=)
              Core.<$> nFSFileShareDefaults,
            ("RequesterPays" Core..=) Core.<$> requesterPays,
            Core.Just ("ClientToken" Core..= clientToken),
            Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("Role" Core..= role'),
            Core.Just ("LocationARN" Core..= locationARN)
          ]
      )

instance Core.ToPath CreateNFSFileShare where
  toPath = Core.const "/"

instance Core.ToQuery CreateNFSFileShare where
  toQuery = Core.const Core.mempty

-- | CreateNFSFileShareOutput
--
-- /See:/ 'newCreateNFSFileShareResponse' smart constructor.
data CreateNFSFileShareResponse = CreateNFSFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the newly created file share.
    fileShareARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNFSFileShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARN', 'createNFSFileShareResponse_fileShareARN' - The Amazon Resource Name (ARN) of the newly created file share.
--
-- 'httpStatus', 'createNFSFileShareResponse_httpStatus' - The response's http status code.
newCreateNFSFileShareResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateNFSFileShareResponse
newCreateNFSFileShareResponse pHttpStatus_ =
  CreateNFSFileShareResponse'
    { fileShareARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
createNFSFileShareResponse_fileShareARN :: Lens.Lens' CreateNFSFileShareResponse (Core.Maybe Core.Text)
createNFSFileShareResponse_fileShareARN = Lens.lens (\CreateNFSFileShareResponse' {fileShareARN} -> fileShareARN) (\s@CreateNFSFileShareResponse' {} a -> s {fileShareARN = a} :: CreateNFSFileShareResponse)

-- | The response's http status code.
createNFSFileShareResponse_httpStatus :: Lens.Lens' CreateNFSFileShareResponse Core.Int
createNFSFileShareResponse_httpStatus = Lens.lens (\CreateNFSFileShareResponse' {httpStatus} -> httpStatus) (\s@CreateNFSFileShareResponse' {} a -> s {httpStatus = a} :: CreateNFSFileShareResponse)

instance Core.NFData CreateNFSFileShareResponse
