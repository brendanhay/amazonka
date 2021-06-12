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
-- Module      : Network.AWS.StorageGateway.CreateSMBFileShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Server Message Block (SMB) file share on an existing file
-- gateway. In Storage Gateway, a file share is a file system mount point
-- backed by Amazon S3 cloud storage. Storage Gateway exposes file shares
-- using an SMB interface. This operation is only supported for file
-- gateways.
--
-- File gateways require AWS Security Token Service (AWS STS) to be
-- activated to enable you to create a file share. Make sure that AWS STS
-- is activated in the AWS Region you are creating your file gateway in. If
-- AWS STS is not activated in this AWS Region, activate it. For
-- information about how to activate AWS STS, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating AWS STS in an AWS Region>
-- in the /AWS Identity and Access Management User Guide/.
--
-- File gateways don\'t support creating hard or symbolic links on a file
-- share.
module Network.AWS.StorageGateway.CreateSMBFileShare
  ( -- * Creating a Request
    CreateSMBFileShare (..),
    newCreateSMBFileShare,

    -- * Request Lenses
    createSMBFileShare_sMBACLEnabled,
    createSMBFileShare_accessBasedEnumeration,
    createSMBFileShare_defaultStorageClass,
    createSMBFileShare_fileShareName,
    createSMBFileShare_caseSensitivity,
    createSMBFileShare_guessMIMETypeEnabled,
    createSMBFileShare_readOnly,
    createSMBFileShare_kmsEncrypted,
    createSMBFileShare_authentication,
    createSMBFileShare_notificationPolicy,
    createSMBFileShare_validUserList,
    createSMBFileShare_kmsKey,
    createSMBFileShare_adminUserList,
    createSMBFileShare_auditDestinationARN,
    createSMBFileShare_tags,
    createSMBFileShare_cacheAttributes,
    createSMBFileShare_objectACL,
    createSMBFileShare_requesterPays,
    createSMBFileShare_invalidUserList,
    createSMBFileShare_clientToken,
    createSMBFileShare_gatewayARN,
    createSMBFileShare_role,
    createSMBFileShare_locationARN,

    -- * Destructuring the Response
    CreateSMBFileShareResponse (..),
    newCreateSMBFileShareResponse,

    -- * Response Lenses
    createSMBFileShareResponse_fileShareARN,
    createSMBFileShareResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | CreateSMBFileShareInput
--
-- /See:/ 'newCreateSMBFileShare' smart constructor.
data CreateSMBFileShare = CreateSMBFileShare'
  { -- | Set this value to @true@ to enable access control list (ACL) on the SMB
    -- file share. Set it to @false@ to map file and directory permissions to
    -- the POSIX permissions.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
    -- in the /AWS Storage Gateway User Guide/.
    --
    -- Valid Values: @true@ | @false@
    sMBACLEnabled :: Core.Maybe Core.Bool,
    -- | The files and folders on this share will only be visible to users with
    -- read access.
    accessBasedEnumeration :: Core.Maybe Core.Bool,
    -- | The default storage class for objects put into an Amazon S3 bucket by
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
    -- | The case of an object name in an Amazon S3 bucket. For
    -- @ClientSpecified@, the client determines the case sensitivity. For
    -- @CaseSensitive@, the gateway determines the case sensitivity. The
    -- default value is @ClientSpecified@.
    caseSensitivity :: Core.Maybe CaseSensitivity,
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
    -- | The authentication method that users use to access the file share. The
    -- default is @ActiveDirectory@.
    --
    -- Valid Values: @ActiveDirectory@ | @GuestAccess@
    authentication :: Core.Maybe Core.Text,
    -- | The notification policy of the file share.
    notificationPolicy :: Core.Maybe Core.Text,
    -- | A list of users or groups in the Active Directory that are allowed to
    -- access the file < > share. A group must be prefixed with the \@
    -- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    validUserList :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Core.Maybe Core.Text,
    -- | A list of users or groups in the Active Directory that will be granted
    -- administrator privileges on the file share. These users can do all file
    -- operations as the super-user. Acceptable formats include:
    -- @DOMAIN\\User1@, @user1@, @\@group1@, and @\@DOMAIN\\group1@.
    --
    -- Use this option very carefully, because any user in this list can do
    -- anything they like on the file share, regardless of file permissions.
    adminUserList :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Core.Maybe Core.Text,
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
    -- | A value that sets the access control list (ACL) permission for objects
    -- in the S3 bucket that a file gateway puts objects into. The default
    -- value is @private@.
    objectACL :: Core.Maybe ObjectACL,
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
    -- | A list of users or groups in the Active Directory that are not allowed
    -- to access the file share. A group must be prefixed with the \@
    -- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    invalidUserList :: Core.Maybe [Core.Text],
    -- | A unique string value that you supply that is used by file gateway to
    -- ensure idempotent file share creation.
    clientToken :: Core.Text,
    -- | The ARN of the file gateway on which you want to create a file share.
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
-- Create a value of 'CreateSMBFileShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sMBACLEnabled', 'createSMBFileShare_sMBACLEnabled' - Set this value to @true@ to enable access control list (ACL) on the SMB
-- file share. Set it to @false@ to map file and directory permissions to
-- the POSIX permissions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /AWS Storage Gateway User Guide/.
--
-- Valid Values: @true@ | @false@
--
-- 'accessBasedEnumeration', 'createSMBFileShare_accessBasedEnumeration' - The files and folders on this share will only be visible to users with
-- read access.
--
-- 'defaultStorageClass', 'createSMBFileShare_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'fileShareName', 'createSMBFileShare_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
--
-- 'caseSensitivity', 'createSMBFileShare_caseSensitivity' - The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
--
-- 'guessMIMETypeEnabled', 'createSMBFileShare_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'readOnly', 'createSMBFileShare_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsEncrypted', 'createSMBFileShare_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'authentication', 'createSMBFileShare_authentication' - The authentication method that users use to access the file share. The
-- default is @ActiveDirectory@.
--
-- Valid Values: @ActiveDirectory@ | @GuestAccess@
--
-- 'notificationPolicy', 'createSMBFileShare_notificationPolicy' - The notification policy of the file share.
--
-- 'validUserList', 'createSMBFileShare_validUserList' - A list of users or groups in the Active Directory that are allowed to
-- access the file < > share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'kmsKey', 'createSMBFileShare_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'adminUserList', 'createSMBFileShare_adminUserList' - A list of users or groups in the Active Directory that will be granted
-- administrator privileges on the file share. These users can do all file
-- operations as the super-user. Acceptable formats include:
-- @DOMAIN\\User1@, @user1@, @\@group1@, and @\@DOMAIN\\group1@.
--
-- Use this option very carefully, because any user in this list can do
-- anything they like on the file share, regardless of file permissions.
--
-- 'auditDestinationARN', 'createSMBFileShare_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- 'tags', 'createSMBFileShare_tags' - A list of up to 50 tags that can be assigned to the NFS file share. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'cacheAttributes', 'createSMBFileShare_cacheAttributes' - Refresh cache information.
--
-- 'objectACL', 'createSMBFileShare_objectACL' - A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a file gateway puts objects into. The default
-- value is @private@.
--
-- 'requesterPays', 'createSMBFileShare_requesterPays' - A value that sets who pays the cost of the request and the cost
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
-- 'invalidUserList', 'createSMBFileShare_invalidUserList' - A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'clientToken', 'createSMBFileShare_clientToken' - A unique string value that you supply that is used by file gateway to
-- ensure idempotent file share creation.
--
-- 'gatewayARN', 'createSMBFileShare_gatewayARN' - The ARN of the file gateway on which you want to create a file share.
--
-- 'role'', 'createSMBFileShare_role' - The ARN of the AWS Identity and Access Management (IAM) role that a file
-- gateway assumes when it accesses the underlying storage.
--
-- 'locationARN', 'createSMBFileShare_locationARN' - The ARN of the backend storage used for storing file data. A prefix name
-- can be added to the S3 bucket name. It must end with a \"\/\".
newCreateSMBFileShare ::
  -- | 'clientToken'
  Core.Text ->
  -- | 'gatewayARN'
  Core.Text ->
  -- | 'role''
  Core.Text ->
  -- | 'locationARN'
  Core.Text ->
  CreateSMBFileShare
newCreateSMBFileShare
  pClientToken_
  pGatewayARN_
  pRole_
  pLocationARN_ =
    CreateSMBFileShare'
      { sMBACLEnabled = Core.Nothing,
        accessBasedEnumeration = Core.Nothing,
        defaultStorageClass = Core.Nothing,
        fileShareName = Core.Nothing,
        caseSensitivity = Core.Nothing,
        guessMIMETypeEnabled = Core.Nothing,
        readOnly = Core.Nothing,
        kmsEncrypted = Core.Nothing,
        authentication = Core.Nothing,
        notificationPolicy = Core.Nothing,
        validUserList = Core.Nothing,
        kmsKey = Core.Nothing,
        adminUserList = Core.Nothing,
        auditDestinationARN = Core.Nothing,
        tags = Core.Nothing,
        cacheAttributes = Core.Nothing,
        objectACL = Core.Nothing,
        requesterPays = Core.Nothing,
        invalidUserList = Core.Nothing,
        clientToken = pClientToken_,
        gatewayARN = pGatewayARN_,
        role' = pRole_,
        locationARN = pLocationARN_
      }

-- | Set this value to @true@ to enable access control list (ACL) on the SMB
-- file share. Set it to @false@ to map file and directory permissions to
-- the POSIX permissions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /AWS Storage Gateway User Guide/.
--
-- Valid Values: @true@ | @false@
createSMBFileShare_sMBACLEnabled :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
createSMBFileShare_sMBACLEnabled = Lens.lens (\CreateSMBFileShare' {sMBACLEnabled} -> sMBACLEnabled) (\s@CreateSMBFileShare' {} a -> s {sMBACLEnabled = a} :: CreateSMBFileShare)

-- | The files and folders on this share will only be visible to users with
-- read access.
createSMBFileShare_accessBasedEnumeration :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
createSMBFileShare_accessBasedEnumeration = Lens.lens (\CreateSMBFileShare' {accessBasedEnumeration} -> accessBasedEnumeration) (\s@CreateSMBFileShare' {} a -> s {accessBasedEnumeration = a} :: CreateSMBFileShare)

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
createSMBFileShare_defaultStorageClass :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Text)
createSMBFileShare_defaultStorageClass = Lens.lens (\CreateSMBFileShare' {defaultStorageClass} -> defaultStorageClass) (\s@CreateSMBFileShare' {} a -> s {defaultStorageClass = a} :: CreateSMBFileShare)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
createSMBFileShare_fileShareName :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Text)
createSMBFileShare_fileShareName = Lens.lens (\CreateSMBFileShare' {fileShareName} -> fileShareName) (\s@CreateSMBFileShare' {} a -> s {fileShareName = a} :: CreateSMBFileShare)

-- | The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
createSMBFileShare_caseSensitivity :: Lens.Lens' CreateSMBFileShare (Core.Maybe CaseSensitivity)
createSMBFileShare_caseSensitivity = Lens.lens (\CreateSMBFileShare' {caseSensitivity} -> caseSensitivity) (\s@CreateSMBFileShare' {} a -> s {caseSensitivity = a} :: CreateSMBFileShare)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
createSMBFileShare_guessMIMETypeEnabled :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
createSMBFileShare_guessMIMETypeEnabled = Lens.lens (\CreateSMBFileShare' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@CreateSMBFileShare' {} a -> s {guessMIMETypeEnabled = a} :: CreateSMBFileShare)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
createSMBFileShare_readOnly :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
createSMBFileShare_readOnly = Lens.lens (\CreateSMBFileShare' {readOnly} -> readOnly) (\s@CreateSMBFileShare' {} a -> s {readOnly = a} :: CreateSMBFileShare)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createSMBFileShare_kmsEncrypted :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
createSMBFileShare_kmsEncrypted = Lens.lens (\CreateSMBFileShare' {kmsEncrypted} -> kmsEncrypted) (\s@CreateSMBFileShare' {} a -> s {kmsEncrypted = a} :: CreateSMBFileShare)

-- | The authentication method that users use to access the file share. The
-- default is @ActiveDirectory@.
--
-- Valid Values: @ActiveDirectory@ | @GuestAccess@
createSMBFileShare_authentication :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Text)
createSMBFileShare_authentication = Lens.lens (\CreateSMBFileShare' {authentication} -> authentication) (\s@CreateSMBFileShare' {} a -> s {authentication = a} :: CreateSMBFileShare)

-- | The notification policy of the file share.
createSMBFileShare_notificationPolicy :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Text)
createSMBFileShare_notificationPolicy = Lens.lens (\CreateSMBFileShare' {notificationPolicy} -> notificationPolicy) (\s@CreateSMBFileShare' {} a -> s {notificationPolicy = a} :: CreateSMBFileShare)

-- | A list of users or groups in the Active Directory that are allowed to
-- access the file < > share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
createSMBFileShare_validUserList :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Core.Text])
createSMBFileShare_validUserList = Lens.lens (\CreateSMBFileShare' {validUserList} -> validUserList) (\s@CreateSMBFileShare' {} a -> s {validUserList = a} :: CreateSMBFileShare) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createSMBFileShare_kmsKey :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Text)
createSMBFileShare_kmsKey = Lens.lens (\CreateSMBFileShare' {kmsKey} -> kmsKey) (\s@CreateSMBFileShare' {} a -> s {kmsKey = a} :: CreateSMBFileShare)

-- | A list of users or groups in the Active Directory that will be granted
-- administrator privileges on the file share. These users can do all file
-- operations as the super-user. Acceptable formats include:
-- @DOMAIN\\User1@, @user1@, @\@group1@, and @\@DOMAIN\\group1@.
--
-- Use this option very carefully, because any user in this list can do
-- anything they like on the file share, regardless of file permissions.
createSMBFileShare_adminUserList :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Core.Text])
createSMBFileShare_adminUserList = Lens.lens (\CreateSMBFileShare' {adminUserList} -> adminUserList) (\s@CreateSMBFileShare' {} a -> s {adminUserList = a} :: CreateSMBFileShare) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
createSMBFileShare_auditDestinationARN :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Text)
createSMBFileShare_auditDestinationARN = Lens.lens (\CreateSMBFileShare' {auditDestinationARN} -> auditDestinationARN) (\s@CreateSMBFileShare' {} a -> s {auditDestinationARN = a} :: CreateSMBFileShare)

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createSMBFileShare_tags :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Tag])
createSMBFileShare_tags = Lens.lens (\CreateSMBFileShare' {tags} -> tags) (\s@CreateSMBFileShare' {} a -> s {tags = a} :: CreateSMBFileShare) Core.. Lens.mapping Lens._Coerce

-- | Refresh cache information.
createSMBFileShare_cacheAttributes :: Lens.Lens' CreateSMBFileShare (Core.Maybe CacheAttributes)
createSMBFileShare_cacheAttributes = Lens.lens (\CreateSMBFileShare' {cacheAttributes} -> cacheAttributes) (\s@CreateSMBFileShare' {} a -> s {cacheAttributes = a} :: CreateSMBFileShare)

-- | A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a file gateway puts objects into. The default
-- value is @private@.
createSMBFileShare_objectACL :: Lens.Lens' CreateSMBFileShare (Core.Maybe ObjectACL)
createSMBFileShare_objectACL = Lens.lens (\CreateSMBFileShare' {objectACL} -> objectACL) (\s@CreateSMBFileShare' {} a -> s {objectACL = a} :: CreateSMBFileShare)

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
createSMBFileShare_requesterPays :: Lens.Lens' CreateSMBFileShare (Core.Maybe Core.Bool)
createSMBFileShare_requesterPays = Lens.lens (\CreateSMBFileShare' {requesterPays} -> requesterPays) (\s@CreateSMBFileShare' {} a -> s {requesterPays = a} :: CreateSMBFileShare)

-- | A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
createSMBFileShare_invalidUserList :: Lens.Lens' CreateSMBFileShare (Core.Maybe [Core.Text])
createSMBFileShare_invalidUserList = Lens.lens (\CreateSMBFileShare' {invalidUserList} -> invalidUserList) (\s@CreateSMBFileShare' {} a -> s {invalidUserList = a} :: CreateSMBFileShare) Core.. Lens.mapping Lens._Coerce

-- | A unique string value that you supply that is used by file gateway to
-- ensure idempotent file share creation.
createSMBFileShare_clientToken :: Lens.Lens' CreateSMBFileShare Core.Text
createSMBFileShare_clientToken = Lens.lens (\CreateSMBFileShare' {clientToken} -> clientToken) (\s@CreateSMBFileShare' {} a -> s {clientToken = a} :: CreateSMBFileShare)

-- | The ARN of the file gateway on which you want to create a file share.
createSMBFileShare_gatewayARN :: Lens.Lens' CreateSMBFileShare Core.Text
createSMBFileShare_gatewayARN = Lens.lens (\CreateSMBFileShare' {gatewayARN} -> gatewayARN) (\s@CreateSMBFileShare' {} a -> s {gatewayARN = a} :: CreateSMBFileShare)

-- | The ARN of the AWS Identity and Access Management (IAM) role that a file
-- gateway assumes when it accesses the underlying storage.
createSMBFileShare_role :: Lens.Lens' CreateSMBFileShare Core.Text
createSMBFileShare_role = Lens.lens (\CreateSMBFileShare' {role'} -> role') (\s@CreateSMBFileShare' {} a -> s {role' = a} :: CreateSMBFileShare)

-- | The ARN of the backend storage used for storing file data. A prefix name
-- can be added to the S3 bucket name. It must end with a \"\/\".
createSMBFileShare_locationARN :: Lens.Lens' CreateSMBFileShare Core.Text
createSMBFileShare_locationARN = Lens.lens (\CreateSMBFileShare' {locationARN} -> locationARN) (\s@CreateSMBFileShare' {} a -> s {locationARN = a} :: CreateSMBFileShare)

instance Core.AWSRequest CreateSMBFileShare where
  type
    AWSResponse CreateSMBFileShare =
      CreateSMBFileShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSMBFileShareResponse'
            Core.<$> (x Core..?> "FileShareARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSMBFileShare

instance Core.NFData CreateSMBFileShare

instance Core.ToHeaders CreateSMBFileShare where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CreateSMBFileShare" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateSMBFileShare where
  toJSON CreateSMBFileShare' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SMBACLEnabled" Core..=) Core.<$> sMBACLEnabled,
            ("AccessBasedEnumeration" Core..=)
              Core.<$> accessBasedEnumeration,
            ("DefaultStorageClass" Core..=)
              Core.<$> defaultStorageClass,
            ("FileShareName" Core..=) Core.<$> fileShareName,
            ("CaseSensitivity" Core..=) Core.<$> caseSensitivity,
            ("GuessMIMETypeEnabled" Core..=)
              Core.<$> guessMIMETypeEnabled,
            ("ReadOnly" Core..=) Core.<$> readOnly,
            ("KMSEncrypted" Core..=) Core.<$> kmsEncrypted,
            ("Authentication" Core..=) Core.<$> authentication,
            ("NotificationPolicy" Core..=)
              Core.<$> notificationPolicy,
            ("ValidUserList" Core..=) Core.<$> validUserList,
            ("KMSKey" Core..=) Core.<$> kmsKey,
            ("AdminUserList" Core..=) Core.<$> adminUserList,
            ("AuditDestinationARN" Core..=)
              Core.<$> auditDestinationARN,
            ("Tags" Core..=) Core.<$> tags,
            ("CacheAttributes" Core..=) Core.<$> cacheAttributes,
            ("ObjectACL" Core..=) Core.<$> objectACL,
            ("RequesterPays" Core..=) Core.<$> requesterPays,
            ("InvalidUserList" Core..=) Core.<$> invalidUserList,
            Core.Just ("ClientToken" Core..= clientToken),
            Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("Role" Core..= role'),
            Core.Just ("LocationARN" Core..= locationARN)
          ]
      )

instance Core.ToPath CreateSMBFileShare where
  toPath = Core.const "/"

instance Core.ToQuery CreateSMBFileShare where
  toQuery = Core.const Core.mempty

-- | CreateSMBFileShareOutput
--
-- /See:/ 'newCreateSMBFileShareResponse' smart constructor.
data CreateSMBFileShareResponse = CreateSMBFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the newly created file share.
    fileShareARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSMBFileShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARN', 'createSMBFileShareResponse_fileShareARN' - The Amazon Resource Name (ARN) of the newly created file share.
--
-- 'httpStatus', 'createSMBFileShareResponse_httpStatus' - The response's http status code.
newCreateSMBFileShareResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSMBFileShareResponse
newCreateSMBFileShareResponse pHttpStatus_ =
  CreateSMBFileShareResponse'
    { fileShareARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
createSMBFileShareResponse_fileShareARN :: Lens.Lens' CreateSMBFileShareResponse (Core.Maybe Core.Text)
createSMBFileShareResponse_fileShareARN = Lens.lens (\CreateSMBFileShareResponse' {fileShareARN} -> fileShareARN) (\s@CreateSMBFileShareResponse' {} a -> s {fileShareARN = a} :: CreateSMBFileShareResponse)

-- | The response's http status code.
createSMBFileShareResponse_httpStatus :: Lens.Lens' CreateSMBFileShareResponse Core.Int
createSMBFileShareResponse_httpStatus = Lens.lens (\CreateSMBFileShareResponse' {httpStatus} -> httpStatus) (\s@CreateSMBFileShareResponse' {} a -> s {httpStatus = a} :: CreateSMBFileShareResponse)

instance Core.NFData CreateSMBFileShareResponse
