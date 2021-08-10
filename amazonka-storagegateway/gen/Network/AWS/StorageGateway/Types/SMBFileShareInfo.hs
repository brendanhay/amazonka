{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.SMBFileShareInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.SMBFileShareInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StorageGateway.Types.CacheAttributes
import Network.AWS.StorageGateway.Types.CaseSensitivity
import Network.AWS.StorageGateway.Types.ObjectACL
import Network.AWS.StorageGateway.Types.Tag

-- | The Windows file permissions and ownership information assigned, by
-- default, to native S3 objects when file gateway discovers them in S3
-- buckets. This operation is only supported for file gateways.
--
-- /See:/ 'newSMBFileShareInfo' smart constructor.
data SMBFileShareInfo = SMBFileShareInfo'
  { -- | If this value is set to @true@, it indicates that access control list
    -- (ACL) is enabled on the SMB file share. If it is set to @false@, it
    -- indicates that file and directory permissions are mapped to the POSIX
    -- permission.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
    -- in the /AWS Storage Gateway User Guide/.
    sMBACLEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether @AccessBasedEnumeration@ is enabled.
    accessBasedEnumeration :: Prelude.Maybe Prelude.Bool,
    -- | The default storage class for objects put into an Amazon S3 bucket by
    -- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
    -- Optional.
    --
    -- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
    -- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
    defaultStorageClass :: Prelude.Maybe Prelude.Text,
    -- | The name of the file share. Optional.
    --
    -- @FileShareName@ must be set if an S3 prefix name is set in
    -- @LocationARN@.
    fileShareName :: Prelude.Maybe Prelude.Text,
    -- | The case of an object name in an Amazon S3 bucket. For
    -- @ClientSpecified@, the client determines the case sensitivity. For
    -- @CaseSensitive@, the gateway determines the case sensitivity. The
    -- default value is @ClientSpecified@.
    caseSensitivity :: Prelude.Maybe CaseSensitivity,
    -- | A value that enables guessing of the MIME type for uploaded objects
    -- based on file extensions. Set this value to @true@ to enable MIME type
    -- guessing, otherwise set to @false@. The default value is @true@.
    --
    -- Valid Values: @true@ | @false@
    guessMIMETypeEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A value that sets the write status of a file share. Set this value to
    -- @true@ to set the write status to read-only, otherwise set to @false@.
    --
    -- Valid Values: @true@ | @false@
    readOnly :: Prelude.Maybe Prelude.Bool,
    fileShareId :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
    -- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Prelude.Maybe Prelude.Bool,
    authentication :: Prelude.Maybe Prelude.Text,
    locationARN :: Prelude.Maybe Prelude.Text,
    -- | The notification policy of the file share.
    notificationPolicy :: Prelude.Maybe Prelude.Text,
    -- | A list of users or groups in the Active Directory that are allowed to
    -- access the file share. A group must be prefixed with the \@ character.
    -- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
    -- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
    -- @ActiveDirectory@.
    validUserList :: Prelude.Maybe [Prelude.Text],
    kmsKey :: Prelude.Maybe Prelude.Text,
    fileShareStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of users or groups in the Active Directory that have
    -- administrator rights to the file share. A group must be prefixed with
    -- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    adminUserList :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Prelude.Maybe Prelude.Text,
    role' :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 50 tags assigned to the SMB file share, sorted
    -- alphabetically by key name. Each tag is a key-value pair. For a gateway
    -- with more than 10 tags assigned, you can view all tags using the
    -- @ListTagsForResource@ API operation.
    tags :: Prelude.Maybe [Tag],
    fileShareARN :: Prelude.Maybe Prelude.Text,
    -- | Refresh cache information.
    cacheAttributes :: Prelude.Maybe CacheAttributes,
    objectACL :: Prelude.Maybe ObjectACL,
    -- | The file share path used by the SMB client to identify the mount point.
    path :: Prelude.Maybe Prelude.Text,
    gatewayARN :: Prelude.Maybe Prelude.Text,
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
    requesterPays :: Prelude.Maybe Prelude.Bool,
    -- | A list of users or groups in the Active Directory that are not allowed
    -- to access the file share. A group must be prefixed with the \@
    -- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    invalidUserList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SMBFileShareInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sMBACLEnabled', 'sMBFileShareInfo_sMBACLEnabled' - If this value is set to @true@, it indicates that access control list
-- (ACL) is enabled on the SMB file share. If it is set to @false@, it
-- indicates that file and directory permissions are mapped to the POSIX
-- permission.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /AWS Storage Gateway User Guide/.
--
-- 'accessBasedEnumeration', 'sMBFileShareInfo_accessBasedEnumeration' - Indicates whether @AccessBasedEnumeration@ is enabled.
--
-- 'defaultStorageClass', 'sMBFileShareInfo_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'fileShareName', 'sMBFileShareInfo_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
--
-- 'caseSensitivity', 'sMBFileShareInfo_caseSensitivity' - The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
--
-- 'guessMIMETypeEnabled', 'sMBFileShareInfo_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'readOnly', 'sMBFileShareInfo_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'fileShareId', 'sMBFileShareInfo_fileShareId' - Undocumented member.
--
-- 'kmsEncrypted', 'sMBFileShareInfo_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'authentication', 'sMBFileShareInfo_authentication' - Undocumented member.
--
-- 'locationARN', 'sMBFileShareInfo_locationARN' - Undocumented member.
--
-- 'notificationPolicy', 'sMBFileShareInfo_notificationPolicy' - The notification policy of the file share.
--
-- 'validUserList', 'sMBFileShareInfo_validUserList' - A list of users or groups in the Active Directory that are allowed to
-- access the file share. A group must be prefixed with the \@ character.
-- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
-- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
-- @ActiveDirectory@.
--
-- 'kmsKey', 'sMBFileShareInfo_kmsKey' - Undocumented member.
--
-- 'fileShareStatus', 'sMBFileShareInfo_fileShareStatus' - Undocumented member.
--
-- 'adminUserList', 'sMBFileShareInfo_adminUserList' - A list of users or groups in the Active Directory that have
-- administrator rights to the file share. A group must be prefixed with
-- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'auditDestinationARN', 'sMBFileShareInfo_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- 'role'', 'sMBFileShareInfo_role' - Undocumented member.
--
-- 'tags', 'sMBFileShareInfo_tags' - A list of up to 50 tags assigned to the SMB file share, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
--
-- 'fileShareARN', 'sMBFileShareInfo_fileShareARN' - Undocumented member.
--
-- 'cacheAttributes', 'sMBFileShareInfo_cacheAttributes' - Refresh cache information.
--
-- 'objectACL', 'sMBFileShareInfo_objectACL' - Undocumented member.
--
-- 'path', 'sMBFileShareInfo_path' - The file share path used by the SMB client to identify the mount point.
--
-- 'gatewayARN', 'sMBFileShareInfo_gatewayARN' - Undocumented member.
--
-- 'requesterPays', 'sMBFileShareInfo_requesterPays' - A value that sets who pays the cost of the request and the cost
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
-- 'invalidUserList', 'sMBFileShareInfo_invalidUserList' - A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
newSMBFileShareInfo ::
  SMBFileShareInfo
newSMBFileShareInfo =
  SMBFileShareInfo'
    { sMBACLEnabled = Prelude.Nothing,
      accessBasedEnumeration = Prelude.Nothing,
      defaultStorageClass = Prelude.Nothing,
      fileShareName = Prelude.Nothing,
      caseSensitivity = Prelude.Nothing,
      guessMIMETypeEnabled = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      fileShareId = Prelude.Nothing,
      kmsEncrypted = Prelude.Nothing,
      authentication = Prelude.Nothing,
      locationARN = Prelude.Nothing,
      notificationPolicy = Prelude.Nothing,
      validUserList = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      fileShareStatus = Prelude.Nothing,
      adminUserList = Prelude.Nothing,
      auditDestinationARN = Prelude.Nothing,
      role' = Prelude.Nothing,
      tags = Prelude.Nothing,
      fileShareARN = Prelude.Nothing,
      cacheAttributes = Prelude.Nothing,
      objectACL = Prelude.Nothing,
      path = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      requesterPays = Prelude.Nothing,
      invalidUserList = Prelude.Nothing
    }

-- | If this value is set to @true@, it indicates that access control list
-- (ACL) is enabled on the SMB file share. If it is set to @false@, it
-- indicates that file and directory permissions are mapped to the POSIX
-- permission.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /AWS Storage Gateway User Guide/.
sMBFileShareInfo_sMBACLEnabled :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_sMBACLEnabled = Lens.lens (\SMBFileShareInfo' {sMBACLEnabled} -> sMBACLEnabled) (\s@SMBFileShareInfo' {} a -> s {sMBACLEnabled = a} :: SMBFileShareInfo)

-- | Indicates whether @AccessBasedEnumeration@ is enabled.
sMBFileShareInfo_accessBasedEnumeration :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_accessBasedEnumeration = Lens.lens (\SMBFileShareInfo' {accessBasedEnumeration} -> accessBasedEnumeration) (\s@SMBFileShareInfo' {} a -> s {accessBasedEnumeration = a} :: SMBFileShareInfo)

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
sMBFileShareInfo_defaultStorageClass :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_defaultStorageClass = Lens.lens (\SMBFileShareInfo' {defaultStorageClass} -> defaultStorageClass) (\s@SMBFileShareInfo' {} a -> s {defaultStorageClass = a} :: SMBFileShareInfo)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
sMBFileShareInfo_fileShareName :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_fileShareName = Lens.lens (\SMBFileShareInfo' {fileShareName} -> fileShareName) (\s@SMBFileShareInfo' {} a -> s {fileShareName = a} :: SMBFileShareInfo)

-- | The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
sMBFileShareInfo_caseSensitivity :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe CaseSensitivity)
sMBFileShareInfo_caseSensitivity = Lens.lens (\SMBFileShareInfo' {caseSensitivity} -> caseSensitivity) (\s@SMBFileShareInfo' {} a -> s {caseSensitivity = a} :: SMBFileShareInfo)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_guessMIMETypeEnabled :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_guessMIMETypeEnabled = Lens.lens (\SMBFileShareInfo' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@SMBFileShareInfo' {} a -> s {guessMIMETypeEnabled = a} :: SMBFileShareInfo)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_readOnly :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_readOnly = Lens.lens (\SMBFileShareInfo' {readOnly} -> readOnly) (\s@SMBFileShareInfo' {} a -> s {readOnly = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_fileShareId :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_fileShareId = Lens.lens (\SMBFileShareInfo' {fileShareId} -> fileShareId) (\s@SMBFileShareInfo' {} a -> s {fileShareId = a} :: SMBFileShareInfo)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_kmsEncrypted :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_kmsEncrypted = Lens.lens (\SMBFileShareInfo' {kmsEncrypted} -> kmsEncrypted) (\s@SMBFileShareInfo' {} a -> s {kmsEncrypted = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_authentication :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_authentication = Lens.lens (\SMBFileShareInfo' {authentication} -> authentication) (\s@SMBFileShareInfo' {} a -> s {authentication = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_locationARN :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_locationARN = Lens.lens (\SMBFileShareInfo' {locationARN} -> locationARN) (\s@SMBFileShareInfo' {} a -> s {locationARN = a} :: SMBFileShareInfo)

-- | The notification policy of the file share.
sMBFileShareInfo_notificationPolicy :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_notificationPolicy = Lens.lens (\SMBFileShareInfo' {notificationPolicy} -> notificationPolicy) (\s@SMBFileShareInfo' {} a -> s {notificationPolicy = a} :: SMBFileShareInfo)

-- | A list of users or groups in the Active Directory that are allowed to
-- access the file share. A group must be prefixed with the \@ character.
-- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
-- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
-- @ActiveDirectory@.
sMBFileShareInfo_validUserList :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe [Prelude.Text])
sMBFileShareInfo_validUserList = Lens.lens (\SMBFileShareInfo' {validUserList} -> validUserList) (\s@SMBFileShareInfo' {} a -> s {validUserList = a} :: SMBFileShareInfo) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
sMBFileShareInfo_kmsKey :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_kmsKey = Lens.lens (\SMBFileShareInfo' {kmsKey} -> kmsKey) (\s@SMBFileShareInfo' {} a -> s {kmsKey = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_fileShareStatus :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_fileShareStatus = Lens.lens (\SMBFileShareInfo' {fileShareStatus} -> fileShareStatus) (\s@SMBFileShareInfo' {} a -> s {fileShareStatus = a} :: SMBFileShareInfo)

-- | A list of users or groups in the Active Directory that have
-- administrator rights to the file share. A group must be prefixed with
-- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
sMBFileShareInfo_adminUserList :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe [Prelude.Text])
sMBFileShareInfo_adminUserList = Lens.lens (\SMBFileShareInfo' {adminUserList} -> adminUserList) (\s@SMBFileShareInfo' {} a -> s {adminUserList = a} :: SMBFileShareInfo) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
sMBFileShareInfo_auditDestinationARN :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_auditDestinationARN = Lens.lens (\SMBFileShareInfo' {auditDestinationARN} -> auditDestinationARN) (\s@SMBFileShareInfo' {} a -> s {auditDestinationARN = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_role :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_role = Lens.lens (\SMBFileShareInfo' {role'} -> role') (\s@SMBFileShareInfo' {} a -> s {role' = a} :: SMBFileShareInfo)

-- | A list of up to 50 tags assigned to the SMB file share, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
sMBFileShareInfo_tags :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe [Tag])
sMBFileShareInfo_tags = Lens.lens (\SMBFileShareInfo' {tags} -> tags) (\s@SMBFileShareInfo' {} a -> s {tags = a} :: SMBFileShareInfo) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
sMBFileShareInfo_fileShareARN :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_fileShareARN = Lens.lens (\SMBFileShareInfo' {fileShareARN} -> fileShareARN) (\s@SMBFileShareInfo' {} a -> s {fileShareARN = a} :: SMBFileShareInfo)

-- | Refresh cache information.
sMBFileShareInfo_cacheAttributes :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe CacheAttributes)
sMBFileShareInfo_cacheAttributes = Lens.lens (\SMBFileShareInfo' {cacheAttributes} -> cacheAttributes) (\s@SMBFileShareInfo' {} a -> s {cacheAttributes = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_objectACL :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe ObjectACL)
sMBFileShareInfo_objectACL = Lens.lens (\SMBFileShareInfo' {objectACL} -> objectACL) (\s@SMBFileShareInfo' {} a -> s {objectACL = a} :: SMBFileShareInfo)

-- | The file share path used by the SMB client to identify the mount point.
sMBFileShareInfo_path :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_path = Lens.lens (\SMBFileShareInfo' {path} -> path) (\s@SMBFileShareInfo' {} a -> s {path = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_gatewayARN :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_gatewayARN = Lens.lens (\SMBFileShareInfo' {gatewayARN} -> gatewayARN) (\s@SMBFileShareInfo' {} a -> s {gatewayARN = a} :: SMBFileShareInfo)

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
sMBFileShareInfo_requesterPays :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_requesterPays = Lens.lens (\SMBFileShareInfo' {requesterPays} -> requesterPays) (\s@SMBFileShareInfo' {} a -> s {requesterPays = a} :: SMBFileShareInfo)

-- | A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
sMBFileShareInfo_invalidUserList :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe [Prelude.Text])
sMBFileShareInfo_invalidUserList = Lens.lens (\SMBFileShareInfo' {invalidUserList} -> invalidUserList) (\s@SMBFileShareInfo' {} a -> s {invalidUserList = a} :: SMBFileShareInfo) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON SMBFileShareInfo where
  parseJSON =
    Core.withObject
      "SMBFileShareInfo"
      ( \x ->
          SMBFileShareInfo'
            Prelude.<$> (x Core..:? "SMBACLEnabled")
            Prelude.<*> (x Core..:? "AccessBasedEnumeration")
            Prelude.<*> (x Core..:? "DefaultStorageClass")
            Prelude.<*> (x Core..:? "FileShareName")
            Prelude.<*> (x Core..:? "CaseSensitivity")
            Prelude.<*> (x Core..:? "GuessMIMETypeEnabled")
            Prelude.<*> (x Core..:? "ReadOnly")
            Prelude.<*> (x Core..:? "FileShareId")
            Prelude.<*> (x Core..:? "KMSEncrypted")
            Prelude.<*> (x Core..:? "Authentication")
            Prelude.<*> (x Core..:? "LocationARN")
            Prelude.<*> (x Core..:? "NotificationPolicy")
            Prelude.<*> (x Core..:? "ValidUserList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "KMSKey")
            Prelude.<*> (x Core..:? "FileShareStatus")
            Prelude.<*> (x Core..:? "AdminUserList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AuditDestinationARN")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "FileShareARN")
            Prelude.<*> (x Core..:? "CacheAttributes")
            Prelude.<*> (x Core..:? "ObjectACL")
            Prelude.<*> (x Core..:? "Path")
            Prelude.<*> (x Core..:? "GatewayARN")
            Prelude.<*> (x Core..:? "RequesterPays")
            Prelude.<*> ( x Core..:? "InvalidUserList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SMBFileShareInfo

instance Prelude.NFData SMBFileShareInfo
