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
    sMBACLEnabled :: Core.Maybe Core.Bool,
    -- | Indicates whether @AccessBasedEnumeration@ is enabled.
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
    fileShareId :: Core.Maybe Core.Text,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
    -- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Core.Maybe Core.Bool,
    authentication :: Core.Maybe Core.Text,
    locationARN :: Core.Maybe Core.Text,
    -- | The notification policy of the file share.
    notificationPolicy :: Core.Maybe Core.Text,
    -- | A list of users or groups in the Active Directory that are allowed to
    -- access the file share. A group must be prefixed with the \@ character.
    -- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
    -- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
    -- @ActiveDirectory@.
    validUserList :: Core.Maybe [Core.Text],
    kmsKey :: Core.Maybe Core.Text,
    fileShareStatus :: Core.Maybe Core.Text,
    -- | A list of users or groups in the Active Directory that have
    -- administrator rights to the file share. A group must be prefixed with
    -- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    adminUserList :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Core.Maybe Core.Text,
    role' :: Core.Maybe Core.Text,
    -- | A list of up to 50 tags assigned to the SMB file share, sorted
    -- alphabetically by key name. Each tag is a key-value pair. For a gateway
    -- with more than 10 tags assigned, you can view all tags using the
    -- @ListTagsForResource@ API operation.
    tags :: Core.Maybe [Tag],
    fileShareARN :: Core.Maybe Core.Text,
    -- | Refresh cache information.
    cacheAttributes :: Core.Maybe CacheAttributes,
    objectACL :: Core.Maybe ObjectACL,
    -- | The file share path used by the SMB client to identify the mount point.
    path :: Core.Maybe Core.Text,
    gatewayARN :: Core.Maybe Core.Text,
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
    invalidUserList :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { sMBACLEnabled = Core.Nothing,
      accessBasedEnumeration = Core.Nothing,
      defaultStorageClass = Core.Nothing,
      fileShareName = Core.Nothing,
      caseSensitivity = Core.Nothing,
      guessMIMETypeEnabled = Core.Nothing,
      readOnly = Core.Nothing,
      fileShareId = Core.Nothing,
      kmsEncrypted = Core.Nothing,
      authentication = Core.Nothing,
      locationARN = Core.Nothing,
      notificationPolicy = Core.Nothing,
      validUserList = Core.Nothing,
      kmsKey = Core.Nothing,
      fileShareStatus = Core.Nothing,
      adminUserList = Core.Nothing,
      auditDestinationARN = Core.Nothing,
      role' = Core.Nothing,
      tags = Core.Nothing,
      fileShareARN = Core.Nothing,
      cacheAttributes = Core.Nothing,
      objectACL = Core.Nothing,
      path = Core.Nothing,
      gatewayARN = Core.Nothing,
      requesterPays = Core.Nothing,
      invalidUserList = Core.Nothing
    }

-- | If this value is set to @true@, it indicates that access control list
-- (ACL) is enabled on the SMB file share. If it is set to @false@, it
-- indicates that file and directory permissions are mapped to the POSIX
-- permission.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /AWS Storage Gateway User Guide/.
sMBFileShareInfo_sMBACLEnabled :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
sMBFileShareInfo_sMBACLEnabled = Lens.lens (\SMBFileShareInfo' {sMBACLEnabled} -> sMBACLEnabled) (\s@SMBFileShareInfo' {} a -> s {sMBACLEnabled = a} :: SMBFileShareInfo)

-- | Indicates whether @AccessBasedEnumeration@ is enabled.
sMBFileShareInfo_accessBasedEnumeration :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
sMBFileShareInfo_accessBasedEnumeration = Lens.lens (\SMBFileShareInfo' {accessBasedEnumeration} -> accessBasedEnumeration) (\s@SMBFileShareInfo' {} a -> s {accessBasedEnumeration = a} :: SMBFileShareInfo)

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
sMBFileShareInfo_defaultStorageClass :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_defaultStorageClass = Lens.lens (\SMBFileShareInfo' {defaultStorageClass} -> defaultStorageClass) (\s@SMBFileShareInfo' {} a -> s {defaultStorageClass = a} :: SMBFileShareInfo)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
sMBFileShareInfo_fileShareName :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_fileShareName = Lens.lens (\SMBFileShareInfo' {fileShareName} -> fileShareName) (\s@SMBFileShareInfo' {} a -> s {fileShareName = a} :: SMBFileShareInfo)

-- | The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
sMBFileShareInfo_caseSensitivity :: Lens.Lens' SMBFileShareInfo (Core.Maybe CaseSensitivity)
sMBFileShareInfo_caseSensitivity = Lens.lens (\SMBFileShareInfo' {caseSensitivity} -> caseSensitivity) (\s@SMBFileShareInfo' {} a -> s {caseSensitivity = a} :: SMBFileShareInfo)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_guessMIMETypeEnabled :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
sMBFileShareInfo_guessMIMETypeEnabled = Lens.lens (\SMBFileShareInfo' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@SMBFileShareInfo' {} a -> s {guessMIMETypeEnabled = a} :: SMBFileShareInfo)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_readOnly :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
sMBFileShareInfo_readOnly = Lens.lens (\SMBFileShareInfo' {readOnly} -> readOnly) (\s@SMBFileShareInfo' {} a -> s {readOnly = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_fileShareId :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_fileShareId = Lens.lens (\SMBFileShareInfo' {fileShareId} -> fileShareId) (\s@SMBFileShareInfo' {} a -> s {fileShareId = a} :: SMBFileShareInfo)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_kmsEncrypted :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
sMBFileShareInfo_kmsEncrypted = Lens.lens (\SMBFileShareInfo' {kmsEncrypted} -> kmsEncrypted) (\s@SMBFileShareInfo' {} a -> s {kmsEncrypted = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_authentication :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_authentication = Lens.lens (\SMBFileShareInfo' {authentication} -> authentication) (\s@SMBFileShareInfo' {} a -> s {authentication = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_locationARN :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_locationARN = Lens.lens (\SMBFileShareInfo' {locationARN} -> locationARN) (\s@SMBFileShareInfo' {} a -> s {locationARN = a} :: SMBFileShareInfo)

-- | The notification policy of the file share.
sMBFileShareInfo_notificationPolicy :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_notificationPolicy = Lens.lens (\SMBFileShareInfo' {notificationPolicy} -> notificationPolicy) (\s@SMBFileShareInfo' {} a -> s {notificationPolicy = a} :: SMBFileShareInfo)

-- | A list of users or groups in the Active Directory that are allowed to
-- access the file share. A group must be prefixed with the \@ character.
-- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
-- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
-- @ActiveDirectory@.
sMBFileShareInfo_validUserList :: Lens.Lens' SMBFileShareInfo (Core.Maybe [Core.Text])
sMBFileShareInfo_validUserList = Lens.lens (\SMBFileShareInfo' {validUserList} -> validUserList) (\s@SMBFileShareInfo' {} a -> s {validUserList = a} :: SMBFileShareInfo) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
sMBFileShareInfo_kmsKey :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_kmsKey = Lens.lens (\SMBFileShareInfo' {kmsKey} -> kmsKey) (\s@SMBFileShareInfo' {} a -> s {kmsKey = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_fileShareStatus :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_fileShareStatus = Lens.lens (\SMBFileShareInfo' {fileShareStatus} -> fileShareStatus) (\s@SMBFileShareInfo' {} a -> s {fileShareStatus = a} :: SMBFileShareInfo)

-- | A list of users or groups in the Active Directory that have
-- administrator rights to the file share. A group must be prefixed with
-- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
sMBFileShareInfo_adminUserList :: Lens.Lens' SMBFileShareInfo (Core.Maybe [Core.Text])
sMBFileShareInfo_adminUserList = Lens.lens (\SMBFileShareInfo' {adminUserList} -> adminUserList) (\s@SMBFileShareInfo' {} a -> s {adminUserList = a} :: SMBFileShareInfo) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
sMBFileShareInfo_auditDestinationARN :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_auditDestinationARN = Lens.lens (\SMBFileShareInfo' {auditDestinationARN} -> auditDestinationARN) (\s@SMBFileShareInfo' {} a -> s {auditDestinationARN = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_role :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_role = Lens.lens (\SMBFileShareInfo' {role'} -> role') (\s@SMBFileShareInfo' {} a -> s {role' = a} :: SMBFileShareInfo)

-- | A list of up to 50 tags assigned to the SMB file share, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
sMBFileShareInfo_tags :: Lens.Lens' SMBFileShareInfo (Core.Maybe [Tag])
sMBFileShareInfo_tags = Lens.lens (\SMBFileShareInfo' {tags} -> tags) (\s@SMBFileShareInfo' {} a -> s {tags = a} :: SMBFileShareInfo) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
sMBFileShareInfo_fileShareARN :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_fileShareARN = Lens.lens (\SMBFileShareInfo' {fileShareARN} -> fileShareARN) (\s@SMBFileShareInfo' {} a -> s {fileShareARN = a} :: SMBFileShareInfo)

-- | Refresh cache information.
sMBFileShareInfo_cacheAttributes :: Lens.Lens' SMBFileShareInfo (Core.Maybe CacheAttributes)
sMBFileShareInfo_cacheAttributes = Lens.lens (\SMBFileShareInfo' {cacheAttributes} -> cacheAttributes) (\s@SMBFileShareInfo' {} a -> s {cacheAttributes = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_objectACL :: Lens.Lens' SMBFileShareInfo (Core.Maybe ObjectACL)
sMBFileShareInfo_objectACL = Lens.lens (\SMBFileShareInfo' {objectACL} -> objectACL) (\s@SMBFileShareInfo' {} a -> s {objectACL = a} :: SMBFileShareInfo)

-- | The file share path used by the SMB client to identify the mount point.
sMBFileShareInfo_path :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
sMBFileShareInfo_path = Lens.lens (\SMBFileShareInfo' {path} -> path) (\s@SMBFileShareInfo' {} a -> s {path = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_gatewayARN :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Text)
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
sMBFileShareInfo_requesterPays :: Lens.Lens' SMBFileShareInfo (Core.Maybe Core.Bool)
sMBFileShareInfo_requesterPays = Lens.lens (\SMBFileShareInfo' {requesterPays} -> requesterPays) (\s@SMBFileShareInfo' {} a -> s {requesterPays = a} :: SMBFileShareInfo)

-- | A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
sMBFileShareInfo_invalidUserList :: Lens.Lens' SMBFileShareInfo (Core.Maybe [Core.Text])
sMBFileShareInfo_invalidUserList = Lens.lens (\SMBFileShareInfo' {invalidUserList} -> invalidUserList) (\s@SMBFileShareInfo' {} a -> s {invalidUserList = a} :: SMBFileShareInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON SMBFileShareInfo where
  parseJSON =
    Core.withObject
      "SMBFileShareInfo"
      ( \x ->
          SMBFileShareInfo'
            Core.<$> (x Core..:? "SMBACLEnabled")
            Core.<*> (x Core..:? "AccessBasedEnumeration")
            Core.<*> (x Core..:? "DefaultStorageClass")
            Core.<*> (x Core..:? "FileShareName")
            Core.<*> (x Core..:? "CaseSensitivity")
            Core.<*> (x Core..:? "GuessMIMETypeEnabled")
            Core.<*> (x Core..:? "ReadOnly")
            Core.<*> (x Core..:? "FileShareId")
            Core.<*> (x Core..:? "KMSEncrypted")
            Core.<*> (x Core..:? "Authentication")
            Core.<*> (x Core..:? "LocationARN")
            Core.<*> (x Core..:? "NotificationPolicy")
            Core.<*> (x Core..:? "ValidUserList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "KMSKey")
            Core.<*> (x Core..:? "FileShareStatus")
            Core.<*> (x Core..:? "AdminUserList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AuditDestinationARN")
            Core.<*> (x Core..:? "Role")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "FileShareARN")
            Core.<*> (x Core..:? "CacheAttributes")
            Core.<*> (x Core..:? "ObjectACL")
            Core.<*> (x Core..:? "Path")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "RequesterPays")
            Core.<*> (x Core..:? "InvalidUserList" Core..!= Core.mempty)
      )

instance Core.Hashable SMBFileShareInfo

instance Core.NFData SMBFileShareInfo
