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
-- Module      : Amazonka.StorageGateway.Types.SMBFileShareInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.SMBFileShareInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.CacheAttributes
import Amazonka.StorageGateway.Types.CaseSensitivity
import Amazonka.StorageGateway.Types.ObjectACL
import Amazonka.StorageGateway.Types.Tag

-- | The Windows file permissions and ownership information assigned, by
-- default, to native S3 objects when S3 File Gateway discovers them in S3
-- buckets. This operation is only supported for S3 File Gateways.
--
-- /See:/ 'newSMBFileShareInfo' smart constructor.
data SMBFileShareInfo = SMBFileShareInfo'
  { -- | A list of up to 50 tags assigned to the SMB file share, sorted
    -- alphabetically by key name. Each tag is a key-value pair. For a gateway
    -- with more than 10 tags assigned, you can view all tags using the
    -- @ListTagsForResource@ API operation.
    tags :: Prelude.Maybe [Tag],
    -- | A list of users or groups in the Active Directory that are allowed to
    -- access the file share. A group must be prefixed with the \@ character.
    -- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
    -- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
    -- @ActiveDirectory@.
    validUserList :: Prelude.Maybe [Prelude.Text],
    authentication :: Prelude.Maybe Prelude.Text,
    fileShareStatus :: Prelude.Maybe Prelude.Text,
    fileShareId :: Prelude.Maybe Prelude.Text,
    -- | The name of the file share. Optional.
    --
    -- @FileShareName@ must be set if an S3 prefix name is set in
    -- @LocationARN@, or if an access point or access point alias is used.
    fileShareName :: Prelude.Maybe Prelude.Text,
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
    objectACL :: Prelude.Maybe ObjectACL,
    -- | The case of an object name in an Amazon S3 bucket. For
    -- @ClientSpecified@, the client determines the case sensitivity. For
    -- @CaseSensitive@, the gateway determines the case sensitivity. The
    -- default value is @ClientSpecified@.
    caseSensitivity :: Prelude.Maybe CaseSensitivity,
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | If this value is set to @true@, it indicates that access control list
    -- (ACL) is enabled on the SMB file share. If it is set to @false@, it
    -- indicates that file and directory permissions are mapped to the POSIX
    -- permission.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
    -- in the /Storage Gateway User Guide/.
    sMBACLEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether opportunistic locking is enabled for the SMB file
    -- share.
    --
    -- Enabling opportunistic locking on case-sensitive shares is not
    -- recommended for workloads that involve access to files with the same
    -- name in different case.
    --
    -- Valid Values: @true@ | @false@
    oplocksEnabled :: Prelude.Maybe Prelude.Bool,
    locationARN :: Prelude.Maybe Prelude.Text,
    -- | The file share path used by the SMB client to identify the mount point.
    path :: Prelude.Maybe Prelude.Text,
    fileShareARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies the DNS name for the VPC endpoint that the SMB file share uses
    -- to connect to Amazon S3.
    --
    -- This parameter is required for SMB file shares that connect to Amazon S3
    -- through a VPC endpoint, a VPC access point, or an access point alias
    -- that points to a VPC access point.
    vPCEndpointDNSName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether @AccessBasedEnumeration@ is enabled.
    accessBasedEnumeration :: Prelude.Maybe Prelude.Bool,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | A list of users or groups in the Active Directory that are not allowed
    -- to access the file share. A group must be prefixed with the \@
    -- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    invalidUserList :: Prelude.Maybe [Prelude.Text],
    -- | A list of users or groups in the Active Directory that have
    -- administrator rights to the file share. A group must be prefixed with
    -- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    adminUserList :: Prelude.Maybe [Prelude.Text],
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
    -- key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The default storage class for objects put into an Amazon S3 bucket by
    -- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
    --
    -- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
    -- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
    defaultStorageClass :: Prelude.Maybe Prelude.Text,
    -- | Refresh cache information for the file share.
    cacheAttributes :: Prelude.Maybe CacheAttributes,
    -- | A value that sets the write status of a file share. Set this value to
    -- @true@ to set the write status to read-only, otherwise set to @false@.
    --
    -- Valid Values: @true@ | @false@
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the Region of the S3 bucket where the SMB file share stores
    -- files.
    --
    -- This parameter is required for SMB file shares that connect to Amazon S3
    -- through a VPC endpoint, a VPC access point, or an access point alias
    -- that points to a VPC access point.
    bucketRegion :: Prelude.Maybe Prelude.Text,
    role' :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the storage used for audit logs.
    auditDestinationARN :: Prelude.Maybe Prelude.Text,
    -- | A value that enables guessing of the MIME type for uploaded objects
    -- based on file extensions. Set this value to @true@ to enable MIME type
    -- guessing, otherwise set to @false@. The default value is @true@.
    --
    -- Valid Values: @true@ | @false@
    guessMIMETypeEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The notification policy of the file share. @SettlingTimeInSeconds@
    -- controls the number of seconds to wait after the last point in time a
    -- client wrote to a file before generating an @ObjectUploaded@
    -- notification. Because clients can make many small writes to files, it\'s
    -- best to set this parameter for as long as possible to avoid generating
    -- multiple notifications for the same file in a small time period.
    --
    -- @SettlingTimeInSeconds@ has no effect on the timing of the object
    -- uploading to Amazon S3, only the timing of the notification.
    --
    -- The following example sets @NotificationPolicy@ on with
    -- @SettlingTimeInSeconds@ set to 60.
    --
    -- @{\\\"Upload\\\": {\\\"SettlingTimeInSeconds\\\": 60}}@
    --
    -- The following example sets @NotificationPolicy@ off.
    --
    -- @{}@
    notificationPolicy :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'sMBFileShareInfo_tags' - A list of up to 50 tags assigned to the SMB file share, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
--
-- 'validUserList', 'sMBFileShareInfo_validUserList' - A list of users or groups in the Active Directory that are allowed to
-- access the file share. A group must be prefixed with the \@ character.
-- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
-- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
-- @ActiveDirectory@.
--
-- 'authentication', 'sMBFileShareInfo_authentication' - Undocumented member.
--
-- 'fileShareStatus', 'sMBFileShareInfo_fileShareStatus' - Undocumented member.
--
-- 'fileShareId', 'sMBFileShareInfo_fileShareId' - Undocumented member.
--
-- 'fileShareName', 'sMBFileShareInfo_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
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
-- 'objectACL', 'sMBFileShareInfo_objectACL' - Undocumented member.
--
-- 'caseSensitivity', 'sMBFileShareInfo_caseSensitivity' - The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
--
-- 'kmsKey', 'sMBFileShareInfo_kmsKey' - Undocumented member.
--
-- 'sMBACLEnabled', 'sMBFileShareInfo_sMBACLEnabled' - If this value is set to @true@, it indicates that access control list
-- (ACL) is enabled on the SMB file share. If it is set to @false@, it
-- indicates that file and directory permissions are mapped to the POSIX
-- permission.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /Storage Gateway User Guide/.
--
-- 'oplocksEnabled', 'sMBFileShareInfo_oplocksEnabled' - Specifies whether opportunistic locking is enabled for the SMB file
-- share.
--
-- Enabling opportunistic locking on case-sensitive shares is not
-- recommended for workloads that involve access to files with the same
-- name in different case.
--
-- Valid Values: @true@ | @false@
--
-- 'locationARN', 'sMBFileShareInfo_locationARN' - Undocumented member.
--
-- 'path', 'sMBFileShareInfo_path' - The file share path used by the SMB client to identify the mount point.
--
-- 'fileShareARN', 'sMBFileShareInfo_fileShareARN' - Undocumented member.
--
-- 'vPCEndpointDNSName', 'sMBFileShareInfo_vPCEndpointDNSName' - Specifies the DNS name for the VPC endpoint that the SMB file share uses
-- to connect to Amazon S3.
--
-- This parameter is required for SMB file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
--
-- 'accessBasedEnumeration', 'sMBFileShareInfo_accessBasedEnumeration' - Indicates whether @AccessBasedEnumeration@ is enabled.
--
-- 'gatewayARN', 'sMBFileShareInfo_gatewayARN' - Undocumented member.
--
-- 'invalidUserList', 'sMBFileShareInfo_invalidUserList' - A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'adminUserList', 'sMBFileShareInfo_adminUserList' - A list of users or groups in the Active Directory that have
-- administrator rights to the file share. A group must be prefixed with
-- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'kmsEncrypted', 'sMBFileShareInfo_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'defaultStorageClass', 'sMBFileShareInfo_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'cacheAttributes', 'sMBFileShareInfo_cacheAttributes' - Refresh cache information for the file share.
--
-- 'readOnly', 'sMBFileShareInfo_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'bucketRegion', 'sMBFileShareInfo_bucketRegion' - Specifies the Region of the S3 bucket where the SMB file share stores
-- files.
--
-- This parameter is required for SMB file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
--
-- 'role'', 'sMBFileShareInfo_role' - Undocumented member.
--
-- 'auditDestinationARN', 'sMBFileShareInfo_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for audit logs.
--
-- 'guessMIMETypeEnabled', 'sMBFileShareInfo_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'notificationPolicy', 'sMBFileShareInfo_notificationPolicy' - The notification policy of the file share. @SettlingTimeInSeconds@
-- controls the number of seconds to wait after the last point in time a
-- client wrote to a file before generating an @ObjectUploaded@
-- notification. Because clients can make many small writes to files, it\'s
-- best to set this parameter for as long as possible to avoid generating
-- multiple notifications for the same file in a small time period.
--
-- @SettlingTimeInSeconds@ has no effect on the timing of the object
-- uploading to Amazon S3, only the timing of the notification.
--
-- The following example sets @NotificationPolicy@ on with
-- @SettlingTimeInSeconds@ set to 60.
--
-- @{\\\"Upload\\\": {\\\"SettlingTimeInSeconds\\\": 60}}@
--
-- The following example sets @NotificationPolicy@ off.
--
-- @{}@
newSMBFileShareInfo ::
  SMBFileShareInfo
newSMBFileShareInfo =
  SMBFileShareInfo'
    { tags = Prelude.Nothing,
      validUserList = Prelude.Nothing,
      authentication = Prelude.Nothing,
      fileShareStatus = Prelude.Nothing,
      fileShareId = Prelude.Nothing,
      fileShareName = Prelude.Nothing,
      requesterPays = Prelude.Nothing,
      objectACL = Prelude.Nothing,
      caseSensitivity = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      sMBACLEnabled = Prelude.Nothing,
      oplocksEnabled = Prelude.Nothing,
      locationARN = Prelude.Nothing,
      path = Prelude.Nothing,
      fileShareARN = Prelude.Nothing,
      vPCEndpointDNSName = Prelude.Nothing,
      accessBasedEnumeration = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      invalidUserList = Prelude.Nothing,
      adminUserList = Prelude.Nothing,
      kmsEncrypted = Prelude.Nothing,
      defaultStorageClass = Prelude.Nothing,
      cacheAttributes = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      bucketRegion = Prelude.Nothing,
      role' = Prelude.Nothing,
      auditDestinationARN = Prelude.Nothing,
      guessMIMETypeEnabled = Prelude.Nothing,
      notificationPolicy = Prelude.Nothing
    }

-- | A list of up to 50 tags assigned to the SMB file share, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
sMBFileShareInfo_tags :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe [Tag])
sMBFileShareInfo_tags = Lens.lens (\SMBFileShareInfo' {tags} -> tags) (\s@SMBFileShareInfo' {} a -> s {tags = a} :: SMBFileShareInfo) Prelude.. Lens.mapping Lens.coerced

-- | A list of users or groups in the Active Directory that are allowed to
-- access the file share. A group must be prefixed with the \@ character.
-- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
-- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
-- @ActiveDirectory@.
sMBFileShareInfo_validUserList :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe [Prelude.Text])
sMBFileShareInfo_validUserList = Lens.lens (\SMBFileShareInfo' {validUserList} -> validUserList) (\s@SMBFileShareInfo' {} a -> s {validUserList = a} :: SMBFileShareInfo) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
sMBFileShareInfo_authentication :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_authentication = Lens.lens (\SMBFileShareInfo' {authentication} -> authentication) (\s@SMBFileShareInfo' {} a -> s {authentication = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_fileShareStatus :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_fileShareStatus = Lens.lens (\SMBFileShareInfo' {fileShareStatus} -> fileShareStatus) (\s@SMBFileShareInfo' {} a -> s {fileShareStatus = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_fileShareId :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_fileShareId = Lens.lens (\SMBFileShareInfo' {fileShareId} -> fileShareId) (\s@SMBFileShareInfo' {} a -> s {fileShareId = a} :: SMBFileShareInfo)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
sMBFileShareInfo_fileShareName :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_fileShareName = Lens.lens (\SMBFileShareInfo' {fileShareName} -> fileShareName) (\s@SMBFileShareInfo' {} a -> s {fileShareName = a} :: SMBFileShareInfo)

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

-- | Undocumented member.
sMBFileShareInfo_objectACL :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe ObjectACL)
sMBFileShareInfo_objectACL = Lens.lens (\SMBFileShareInfo' {objectACL} -> objectACL) (\s@SMBFileShareInfo' {} a -> s {objectACL = a} :: SMBFileShareInfo)

-- | The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
sMBFileShareInfo_caseSensitivity :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe CaseSensitivity)
sMBFileShareInfo_caseSensitivity = Lens.lens (\SMBFileShareInfo' {caseSensitivity} -> caseSensitivity) (\s@SMBFileShareInfo' {} a -> s {caseSensitivity = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_kmsKey :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_kmsKey = Lens.lens (\SMBFileShareInfo' {kmsKey} -> kmsKey) (\s@SMBFileShareInfo' {} a -> s {kmsKey = a} :: SMBFileShareInfo)

-- | If this value is set to @true@, it indicates that access control list
-- (ACL) is enabled on the SMB file share. If it is set to @false@, it
-- indicates that file and directory permissions are mapped to the POSIX
-- permission.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /Storage Gateway User Guide/.
sMBFileShareInfo_sMBACLEnabled :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_sMBACLEnabled = Lens.lens (\SMBFileShareInfo' {sMBACLEnabled} -> sMBACLEnabled) (\s@SMBFileShareInfo' {} a -> s {sMBACLEnabled = a} :: SMBFileShareInfo)

-- | Specifies whether opportunistic locking is enabled for the SMB file
-- share.
--
-- Enabling opportunistic locking on case-sensitive shares is not
-- recommended for workloads that involve access to files with the same
-- name in different case.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_oplocksEnabled :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_oplocksEnabled = Lens.lens (\SMBFileShareInfo' {oplocksEnabled} -> oplocksEnabled) (\s@SMBFileShareInfo' {} a -> s {oplocksEnabled = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_locationARN :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_locationARN = Lens.lens (\SMBFileShareInfo' {locationARN} -> locationARN) (\s@SMBFileShareInfo' {} a -> s {locationARN = a} :: SMBFileShareInfo)

-- | The file share path used by the SMB client to identify the mount point.
sMBFileShareInfo_path :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_path = Lens.lens (\SMBFileShareInfo' {path} -> path) (\s@SMBFileShareInfo' {} a -> s {path = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_fileShareARN :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_fileShareARN = Lens.lens (\SMBFileShareInfo' {fileShareARN} -> fileShareARN) (\s@SMBFileShareInfo' {} a -> s {fileShareARN = a} :: SMBFileShareInfo)

-- | Specifies the DNS name for the VPC endpoint that the SMB file share uses
-- to connect to Amazon S3.
--
-- This parameter is required for SMB file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
sMBFileShareInfo_vPCEndpointDNSName :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_vPCEndpointDNSName = Lens.lens (\SMBFileShareInfo' {vPCEndpointDNSName} -> vPCEndpointDNSName) (\s@SMBFileShareInfo' {} a -> s {vPCEndpointDNSName = a} :: SMBFileShareInfo)

-- | Indicates whether @AccessBasedEnumeration@ is enabled.
sMBFileShareInfo_accessBasedEnumeration :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_accessBasedEnumeration = Lens.lens (\SMBFileShareInfo' {accessBasedEnumeration} -> accessBasedEnumeration) (\s@SMBFileShareInfo' {} a -> s {accessBasedEnumeration = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_gatewayARN :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_gatewayARN = Lens.lens (\SMBFileShareInfo' {gatewayARN} -> gatewayARN) (\s@SMBFileShareInfo' {} a -> s {gatewayARN = a} :: SMBFileShareInfo)

-- | A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
sMBFileShareInfo_invalidUserList :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe [Prelude.Text])
sMBFileShareInfo_invalidUserList = Lens.lens (\SMBFileShareInfo' {invalidUserList} -> invalidUserList) (\s@SMBFileShareInfo' {} a -> s {invalidUserList = a} :: SMBFileShareInfo) Prelude.. Lens.mapping Lens.coerced

-- | A list of users or groups in the Active Directory that have
-- administrator rights to the file share. A group must be prefixed with
-- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
sMBFileShareInfo_adminUserList :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe [Prelude.Text])
sMBFileShareInfo_adminUserList = Lens.lens (\SMBFileShareInfo' {adminUserList} -> adminUserList) (\s@SMBFileShareInfo' {} a -> s {adminUserList = a} :: SMBFileShareInfo) Prelude.. Lens.mapping Lens.coerced

-- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_kmsEncrypted :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_kmsEncrypted = Lens.lens (\SMBFileShareInfo' {kmsEncrypted} -> kmsEncrypted) (\s@SMBFileShareInfo' {} a -> s {kmsEncrypted = a} :: SMBFileShareInfo)

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
sMBFileShareInfo_defaultStorageClass :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_defaultStorageClass = Lens.lens (\SMBFileShareInfo' {defaultStorageClass} -> defaultStorageClass) (\s@SMBFileShareInfo' {} a -> s {defaultStorageClass = a} :: SMBFileShareInfo)

-- | Refresh cache information for the file share.
sMBFileShareInfo_cacheAttributes :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe CacheAttributes)
sMBFileShareInfo_cacheAttributes = Lens.lens (\SMBFileShareInfo' {cacheAttributes} -> cacheAttributes) (\s@SMBFileShareInfo' {} a -> s {cacheAttributes = a} :: SMBFileShareInfo)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_readOnly :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_readOnly = Lens.lens (\SMBFileShareInfo' {readOnly} -> readOnly) (\s@SMBFileShareInfo' {} a -> s {readOnly = a} :: SMBFileShareInfo)

-- | Specifies the Region of the S3 bucket where the SMB file share stores
-- files.
--
-- This parameter is required for SMB file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
sMBFileShareInfo_bucketRegion :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_bucketRegion = Lens.lens (\SMBFileShareInfo' {bucketRegion} -> bucketRegion) (\s@SMBFileShareInfo' {} a -> s {bucketRegion = a} :: SMBFileShareInfo)

-- | Undocumented member.
sMBFileShareInfo_role :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_role = Lens.lens (\SMBFileShareInfo' {role'} -> role') (\s@SMBFileShareInfo' {} a -> s {role' = a} :: SMBFileShareInfo)

-- | The Amazon Resource Name (ARN) of the storage used for audit logs.
sMBFileShareInfo_auditDestinationARN :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_auditDestinationARN = Lens.lens (\SMBFileShareInfo' {auditDestinationARN} -> auditDestinationARN) (\s@SMBFileShareInfo' {} a -> s {auditDestinationARN = a} :: SMBFileShareInfo)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
sMBFileShareInfo_guessMIMETypeEnabled :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Bool)
sMBFileShareInfo_guessMIMETypeEnabled = Lens.lens (\SMBFileShareInfo' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@SMBFileShareInfo' {} a -> s {guessMIMETypeEnabled = a} :: SMBFileShareInfo)

-- | The notification policy of the file share. @SettlingTimeInSeconds@
-- controls the number of seconds to wait after the last point in time a
-- client wrote to a file before generating an @ObjectUploaded@
-- notification. Because clients can make many small writes to files, it\'s
-- best to set this parameter for as long as possible to avoid generating
-- multiple notifications for the same file in a small time period.
--
-- @SettlingTimeInSeconds@ has no effect on the timing of the object
-- uploading to Amazon S3, only the timing of the notification.
--
-- The following example sets @NotificationPolicy@ on with
-- @SettlingTimeInSeconds@ set to 60.
--
-- @{\\\"Upload\\\": {\\\"SettlingTimeInSeconds\\\": 60}}@
--
-- The following example sets @NotificationPolicy@ off.
--
-- @{}@
sMBFileShareInfo_notificationPolicy :: Lens.Lens' SMBFileShareInfo (Prelude.Maybe Prelude.Text)
sMBFileShareInfo_notificationPolicy = Lens.lens (\SMBFileShareInfo' {notificationPolicy} -> notificationPolicy) (\s@SMBFileShareInfo' {} a -> s {notificationPolicy = a} :: SMBFileShareInfo)

instance Data.FromJSON SMBFileShareInfo where
  parseJSON =
    Data.withObject
      "SMBFileShareInfo"
      ( \x ->
          SMBFileShareInfo'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ValidUserList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Authentication")
            Prelude.<*> (x Data..:? "FileShareStatus")
            Prelude.<*> (x Data..:? "FileShareId")
            Prelude.<*> (x Data..:? "FileShareName")
            Prelude.<*> (x Data..:? "RequesterPays")
            Prelude.<*> (x Data..:? "ObjectACL")
            Prelude.<*> (x Data..:? "CaseSensitivity")
            Prelude.<*> (x Data..:? "KMSKey")
            Prelude.<*> (x Data..:? "SMBACLEnabled")
            Prelude.<*> (x Data..:? "OplocksEnabled")
            Prelude.<*> (x Data..:? "LocationARN")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "FileShareARN")
            Prelude.<*> (x Data..:? "VPCEndpointDNSName")
            Prelude.<*> (x Data..:? "AccessBasedEnumeration")
            Prelude.<*> (x Data..:? "GatewayARN")
            Prelude.<*> ( x Data..:? "InvalidUserList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AdminUserList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "KMSEncrypted")
            Prelude.<*> (x Data..:? "DefaultStorageClass")
            Prelude.<*> (x Data..:? "CacheAttributes")
            Prelude.<*> (x Data..:? "ReadOnly")
            Prelude.<*> (x Data..:? "BucketRegion")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "AuditDestinationARN")
            Prelude.<*> (x Data..:? "GuessMIMETypeEnabled")
            Prelude.<*> (x Data..:? "NotificationPolicy")
      )

instance Prelude.Hashable SMBFileShareInfo where
  hashWithSalt _salt SMBFileShareInfo' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` validUserList
      `Prelude.hashWithSalt` authentication
      `Prelude.hashWithSalt` fileShareStatus
      `Prelude.hashWithSalt` fileShareId
      `Prelude.hashWithSalt` fileShareName
      `Prelude.hashWithSalt` requesterPays
      `Prelude.hashWithSalt` objectACL
      `Prelude.hashWithSalt` caseSensitivity
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` sMBACLEnabled
      `Prelude.hashWithSalt` oplocksEnabled
      `Prelude.hashWithSalt` locationARN
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` fileShareARN
      `Prelude.hashWithSalt` vPCEndpointDNSName
      `Prelude.hashWithSalt` accessBasedEnumeration
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` invalidUserList
      `Prelude.hashWithSalt` adminUserList
      `Prelude.hashWithSalt` kmsEncrypted
      `Prelude.hashWithSalt` defaultStorageClass
      `Prelude.hashWithSalt` cacheAttributes
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` bucketRegion
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` auditDestinationARN
      `Prelude.hashWithSalt` guessMIMETypeEnabled
      `Prelude.hashWithSalt` notificationPolicy

instance Prelude.NFData SMBFileShareInfo where
  rnf SMBFileShareInfo' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf validUserList
      `Prelude.seq` Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf fileShareStatus
      `Prelude.seq` Prelude.rnf fileShareId
      `Prelude.seq` Prelude.rnf fileShareName
      `Prelude.seq` Prelude.rnf requesterPays
      `Prelude.seq` Prelude.rnf objectACL
      `Prelude.seq` Prelude.rnf caseSensitivity
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf sMBACLEnabled
      `Prelude.seq` Prelude.rnf oplocksEnabled
      `Prelude.seq` Prelude.rnf locationARN
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf fileShareARN
      `Prelude.seq` Prelude.rnf vPCEndpointDNSName
      `Prelude.seq` Prelude.rnf accessBasedEnumeration
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf invalidUserList
      `Prelude.seq` Prelude.rnf adminUserList
      `Prelude.seq` Prelude.rnf kmsEncrypted
      `Prelude.seq` Prelude.rnf
        defaultStorageClass
      `Prelude.seq` Prelude.rnf
        cacheAttributes
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf
        bucketRegion
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf
        auditDestinationARN
      `Prelude.seq` Prelude.rnf
        guessMIMETypeEnabled
      `Prelude.seq` Prelude.rnf
        notificationPolicy
