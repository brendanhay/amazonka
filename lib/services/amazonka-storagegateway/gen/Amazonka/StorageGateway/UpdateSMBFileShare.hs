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
-- Module      : Amazonka.StorageGateway.UpdateSMBFileShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Server Message Block (SMB) file share. This operation is only
-- supported for S3 File Gateways.
--
-- To leave a file share field unchanged, set the corresponding input field
-- to null.
--
-- File gateways require Security Token Service (Amazon Web Services STS)
-- to be activated to enable you to create a file share. Make sure that
-- Amazon Web Services STS is activated in the Amazon Web Services Region
-- you are creating your file gateway in. If Amazon Web Services STS is not
-- activated in this Amazon Web Services Region, activate it. For
-- information about how to activate Amazon Web Services STS, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating Amazon Web Services STS in an Amazon Web Services Region>
-- in the /Identity and Access Management User Guide/.
--
-- File gateways don\'t support creating hard or symbolic links on a file
-- share.
module Amazonka.StorageGateway.UpdateSMBFileShare
  ( -- * Creating a Request
    UpdateSMBFileShare (..),
    newUpdateSMBFileShare,

    -- * Request Lenses
    updateSMBFileShare_accessBasedEnumeration,
    updateSMBFileShare_adminUserList,
    updateSMBFileShare_auditDestinationARN,
    updateSMBFileShare_cacheAttributes,
    updateSMBFileShare_caseSensitivity,
    updateSMBFileShare_defaultStorageClass,
    updateSMBFileShare_fileShareName,
    updateSMBFileShare_guessMIMETypeEnabled,
    updateSMBFileShare_invalidUserList,
    updateSMBFileShare_kmsEncrypted,
    updateSMBFileShare_kmsKey,
    updateSMBFileShare_notificationPolicy,
    updateSMBFileShare_objectACL,
    updateSMBFileShare_oplocksEnabled,
    updateSMBFileShare_readOnly,
    updateSMBFileShare_requesterPays,
    updateSMBFileShare_sMBACLEnabled,
    updateSMBFileShare_validUserList,
    updateSMBFileShare_fileShareARN,

    -- * Destructuring the Response
    UpdateSMBFileShareResponse (..),
    newUpdateSMBFileShareResponse,

    -- * Response Lenses
    updateSMBFileShareResponse_fileShareARN,
    updateSMBFileShareResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | UpdateSMBFileShareInput
--
-- /See:/ 'newUpdateSMBFileShare' smart constructor.
data UpdateSMBFileShare = UpdateSMBFileShare'
  { -- | The files and folders on this share will only be visible to users with
    -- read access.
    accessBasedEnumeration :: Prelude.Maybe Prelude.Bool,
    -- | A list of users or groups in the Active Directory that have
    -- administrator rights to the file share. A group must be prefixed with
    -- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    adminUserList :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the storage used for audit logs.
    auditDestinationARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies refresh cache information for the file share.
    cacheAttributes :: Prelude.Maybe CacheAttributes,
    -- | The case of an object name in an Amazon S3 bucket. For
    -- @ClientSpecified@, the client determines the case sensitivity. For
    -- @CaseSensitive@, the gateway determines the case sensitivity. The
    -- default value is @ClientSpecified@.
    caseSensitivity :: Prelude.Maybe CaseSensitivity,
    -- | The default storage class for objects put into an Amazon S3 bucket by
    -- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
    --
    -- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
    -- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
    defaultStorageClass :: Prelude.Maybe Prelude.Text,
    -- | The name of the file share. Optional.
    --
    -- @FileShareName@ must be set if an S3 prefix name is set in
    -- @LocationARN@, or if an access point or access point alias is used.
    fileShareName :: Prelude.Maybe Prelude.Text,
    -- | A value that enables guessing of the MIME type for uploaded objects
    -- based on file extensions. Set this value to @true@ to enable MIME type
    -- guessing, otherwise set to @false@. The default value is @true@.
    --
    -- Valid Values: @true@ | @false@
    guessMIMETypeEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of users or groups in the Active Directory that are not allowed
    -- to access the file share. A group must be prefixed with the \@
    -- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    invalidUserList :: Prelude.Maybe [Prelude.Text],
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
    -- key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Prelude.Maybe Prelude.Text,
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
    notificationPolicy :: Prelude.Maybe Prelude.Text,
    -- | A value that sets the access control list (ACL) permission for objects
    -- in the S3 bucket that a S3 File Gateway puts objects into. The default
    -- value is @private@.
    objectACL :: Prelude.Maybe ObjectACL,
    -- | Specifies whether opportunistic locking is enabled for the SMB file
    -- share.
    --
    -- Enabling opportunistic locking on case-sensitive shares is not
    -- recommended for workloads that involve access to files with the same
    -- name in different case.
    --
    -- Valid Values: @true@ | @false@
    oplocksEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A value that sets the write status of a file share. Set this value to
    -- @true@ to set write status to read-only, otherwise set to @false@.
    --
    -- Valid Values: @true@ | @false@
    readOnly :: Prelude.Maybe Prelude.Bool,
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
    -- | Set this value to @true@ to enable access control list (ACL) on the SMB
    -- file share. Set it to @false@ to map file and directory permissions to
    -- the POSIX permissions.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
    -- in the /Storage Gateway User Guide/.
    --
    -- Valid Values: @true@ | @false@
    sMBACLEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of users or groups in the Active Directory that are allowed to
    -- access the file share. A group must be prefixed with the \@ character.
    -- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
    -- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
    -- @ActiveDirectory@.
    validUserList :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the SMB file share that you want to
    -- update.
    fileShareARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSMBFileShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessBasedEnumeration', 'updateSMBFileShare_accessBasedEnumeration' - The files and folders on this share will only be visible to users with
-- read access.
--
-- 'adminUserList', 'updateSMBFileShare_adminUserList' - A list of users or groups in the Active Directory that have
-- administrator rights to the file share. A group must be prefixed with
-- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'auditDestinationARN', 'updateSMBFileShare_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for audit logs.
--
-- 'cacheAttributes', 'updateSMBFileShare_cacheAttributes' - Specifies refresh cache information for the file share.
--
-- 'caseSensitivity', 'updateSMBFileShare_caseSensitivity' - The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
--
-- 'defaultStorageClass', 'updateSMBFileShare_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'fileShareName', 'updateSMBFileShare_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
--
-- 'guessMIMETypeEnabled', 'updateSMBFileShare_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'invalidUserList', 'updateSMBFileShare_invalidUserList' - A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'kmsEncrypted', 'updateSMBFileShare_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsKey', 'updateSMBFileShare_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'notificationPolicy', 'updateSMBFileShare_notificationPolicy' - The notification policy of the file share. @SettlingTimeInSeconds@
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
--
-- 'objectACL', 'updateSMBFileShare_objectACL' - A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a S3 File Gateway puts objects into. The default
-- value is @private@.
--
-- 'oplocksEnabled', 'updateSMBFileShare_oplocksEnabled' - Specifies whether opportunistic locking is enabled for the SMB file
-- share.
--
-- Enabling opportunistic locking on case-sensitive shares is not
-- recommended for workloads that involve access to files with the same
-- name in different case.
--
-- Valid Values: @true@ | @false@
--
-- 'readOnly', 'updateSMBFileShare_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'requesterPays', 'updateSMBFileShare_requesterPays' - A value that sets who pays the cost of the request and the cost
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
-- 'sMBACLEnabled', 'updateSMBFileShare_sMBACLEnabled' - Set this value to @true@ to enable access control list (ACL) on the SMB
-- file share. Set it to @false@ to map file and directory permissions to
-- the POSIX permissions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /Storage Gateway User Guide/.
--
-- Valid Values: @true@ | @false@
--
-- 'validUserList', 'updateSMBFileShare_validUserList' - A list of users or groups in the Active Directory that are allowed to
-- access the file share. A group must be prefixed with the \@ character.
-- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
-- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
-- @ActiveDirectory@.
--
-- 'fileShareARN', 'updateSMBFileShare_fileShareARN' - The Amazon Resource Name (ARN) of the SMB file share that you want to
-- update.
newUpdateSMBFileShare ::
  -- | 'fileShareARN'
  Prelude.Text ->
  UpdateSMBFileShare
newUpdateSMBFileShare pFileShareARN_ =
  UpdateSMBFileShare'
    { accessBasedEnumeration =
        Prelude.Nothing,
      adminUserList = Prelude.Nothing,
      auditDestinationARN = Prelude.Nothing,
      cacheAttributes = Prelude.Nothing,
      caseSensitivity = Prelude.Nothing,
      defaultStorageClass = Prelude.Nothing,
      fileShareName = Prelude.Nothing,
      guessMIMETypeEnabled = Prelude.Nothing,
      invalidUserList = Prelude.Nothing,
      kmsEncrypted = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      notificationPolicy = Prelude.Nothing,
      objectACL = Prelude.Nothing,
      oplocksEnabled = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      requesterPays = Prelude.Nothing,
      sMBACLEnabled = Prelude.Nothing,
      validUserList = Prelude.Nothing,
      fileShareARN = pFileShareARN_
    }

-- | The files and folders on this share will only be visible to users with
-- read access.
updateSMBFileShare_accessBasedEnumeration :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_accessBasedEnumeration = Lens.lens (\UpdateSMBFileShare' {accessBasedEnumeration} -> accessBasedEnumeration) (\s@UpdateSMBFileShare' {} a -> s {accessBasedEnumeration = a} :: UpdateSMBFileShare)

-- | A list of users or groups in the Active Directory that have
-- administrator rights to the file share. A group must be prefixed with
-- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
updateSMBFileShare_adminUserList :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe [Prelude.Text])
updateSMBFileShare_adminUserList = Lens.lens (\UpdateSMBFileShare' {adminUserList} -> adminUserList) (\s@UpdateSMBFileShare' {} a -> s {adminUserList = a} :: UpdateSMBFileShare) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the storage used for audit logs.
updateSMBFileShare_auditDestinationARN :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_auditDestinationARN = Lens.lens (\UpdateSMBFileShare' {auditDestinationARN} -> auditDestinationARN) (\s@UpdateSMBFileShare' {} a -> s {auditDestinationARN = a} :: UpdateSMBFileShare)

-- | Specifies refresh cache information for the file share.
updateSMBFileShare_cacheAttributes :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe CacheAttributes)
updateSMBFileShare_cacheAttributes = Lens.lens (\UpdateSMBFileShare' {cacheAttributes} -> cacheAttributes) (\s@UpdateSMBFileShare' {} a -> s {cacheAttributes = a} :: UpdateSMBFileShare)

-- | The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
updateSMBFileShare_caseSensitivity :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe CaseSensitivity)
updateSMBFileShare_caseSensitivity = Lens.lens (\UpdateSMBFileShare' {caseSensitivity} -> caseSensitivity) (\s@UpdateSMBFileShare' {} a -> s {caseSensitivity = a} :: UpdateSMBFileShare)

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
updateSMBFileShare_defaultStorageClass :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_defaultStorageClass = Lens.lens (\UpdateSMBFileShare' {defaultStorageClass} -> defaultStorageClass) (\s@UpdateSMBFileShare' {} a -> s {defaultStorageClass = a} :: UpdateSMBFileShare)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
updateSMBFileShare_fileShareName :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_fileShareName = Lens.lens (\UpdateSMBFileShare' {fileShareName} -> fileShareName) (\s@UpdateSMBFileShare' {} a -> s {fileShareName = a} :: UpdateSMBFileShare)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
updateSMBFileShare_guessMIMETypeEnabled :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_guessMIMETypeEnabled = Lens.lens (\UpdateSMBFileShare' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@UpdateSMBFileShare' {} a -> s {guessMIMETypeEnabled = a} :: UpdateSMBFileShare)

-- | A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
updateSMBFileShare_invalidUserList :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe [Prelude.Text])
updateSMBFileShare_invalidUserList = Lens.lens (\UpdateSMBFileShare' {invalidUserList} -> invalidUserList) (\s@UpdateSMBFileShare' {} a -> s {invalidUserList = a} :: UpdateSMBFileShare) Prelude.. Lens.mapping Lens.coerced

-- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
updateSMBFileShare_kmsEncrypted :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_kmsEncrypted = Lens.lens (\UpdateSMBFileShare' {kmsEncrypted} -> kmsEncrypted) (\s@UpdateSMBFileShare' {} a -> s {kmsEncrypted = a} :: UpdateSMBFileShare)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
updateSMBFileShare_kmsKey :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_kmsKey = Lens.lens (\UpdateSMBFileShare' {kmsKey} -> kmsKey) (\s@UpdateSMBFileShare' {} a -> s {kmsKey = a} :: UpdateSMBFileShare)

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
updateSMBFileShare_notificationPolicy :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_notificationPolicy = Lens.lens (\UpdateSMBFileShare' {notificationPolicy} -> notificationPolicy) (\s@UpdateSMBFileShare' {} a -> s {notificationPolicy = a} :: UpdateSMBFileShare)

-- | A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a S3 File Gateway puts objects into. The default
-- value is @private@.
updateSMBFileShare_objectACL :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe ObjectACL)
updateSMBFileShare_objectACL = Lens.lens (\UpdateSMBFileShare' {objectACL} -> objectACL) (\s@UpdateSMBFileShare' {} a -> s {objectACL = a} :: UpdateSMBFileShare)

-- | Specifies whether opportunistic locking is enabled for the SMB file
-- share.
--
-- Enabling opportunistic locking on case-sensitive shares is not
-- recommended for workloads that involve access to files with the same
-- name in different case.
--
-- Valid Values: @true@ | @false@
updateSMBFileShare_oplocksEnabled :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_oplocksEnabled = Lens.lens (\UpdateSMBFileShare' {oplocksEnabled} -> oplocksEnabled) (\s@UpdateSMBFileShare' {} a -> s {oplocksEnabled = a} :: UpdateSMBFileShare)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
updateSMBFileShare_readOnly :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_readOnly = Lens.lens (\UpdateSMBFileShare' {readOnly} -> readOnly) (\s@UpdateSMBFileShare' {} a -> s {readOnly = a} :: UpdateSMBFileShare)

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
updateSMBFileShare_requesterPays :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_requesterPays = Lens.lens (\UpdateSMBFileShare' {requesterPays} -> requesterPays) (\s@UpdateSMBFileShare' {} a -> s {requesterPays = a} :: UpdateSMBFileShare)

-- | Set this value to @true@ to enable access control list (ACL) on the SMB
-- file share. Set it to @false@ to map file and directory permissions to
-- the POSIX permissions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /Storage Gateway User Guide/.
--
-- Valid Values: @true@ | @false@
updateSMBFileShare_sMBACLEnabled :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_sMBACLEnabled = Lens.lens (\UpdateSMBFileShare' {sMBACLEnabled} -> sMBACLEnabled) (\s@UpdateSMBFileShare' {} a -> s {sMBACLEnabled = a} :: UpdateSMBFileShare)

-- | A list of users or groups in the Active Directory that are allowed to
-- access the file share. A group must be prefixed with the \@ character.
-- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
-- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
-- @ActiveDirectory@.
updateSMBFileShare_validUserList :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe [Prelude.Text])
updateSMBFileShare_validUserList = Lens.lens (\UpdateSMBFileShare' {validUserList} -> validUserList) (\s@UpdateSMBFileShare' {} a -> s {validUserList = a} :: UpdateSMBFileShare) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the SMB file share that you want to
-- update.
updateSMBFileShare_fileShareARN :: Lens.Lens' UpdateSMBFileShare Prelude.Text
updateSMBFileShare_fileShareARN = Lens.lens (\UpdateSMBFileShare' {fileShareARN} -> fileShareARN) (\s@UpdateSMBFileShare' {} a -> s {fileShareARN = a} :: UpdateSMBFileShare)

instance Core.AWSRequest UpdateSMBFileShare where
  type
    AWSResponse UpdateSMBFileShare =
      UpdateSMBFileShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSMBFileShareResponse'
            Prelude.<$> (x Data..?> "FileShareARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSMBFileShare where
  hashWithSalt _salt UpdateSMBFileShare' {..} =
    _salt
      `Prelude.hashWithSalt` accessBasedEnumeration
      `Prelude.hashWithSalt` adminUserList
      `Prelude.hashWithSalt` auditDestinationARN
      `Prelude.hashWithSalt` cacheAttributes
      `Prelude.hashWithSalt` caseSensitivity
      `Prelude.hashWithSalt` defaultStorageClass
      `Prelude.hashWithSalt` fileShareName
      `Prelude.hashWithSalt` guessMIMETypeEnabled
      `Prelude.hashWithSalt` invalidUserList
      `Prelude.hashWithSalt` kmsEncrypted
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` notificationPolicy
      `Prelude.hashWithSalt` objectACL
      `Prelude.hashWithSalt` oplocksEnabled
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` requesterPays
      `Prelude.hashWithSalt` sMBACLEnabled
      `Prelude.hashWithSalt` validUserList
      `Prelude.hashWithSalt` fileShareARN

instance Prelude.NFData UpdateSMBFileShare where
  rnf UpdateSMBFileShare' {..} =
    Prelude.rnf accessBasedEnumeration
      `Prelude.seq` Prelude.rnf adminUserList
      `Prelude.seq` Prelude.rnf auditDestinationARN
      `Prelude.seq` Prelude.rnf cacheAttributes
      `Prelude.seq` Prelude.rnf caseSensitivity
      `Prelude.seq` Prelude.rnf defaultStorageClass
      `Prelude.seq` Prelude.rnf fileShareName
      `Prelude.seq` Prelude.rnf guessMIMETypeEnabled
      `Prelude.seq` Prelude.rnf invalidUserList
      `Prelude.seq` Prelude.rnf kmsEncrypted
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf notificationPolicy
      `Prelude.seq` Prelude.rnf objectACL
      `Prelude.seq` Prelude.rnf oplocksEnabled
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf requesterPays
      `Prelude.seq` Prelude.rnf sMBACLEnabled
      `Prelude.seq` Prelude.rnf validUserList
      `Prelude.seq` Prelude.rnf fileShareARN

instance Data.ToHeaders UpdateSMBFileShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.UpdateSMBFileShare" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSMBFileShare where
  toJSON UpdateSMBFileShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessBasedEnumeration" Data..=)
              Prelude.<$> accessBasedEnumeration,
            ("AdminUserList" Data..=) Prelude.<$> adminUserList,
            ("AuditDestinationARN" Data..=)
              Prelude.<$> auditDestinationARN,
            ("CacheAttributes" Data..=)
              Prelude.<$> cacheAttributes,
            ("CaseSensitivity" Data..=)
              Prelude.<$> caseSensitivity,
            ("DefaultStorageClass" Data..=)
              Prelude.<$> defaultStorageClass,
            ("FileShareName" Data..=) Prelude.<$> fileShareName,
            ("GuessMIMETypeEnabled" Data..=)
              Prelude.<$> guessMIMETypeEnabled,
            ("InvalidUserList" Data..=)
              Prelude.<$> invalidUserList,
            ("KMSEncrypted" Data..=) Prelude.<$> kmsEncrypted,
            ("KMSKey" Data..=) Prelude.<$> kmsKey,
            ("NotificationPolicy" Data..=)
              Prelude.<$> notificationPolicy,
            ("ObjectACL" Data..=) Prelude.<$> objectACL,
            ("OplocksEnabled" Data..=)
              Prelude.<$> oplocksEnabled,
            ("ReadOnly" Data..=) Prelude.<$> readOnly,
            ("RequesterPays" Data..=) Prelude.<$> requesterPays,
            ("SMBACLEnabled" Data..=) Prelude.<$> sMBACLEnabled,
            ("ValidUserList" Data..=) Prelude.<$> validUserList,
            Prelude.Just ("FileShareARN" Data..= fileShareARN)
          ]
      )

instance Data.ToPath UpdateSMBFileShare where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSMBFileShare where
  toQuery = Prelude.const Prelude.mempty

-- | UpdateSMBFileShareOutput
--
-- /See:/ 'newUpdateSMBFileShareResponse' smart constructor.
data UpdateSMBFileShareResponse = UpdateSMBFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the updated SMB file share.
    fileShareARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSMBFileShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARN', 'updateSMBFileShareResponse_fileShareARN' - The Amazon Resource Name (ARN) of the updated SMB file share.
--
-- 'httpStatus', 'updateSMBFileShareResponse_httpStatus' - The response's http status code.
newUpdateSMBFileShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSMBFileShareResponse
newUpdateSMBFileShareResponse pHttpStatus_ =
  UpdateSMBFileShareResponse'
    { fileShareARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated SMB file share.
updateSMBFileShareResponse_fileShareARN :: Lens.Lens' UpdateSMBFileShareResponse (Prelude.Maybe Prelude.Text)
updateSMBFileShareResponse_fileShareARN = Lens.lens (\UpdateSMBFileShareResponse' {fileShareARN} -> fileShareARN) (\s@UpdateSMBFileShareResponse' {} a -> s {fileShareARN = a} :: UpdateSMBFileShareResponse)

-- | The response's http status code.
updateSMBFileShareResponse_httpStatus :: Lens.Lens' UpdateSMBFileShareResponse Prelude.Int
updateSMBFileShareResponse_httpStatus = Lens.lens (\UpdateSMBFileShareResponse' {httpStatus} -> httpStatus) (\s@UpdateSMBFileShareResponse' {} a -> s {httpStatus = a} :: UpdateSMBFileShareResponse)

instance Prelude.NFData UpdateSMBFileShareResponse where
  rnf UpdateSMBFileShareResponse' {..} =
    Prelude.rnf fileShareARN
      `Prelude.seq` Prelude.rnf httpStatus
