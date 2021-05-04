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
-- Module      : Network.AWS.StorageGateway.UpdateSMBFileShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Server Message Block (SMB) file share. This operation is only
-- supported for file gateways.
--
-- To leave a file share field unchanged, set the corresponding input field
-- to null.
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
module Network.AWS.StorageGateway.UpdateSMBFileShare
  ( -- * Creating a Request
    UpdateSMBFileShare (..),
    newUpdateSMBFileShare,

    -- * Request Lenses
    updateSMBFileShare_sMBACLEnabled,
    updateSMBFileShare_accessBasedEnumeration,
    updateSMBFileShare_defaultStorageClass,
    updateSMBFileShare_fileShareName,
    updateSMBFileShare_caseSensitivity,
    updateSMBFileShare_guessMIMETypeEnabled,
    updateSMBFileShare_readOnly,
    updateSMBFileShare_kmsEncrypted,
    updateSMBFileShare_notificationPolicy,
    updateSMBFileShare_validUserList,
    updateSMBFileShare_kmsKey,
    updateSMBFileShare_adminUserList,
    updateSMBFileShare_auditDestinationARN,
    updateSMBFileShare_cacheAttributes,
    updateSMBFileShare_objectACL,
    updateSMBFileShare_requesterPays,
    updateSMBFileShare_invalidUserList,
    updateSMBFileShare_fileShareARN,

    -- * Destructuring the Response
    UpdateSMBFileShareResponse (..),
    newUpdateSMBFileShareResponse,

    -- * Response Lenses
    updateSMBFileShareResponse_fileShareARN,
    updateSMBFileShareResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | UpdateSMBFileShareInput
--
-- /See:/ 'newUpdateSMBFileShare' smart constructor.
data UpdateSMBFileShare = UpdateSMBFileShare'
  { -- | Set this value to @true@ to enable access control list (ACL) on the SMB
    -- file share. Set it to @false@ to map file and directory permissions to
    -- the POSIX permissions.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
    -- in the /AWS Storage Gateway User Guide/.
    --
    -- Valid Values: @true@ | @false@
    sMBACLEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The files and folders on this share will only be visible to users with
    -- read access.
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
    -- @true@ to set write status to read-only, otherwise set to @false@.
    --
    -- Valid Values: @true@ | @false@
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
    -- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The notification policy of the file share.
    notificationPolicy :: Prelude.Maybe Prelude.Text,
    -- | A list of users or groups in the Active Directory that are allowed to
    -- access the file share. A group must be prefixed with the \@ character.
    -- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
    -- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
    -- @ActiveDirectory@.
    validUserList :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | A list of users or groups in the Active Directory that have
    -- administrator rights to the file share. A group must be prefixed with
    -- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    adminUserList :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Prelude.Maybe Prelude.Text,
    -- | Refresh cache information.
    cacheAttributes :: Prelude.Maybe CacheAttributes,
    -- | A value that sets the access control list (ACL) permission for objects
    -- in the S3 bucket that a file gateway puts objects into. The default
    -- value is @private@.
    objectACL :: Prelude.Maybe ObjectACL,
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
    invalidUserList :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the SMB file share that you want to
    -- update.
    fileShareARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSMBFileShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sMBACLEnabled', 'updateSMBFileShare_sMBACLEnabled' - Set this value to @true@ to enable access control list (ACL) on the SMB
-- file share. Set it to @false@ to map file and directory permissions to
-- the POSIX permissions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /AWS Storage Gateway User Guide/.
--
-- Valid Values: @true@ | @false@
--
-- 'accessBasedEnumeration', 'updateSMBFileShare_accessBasedEnumeration' - The files and folders on this share will only be visible to users with
-- read access.
--
-- 'defaultStorageClass', 'updateSMBFileShare_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'fileShareName', 'updateSMBFileShare_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
--
-- 'caseSensitivity', 'updateSMBFileShare_caseSensitivity' - The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
--
-- 'guessMIMETypeEnabled', 'updateSMBFileShare_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'readOnly', 'updateSMBFileShare_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsEncrypted', 'updateSMBFileShare_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'notificationPolicy', 'updateSMBFileShare_notificationPolicy' - The notification policy of the file share.
--
-- 'validUserList', 'updateSMBFileShare_validUserList' - A list of users or groups in the Active Directory that are allowed to
-- access the file share. A group must be prefixed with the \@ character.
-- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
-- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
-- @ActiveDirectory@.
--
-- 'kmsKey', 'updateSMBFileShare_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'adminUserList', 'updateSMBFileShare_adminUserList' - A list of users or groups in the Active Directory that have
-- administrator rights to the file share. A group must be prefixed with
-- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'auditDestinationARN', 'updateSMBFileShare_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- 'cacheAttributes', 'updateSMBFileShare_cacheAttributes' - Refresh cache information.
--
-- 'objectACL', 'updateSMBFileShare_objectACL' - A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a file gateway puts objects into. The default
-- value is @private@.
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
-- 'invalidUserList', 'updateSMBFileShare_invalidUserList' - A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'fileShareARN', 'updateSMBFileShare_fileShareARN' - The Amazon Resource Name (ARN) of the SMB file share that you want to
-- update.
newUpdateSMBFileShare ::
  -- | 'fileShareARN'
  Prelude.Text ->
  UpdateSMBFileShare
newUpdateSMBFileShare pFileShareARN_ =
  UpdateSMBFileShare'
    { sMBACLEnabled =
        Prelude.Nothing,
      accessBasedEnumeration = Prelude.Nothing,
      defaultStorageClass = Prelude.Nothing,
      fileShareName = Prelude.Nothing,
      caseSensitivity = Prelude.Nothing,
      guessMIMETypeEnabled = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      kmsEncrypted = Prelude.Nothing,
      notificationPolicy = Prelude.Nothing,
      validUserList = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      adminUserList = Prelude.Nothing,
      auditDestinationARN = Prelude.Nothing,
      cacheAttributes = Prelude.Nothing,
      objectACL = Prelude.Nothing,
      requesterPays = Prelude.Nothing,
      invalidUserList = Prelude.Nothing,
      fileShareARN = pFileShareARN_
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
updateSMBFileShare_sMBACLEnabled :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_sMBACLEnabled = Lens.lens (\UpdateSMBFileShare' {sMBACLEnabled} -> sMBACLEnabled) (\s@UpdateSMBFileShare' {} a -> s {sMBACLEnabled = a} :: UpdateSMBFileShare)

-- | The files and folders on this share will only be visible to users with
-- read access.
updateSMBFileShare_accessBasedEnumeration :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_accessBasedEnumeration = Lens.lens (\UpdateSMBFileShare' {accessBasedEnumeration} -> accessBasedEnumeration) (\s@UpdateSMBFileShare' {} a -> s {accessBasedEnumeration = a} :: UpdateSMBFileShare)

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
updateSMBFileShare_defaultStorageClass :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_defaultStorageClass = Lens.lens (\UpdateSMBFileShare' {defaultStorageClass} -> defaultStorageClass) (\s@UpdateSMBFileShare' {} a -> s {defaultStorageClass = a} :: UpdateSMBFileShare)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
updateSMBFileShare_fileShareName :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_fileShareName = Lens.lens (\UpdateSMBFileShare' {fileShareName} -> fileShareName) (\s@UpdateSMBFileShare' {} a -> s {fileShareName = a} :: UpdateSMBFileShare)

-- | The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
updateSMBFileShare_caseSensitivity :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe CaseSensitivity)
updateSMBFileShare_caseSensitivity = Lens.lens (\UpdateSMBFileShare' {caseSensitivity} -> caseSensitivity) (\s@UpdateSMBFileShare' {} a -> s {caseSensitivity = a} :: UpdateSMBFileShare)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
updateSMBFileShare_guessMIMETypeEnabled :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_guessMIMETypeEnabled = Lens.lens (\UpdateSMBFileShare' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@UpdateSMBFileShare' {} a -> s {guessMIMETypeEnabled = a} :: UpdateSMBFileShare)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
updateSMBFileShare_readOnly :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_readOnly = Lens.lens (\UpdateSMBFileShare' {readOnly} -> readOnly) (\s@UpdateSMBFileShare' {} a -> s {readOnly = a} :: UpdateSMBFileShare)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
updateSMBFileShare_kmsEncrypted :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Bool)
updateSMBFileShare_kmsEncrypted = Lens.lens (\UpdateSMBFileShare' {kmsEncrypted} -> kmsEncrypted) (\s@UpdateSMBFileShare' {} a -> s {kmsEncrypted = a} :: UpdateSMBFileShare)

-- | The notification policy of the file share.
updateSMBFileShare_notificationPolicy :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_notificationPolicy = Lens.lens (\UpdateSMBFileShare' {notificationPolicy} -> notificationPolicy) (\s@UpdateSMBFileShare' {} a -> s {notificationPolicy = a} :: UpdateSMBFileShare)

-- | A list of users or groups in the Active Directory that are allowed to
-- access the file share. A group must be prefixed with the \@ character.
-- Acceptable formats include: @DOMAIN\\User1@, @user1@, @\@group1@, and
-- @\@DOMAIN\\group1@. Can only be set if Authentication is set to
-- @ActiveDirectory@.
updateSMBFileShare_validUserList :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe [Prelude.Text])
updateSMBFileShare_validUserList = Lens.lens (\UpdateSMBFileShare' {validUserList} -> validUserList) (\s@UpdateSMBFileShare' {} a -> s {validUserList = a} :: UpdateSMBFileShare) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
updateSMBFileShare_kmsKey :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_kmsKey = Lens.lens (\UpdateSMBFileShare' {kmsKey} -> kmsKey) (\s@UpdateSMBFileShare' {} a -> s {kmsKey = a} :: UpdateSMBFileShare)

-- | A list of users or groups in the Active Directory that have
-- administrator rights to the file share. A group must be prefixed with
-- the \@ character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
updateSMBFileShare_adminUserList :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe [Prelude.Text])
updateSMBFileShare_adminUserList = Lens.lens (\UpdateSMBFileShare' {adminUserList} -> adminUserList) (\s@UpdateSMBFileShare' {} a -> s {adminUserList = a} :: UpdateSMBFileShare) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
updateSMBFileShare_auditDestinationARN :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe Prelude.Text)
updateSMBFileShare_auditDestinationARN = Lens.lens (\UpdateSMBFileShare' {auditDestinationARN} -> auditDestinationARN) (\s@UpdateSMBFileShare' {} a -> s {auditDestinationARN = a} :: UpdateSMBFileShare)

-- | Refresh cache information.
updateSMBFileShare_cacheAttributes :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe CacheAttributes)
updateSMBFileShare_cacheAttributes = Lens.lens (\UpdateSMBFileShare' {cacheAttributes} -> cacheAttributes) (\s@UpdateSMBFileShare' {} a -> s {cacheAttributes = a} :: UpdateSMBFileShare)

-- | A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a file gateway puts objects into. The default
-- value is @private@.
updateSMBFileShare_objectACL :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe ObjectACL)
updateSMBFileShare_objectACL = Lens.lens (\UpdateSMBFileShare' {objectACL} -> objectACL) (\s@UpdateSMBFileShare' {} a -> s {objectACL = a} :: UpdateSMBFileShare)

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

-- | A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
updateSMBFileShare_invalidUserList :: Lens.Lens' UpdateSMBFileShare (Prelude.Maybe [Prelude.Text])
updateSMBFileShare_invalidUserList = Lens.lens (\UpdateSMBFileShare' {invalidUserList} -> invalidUserList) (\s@UpdateSMBFileShare' {} a -> s {invalidUserList = a} :: UpdateSMBFileShare) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Name (ARN) of the SMB file share that you want to
-- update.
updateSMBFileShare_fileShareARN :: Lens.Lens' UpdateSMBFileShare Prelude.Text
updateSMBFileShare_fileShareARN = Lens.lens (\UpdateSMBFileShare' {fileShareARN} -> fileShareARN) (\s@UpdateSMBFileShare' {} a -> s {fileShareARN = a} :: UpdateSMBFileShare)

instance Prelude.AWSRequest UpdateSMBFileShare where
  type
    Rs UpdateSMBFileShare =
      UpdateSMBFileShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSMBFileShareResponse'
            Prelude.<$> (x Prelude..?> "FileShareARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSMBFileShare

instance Prelude.NFData UpdateSMBFileShare

instance Prelude.ToHeaders UpdateSMBFileShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.UpdateSMBFileShare" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateSMBFileShare where
  toJSON UpdateSMBFileShare' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SMBACLEnabled" Prelude..=)
              Prelude.<$> sMBACLEnabled,
            ("AccessBasedEnumeration" Prelude..=)
              Prelude.<$> accessBasedEnumeration,
            ("DefaultStorageClass" Prelude..=)
              Prelude.<$> defaultStorageClass,
            ("FileShareName" Prelude..=)
              Prelude.<$> fileShareName,
            ("CaseSensitivity" Prelude..=)
              Prelude.<$> caseSensitivity,
            ("GuessMIMETypeEnabled" Prelude..=)
              Prelude.<$> guessMIMETypeEnabled,
            ("ReadOnly" Prelude..=) Prelude.<$> readOnly,
            ("KMSEncrypted" Prelude..=) Prelude.<$> kmsEncrypted,
            ("NotificationPolicy" Prelude..=)
              Prelude.<$> notificationPolicy,
            ("ValidUserList" Prelude..=)
              Prelude.<$> validUserList,
            ("KMSKey" Prelude..=) Prelude.<$> kmsKey,
            ("AdminUserList" Prelude..=)
              Prelude.<$> adminUserList,
            ("AuditDestinationARN" Prelude..=)
              Prelude.<$> auditDestinationARN,
            ("CacheAttributes" Prelude..=)
              Prelude.<$> cacheAttributes,
            ("ObjectACL" Prelude..=) Prelude.<$> objectACL,
            ("RequesterPays" Prelude..=)
              Prelude.<$> requesterPays,
            ("InvalidUserList" Prelude..=)
              Prelude.<$> invalidUserList,
            Prelude.Just
              ("FileShareARN" Prelude..= fileShareARN)
          ]
      )

instance Prelude.ToPath UpdateSMBFileShare where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateSMBFileShare where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateSMBFileShareResponse
