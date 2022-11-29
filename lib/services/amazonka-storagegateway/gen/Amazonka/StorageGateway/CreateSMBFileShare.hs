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
-- Module      : Amazonka.StorageGateway.CreateSMBFileShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Server Message Block (SMB) file share on an existing S3 File
-- Gateway. In Storage Gateway, a file share is a file system mount point
-- backed by Amazon S3 cloud storage. Storage Gateway exposes file shares
-- using an SMB interface. This operation is only supported for S3 File
-- Gateways.
--
-- S3 File Gateways require Security Token Service (Amazon Web Services
-- STS) to be activated to enable you to create a file share. Make sure
-- that Amazon Web Services STS is activated in the Amazon Web Services
-- Region you are creating your S3 File Gateway in. If Amazon Web Services
-- STS is not activated in this Amazon Web Services Region, activate it.
-- For information about how to activate Amazon Web Services STS, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating Amazon Web Services STS in an Amazon Web Services Region>
-- in the /Identity and Access Management User Guide/.
--
-- File gateways don\'t support creating hard or symbolic links on a file
-- share.
module Amazonka.StorageGateway.CreateSMBFileShare
  ( -- * Creating a Request
    CreateSMBFileShare (..),
    newCreateSMBFileShare,

    -- * Request Lenses
    createSMBFileShare_tags,
    createSMBFileShare_validUserList,
    createSMBFileShare_authentication,
    createSMBFileShare_fileShareName,
    createSMBFileShare_requesterPays,
    createSMBFileShare_objectACL,
    createSMBFileShare_caseSensitivity,
    createSMBFileShare_kmsKey,
    createSMBFileShare_sMBACLEnabled,
    createSMBFileShare_oplocksEnabled,
    createSMBFileShare_vPCEndpointDNSName,
    createSMBFileShare_accessBasedEnumeration,
    createSMBFileShare_invalidUserList,
    createSMBFileShare_adminUserList,
    createSMBFileShare_kmsEncrypted,
    createSMBFileShare_defaultStorageClass,
    createSMBFileShare_cacheAttributes,
    createSMBFileShare_readOnly,
    createSMBFileShare_bucketRegion,
    createSMBFileShare_auditDestinationARN,
    createSMBFileShare_guessMIMETypeEnabled,
    createSMBFileShare_notificationPolicy,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | CreateSMBFileShareInput
--
-- /See:/ 'newCreateSMBFileShare' smart constructor.
data CreateSMBFileShare = CreateSMBFileShare'
  { -- | A list of up to 50 tags that can be assigned to the NFS file share. Each
    -- tag is a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
    -- | A list of users or groups in the Active Directory that are allowed to
    -- access the file < > share. A group must be prefixed with the \@
    -- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    validUserList :: Prelude.Maybe [Prelude.Text],
    -- | The authentication method that users use to access the file share. The
    -- default is @ActiveDirectory@.
    --
    -- Valid Values: @ActiveDirectory@ | @GuestAccess@
    authentication :: Prelude.Maybe Prelude.Text,
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
    -- | A value that sets the access control list (ACL) permission for objects
    -- in the S3 bucket that a S3 File Gateway puts objects into. The default
    -- value is @private@.
    objectACL :: Prelude.Maybe ObjectACL,
    -- | The case of an object name in an Amazon S3 bucket. For
    -- @ClientSpecified@, the client determines the case sensitivity. For
    -- @CaseSensitive@, the gateway determines the case sensitivity. The
    -- default value is @ClientSpecified@.
    caseSensitivity :: Prelude.Maybe CaseSensitivity,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies whether opportunistic locking is enabled for the SMB file
    -- share.
    --
    -- Enabling opportunistic locking on case-sensitive shares is not
    -- recommended for workloads that involve access to files with the same
    -- name in different case.
    --
    -- Valid Values: @true@ | @false@
    oplocksEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the DNS name for the VPC endpoint that the SMB file share uses
    -- to connect to Amazon S3.
    --
    -- This parameter is required for SMB file shares that connect to Amazon S3
    -- through a VPC endpoint, a VPC access point, or an access point alias
    -- that points to a VPC access point.
    vPCEndpointDNSName :: Prelude.Maybe Prelude.Text,
    -- | The files and folders on this share will only be visible to users with
    -- read access.
    accessBasedEnumeration :: Prelude.Maybe Prelude.Bool,
    -- | A list of users or groups in the Active Directory that are not allowed
    -- to access the file share. A group must be prefixed with the \@
    -- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
    -- set to @ActiveDirectory@.
    invalidUserList :: Prelude.Maybe [Prelude.Text],
    -- | A list of users or groups in the Active Directory that will be granted
    -- administrator privileges on the file share. These users can do all file
    -- operations as the super-user. Acceptable formats include:
    -- @DOMAIN\\User1@, @user1@, @\@group1@, and @\@DOMAIN\\group1@.
    --
    -- Use this option very carefully, because any user in this list can do
    -- anything they like on the file share, regardless of file permissions.
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
    -- | Specifies refresh cache information for the file share.
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
    notificationPolicy :: Prelude.Maybe Prelude.Text,
    -- | A unique string value that you supply that is used by S3 File Gateway to
    -- ensure idempotent file share creation.
    clientToken :: Prelude.Text,
    -- | The ARN of the S3 File Gateway on which you want to create a file share.
    gatewayARN :: Prelude.Text,
    -- | The ARN of the Identity and Access Management (IAM) role that an S3 File
    -- Gateway assumes when it accesses the underlying storage.
    role' :: Prelude.Text,
    -- | A custom ARN for the backend storage used for storing data for file
    -- shares. It includes a resource ARN with an optional prefix
    -- concatenation. The prefix must end with a forward slash (\/).
    --
    -- You can specify LocationARN as a bucket ARN, access point ARN or access
    -- point alias, as shown in the following examples.
    --
    -- Bucket ARN:
    --
    -- @arn:aws:s3:::my-bucket\/prefix\/@
    --
    -- Access point ARN:
    --
    -- @arn:aws:s3:region:account-id:accesspoint\/access-point-name\/prefix\/@
    --
    -- If you specify an access point, the bucket policy must be configured to
    -- delegate access control to the access point. For information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-points-policies.html#access-points-delegating-control Delegating access control to access points>
    -- in the /Amazon S3 User Guide/.
    --
    -- Access point alias:
    --
    -- @test-ap-ab123cdef4gehijklmn5opqrstuvuse1a-s3alias@
    locationARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSMBFileShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSMBFileShare_tags' - A list of up to 50 tags that can be assigned to the NFS file share. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'validUserList', 'createSMBFileShare_validUserList' - A list of users or groups in the Active Directory that are allowed to
-- access the file < > share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'authentication', 'createSMBFileShare_authentication' - The authentication method that users use to access the file share. The
-- default is @ActiveDirectory@.
--
-- Valid Values: @ActiveDirectory@ | @GuestAccess@
--
-- 'fileShareName', 'createSMBFileShare_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
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
-- 'objectACL', 'createSMBFileShare_objectACL' - A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a S3 File Gateway puts objects into. The default
-- value is @private@.
--
-- 'caseSensitivity', 'createSMBFileShare_caseSensitivity' - The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
--
-- 'kmsKey', 'createSMBFileShare_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'sMBACLEnabled', 'createSMBFileShare_sMBACLEnabled' - Set this value to @true@ to enable access control list (ACL) on the SMB
-- file share. Set it to @false@ to map file and directory permissions to
-- the POSIX permissions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /Storage Gateway User Guide/.
--
-- Valid Values: @true@ | @false@
--
-- 'oplocksEnabled', 'createSMBFileShare_oplocksEnabled' - Specifies whether opportunistic locking is enabled for the SMB file
-- share.
--
-- Enabling opportunistic locking on case-sensitive shares is not
-- recommended for workloads that involve access to files with the same
-- name in different case.
--
-- Valid Values: @true@ | @false@
--
-- 'vPCEndpointDNSName', 'createSMBFileShare_vPCEndpointDNSName' - Specifies the DNS name for the VPC endpoint that the SMB file share uses
-- to connect to Amazon S3.
--
-- This parameter is required for SMB file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
--
-- 'accessBasedEnumeration', 'createSMBFileShare_accessBasedEnumeration' - The files and folders on this share will only be visible to users with
-- read access.
--
-- 'invalidUserList', 'createSMBFileShare_invalidUserList' - A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
--
-- 'adminUserList', 'createSMBFileShare_adminUserList' - A list of users or groups in the Active Directory that will be granted
-- administrator privileges on the file share. These users can do all file
-- operations as the super-user. Acceptable formats include:
-- @DOMAIN\\User1@, @user1@, @\@group1@, and @\@DOMAIN\\group1@.
--
-- Use this option very carefully, because any user in this list can do
-- anything they like on the file share, regardless of file permissions.
--
-- 'kmsEncrypted', 'createSMBFileShare_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'defaultStorageClass', 'createSMBFileShare_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'cacheAttributes', 'createSMBFileShare_cacheAttributes' - Specifies refresh cache information for the file share.
--
-- 'readOnly', 'createSMBFileShare_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'bucketRegion', 'createSMBFileShare_bucketRegion' - Specifies the Region of the S3 bucket where the SMB file share stores
-- files.
--
-- This parameter is required for SMB file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
--
-- 'auditDestinationARN', 'createSMBFileShare_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for audit logs.
--
-- 'guessMIMETypeEnabled', 'createSMBFileShare_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'notificationPolicy', 'createSMBFileShare_notificationPolicy' - The notification policy of the file share. @SettlingTimeInSeconds@
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
-- 'clientToken', 'createSMBFileShare_clientToken' - A unique string value that you supply that is used by S3 File Gateway to
-- ensure idempotent file share creation.
--
-- 'gatewayARN', 'createSMBFileShare_gatewayARN' - The ARN of the S3 File Gateway on which you want to create a file share.
--
-- 'role'', 'createSMBFileShare_role' - The ARN of the Identity and Access Management (IAM) role that an S3 File
-- Gateway assumes when it accesses the underlying storage.
--
-- 'locationARN', 'createSMBFileShare_locationARN' - A custom ARN for the backend storage used for storing data for file
-- shares. It includes a resource ARN with an optional prefix
-- concatenation. The prefix must end with a forward slash (\/).
--
-- You can specify LocationARN as a bucket ARN, access point ARN or access
-- point alias, as shown in the following examples.
--
-- Bucket ARN:
--
-- @arn:aws:s3:::my-bucket\/prefix\/@
--
-- Access point ARN:
--
-- @arn:aws:s3:region:account-id:accesspoint\/access-point-name\/prefix\/@
--
-- If you specify an access point, the bucket policy must be configured to
-- delegate access control to the access point. For information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-points-policies.html#access-points-delegating-control Delegating access control to access points>
-- in the /Amazon S3 User Guide/.
--
-- Access point alias:
--
-- @test-ap-ab123cdef4gehijklmn5opqrstuvuse1a-s3alias@
newCreateSMBFileShare ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  -- | 'locationARN'
  Prelude.Text ->
  CreateSMBFileShare
newCreateSMBFileShare
  pClientToken_
  pGatewayARN_
  pRole_
  pLocationARN_ =
    CreateSMBFileShare'
      { tags = Prelude.Nothing,
        validUserList = Prelude.Nothing,
        authentication = Prelude.Nothing,
        fileShareName = Prelude.Nothing,
        requesterPays = Prelude.Nothing,
        objectACL = Prelude.Nothing,
        caseSensitivity = Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        sMBACLEnabled = Prelude.Nothing,
        oplocksEnabled = Prelude.Nothing,
        vPCEndpointDNSName = Prelude.Nothing,
        accessBasedEnumeration = Prelude.Nothing,
        invalidUserList = Prelude.Nothing,
        adminUserList = Prelude.Nothing,
        kmsEncrypted = Prelude.Nothing,
        defaultStorageClass = Prelude.Nothing,
        cacheAttributes = Prelude.Nothing,
        readOnly = Prelude.Nothing,
        bucketRegion = Prelude.Nothing,
        auditDestinationARN = Prelude.Nothing,
        guessMIMETypeEnabled = Prelude.Nothing,
        notificationPolicy = Prelude.Nothing,
        clientToken = pClientToken_,
        gatewayARN = pGatewayARN_,
        role' = pRole_,
        locationARN = pLocationARN_
      }

-- | A list of up to 50 tags that can be assigned to the NFS file share. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createSMBFileShare_tags :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe [Tag])
createSMBFileShare_tags = Lens.lens (\CreateSMBFileShare' {tags} -> tags) (\s@CreateSMBFileShare' {} a -> s {tags = a} :: CreateSMBFileShare) Prelude.. Lens.mapping Lens.coerced

-- | A list of users or groups in the Active Directory that are allowed to
-- access the file < > share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
createSMBFileShare_validUserList :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe [Prelude.Text])
createSMBFileShare_validUserList = Lens.lens (\CreateSMBFileShare' {validUserList} -> validUserList) (\s@CreateSMBFileShare' {} a -> s {validUserList = a} :: CreateSMBFileShare) Prelude.. Lens.mapping Lens.coerced

-- | The authentication method that users use to access the file share. The
-- default is @ActiveDirectory@.
--
-- Valid Values: @ActiveDirectory@ | @GuestAccess@
createSMBFileShare_authentication :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Text)
createSMBFileShare_authentication = Lens.lens (\CreateSMBFileShare' {authentication} -> authentication) (\s@CreateSMBFileShare' {} a -> s {authentication = a} :: CreateSMBFileShare)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
createSMBFileShare_fileShareName :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Text)
createSMBFileShare_fileShareName = Lens.lens (\CreateSMBFileShare' {fileShareName} -> fileShareName) (\s@CreateSMBFileShare' {} a -> s {fileShareName = a} :: CreateSMBFileShare)

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
createSMBFileShare_requesterPays :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Bool)
createSMBFileShare_requesterPays = Lens.lens (\CreateSMBFileShare' {requesterPays} -> requesterPays) (\s@CreateSMBFileShare' {} a -> s {requesterPays = a} :: CreateSMBFileShare)

-- | A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a S3 File Gateway puts objects into. The default
-- value is @private@.
createSMBFileShare_objectACL :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe ObjectACL)
createSMBFileShare_objectACL = Lens.lens (\CreateSMBFileShare' {objectACL} -> objectACL) (\s@CreateSMBFileShare' {} a -> s {objectACL = a} :: CreateSMBFileShare)

-- | The case of an object name in an Amazon S3 bucket. For
-- @ClientSpecified@, the client determines the case sensitivity. For
-- @CaseSensitive@, the gateway determines the case sensitivity. The
-- default value is @ClientSpecified@.
createSMBFileShare_caseSensitivity :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe CaseSensitivity)
createSMBFileShare_caseSensitivity = Lens.lens (\CreateSMBFileShare' {caseSensitivity} -> caseSensitivity) (\s@CreateSMBFileShare' {} a -> s {caseSensitivity = a} :: CreateSMBFileShare)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createSMBFileShare_kmsKey :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Text)
createSMBFileShare_kmsKey = Lens.lens (\CreateSMBFileShare' {kmsKey} -> kmsKey) (\s@CreateSMBFileShare' {} a -> s {kmsKey = a} :: CreateSMBFileShare)

-- | Set this value to @true@ to enable access control list (ACL) on the SMB
-- file share. Set it to @false@ to map file and directory permissions to
-- the POSIX permissions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/smb-acl.html Using Microsoft Windows ACLs to control access to an SMB file share>
-- in the /Storage Gateway User Guide/.
--
-- Valid Values: @true@ | @false@
createSMBFileShare_sMBACLEnabled :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Bool)
createSMBFileShare_sMBACLEnabled = Lens.lens (\CreateSMBFileShare' {sMBACLEnabled} -> sMBACLEnabled) (\s@CreateSMBFileShare' {} a -> s {sMBACLEnabled = a} :: CreateSMBFileShare)

-- | Specifies whether opportunistic locking is enabled for the SMB file
-- share.
--
-- Enabling opportunistic locking on case-sensitive shares is not
-- recommended for workloads that involve access to files with the same
-- name in different case.
--
-- Valid Values: @true@ | @false@
createSMBFileShare_oplocksEnabled :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Bool)
createSMBFileShare_oplocksEnabled = Lens.lens (\CreateSMBFileShare' {oplocksEnabled} -> oplocksEnabled) (\s@CreateSMBFileShare' {} a -> s {oplocksEnabled = a} :: CreateSMBFileShare)

-- | Specifies the DNS name for the VPC endpoint that the SMB file share uses
-- to connect to Amazon S3.
--
-- This parameter is required for SMB file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
createSMBFileShare_vPCEndpointDNSName :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Text)
createSMBFileShare_vPCEndpointDNSName = Lens.lens (\CreateSMBFileShare' {vPCEndpointDNSName} -> vPCEndpointDNSName) (\s@CreateSMBFileShare' {} a -> s {vPCEndpointDNSName = a} :: CreateSMBFileShare)

-- | The files and folders on this share will only be visible to users with
-- read access.
createSMBFileShare_accessBasedEnumeration :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Bool)
createSMBFileShare_accessBasedEnumeration = Lens.lens (\CreateSMBFileShare' {accessBasedEnumeration} -> accessBasedEnumeration) (\s@CreateSMBFileShare' {} a -> s {accessBasedEnumeration = a} :: CreateSMBFileShare)

-- | A list of users or groups in the Active Directory that are not allowed
-- to access the file share. A group must be prefixed with the \@
-- character. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @\@group1@, and @\@DOMAIN\\group1@. Can only be set if Authentication is
-- set to @ActiveDirectory@.
createSMBFileShare_invalidUserList :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe [Prelude.Text])
createSMBFileShare_invalidUserList = Lens.lens (\CreateSMBFileShare' {invalidUserList} -> invalidUserList) (\s@CreateSMBFileShare' {} a -> s {invalidUserList = a} :: CreateSMBFileShare) Prelude.. Lens.mapping Lens.coerced

-- | A list of users or groups in the Active Directory that will be granted
-- administrator privileges on the file share. These users can do all file
-- operations as the super-user. Acceptable formats include:
-- @DOMAIN\\User1@, @user1@, @\@group1@, and @\@DOMAIN\\group1@.
--
-- Use this option very carefully, because any user in this list can do
-- anything they like on the file share, regardless of file permissions.
createSMBFileShare_adminUserList :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe [Prelude.Text])
createSMBFileShare_adminUserList = Lens.lens (\CreateSMBFileShare' {adminUserList} -> adminUserList) (\s@CreateSMBFileShare' {} a -> s {adminUserList = a} :: CreateSMBFileShare) Prelude.. Lens.mapping Lens.coerced

-- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createSMBFileShare_kmsEncrypted :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Bool)
createSMBFileShare_kmsEncrypted = Lens.lens (\CreateSMBFileShare' {kmsEncrypted} -> kmsEncrypted) (\s@CreateSMBFileShare' {} a -> s {kmsEncrypted = a} :: CreateSMBFileShare)

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
createSMBFileShare_defaultStorageClass :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Text)
createSMBFileShare_defaultStorageClass = Lens.lens (\CreateSMBFileShare' {defaultStorageClass} -> defaultStorageClass) (\s@CreateSMBFileShare' {} a -> s {defaultStorageClass = a} :: CreateSMBFileShare)

-- | Specifies refresh cache information for the file share.
createSMBFileShare_cacheAttributes :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe CacheAttributes)
createSMBFileShare_cacheAttributes = Lens.lens (\CreateSMBFileShare' {cacheAttributes} -> cacheAttributes) (\s@CreateSMBFileShare' {} a -> s {cacheAttributes = a} :: CreateSMBFileShare)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
createSMBFileShare_readOnly :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Bool)
createSMBFileShare_readOnly = Lens.lens (\CreateSMBFileShare' {readOnly} -> readOnly) (\s@CreateSMBFileShare' {} a -> s {readOnly = a} :: CreateSMBFileShare)

-- | Specifies the Region of the S3 bucket where the SMB file share stores
-- files.
--
-- This parameter is required for SMB file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
createSMBFileShare_bucketRegion :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Text)
createSMBFileShare_bucketRegion = Lens.lens (\CreateSMBFileShare' {bucketRegion} -> bucketRegion) (\s@CreateSMBFileShare' {} a -> s {bucketRegion = a} :: CreateSMBFileShare)

-- | The Amazon Resource Name (ARN) of the storage used for audit logs.
createSMBFileShare_auditDestinationARN :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Text)
createSMBFileShare_auditDestinationARN = Lens.lens (\CreateSMBFileShare' {auditDestinationARN} -> auditDestinationARN) (\s@CreateSMBFileShare' {} a -> s {auditDestinationARN = a} :: CreateSMBFileShare)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
createSMBFileShare_guessMIMETypeEnabled :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Bool)
createSMBFileShare_guessMIMETypeEnabled = Lens.lens (\CreateSMBFileShare' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@CreateSMBFileShare' {} a -> s {guessMIMETypeEnabled = a} :: CreateSMBFileShare)

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
createSMBFileShare_notificationPolicy :: Lens.Lens' CreateSMBFileShare (Prelude.Maybe Prelude.Text)
createSMBFileShare_notificationPolicy = Lens.lens (\CreateSMBFileShare' {notificationPolicy} -> notificationPolicy) (\s@CreateSMBFileShare' {} a -> s {notificationPolicy = a} :: CreateSMBFileShare)

-- | A unique string value that you supply that is used by S3 File Gateway to
-- ensure idempotent file share creation.
createSMBFileShare_clientToken :: Lens.Lens' CreateSMBFileShare Prelude.Text
createSMBFileShare_clientToken = Lens.lens (\CreateSMBFileShare' {clientToken} -> clientToken) (\s@CreateSMBFileShare' {} a -> s {clientToken = a} :: CreateSMBFileShare)

-- | The ARN of the S3 File Gateway on which you want to create a file share.
createSMBFileShare_gatewayARN :: Lens.Lens' CreateSMBFileShare Prelude.Text
createSMBFileShare_gatewayARN = Lens.lens (\CreateSMBFileShare' {gatewayARN} -> gatewayARN) (\s@CreateSMBFileShare' {} a -> s {gatewayARN = a} :: CreateSMBFileShare)

-- | The ARN of the Identity and Access Management (IAM) role that an S3 File
-- Gateway assumes when it accesses the underlying storage.
createSMBFileShare_role :: Lens.Lens' CreateSMBFileShare Prelude.Text
createSMBFileShare_role = Lens.lens (\CreateSMBFileShare' {role'} -> role') (\s@CreateSMBFileShare' {} a -> s {role' = a} :: CreateSMBFileShare)

-- | A custom ARN for the backend storage used for storing data for file
-- shares. It includes a resource ARN with an optional prefix
-- concatenation. The prefix must end with a forward slash (\/).
--
-- You can specify LocationARN as a bucket ARN, access point ARN or access
-- point alias, as shown in the following examples.
--
-- Bucket ARN:
--
-- @arn:aws:s3:::my-bucket\/prefix\/@
--
-- Access point ARN:
--
-- @arn:aws:s3:region:account-id:accesspoint\/access-point-name\/prefix\/@
--
-- If you specify an access point, the bucket policy must be configured to
-- delegate access control to the access point. For information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-points-policies.html#access-points-delegating-control Delegating access control to access points>
-- in the /Amazon S3 User Guide/.
--
-- Access point alias:
--
-- @test-ap-ab123cdef4gehijklmn5opqrstuvuse1a-s3alias@
createSMBFileShare_locationARN :: Lens.Lens' CreateSMBFileShare Prelude.Text
createSMBFileShare_locationARN = Lens.lens (\CreateSMBFileShare' {locationARN} -> locationARN) (\s@CreateSMBFileShare' {} a -> s {locationARN = a} :: CreateSMBFileShare)

instance Core.AWSRequest CreateSMBFileShare where
  type
    AWSResponse CreateSMBFileShare =
      CreateSMBFileShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSMBFileShareResponse'
            Prelude.<$> (x Core..?> "FileShareARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSMBFileShare where
  hashWithSalt _salt CreateSMBFileShare' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` validUserList
      `Prelude.hashWithSalt` authentication
      `Prelude.hashWithSalt` fileShareName
      `Prelude.hashWithSalt` requesterPays
      `Prelude.hashWithSalt` objectACL
      `Prelude.hashWithSalt` caseSensitivity
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` sMBACLEnabled
      `Prelude.hashWithSalt` oplocksEnabled
      `Prelude.hashWithSalt` vPCEndpointDNSName
      `Prelude.hashWithSalt` accessBasedEnumeration
      `Prelude.hashWithSalt` invalidUserList
      `Prelude.hashWithSalt` adminUserList
      `Prelude.hashWithSalt` kmsEncrypted
      `Prelude.hashWithSalt` defaultStorageClass
      `Prelude.hashWithSalt` cacheAttributes
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` bucketRegion
      `Prelude.hashWithSalt` auditDestinationARN
      `Prelude.hashWithSalt` guessMIMETypeEnabled
      `Prelude.hashWithSalt` notificationPolicy
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` locationARN

instance Prelude.NFData CreateSMBFileShare where
  rnf CreateSMBFileShare' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf validUserList
      `Prelude.seq` Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf fileShareName
      `Prelude.seq` Prelude.rnf requesterPays
      `Prelude.seq` Prelude.rnf objectACL
      `Prelude.seq` Prelude.rnf caseSensitivity
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf sMBACLEnabled
      `Prelude.seq` Prelude.rnf oplocksEnabled
      `Prelude.seq` Prelude.rnf vPCEndpointDNSName
      `Prelude.seq` Prelude.rnf accessBasedEnumeration
      `Prelude.seq` Prelude.rnf invalidUserList
      `Prelude.seq` Prelude.rnf adminUserList
      `Prelude.seq` Prelude.rnf kmsEncrypted
      `Prelude.seq` Prelude.rnf defaultStorageClass
      `Prelude.seq` Prelude.rnf cacheAttributes
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf bucketRegion
      `Prelude.seq` Prelude.rnf
        auditDestinationARN
      `Prelude.seq` Prelude.rnf
        guessMIMETypeEnabled
      `Prelude.seq` Prelude.rnf
        notificationPolicy
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf
        locationARN

instance Core.ToHeaders CreateSMBFileShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CreateSMBFileShare" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSMBFileShare where
  toJSON CreateSMBFileShare' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ValidUserList" Core..=) Prelude.<$> validUserList,
            ("Authentication" Core..=)
              Prelude.<$> authentication,
            ("FileShareName" Core..=) Prelude.<$> fileShareName,
            ("RequesterPays" Core..=) Prelude.<$> requesterPays,
            ("ObjectACL" Core..=) Prelude.<$> objectACL,
            ("CaseSensitivity" Core..=)
              Prelude.<$> caseSensitivity,
            ("KMSKey" Core..=) Prelude.<$> kmsKey,
            ("SMBACLEnabled" Core..=) Prelude.<$> sMBACLEnabled,
            ("OplocksEnabled" Core..=)
              Prelude.<$> oplocksEnabled,
            ("VPCEndpointDNSName" Core..=)
              Prelude.<$> vPCEndpointDNSName,
            ("AccessBasedEnumeration" Core..=)
              Prelude.<$> accessBasedEnumeration,
            ("InvalidUserList" Core..=)
              Prelude.<$> invalidUserList,
            ("AdminUserList" Core..=) Prelude.<$> adminUserList,
            ("KMSEncrypted" Core..=) Prelude.<$> kmsEncrypted,
            ("DefaultStorageClass" Core..=)
              Prelude.<$> defaultStorageClass,
            ("CacheAttributes" Core..=)
              Prelude.<$> cacheAttributes,
            ("ReadOnly" Core..=) Prelude.<$> readOnly,
            ("BucketRegion" Core..=) Prelude.<$> bucketRegion,
            ("AuditDestinationARN" Core..=)
              Prelude.<$> auditDestinationARN,
            ("GuessMIMETypeEnabled" Core..=)
              Prelude.<$> guessMIMETypeEnabled,
            ("NotificationPolicy" Core..=)
              Prelude.<$> notificationPolicy,
            Prelude.Just ("ClientToken" Core..= clientToken),
            Prelude.Just ("GatewayARN" Core..= gatewayARN),
            Prelude.Just ("Role" Core..= role'),
            Prelude.Just ("LocationARN" Core..= locationARN)
          ]
      )

instance Core.ToPath CreateSMBFileShare where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSMBFileShare where
  toQuery = Prelude.const Prelude.mempty

-- | CreateSMBFileShareOutput
--
-- /See:/ 'newCreateSMBFileShareResponse' smart constructor.
data CreateSMBFileShareResponse = CreateSMBFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the newly created file share.
    fileShareARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateSMBFileShareResponse
newCreateSMBFileShareResponse pHttpStatus_ =
  CreateSMBFileShareResponse'
    { fileShareARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
createSMBFileShareResponse_fileShareARN :: Lens.Lens' CreateSMBFileShareResponse (Prelude.Maybe Prelude.Text)
createSMBFileShareResponse_fileShareARN = Lens.lens (\CreateSMBFileShareResponse' {fileShareARN} -> fileShareARN) (\s@CreateSMBFileShareResponse' {} a -> s {fileShareARN = a} :: CreateSMBFileShareResponse)

-- | The response's http status code.
createSMBFileShareResponse_httpStatus :: Lens.Lens' CreateSMBFileShareResponse Prelude.Int
createSMBFileShareResponse_httpStatus = Lens.lens (\CreateSMBFileShareResponse' {httpStatus} -> httpStatus) (\s@CreateSMBFileShareResponse' {} a -> s {httpStatus = a} :: CreateSMBFileShareResponse)

instance Prelude.NFData CreateSMBFileShareResponse where
  rnf CreateSMBFileShareResponse' {..} =
    Prelude.rnf fileShareARN
      `Prelude.seq` Prelude.rnf httpStatus
