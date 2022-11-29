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
-- Module      : Amazonka.StorageGateway.CreateNFSFileShare
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Network File System (NFS) file share on an existing S3 File
-- Gateway. In Storage Gateway, a file share is a file system mount point
-- backed by Amazon S3 cloud storage. Storage Gateway exposes file shares
-- using an NFS interface. This operation is only supported for S3 File
-- Gateways.
--
-- S3 File gateway requires Security Token Service (Amazon Web Services
-- STS) to be activated to enable you to create a file share. Make sure
-- Amazon Web Services STS is activated in the Amazon Web Services Region
-- you are creating your S3 File Gateway in. If Amazon Web Services STS is
-- not activated in the Amazon Web Services Region, activate it. For
-- information about how to activate Amazon Web Services STS, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and deactivating Amazon Web Services STS in an Amazon Web Services Region>
-- in the /Identity and Access Management User Guide/.
--
-- S3 File Gateways do not support creating hard or symbolic links on a
-- file share.
module Amazonka.StorageGateway.CreateNFSFileShare
  ( -- * Creating a Request
    CreateNFSFileShare (..),
    newCreateNFSFileShare,

    -- * Request Lenses
    createNFSFileShare_tags,
    createNFSFileShare_squash,
    createNFSFileShare_nFSFileShareDefaults,
    createNFSFileShare_fileShareName,
    createNFSFileShare_requesterPays,
    createNFSFileShare_objectACL,
    createNFSFileShare_kmsKey,
    createNFSFileShare_vPCEndpointDNSName,
    createNFSFileShare_kmsEncrypted,
    createNFSFileShare_defaultStorageClass,
    createNFSFileShare_cacheAttributes,
    createNFSFileShare_readOnly,
    createNFSFileShare_bucketRegion,
    createNFSFileShare_auditDestinationARN,
    createNFSFileShare_clientList,
    createNFSFileShare_guessMIMETypeEnabled,
    createNFSFileShare_notificationPolicy,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | CreateNFSFileShareInput
--
-- /See:/ 'newCreateNFSFileShare' smart constructor.
data CreateNFSFileShare = CreateNFSFileShare'
  { -- | A list of up to 50 tags that can be assigned to the NFS file share. Each
    -- tag is a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
    -- | A value that maps a user to anonymous user.
    --
    -- Valid values are the following:
    --
    -- -   @RootSquash@: Only root is mapped to anonymous user.
    --
    -- -   @NoSquash@: No one is mapped to anonymous user.
    --
    -- -   @AllSquash@: Everyone is mapped to anonymous user.
    squash :: Prelude.Maybe Prelude.Text,
    -- | File share default values. Optional.
    nFSFileShareDefaults :: Prelude.Maybe NFSFileShareDefaults,
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
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | Specifies the DNS name for the VPC endpoint that the NFS file share uses
    -- to connect to Amazon S3.
    --
    -- This parameter is required for NFS file shares that connect to Amazon S3
    -- through a VPC endpoint, a VPC access point, or an access point alias
    -- that points to a VPC access point.
    vPCEndpointDNSName :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies the Region of the S3 bucket where the NFS file share stores
    -- files.
    --
    -- This parameter is required for NFS file shares that connect to Amazon S3
    -- through a VPC endpoint, a VPC access point, or an access point alias
    -- that points to a VPC access point.
    bucketRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the storage used for audit logs.
    auditDestinationARN :: Prelude.Maybe Prelude.Text,
    -- | The list of clients that are allowed to access the S3 File Gateway. The
    -- list must contain either valid IP addresses or valid CIDR blocks.
    clientList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
    -- | The Amazon Resource Name (ARN) of the S3 File Gateway on which you want
    -- to create a file share.
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
-- Create a value of 'CreateNFSFileShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createNFSFileShare_tags' - A list of up to 50 tags that can be assigned to the NFS file share. Each
-- tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
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
-- 'nFSFileShareDefaults', 'createNFSFileShare_nFSFileShareDefaults' - File share default values. Optional.
--
-- 'fileShareName', 'createNFSFileShare_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
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
-- 'objectACL', 'createNFSFileShare_objectACL' - A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a S3 File Gateway puts objects into. The default
-- value is @private@.
--
-- 'kmsKey', 'createNFSFileShare_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'vPCEndpointDNSName', 'createNFSFileShare_vPCEndpointDNSName' - Specifies the DNS name for the VPC endpoint that the NFS file share uses
-- to connect to Amazon S3.
--
-- This parameter is required for NFS file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
--
-- 'kmsEncrypted', 'createNFSFileShare_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'defaultStorageClass', 'createNFSFileShare_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'cacheAttributes', 'createNFSFileShare_cacheAttributes' - Specifies refresh cache information for the file share.
--
-- 'readOnly', 'createNFSFileShare_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'bucketRegion', 'createNFSFileShare_bucketRegion' - Specifies the Region of the S3 bucket where the NFS file share stores
-- files.
--
-- This parameter is required for NFS file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
--
-- 'auditDestinationARN', 'createNFSFileShare_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for audit logs.
--
-- 'clientList', 'createNFSFileShare_clientList' - The list of clients that are allowed to access the S3 File Gateway. The
-- list must contain either valid IP addresses or valid CIDR blocks.
--
-- 'guessMIMETypeEnabled', 'createNFSFileShare_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'notificationPolicy', 'createNFSFileShare_notificationPolicy' - The notification policy of the file share. @SettlingTimeInSeconds@
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
-- 'clientToken', 'createNFSFileShare_clientToken' - A unique string value that you supply that is used by S3 File Gateway to
-- ensure idempotent file share creation.
--
-- 'gatewayARN', 'createNFSFileShare_gatewayARN' - The Amazon Resource Name (ARN) of the S3 File Gateway on which you want
-- to create a file share.
--
-- 'role'', 'createNFSFileShare_role' - The ARN of the Identity and Access Management (IAM) role that an S3 File
-- Gateway assumes when it accesses the underlying storage.
--
-- 'locationARN', 'createNFSFileShare_locationARN' - A custom ARN for the backend storage used for storing data for file
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
newCreateNFSFileShare ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  -- | 'locationARN'
  Prelude.Text ->
  CreateNFSFileShare
newCreateNFSFileShare
  pClientToken_
  pGatewayARN_
  pRole_
  pLocationARN_ =
    CreateNFSFileShare'
      { tags = Prelude.Nothing,
        squash = Prelude.Nothing,
        nFSFileShareDefaults = Prelude.Nothing,
        fileShareName = Prelude.Nothing,
        requesterPays = Prelude.Nothing,
        objectACL = Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        vPCEndpointDNSName = Prelude.Nothing,
        kmsEncrypted = Prelude.Nothing,
        defaultStorageClass = Prelude.Nothing,
        cacheAttributes = Prelude.Nothing,
        readOnly = Prelude.Nothing,
        bucketRegion = Prelude.Nothing,
        auditDestinationARN = Prelude.Nothing,
        clientList = Prelude.Nothing,
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
createNFSFileShare_tags :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe [Tag])
createNFSFileShare_tags = Lens.lens (\CreateNFSFileShare' {tags} -> tags) (\s@CreateNFSFileShare' {} a -> s {tags = a} :: CreateNFSFileShare) Prelude.. Lens.mapping Lens.coerced

-- | A value that maps a user to anonymous user.
--
-- Valid values are the following:
--
-- -   @RootSquash@: Only root is mapped to anonymous user.
--
-- -   @NoSquash@: No one is mapped to anonymous user.
--
-- -   @AllSquash@: Everyone is mapped to anonymous user.
createNFSFileShare_squash :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Text)
createNFSFileShare_squash = Lens.lens (\CreateNFSFileShare' {squash} -> squash) (\s@CreateNFSFileShare' {} a -> s {squash = a} :: CreateNFSFileShare)

-- | File share default values. Optional.
createNFSFileShare_nFSFileShareDefaults :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe NFSFileShareDefaults)
createNFSFileShare_nFSFileShareDefaults = Lens.lens (\CreateNFSFileShare' {nFSFileShareDefaults} -> nFSFileShareDefaults) (\s@CreateNFSFileShare' {} a -> s {nFSFileShareDefaults = a} :: CreateNFSFileShare)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
createNFSFileShare_fileShareName :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Text)
createNFSFileShare_fileShareName = Lens.lens (\CreateNFSFileShare' {fileShareName} -> fileShareName) (\s@CreateNFSFileShare' {} a -> s {fileShareName = a} :: CreateNFSFileShare)

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
createNFSFileShare_requesterPays :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Bool)
createNFSFileShare_requesterPays = Lens.lens (\CreateNFSFileShare' {requesterPays} -> requesterPays) (\s@CreateNFSFileShare' {} a -> s {requesterPays = a} :: CreateNFSFileShare)

-- | A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a S3 File Gateway puts objects into. The default
-- value is @private@.
createNFSFileShare_objectACL :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe ObjectACL)
createNFSFileShare_objectACL = Lens.lens (\CreateNFSFileShare' {objectACL} -> objectACL) (\s@CreateNFSFileShare' {} a -> s {objectACL = a} :: CreateNFSFileShare)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createNFSFileShare_kmsKey :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Text)
createNFSFileShare_kmsKey = Lens.lens (\CreateNFSFileShare' {kmsKey} -> kmsKey) (\s@CreateNFSFileShare' {} a -> s {kmsKey = a} :: CreateNFSFileShare)

-- | Specifies the DNS name for the VPC endpoint that the NFS file share uses
-- to connect to Amazon S3.
--
-- This parameter is required for NFS file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
createNFSFileShare_vPCEndpointDNSName :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Text)
createNFSFileShare_vPCEndpointDNSName = Lens.lens (\CreateNFSFileShare' {vPCEndpointDNSName} -> vPCEndpointDNSName) (\s@CreateNFSFileShare' {} a -> s {vPCEndpointDNSName = a} :: CreateNFSFileShare)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createNFSFileShare_kmsEncrypted :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Bool)
createNFSFileShare_kmsEncrypted = Lens.lens (\CreateNFSFileShare' {kmsEncrypted} -> kmsEncrypted) (\s@CreateNFSFileShare' {} a -> s {kmsEncrypted = a} :: CreateNFSFileShare)

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
createNFSFileShare_defaultStorageClass :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Text)
createNFSFileShare_defaultStorageClass = Lens.lens (\CreateNFSFileShare' {defaultStorageClass} -> defaultStorageClass) (\s@CreateNFSFileShare' {} a -> s {defaultStorageClass = a} :: CreateNFSFileShare)

-- | Specifies refresh cache information for the file share.
createNFSFileShare_cacheAttributes :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe CacheAttributes)
createNFSFileShare_cacheAttributes = Lens.lens (\CreateNFSFileShare' {cacheAttributes} -> cacheAttributes) (\s@CreateNFSFileShare' {} a -> s {cacheAttributes = a} :: CreateNFSFileShare)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
createNFSFileShare_readOnly :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Bool)
createNFSFileShare_readOnly = Lens.lens (\CreateNFSFileShare' {readOnly} -> readOnly) (\s@CreateNFSFileShare' {} a -> s {readOnly = a} :: CreateNFSFileShare)

-- | Specifies the Region of the S3 bucket where the NFS file share stores
-- files.
--
-- This parameter is required for NFS file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
createNFSFileShare_bucketRegion :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Text)
createNFSFileShare_bucketRegion = Lens.lens (\CreateNFSFileShare' {bucketRegion} -> bucketRegion) (\s@CreateNFSFileShare' {} a -> s {bucketRegion = a} :: CreateNFSFileShare)

-- | The Amazon Resource Name (ARN) of the storage used for audit logs.
createNFSFileShare_auditDestinationARN :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Text)
createNFSFileShare_auditDestinationARN = Lens.lens (\CreateNFSFileShare' {auditDestinationARN} -> auditDestinationARN) (\s@CreateNFSFileShare' {} a -> s {auditDestinationARN = a} :: CreateNFSFileShare)

-- | The list of clients that are allowed to access the S3 File Gateway. The
-- list must contain either valid IP addresses or valid CIDR blocks.
createNFSFileShare_clientList :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createNFSFileShare_clientList = Lens.lens (\CreateNFSFileShare' {clientList} -> clientList) (\s@CreateNFSFileShare' {} a -> s {clientList = a} :: CreateNFSFileShare) Prelude.. Lens.mapping Lens.coerced

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
createNFSFileShare_guessMIMETypeEnabled :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Bool)
createNFSFileShare_guessMIMETypeEnabled = Lens.lens (\CreateNFSFileShare' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@CreateNFSFileShare' {} a -> s {guessMIMETypeEnabled = a} :: CreateNFSFileShare)

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
createNFSFileShare_notificationPolicy :: Lens.Lens' CreateNFSFileShare (Prelude.Maybe Prelude.Text)
createNFSFileShare_notificationPolicy = Lens.lens (\CreateNFSFileShare' {notificationPolicy} -> notificationPolicy) (\s@CreateNFSFileShare' {} a -> s {notificationPolicy = a} :: CreateNFSFileShare)

-- | A unique string value that you supply that is used by S3 File Gateway to
-- ensure idempotent file share creation.
createNFSFileShare_clientToken :: Lens.Lens' CreateNFSFileShare Prelude.Text
createNFSFileShare_clientToken = Lens.lens (\CreateNFSFileShare' {clientToken} -> clientToken) (\s@CreateNFSFileShare' {} a -> s {clientToken = a} :: CreateNFSFileShare)

-- | The Amazon Resource Name (ARN) of the S3 File Gateway on which you want
-- to create a file share.
createNFSFileShare_gatewayARN :: Lens.Lens' CreateNFSFileShare Prelude.Text
createNFSFileShare_gatewayARN = Lens.lens (\CreateNFSFileShare' {gatewayARN} -> gatewayARN) (\s@CreateNFSFileShare' {} a -> s {gatewayARN = a} :: CreateNFSFileShare)

-- | The ARN of the Identity and Access Management (IAM) role that an S3 File
-- Gateway assumes when it accesses the underlying storage.
createNFSFileShare_role :: Lens.Lens' CreateNFSFileShare Prelude.Text
createNFSFileShare_role = Lens.lens (\CreateNFSFileShare' {role'} -> role') (\s@CreateNFSFileShare' {} a -> s {role' = a} :: CreateNFSFileShare)

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
createNFSFileShare_locationARN :: Lens.Lens' CreateNFSFileShare Prelude.Text
createNFSFileShare_locationARN = Lens.lens (\CreateNFSFileShare' {locationARN} -> locationARN) (\s@CreateNFSFileShare' {} a -> s {locationARN = a} :: CreateNFSFileShare)

instance Core.AWSRequest CreateNFSFileShare where
  type
    AWSResponse CreateNFSFileShare =
      CreateNFSFileShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNFSFileShareResponse'
            Prelude.<$> (x Core..?> "FileShareARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNFSFileShare where
  hashWithSalt _salt CreateNFSFileShare' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` squash
      `Prelude.hashWithSalt` nFSFileShareDefaults
      `Prelude.hashWithSalt` fileShareName
      `Prelude.hashWithSalt` requesterPays
      `Prelude.hashWithSalt` objectACL
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` vPCEndpointDNSName
      `Prelude.hashWithSalt` kmsEncrypted
      `Prelude.hashWithSalt` defaultStorageClass
      `Prelude.hashWithSalt` cacheAttributes
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` bucketRegion
      `Prelude.hashWithSalt` auditDestinationARN
      `Prelude.hashWithSalt` clientList
      `Prelude.hashWithSalt` guessMIMETypeEnabled
      `Prelude.hashWithSalt` notificationPolicy
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` locationARN

instance Prelude.NFData CreateNFSFileShare where
  rnf CreateNFSFileShare' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf squash
      `Prelude.seq` Prelude.rnf nFSFileShareDefaults
      `Prelude.seq` Prelude.rnf fileShareName
      `Prelude.seq` Prelude.rnf requesterPays
      `Prelude.seq` Prelude.rnf objectACL
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf vPCEndpointDNSName
      `Prelude.seq` Prelude.rnf kmsEncrypted
      `Prelude.seq` Prelude.rnf defaultStorageClass
      `Prelude.seq` Prelude.rnf cacheAttributes
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf bucketRegion
      `Prelude.seq` Prelude.rnf auditDestinationARN
      `Prelude.seq` Prelude.rnf clientList
      `Prelude.seq` Prelude.rnf guessMIMETypeEnabled
      `Prelude.seq` Prelude.rnf notificationPolicy
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf locationARN

instance Core.ToHeaders CreateNFSFileShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CreateNFSFileShare" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateNFSFileShare where
  toJSON CreateNFSFileShare' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Squash" Core..=) Prelude.<$> squash,
            ("NFSFileShareDefaults" Core..=)
              Prelude.<$> nFSFileShareDefaults,
            ("FileShareName" Core..=) Prelude.<$> fileShareName,
            ("RequesterPays" Core..=) Prelude.<$> requesterPays,
            ("ObjectACL" Core..=) Prelude.<$> objectACL,
            ("KMSKey" Core..=) Prelude.<$> kmsKey,
            ("VPCEndpointDNSName" Core..=)
              Prelude.<$> vPCEndpointDNSName,
            ("KMSEncrypted" Core..=) Prelude.<$> kmsEncrypted,
            ("DefaultStorageClass" Core..=)
              Prelude.<$> defaultStorageClass,
            ("CacheAttributes" Core..=)
              Prelude.<$> cacheAttributes,
            ("ReadOnly" Core..=) Prelude.<$> readOnly,
            ("BucketRegion" Core..=) Prelude.<$> bucketRegion,
            ("AuditDestinationARN" Core..=)
              Prelude.<$> auditDestinationARN,
            ("ClientList" Core..=) Prelude.<$> clientList,
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

instance Core.ToPath CreateNFSFileShare where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateNFSFileShare where
  toQuery = Prelude.const Prelude.mempty

-- | CreateNFSFileShareOutput
--
-- /See:/ 'newCreateNFSFileShareResponse' smart constructor.
data CreateNFSFileShareResponse = CreateNFSFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the newly created file share.
    fileShareARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateNFSFileShareResponse
newCreateNFSFileShareResponse pHttpStatus_ =
  CreateNFSFileShareResponse'
    { fileShareARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the newly created file share.
createNFSFileShareResponse_fileShareARN :: Lens.Lens' CreateNFSFileShareResponse (Prelude.Maybe Prelude.Text)
createNFSFileShareResponse_fileShareARN = Lens.lens (\CreateNFSFileShareResponse' {fileShareARN} -> fileShareARN) (\s@CreateNFSFileShareResponse' {} a -> s {fileShareARN = a} :: CreateNFSFileShareResponse)

-- | The response's http status code.
createNFSFileShareResponse_httpStatus :: Lens.Lens' CreateNFSFileShareResponse Prelude.Int
createNFSFileShareResponse_httpStatus = Lens.lens (\CreateNFSFileShareResponse' {httpStatus} -> httpStatus) (\s@CreateNFSFileShareResponse' {} a -> s {httpStatus = a} :: CreateNFSFileShareResponse)

instance Prelude.NFData CreateNFSFileShareResponse where
  rnf CreateNFSFileShareResponse' {..} =
    Prelude.rnf fileShareARN
      `Prelude.seq` Prelude.rnf httpStatus
