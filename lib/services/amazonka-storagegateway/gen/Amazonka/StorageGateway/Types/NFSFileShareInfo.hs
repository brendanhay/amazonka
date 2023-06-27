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
-- Module      : Amazonka.StorageGateway.Types.NFSFileShareInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.NFSFileShareInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.CacheAttributes
import Amazonka.StorageGateway.Types.NFSFileShareDefaults
import Amazonka.StorageGateway.Types.ObjectACL
import Amazonka.StorageGateway.Types.Tag

-- | The Unix file permissions and ownership information assigned, by
-- default, to native S3 objects when an S3 File Gateway discovers them in
-- S3 buckets. This operation is only supported in S3 File Gateways.
--
-- /See:/ 'newNFSFileShareInfo' smart constructor.
data NFSFileShareInfo = NFSFileShareInfo'
  { -- | The Amazon Resource Name (ARN) of the storage used for audit logs.
    auditDestinationARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Region of the S3 bucket where the NFS file share stores
    -- files.
    --
    -- This parameter is required for NFS file shares that connect to Amazon S3
    -- through a VPC endpoint, a VPC access point, or an access point alias
    -- that points to a VPC access point.
    bucketRegion :: Prelude.Maybe Prelude.Text,
    -- | Refresh cache information for the file share.
    cacheAttributes :: Prelude.Maybe CacheAttributes,
    clientList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The default storage class for objects put into an Amazon S3 bucket by
    -- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
    --
    -- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
    -- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
    defaultStorageClass :: Prelude.Maybe Prelude.Text,
    fileShareARN :: Prelude.Maybe Prelude.Text,
    fileShareId :: Prelude.Maybe Prelude.Text,
    -- | The name of the file share. Optional.
    --
    -- @FileShareName@ must be set if an S3 prefix name is set in
    -- @LocationARN@, or if an access point or access point alias is used.
    fileShareName :: Prelude.Maybe Prelude.Text,
    fileShareStatus :: Prelude.Maybe Prelude.Text,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | A value that enables guessing of the MIME type for uploaded objects
    -- based on file extensions. Set this value to @true@ to enable MIME type
    -- guessing, otherwise set to @false@. The default value is @true@.
    --
    -- Valid Values: @true@ | @false@
    guessMIMETypeEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
    -- key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Prelude.Maybe Prelude.Bool,
    kmsKey :: Prelude.Maybe Prelude.Text,
    locationARN :: Prelude.Maybe Prelude.Text,
    nFSFileShareDefaults :: Prelude.Maybe NFSFileShareDefaults,
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
    objectACL :: Prelude.Maybe ObjectACL,
    path :: Prelude.Maybe Prelude.Text,
    -- | A value that sets the write status of a file share. Set this value to
    -- @true@ to set the write status to read-only, otherwise set to @false@.
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
    role' :: Prelude.Maybe Prelude.Text,
    squash :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 50 tags assigned to the NFS file share, sorted
    -- alphabetically by key name. Each tag is a key-value pair. For a gateway
    -- with more than 10 tags assigned, you can view all tags using the
    -- @ListTagsForResource@ API operation.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the DNS name for the VPC endpoint that the NFS file share uses
    -- to connect to Amazon S3.
    --
    -- This parameter is required for NFS file shares that connect to Amazon S3
    -- through a VPC endpoint, a VPC access point, or an access point alias
    -- that points to a VPC access point.
    vPCEndpointDNSName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NFSFileShareInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditDestinationARN', 'nFSFileShareInfo_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for audit logs.
--
-- 'bucketRegion', 'nFSFileShareInfo_bucketRegion' - Specifies the Region of the S3 bucket where the NFS file share stores
-- files.
--
-- This parameter is required for NFS file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
--
-- 'cacheAttributes', 'nFSFileShareInfo_cacheAttributes' - Refresh cache information for the file share.
--
-- 'clientList', 'nFSFileShareInfo_clientList' - Undocumented member.
--
-- 'defaultStorageClass', 'nFSFileShareInfo_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'fileShareARN', 'nFSFileShareInfo_fileShareARN' - Undocumented member.
--
-- 'fileShareId', 'nFSFileShareInfo_fileShareId' - Undocumented member.
--
-- 'fileShareName', 'nFSFileShareInfo_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
--
-- 'fileShareStatus', 'nFSFileShareInfo_fileShareStatus' - Undocumented member.
--
-- 'gatewayARN', 'nFSFileShareInfo_gatewayARN' - Undocumented member.
--
-- 'guessMIMETypeEnabled', 'nFSFileShareInfo_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsEncrypted', 'nFSFileShareInfo_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsKey', 'nFSFileShareInfo_kmsKey' - Undocumented member.
--
-- 'locationARN', 'nFSFileShareInfo_locationARN' - Undocumented member.
--
-- 'nFSFileShareDefaults', 'nFSFileShareInfo_nFSFileShareDefaults' - Undocumented member.
--
-- 'notificationPolicy', 'nFSFileShareInfo_notificationPolicy' - The notification policy of the file share. @SettlingTimeInSeconds@
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
-- 'objectACL', 'nFSFileShareInfo_objectACL' - Undocumented member.
--
-- 'path', 'nFSFileShareInfo_path' - Undocumented member.
--
-- 'readOnly', 'nFSFileShareInfo_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'requesterPays', 'nFSFileShareInfo_requesterPays' - A value that sets who pays the cost of the request and the cost
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
-- 'role'', 'nFSFileShareInfo_role' - Undocumented member.
--
-- 'squash', 'nFSFileShareInfo_squash' - Undocumented member.
--
-- 'tags', 'nFSFileShareInfo_tags' - A list of up to 50 tags assigned to the NFS file share, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
--
-- 'vPCEndpointDNSName', 'nFSFileShareInfo_vPCEndpointDNSName' - Specifies the DNS name for the VPC endpoint that the NFS file share uses
-- to connect to Amazon S3.
--
-- This parameter is required for NFS file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
newNFSFileShareInfo ::
  NFSFileShareInfo
newNFSFileShareInfo =
  NFSFileShareInfo'
    { auditDestinationARN =
        Prelude.Nothing,
      bucketRegion = Prelude.Nothing,
      cacheAttributes = Prelude.Nothing,
      clientList = Prelude.Nothing,
      defaultStorageClass = Prelude.Nothing,
      fileShareARN = Prelude.Nothing,
      fileShareId = Prelude.Nothing,
      fileShareName = Prelude.Nothing,
      fileShareStatus = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      guessMIMETypeEnabled = Prelude.Nothing,
      kmsEncrypted = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      locationARN = Prelude.Nothing,
      nFSFileShareDefaults = Prelude.Nothing,
      notificationPolicy = Prelude.Nothing,
      objectACL = Prelude.Nothing,
      path = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      requesterPays = Prelude.Nothing,
      role' = Prelude.Nothing,
      squash = Prelude.Nothing,
      tags = Prelude.Nothing,
      vPCEndpointDNSName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the storage used for audit logs.
nFSFileShareInfo_auditDestinationARN :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_auditDestinationARN = Lens.lens (\NFSFileShareInfo' {auditDestinationARN} -> auditDestinationARN) (\s@NFSFileShareInfo' {} a -> s {auditDestinationARN = a} :: NFSFileShareInfo)

-- | Specifies the Region of the S3 bucket where the NFS file share stores
-- files.
--
-- This parameter is required for NFS file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
nFSFileShareInfo_bucketRegion :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_bucketRegion = Lens.lens (\NFSFileShareInfo' {bucketRegion} -> bucketRegion) (\s@NFSFileShareInfo' {} a -> s {bucketRegion = a} :: NFSFileShareInfo)

-- | Refresh cache information for the file share.
nFSFileShareInfo_cacheAttributes :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe CacheAttributes)
nFSFileShareInfo_cacheAttributes = Lens.lens (\NFSFileShareInfo' {cacheAttributes} -> cacheAttributes) (\s@NFSFileShareInfo' {} a -> s {cacheAttributes = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_clientList :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
nFSFileShareInfo_clientList = Lens.lens (\NFSFileShareInfo' {clientList} -> clientList) (\s@NFSFileShareInfo' {} a -> s {clientList = a} :: NFSFileShareInfo) Prelude.. Lens.mapping Lens.coerced

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the S3 File Gateway. The default value is @S3_STANDARD@. Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
nFSFileShareInfo_defaultStorageClass :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_defaultStorageClass = Lens.lens (\NFSFileShareInfo' {defaultStorageClass} -> defaultStorageClass) (\s@NFSFileShareInfo' {} a -> s {defaultStorageClass = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_fileShareARN :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_fileShareARN = Lens.lens (\NFSFileShareInfo' {fileShareARN} -> fileShareARN) (\s@NFSFileShareInfo' {} a -> s {fileShareARN = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_fileShareId :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_fileShareId = Lens.lens (\NFSFileShareInfo' {fileShareId} -> fileShareId) (\s@NFSFileShareInfo' {} a -> s {fileShareId = a} :: NFSFileShareInfo)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@, or if an access point or access point alias is used.
nFSFileShareInfo_fileShareName :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_fileShareName = Lens.lens (\NFSFileShareInfo' {fileShareName} -> fileShareName) (\s@NFSFileShareInfo' {} a -> s {fileShareName = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_fileShareStatus :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_fileShareStatus = Lens.lens (\NFSFileShareInfo' {fileShareStatus} -> fileShareStatus) (\s@NFSFileShareInfo' {} a -> s {fileShareStatus = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_gatewayARN :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_gatewayARN = Lens.lens (\NFSFileShareInfo' {gatewayARN} -> gatewayARN) (\s@NFSFileShareInfo' {} a -> s {gatewayARN = a} :: NFSFileShareInfo)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
nFSFileShareInfo_guessMIMETypeEnabled :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Bool)
nFSFileShareInfo_guessMIMETypeEnabled = Lens.lens (\NFSFileShareInfo' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@NFSFileShareInfo' {} a -> s {guessMIMETypeEnabled = a} :: NFSFileShareInfo)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
nFSFileShareInfo_kmsEncrypted :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Bool)
nFSFileShareInfo_kmsEncrypted = Lens.lens (\NFSFileShareInfo' {kmsEncrypted} -> kmsEncrypted) (\s@NFSFileShareInfo' {} a -> s {kmsEncrypted = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_kmsKey :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_kmsKey = Lens.lens (\NFSFileShareInfo' {kmsKey} -> kmsKey) (\s@NFSFileShareInfo' {} a -> s {kmsKey = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_locationARN :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_locationARN = Lens.lens (\NFSFileShareInfo' {locationARN} -> locationARN) (\s@NFSFileShareInfo' {} a -> s {locationARN = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_nFSFileShareDefaults :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe NFSFileShareDefaults)
nFSFileShareInfo_nFSFileShareDefaults = Lens.lens (\NFSFileShareInfo' {nFSFileShareDefaults} -> nFSFileShareDefaults) (\s@NFSFileShareInfo' {} a -> s {nFSFileShareDefaults = a} :: NFSFileShareInfo)

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
nFSFileShareInfo_notificationPolicy :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_notificationPolicy = Lens.lens (\NFSFileShareInfo' {notificationPolicy} -> notificationPolicy) (\s@NFSFileShareInfo' {} a -> s {notificationPolicy = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_objectACL :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe ObjectACL)
nFSFileShareInfo_objectACL = Lens.lens (\NFSFileShareInfo' {objectACL} -> objectACL) (\s@NFSFileShareInfo' {} a -> s {objectACL = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_path :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_path = Lens.lens (\NFSFileShareInfo' {path} -> path) (\s@NFSFileShareInfo' {} a -> s {path = a} :: NFSFileShareInfo)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
nFSFileShareInfo_readOnly :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Bool)
nFSFileShareInfo_readOnly = Lens.lens (\NFSFileShareInfo' {readOnly} -> readOnly) (\s@NFSFileShareInfo' {} a -> s {readOnly = a} :: NFSFileShareInfo)

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
nFSFileShareInfo_requesterPays :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Bool)
nFSFileShareInfo_requesterPays = Lens.lens (\NFSFileShareInfo' {requesterPays} -> requesterPays) (\s@NFSFileShareInfo' {} a -> s {requesterPays = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_role :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_role = Lens.lens (\NFSFileShareInfo' {role'} -> role') (\s@NFSFileShareInfo' {} a -> s {role' = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_squash :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_squash = Lens.lens (\NFSFileShareInfo' {squash} -> squash) (\s@NFSFileShareInfo' {} a -> s {squash = a} :: NFSFileShareInfo)

-- | A list of up to 50 tags assigned to the NFS file share, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
nFSFileShareInfo_tags :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe [Tag])
nFSFileShareInfo_tags = Lens.lens (\NFSFileShareInfo' {tags} -> tags) (\s@NFSFileShareInfo' {} a -> s {tags = a} :: NFSFileShareInfo) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the DNS name for the VPC endpoint that the NFS file share uses
-- to connect to Amazon S3.
--
-- This parameter is required for NFS file shares that connect to Amazon S3
-- through a VPC endpoint, a VPC access point, or an access point alias
-- that points to a VPC access point.
nFSFileShareInfo_vPCEndpointDNSName :: Lens.Lens' NFSFileShareInfo (Prelude.Maybe Prelude.Text)
nFSFileShareInfo_vPCEndpointDNSName = Lens.lens (\NFSFileShareInfo' {vPCEndpointDNSName} -> vPCEndpointDNSName) (\s@NFSFileShareInfo' {} a -> s {vPCEndpointDNSName = a} :: NFSFileShareInfo)

instance Data.FromJSON NFSFileShareInfo where
  parseJSON =
    Data.withObject
      "NFSFileShareInfo"
      ( \x ->
          NFSFileShareInfo'
            Prelude.<$> (x Data..:? "AuditDestinationARN")
            Prelude.<*> (x Data..:? "BucketRegion")
            Prelude.<*> (x Data..:? "CacheAttributes")
            Prelude.<*> (x Data..:? "ClientList")
            Prelude.<*> (x Data..:? "DefaultStorageClass")
            Prelude.<*> (x Data..:? "FileShareARN")
            Prelude.<*> (x Data..:? "FileShareId")
            Prelude.<*> (x Data..:? "FileShareName")
            Prelude.<*> (x Data..:? "FileShareStatus")
            Prelude.<*> (x Data..:? "GatewayARN")
            Prelude.<*> (x Data..:? "GuessMIMETypeEnabled")
            Prelude.<*> (x Data..:? "KMSEncrypted")
            Prelude.<*> (x Data..:? "KMSKey")
            Prelude.<*> (x Data..:? "LocationARN")
            Prelude.<*> (x Data..:? "NFSFileShareDefaults")
            Prelude.<*> (x Data..:? "NotificationPolicy")
            Prelude.<*> (x Data..:? "ObjectACL")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "ReadOnly")
            Prelude.<*> (x Data..:? "RequesterPays")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "Squash")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VPCEndpointDNSName")
      )

instance Prelude.Hashable NFSFileShareInfo where
  hashWithSalt _salt NFSFileShareInfo' {..} =
    _salt
      `Prelude.hashWithSalt` auditDestinationARN
      `Prelude.hashWithSalt` bucketRegion
      `Prelude.hashWithSalt` cacheAttributes
      `Prelude.hashWithSalt` clientList
      `Prelude.hashWithSalt` defaultStorageClass
      `Prelude.hashWithSalt` fileShareARN
      `Prelude.hashWithSalt` fileShareId
      `Prelude.hashWithSalt` fileShareName
      `Prelude.hashWithSalt` fileShareStatus
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` guessMIMETypeEnabled
      `Prelude.hashWithSalt` kmsEncrypted
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` locationARN
      `Prelude.hashWithSalt` nFSFileShareDefaults
      `Prelude.hashWithSalt` notificationPolicy
      `Prelude.hashWithSalt` objectACL
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` requesterPays
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` squash
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vPCEndpointDNSName

instance Prelude.NFData NFSFileShareInfo where
  rnf NFSFileShareInfo' {..} =
    Prelude.rnf auditDestinationARN
      `Prelude.seq` Prelude.rnf bucketRegion
      `Prelude.seq` Prelude.rnf cacheAttributes
      `Prelude.seq` Prelude.rnf clientList
      `Prelude.seq` Prelude.rnf defaultStorageClass
      `Prelude.seq` Prelude.rnf fileShareARN
      `Prelude.seq` Prelude.rnf fileShareId
      `Prelude.seq` Prelude.rnf fileShareName
      `Prelude.seq` Prelude.rnf fileShareStatus
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf guessMIMETypeEnabled
      `Prelude.seq` Prelude.rnf kmsEncrypted
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf locationARN
      `Prelude.seq` Prelude.rnf nFSFileShareDefaults
      `Prelude.seq` Prelude.rnf notificationPolicy
      `Prelude.seq` Prelude.rnf objectACL
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf requesterPays
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf squash
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf
        vPCEndpointDNSName
