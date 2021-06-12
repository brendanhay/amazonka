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
-- Module      : Network.AWS.StorageGateway.Types.NFSFileShareInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.NFSFileShareInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.StorageGateway.Types.CacheAttributes
import Network.AWS.StorageGateway.Types.NFSFileShareDefaults
import Network.AWS.StorageGateway.Types.ObjectACL
import Network.AWS.StorageGateway.Types.Tag

-- | The Unix file permissions and ownership information assigned, by
-- default, to native S3 objects when file gateway discovers them in S3
-- buckets. This operation is only supported in file gateways.
--
-- /See:/ 'newNFSFileShareInfo' smart constructor.
data NFSFileShareInfo = NFSFileShareInfo'
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
    fileShareId :: Core.Maybe Core.Text,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
    -- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Core.Maybe Core.Bool,
    locationARN :: Core.Maybe Core.Text,
    squash :: Core.Maybe Core.Text,
    -- | The notification policy of the file share.
    notificationPolicy :: Core.Maybe Core.Text,
    kmsKey :: Core.Maybe Core.Text,
    fileShareStatus :: Core.Maybe Core.Text,
    role' :: Core.Maybe Core.Text,
    -- | A list of up to 50 tags assigned to the NFS file share, sorted
    -- alphabetically by key name. Each tag is a key-value pair. For a gateway
    -- with more than 10 tags assigned, you can view all tags using the
    -- @ListTagsForResource@ API operation.
    tags :: Core.Maybe [Tag],
    fileShareARN :: Core.Maybe Core.Text,
    -- | Refresh cache information.
    cacheAttributes :: Core.Maybe CacheAttributes,
    clientList :: Core.Maybe (Core.NonEmpty Core.Text),
    objectACL :: Core.Maybe ObjectACL,
    nFSFileShareDefaults :: Core.Maybe NFSFileShareDefaults,
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
    requesterPays :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NFSFileShareInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultStorageClass', 'nFSFileShareInfo_defaultStorageClass' - The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
--
-- 'fileShareName', 'nFSFileShareInfo_fileShareName' - The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
--
-- 'guessMIMETypeEnabled', 'nFSFileShareInfo_guessMIMETypeEnabled' - A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
--
-- 'readOnly', 'nFSFileShareInfo_readOnly' - A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
--
-- 'fileShareId', 'nFSFileShareInfo_fileShareId' - Undocumented member.
--
-- 'kmsEncrypted', 'nFSFileShareInfo_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'locationARN', 'nFSFileShareInfo_locationARN' - Undocumented member.
--
-- 'squash', 'nFSFileShareInfo_squash' - Undocumented member.
--
-- 'notificationPolicy', 'nFSFileShareInfo_notificationPolicy' - The notification policy of the file share.
--
-- 'kmsKey', 'nFSFileShareInfo_kmsKey' - Undocumented member.
--
-- 'fileShareStatus', 'nFSFileShareInfo_fileShareStatus' - Undocumented member.
--
-- 'role'', 'nFSFileShareInfo_role' - Undocumented member.
--
-- 'tags', 'nFSFileShareInfo_tags' - A list of up to 50 tags assigned to the NFS file share, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
--
-- 'fileShareARN', 'nFSFileShareInfo_fileShareARN' - Undocumented member.
--
-- 'cacheAttributes', 'nFSFileShareInfo_cacheAttributes' - Refresh cache information.
--
-- 'clientList', 'nFSFileShareInfo_clientList' - Undocumented member.
--
-- 'objectACL', 'nFSFileShareInfo_objectACL' - Undocumented member.
--
-- 'nFSFileShareDefaults', 'nFSFileShareInfo_nFSFileShareDefaults' - Undocumented member.
--
-- 'path', 'nFSFileShareInfo_path' - Undocumented member.
--
-- 'gatewayARN', 'nFSFileShareInfo_gatewayARN' - Undocumented member.
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
newNFSFileShareInfo ::
  NFSFileShareInfo
newNFSFileShareInfo =
  NFSFileShareInfo'
    { defaultStorageClass =
        Core.Nothing,
      fileShareName = Core.Nothing,
      guessMIMETypeEnabled = Core.Nothing,
      readOnly = Core.Nothing,
      fileShareId = Core.Nothing,
      kmsEncrypted = Core.Nothing,
      locationARN = Core.Nothing,
      squash = Core.Nothing,
      notificationPolicy = Core.Nothing,
      kmsKey = Core.Nothing,
      fileShareStatus = Core.Nothing,
      role' = Core.Nothing,
      tags = Core.Nothing,
      fileShareARN = Core.Nothing,
      cacheAttributes = Core.Nothing,
      clientList = Core.Nothing,
      objectACL = Core.Nothing,
      nFSFileShareDefaults = Core.Nothing,
      path = Core.Nothing,
      gatewayARN = Core.Nothing,
      requesterPays = Core.Nothing
    }

-- | The default storage class for objects put into an Amazon S3 bucket by
-- the file gateway. The default value is @S3_INTELLIGENT_TIERING@.
-- Optional.
--
-- Valid Values: @S3_STANDARD@ | @S3_INTELLIGENT_TIERING@ |
-- @S3_STANDARD_IA@ | @S3_ONEZONE_IA@
nFSFileShareInfo_defaultStorageClass :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_defaultStorageClass = Lens.lens (\NFSFileShareInfo' {defaultStorageClass} -> defaultStorageClass) (\s@NFSFileShareInfo' {} a -> s {defaultStorageClass = a} :: NFSFileShareInfo)

-- | The name of the file share. Optional.
--
-- @FileShareName@ must be set if an S3 prefix name is set in
-- @LocationARN@.
nFSFileShareInfo_fileShareName :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_fileShareName = Lens.lens (\NFSFileShareInfo' {fileShareName} -> fileShareName) (\s@NFSFileShareInfo' {} a -> s {fileShareName = a} :: NFSFileShareInfo)

-- | A value that enables guessing of the MIME type for uploaded objects
-- based on file extensions. Set this value to @true@ to enable MIME type
-- guessing, otherwise set to @false@. The default value is @true@.
--
-- Valid Values: @true@ | @false@
nFSFileShareInfo_guessMIMETypeEnabled :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Bool)
nFSFileShareInfo_guessMIMETypeEnabled = Lens.lens (\NFSFileShareInfo' {guessMIMETypeEnabled} -> guessMIMETypeEnabled) (\s@NFSFileShareInfo' {} a -> s {guessMIMETypeEnabled = a} :: NFSFileShareInfo)

-- | A value that sets the write status of a file share. Set this value to
-- @true@ to set the write status to read-only, otherwise set to @false@.
--
-- Valid Values: @true@ | @false@
nFSFileShareInfo_readOnly :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Bool)
nFSFileShareInfo_readOnly = Lens.lens (\NFSFileShareInfo' {readOnly} -> readOnly) (\s@NFSFileShareInfo' {} a -> s {readOnly = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_fileShareId :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_fileShareId = Lens.lens (\NFSFileShareInfo' {fileShareId} -> fileShareId) (\s@NFSFileShareInfo' {} a -> s {fileShareId = a} :: NFSFileShareInfo)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
nFSFileShareInfo_kmsEncrypted :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Bool)
nFSFileShareInfo_kmsEncrypted = Lens.lens (\NFSFileShareInfo' {kmsEncrypted} -> kmsEncrypted) (\s@NFSFileShareInfo' {} a -> s {kmsEncrypted = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_locationARN :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_locationARN = Lens.lens (\NFSFileShareInfo' {locationARN} -> locationARN) (\s@NFSFileShareInfo' {} a -> s {locationARN = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_squash :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_squash = Lens.lens (\NFSFileShareInfo' {squash} -> squash) (\s@NFSFileShareInfo' {} a -> s {squash = a} :: NFSFileShareInfo)

-- | The notification policy of the file share.
nFSFileShareInfo_notificationPolicy :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_notificationPolicy = Lens.lens (\NFSFileShareInfo' {notificationPolicy} -> notificationPolicy) (\s@NFSFileShareInfo' {} a -> s {notificationPolicy = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_kmsKey :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_kmsKey = Lens.lens (\NFSFileShareInfo' {kmsKey} -> kmsKey) (\s@NFSFileShareInfo' {} a -> s {kmsKey = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_fileShareStatus :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_fileShareStatus = Lens.lens (\NFSFileShareInfo' {fileShareStatus} -> fileShareStatus) (\s@NFSFileShareInfo' {} a -> s {fileShareStatus = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_role :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_role = Lens.lens (\NFSFileShareInfo' {role'} -> role') (\s@NFSFileShareInfo' {} a -> s {role' = a} :: NFSFileShareInfo)

-- | A list of up to 50 tags assigned to the NFS file share, sorted
-- alphabetically by key name. Each tag is a key-value pair. For a gateway
-- with more than 10 tags assigned, you can view all tags using the
-- @ListTagsForResource@ API operation.
nFSFileShareInfo_tags :: Lens.Lens' NFSFileShareInfo (Core.Maybe [Tag])
nFSFileShareInfo_tags = Lens.lens (\NFSFileShareInfo' {tags} -> tags) (\s@NFSFileShareInfo' {} a -> s {tags = a} :: NFSFileShareInfo) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
nFSFileShareInfo_fileShareARN :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_fileShareARN = Lens.lens (\NFSFileShareInfo' {fileShareARN} -> fileShareARN) (\s@NFSFileShareInfo' {} a -> s {fileShareARN = a} :: NFSFileShareInfo)

-- | Refresh cache information.
nFSFileShareInfo_cacheAttributes :: Lens.Lens' NFSFileShareInfo (Core.Maybe CacheAttributes)
nFSFileShareInfo_cacheAttributes = Lens.lens (\NFSFileShareInfo' {cacheAttributes} -> cacheAttributes) (\s@NFSFileShareInfo' {} a -> s {cacheAttributes = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_clientList :: Lens.Lens' NFSFileShareInfo (Core.Maybe (Core.NonEmpty Core.Text))
nFSFileShareInfo_clientList = Lens.lens (\NFSFileShareInfo' {clientList} -> clientList) (\s@NFSFileShareInfo' {} a -> s {clientList = a} :: NFSFileShareInfo) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
nFSFileShareInfo_objectACL :: Lens.Lens' NFSFileShareInfo (Core.Maybe ObjectACL)
nFSFileShareInfo_objectACL = Lens.lens (\NFSFileShareInfo' {objectACL} -> objectACL) (\s@NFSFileShareInfo' {} a -> s {objectACL = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_nFSFileShareDefaults :: Lens.Lens' NFSFileShareInfo (Core.Maybe NFSFileShareDefaults)
nFSFileShareInfo_nFSFileShareDefaults = Lens.lens (\NFSFileShareInfo' {nFSFileShareDefaults} -> nFSFileShareDefaults) (\s@NFSFileShareInfo' {} a -> s {nFSFileShareDefaults = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_path :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_path = Lens.lens (\NFSFileShareInfo' {path} -> path) (\s@NFSFileShareInfo' {} a -> s {path = a} :: NFSFileShareInfo)

-- | Undocumented member.
nFSFileShareInfo_gatewayARN :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Text)
nFSFileShareInfo_gatewayARN = Lens.lens (\NFSFileShareInfo' {gatewayARN} -> gatewayARN) (\s@NFSFileShareInfo' {} a -> s {gatewayARN = a} :: NFSFileShareInfo)

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
nFSFileShareInfo_requesterPays :: Lens.Lens' NFSFileShareInfo (Core.Maybe Core.Bool)
nFSFileShareInfo_requesterPays = Lens.lens (\NFSFileShareInfo' {requesterPays} -> requesterPays) (\s@NFSFileShareInfo' {} a -> s {requesterPays = a} :: NFSFileShareInfo)

instance Core.FromJSON NFSFileShareInfo where
  parseJSON =
    Core.withObject
      "NFSFileShareInfo"
      ( \x ->
          NFSFileShareInfo'
            Core.<$> (x Core..:? "DefaultStorageClass")
            Core.<*> (x Core..:? "FileShareName")
            Core.<*> (x Core..:? "GuessMIMETypeEnabled")
            Core.<*> (x Core..:? "ReadOnly")
            Core.<*> (x Core..:? "FileShareId")
            Core.<*> (x Core..:? "KMSEncrypted")
            Core.<*> (x Core..:? "LocationARN")
            Core.<*> (x Core..:? "Squash")
            Core.<*> (x Core..:? "NotificationPolicy")
            Core.<*> (x Core..:? "KMSKey")
            Core.<*> (x Core..:? "FileShareStatus")
            Core.<*> (x Core..:? "Role")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "FileShareARN")
            Core.<*> (x Core..:? "CacheAttributes")
            Core.<*> (x Core..:? "ClientList")
            Core.<*> (x Core..:? "ObjectACL")
            Core.<*> (x Core..:? "NFSFileShareDefaults")
            Core.<*> (x Core..:? "Path")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "RequesterPays")
      )

instance Core.Hashable NFSFileShareInfo

instance Core.NFData NFSFileShareInfo
