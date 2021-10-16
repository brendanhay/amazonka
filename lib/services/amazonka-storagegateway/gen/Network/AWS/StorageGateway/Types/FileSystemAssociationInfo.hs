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
-- Module      : Network.AWS.StorageGateway.Types.FileSystemAssociationInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.FileSystemAssociationInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StorageGateway.Types.CacheAttributes
import Network.AWS.StorageGateway.Types.EndpointNetworkConfiguration
import Network.AWS.StorageGateway.Types.Tag

-- | Describes the object returned by @DescribeFileSystemAssociations@ that
-- describes a created file system association.
--
-- /See:/ 'newFileSystemAssociationInfo' smart constructor.
data FileSystemAssociationInfo = FileSystemAssociationInfo'
  { -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the file system association.
    fileSystemAssociationARN :: Prelude.Maybe Prelude.Text,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    cacheAttributes :: Prelude.Maybe CacheAttributes,
    -- | Specifies network configuration information for the gateway associated
    -- with the Amazon FSx file system.
    --
    -- If multiple file systems are associated with this gateway, this
    -- parameter\'s @IpAddresses@ field is required.
    endpointNetworkConfiguration :: Prelude.Maybe EndpointNetworkConfiguration,
    -- | The ARN of the backend Amazon FSx file system used for storing file
    -- data. For information, see
    -- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_FileSystem.html FileSystem>
    -- in the /Amazon FSx API Reference/.
    locationARN :: Prelude.Maybe Prelude.Text,
    -- | The status of the file system association. Valid Values: @AVAILABLE@ |
    -- @CREATING@ | @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
    fileSystemAssociationStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 50 tags assigned to the SMB file share, sorted
    -- alphabetically by key name. Each tag is a key-value pair.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemAssociationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditDestinationARN', 'fileSystemAssociationInfo_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- 'fileSystemAssociationARN', 'fileSystemAssociationInfo_fileSystemAssociationARN' - The Amazon Resource Name (ARN) of the file system association.
--
-- 'gatewayARN', 'fileSystemAssociationInfo_gatewayARN' - Undocumented member.
--
-- 'cacheAttributes', 'fileSystemAssociationInfo_cacheAttributes' - Undocumented member.
--
-- 'endpointNetworkConfiguration', 'fileSystemAssociationInfo_endpointNetworkConfiguration' - Specifies network configuration information for the gateway associated
-- with the Amazon FSx file system.
--
-- If multiple file systems are associated with this gateway, this
-- parameter\'s @IpAddresses@ field is required.
--
-- 'locationARN', 'fileSystemAssociationInfo_locationARN' - The ARN of the backend Amazon FSx file system used for storing file
-- data. For information, see
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_FileSystem.html FileSystem>
-- in the /Amazon FSx API Reference/.
--
-- 'fileSystemAssociationStatus', 'fileSystemAssociationInfo_fileSystemAssociationStatus' - The status of the file system association. Valid Values: @AVAILABLE@ |
-- @CREATING@ | @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
--
-- 'tags', 'fileSystemAssociationInfo_tags' - A list of up to 50 tags assigned to the SMB file share, sorted
-- alphabetically by key name. Each tag is a key-value pair.
newFileSystemAssociationInfo ::
  FileSystemAssociationInfo
newFileSystemAssociationInfo =
  FileSystemAssociationInfo'
    { auditDestinationARN =
        Prelude.Nothing,
      fileSystemAssociationARN = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      cacheAttributes = Prelude.Nothing,
      endpointNetworkConfiguration = Prelude.Nothing,
      locationARN = Prelude.Nothing,
      fileSystemAssociationStatus = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
fileSystemAssociationInfo_auditDestinationARN :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_auditDestinationARN = Lens.lens (\FileSystemAssociationInfo' {auditDestinationARN} -> auditDestinationARN) (\s@FileSystemAssociationInfo' {} a -> s {auditDestinationARN = a} :: FileSystemAssociationInfo)

-- | The Amazon Resource Name (ARN) of the file system association.
fileSystemAssociationInfo_fileSystemAssociationARN :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_fileSystemAssociationARN = Lens.lens (\FileSystemAssociationInfo' {fileSystemAssociationARN} -> fileSystemAssociationARN) (\s@FileSystemAssociationInfo' {} a -> s {fileSystemAssociationARN = a} :: FileSystemAssociationInfo)

-- | Undocumented member.
fileSystemAssociationInfo_gatewayARN :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_gatewayARN = Lens.lens (\FileSystemAssociationInfo' {gatewayARN} -> gatewayARN) (\s@FileSystemAssociationInfo' {} a -> s {gatewayARN = a} :: FileSystemAssociationInfo)

-- | Undocumented member.
fileSystemAssociationInfo_cacheAttributes :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe CacheAttributes)
fileSystemAssociationInfo_cacheAttributes = Lens.lens (\FileSystemAssociationInfo' {cacheAttributes} -> cacheAttributes) (\s@FileSystemAssociationInfo' {} a -> s {cacheAttributes = a} :: FileSystemAssociationInfo)

-- | Specifies network configuration information for the gateway associated
-- with the Amazon FSx file system.
--
-- If multiple file systems are associated with this gateway, this
-- parameter\'s @IpAddresses@ field is required.
fileSystemAssociationInfo_endpointNetworkConfiguration :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe EndpointNetworkConfiguration)
fileSystemAssociationInfo_endpointNetworkConfiguration = Lens.lens (\FileSystemAssociationInfo' {endpointNetworkConfiguration} -> endpointNetworkConfiguration) (\s@FileSystemAssociationInfo' {} a -> s {endpointNetworkConfiguration = a} :: FileSystemAssociationInfo)

-- | The ARN of the backend Amazon FSx file system used for storing file
-- data. For information, see
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_FileSystem.html FileSystem>
-- in the /Amazon FSx API Reference/.
fileSystemAssociationInfo_locationARN :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_locationARN = Lens.lens (\FileSystemAssociationInfo' {locationARN} -> locationARN) (\s@FileSystemAssociationInfo' {} a -> s {locationARN = a} :: FileSystemAssociationInfo)

-- | The status of the file system association. Valid Values: @AVAILABLE@ |
-- @CREATING@ | @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
fileSystemAssociationInfo_fileSystemAssociationStatus :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_fileSystemAssociationStatus = Lens.lens (\FileSystemAssociationInfo' {fileSystemAssociationStatus} -> fileSystemAssociationStatus) (\s@FileSystemAssociationInfo' {} a -> s {fileSystemAssociationStatus = a} :: FileSystemAssociationInfo)

-- | A list of up to 50 tags assigned to the SMB file share, sorted
-- alphabetically by key name. Each tag is a key-value pair.
fileSystemAssociationInfo_tags :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe [Tag])
fileSystemAssociationInfo_tags = Lens.lens (\FileSystemAssociationInfo' {tags} -> tags) (\s@FileSystemAssociationInfo' {} a -> s {tags = a} :: FileSystemAssociationInfo) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON FileSystemAssociationInfo where
  parseJSON =
    Core.withObject
      "FileSystemAssociationInfo"
      ( \x ->
          FileSystemAssociationInfo'
            Prelude.<$> (x Core..:? "AuditDestinationARN")
            Prelude.<*> (x Core..:? "FileSystemAssociationARN")
            Prelude.<*> (x Core..:? "GatewayARN")
            Prelude.<*> (x Core..:? "CacheAttributes")
            Prelude.<*> (x Core..:? "EndpointNetworkConfiguration")
            Prelude.<*> (x Core..:? "LocationARN")
            Prelude.<*> (x Core..:? "FileSystemAssociationStatus")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable FileSystemAssociationInfo

instance Prelude.NFData FileSystemAssociationInfo
