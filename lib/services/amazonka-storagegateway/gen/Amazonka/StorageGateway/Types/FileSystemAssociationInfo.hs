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
-- Module      : Amazonka.StorageGateway.Types.FileSystemAssociationInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.FileSystemAssociationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.CacheAttributes
import Amazonka.StorageGateway.Types.EndpointNetworkConfiguration
import Amazonka.StorageGateway.Types.FileSystemAssociationStatusDetail
import Amazonka.StorageGateway.Types.Tag

-- | Describes the object returned by @DescribeFileSystemAssociations@ that
-- describes a created file system association.
--
-- /See:/ 'newFileSystemAssociationInfo' smart constructor.
data FileSystemAssociationInfo = FileSystemAssociationInfo'
  { -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Prelude.Maybe Prelude.Text,
    cacheAttributes :: Prelude.Maybe CacheAttributes,
    -- | Specifies network configuration information for the gateway associated
    -- with the Amazon FSx file system.
    --
    -- If multiple file systems are associated with this gateway, this
    -- parameter\'s @IpAddresses@ field is required.
    endpointNetworkConfiguration :: Prelude.Maybe EndpointNetworkConfiguration,
    -- | The Amazon Resource Name (ARN) of the file system association.
    fileSystemAssociationARN :: Prelude.Maybe Prelude.Text,
    -- | The status of the file system association. Valid Values: @AVAILABLE@ |
    -- @CREATING@ | @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
    fileSystemAssociationStatus :: Prelude.Maybe Prelude.Text,
    -- | An array containing the FileSystemAssociationStatusDetail data type,
    -- which provides detailed information on file system association status.
    fileSystemAssociationStatusDetails :: Prelude.Maybe [FileSystemAssociationStatusDetail],
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the backend Amazon FSx file system used for storing file
    -- data. For information, see
    -- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_FileSystem.html FileSystem>
    -- in the /Amazon FSx API Reference/.
    locationARN :: Prelude.Maybe Prelude.Text,
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
-- 'cacheAttributes', 'fileSystemAssociationInfo_cacheAttributes' - Undocumented member.
--
-- 'endpointNetworkConfiguration', 'fileSystemAssociationInfo_endpointNetworkConfiguration' - Specifies network configuration information for the gateway associated
-- with the Amazon FSx file system.
--
-- If multiple file systems are associated with this gateway, this
-- parameter\'s @IpAddresses@ field is required.
--
-- 'fileSystemAssociationARN', 'fileSystemAssociationInfo_fileSystemAssociationARN' - The Amazon Resource Name (ARN) of the file system association.
--
-- 'fileSystemAssociationStatus', 'fileSystemAssociationInfo_fileSystemAssociationStatus' - The status of the file system association. Valid Values: @AVAILABLE@ |
-- @CREATING@ | @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
--
-- 'fileSystemAssociationStatusDetails', 'fileSystemAssociationInfo_fileSystemAssociationStatusDetails' - An array containing the FileSystemAssociationStatusDetail data type,
-- which provides detailed information on file system association status.
--
-- 'gatewayARN', 'fileSystemAssociationInfo_gatewayARN' - Undocumented member.
--
-- 'locationARN', 'fileSystemAssociationInfo_locationARN' - The ARN of the backend Amazon FSx file system used for storing file
-- data. For information, see
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_FileSystem.html FileSystem>
-- in the /Amazon FSx API Reference/.
--
-- 'tags', 'fileSystemAssociationInfo_tags' - A list of up to 50 tags assigned to the SMB file share, sorted
-- alphabetically by key name. Each tag is a key-value pair.
newFileSystemAssociationInfo ::
  FileSystemAssociationInfo
newFileSystemAssociationInfo =
  FileSystemAssociationInfo'
    { auditDestinationARN =
        Prelude.Nothing,
      cacheAttributes = Prelude.Nothing,
      endpointNetworkConfiguration = Prelude.Nothing,
      fileSystemAssociationARN = Prelude.Nothing,
      fileSystemAssociationStatus = Prelude.Nothing,
      fileSystemAssociationStatusDetails =
        Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      locationARN = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
fileSystemAssociationInfo_auditDestinationARN :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_auditDestinationARN = Lens.lens (\FileSystemAssociationInfo' {auditDestinationARN} -> auditDestinationARN) (\s@FileSystemAssociationInfo' {} a -> s {auditDestinationARN = a} :: FileSystemAssociationInfo)

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

-- | The Amazon Resource Name (ARN) of the file system association.
fileSystemAssociationInfo_fileSystemAssociationARN :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_fileSystemAssociationARN = Lens.lens (\FileSystemAssociationInfo' {fileSystemAssociationARN} -> fileSystemAssociationARN) (\s@FileSystemAssociationInfo' {} a -> s {fileSystemAssociationARN = a} :: FileSystemAssociationInfo)

-- | The status of the file system association. Valid Values: @AVAILABLE@ |
-- @CREATING@ | @DELETING@ | @FORCE_DELETING@ | @UPDATING@ | @ERROR@
fileSystemAssociationInfo_fileSystemAssociationStatus :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_fileSystemAssociationStatus = Lens.lens (\FileSystemAssociationInfo' {fileSystemAssociationStatus} -> fileSystemAssociationStatus) (\s@FileSystemAssociationInfo' {} a -> s {fileSystemAssociationStatus = a} :: FileSystemAssociationInfo)

-- | An array containing the FileSystemAssociationStatusDetail data type,
-- which provides detailed information on file system association status.
fileSystemAssociationInfo_fileSystemAssociationStatusDetails :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe [FileSystemAssociationStatusDetail])
fileSystemAssociationInfo_fileSystemAssociationStatusDetails = Lens.lens (\FileSystemAssociationInfo' {fileSystemAssociationStatusDetails} -> fileSystemAssociationStatusDetails) (\s@FileSystemAssociationInfo' {} a -> s {fileSystemAssociationStatusDetails = a} :: FileSystemAssociationInfo) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
fileSystemAssociationInfo_gatewayARN :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_gatewayARN = Lens.lens (\FileSystemAssociationInfo' {gatewayARN} -> gatewayARN) (\s@FileSystemAssociationInfo' {} a -> s {gatewayARN = a} :: FileSystemAssociationInfo)

-- | The ARN of the backend Amazon FSx file system used for storing file
-- data. For information, see
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_FileSystem.html FileSystem>
-- in the /Amazon FSx API Reference/.
fileSystemAssociationInfo_locationARN :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe Prelude.Text)
fileSystemAssociationInfo_locationARN = Lens.lens (\FileSystemAssociationInfo' {locationARN} -> locationARN) (\s@FileSystemAssociationInfo' {} a -> s {locationARN = a} :: FileSystemAssociationInfo)

-- | A list of up to 50 tags assigned to the SMB file share, sorted
-- alphabetically by key name. Each tag is a key-value pair.
fileSystemAssociationInfo_tags :: Lens.Lens' FileSystemAssociationInfo (Prelude.Maybe [Tag])
fileSystemAssociationInfo_tags = Lens.lens (\FileSystemAssociationInfo' {tags} -> tags) (\s@FileSystemAssociationInfo' {} a -> s {tags = a} :: FileSystemAssociationInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FileSystemAssociationInfo where
  parseJSON =
    Data.withObject
      "FileSystemAssociationInfo"
      ( \x ->
          FileSystemAssociationInfo'
            Prelude.<$> (x Data..:? "AuditDestinationARN")
            Prelude.<*> (x Data..:? "CacheAttributes")
            Prelude.<*> (x Data..:? "EndpointNetworkConfiguration")
            Prelude.<*> (x Data..:? "FileSystemAssociationARN")
            Prelude.<*> (x Data..:? "FileSystemAssociationStatus")
            Prelude.<*> ( x
                            Data..:? "FileSystemAssociationStatusDetails"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "GatewayARN")
            Prelude.<*> (x Data..:? "LocationARN")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FileSystemAssociationInfo where
  hashWithSalt _salt FileSystemAssociationInfo' {..} =
    _salt
      `Prelude.hashWithSalt` auditDestinationARN
      `Prelude.hashWithSalt` cacheAttributes
      `Prelude.hashWithSalt` endpointNetworkConfiguration
      `Prelude.hashWithSalt` fileSystemAssociationARN
      `Prelude.hashWithSalt` fileSystemAssociationStatus
      `Prelude.hashWithSalt` fileSystemAssociationStatusDetails
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` locationARN
      `Prelude.hashWithSalt` tags

instance Prelude.NFData FileSystemAssociationInfo where
  rnf FileSystemAssociationInfo' {..} =
    Prelude.rnf auditDestinationARN `Prelude.seq`
      Prelude.rnf cacheAttributes `Prelude.seq`
        Prelude.rnf endpointNetworkConfiguration `Prelude.seq`
          Prelude.rnf fileSystemAssociationARN `Prelude.seq`
            Prelude.rnf fileSystemAssociationStatus `Prelude.seq`
              Prelude.rnf fileSystemAssociationStatusDetails `Prelude.seq`
                Prelude.rnf gatewayARN `Prelude.seq`
                  Prelude.rnf locationARN `Prelude.seq`
                    Prelude.rnf tags
