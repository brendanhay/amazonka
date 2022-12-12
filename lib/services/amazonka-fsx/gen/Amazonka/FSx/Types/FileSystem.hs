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
-- Module      : Amazonka.FSx.Types.FileSystem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileSystem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import {-# SOURCE #-} Amazonka.FSx.Types.AdministrativeAction
import Amazonka.FSx.Types.FileSystemFailureDetails
import Amazonka.FSx.Types.FileSystemLifecycle
import Amazonka.FSx.Types.FileSystemType
import Amazonka.FSx.Types.LustreFileSystemConfiguration
import Amazonka.FSx.Types.OntapFileSystemConfiguration
import Amazonka.FSx.Types.OpenZFSFileSystemConfiguration
import Amazonka.FSx.Types.StorageType
import Amazonka.FSx.Types.Tag
import Amazonka.FSx.Types.WindowsFileSystemConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A description of a specific Amazon FSx file system.
--
-- /See:/ 'newFileSystem' smart constructor.
data FileSystem = FileSystem'
  { -- | A list of administrative actions for the file system that are in process
    -- or waiting to be processed. Administrative actions describe changes to
    -- the Amazon FSx system that you have initiated using the
    -- @UpdateFileSystem@ operation.
    administrativeActions :: Prelude.Maybe [AdministrativeAction],
    -- | The time that the file system was created, in seconds (since
    -- 1970-01-01T00:00:00Z), also known as Unix time.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Domain Name System (DNS) name for the file system.
    dNSName :: Prelude.Maybe Prelude.Text,
    failureDetails :: Prelude.Maybe FileSystemFailureDetails,
    -- | The system-generated, unique 17-digit ID of the file system.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon FSx file system, which can be @LUSTRE@, @WINDOWS@,
    -- @ONTAP@, or @OPENZFS@.
    fileSystemType :: Prelude.Maybe FileSystemType,
    -- | The Lustre version of the Amazon FSx for Lustre file system, either
    -- @2.10@ or @2.12@.
    fileSystemTypeVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Key Management Service (KMS) key used to encrypt Amazon
    -- FSx file system data. Used as follows with Amazon FSx file system types:
    --
    -- -   Amazon FSx for Lustre @PERSISTENT_1@ and @PERSISTENT_2@ deployment
    --     types only.
    --
    --     @SCRATCH_1@ and @SCRATCH_2@ types are encrypted using the Amazon FSx
    --     service KMS key for your account.
    --
    -- -   Amazon FSx for NetApp ONTAP
    --
    -- -   Amazon FSx for OpenZFS
    --
    -- -   Amazon FSx for Windows File Server
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle status of the file system. The following are the possible
    -- values and what they mean:
    --
    -- -   @AVAILABLE@ - The file system is in a healthy state, and is
    --     reachable and available for use.
    --
    -- -   @CREATING@ - Amazon FSx is creating the new file system.
    --
    -- -   @DELETING@ - Amazon FSx is deleting an existing file system.
    --
    -- -   @FAILED@ - An existing file system has experienced an unrecoverable
    --     failure. When creating a new file system, Amazon FSx was unable to
    --     create the file system.
    --
    -- -   @MISCONFIGURED@ - The file system is in a failed but recoverable
    --     state.
    --
    -- -   @MISCONFIGURED_UNAVAILABLE@ - (Amazon FSx for Windows File Server
    --     only) The file system is currently unavailable due to a change in
    --     your Active Directory configuration.
    --
    -- -   @UPDATING@ - The file system is undergoing a customer-initiated
    --     update.
    lifecycle :: Prelude.Maybe FileSystemLifecycle,
    lustreConfiguration :: Prelude.Maybe LustreFileSystemConfiguration,
    -- | The IDs of the elastic network interfaces from which a specific file
    -- system is accessible. The elastic network interface is automatically
    -- created in the same virtual private cloud (VPC) that the Amazon FSx file
    -- system was created in. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html Elastic Network Interfaces>
    -- in the /Amazon EC2 User Guide./
    --
    -- For an Amazon FSx for Windows File Server file system, you can have one
    -- network interface ID. For an Amazon FSx for Lustre file system, you can
    -- have more than one.
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | The configuration for this Amazon FSx for NetApp ONTAP file system.
    ontapConfiguration :: Prelude.Maybe OntapFileSystemConfiguration,
    -- | The configuration for this Amazon FSx for OpenZFS file system.
    openZFSConfiguration :: Prelude.Maybe OpenZFSFileSystemConfiguration,
    -- | The Amazon Web Services account that created the file system. If the
    -- file system was created by an Identity and Access Management (IAM) user,
    -- the Amazon Web Services account to which the IAM user belongs is the
    -- owner.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the file system resource.
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The storage capacity of the file system in gibibytes (GiB).
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The type of storage the file system is using. If set to @SSD@, the file
    -- system uses solid state drive storage. If set to @HDD@, the file system
    -- uses hard disk drive storage.
    storageType :: Prelude.Maybe StorageType,
    -- | Specifies the IDs of the subnets that the file system is accessible
    -- from. For the Amazon FSx Windows and ONTAP @MULTI_AZ_1@ file system
    -- deployment type, there are two subnet IDs, one for the preferred file
    -- server and one for the standby file server. The preferred file server
    -- subnet identified in the @PreferredSubnetID@ property. All other file
    -- systems have only one subnet ID.
    --
    -- For FSx for Lustre file systems, and Single-AZ Windows file systems,
    -- this is the ID of the subnet that contains the file system\'s endpoint.
    -- For @MULTI_AZ_1@ Windows and ONTAP file systems, the file system
    -- endpoint is available in the @PreferredSubnetID@.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags to associate with the file system. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
    -- in the /Amazon EC2 User Guide/.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The ID of the primary virtual private cloud (VPC) for the file system.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The configuration for this Amazon FSx for Windows File Server file
    -- system.
    windowsConfiguration :: Prelude.Maybe WindowsFileSystemConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'administrativeActions', 'fileSystem_administrativeActions' - A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system that you have initiated using the
-- @UpdateFileSystem@ operation.
--
-- 'creationTime', 'fileSystem_creationTime' - The time that the file system was created, in seconds (since
-- 1970-01-01T00:00:00Z), also known as Unix time.
--
-- 'dNSName', 'fileSystem_dNSName' - The Domain Name System (DNS) name for the file system.
--
-- 'failureDetails', 'fileSystem_failureDetails' - Undocumented member.
--
-- 'fileSystemId', 'fileSystem_fileSystemId' - The system-generated, unique 17-digit ID of the file system.
--
-- 'fileSystemType', 'fileSystem_fileSystemType' - The type of Amazon FSx file system, which can be @LUSTRE@, @WINDOWS@,
-- @ONTAP@, or @OPENZFS@.
--
-- 'fileSystemTypeVersion', 'fileSystem_fileSystemTypeVersion' - The Lustre version of the Amazon FSx for Lustre file system, either
-- @2.10@ or @2.12@.
--
-- 'kmsKeyId', 'fileSystem_kmsKeyId' - The ID of the Key Management Service (KMS) key used to encrypt Amazon
-- FSx file system data. Used as follows with Amazon FSx file system types:
--
-- -   Amazon FSx for Lustre @PERSISTENT_1@ and @PERSISTENT_2@ deployment
--     types only.
--
--     @SCRATCH_1@ and @SCRATCH_2@ types are encrypted using the Amazon FSx
--     service KMS key for your account.
--
-- -   Amazon FSx for NetApp ONTAP
--
-- -   Amazon FSx for OpenZFS
--
-- -   Amazon FSx for Windows File Server
--
-- 'lifecycle', 'fileSystem_lifecycle' - The lifecycle status of the file system. The following are the possible
-- values and what they mean:
--
-- -   @AVAILABLE@ - The file system is in a healthy state, and is
--     reachable and available for use.
--
-- -   @CREATING@ - Amazon FSx is creating the new file system.
--
-- -   @DELETING@ - Amazon FSx is deleting an existing file system.
--
-- -   @FAILED@ - An existing file system has experienced an unrecoverable
--     failure. When creating a new file system, Amazon FSx was unable to
--     create the file system.
--
-- -   @MISCONFIGURED@ - The file system is in a failed but recoverable
--     state.
--
-- -   @MISCONFIGURED_UNAVAILABLE@ - (Amazon FSx for Windows File Server
--     only) The file system is currently unavailable due to a change in
--     your Active Directory configuration.
--
-- -   @UPDATING@ - The file system is undergoing a customer-initiated
--     update.
--
-- 'lustreConfiguration', 'fileSystem_lustreConfiguration' - Undocumented member.
--
-- 'networkInterfaceIds', 'fileSystem_networkInterfaceIds' - The IDs of the elastic network interfaces from which a specific file
-- system is accessible. The elastic network interface is automatically
-- created in the same virtual private cloud (VPC) that the Amazon FSx file
-- system was created in. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html Elastic Network Interfaces>
-- in the /Amazon EC2 User Guide./
--
-- For an Amazon FSx for Windows File Server file system, you can have one
-- network interface ID. For an Amazon FSx for Lustre file system, you can
-- have more than one.
--
-- 'ontapConfiguration', 'fileSystem_ontapConfiguration' - The configuration for this Amazon FSx for NetApp ONTAP file system.
--
-- 'openZFSConfiguration', 'fileSystem_openZFSConfiguration' - The configuration for this Amazon FSx for OpenZFS file system.
--
-- 'ownerId', 'fileSystem_ownerId' - The Amazon Web Services account that created the file system. If the
-- file system was created by an Identity and Access Management (IAM) user,
-- the Amazon Web Services account to which the IAM user belongs is the
-- owner.
--
-- 'resourceARN', 'fileSystem_resourceARN' - The Amazon Resource Name (ARN) of the file system resource.
--
-- 'storageCapacity', 'fileSystem_storageCapacity' - The storage capacity of the file system in gibibytes (GiB).
--
-- 'storageType', 'fileSystem_storageType' - The type of storage the file system is using. If set to @SSD@, the file
-- system uses solid state drive storage. If set to @HDD@, the file system
-- uses hard disk drive storage.
--
-- 'subnetIds', 'fileSystem_subnetIds' - Specifies the IDs of the subnets that the file system is accessible
-- from. For the Amazon FSx Windows and ONTAP @MULTI_AZ_1@ file system
-- deployment type, there are two subnet IDs, one for the preferred file
-- server and one for the standby file server. The preferred file server
-- subnet identified in the @PreferredSubnetID@ property. All other file
-- systems have only one subnet ID.
--
-- For FSx for Lustre file systems, and Single-AZ Windows file systems,
-- this is the ID of the subnet that contains the file system\'s endpoint.
-- For @MULTI_AZ_1@ Windows and ONTAP file systems, the file system
-- endpoint is available in the @PreferredSubnetID@.
--
-- 'tags', 'fileSystem_tags' - The tags to associate with the file system. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
-- in the /Amazon EC2 User Guide/.
--
-- 'vpcId', 'fileSystem_vpcId' - The ID of the primary virtual private cloud (VPC) for the file system.
--
-- 'windowsConfiguration', 'fileSystem_windowsConfiguration' - The configuration for this Amazon FSx for Windows File Server file
-- system.
newFileSystem ::
  FileSystem
newFileSystem =
  FileSystem'
    { administrativeActions =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dNSName = Prelude.Nothing,
      failureDetails = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      fileSystemType = Prelude.Nothing,
      fileSystemTypeVersion = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      lustreConfiguration = Prelude.Nothing,
      networkInterfaceIds = Prelude.Nothing,
      ontapConfiguration = Prelude.Nothing,
      openZFSConfiguration = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      storageType = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      windowsConfiguration = Prelude.Nothing
    }

-- | A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system that you have initiated using the
-- @UpdateFileSystem@ operation.
fileSystem_administrativeActions :: Lens.Lens' FileSystem (Prelude.Maybe [AdministrativeAction])
fileSystem_administrativeActions = Lens.lens (\FileSystem' {administrativeActions} -> administrativeActions) (\s@FileSystem' {} a -> s {administrativeActions = a} :: FileSystem) Prelude.. Lens.mapping Lens.coerced

-- | The time that the file system was created, in seconds (since
-- 1970-01-01T00:00:00Z), also known as Unix time.
fileSystem_creationTime :: Lens.Lens' FileSystem (Prelude.Maybe Prelude.UTCTime)
fileSystem_creationTime = Lens.lens (\FileSystem' {creationTime} -> creationTime) (\s@FileSystem' {} a -> s {creationTime = a} :: FileSystem) Prelude.. Lens.mapping Data._Time

-- | The Domain Name System (DNS) name for the file system.
fileSystem_dNSName :: Lens.Lens' FileSystem (Prelude.Maybe Prelude.Text)
fileSystem_dNSName = Lens.lens (\FileSystem' {dNSName} -> dNSName) (\s@FileSystem' {} a -> s {dNSName = a} :: FileSystem)

-- | Undocumented member.
fileSystem_failureDetails :: Lens.Lens' FileSystem (Prelude.Maybe FileSystemFailureDetails)
fileSystem_failureDetails = Lens.lens (\FileSystem' {failureDetails} -> failureDetails) (\s@FileSystem' {} a -> s {failureDetails = a} :: FileSystem)

-- | The system-generated, unique 17-digit ID of the file system.
fileSystem_fileSystemId :: Lens.Lens' FileSystem (Prelude.Maybe Prelude.Text)
fileSystem_fileSystemId = Lens.lens (\FileSystem' {fileSystemId} -> fileSystemId) (\s@FileSystem' {} a -> s {fileSystemId = a} :: FileSystem)

-- | The type of Amazon FSx file system, which can be @LUSTRE@, @WINDOWS@,
-- @ONTAP@, or @OPENZFS@.
fileSystem_fileSystemType :: Lens.Lens' FileSystem (Prelude.Maybe FileSystemType)
fileSystem_fileSystemType = Lens.lens (\FileSystem' {fileSystemType} -> fileSystemType) (\s@FileSystem' {} a -> s {fileSystemType = a} :: FileSystem)

-- | The Lustre version of the Amazon FSx for Lustre file system, either
-- @2.10@ or @2.12@.
fileSystem_fileSystemTypeVersion :: Lens.Lens' FileSystem (Prelude.Maybe Prelude.Text)
fileSystem_fileSystemTypeVersion = Lens.lens (\FileSystem' {fileSystemTypeVersion} -> fileSystemTypeVersion) (\s@FileSystem' {} a -> s {fileSystemTypeVersion = a} :: FileSystem)

-- | The ID of the Key Management Service (KMS) key used to encrypt Amazon
-- FSx file system data. Used as follows with Amazon FSx file system types:
--
-- -   Amazon FSx for Lustre @PERSISTENT_1@ and @PERSISTENT_2@ deployment
--     types only.
--
--     @SCRATCH_1@ and @SCRATCH_2@ types are encrypted using the Amazon FSx
--     service KMS key for your account.
--
-- -   Amazon FSx for NetApp ONTAP
--
-- -   Amazon FSx for OpenZFS
--
-- -   Amazon FSx for Windows File Server
fileSystem_kmsKeyId :: Lens.Lens' FileSystem (Prelude.Maybe Prelude.Text)
fileSystem_kmsKeyId = Lens.lens (\FileSystem' {kmsKeyId} -> kmsKeyId) (\s@FileSystem' {} a -> s {kmsKeyId = a} :: FileSystem)

-- | The lifecycle status of the file system. The following are the possible
-- values and what they mean:
--
-- -   @AVAILABLE@ - The file system is in a healthy state, and is
--     reachable and available for use.
--
-- -   @CREATING@ - Amazon FSx is creating the new file system.
--
-- -   @DELETING@ - Amazon FSx is deleting an existing file system.
--
-- -   @FAILED@ - An existing file system has experienced an unrecoverable
--     failure. When creating a new file system, Amazon FSx was unable to
--     create the file system.
--
-- -   @MISCONFIGURED@ - The file system is in a failed but recoverable
--     state.
--
-- -   @MISCONFIGURED_UNAVAILABLE@ - (Amazon FSx for Windows File Server
--     only) The file system is currently unavailable due to a change in
--     your Active Directory configuration.
--
-- -   @UPDATING@ - The file system is undergoing a customer-initiated
--     update.
fileSystem_lifecycle :: Lens.Lens' FileSystem (Prelude.Maybe FileSystemLifecycle)
fileSystem_lifecycle = Lens.lens (\FileSystem' {lifecycle} -> lifecycle) (\s@FileSystem' {} a -> s {lifecycle = a} :: FileSystem)

-- | Undocumented member.
fileSystem_lustreConfiguration :: Lens.Lens' FileSystem (Prelude.Maybe LustreFileSystemConfiguration)
fileSystem_lustreConfiguration = Lens.lens (\FileSystem' {lustreConfiguration} -> lustreConfiguration) (\s@FileSystem' {} a -> s {lustreConfiguration = a} :: FileSystem)

-- | The IDs of the elastic network interfaces from which a specific file
-- system is accessible. The elastic network interface is automatically
-- created in the same virtual private cloud (VPC) that the Amazon FSx file
-- system was created in. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html Elastic Network Interfaces>
-- in the /Amazon EC2 User Guide./
--
-- For an Amazon FSx for Windows File Server file system, you can have one
-- network interface ID. For an Amazon FSx for Lustre file system, you can
-- have more than one.
fileSystem_networkInterfaceIds :: Lens.Lens' FileSystem (Prelude.Maybe [Prelude.Text])
fileSystem_networkInterfaceIds = Lens.lens (\FileSystem' {networkInterfaceIds} -> networkInterfaceIds) (\s@FileSystem' {} a -> s {networkInterfaceIds = a} :: FileSystem) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for this Amazon FSx for NetApp ONTAP file system.
fileSystem_ontapConfiguration :: Lens.Lens' FileSystem (Prelude.Maybe OntapFileSystemConfiguration)
fileSystem_ontapConfiguration = Lens.lens (\FileSystem' {ontapConfiguration} -> ontapConfiguration) (\s@FileSystem' {} a -> s {ontapConfiguration = a} :: FileSystem)

-- | The configuration for this Amazon FSx for OpenZFS file system.
fileSystem_openZFSConfiguration :: Lens.Lens' FileSystem (Prelude.Maybe OpenZFSFileSystemConfiguration)
fileSystem_openZFSConfiguration = Lens.lens (\FileSystem' {openZFSConfiguration} -> openZFSConfiguration) (\s@FileSystem' {} a -> s {openZFSConfiguration = a} :: FileSystem)

-- | The Amazon Web Services account that created the file system. If the
-- file system was created by an Identity and Access Management (IAM) user,
-- the Amazon Web Services account to which the IAM user belongs is the
-- owner.
fileSystem_ownerId :: Lens.Lens' FileSystem (Prelude.Maybe Prelude.Text)
fileSystem_ownerId = Lens.lens (\FileSystem' {ownerId} -> ownerId) (\s@FileSystem' {} a -> s {ownerId = a} :: FileSystem)

-- | The Amazon Resource Name (ARN) of the file system resource.
fileSystem_resourceARN :: Lens.Lens' FileSystem (Prelude.Maybe Prelude.Text)
fileSystem_resourceARN = Lens.lens (\FileSystem' {resourceARN} -> resourceARN) (\s@FileSystem' {} a -> s {resourceARN = a} :: FileSystem)

-- | The storage capacity of the file system in gibibytes (GiB).
fileSystem_storageCapacity :: Lens.Lens' FileSystem (Prelude.Maybe Prelude.Natural)
fileSystem_storageCapacity = Lens.lens (\FileSystem' {storageCapacity} -> storageCapacity) (\s@FileSystem' {} a -> s {storageCapacity = a} :: FileSystem)

-- | The type of storage the file system is using. If set to @SSD@, the file
-- system uses solid state drive storage. If set to @HDD@, the file system
-- uses hard disk drive storage.
fileSystem_storageType :: Lens.Lens' FileSystem (Prelude.Maybe StorageType)
fileSystem_storageType = Lens.lens (\FileSystem' {storageType} -> storageType) (\s@FileSystem' {} a -> s {storageType = a} :: FileSystem)

-- | Specifies the IDs of the subnets that the file system is accessible
-- from. For the Amazon FSx Windows and ONTAP @MULTI_AZ_1@ file system
-- deployment type, there are two subnet IDs, one for the preferred file
-- server and one for the standby file server. The preferred file server
-- subnet identified in the @PreferredSubnetID@ property. All other file
-- systems have only one subnet ID.
--
-- For FSx for Lustre file systems, and Single-AZ Windows file systems,
-- this is the ID of the subnet that contains the file system\'s endpoint.
-- For @MULTI_AZ_1@ Windows and ONTAP file systems, the file system
-- endpoint is available in the @PreferredSubnetID@.
fileSystem_subnetIds :: Lens.Lens' FileSystem (Prelude.Maybe [Prelude.Text])
fileSystem_subnetIds = Lens.lens (\FileSystem' {subnetIds} -> subnetIds) (\s@FileSystem' {} a -> s {subnetIds = a} :: FileSystem) Prelude.. Lens.mapping Lens.coerced

-- | The tags to associate with the file system. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
-- in the /Amazon EC2 User Guide/.
fileSystem_tags :: Lens.Lens' FileSystem (Prelude.Maybe (Prelude.NonEmpty Tag))
fileSystem_tags = Lens.lens (\FileSystem' {tags} -> tags) (\s@FileSystem' {} a -> s {tags = a} :: FileSystem) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the primary virtual private cloud (VPC) for the file system.
fileSystem_vpcId :: Lens.Lens' FileSystem (Prelude.Maybe Prelude.Text)
fileSystem_vpcId = Lens.lens (\FileSystem' {vpcId} -> vpcId) (\s@FileSystem' {} a -> s {vpcId = a} :: FileSystem)

-- | The configuration for this Amazon FSx for Windows File Server file
-- system.
fileSystem_windowsConfiguration :: Lens.Lens' FileSystem (Prelude.Maybe WindowsFileSystemConfiguration)
fileSystem_windowsConfiguration = Lens.lens (\FileSystem' {windowsConfiguration} -> windowsConfiguration) (\s@FileSystem' {} a -> s {windowsConfiguration = a} :: FileSystem)

instance Data.FromJSON FileSystem where
  parseJSON =
    Data.withObject
      "FileSystem"
      ( \x ->
          FileSystem'
            Prelude.<$> ( x Data..:? "AdministrativeActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DNSName")
            Prelude.<*> (x Data..:? "FailureDetails")
            Prelude.<*> (x Data..:? "FileSystemId")
            Prelude.<*> (x Data..:? "FileSystemType")
            Prelude.<*> (x Data..:? "FileSystemTypeVersion")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "LustreConfiguration")
            Prelude.<*> ( x Data..:? "NetworkInterfaceIds"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OntapConfiguration")
            Prelude.<*> (x Data..:? "OpenZFSConfiguration")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "StorageCapacity")
            Prelude.<*> (x Data..:? "StorageType")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "WindowsConfiguration")
      )

instance Prelude.Hashable FileSystem where
  hashWithSalt _salt FileSystem' {..} =
    _salt `Prelude.hashWithSalt` administrativeActions
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dNSName
      `Prelude.hashWithSalt` failureDetails
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` fileSystemType
      `Prelude.hashWithSalt` fileSystemTypeVersion
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` lustreConfiguration
      `Prelude.hashWithSalt` networkInterfaceIds
      `Prelude.hashWithSalt` ontapConfiguration
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` windowsConfiguration

instance Prelude.NFData FileSystem where
  rnf FileSystem' {..} =
    Prelude.rnf administrativeActions
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dNSName
      `Prelude.seq` Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf fileSystemType
      `Prelude.seq` Prelude.rnf fileSystemTypeVersion
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf lustreConfiguration
      `Prelude.seq` Prelude.rnf networkInterfaceIds
      `Prelude.seq` Prelude.rnf ontapConfiguration
      `Prelude.seq` Prelude.rnf openZFSConfiguration
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf
        windowsConfiguration
