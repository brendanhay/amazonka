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
-- Module      : Amazonka.FSx.CreateFileSystem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty Amazon FSx file system. You can create the
-- following supported Amazon FSx file systems using the @CreateFileSystem@
-- API operation:
--
-- -   Amazon FSx for Lustre
--
-- -   Amazon FSx for NetApp ONTAP
--
-- -   Amazon FSx for OpenZFS
--
-- -   Amazon FSx for Windows File Server
--
-- This operation requires a client request token in the request that
-- Amazon FSx uses to ensure idempotent creation. This means that calling
-- the operation multiple times with the same client request token has no
-- effect. By using the idempotent operation, you can retry a
-- @CreateFileSystem@ operation without the risk of creating an extra file
-- system. This approach can be useful when an initial call fails in a way
-- that makes it unclear whether a file system was created. Examples are if
-- a transport level timeout occurred, or your connection was reset. If you
-- use the same client request token and the initial call created a file
-- system, the client receives success as long as the parameters are the
-- same.
--
-- If a file system with the specified client request token exists and the
-- parameters match, @CreateFileSystem@ returns the description of the
-- existing file system. If a file system with the specified client request
-- token exists and the parameters don\'t match, this call returns
-- @IncompatibleParameterError@. If a file system with the specified client
-- request token doesn\'t exist, @CreateFileSystem@ does the following:
--
-- -   Creates a new, empty Amazon FSx file system with an assigned ID, and
--     an initial lifecycle state of @CREATING@.
--
-- -   Returns the description of the file system in JSON format.
--
-- The @CreateFileSystem@ call returns while the file system\'s lifecycle
-- state is still @CREATING@. You can check the file-system creation status
-- by calling the
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_DescribeFileSystems.html DescribeFileSystems>
-- operation, which returns the file system state along with other
-- information.
module Amazonka.FSx.CreateFileSystem
  ( -- * Creating a Request
    CreateFileSystem (..),
    newCreateFileSystem,

    -- * Request Lenses
    createFileSystem_tags,
    createFileSystem_clientRequestToken,
    createFileSystem_fileSystemTypeVersion,
    createFileSystem_securityGroupIds,
    createFileSystem_openZFSConfiguration,
    createFileSystem_storageType,
    createFileSystem_ontapConfiguration,
    createFileSystem_windowsConfiguration,
    createFileSystem_kmsKeyId,
    createFileSystem_lustreConfiguration,
    createFileSystem_fileSystemType,
    createFileSystem_storageCapacity,
    createFileSystem_subnetIds,

    -- * Destructuring the Response
    CreateFileSystemResponse (..),
    newCreateFileSystemResponse,

    -- * Response Lenses
    createFileSystemResponse_fileSystem,
    createFileSystemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object used to create a new Amazon FSx file system.
--
-- /See:/ 'newCreateFileSystem' smart constructor.
data CreateFileSystem = CreateFileSystem'
  { -- | The tags to apply to the file system that\'s being created. The key
    -- value of the @Name@ tag appears in the console as the file system name.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A string of up to 64 ASCII characters that Amazon FSx uses to ensure
    -- idempotent creation. This string is automatically filled on your behalf
    -- when you use the Command Line Interface (CLI) or an Amazon Web Services
    -- SDK.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) For FSx for Lustre file systems, sets the Lustre version for
    -- the file system that you\'re creating. Valid values are @2.10@ and
    -- @2.12@:
    --
    -- -   2.10 is supported by the Scratch and Persistent_1 Lustre deployment
    --     types.
    --
    -- -   2.12 is supported by all Lustre deployment types. @2.12@ is required
    --     when setting FSx for Lustre @DeploymentType@ to @PERSISTENT_2@.
    --
    -- Default value = @2.10@, except when @DeploymentType@ is set to
    -- @PERSISTENT_2@, then the default is @2.12@.
    --
    -- If you set @FileSystemTypeVersion@ to @2.10@ for a @PERSISTENT_2@ Lustre
    -- deployment type, the @CreateFileSystem@ operation fails.
    fileSystemTypeVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of IDs specifying the security groups to apply to all network
    -- interfaces created for file system access. This list isn\'t returned in
    -- later requests to describe the file system.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The OpenZFS configuration for the file system that\'s being created.
    openZFSConfiguration :: Prelude.Maybe CreateFileSystemOpenZFSConfiguration,
    -- | Sets the storage type for the file system that you\'re creating. Valid
    -- values are @SSD@ and @HDD@.
    --
    -- -   Set to @SSD@ to use solid state drive storage. SSD is supported on
    --     all Windows, Lustre, ONTAP, and OpenZFS deployment types.
    --
    -- -   Set to @HDD@ to use hard disk drive storage. HDD is supported on
    --     @SINGLE_AZ_2@ and @MULTI_AZ_1@ Windows file system deployment types,
    --     and on @PERSISTENT_1@ Lustre file system deployment types.
    --
    -- Default value is @SSD@. For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/optimize-fsx-costs.html#storage-type-options Storage type options>
    -- in the /FSx for Windows File Server User Guide/ and
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/what-is.html#storage-options Multiple storage options>
    -- in the /FSx for Lustre User Guide/.
    storageType :: Prelude.Maybe StorageType,
    ontapConfiguration :: Prelude.Maybe CreateFileSystemOntapConfiguration,
    -- | The Microsoft Windows configuration for the file system that\'s being
    -- created.
    windowsConfiguration :: Prelude.Maybe CreateFileSystemWindowsConfiguration,
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    lustreConfiguration :: Prelude.Maybe CreateFileSystemLustreConfiguration,
    -- | The type of Amazon FSx file system to create. Valid values are
    -- @WINDOWS@, @LUSTRE@, @ONTAP@, and @OPENZFS@.
    fileSystemType :: FileSystemType,
    -- | Sets the storage capacity of the file system that you\'re creating, in
    -- gibibytes (GiB).
    --
    -- __FSx for Lustre file systems__ - The amount of storage capacity that
    -- you can configure depends on the value that you set for @StorageType@
    -- and the Lustre @DeploymentType@, as follows:
    --
    -- -   For @SCRATCH_2@, @PERSISTENT_2@ and @PERSISTENT_1@ deployment types
    --     using SSD storage type, the valid values are 1200 GiB, 2400 GiB, and
    --     increments of 2400 GiB.
    --
    -- -   For @PERSISTENT_1@ HDD file systems, valid values are increments of
    --     6000 GiB for 12 MB\/s\/TiB file systems and increments of 1800 GiB
    --     for 40 MB\/s\/TiB file systems.
    --
    -- -   For @SCRATCH_1@ deployment type, valid values are 1200 GiB, 2400
    --     GiB, and increments of 3600 GiB.
    --
    -- __FSx for ONTAP file systems__ - The amount of storage capacity that you
    -- can configure is from 1024 GiB up to 196,608 GiB (192 TiB).
    --
    -- __FSx for OpenZFS file systems__ - The amount of storage capacity that
    -- you can configure is from 64 GiB up to 524,288 GiB (512 TiB).
    --
    -- __FSx for Windows File Server file systems__ - The amount of storage
    -- capacity that you can configure depends on the value that you set for
    -- @StorageType@ as follows:
    --
    -- -   For SSD storage, valid values are 32 GiB-65,536 GiB (64 TiB).
    --
    -- -   For HDD storage, valid values are 2000 GiB-65,536 GiB (64 TiB).
    storageCapacity :: Prelude.Natural,
    -- | Specifies the IDs of the subnets that the file system will be accessible
    -- from. For Windows and ONTAP @MULTI_AZ_1@ deployment types,provide
    -- exactly two subnet IDs, one for the preferred file server and one for
    -- the standby file server. You specify one of these subnets as the
    -- preferred subnet using the @WindowsConfiguration > PreferredSubnetID@ or
    -- @OntapConfiguration > PreferredSubnetID@ properties. For more
    -- information about Multi-AZ file system configuration, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html Availability and durability: Single-AZ and Multi-AZ file systems>
    -- in the /Amazon FSx for Windows User Guide/ and
    -- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/high-availability-multiAZ.html Availability and durability>
    -- in the /Amazon FSx for ONTAP User Guide/.
    --
    -- For Windows @SINGLE_AZ_1@ and @SINGLE_AZ_2@ and all Lustre deployment
    -- types, provide exactly one subnet ID. The file server is launched in
    -- that subnet\'s Availability Zone.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFileSystem_tags' - The tags to apply to the file system that\'s being created. The key
-- value of the @Name@ tag appears in the console as the file system name.
--
-- 'clientRequestToken', 'createFileSystem_clientRequestToken' - A string of up to 64 ASCII characters that Amazon FSx uses to ensure
-- idempotent creation. This string is automatically filled on your behalf
-- when you use the Command Line Interface (CLI) or an Amazon Web Services
-- SDK.
--
-- 'fileSystemTypeVersion', 'createFileSystem_fileSystemTypeVersion' - (Optional) For FSx for Lustre file systems, sets the Lustre version for
-- the file system that you\'re creating. Valid values are @2.10@ and
-- @2.12@:
--
-- -   2.10 is supported by the Scratch and Persistent_1 Lustre deployment
--     types.
--
-- -   2.12 is supported by all Lustre deployment types. @2.12@ is required
--     when setting FSx for Lustre @DeploymentType@ to @PERSISTENT_2@.
--
-- Default value = @2.10@, except when @DeploymentType@ is set to
-- @PERSISTENT_2@, then the default is @2.12@.
--
-- If you set @FileSystemTypeVersion@ to @2.10@ for a @PERSISTENT_2@ Lustre
-- deployment type, the @CreateFileSystem@ operation fails.
--
-- 'securityGroupIds', 'createFileSystem_securityGroupIds' - A list of IDs specifying the security groups to apply to all network
-- interfaces created for file system access. This list isn\'t returned in
-- later requests to describe the file system.
--
-- 'openZFSConfiguration', 'createFileSystem_openZFSConfiguration' - The OpenZFS configuration for the file system that\'s being created.
--
-- 'storageType', 'createFileSystem_storageType' - Sets the storage type for the file system that you\'re creating. Valid
-- values are @SSD@ and @HDD@.
--
-- -   Set to @SSD@ to use solid state drive storage. SSD is supported on
--     all Windows, Lustre, ONTAP, and OpenZFS deployment types.
--
-- -   Set to @HDD@ to use hard disk drive storage. HDD is supported on
--     @SINGLE_AZ_2@ and @MULTI_AZ_1@ Windows file system deployment types,
--     and on @PERSISTENT_1@ Lustre file system deployment types.
--
-- Default value is @SSD@. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/optimize-fsx-costs.html#storage-type-options Storage type options>
-- in the /FSx for Windows File Server User Guide/ and
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/what-is.html#storage-options Multiple storage options>
-- in the /FSx for Lustre User Guide/.
--
-- 'ontapConfiguration', 'createFileSystem_ontapConfiguration' - Undocumented member.
--
-- 'windowsConfiguration', 'createFileSystem_windowsConfiguration' - The Microsoft Windows configuration for the file system that\'s being
-- created.
--
-- 'kmsKeyId', 'createFileSystem_kmsKeyId' - Undocumented member.
--
-- 'lustreConfiguration', 'createFileSystem_lustreConfiguration' - Undocumented member.
--
-- 'fileSystemType', 'createFileSystem_fileSystemType' - The type of Amazon FSx file system to create. Valid values are
-- @WINDOWS@, @LUSTRE@, @ONTAP@, and @OPENZFS@.
--
-- 'storageCapacity', 'createFileSystem_storageCapacity' - Sets the storage capacity of the file system that you\'re creating, in
-- gibibytes (GiB).
--
-- __FSx for Lustre file systems__ - The amount of storage capacity that
-- you can configure depends on the value that you set for @StorageType@
-- and the Lustre @DeploymentType@, as follows:
--
-- -   For @SCRATCH_2@, @PERSISTENT_2@ and @PERSISTENT_1@ deployment types
--     using SSD storage type, the valid values are 1200 GiB, 2400 GiB, and
--     increments of 2400 GiB.
--
-- -   For @PERSISTENT_1@ HDD file systems, valid values are increments of
--     6000 GiB for 12 MB\/s\/TiB file systems and increments of 1800 GiB
--     for 40 MB\/s\/TiB file systems.
--
-- -   For @SCRATCH_1@ deployment type, valid values are 1200 GiB, 2400
--     GiB, and increments of 3600 GiB.
--
-- __FSx for ONTAP file systems__ - The amount of storage capacity that you
-- can configure is from 1024 GiB up to 196,608 GiB (192 TiB).
--
-- __FSx for OpenZFS file systems__ - The amount of storage capacity that
-- you can configure is from 64 GiB up to 524,288 GiB (512 TiB).
--
-- __FSx for Windows File Server file systems__ - The amount of storage
-- capacity that you can configure depends on the value that you set for
-- @StorageType@ as follows:
--
-- -   For SSD storage, valid values are 32 GiB-65,536 GiB (64 TiB).
--
-- -   For HDD storage, valid values are 2000 GiB-65,536 GiB (64 TiB).
--
-- 'subnetIds', 'createFileSystem_subnetIds' - Specifies the IDs of the subnets that the file system will be accessible
-- from. For Windows and ONTAP @MULTI_AZ_1@ deployment types,provide
-- exactly two subnet IDs, one for the preferred file server and one for
-- the standby file server. You specify one of these subnets as the
-- preferred subnet using the @WindowsConfiguration > PreferredSubnetID@ or
-- @OntapConfiguration > PreferredSubnetID@ properties. For more
-- information about Multi-AZ file system configuration, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html Availability and durability: Single-AZ and Multi-AZ file systems>
-- in the /Amazon FSx for Windows User Guide/ and
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/high-availability-multiAZ.html Availability and durability>
-- in the /Amazon FSx for ONTAP User Guide/.
--
-- For Windows @SINGLE_AZ_1@ and @SINGLE_AZ_2@ and all Lustre deployment
-- types, provide exactly one subnet ID. The file server is launched in
-- that subnet\'s Availability Zone.
newCreateFileSystem ::
  -- | 'fileSystemType'
  FileSystemType ->
  -- | 'storageCapacity'
  Prelude.Natural ->
  CreateFileSystem
newCreateFileSystem
  pFileSystemType_
  pStorageCapacity_ =
    CreateFileSystem'
      { tags = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        fileSystemTypeVersion = Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        openZFSConfiguration = Prelude.Nothing,
        storageType = Prelude.Nothing,
        ontapConfiguration = Prelude.Nothing,
        windowsConfiguration = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        lustreConfiguration = Prelude.Nothing,
        fileSystemType = pFileSystemType_,
        storageCapacity = pStorageCapacity_,
        subnetIds = Prelude.mempty
      }

-- | The tags to apply to the file system that\'s being created. The key
-- value of the @Name@ tag appears in the console as the file system name.
createFileSystem_tags :: Lens.Lens' CreateFileSystem (Prelude.Maybe (Prelude.NonEmpty Tag))
createFileSystem_tags = Lens.lens (\CreateFileSystem' {tags} -> tags) (\s@CreateFileSystem' {} a -> s {tags = a} :: CreateFileSystem) Prelude.. Lens.mapping Lens.coerced

-- | A string of up to 64 ASCII characters that Amazon FSx uses to ensure
-- idempotent creation. This string is automatically filled on your behalf
-- when you use the Command Line Interface (CLI) or an Amazon Web Services
-- SDK.
createFileSystem_clientRequestToken :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Text)
createFileSystem_clientRequestToken = Lens.lens (\CreateFileSystem' {clientRequestToken} -> clientRequestToken) (\s@CreateFileSystem' {} a -> s {clientRequestToken = a} :: CreateFileSystem)

-- | (Optional) For FSx for Lustre file systems, sets the Lustre version for
-- the file system that you\'re creating. Valid values are @2.10@ and
-- @2.12@:
--
-- -   2.10 is supported by the Scratch and Persistent_1 Lustre deployment
--     types.
--
-- -   2.12 is supported by all Lustre deployment types. @2.12@ is required
--     when setting FSx for Lustre @DeploymentType@ to @PERSISTENT_2@.
--
-- Default value = @2.10@, except when @DeploymentType@ is set to
-- @PERSISTENT_2@, then the default is @2.12@.
--
-- If you set @FileSystemTypeVersion@ to @2.10@ for a @PERSISTENT_2@ Lustre
-- deployment type, the @CreateFileSystem@ operation fails.
createFileSystem_fileSystemTypeVersion :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Text)
createFileSystem_fileSystemTypeVersion = Lens.lens (\CreateFileSystem' {fileSystemTypeVersion} -> fileSystemTypeVersion) (\s@CreateFileSystem' {} a -> s {fileSystemTypeVersion = a} :: CreateFileSystem)

-- | A list of IDs specifying the security groups to apply to all network
-- interfaces created for file system access. This list isn\'t returned in
-- later requests to describe the file system.
createFileSystem_securityGroupIds :: Lens.Lens' CreateFileSystem (Prelude.Maybe [Prelude.Text])
createFileSystem_securityGroupIds = Lens.lens (\CreateFileSystem' {securityGroupIds} -> securityGroupIds) (\s@CreateFileSystem' {} a -> s {securityGroupIds = a} :: CreateFileSystem) Prelude.. Lens.mapping Lens.coerced

-- | The OpenZFS configuration for the file system that\'s being created.
createFileSystem_openZFSConfiguration :: Lens.Lens' CreateFileSystem (Prelude.Maybe CreateFileSystemOpenZFSConfiguration)
createFileSystem_openZFSConfiguration = Lens.lens (\CreateFileSystem' {openZFSConfiguration} -> openZFSConfiguration) (\s@CreateFileSystem' {} a -> s {openZFSConfiguration = a} :: CreateFileSystem)

-- | Sets the storage type for the file system that you\'re creating. Valid
-- values are @SSD@ and @HDD@.
--
-- -   Set to @SSD@ to use solid state drive storage. SSD is supported on
--     all Windows, Lustre, ONTAP, and OpenZFS deployment types.
--
-- -   Set to @HDD@ to use hard disk drive storage. HDD is supported on
--     @SINGLE_AZ_2@ and @MULTI_AZ_1@ Windows file system deployment types,
--     and on @PERSISTENT_1@ Lustre file system deployment types.
--
-- Default value is @SSD@. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/optimize-fsx-costs.html#storage-type-options Storage type options>
-- in the /FSx for Windows File Server User Guide/ and
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/what-is.html#storage-options Multiple storage options>
-- in the /FSx for Lustre User Guide/.
createFileSystem_storageType :: Lens.Lens' CreateFileSystem (Prelude.Maybe StorageType)
createFileSystem_storageType = Lens.lens (\CreateFileSystem' {storageType} -> storageType) (\s@CreateFileSystem' {} a -> s {storageType = a} :: CreateFileSystem)

-- | Undocumented member.
createFileSystem_ontapConfiguration :: Lens.Lens' CreateFileSystem (Prelude.Maybe CreateFileSystemOntapConfiguration)
createFileSystem_ontapConfiguration = Lens.lens (\CreateFileSystem' {ontapConfiguration} -> ontapConfiguration) (\s@CreateFileSystem' {} a -> s {ontapConfiguration = a} :: CreateFileSystem)

-- | The Microsoft Windows configuration for the file system that\'s being
-- created.
createFileSystem_windowsConfiguration :: Lens.Lens' CreateFileSystem (Prelude.Maybe CreateFileSystemWindowsConfiguration)
createFileSystem_windowsConfiguration = Lens.lens (\CreateFileSystem' {windowsConfiguration} -> windowsConfiguration) (\s@CreateFileSystem' {} a -> s {windowsConfiguration = a} :: CreateFileSystem)

-- | Undocumented member.
createFileSystem_kmsKeyId :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Text)
createFileSystem_kmsKeyId = Lens.lens (\CreateFileSystem' {kmsKeyId} -> kmsKeyId) (\s@CreateFileSystem' {} a -> s {kmsKeyId = a} :: CreateFileSystem)

-- | Undocumented member.
createFileSystem_lustreConfiguration :: Lens.Lens' CreateFileSystem (Prelude.Maybe CreateFileSystemLustreConfiguration)
createFileSystem_lustreConfiguration = Lens.lens (\CreateFileSystem' {lustreConfiguration} -> lustreConfiguration) (\s@CreateFileSystem' {} a -> s {lustreConfiguration = a} :: CreateFileSystem)

-- | The type of Amazon FSx file system to create. Valid values are
-- @WINDOWS@, @LUSTRE@, @ONTAP@, and @OPENZFS@.
createFileSystem_fileSystemType :: Lens.Lens' CreateFileSystem FileSystemType
createFileSystem_fileSystemType = Lens.lens (\CreateFileSystem' {fileSystemType} -> fileSystemType) (\s@CreateFileSystem' {} a -> s {fileSystemType = a} :: CreateFileSystem)

-- | Sets the storage capacity of the file system that you\'re creating, in
-- gibibytes (GiB).
--
-- __FSx for Lustre file systems__ - The amount of storage capacity that
-- you can configure depends on the value that you set for @StorageType@
-- and the Lustre @DeploymentType@, as follows:
--
-- -   For @SCRATCH_2@, @PERSISTENT_2@ and @PERSISTENT_1@ deployment types
--     using SSD storage type, the valid values are 1200 GiB, 2400 GiB, and
--     increments of 2400 GiB.
--
-- -   For @PERSISTENT_1@ HDD file systems, valid values are increments of
--     6000 GiB for 12 MB\/s\/TiB file systems and increments of 1800 GiB
--     for 40 MB\/s\/TiB file systems.
--
-- -   For @SCRATCH_1@ deployment type, valid values are 1200 GiB, 2400
--     GiB, and increments of 3600 GiB.
--
-- __FSx for ONTAP file systems__ - The amount of storage capacity that you
-- can configure is from 1024 GiB up to 196,608 GiB (192 TiB).
--
-- __FSx for OpenZFS file systems__ - The amount of storage capacity that
-- you can configure is from 64 GiB up to 524,288 GiB (512 TiB).
--
-- __FSx for Windows File Server file systems__ - The amount of storage
-- capacity that you can configure depends on the value that you set for
-- @StorageType@ as follows:
--
-- -   For SSD storage, valid values are 32 GiB-65,536 GiB (64 TiB).
--
-- -   For HDD storage, valid values are 2000 GiB-65,536 GiB (64 TiB).
createFileSystem_storageCapacity :: Lens.Lens' CreateFileSystem Prelude.Natural
createFileSystem_storageCapacity = Lens.lens (\CreateFileSystem' {storageCapacity} -> storageCapacity) (\s@CreateFileSystem' {} a -> s {storageCapacity = a} :: CreateFileSystem)

-- | Specifies the IDs of the subnets that the file system will be accessible
-- from. For Windows and ONTAP @MULTI_AZ_1@ deployment types,provide
-- exactly two subnet IDs, one for the preferred file server and one for
-- the standby file server. You specify one of these subnets as the
-- preferred subnet using the @WindowsConfiguration > PreferredSubnetID@ or
-- @OntapConfiguration > PreferredSubnetID@ properties. For more
-- information about Multi-AZ file system configuration, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/high-availability-multiAZ.html Availability and durability: Single-AZ and Multi-AZ file systems>
-- in the /Amazon FSx for Windows User Guide/ and
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/high-availability-multiAZ.html Availability and durability>
-- in the /Amazon FSx for ONTAP User Guide/.
--
-- For Windows @SINGLE_AZ_1@ and @SINGLE_AZ_2@ and all Lustre deployment
-- types, provide exactly one subnet ID. The file server is launched in
-- that subnet\'s Availability Zone.
createFileSystem_subnetIds :: Lens.Lens' CreateFileSystem [Prelude.Text]
createFileSystem_subnetIds = Lens.lens (\CreateFileSystem' {subnetIds} -> subnetIds) (\s@CreateFileSystem' {} a -> s {subnetIds = a} :: CreateFileSystem) Prelude.. Lens.coerced

instance Core.AWSRequest CreateFileSystem where
  type
    AWSResponse CreateFileSystem =
      CreateFileSystemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFileSystemResponse'
            Prelude.<$> (x Core..?> "FileSystem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFileSystem where
  hashWithSalt _salt CreateFileSystem' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` fileSystemTypeVersion
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` ontapConfiguration
      `Prelude.hashWithSalt` windowsConfiguration
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` lustreConfiguration
      `Prelude.hashWithSalt` fileSystemType
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData CreateFileSystem where
  rnf CreateFileSystem' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf fileSystemTypeVersion
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf openZFSConfiguration
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf ontapConfiguration
      `Prelude.seq` Prelude.rnf windowsConfiguration
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf lustreConfiguration
      `Prelude.seq` Prelude.rnf fileSystemType
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf subnetIds

instance Core.ToHeaders CreateFileSystem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSimbaAPIService_v20180301.CreateFileSystem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFileSystem where
  toJSON CreateFileSystem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("FileSystemTypeVersion" Core..=)
              Prelude.<$> fileSystemTypeVersion,
            ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("OpenZFSConfiguration" Core..=)
              Prelude.<$> openZFSConfiguration,
            ("StorageType" Core..=) Prelude.<$> storageType,
            ("OntapConfiguration" Core..=)
              Prelude.<$> ontapConfiguration,
            ("WindowsConfiguration" Core..=)
              Prelude.<$> windowsConfiguration,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("LustreConfiguration" Core..=)
              Prelude.<$> lustreConfiguration,
            Prelude.Just
              ("FileSystemType" Core..= fileSystemType),
            Prelude.Just
              ("StorageCapacity" Core..= storageCapacity),
            Prelude.Just ("SubnetIds" Core..= subnetIds)
          ]
      )

instance Core.ToPath CreateFileSystem where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateFileSystem where
  toQuery = Prelude.const Prelude.mempty

-- | The response object returned after the file system is created.
--
-- /See:/ 'newCreateFileSystemResponse' smart constructor.
data CreateFileSystemResponse = CreateFileSystemResponse'
  { -- | The configuration of the file system that was created.
    fileSystem :: Prelude.Maybe FileSystem,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystem', 'createFileSystemResponse_fileSystem' - The configuration of the file system that was created.
--
-- 'httpStatus', 'createFileSystemResponse_httpStatus' - The response's http status code.
newCreateFileSystemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFileSystemResponse
newCreateFileSystemResponse pHttpStatus_ =
  CreateFileSystemResponse'
    { fileSystem =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration of the file system that was created.
createFileSystemResponse_fileSystem :: Lens.Lens' CreateFileSystemResponse (Prelude.Maybe FileSystem)
createFileSystemResponse_fileSystem = Lens.lens (\CreateFileSystemResponse' {fileSystem} -> fileSystem) (\s@CreateFileSystemResponse' {} a -> s {fileSystem = a} :: CreateFileSystemResponse)

-- | The response's http status code.
createFileSystemResponse_httpStatus :: Lens.Lens' CreateFileSystemResponse Prelude.Int
createFileSystemResponse_httpStatus = Lens.lens (\CreateFileSystemResponse' {httpStatus} -> httpStatus) (\s@CreateFileSystemResponse' {} a -> s {httpStatus = a} :: CreateFileSystemResponse)

instance Prelude.NFData CreateFileSystemResponse where
  rnf CreateFileSystemResponse' {..} =
    Prelude.rnf fileSystem
      `Prelude.seq` Prelude.rnf httpStatus
