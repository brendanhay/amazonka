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
-- Module      : Amazonka.FSx.Types.CreateOntapVolumeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateOntapVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.SecurityStyle
import Amazonka.FSx.Types.TieringPolicy
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration of the ONTAP volume that you are creating.
--
-- /See:/ 'newCreateOntapVolumeConfiguration' smart constructor.
data CreateOntapVolumeConfiguration = CreateOntapVolumeConfiguration'
  { tieringPolicy :: Prelude.Maybe TieringPolicy,
    -- | Specifies the security style for the volume. If a volume\'s security
    -- style is not specified, it is automatically set to the root volume\'s
    -- security style. The security style determines the type of permissions
    -- that FSx for ONTAP uses to control data access. For more information,
    -- see
    -- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/managing-volumes.html#volume-security-style Volume security style>
    -- in the /Amazon FSx for NetApp ONTAP User Guide/. Specify one of the
    -- following values:
    --
    -- -   @UNIX@ if the file system is managed by a UNIX administrator, the
    --     majority of users are NFS clients, and an application accessing the
    --     data uses a UNIX user as the service account.
    --
    -- -   @NTFS@ if the file system is managed by a Windows administrator, the
    --     majority of users are SMB clients, and an application accessing the
    --     data uses a Windows user as the service account.
    --
    -- -   @MIXED@ if the file system is managed by both UNIX and Windows
    --     administrators and users consist of both NFS and SMB clients.
    securityStyle :: Prelude.Maybe SecurityStyle,
    -- | Specifies the location in the SVM\'s namespace where the volume is
    -- mounted. The @JunctionPath@ must have a leading forward slash, such as
    -- @\/vol3@.
    junctionPath :: Prelude.Text,
    -- | Specifies the size of the volume, in megabytes (MB), that you are
    -- creating.
    sizeInMegabytes :: Prelude.Natural,
    -- | Set to true to enable deduplication, compression, and compaction storage
    -- efficiency features on the volume.
    storageEfficiencyEnabled :: Prelude.Bool,
    -- | Specifies the ONTAP SVM in which to create the volume.
    storageVirtualMachineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOntapVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tieringPolicy', 'createOntapVolumeConfiguration_tieringPolicy' - Undocumented member.
--
-- 'securityStyle', 'createOntapVolumeConfiguration_securityStyle' - Specifies the security style for the volume. If a volume\'s security
-- style is not specified, it is automatically set to the root volume\'s
-- security style. The security style determines the type of permissions
-- that FSx for ONTAP uses to control data access. For more information,
-- see
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/managing-volumes.html#volume-security-style Volume security style>
-- in the /Amazon FSx for NetApp ONTAP User Guide/. Specify one of the
-- following values:
--
-- -   @UNIX@ if the file system is managed by a UNIX administrator, the
--     majority of users are NFS clients, and an application accessing the
--     data uses a UNIX user as the service account.
--
-- -   @NTFS@ if the file system is managed by a Windows administrator, the
--     majority of users are SMB clients, and an application accessing the
--     data uses a Windows user as the service account.
--
-- -   @MIXED@ if the file system is managed by both UNIX and Windows
--     administrators and users consist of both NFS and SMB clients.
--
-- 'junctionPath', 'createOntapVolumeConfiguration_junctionPath' - Specifies the location in the SVM\'s namespace where the volume is
-- mounted. The @JunctionPath@ must have a leading forward slash, such as
-- @\/vol3@.
--
-- 'sizeInMegabytes', 'createOntapVolumeConfiguration_sizeInMegabytes' - Specifies the size of the volume, in megabytes (MB), that you are
-- creating.
--
-- 'storageEfficiencyEnabled', 'createOntapVolumeConfiguration_storageEfficiencyEnabled' - Set to true to enable deduplication, compression, and compaction storage
-- efficiency features on the volume.
--
-- 'storageVirtualMachineId', 'createOntapVolumeConfiguration_storageVirtualMachineId' - Specifies the ONTAP SVM in which to create the volume.
newCreateOntapVolumeConfiguration ::
  -- | 'junctionPath'
  Prelude.Text ->
  -- | 'sizeInMegabytes'
  Prelude.Natural ->
  -- | 'storageEfficiencyEnabled'
  Prelude.Bool ->
  -- | 'storageVirtualMachineId'
  Prelude.Text ->
  CreateOntapVolumeConfiguration
newCreateOntapVolumeConfiguration
  pJunctionPath_
  pSizeInMegabytes_
  pStorageEfficiencyEnabled_
  pStorageVirtualMachineId_ =
    CreateOntapVolumeConfiguration'
      { tieringPolicy =
          Prelude.Nothing,
        securityStyle = Prelude.Nothing,
        junctionPath = pJunctionPath_,
        sizeInMegabytes = pSizeInMegabytes_,
        storageEfficiencyEnabled =
          pStorageEfficiencyEnabled_,
        storageVirtualMachineId =
          pStorageVirtualMachineId_
      }

-- | Undocumented member.
createOntapVolumeConfiguration_tieringPolicy :: Lens.Lens' CreateOntapVolumeConfiguration (Prelude.Maybe TieringPolicy)
createOntapVolumeConfiguration_tieringPolicy = Lens.lens (\CreateOntapVolumeConfiguration' {tieringPolicy} -> tieringPolicy) (\s@CreateOntapVolumeConfiguration' {} a -> s {tieringPolicy = a} :: CreateOntapVolumeConfiguration)

-- | Specifies the security style for the volume. If a volume\'s security
-- style is not specified, it is automatically set to the root volume\'s
-- security style. The security style determines the type of permissions
-- that FSx for ONTAP uses to control data access. For more information,
-- see
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/managing-volumes.html#volume-security-style Volume security style>
-- in the /Amazon FSx for NetApp ONTAP User Guide/. Specify one of the
-- following values:
--
-- -   @UNIX@ if the file system is managed by a UNIX administrator, the
--     majority of users are NFS clients, and an application accessing the
--     data uses a UNIX user as the service account.
--
-- -   @NTFS@ if the file system is managed by a Windows administrator, the
--     majority of users are SMB clients, and an application accessing the
--     data uses a Windows user as the service account.
--
-- -   @MIXED@ if the file system is managed by both UNIX and Windows
--     administrators and users consist of both NFS and SMB clients.
createOntapVolumeConfiguration_securityStyle :: Lens.Lens' CreateOntapVolumeConfiguration (Prelude.Maybe SecurityStyle)
createOntapVolumeConfiguration_securityStyle = Lens.lens (\CreateOntapVolumeConfiguration' {securityStyle} -> securityStyle) (\s@CreateOntapVolumeConfiguration' {} a -> s {securityStyle = a} :: CreateOntapVolumeConfiguration)

-- | Specifies the location in the SVM\'s namespace where the volume is
-- mounted. The @JunctionPath@ must have a leading forward slash, such as
-- @\/vol3@.
createOntapVolumeConfiguration_junctionPath :: Lens.Lens' CreateOntapVolumeConfiguration Prelude.Text
createOntapVolumeConfiguration_junctionPath = Lens.lens (\CreateOntapVolumeConfiguration' {junctionPath} -> junctionPath) (\s@CreateOntapVolumeConfiguration' {} a -> s {junctionPath = a} :: CreateOntapVolumeConfiguration)

-- | Specifies the size of the volume, in megabytes (MB), that you are
-- creating.
createOntapVolumeConfiguration_sizeInMegabytes :: Lens.Lens' CreateOntapVolumeConfiguration Prelude.Natural
createOntapVolumeConfiguration_sizeInMegabytes = Lens.lens (\CreateOntapVolumeConfiguration' {sizeInMegabytes} -> sizeInMegabytes) (\s@CreateOntapVolumeConfiguration' {} a -> s {sizeInMegabytes = a} :: CreateOntapVolumeConfiguration)

-- | Set to true to enable deduplication, compression, and compaction storage
-- efficiency features on the volume.
createOntapVolumeConfiguration_storageEfficiencyEnabled :: Lens.Lens' CreateOntapVolumeConfiguration Prelude.Bool
createOntapVolumeConfiguration_storageEfficiencyEnabled = Lens.lens (\CreateOntapVolumeConfiguration' {storageEfficiencyEnabled} -> storageEfficiencyEnabled) (\s@CreateOntapVolumeConfiguration' {} a -> s {storageEfficiencyEnabled = a} :: CreateOntapVolumeConfiguration)

-- | Specifies the ONTAP SVM in which to create the volume.
createOntapVolumeConfiguration_storageVirtualMachineId :: Lens.Lens' CreateOntapVolumeConfiguration Prelude.Text
createOntapVolumeConfiguration_storageVirtualMachineId = Lens.lens (\CreateOntapVolumeConfiguration' {storageVirtualMachineId} -> storageVirtualMachineId) (\s@CreateOntapVolumeConfiguration' {} a -> s {storageVirtualMachineId = a} :: CreateOntapVolumeConfiguration)

instance
  Prelude.Hashable
    CreateOntapVolumeConfiguration
  where
  hashWithSalt
    _salt
    CreateOntapVolumeConfiguration' {..} =
      _salt `Prelude.hashWithSalt` tieringPolicy
        `Prelude.hashWithSalt` securityStyle
        `Prelude.hashWithSalt` junctionPath
        `Prelude.hashWithSalt` sizeInMegabytes
        `Prelude.hashWithSalt` storageEfficiencyEnabled
        `Prelude.hashWithSalt` storageVirtualMachineId

instance
  Prelude.NFData
    CreateOntapVolumeConfiguration
  where
  rnf CreateOntapVolumeConfiguration' {..} =
    Prelude.rnf tieringPolicy
      `Prelude.seq` Prelude.rnf securityStyle
      `Prelude.seq` Prelude.rnf junctionPath
      `Prelude.seq` Prelude.rnf sizeInMegabytes
      `Prelude.seq` Prelude.rnf storageEfficiencyEnabled
      `Prelude.seq` Prelude.rnf storageVirtualMachineId

instance Data.ToJSON CreateOntapVolumeConfiguration where
  toJSON CreateOntapVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TieringPolicy" Data..=) Prelude.<$> tieringPolicy,
            ("SecurityStyle" Data..=) Prelude.<$> securityStyle,
            Prelude.Just ("JunctionPath" Data..= junctionPath),
            Prelude.Just
              ("SizeInMegabytes" Data..= sizeInMegabytes),
            Prelude.Just
              ( "StorageEfficiencyEnabled"
                  Data..= storageEfficiencyEnabled
              ),
            Prelude.Just
              ( "StorageVirtualMachineId"
                  Data..= storageVirtualMachineId
              )
          ]
      )
