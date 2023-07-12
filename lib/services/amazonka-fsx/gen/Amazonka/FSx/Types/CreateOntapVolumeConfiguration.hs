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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CreateOntapVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.InputOntapVolumeType
import Amazonka.FSx.Types.SecurityStyle
import Amazonka.FSx.Types.TieringPolicy
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration of the ONTAP volume that you are creating.
--
-- /See:/ 'newCreateOntapVolumeConfiguration' smart constructor.
data CreateOntapVolumeConfiguration = CreateOntapVolumeConfiguration'
  { -- | A boolean flag indicating whether tags for the volume should be copied
    -- to backups. This value defaults to false. If it\'s set to true, all tags
    -- for the volume are copied to all automatic and user-initiated backups
    -- where the user doesn\'t specify tags. If this value is true, and you
    -- specify one or more tags, only the specified tags are copied to backups.
    -- If you specify one or more tags when creating a user-initiated backup,
    -- no tags are copied from the volume, regardless of this value.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the location in the SVM\'s namespace where the volume is
    -- mounted. The @JunctionPath@ must have a leading forward slash, such as
    -- @\/vol3@.
    junctionPath :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of volume you are creating. Valid values are the
    -- following:
    --
    -- -   @RW@ specifies a read\/write volume. @RW@ is the default.
    --
    -- -   @DP@ specifies a data-protection volume. A @DP@ volume is read-only
    --     and can be used as the destination of a NetApp SnapMirror
    --     relationship.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/volume-types Volume types>
    -- in the /Amazon FSx for NetApp ONTAP User Guide/.
    ontapVolumeType :: Prelude.Maybe InputOntapVolumeType,
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
    -- | Specifies the snapshot policy for the volume. There are three built-in
    -- snapshot policies:
    --
    -- -   @default@: This is the default policy. A maximum of six hourly
    --     snapshots taken five minutes past the hour. A maximum of two daily
    --     snapshots taken Monday through Saturday at 10 minutes after
    --     midnight. A maximum of two weekly snapshots taken every Sunday at 15
    --     minutes after midnight.
    --
    -- -   @default-1weekly@: This policy is the same as the @default@ policy
    --     except that it only retains one snapshot from the weekly schedule.
    --
    -- -   @none@: This policy does not take any snapshots. This policy can be
    --     assigned to volumes to prevent automatic snapshots from being taken.
    --
    -- You can also provide the name of a custom policy that you created with
    -- the ONTAP CLI or REST API.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/snapshots-ontap.html#snapshot-policies Snapshot policies>
    -- in the /Amazon FSx for NetApp ONTAP User Guide/.
    snapshotPolicy :: Prelude.Maybe Prelude.Text,
    -- | Set to true to enable deduplication, compression, and compaction storage
    -- efficiency features on the volume.
    storageEfficiencyEnabled :: Prelude.Maybe Prelude.Bool,
    tieringPolicy :: Prelude.Maybe TieringPolicy,
    -- | Specifies the size of the volume, in megabytes (MB), that you are
    -- creating.
    sizeInMegabytes :: Prelude.Natural,
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
-- 'copyTagsToBackups', 'createOntapVolumeConfiguration_copyTagsToBackups' - A boolean flag indicating whether tags for the volume should be copied
-- to backups. This value defaults to false. If it\'s set to true, all tags
-- for the volume are copied to all automatic and user-initiated backups
-- where the user doesn\'t specify tags. If this value is true, and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the volume, regardless of this value.
--
-- 'junctionPath', 'createOntapVolumeConfiguration_junctionPath' - Specifies the location in the SVM\'s namespace where the volume is
-- mounted. The @JunctionPath@ must have a leading forward slash, such as
-- @\/vol3@.
--
-- 'ontapVolumeType', 'createOntapVolumeConfiguration_ontapVolumeType' - Specifies the type of volume you are creating. Valid values are the
-- following:
--
-- -   @RW@ specifies a read\/write volume. @RW@ is the default.
--
-- -   @DP@ specifies a data-protection volume. A @DP@ volume is read-only
--     and can be used as the destination of a NetApp SnapMirror
--     relationship.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/volume-types Volume types>
-- in the /Amazon FSx for NetApp ONTAP User Guide/.
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
-- 'snapshotPolicy', 'createOntapVolumeConfiguration_snapshotPolicy' - Specifies the snapshot policy for the volume. There are three built-in
-- snapshot policies:
--
-- -   @default@: This is the default policy. A maximum of six hourly
--     snapshots taken five minutes past the hour. A maximum of two daily
--     snapshots taken Monday through Saturday at 10 minutes after
--     midnight. A maximum of two weekly snapshots taken every Sunday at 15
--     minutes after midnight.
--
-- -   @default-1weekly@: This policy is the same as the @default@ policy
--     except that it only retains one snapshot from the weekly schedule.
--
-- -   @none@: This policy does not take any snapshots. This policy can be
--     assigned to volumes to prevent automatic snapshots from being taken.
--
-- You can also provide the name of a custom policy that you created with
-- the ONTAP CLI or REST API.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/snapshots-ontap.html#snapshot-policies Snapshot policies>
-- in the /Amazon FSx for NetApp ONTAP User Guide/.
--
-- 'storageEfficiencyEnabled', 'createOntapVolumeConfiguration_storageEfficiencyEnabled' - Set to true to enable deduplication, compression, and compaction storage
-- efficiency features on the volume.
--
-- 'tieringPolicy', 'createOntapVolumeConfiguration_tieringPolicy' - Undocumented member.
--
-- 'sizeInMegabytes', 'createOntapVolumeConfiguration_sizeInMegabytes' - Specifies the size of the volume, in megabytes (MB), that you are
-- creating.
--
-- 'storageVirtualMachineId', 'createOntapVolumeConfiguration_storageVirtualMachineId' - Specifies the ONTAP SVM in which to create the volume.
newCreateOntapVolumeConfiguration ::
  -- | 'sizeInMegabytes'
  Prelude.Natural ->
  -- | 'storageVirtualMachineId'
  Prelude.Text ->
  CreateOntapVolumeConfiguration
newCreateOntapVolumeConfiguration
  pSizeInMegabytes_
  pStorageVirtualMachineId_ =
    CreateOntapVolumeConfiguration'
      { copyTagsToBackups =
          Prelude.Nothing,
        junctionPath = Prelude.Nothing,
        ontapVolumeType = Prelude.Nothing,
        securityStyle = Prelude.Nothing,
        snapshotPolicy = Prelude.Nothing,
        storageEfficiencyEnabled = Prelude.Nothing,
        tieringPolicy = Prelude.Nothing,
        sizeInMegabytes = pSizeInMegabytes_,
        storageVirtualMachineId =
          pStorageVirtualMachineId_
      }

-- | A boolean flag indicating whether tags for the volume should be copied
-- to backups. This value defaults to false. If it\'s set to true, all tags
-- for the volume are copied to all automatic and user-initiated backups
-- where the user doesn\'t specify tags. If this value is true, and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the volume, regardless of this value.
createOntapVolumeConfiguration_copyTagsToBackups :: Lens.Lens' CreateOntapVolumeConfiguration (Prelude.Maybe Prelude.Bool)
createOntapVolumeConfiguration_copyTagsToBackups = Lens.lens (\CreateOntapVolumeConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@CreateOntapVolumeConfiguration' {} a -> s {copyTagsToBackups = a} :: CreateOntapVolumeConfiguration)

-- | Specifies the location in the SVM\'s namespace where the volume is
-- mounted. The @JunctionPath@ must have a leading forward slash, such as
-- @\/vol3@.
createOntapVolumeConfiguration_junctionPath :: Lens.Lens' CreateOntapVolumeConfiguration (Prelude.Maybe Prelude.Text)
createOntapVolumeConfiguration_junctionPath = Lens.lens (\CreateOntapVolumeConfiguration' {junctionPath} -> junctionPath) (\s@CreateOntapVolumeConfiguration' {} a -> s {junctionPath = a} :: CreateOntapVolumeConfiguration)

-- | Specifies the type of volume you are creating. Valid values are the
-- following:
--
-- -   @RW@ specifies a read\/write volume. @RW@ is the default.
--
-- -   @DP@ specifies a data-protection volume. A @DP@ volume is read-only
--     and can be used as the destination of a NetApp SnapMirror
--     relationship.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/volume-types Volume types>
-- in the /Amazon FSx for NetApp ONTAP User Guide/.
createOntapVolumeConfiguration_ontapVolumeType :: Lens.Lens' CreateOntapVolumeConfiguration (Prelude.Maybe InputOntapVolumeType)
createOntapVolumeConfiguration_ontapVolumeType = Lens.lens (\CreateOntapVolumeConfiguration' {ontapVolumeType} -> ontapVolumeType) (\s@CreateOntapVolumeConfiguration' {} a -> s {ontapVolumeType = a} :: CreateOntapVolumeConfiguration)

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

-- | Specifies the snapshot policy for the volume. There are three built-in
-- snapshot policies:
--
-- -   @default@: This is the default policy. A maximum of six hourly
--     snapshots taken five minutes past the hour. A maximum of two daily
--     snapshots taken Monday through Saturday at 10 minutes after
--     midnight. A maximum of two weekly snapshots taken every Sunday at 15
--     minutes after midnight.
--
-- -   @default-1weekly@: This policy is the same as the @default@ policy
--     except that it only retains one snapshot from the weekly schedule.
--
-- -   @none@: This policy does not take any snapshots. This policy can be
--     assigned to volumes to prevent automatic snapshots from being taken.
--
-- You can also provide the name of a custom policy that you created with
-- the ONTAP CLI or REST API.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/snapshots-ontap.html#snapshot-policies Snapshot policies>
-- in the /Amazon FSx for NetApp ONTAP User Guide/.
createOntapVolumeConfiguration_snapshotPolicy :: Lens.Lens' CreateOntapVolumeConfiguration (Prelude.Maybe Prelude.Text)
createOntapVolumeConfiguration_snapshotPolicy = Lens.lens (\CreateOntapVolumeConfiguration' {snapshotPolicy} -> snapshotPolicy) (\s@CreateOntapVolumeConfiguration' {} a -> s {snapshotPolicy = a} :: CreateOntapVolumeConfiguration)

-- | Set to true to enable deduplication, compression, and compaction storage
-- efficiency features on the volume.
createOntapVolumeConfiguration_storageEfficiencyEnabled :: Lens.Lens' CreateOntapVolumeConfiguration (Prelude.Maybe Prelude.Bool)
createOntapVolumeConfiguration_storageEfficiencyEnabled = Lens.lens (\CreateOntapVolumeConfiguration' {storageEfficiencyEnabled} -> storageEfficiencyEnabled) (\s@CreateOntapVolumeConfiguration' {} a -> s {storageEfficiencyEnabled = a} :: CreateOntapVolumeConfiguration)

-- | Undocumented member.
createOntapVolumeConfiguration_tieringPolicy :: Lens.Lens' CreateOntapVolumeConfiguration (Prelude.Maybe TieringPolicy)
createOntapVolumeConfiguration_tieringPolicy = Lens.lens (\CreateOntapVolumeConfiguration' {tieringPolicy} -> tieringPolicy) (\s@CreateOntapVolumeConfiguration' {} a -> s {tieringPolicy = a} :: CreateOntapVolumeConfiguration)

-- | Specifies the size of the volume, in megabytes (MB), that you are
-- creating.
createOntapVolumeConfiguration_sizeInMegabytes :: Lens.Lens' CreateOntapVolumeConfiguration Prelude.Natural
createOntapVolumeConfiguration_sizeInMegabytes = Lens.lens (\CreateOntapVolumeConfiguration' {sizeInMegabytes} -> sizeInMegabytes) (\s@CreateOntapVolumeConfiguration' {} a -> s {sizeInMegabytes = a} :: CreateOntapVolumeConfiguration)

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
      _salt
        `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` junctionPath
        `Prelude.hashWithSalt` ontapVolumeType
        `Prelude.hashWithSalt` securityStyle
        `Prelude.hashWithSalt` snapshotPolicy
        `Prelude.hashWithSalt` storageEfficiencyEnabled
        `Prelude.hashWithSalt` tieringPolicy
        `Prelude.hashWithSalt` sizeInMegabytes
        `Prelude.hashWithSalt` storageVirtualMachineId

instance
  Prelude.NFData
    CreateOntapVolumeConfiguration
  where
  rnf CreateOntapVolumeConfiguration' {..} =
    Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf junctionPath
      `Prelude.seq` Prelude.rnf ontapVolumeType
      `Prelude.seq` Prelude.rnf securityStyle
      `Prelude.seq` Prelude.rnf snapshotPolicy
      `Prelude.seq` Prelude.rnf storageEfficiencyEnabled
      `Prelude.seq` Prelude.rnf tieringPolicy
      `Prelude.seq` Prelude.rnf sizeInMegabytes
      `Prelude.seq` Prelude.rnf storageVirtualMachineId

instance Data.ToJSON CreateOntapVolumeConfiguration where
  toJSON CreateOntapVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CopyTagsToBackups" Data..=)
              Prelude.<$> copyTagsToBackups,
            ("JunctionPath" Data..=) Prelude.<$> junctionPath,
            ("OntapVolumeType" Data..=)
              Prelude.<$> ontapVolumeType,
            ("SecurityStyle" Data..=) Prelude.<$> securityStyle,
            ("SnapshotPolicy" Data..=)
              Prelude.<$> snapshotPolicy,
            ("StorageEfficiencyEnabled" Data..=)
              Prelude.<$> storageEfficiencyEnabled,
            ("TieringPolicy" Data..=) Prelude.<$> tieringPolicy,
            Prelude.Just
              ("SizeInMegabytes" Data..= sizeInMegabytes),
            Prelude.Just
              ( "StorageVirtualMachineId"
                  Data..= storageVirtualMachineId
              )
          ]
      )
