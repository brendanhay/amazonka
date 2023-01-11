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
-- Module      : Amazonka.FSx.Types.OntapVolumeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OntapVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.FlexCacheEndpointType
import Amazonka.FSx.Types.OntapVolumeType
import Amazonka.FSx.Types.SecurityStyle
import Amazonka.FSx.Types.TieringPolicy
import qualified Amazonka.Prelude as Prelude

-- | The configuration of an Amazon FSx for NetApp ONTAP volume.
--
-- /See:/ 'newOntapVolumeConfiguration' smart constructor.
data OntapVolumeConfiguration = OntapVolumeConfiguration'
  { -- | A boolean flag indicating whether tags for the volume should be copied
    -- to backups. This value defaults to false. If it\'s set to true, all tags
    -- for the volume are copied to all automatic and user-initiated backups
    -- where the user doesn\'t specify tags. If this value is true, and you
    -- specify one or more tags, only the specified tags are copied to backups.
    -- If you specify one or more tags when creating a user-initiated backup,
    -- no tags are copied from the volume, regardless of this value.
    copyTagsToBackups :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the FlexCache endpoint type of the volume. Valid values are
    -- the following:
    --
    -- -   @NONE@ specifies that the volume doesn\'t have a FlexCache
    --     configuration. @NONE@ is the default.
    --
    -- -   @ORIGIN@ specifies that the volume is the origin volume for a
    --     FlexCache volume.
    --
    -- -   @CACHE@ specifies that the volume is a FlexCache volume.
    flexCacheEndpointType :: Prelude.Maybe FlexCacheEndpointType,
    -- | Specifies the directory that network-attached storage (NAS) clients use
    -- to mount the volume, along with the storage virtual machine (SVM) Domain
    -- Name System (DNS) name or IP address. You can create a @JunctionPath@
    -- directly below a parent volume junction or on a directory within a
    -- volume. A @JunctionPath@ for a volume named @vol3@ might be
    -- @\/vol1\/vol2\/vol3@, or @\/vol1\/dir2\/vol3@, or even
    -- @\/dir1\/dir2\/vol3@.
    junctionPath :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of volume. Valid values are the following:
    --
    -- -   @RW@ specifies a read\/write volume. @RW@ is the default.
    --
    -- -   @DP@ specifies a data-protection volume. You can protect data by
    --     replicating it to data-protection mirror copies. If a disaster
    --     occurs, you can use these data-protection mirror copies to recover
    --     data.
    --
    -- -   @LS@ specifies a load-sharing mirror volume. A load-sharing mirror
    --     reduces the network traffic to a FlexVol volume by providing
    --     additional read-only access to clients.
    ontapVolumeType :: Prelude.Maybe OntapVolumeType,
    -- | The security style for the volume, which can be @UNIX@, @NTFS@, or
    -- @MIXED@.
    securityStyle :: Prelude.Maybe SecurityStyle,
    -- | The configured size of the volume, in megabytes (MBs).
    sizeInMegabytes :: Prelude.Maybe Prelude.Natural,
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
    -- | The volume\'s storage efficiency setting.
    storageEfficiencyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the volume\'s storage virtual machine.
    storageVirtualMachineId :: Prelude.Maybe Prelude.Text,
    -- | A Boolean flag indicating whether this volume is the root volume for its
    -- storage virtual machine (SVM). Only one volume on an SVM can be the root
    -- volume. This value defaults to @false@. If this value is @true@, then
    -- this is the SVM root volume.
    --
    -- This flag is useful when you\'re deleting an SVM, because you must first
    -- delete all non-root volumes. This flag, when set to @false@, helps you
    -- identify which volumes to delete before you can delete the SVM.
    storageVirtualMachineRoot :: Prelude.Maybe Prelude.Bool,
    -- | The volume\'s @TieringPolicy@ setting.
    tieringPolicy :: Prelude.Maybe TieringPolicy,
    -- | The volume\'s universally unique identifier (UUID).
    uuid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OntapVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyTagsToBackups', 'ontapVolumeConfiguration_copyTagsToBackups' - A boolean flag indicating whether tags for the volume should be copied
-- to backups. This value defaults to false. If it\'s set to true, all tags
-- for the volume are copied to all automatic and user-initiated backups
-- where the user doesn\'t specify tags. If this value is true, and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the volume, regardless of this value.
--
-- 'flexCacheEndpointType', 'ontapVolumeConfiguration_flexCacheEndpointType' - Specifies the FlexCache endpoint type of the volume. Valid values are
-- the following:
--
-- -   @NONE@ specifies that the volume doesn\'t have a FlexCache
--     configuration. @NONE@ is the default.
--
-- -   @ORIGIN@ specifies that the volume is the origin volume for a
--     FlexCache volume.
--
-- -   @CACHE@ specifies that the volume is a FlexCache volume.
--
-- 'junctionPath', 'ontapVolumeConfiguration_junctionPath' - Specifies the directory that network-attached storage (NAS) clients use
-- to mount the volume, along with the storage virtual machine (SVM) Domain
-- Name System (DNS) name or IP address. You can create a @JunctionPath@
-- directly below a parent volume junction or on a directory within a
-- volume. A @JunctionPath@ for a volume named @vol3@ might be
-- @\/vol1\/vol2\/vol3@, or @\/vol1\/dir2\/vol3@, or even
-- @\/dir1\/dir2\/vol3@.
--
-- 'ontapVolumeType', 'ontapVolumeConfiguration_ontapVolumeType' - Specifies the type of volume. Valid values are the following:
--
-- -   @RW@ specifies a read\/write volume. @RW@ is the default.
--
-- -   @DP@ specifies a data-protection volume. You can protect data by
--     replicating it to data-protection mirror copies. If a disaster
--     occurs, you can use these data-protection mirror copies to recover
--     data.
--
-- -   @LS@ specifies a load-sharing mirror volume. A load-sharing mirror
--     reduces the network traffic to a FlexVol volume by providing
--     additional read-only access to clients.
--
-- 'securityStyle', 'ontapVolumeConfiguration_securityStyle' - The security style for the volume, which can be @UNIX@, @NTFS@, or
-- @MIXED@.
--
-- 'sizeInMegabytes', 'ontapVolumeConfiguration_sizeInMegabytes' - The configured size of the volume, in megabytes (MBs).
--
-- 'snapshotPolicy', 'ontapVolumeConfiguration_snapshotPolicy' - Specifies the snapshot policy for the volume. There are three built-in
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
-- 'storageEfficiencyEnabled', 'ontapVolumeConfiguration_storageEfficiencyEnabled' - The volume\'s storage efficiency setting.
--
-- 'storageVirtualMachineId', 'ontapVolumeConfiguration_storageVirtualMachineId' - The ID of the volume\'s storage virtual machine.
--
-- 'storageVirtualMachineRoot', 'ontapVolumeConfiguration_storageVirtualMachineRoot' - A Boolean flag indicating whether this volume is the root volume for its
-- storage virtual machine (SVM). Only one volume on an SVM can be the root
-- volume. This value defaults to @false@. If this value is @true@, then
-- this is the SVM root volume.
--
-- This flag is useful when you\'re deleting an SVM, because you must first
-- delete all non-root volumes. This flag, when set to @false@, helps you
-- identify which volumes to delete before you can delete the SVM.
--
-- 'tieringPolicy', 'ontapVolumeConfiguration_tieringPolicy' - The volume\'s @TieringPolicy@ setting.
--
-- 'uuid', 'ontapVolumeConfiguration_uuid' - The volume\'s universally unique identifier (UUID).
newOntapVolumeConfiguration ::
  OntapVolumeConfiguration
newOntapVolumeConfiguration =
  OntapVolumeConfiguration'
    { copyTagsToBackups =
        Prelude.Nothing,
      flexCacheEndpointType = Prelude.Nothing,
      junctionPath = Prelude.Nothing,
      ontapVolumeType = Prelude.Nothing,
      securityStyle = Prelude.Nothing,
      sizeInMegabytes = Prelude.Nothing,
      snapshotPolicy = Prelude.Nothing,
      storageEfficiencyEnabled = Prelude.Nothing,
      storageVirtualMachineId = Prelude.Nothing,
      storageVirtualMachineRoot = Prelude.Nothing,
      tieringPolicy = Prelude.Nothing,
      uuid = Prelude.Nothing
    }

-- | A boolean flag indicating whether tags for the volume should be copied
-- to backups. This value defaults to false. If it\'s set to true, all tags
-- for the volume are copied to all automatic and user-initiated backups
-- where the user doesn\'t specify tags. If this value is true, and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the volume, regardless of this value.
ontapVolumeConfiguration_copyTagsToBackups :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe Prelude.Bool)
ontapVolumeConfiguration_copyTagsToBackups = Lens.lens (\OntapVolumeConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@OntapVolumeConfiguration' {} a -> s {copyTagsToBackups = a} :: OntapVolumeConfiguration)

-- | Specifies the FlexCache endpoint type of the volume. Valid values are
-- the following:
--
-- -   @NONE@ specifies that the volume doesn\'t have a FlexCache
--     configuration. @NONE@ is the default.
--
-- -   @ORIGIN@ specifies that the volume is the origin volume for a
--     FlexCache volume.
--
-- -   @CACHE@ specifies that the volume is a FlexCache volume.
ontapVolumeConfiguration_flexCacheEndpointType :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe FlexCacheEndpointType)
ontapVolumeConfiguration_flexCacheEndpointType = Lens.lens (\OntapVolumeConfiguration' {flexCacheEndpointType} -> flexCacheEndpointType) (\s@OntapVolumeConfiguration' {} a -> s {flexCacheEndpointType = a} :: OntapVolumeConfiguration)

-- | Specifies the directory that network-attached storage (NAS) clients use
-- to mount the volume, along with the storage virtual machine (SVM) Domain
-- Name System (DNS) name or IP address. You can create a @JunctionPath@
-- directly below a parent volume junction or on a directory within a
-- volume. A @JunctionPath@ for a volume named @vol3@ might be
-- @\/vol1\/vol2\/vol3@, or @\/vol1\/dir2\/vol3@, or even
-- @\/dir1\/dir2\/vol3@.
ontapVolumeConfiguration_junctionPath :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe Prelude.Text)
ontapVolumeConfiguration_junctionPath = Lens.lens (\OntapVolumeConfiguration' {junctionPath} -> junctionPath) (\s@OntapVolumeConfiguration' {} a -> s {junctionPath = a} :: OntapVolumeConfiguration)

-- | Specifies the type of volume. Valid values are the following:
--
-- -   @RW@ specifies a read\/write volume. @RW@ is the default.
--
-- -   @DP@ specifies a data-protection volume. You can protect data by
--     replicating it to data-protection mirror copies. If a disaster
--     occurs, you can use these data-protection mirror copies to recover
--     data.
--
-- -   @LS@ specifies a load-sharing mirror volume. A load-sharing mirror
--     reduces the network traffic to a FlexVol volume by providing
--     additional read-only access to clients.
ontapVolumeConfiguration_ontapVolumeType :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe OntapVolumeType)
ontapVolumeConfiguration_ontapVolumeType = Lens.lens (\OntapVolumeConfiguration' {ontapVolumeType} -> ontapVolumeType) (\s@OntapVolumeConfiguration' {} a -> s {ontapVolumeType = a} :: OntapVolumeConfiguration)

-- | The security style for the volume, which can be @UNIX@, @NTFS@, or
-- @MIXED@.
ontapVolumeConfiguration_securityStyle :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe SecurityStyle)
ontapVolumeConfiguration_securityStyle = Lens.lens (\OntapVolumeConfiguration' {securityStyle} -> securityStyle) (\s@OntapVolumeConfiguration' {} a -> s {securityStyle = a} :: OntapVolumeConfiguration)

-- | The configured size of the volume, in megabytes (MBs).
ontapVolumeConfiguration_sizeInMegabytes :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe Prelude.Natural)
ontapVolumeConfiguration_sizeInMegabytes = Lens.lens (\OntapVolumeConfiguration' {sizeInMegabytes} -> sizeInMegabytes) (\s@OntapVolumeConfiguration' {} a -> s {sizeInMegabytes = a} :: OntapVolumeConfiguration)

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
ontapVolumeConfiguration_snapshotPolicy :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe Prelude.Text)
ontapVolumeConfiguration_snapshotPolicy = Lens.lens (\OntapVolumeConfiguration' {snapshotPolicy} -> snapshotPolicy) (\s@OntapVolumeConfiguration' {} a -> s {snapshotPolicy = a} :: OntapVolumeConfiguration)

-- | The volume\'s storage efficiency setting.
ontapVolumeConfiguration_storageEfficiencyEnabled :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe Prelude.Bool)
ontapVolumeConfiguration_storageEfficiencyEnabled = Lens.lens (\OntapVolumeConfiguration' {storageEfficiencyEnabled} -> storageEfficiencyEnabled) (\s@OntapVolumeConfiguration' {} a -> s {storageEfficiencyEnabled = a} :: OntapVolumeConfiguration)

-- | The ID of the volume\'s storage virtual machine.
ontapVolumeConfiguration_storageVirtualMachineId :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe Prelude.Text)
ontapVolumeConfiguration_storageVirtualMachineId = Lens.lens (\OntapVolumeConfiguration' {storageVirtualMachineId} -> storageVirtualMachineId) (\s@OntapVolumeConfiguration' {} a -> s {storageVirtualMachineId = a} :: OntapVolumeConfiguration)

-- | A Boolean flag indicating whether this volume is the root volume for its
-- storage virtual machine (SVM). Only one volume on an SVM can be the root
-- volume. This value defaults to @false@. If this value is @true@, then
-- this is the SVM root volume.
--
-- This flag is useful when you\'re deleting an SVM, because you must first
-- delete all non-root volumes. This flag, when set to @false@, helps you
-- identify which volumes to delete before you can delete the SVM.
ontapVolumeConfiguration_storageVirtualMachineRoot :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe Prelude.Bool)
ontapVolumeConfiguration_storageVirtualMachineRoot = Lens.lens (\OntapVolumeConfiguration' {storageVirtualMachineRoot} -> storageVirtualMachineRoot) (\s@OntapVolumeConfiguration' {} a -> s {storageVirtualMachineRoot = a} :: OntapVolumeConfiguration)

-- | The volume\'s @TieringPolicy@ setting.
ontapVolumeConfiguration_tieringPolicy :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe TieringPolicy)
ontapVolumeConfiguration_tieringPolicy = Lens.lens (\OntapVolumeConfiguration' {tieringPolicy} -> tieringPolicy) (\s@OntapVolumeConfiguration' {} a -> s {tieringPolicy = a} :: OntapVolumeConfiguration)

-- | The volume\'s universally unique identifier (UUID).
ontapVolumeConfiguration_uuid :: Lens.Lens' OntapVolumeConfiguration (Prelude.Maybe Prelude.Text)
ontapVolumeConfiguration_uuid = Lens.lens (\OntapVolumeConfiguration' {uuid} -> uuid) (\s@OntapVolumeConfiguration' {} a -> s {uuid = a} :: OntapVolumeConfiguration)

instance Data.FromJSON OntapVolumeConfiguration where
  parseJSON =
    Data.withObject
      "OntapVolumeConfiguration"
      ( \x ->
          OntapVolumeConfiguration'
            Prelude.<$> (x Data..:? "CopyTagsToBackups")
            Prelude.<*> (x Data..:? "FlexCacheEndpointType")
            Prelude.<*> (x Data..:? "JunctionPath")
            Prelude.<*> (x Data..:? "OntapVolumeType")
            Prelude.<*> (x Data..:? "SecurityStyle")
            Prelude.<*> (x Data..:? "SizeInMegabytes")
            Prelude.<*> (x Data..:? "SnapshotPolicy")
            Prelude.<*> (x Data..:? "StorageEfficiencyEnabled")
            Prelude.<*> (x Data..:? "StorageVirtualMachineId")
            Prelude.<*> (x Data..:? "StorageVirtualMachineRoot")
            Prelude.<*> (x Data..:? "TieringPolicy")
            Prelude.<*> (x Data..:? "UUID")
      )

instance Prelude.Hashable OntapVolumeConfiguration where
  hashWithSalt _salt OntapVolumeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` copyTagsToBackups
      `Prelude.hashWithSalt` flexCacheEndpointType
      `Prelude.hashWithSalt` junctionPath
      `Prelude.hashWithSalt` ontapVolumeType
      `Prelude.hashWithSalt` securityStyle
      `Prelude.hashWithSalt` sizeInMegabytes
      `Prelude.hashWithSalt` snapshotPolicy
      `Prelude.hashWithSalt` storageEfficiencyEnabled
      `Prelude.hashWithSalt` storageVirtualMachineId
      `Prelude.hashWithSalt` storageVirtualMachineRoot
      `Prelude.hashWithSalt` tieringPolicy
      `Prelude.hashWithSalt` uuid

instance Prelude.NFData OntapVolumeConfiguration where
  rnf OntapVolumeConfiguration' {..} =
    Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf flexCacheEndpointType
      `Prelude.seq` Prelude.rnf junctionPath
      `Prelude.seq` Prelude.rnf ontapVolumeType
      `Prelude.seq` Prelude.rnf securityStyle
      `Prelude.seq` Prelude.rnf sizeInMegabytes
      `Prelude.seq` Prelude.rnf snapshotPolicy
      `Prelude.seq` Prelude.rnf storageEfficiencyEnabled
      `Prelude.seq` Prelude.rnf storageVirtualMachineId
      `Prelude.seq` Prelude.rnf storageVirtualMachineRoot
      `Prelude.seq` Prelude.rnf tieringPolicy
      `Prelude.seq` Prelude.rnf uuid
