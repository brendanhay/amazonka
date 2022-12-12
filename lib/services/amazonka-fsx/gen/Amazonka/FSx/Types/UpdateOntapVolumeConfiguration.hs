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
-- Module      : Amazonka.FSx.Types.UpdateOntapVolumeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.UpdateOntapVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.SecurityStyle
import Amazonka.FSx.Types.TieringPolicy
import qualified Amazonka.Prelude as Prelude

-- | Used to specify changes to the ONTAP configuration for the volume you
-- are updating.
--
-- /See:/ 'newUpdateOntapVolumeConfiguration' smart constructor.
data UpdateOntapVolumeConfiguration = UpdateOntapVolumeConfiguration'
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
    -- | The security style for the volume, which can be @UNIX@. @NTFS@, or
    -- @MIXED@.
    securityStyle :: Prelude.Maybe SecurityStyle,
    -- | Specifies the size of the volume in megabytes.
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
    -- | Default is @false@. Set to true to enable the deduplication,
    -- compression, and compaction storage efficiency features on the volume.
    storageEfficiencyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Update the volume\'s data tiering policy.
    tieringPolicy :: Prelude.Maybe TieringPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOntapVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyTagsToBackups', 'updateOntapVolumeConfiguration_copyTagsToBackups' - A boolean flag indicating whether tags for the volume should be copied
-- to backups. This value defaults to false. If it\'s set to true, all tags
-- for the volume are copied to all automatic and user-initiated backups
-- where the user doesn\'t specify tags. If this value is true, and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the volume, regardless of this value.
--
-- 'junctionPath', 'updateOntapVolumeConfiguration_junctionPath' - Specifies the location in the SVM\'s namespace where the volume is
-- mounted. The @JunctionPath@ must have a leading forward slash, such as
-- @\/vol3@.
--
-- 'securityStyle', 'updateOntapVolumeConfiguration_securityStyle' - The security style for the volume, which can be @UNIX@. @NTFS@, or
-- @MIXED@.
--
-- 'sizeInMegabytes', 'updateOntapVolumeConfiguration_sizeInMegabytes' - Specifies the size of the volume in megabytes.
--
-- 'snapshotPolicy', 'updateOntapVolumeConfiguration_snapshotPolicy' - Specifies the snapshot policy for the volume. There are three built-in
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
-- 'storageEfficiencyEnabled', 'updateOntapVolumeConfiguration_storageEfficiencyEnabled' - Default is @false@. Set to true to enable the deduplication,
-- compression, and compaction storage efficiency features on the volume.
--
-- 'tieringPolicy', 'updateOntapVolumeConfiguration_tieringPolicy' - Update the volume\'s data tiering policy.
newUpdateOntapVolumeConfiguration ::
  UpdateOntapVolumeConfiguration
newUpdateOntapVolumeConfiguration =
  UpdateOntapVolumeConfiguration'
    { copyTagsToBackups =
        Prelude.Nothing,
      junctionPath = Prelude.Nothing,
      securityStyle = Prelude.Nothing,
      sizeInMegabytes = Prelude.Nothing,
      snapshotPolicy = Prelude.Nothing,
      storageEfficiencyEnabled = Prelude.Nothing,
      tieringPolicy = Prelude.Nothing
    }

-- | A boolean flag indicating whether tags for the volume should be copied
-- to backups. This value defaults to false. If it\'s set to true, all tags
-- for the volume are copied to all automatic and user-initiated backups
-- where the user doesn\'t specify tags. If this value is true, and you
-- specify one or more tags, only the specified tags are copied to backups.
-- If you specify one or more tags when creating a user-initiated backup,
-- no tags are copied from the volume, regardless of this value.
updateOntapVolumeConfiguration_copyTagsToBackups :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe Prelude.Bool)
updateOntapVolumeConfiguration_copyTagsToBackups = Lens.lens (\UpdateOntapVolumeConfiguration' {copyTagsToBackups} -> copyTagsToBackups) (\s@UpdateOntapVolumeConfiguration' {} a -> s {copyTagsToBackups = a} :: UpdateOntapVolumeConfiguration)

-- | Specifies the location in the SVM\'s namespace where the volume is
-- mounted. The @JunctionPath@ must have a leading forward slash, such as
-- @\/vol3@.
updateOntapVolumeConfiguration_junctionPath :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe Prelude.Text)
updateOntapVolumeConfiguration_junctionPath = Lens.lens (\UpdateOntapVolumeConfiguration' {junctionPath} -> junctionPath) (\s@UpdateOntapVolumeConfiguration' {} a -> s {junctionPath = a} :: UpdateOntapVolumeConfiguration)

-- | The security style for the volume, which can be @UNIX@. @NTFS@, or
-- @MIXED@.
updateOntapVolumeConfiguration_securityStyle :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe SecurityStyle)
updateOntapVolumeConfiguration_securityStyle = Lens.lens (\UpdateOntapVolumeConfiguration' {securityStyle} -> securityStyle) (\s@UpdateOntapVolumeConfiguration' {} a -> s {securityStyle = a} :: UpdateOntapVolumeConfiguration)

-- | Specifies the size of the volume in megabytes.
updateOntapVolumeConfiguration_sizeInMegabytes :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe Prelude.Natural)
updateOntapVolumeConfiguration_sizeInMegabytes = Lens.lens (\UpdateOntapVolumeConfiguration' {sizeInMegabytes} -> sizeInMegabytes) (\s@UpdateOntapVolumeConfiguration' {} a -> s {sizeInMegabytes = a} :: UpdateOntapVolumeConfiguration)

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
updateOntapVolumeConfiguration_snapshotPolicy :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe Prelude.Text)
updateOntapVolumeConfiguration_snapshotPolicy = Lens.lens (\UpdateOntapVolumeConfiguration' {snapshotPolicy} -> snapshotPolicy) (\s@UpdateOntapVolumeConfiguration' {} a -> s {snapshotPolicy = a} :: UpdateOntapVolumeConfiguration)

-- | Default is @false@. Set to true to enable the deduplication,
-- compression, and compaction storage efficiency features on the volume.
updateOntapVolumeConfiguration_storageEfficiencyEnabled :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe Prelude.Bool)
updateOntapVolumeConfiguration_storageEfficiencyEnabled = Lens.lens (\UpdateOntapVolumeConfiguration' {storageEfficiencyEnabled} -> storageEfficiencyEnabled) (\s@UpdateOntapVolumeConfiguration' {} a -> s {storageEfficiencyEnabled = a} :: UpdateOntapVolumeConfiguration)

-- | Update the volume\'s data tiering policy.
updateOntapVolumeConfiguration_tieringPolicy :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe TieringPolicy)
updateOntapVolumeConfiguration_tieringPolicy = Lens.lens (\UpdateOntapVolumeConfiguration' {tieringPolicy} -> tieringPolicy) (\s@UpdateOntapVolumeConfiguration' {} a -> s {tieringPolicy = a} :: UpdateOntapVolumeConfiguration)

instance
  Prelude.Hashable
    UpdateOntapVolumeConfiguration
  where
  hashWithSalt
    _salt
    UpdateOntapVolumeConfiguration' {..} =
      _salt `Prelude.hashWithSalt` copyTagsToBackups
        `Prelude.hashWithSalt` junctionPath
        `Prelude.hashWithSalt` securityStyle
        `Prelude.hashWithSalt` sizeInMegabytes
        `Prelude.hashWithSalt` snapshotPolicy
        `Prelude.hashWithSalt` storageEfficiencyEnabled
        `Prelude.hashWithSalt` tieringPolicy

instance
  Prelude.NFData
    UpdateOntapVolumeConfiguration
  where
  rnf UpdateOntapVolumeConfiguration' {..} =
    Prelude.rnf copyTagsToBackups
      `Prelude.seq` Prelude.rnf junctionPath
      `Prelude.seq` Prelude.rnf securityStyle
      `Prelude.seq` Prelude.rnf sizeInMegabytes
      `Prelude.seq` Prelude.rnf snapshotPolicy
      `Prelude.seq` Prelude.rnf storageEfficiencyEnabled
      `Prelude.seq` Prelude.rnf tieringPolicy

instance Data.ToJSON UpdateOntapVolumeConfiguration where
  toJSON UpdateOntapVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CopyTagsToBackups" Data..=)
              Prelude.<$> copyTagsToBackups,
            ("JunctionPath" Data..=) Prelude.<$> junctionPath,
            ("SecurityStyle" Data..=) Prelude.<$> securityStyle,
            ("SizeInMegabytes" Data..=)
              Prelude.<$> sizeInMegabytes,
            ("SnapshotPolicy" Data..=)
              Prelude.<$> snapshotPolicy,
            ("StorageEfficiencyEnabled" Data..=)
              Prelude.<$> storageEfficiencyEnabled,
            ("TieringPolicy" Data..=) Prelude.<$> tieringPolicy
          ]
      )
