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
import Amazonka.FSx.Types.SecurityStyle
import Amazonka.FSx.Types.TieringPolicy
import qualified Amazonka.Prelude as Prelude

-- | Used to specify changes to the ONTAP configuration for the volume you
-- are updating.
--
-- /See:/ 'newUpdateOntapVolumeConfiguration' smart constructor.
data UpdateOntapVolumeConfiguration = UpdateOntapVolumeConfiguration'
  { -- | Default is @false@. Set to true to enable the deduplication,
    -- compression, and compaction storage efficiency features on the volume.
    storageEfficiencyEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Update the volume\'s data tiering policy.
    tieringPolicy :: Prelude.Maybe TieringPolicy,
    -- | The security style for the volume, which can be @UNIX@. @NTFS@, or
    -- @MIXED@.
    securityStyle :: Prelude.Maybe SecurityStyle,
    -- | Specifies the location in the SVM\'s namespace where the volume is
    -- mounted. The @JunctionPath@ must have a leading forward slash, such as
    -- @\/vol3@.
    junctionPath :: Prelude.Maybe Prelude.Text,
    -- | Specifies the size of the volume in megabytes.
    sizeInMegabytes :: Prelude.Maybe Prelude.Natural
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
-- 'storageEfficiencyEnabled', 'updateOntapVolumeConfiguration_storageEfficiencyEnabled' - Default is @false@. Set to true to enable the deduplication,
-- compression, and compaction storage efficiency features on the volume.
--
-- 'tieringPolicy', 'updateOntapVolumeConfiguration_tieringPolicy' - Update the volume\'s data tiering policy.
--
-- 'securityStyle', 'updateOntapVolumeConfiguration_securityStyle' - The security style for the volume, which can be @UNIX@. @NTFS@, or
-- @MIXED@.
--
-- 'junctionPath', 'updateOntapVolumeConfiguration_junctionPath' - Specifies the location in the SVM\'s namespace where the volume is
-- mounted. The @JunctionPath@ must have a leading forward slash, such as
-- @\/vol3@.
--
-- 'sizeInMegabytes', 'updateOntapVolumeConfiguration_sizeInMegabytes' - Specifies the size of the volume in megabytes.
newUpdateOntapVolumeConfiguration ::
  UpdateOntapVolumeConfiguration
newUpdateOntapVolumeConfiguration =
  UpdateOntapVolumeConfiguration'
    { storageEfficiencyEnabled =
        Prelude.Nothing,
      tieringPolicy = Prelude.Nothing,
      securityStyle = Prelude.Nothing,
      junctionPath = Prelude.Nothing,
      sizeInMegabytes = Prelude.Nothing
    }

-- | Default is @false@. Set to true to enable the deduplication,
-- compression, and compaction storage efficiency features on the volume.
updateOntapVolumeConfiguration_storageEfficiencyEnabled :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe Prelude.Bool)
updateOntapVolumeConfiguration_storageEfficiencyEnabled = Lens.lens (\UpdateOntapVolumeConfiguration' {storageEfficiencyEnabled} -> storageEfficiencyEnabled) (\s@UpdateOntapVolumeConfiguration' {} a -> s {storageEfficiencyEnabled = a} :: UpdateOntapVolumeConfiguration)

-- | Update the volume\'s data tiering policy.
updateOntapVolumeConfiguration_tieringPolicy :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe TieringPolicy)
updateOntapVolumeConfiguration_tieringPolicy = Lens.lens (\UpdateOntapVolumeConfiguration' {tieringPolicy} -> tieringPolicy) (\s@UpdateOntapVolumeConfiguration' {} a -> s {tieringPolicy = a} :: UpdateOntapVolumeConfiguration)

-- | The security style for the volume, which can be @UNIX@. @NTFS@, or
-- @MIXED@.
updateOntapVolumeConfiguration_securityStyle :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe SecurityStyle)
updateOntapVolumeConfiguration_securityStyle = Lens.lens (\UpdateOntapVolumeConfiguration' {securityStyle} -> securityStyle) (\s@UpdateOntapVolumeConfiguration' {} a -> s {securityStyle = a} :: UpdateOntapVolumeConfiguration)

-- | Specifies the location in the SVM\'s namespace where the volume is
-- mounted. The @JunctionPath@ must have a leading forward slash, such as
-- @\/vol3@.
updateOntapVolumeConfiguration_junctionPath :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe Prelude.Text)
updateOntapVolumeConfiguration_junctionPath = Lens.lens (\UpdateOntapVolumeConfiguration' {junctionPath} -> junctionPath) (\s@UpdateOntapVolumeConfiguration' {} a -> s {junctionPath = a} :: UpdateOntapVolumeConfiguration)

-- | Specifies the size of the volume in megabytes.
updateOntapVolumeConfiguration_sizeInMegabytes :: Lens.Lens' UpdateOntapVolumeConfiguration (Prelude.Maybe Prelude.Natural)
updateOntapVolumeConfiguration_sizeInMegabytes = Lens.lens (\UpdateOntapVolumeConfiguration' {sizeInMegabytes} -> sizeInMegabytes) (\s@UpdateOntapVolumeConfiguration' {} a -> s {sizeInMegabytes = a} :: UpdateOntapVolumeConfiguration)

instance
  Prelude.Hashable
    UpdateOntapVolumeConfiguration
  where
  hashWithSalt
    _salt
    UpdateOntapVolumeConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` storageEfficiencyEnabled
        `Prelude.hashWithSalt` tieringPolicy
        `Prelude.hashWithSalt` securityStyle
        `Prelude.hashWithSalt` junctionPath
        `Prelude.hashWithSalt` sizeInMegabytes

instance
  Prelude.NFData
    UpdateOntapVolumeConfiguration
  where
  rnf UpdateOntapVolumeConfiguration' {..} =
    Prelude.rnf storageEfficiencyEnabled
      `Prelude.seq` Prelude.rnf tieringPolicy
      `Prelude.seq` Prelude.rnf securityStyle
      `Prelude.seq` Prelude.rnf junctionPath
      `Prelude.seq` Prelude.rnf sizeInMegabytes

instance Core.ToJSON UpdateOntapVolumeConfiguration where
  toJSON UpdateOntapVolumeConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StorageEfficiencyEnabled" Core..=)
              Prelude.<$> storageEfficiencyEnabled,
            ("TieringPolicy" Core..=) Prelude.<$> tieringPolicy,
            ("SecurityStyle" Core..=) Prelude.<$> securityStyle,
            ("JunctionPath" Core..=) Prelude.<$> junctionPath,
            ("SizeInMegabytes" Core..=)
              Prelude.<$> sizeInMegabytes
          ]
      )
