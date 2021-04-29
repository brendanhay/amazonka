{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputGroupSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmafGroupSettings
import Network.AWS.MediaConvert.Types.DashIsoGroupSettings
import Network.AWS.MediaConvert.Types.FileGroupSettings
import Network.AWS.MediaConvert.Types.HlsGroupSettings
import Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
import Network.AWS.MediaConvert.Types.OutputGroupType
import qualified Network.AWS.Prelude as Prelude

-- | Output Group settings, including type
--
-- /See:/ 'newOutputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to MS_SMOOTH_GROUP_SETTINGS.
    msSmoothGroupSettings :: Prelude.Maybe MsSmoothGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to HLS_GROUP_SETTINGS.
    hlsGroupSettings :: Prelude.Maybe HlsGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to FILE_GROUP_SETTINGS.
    fileGroupSettings :: Prelude.Maybe FileGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to DASH_ISO_GROUP_SETTINGS.
    dashIsoGroupSettings :: Prelude.Maybe DashIsoGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only
    -- contain a single video, audio, or caption output.
    cmafGroupSettings :: Prelude.Maybe CmafGroupSettings,
    -- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth
    -- Streaming, CMAF)
    type' :: Prelude.Maybe OutputGroupType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'msSmoothGroupSettings', 'outputGroupSettings_msSmoothGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to MS_SMOOTH_GROUP_SETTINGS.
--
-- 'hlsGroupSettings', 'outputGroupSettings_hlsGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to HLS_GROUP_SETTINGS.
--
-- 'fileGroupSettings', 'outputGroupSettings_fileGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to FILE_GROUP_SETTINGS.
--
-- 'dashIsoGroupSettings', 'outputGroupSettings_dashIsoGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to DASH_ISO_GROUP_SETTINGS.
--
-- 'cmafGroupSettings', 'outputGroupSettings_cmafGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only
-- contain a single video, audio, or caption output.
--
-- 'type'', 'outputGroupSettings_type' - Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth
-- Streaming, CMAF)
newOutputGroupSettings ::
  OutputGroupSettings
newOutputGroupSettings =
  OutputGroupSettings'
    { msSmoothGroupSettings =
        Prelude.Nothing,
      hlsGroupSettings = Prelude.Nothing,
      fileGroupSettings = Prelude.Nothing,
      dashIsoGroupSettings = Prelude.Nothing,
      cmafGroupSettings = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to MS_SMOOTH_GROUP_SETTINGS.
outputGroupSettings_msSmoothGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe MsSmoothGroupSettings)
outputGroupSettings_msSmoothGroupSettings = Lens.lens (\OutputGroupSettings' {msSmoothGroupSettings} -> msSmoothGroupSettings) (\s@OutputGroupSettings' {} a -> s {msSmoothGroupSettings = a} :: OutputGroupSettings)

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to HLS_GROUP_SETTINGS.
outputGroupSettings_hlsGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe HlsGroupSettings)
outputGroupSettings_hlsGroupSettings = Lens.lens (\OutputGroupSettings' {hlsGroupSettings} -> hlsGroupSettings) (\s@OutputGroupSettings' {} a -> s {hlsGroupSettings = a} :: OutputGroupSettings)

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to FILE_GROUP_SETTINGS.
outputGroupSettings_fileGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe FileGroupSettings)
outputGroupSettings_fileGroupSettings = Lens.lens (\OutputGroupSettings' {fileGroupSettings} -> fileGroupSettings) (\s@OutputGroupSettings' {} a -> s {fileGroupSettings = a} :: OutputGroupSettings)

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to DASH_ISO_GROUP_SETTINGS.
outputGroupSettings_dashIsoGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe DashIsoGroupSettings)
outputGroupSettings_dashIsoGroupSettings = Lens.lens (\OutputGroupSettings' {dashIsoGroupSettings} -> dashIsoGroupSettings) (\s@OutputGroupSettings' {} a -> s {dashIsoGroupSettings = a} :: OutputGroupSettings)

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only
-- contain a single video, audio, or caption output.
outputGroupSettings_cmafGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe CmafGroupSettings)
outputGroupSettings_cmafGroupSettings = Lens.lens (\OutputGroupSettings' {cmafGroupSettings} -> cmafGroupSettings) (\s@OutputGroupSettings' {} a -> s {cmafGroupSettings = a} :: OutputGroupSettings)

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth
-- Streaming, CMAF)
outputGroupSettings_type :: Lens.Lens' OutputGroupSettings (Prelude.Maybe OutputGroupType)
outputGroupSettings_type = Lens.lens (\OutputGroupSettings' {type'} -> type') (\s@OutputGroupSettings' {} a -> s {type' = a} :: OutputGroupSettings)

instance Prelude.FromJSON OutputGroupSettings where
  parseJSON =
    Prelude.withObject
      "OutputGroupSettings"
      ( \x ->
          OutputGroupSettings'
            Prelude.<$> (x Prelude..:? "msSmoothGroupSettings")
            Prelude.<*> (x Prelude..:? "hlsGroupSettings")
            Prelude.<*> (x Prelude..:? "fileGroupSettings")
            Prelude.<*> (x Prelude..:? "dashIsoGroupSettings")
            Prelude.<*> (x Prelude..:? "cmafGroupSettings")
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable OutputGroupSettings

instance Prelude.NFData OutputGroupSettings

instance Prelude.ToJSON OutputGroupSettings where
  toJSON OutputGroupSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("msSmoothGroupSettings" Prelude..=)
              Prelude.<$> msSmoothGroupSettings,
            ("hlsGroupSettings" Prelude..=)
              Prelude.<$> hlsGroupSettings,
            ("fileGroupSettings" Prelude..=)
              Prelude.<$> fileGroupSettings,
            ("dashIsoGroupSettings" Prelude..=)
              Prelude.<$> dashIsoGroupSettings,
            ("cmafGroupSettings" Prelude..=)
              Prelude.<$> cmafGroupSettings,
            ("type" Prelude..=) Prelude.<$> type'
          ]
      )
