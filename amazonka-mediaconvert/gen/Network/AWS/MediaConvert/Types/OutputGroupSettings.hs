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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmafGroupSettings
import Network.AWS.MediaConvert.Types.DashIsoGroupSettings
import Network.AWS.MediaConvert.Types.FileGroupSettings
import Network.AWS.MediaConvert.Types.HlsGroupSettings
import Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
import Network.AWS.MediaConvert.Types.OutputGroupType

-- | Output Group settings, including type
--
-- /See:/ 'newOutputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to MS_SMOOTH_GROUP_SETTINGS.
    msSmoothGroupSettings :: Core.Maybe MsSmoothGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to HLS_GROUP_SETTINGS.
    hlsGroupSettings :: Core.Maybe HlsGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to FILE_GROUP_SETTINGS.
    fileGroupSettings :: Core.Maybe FileGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to DASH_ISO_GROUP_SETTINGS.
    dashIsoGroupSettings :: Core.Maybe DashIsoGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
    -- to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only
    -- contain a single video, audio, or caption output.
    cmafGroupSettings :: Core.Maybe CmafGroupSettings,
    -- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth
    -- Streaming, CMAF)
    type' :: Core.Maybe OutputGroupType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      hlsGroupSettings = Core.Nothing,
      fileGroupSettings = Core.Nothing,
      dashIsoGroupSettings = Core.Nothing,
      cmafGroupSettings = Core.Nothing,
      type' = Core.Nothing
    }

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to MS_SMOOTH_GROUP_SETTINGS.
outputGroupSettings_msSmoothGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe MsSmoothGroupSettings)
outputGroupSettings_msSmoothGroupSettings = Lens.lens (\OutputGroupSettings' {msSmoothGroupSettings} -> msSmoothGroupSettings) (\s@OutputGroupSettings' {} a -> s {msSmoothGroupSettings = a} :: OutputGroupSettings)

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to HLS_GROUP_SETTINGS.
outputGroupSettings_hlsGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe HlsGroupSettings)
outputGroupSettings_hlsGroupSettings = Lens.lens (\OutputGroupSettings' {hlsGroupSettings} -> hlsGroupSettings) (\s@OutputGroupSettings' {} a -> s {hlsGroupSettings = a} :: OutputGroupSettings)

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to FILE_GROUP_SETTINGS.
outputGroupSettings_fileGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe FileGroupSettings)
outputGroupSettings_fileGroupSettings = Lens.lens (\OutputGroupSettings' {fileGroupSettings} -> fileGroupSettings) (\s@OutputGroupSettings' {} a -> s {fileGroupSettings = a} :: OutputGroupSettings)

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to DASH_ISO_GROUP_SETTINGS.
outputGroupSettings_dashIsoGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe DashIsoGroupSettings)
outputGroupSettings_dashIsoGroupSettings = Lens.lens (\OutputGroupSettings' {dashIsoGroupSettings} -> dashIsoGroupSettings) (\s@OutputGroupSettings' {} a -> s {dashIsoGroupSettings = a} :: OutputGroupSettings)

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only
-- contain a single video, audio, or caption output.
outputGroupSettings_cmafGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe CmafGroupSettings)
outputGroupSettings_cmafGroupSettings = Lens.lens (\OutputGroupSettings' {cmafGroupSettings} -> cmafGroupSettings) (\s@OutputGroupSettings' {} a -> s {cmafGroupSettings = a} :: OutputGroupSettings)

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth
-- Streaming, CMAF)
outputGroupSettings_type :: Lens.Lens' OutputGroupSettings (Core.Maybe OutputGroupType)
outputGroupSettings_type = Lens.lens (\OutputGroupSettings' {type'} -> type') (\s@OutputGroupSettings' {} a -> s {type' = a} :: OutputGroupSettings)

instance Core.FromJSON OutputGroupSettings where
  parseJSON =
    Core.withObject
      "OutputGroupSettings"
      ( \x ->
          OutputGroupSettings'
            Core.<$> (x Core..:? "msSmoothGroupSettings")
            Core.<*> (x Core..:? "hlsGroupSettings")
            Core.<*> (x Core..:? "fileGroupSettings")
            Core.<*> (x Core..:? "dashIsoGroupSettings")
            Core.<*> (x Core..:? "cmafGroupSettings")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable OutputGroupSettings

instance Core.NFData OutputGroupSettings

instance Core.ToJSON OutputGroupSettings where
  toJSON OutputGroupSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("msSmoothGroupSettings" Core..=)
              Core.<$> msSmoothGroupSettings,
            ("hlsGroupSettings" Core..=)
              Core.<$> hlsGroupSettings,
            ("fileGroupSettings" Core..=)
              Core.<$> fileGroupSettings,
            ("dashIsoGroupSettings" Core..=)
              Core.<$> dashIsoGroupSettings,
            ("cmafGroupSettings" Core..=)
              Core.<$> cmafGroupSettings,
            ("type" Core..=) Core.<$> type'
          ]
      )
