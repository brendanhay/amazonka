{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputGroupSettings
  ( OutputGroupSettings (..),

    -- * Smart constructor
    mkOutputGroupSettings,

    -- * Lenses
    ogsCmafGroupSettings,
    ogsDashIsoGroupSettings,
    ogsFileGroupSettings,
    ogsHlsGroupSettings,
    ogsMsSmoothGroupSettings,
    ogsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.CmafGroupSettings as Types
import qualified Network.AWS.MediaConvert.Types.DashIsoGroupSettings as Types
import qualified Network.AWS.MediaConvert.Types.FileGroupSettings as Types
import qualified Network.AWS.MediaConvert.Types.HlsGroupSettings as Types
import qualified Network.AWS.MediaConvert.Types.MsSmoothGroupSettings as Types
import qualified Network.AWS.MediaConvert.Types.OutputGroupType as Types
import qualified Network.AWS.Prelude as Core

-- | Output Group settings, including type
--
-- /See:/ 'mkOutputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only contain a single video, audio, or caption output.
    cmafGroupSettings :: Core.Maybe Types.CmafGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
    dashIsoGroupSettings :: Core.Maybe Types.DashIsoGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
    fileGroupSettings :: Core.Maybe Types.FileGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
    hlsGroupSettings :: Core.Maybe Types.HlsGroupSettings,
    -- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.
    msSmoothGroupSettings :: Core.Maybe Types.MsSmoothGroupSettings,
    -- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming, CMAF)
    type' :: Core.Maybe Types.OutputGroupType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputGroupSettings' value with any optional fields omitted.
mkOutputGroupSettings ::
  OutputGroupSettings
mkOutputGroupSettings =
  OutputGroupSettings'
    { cmafGroupSettings = Core.Nothing,
      dashIsoGroupSettings = Core.Nothing,
      fileGroupSettings = Core.Nothing,
      hlsGroupSettings = Core.Nothing,
      msSmoothGroupSettings = Core.Nothing,
      type' = Core.Nothing
    }

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only contain a single video, audio, or caption output.
--
-- /Note:/ Consider using 'cmafGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsCmafGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.CmafGroupSettings)
ogsCmafGroupSettings = Lens.field @"cmafGroupSettings"
{-# DEPRECATED ogsCmafGroupSettings "Use generic-lens or generic-optics with 'cmafGroupSettings' instead." #-}

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
--
-- /Note:/ Consider using 'dashIsoGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsDashIsoGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.DashIsoGroupSettings)
ogsDashIsoGroupSettings = Lens.field @"dashIsoGroupSettings"
{-# DEPRECATED ogsDashIsoGroupSettings "Use generic-lens or generic-optics with 'dashIsoGroupSettings' instead." #-}

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
--
-- /Note:/ Consider using 'fileGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsFileGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.FileGroupSettings)
ogsFileGroupSettings = Lens.field @"fileGroupSettings"
{-# DEPRECATED ogsFileGroupSettings "Use generic-lens or generic-optics with 'fileGroupSettings' instead." #-}

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
--
-- /Note:/ Consider using 'hlsGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsHlsGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.HlsGroupSettings)
ogsHlsGroupSettings = Lens.field @"hlsGroupSettings"
{-# DEPRECATED ogsHlsGroupSettings "Use generic-lens or generic-optics with 'hlsGroupSettings' instead." #-}

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.
--
-- /Note:/ Consider using 'msSmoothGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsMsSmoothGroupSettings :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.MsSmoothGroupSettings)
ogsMsSmoothGroupSettings = Lens.field @"msSmoothGroupSettings"
{-# DEPRECATED ogsMsSmoothGroupSettings "Use generic-lens or generic-optics with 'msSmoothGroupSettings' instead." #-}

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming, CMAF)
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsType :: Lens.Lens' OutputGroupSettings (Core.Maybe Types.OutputGroupType)
ogsType = Lens.field @"type'"
{-# DEPRECATED ogsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON OutputGroupSettings where
  toJSON OutputGroupSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("cmafGroupSettings" Core..=) Core.<$> cmafGroupSettings,
            ("dashIsoGroupSettings" Core..=) Core.<$> dashIsoGroupSettings,
            ("fileGroupSettings" Core..=) Core.<$> fileGroupSettings,
            ("hlsGroupSettings" Core..=) Core.<$> hlsGroupSettings,
            ("msSmoothGroupSettings" Core..=) Core.<$> msSmoothGroupSettings,
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON OutputGroupSettings where
  parseJSON =
    Core.withObject "OutputGroupSettings" Core.$
      \x ->
        OutputGroupSettings'
          Core.<$> (x Core..:? "cmafGroupSettings")
          Core.<*> (x Core..:? "dashIsoGroupSettings")
          Core.<*> (x Core..:? "fileGroupSettings")
          Core.<*> (x Core..:? "hlsGroupSettings")
          Core.<*> (x Core..:? "msSmoothGroupSettings")
          Core.<*> (x Core..:? "type")
