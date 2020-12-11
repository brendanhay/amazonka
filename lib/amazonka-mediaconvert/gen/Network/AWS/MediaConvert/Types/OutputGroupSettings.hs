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
    ogsFileGroupSettings,
    ogsCmafGroupSettings,
    ogsMsSmoothGroupSettings,
    ogsHlsGroupSettings,
    ogsType,
    ogsDashIsoGroupSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmafGroupSettings
import Network.AWS.MediaConvert.Types.DashIsoGroupSettings
import Network.AWS.MediaConvert.Types.FileGroupSettings
import Network.AWS.MediaConvert.Types.HlsGroupSettings
import Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
import Network.AWS.MediaConvert.Types.OutputGroupType
import qualified Network.AWS.Prelude as Lude

-- | Output Group settings, including type
--
-- /See:/ 'mkOutputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { fileGroupSettings ::
      Lude.Maybe FileGroupSettings,
    cmafGroupSettings :: Lude.Maybe CmafGroupSettings,
    msSmoothGroupSettings ::
      Lude.Maybe MsSmoothGroupSettings,
    hlsGroupSettings :: Lude.Maybe HlsGroupSettings,
    type' :: Lude.Maybe OutputGroupType,
    dashIsoGroupSettings ::
      Lude.Maybe DashIsoGroupSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputGroupSettings' with the minimum fields required to make a request.
--
-- * 'cmafGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only contain a single video, audio, or caption output.
-- * 'dashIsoGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
-- * 'fileGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
-- * 'hlsGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
-- * 'msSmoothGroupSettings' - Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.
-- * 'type'' - Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming, CMAF)
mkOutputGroupSettings ::
  OutputGroupSettings
mkOutputGroupSettings =
  OutputGroupSettings'
    { fileGroupSettings = Lude.Nothing,
      cmafGroupSettings = Lude.Nothing,
      msSmoothGroupSettings = Lude.Nothing,
      hlsGroupSettings = Lude.Nothing,
      type' = Lude.Nothing,
      dashIsoGroupSettings = Lude.Nothing
    }

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
--
-- /Note:/ Consider using 'fileGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsFileGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe FileGroupSettings)
ogsFileGroupSettings = Lens.lens (fileGroupSettings :: OutputGroupSettings -> Lude.Maybe FileGroupSettings) (\s a -> s {fileGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsFileGroupSettings "Use generic-lens or generic-optics with 'fileGroupSettings' instead." #-}

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only contain a single video, audio, or caption output.
--
-- /Note:/ Consider using 'cmafGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsCmafGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe CmafGroupSettings)
ogsCmafGroupSettings = Lens.lens (cmafGroupSettings :: OutputGroupSettings -> Lude.Maybe CmafGroupSettings) (\s a -> s {cmafGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsCmafGroupSettings "Use generic-lens or generic-optics with 'cmafGroupSettings' instead." #-}

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.
--
-- /Note:/ Consider using 'msSmoothGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsMsSmoothGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe MsSmoothGroupSettings)
ogsMsSmoothGroupSettings = Lens.lens (msSmoothGroupSettings :: OutputGroupSettings -> Lude.Maybe MsSmoothGroupSettings) (\s a -> s {msSmoothGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsMsSmoothGroupSettings "Use generic-lens or generic-optics with 'msSmoothGroupSettings' instead." #-}

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
--
-- /Note:/ Consider using 'hlsGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsHlsGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe HlsGroupSettings)
ogsHlsGroupSettings = Lens.lens (hlsGroupSettings :: OutputGroupSettings -> Lude.Maybe HlsGroupSettings) (\s a -> s {hlsGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsHlsGroupSettings "Use generic-lens or generic-optics with 'hlsGroupSettings' instead." #-}

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming, CMAF)
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsType :: Lens.Lens' OutputGroupSettings (Lude.Maybe OutputGroupType)
ogsType = Lens.lens (type' :: OutputGroupSettings -> Lude.Maybe OutputGroupType) (\s a -> s {type' = a} :: OutputGroupSettings)
{-# DEPRECATED ogsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
--
-- /Note:/ Consider using 'dashIsoGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogsDashIsoGroupSettings :: Lens.Lens' OutputGroupSettings (Lude.Maybe DashIsoGroupSettings)
ogsDashIsoGroupSettings = Lens.lens (dashIsoGroupSettings :: OutputGroupSettings -> Lude.Maybe DashIsoGroupSettings) (\s a -> s {dashIsoGroupSettings = a} :: OutputGroupSettings)
{-# DEPRECATED ogsDashIsoGroupSettings "Use generic-lens or generic-optics with 'dashIsoGroupSettings' instead." #-}

instance Lude.FromJSON OutputGroupSettings where
  parseJSON =
    Lude.withObject
      "OutputGroupSettings"
      ( \x ->
          OutputGroupSettings'
            Lude.<$> (x Lude..:? "fileGroupSettings")
            Lude.<*> (x Lude..:? "cmafGroupSettings")
            Lude.<*> (x Lude..:? "msSmoothGroupSettings")
            Lude.<*> (x Lude..:? "hlsGroupSettings")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "dashIsoGroupSettings")
      )

instance Lude.ToJSON OutputGroupSettings where
  toJSON OutputGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fileGroupSettings" Lude..=) Lude.<$> fileGroupSettings,
            ("cmafGroupSettings" Lude..=) Lude.<$> cmafGroupSettings,
            ("msSmoothGroupSettings" Lude..=) Lude.<$> msSmoothGroupSettings,
            ("hlsGroupSettings" Lude..=) Lude.<$> hlsGroupSettings,
            ("type" Lude..=) Lude.<$> type',
            ("dashIsoGroupSettings" Lude..=) Lude.<$> dashIsoGroupSettings
          ]
      )
