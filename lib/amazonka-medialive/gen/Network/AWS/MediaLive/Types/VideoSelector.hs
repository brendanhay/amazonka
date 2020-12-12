{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelector
  ( VideoSelector (..),

    -- * Smart constructor
    mkVideoSelector,

    -- * Lenses
    vsSelectorSettings,
    vsColorSpaceUsage,
    vsColorSpace,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.VideoSelectorColorSpace
import Network.AWS.MediaLive.Types.VideoSelectorColorSpaceUsage
import Network.AWS.MediaLive.Types.VideoSelectorSettings
import qualified Network.AWS.Prelude as Lude

-- | Specifies a particular video stream within an input source. An input may have only a single video selector.
--
-- /See:/ 'mkVideoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { selectorSettings ::
      Lude.Maybe VideoSelectorSettings,
    colorSpaceUsage :: Lude.Maybe VideoSelectorColorSpaceUsage,
    colorSpace :: Lude.Maybe VideoSelectorColorSpace
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VideoSelector' with the minimum fields required to make a request.
--
-- * 'colorSpace' - Specifies the color space of an input. This setting works in tandem with colorSpaceUsage and a video description's colorSpaceSettingsChoice to determine if any conversion will be performed.
-- * 'colorSpaceUsage' - Applies only if colorSpace is a value other than follow. This field controls how the value in the colorSpace field will be used. fallback means that when the input does include color space data, that data will be used, but when the input has no color space data, the value in colorSpace will be used. Choose fallback if your input is sometimes missing color space data, but when it does have color space data, that data is correct. force means to always use the value in colorSpace. Choose force if your input usually has no color space data or might have unreliable color space data.
-- * 'selectorSettings' - The video selector settings.
mkVideoSelector ::
  VideoSelector
mkVideoSelector =
  VideoSelector'
    { selectorSettings = Lude.Nothing,
      colorSpaceUsage = Lude.Nothing,
      colorSpace = Lude.Nothing
    }

-- | The video selector settings.
--
-- /Note:/ Consider using 'selectorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSelectorSettings :: Lens.Lens' VideoSelector (Lude.Maybe VideoSelectorSettings)
vsSelectorSettings = Lens.lens (selectorSettings :: VideoSelector -> Lude.Maybe VideoSelectorSettings) (\s a -> s {selectorSettings = a} :: VideoSelector)
{-# DEPRECATED vsSelectorSettings "Use generic-lens or generic-optics with 'selectorSettings' instead." #-}

-- | Applies only if colorSpace is a value other than follow. This field controls how the value in the colorSpace field will be used. fallback means that when the input does include color space data, that data will be used, but when the input has no color space data, the value in colorSpace will be used. Choose fallback if your input is sometimes missing color space data, but when it does have color space data, that data is correct. force means to always use the value in colorSpace. Choose force if your input usually has no color space data or might have unreliable color space data.
--
-- /Note:/ Consider using 'colorSpaceUsage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsColorSpaceUsage :: Lens.Lens' VideoSelector (Lude.Maybe VideoSelectorColorSpaceUsage)
vsColorSpaceUsage = Lens.lens (colorSpaceUsage :: VideoSelector -> Lude.Maybe VideoSelectorColorSpaceUsage) (\s a -> s {colorSpaceUsage = a} :: VideoSelector)
{-# DEPRECATED vsColorSpaceUsage "Use generic-lens or generic-optics with 'colorSpaceUsage' instead." #-}

-- | Specifies the color space of an input. This setting works in tandem with colorSpaceUsage and a video description's colorSpaceSettingsChoice to determine if any conversion will be performed.
--
-- /Note:/ Consider using 'colorSpace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsColorSpace :: Lens.Lens' VideoSelector (Lude.Maybe VideoSelectorColorSpace)
vsColorSpace = Lens.lens (colorSpace :: VideoSelector -> Lude.Maybe VideoSelectorColorSpace) (\s a -> s {colorSpace = a} :: VideoSelector)
{-# DEPRECATED vsColorSpace "Use generic-lens or generic-optics with 'colorSpace' instead." #-}

instance Lude.FromJSON VideoSelector where
  parseJSON =
    Lude.withObject
      "VideoSelector"
      ( \x ->
          VideoSelector'
            Lude.<$> (x Lude..:? "selectorSettings")
            Lude.<*> (x Lude..:? "colorSpaceUsage")
            Lude.<*> (x Lude..:? "colorSpace")
      )

instance Lude.ToJSON VideoSelector where
  toJSON VideoSelector' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("selectorSettings" Lude..=) Lude.<$> selectorSettings,
            ("colorSpaceUsage" Lude..=) Lude.<$> colorSpaceUsage,
            ("colorSpace" Lude..=) Lude.<$> colorSpace
          ]
      )
