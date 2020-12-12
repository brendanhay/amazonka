{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
  ( StaticImageDeactivateScheduleActionSettings (..),

    -- * Smart constructor
    mkStaticImageDeactivateScheduleActionSettings,

    -- * Lenses
    sidsasFadeOut,
    sidsasLayer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for the action to deactivate the image in a specific layer.
--
-- /See:/ 'mkStaticImageDeactivateScheduleActionSettings' smart constructor.
data StaticImageDeactivateScheduleActionSettings = StaticImageDeactivateScheduleActionSettings'
  { fadeOut ::
      Lude.Maybe
        Lude.Natural,
    layer ::
      Lude.Maybe
        Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StaticImageDeactivateScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'fadeOut' - The time in milliseconds for the image to fade out. Default is 0 (no fade-out).
-- * 'layer' - The image overlay layer to deactivate, 0 to 7. Default is 0.
mkStaticImageDeactivateScheduleActionSettings ::
  StaticImageDeactivateScheduleActionSettings
mkStaticImageDeactivateScheduleActionSettings =
  StaticImageDeactivateScheduleActionSettings'
    { fadeOut =
        Lude.Nothing,
      layer = Lude.Nothing
    }

-- | The time in milliseconds for the image to fade out. Default is 0 (no fade-out).
--
-- /Note:/ Consider using 'fadeOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sidsasFadeOut :: Lens.Lens' StaticImageDeactivateScheduleActionSettings (Lude.Maybe Lude.Natural)
sidsasFadeOut = Lens.lens (fadeOut :: StaticImageDeactivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fadeOut = a} :: StaticImageDeactivateScheduleActionSettings)
{-# DEPRECATED sidsasFadeOut "Use generic-lens or generic-optics with 'fadeOut' instead." #-}

-- | The image overlay layer to deactivate, 0 to 7. Default is 0.
--
-- /Note:/ Consider using 'layer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sidsasLayer :: Lens.Lens' StaticImageDeactivateScheduleActionSettings (Lude.Maybe Lude.Natural)
sidsasLayer = Lens.lens (layer :: StaticImageDeactivateScheduleActionSettings -> Lude.Maybe Lude.Natural) (\s a -> s {layer = a} :: StaticImageDeactivateScheduleActionSettings)
{-# DEPRECATED sidsasLayer "Use generic-lens or generic-optics with 'layer' instead." #-}

instance Lude.FromJSON StaticImageDeactivateScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "StaticImageDeactivateScheduleActionSettings"
      ( \x ->
          StaticImageDeactivateScheduleActionSettings'
            Lude.<$> (x Lude..:? "fadeOut") Lude.<*> (x Lude..:? "layer")
      )

instance Lude.ToJSON StaticImageDeactivateScheduleActionSettings where
  toJSON StaticImageDeactivateScheduleActionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fadeOut" Lude..=) Lude.<$> fadeOut,
            ("layer" Lude..=) Lude.<$> layer
          ]
      )
