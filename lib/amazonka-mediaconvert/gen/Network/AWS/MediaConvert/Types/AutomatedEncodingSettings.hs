-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
  ( AutomatedEncodingSettings (..),

    -- * Smart constructor
    mkAutomatedEncodingSettings,

    -- * Lenses
    aesAbrSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AutomatedAbrSettings
import qualified Network.AWS.Prelude as Lude

-- | Use automated encoding to have MediaConvert choose your encoding settings for you, based on characteristics of your input video.
--
-- /See:/ 'mkAutomatedEncodingSettings' smart constructor.
newtype AutomatedEncodingSettings = AutomatedEncodingSettings'
  { abrSettings ::
      Lude.Maybe AutomatedAbrSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutomatedEncodingSettings' with the minimum fields required to make a request.
--
-- * 'abrSettings' - Use automated ABR to have MediaConvert set up the renditions in your ABR package for you automatically, based on characteristics of your input video. This feature optimizes video quality while minimizing the overall size of your ABR package.
mkAutomatedEncodingSettings ::
  AutomatedEncodingSettings
mkAutomatedEncodingSettings =
  AutomatedEncodingSettings' {abrSettings = Lude.Nothing}

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR package for you automatically, based on characteristics of your input video. This feature optimizes video quality while minimizing the overall size of your ABR package.
--
-- /Note:/ Consider using 'abrSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aesAbrSettings :: Lens.Lens' AutomatedEncodingSettings (Lude.Maybe AutomatedAbrSettings)
aesAbrSettings = Lens.lens (abrSettings :: AutomatedEncodingSettings -> Lude.Maybe AutomatedAbrSettings) (\s a -> s {abrSettings = a} :: AutomatedEncodingSettings)
{-# DEPRECATED aesAbrSettings "Use generic-lens or generic-optics with 'abrSettings' instead." #-}

instance Lude.FromJSON AutomatedEncodingSettings where
  parseJSON =
    Lude.withObject
      "AutomatedEncodingSettings"
      ( \x ->
          AutomatedEncodingSettings' Lude.<$> (x Lude..:? "abrSettings")
      )

instance Lude.ToJSON AutomatedEncodingSettings where
  toJSON AutomatedEncodingSettings' {..} =
    Lude.object
      (Lude.catMaybes [("abrSettings" Lude..=) Lude.<$> abrSettings])
