-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputSettings
  ( OutputSettings (..),

    -- * Smart constructor
    mkOutputSettings,

    -- * Lenses
    osHlsSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.HlsSettings
import qualified Network.AWS.Prelude as Lude

-- | Specific settings for this type of output.
--
-- /See:/ 'mkOutputSettings' smart constructor.
newtype OutputSettings = OutputSettings'
  { hlsSettings ::
      Lude.Maybe HlsSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputSettings' with the minimum fields required to make a request.
--
-- * 'hlsSettings' - Settings for HLS output groups
mkOutputSettings ::
  OutputSettings
mkOutputSettings = OutputSettings' {hlsSettings = Lude.Nothing}

-- | Settings for HLS output groups
--
-- /Note:/ Consider using 'hlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osHlsSettings :: Lens.Lens' OutputSettings (Lude.Maybe HlsSettings)
osHlsSettings = Lens.lens (hlsSettings :: OutputSettings -> Lude.Maybe HlsSettings) (\s a -> s {hlsSettings = a} :: OutputSettings)
{-# DEPRECATED osHlsSettings "Use generic-lens or generic-optics with 'hlsSettings' instead." #-}

instance Lude.FromJSON OutputSettings where
  parseJSON =
    Lude.withObject
      "OutputSettings"
      (\x -> OutputSettings' Lude.<$> (x Lude..:? "hlsSettings"))

instance Lude.ToJSON OutputSettings where
  toJSON OutputSettings' {..} =
    Lude.object
      (Lude.catMaybes [("hlsSettings" Lude..=) Lude.<$> hlsSettings])
