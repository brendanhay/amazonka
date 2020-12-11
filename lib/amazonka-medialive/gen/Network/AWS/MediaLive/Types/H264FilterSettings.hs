-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264FilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264FilterSettings
  ( H264FilterSettings (..),

    -- * Smart constructor
    mkH264FilterSettings,

    -- * Lenses
    hTemporalFilterSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TemporalFilterSettings
import qualified Network.AWS.Prelude as Lude

-- | H264 Filter Settings
--
-- /See:/ 'mkH264FilterSettings' smart constructor.
newtype H264FilterSettings = H264FilterSettings'
  { temporalFilterSettings ::
      Lude.Maybe TemporalFilterSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'H264FilterSettings' with the minimum fields required to make a request.
--
-- * 'temporalFilterSettings' - Undocumented field.
mkH264FilterSettings ::
  H264FilterSettings
mkH264FilterSettings =
  H264FilterSettings' {temporalFilterSettings = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'temporalFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hTemporalFilterSettings :: Lens.Lens' H264FilterSettings (Lude.Maybe TemporalFilterSettings)
hTemporalFilterSettings = Lens.lens (temporalFilterSettings :: H264FilterSettings -> Lude.Maybe TemporalFilterSettings) (\s a -> s {temporalFilterSettings = a} :: H264FilterSettings)
{-# DEPRECATED hTemporalFilterSettings "Use generic-lens or generic-optics with 'temporalFilterSettings' instead." #-}

instance Lude.FromJSON H264FilterSettings where
  parseJSON =
    Lude.withObject
      "H264FilterSettings"
      ( \x ->
          H264FilterSettings' Lude.<$> (x Lude..:? "temporalFilterSettings")
      )

instance Lude.ToJSON H264FilterSettings where
  toJSON H264FilterSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("temporalFilterSettings" Lude..=)
              Lude.<$> temporalFilterSettings
          ]
      )
