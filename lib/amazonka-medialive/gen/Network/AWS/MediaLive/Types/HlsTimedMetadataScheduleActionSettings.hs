-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsTimedMetadataScheduleActionSettings
  ( HlsTimedMetadataScheduleActionSettings (..),

    -- * Smart constructor
    mkHlsTimedMetadataScheduleActionSettings,

    -- * Lenses
    htmsasId3,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for the action to emit HLS metadata
--
-- /See:/ 'mkHlsTimedMetadataScheduleActionSettings' smart constructor.
newtype HlsTimedMetadataScheduleActionSettings = HlsTimedMetadataScheduleActionSettings'
  { id3 ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsTimedMetadataScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'id3' - Base64 string formatted according to the ID3 specification: http://id3.org/id3v2.4.0-structure
mkHlsTimedMetadataScheduleActionSettings ::
  -- | 'id3'
  Lude.Text ->
  HlsTimedMetadataScheduleActionSettings
mkHlsTimedMetadataScheduleActionSettings pId3_ =
  HlsTimedMetadataScheduleActionSettings' {id3 = pId3_}

-- | Base64 string formatted according to the ID3 specification: http://id3.org/id3v2.4.0-structure
--
-- /Note:/ Consider using 'id3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
htmsasId3 :: Lens.Lens' HlsTimedMetadataScheduleActionSettings Lude.Text
htmsasId3 = Lens.lens (id3 :: HlsTimedMetadataScheduleActionSettings -> Lude.Text) (\s a -> s {id3 = a} :: HlsTimedMetadataScheduleActionSettings)
{-# DEPRECATED htmsasId3 "Use generic-lens or generic-optics with 'id3' instead." #-}

instance Lude.FromJSON HlsTimedMetadataScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "HlsTimedMetadataScheduleActionSettings"
      ( \x ->
          HlsTimedMetadataScheduleActionSettings' Lude.<$> (x Lude..: "id3")
      )

instance Lude.ToJSON HlsTimedMetadataScheduleActionSettings where
  toJSON HlsTimedMetadataScheduleActionSettings' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("id3" Lude..= id3)])
