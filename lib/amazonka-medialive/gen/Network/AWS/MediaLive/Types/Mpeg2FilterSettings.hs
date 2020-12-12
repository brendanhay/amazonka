{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2FilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2FilterSettings
  ( Mpeg2FilterSettings (..),

    -- * Smart constructor
    mkMpeg2FilterSettings,

    -- * Lenses
    mfsTemporalFilterSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TemporalFilterSettings
import qualified Network.AWS.Prelude as Lude

-- | Mpeg2 Filter Settings
--
-- /See:/ 'mkMpeg2FilterSettings' smart constructor.
newtype Mpeg2FilterSettings = Mpeg2FilterSettings'
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

-- | Creates a value of 'Mpeg2FilterSettings' with the minimum fields required to make a request.
--
-- * 'temporalFilterSettings' - Undocumented field.
mkMpeg2FilterSettings ::
  Mpeg2FilterSettings
mkMpeg2FilterSettings =
  Mpeg2FilterSettings' {temporalFilterSettings = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'temporalFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfsTemporalFilterSettings :: Lens.Lens' Mpeg2FilterSettings (Lude.Maybe TemporalFilterSettings)
mfsTemporalFilterSettings = Lens.lens (temporalFilterSettings :: Mpeg2FilterSettings -> Lude.Maybe TemporalFilterSettings) (\s a -> s {temporalFilterSettings = a} :: Mpeg2FilterSettings)
{-# DEPRECATED mfsTemporalFilterSettings "Use generic-lens or generic-optics with 'temporalFilterSettings' instead." #-}

instance Lude.FromJSON Mpeg2FilterSettings where
  parseJSON =
    Lude.withObject
      "Mpeg2FilterSettings"
      ( \x ->
          Mpeg2FilterSettings'
            Lude.<$> (x Lude..:? "temporalFilterSettings")
      )

instance Lude.ToJSON Mpeg2FilterSettings where
  toJSON Mpeg2FilterSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("temporalFilterSettings" Lude..=)
              Lude.<$> temporalFilterSettings
          ]
      )
