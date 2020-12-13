{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265FilterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265FilterSettings
  ( H265FilterSettings (..),

    -- * Smart constructor
    mkH265FilterSettings,

    -- * Lenses
    hfsTemporalFilterSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TemporalFilterSettings
import qualified Network.AWS.Prelude as Lude

-- | H265 Filter Settings
--
-- /See:/ 'mkH265FilterSettings' smart constructor.
newtype H265FilterSettings = H265FilterSettings'
  { temporalFilterSettings :: Lude.Maybe TemporalFilterSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'H265FilterSettings' with the minimum fields required to make a request.
--
-- * 'temporalFilterSettings' -
mkH265FilterSettings ::
  H265FilterSettings
mkH265FilterSettings =
  H265FilterSettings' {temporalFilterSettings = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'temporalFilterSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hfsTemporalFilterSettings :: Lens.Lens' H265FilterSettings (Lude.Maybe TemporalFilterSettings)
hfsTemporalFilterSettings = Lens.lens (temporalFilterSettings :: H265FilterSettings -> Lude.Maybe TemporalFilterSettings) (\s a -> s {temporalFilterSettings = a} :: H265FilterSettings)
{-# DEPRECATED hfsTemporalFilterSettings "Use generic-lens or generic-optics with 'temporalFilterSettings' instead." #-}

instance Lude.FromJSON H265FilterSettings where
  parseJSON =
    Lude.withObject
      "H265FilterSettings"
      ( \x ->
          H265FilterSettings' Lude.<$> (x Lude..:? "temporalFilterSettings")
      )

instance Lude.ToJSON H265FilterSettings where
  toJSON H265FilterSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("temporalFilterSettings" Lude..=)
              Lude.<$> temporalFilterSettings
          ]
      )
