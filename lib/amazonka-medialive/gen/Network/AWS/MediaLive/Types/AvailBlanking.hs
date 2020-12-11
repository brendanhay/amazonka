-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AvailBlanking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailBlanking
  ( AvailBlanking (..),

    -- * Smart constructor
    mkAvailBlanking,

    -- * Lenses
    abState,
    abAvailBlankingImage,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AvailBlankingState
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Lude

-- | Avail Blanking
--
-- /See:/ 'mkAvailBlanking' smart constructor.
data AvailBlanking = AvailBlanking'
  { state ::
      Lude.Maybe AvailBlankingState,
    availBlankingImage :: Lude.Maybe InputLocation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailBlanking' with the minimum fields required to make a request.
--
-- * 'availBlankingImage' - Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
-- * 'state' - When set to enabled, causes video, audio and captions to be blanked when insertion metadata is added.
mkAvailBlanking ::
  AvailBlanking
mkAvailBlanking =
  AvailBlanking'
    { state = Lude.Nothing,
      availBlankingImage = Lude.Nothing
    }

-- | When set to enabled, causes video, audio and captions to be blanked when insertion metadata is added.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abState :: Lens.Lens' AvailBlanking (Lude.Maybe AvailBlankingState)
abState = Lens.lens (state :: AvailBlanking -> Lude.Maybe AvailBlankingState) (\s a -> s {state = a} :: AvailBlanking)
{-# DEPRECATED abState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
--
-- /Note:/ Consider using 'availBlankingImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abAvailBlankingImage :: Lens.Lens' AvailBlanking (Lude.Maybe InputLocation)
abAvailBlankingImage = Lens.lens (availBlankingImage :: AvailBlanking -> Lude.Maybe InputLocation) (\s a -> s {availBlankingImage = a} :: AvailBlanking)
{-# DEPRECATED abAvailBlankingImage "Use generic-lens or generic-optics with 'availBlankingImage' instead." #-}

instance Lude.FromJSON AvailBlanking where
  parseJSON =
    Lude.withObject
      "AvailBlanking"
      ( \x ->
          AvailBlanking'
            Lude.<$> (x Lude..:? "state") Lude.<*> (x Lude..:? "availBlankingImage")
      )

instance Lude.ToJSON AvailBlanking where
  toJSON AvailBlanking' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("state" Lude..=) Lude.<$> state,
            ("availBlankingImage" Lude..=) Lude.<$> availBlankingImage
          ]
      )
