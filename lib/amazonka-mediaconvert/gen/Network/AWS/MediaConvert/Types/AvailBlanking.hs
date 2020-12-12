{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvailBlanking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvailBlanking
  ( AvailBlanking (..),

    -- * Smart constructor
    mkAvailBlanking,

    -- * Lenses
    abAvailBlankingImage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for Avail Blanking
--
-- /See:/ 'mkAvailBlanking' smart constructor.
newtype AvailBlanking = AvailBlanking'
  { availBlankingImage ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailBlanking' with the minimum fields required to make a request.
--
-- * 'availBlankingImage' - Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
mkAvailBlanking ::
  AvailBlanking
mkAvailBlanking = AvailBlanking' {availBlankingImage = Lude.Nothing}

-- | Blanking image to be used. Leave empty for solid black. Only bmp and png images are supported.
--
-- /Note:/ Consider using 'availBlankingImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
abAvailBlankingImage :: Lens.Lens' AvailBlanking (Lude.Maybe Lude.Text)
abAvailBlankingImage = Lens.lens (availBlankingImage :: AvailBlanking -> Lude.Maybe Lude.Text) (\s a -> s {availBlankingImage = a} :: AvailBlanking)
{-# DEPRECATED abAvailBlankingImage "Use generic-lens or generic-optics with 'availBlankingImage' instead." #-}

instance Lude.FromJSON AvailBlanking where
  parseJSON =
    Lude.withObject
      "AvailBlanking"
      (\x -> AvailBlanking' Lude.<$> (x Lude..:? "availBlankingImage"))

instance Lude.ToJSON AvailBlanking where
  toJSON AvailBlanking' {..} =
    Lude.object
      ( Lude.catMaybes
          [("availBlankingImage" Lude..=) Lude.<$> availBlankingImage]
      )
