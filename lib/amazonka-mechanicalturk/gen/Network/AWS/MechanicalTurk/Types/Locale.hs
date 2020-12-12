{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Locale
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Locale
  ( Locale (..),

    -- * Smart constructor
    mkLocale,

    -- * Lenses
    lSubdivision,
    lCountry,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Locale data structure represents a geographical region or location.
--
-- /See:/ 'mkLocale' smart constructor.
data Locale = Locale'
  { subdivision :: Lude.Maybe Lude.Text,
    country :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Locale' with the minimum fields required to make a request.
--
-- * 'country' - The country of the locale. Must be a valid ISO 3166 country code. For example, the code US refers to the United States of America.
-- * 'subdivision' - The state or subdivision of the locale. A valid ISO 3166-2 subdivision code. For example, the code WA refers to the state of Washington.
mkLocale ::
  -- | 'country'
  Lude.Text ->
  Locale
mkLocale pCountry_ =
  Locale' {subdivision = Lude.Nothing, country = pCountry_}

-- | The state or subdivision of the locale. A valid ISO 3166-2 subdivision code. For example, the code WA refers to the state of Washington.
--
-- /Note:/ Consider using 'subdivision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSubdivision :: Lens.Lens' Locale (Lude.Maybe Lude.Text)
lSubdivision = Lens.lens (subdivision :: Locale -> Lude.Maybe Lude.Text) (\s a -> s {subdivision = a} :: Locale)
{-# DEPRECATED lSubdivision "Use generic-lens or generic-optics with 'subdivision' instead." #-}

-- | The country of the locale. Must be a valid ISO 3166 country code. For example, the code US refers to the United States of America.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCountry :: Lens.Lens' Locale Lude.Text
lCountry = Lens.lens (country :: Locale -> Lude.Text) (\s a -> s {country = a} :: Locale)
{-# DEPRECATED lCountry "Use generic-lens or generic-optics with 'country' instead." #-}

instance Lude.FromJSON Locale where
  parseJSON =
    Lude.withObject
      "Locale"
      ( \x ->
          Locale'
            Lude.<$> (x Lude..:? "Subdivision") Lude.<*> (x Lude..: "Country")
      )

instance Lude.ToJSON Locale where
  toJSON Locale' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Subdivision" Lude..=) Lude.<$> subdivision,
            Lude.Just ("Country" Lude..= country)
          ]
      )
