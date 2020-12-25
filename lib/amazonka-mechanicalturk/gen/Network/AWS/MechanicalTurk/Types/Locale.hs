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
    lCountry,
    lSubdivision,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.Country as Types
import qualified Network.AWS.MechanicalTurk.Types.Subdivision as Types
import qualified Network.AWS.Prelude as Core

-- | The Locale data structure represents a geographical region or location.
--
-- /See:/ 'mkLocale' smart constructor.
data Locale = Locale'
  { -- | The country of the locale. Must be a valid ISO 3166 country code. For example, the code US refers to the United States of America.
    country :: Types.Country,
    -- | The state or subdivision of the locale. A valid ISO 3166-2 subdivision code. For example, the code WA refers to the state of Washington.
    subdivision :: Core.Maybe Types.Subdivision
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Locale' value with any optional fields omitted.
mkLocale ::
  -- | 'country'
  Types.Country ->
  Locale
mkLocale country = Locale' {country, subdivision = Core.Nothing}

-- | The country of the locale. Must be a valid ISO 3166 country code. For example, the code US refers to the United States of America.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCountry :: Lens.Lens' Locale Types.Country
lCountry = Lens.field @"country"
{-# DEPRECATED lCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The state or subdivision of the locale. A valid ISO 3166-2 subdivision code. For example, the code WA refers to the state of Washington.
--
-- /Note:/ Consider using 'subdivision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lSubdivision :: Lens.Lens' Locale (Core.Maybe Types.Subdivision)
lSubdivision = Lens.field @"subdivision"
{-# DEPRECATED lSubdivision "Use generic-lens or generic-optics with 'subdivision' instead." #-}

instance Core.FromJSON Locale where
  toJSON Locale {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Country" Core..= country),
            ("Subdivision" Core..=) Core.<$> subdivision
          ]
      )

instance Core.FromJSON Locale where
  parseJSON =
    Core.withObject "Locale" Core.$
      \x ->
        Locale'
          Core.<$> (x Core..: "Country") Core.<*> (x Core..:? "Subdivision")
