{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Country
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Country
  ( Country (..),

    -- * Smart constructor
    mkCountry,

    -- * Lenses
    cCountryCode,
    cCountryName,
  )
where

import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the country where the remote IP address is located.
--
-- /See:/ 'mkCountry' smart constructor.
data Country = Country'
  { -- | The country code of the remote IP address.
    countryCode :: Core.Maybe Types.String,
    -- | The country name of the remote IP address.
    countryName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Country' value with any optional fields omitted.
mkCountry ::
  Country
mkCountry =
  Country' {countryCode = Core.Nothing, countryName = Core.Nothing}

-- | The country code of the remote IP address.
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCountryCode :: Lens.Lens' Country (Core.Maybe Types.String)
cCountryCode = Lens.field @"countryCode"
{-# DEPRECATED cCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

-- | The country name of the remote IP address.
--
-- /Note:/ Consider using 'countryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCountryName :: Lens.Lens' Country (Core.Maybe Types.String)
cCountryName = Lens.field @"countryName"
{-# DEPRECATED cCountryName "Use generic-lens or generic-optics with 'countryName' instead." #-}

instance Core.FromJSON Country where
  parseJSON =
    Core.withObject "Country" Core.$
      \x ->
        Country'
          Core.<$> (x Core..:? "countryCode") Core.<*> (x Core..:? "countryName")
