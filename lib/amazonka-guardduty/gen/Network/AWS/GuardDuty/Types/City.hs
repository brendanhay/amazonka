{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.City
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.City
  ( City (..)
  -- * Smart constructor
  , mkCity
  -- * Lenses
  , cCityName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the city associated with the IP address.
--
-- /See:/ 'mkCity' smart constructor.
newtype City = City'
  { cityName :: Core.Maybe Core.Text
    -- ^ The city name of the remote IP address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'City' value with any optional fields omitted.
mkCity
    :: City
mkCity = City'{cityName = Core.Nothing}

-- | The city name of the remote IP address.
--
-- /Note:/ Consider using 'cityName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCityName :: Lens.Lens' City (Core.Maybe Core.Text)
cCityName = Lens.field @"cityName"
{-# INLINEABLE cCityName #-}
{-# DEPRECATED cityName "Use generic-lens or generic-optics with 'cityName' instead"  #-}

instance Core.FromJSON City where
        parseJSON
          = Core.withObject "City" Core.$
              \ x -> City' Core.<$> (x Core..:? "cityName")
