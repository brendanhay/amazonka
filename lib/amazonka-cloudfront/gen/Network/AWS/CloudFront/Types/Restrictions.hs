{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Restrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Restrictions
  ( Restrictions (..),

    -- * Smart constructor
    mkRestrictions,

    -- * Lenses
    rGeoRestriction,
  )
where

import qualified Network.AWS.CloudFront.Types.GeoRestriction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
--
-- /See:/ 'mkRestrictions' smart constructor.
newtype Restrictions = Restrictions'
  { -- | A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
    geoRestriction :: Types.GeoRestriction
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Restrictions' value with any optional fields omitted.
mkRestrictions ::
  -- | 'geoRestriction'
  Types.GeoRestriction ->
  Restrictions
mkRestrictions geoRestriction = Restrictions' {geoRestriction}

-- | A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
--
-- /Note:/ Consider using 'geoRestriction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGeoRestriction :: Lens.Lens' Restrictions Types.GeoRestriction
rGeoRestriction = Lens.field @"geoRestriction"
{-# DEPRECATED rGeoRestriction "Use generic-lens or generic-optics with 'geoRestriction' instead." #-}

instance Core.ToXML Restrictions where
  toXML Restrictions {..} =
    Core.toXMLNode "GeoRestriction" geoRestriction

instance Core.FromXML Restrictions where
  parseXML x = Restrictions' Core.<$> (x Core..@ "GeoRestriction")
