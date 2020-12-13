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

import Network.AWS.CloudFront.Types.GeoRestriction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that identifies ways in which you want to restrict distribution of your content.
--
-- /See:/ 'mkRestrictions' smart constructor.
newtype Restrictions = Restrictions'
  { -- | A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
    geoRestriction :: GeoRestriction
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Restrictions' with the minimum fields required to make a request.
--
-- * 'geoRestriction' - A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
mkRestrictions ::
  -- | 'geoRestriction'
  GeoRestriction ->
  Restrictions
mkRestrictions pGeoRestriction_ =
  Restrictions' {geoRestriction = pGeoRestriction_}

-- | A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
--
-- /Note:/ Consider using 'geoRestriction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGeoRestriction :: Lens.Lens' Restrictions GeoRestriction
rGeoRestriction = Lens.lens (geoRestriction :: Restrictions -> GeoRestriction) (\s a -> s {geoRestriction = a} :: Restrictions)
{-# DEPRECATED rGeoRestriction "Use generic-lens or generic-optics with 'geoRestriction' instead." #-}

instance Lude.FromXML Restrictions where
  parseXML x = Restrictions' Lude.<$> (x Lude..@ "GeoRestriction")

instance Lude.ToXML Restrictions where
  toXML Restrictions' {..} =
    Lude.mconcat ["GeoRestriction" Lude.@= geoRestriction]
