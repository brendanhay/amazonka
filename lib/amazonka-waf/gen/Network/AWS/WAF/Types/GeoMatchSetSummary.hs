{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.GeoMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchSetSummary
  ( GeoMatchSetSummary (..),

    -- * Smart constructor
    mkGeoMatchSetSummary,

    -- * Lenses
    gmssGeoMatchSetId,
    gmssName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ResourceId as Types
import qualified Network.AWS.WAF.Types.ResourceName as Types

-- | Contains the identifier and the name of the @GeoMatchSet@ .
--
-- /See:/ 'mkGeoMatchSetSummary' smart constructor.
data GeoMatchSetSummary = GeoMatchSetSummary'
  { -- | The @GeoMatchSetId@ for an 'GeoMatchSet' . You can use @GeoMatchSetId@ in a 'GetGeoMatchSet' request to get detailed information about an 'GeoMatchSet' .
    geoMatchSetId :: Types.ResourceId,
    -- | A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
    name :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GeoMatchSetSummary' value with any optional fields omitted.
mkGeoMatchSetSummary ::
  -- | 'geoMatchSetId'
  Types.ResourceId ->
  -- | 'name'
  Types.ResourceName ->
  GeoMatchSetSummary
mkGeoMatchSetSummary geoMatchSetId name =
  GeoMatchSetSummary' {geoMatchSetId, name}

-- | The @GeoMatchSetId@ for an 'GeoMatchSet' . You can use @GeoMatchSetId@ in a 'GetGeoMatchSet' request to get detailed information about an 'GeoMatchSet' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmssGeoMatchSetId :: Lens.Lens' GeoMatchSetSummary Types.ResourceId
gmssGeoMatchSetId = Lens.field @"geoMatchSetId"
{-# DEPRECATED gmssGeoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead." #-}

-- | A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmssName :: Lens.Lens' GeoMatchSetSummary Types.ResourceName
gmssName = Lens.field @"name"
{-# DEPRECATED gmssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GeoMatchSetSummary where
  parseJSON =
    Core.withObject "GeoMatchSetSummary" Core.$
      \x ->
        GeoMatchSetSummary'
          Core.<$> (x Core..: "GeoMatchSetId") Core.<*> (x Core..: "Name")
