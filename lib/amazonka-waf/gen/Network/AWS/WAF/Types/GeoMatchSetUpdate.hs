{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.GeoMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.GeoMatchSetUpdate
  ( GeoMatchSetUpdate (..)
  -- * Smart constructor
  , mkGeoMatchSetUpdate
  -- * Lenses
  , gmsuAction
  , gmsuGeoMatchConstraint
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ChangeAction as Types
import qualified Network.AWS.WAF.Types.GeoMatchConstraint as Types

-- | Specifies the type of update to perform to an 'GeoMatchSet' with 'UpdateGeoMatchSet' .
--
-- /See:/ 'mkGeoMatchSetUpdate' smart constructor.
data GeoMatchSetUpdate = GeoMatchSetUpdate'
  { action :: Types.ChangeAction
    -- ^ Specifies whether to insert or delete a country with 'UpdateGeoMatchSet' .
  , geoMatchConstraint :: Types.GeoMatchConstraint
    -- ^ The country from which web requests originate that you want AWS WAF to search for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GeoMatchSetUpdate' value with any optional fields omitted.
mkGeoMatchSetUpdate
    :: Types.ChangeAction -- ^ 'action'
    -> Types.GeoMatchConstraint -- ^ 'geoMatchConstraint'
    -> GeoMatchSetUpdate
mkGeoMatchSetUpdate action geoMatchConstraint
  = GeoMatchSetUpdate'{action, geoMatchConstraint}

-- | Specifies whether to insert or delete a country with 'UpdateGeoMatchSet' .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsuAction :: Lens.Lens' GeoMatchSetUpdate Types.ChangeAction
gmsuAction = Lens.field @"action"
{-# INLINEABLE gmsuAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The country from which web requests originate that you want AWS WAF to search for.
--
-- /Note:/ Consider using 'geoMatchConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsuGeoMatchConstraint :: Lens.Lens' GeoMatchSetUpdate Types.GeoMatchConstraint
gmsuGeoMatchConstraint = Lens.field @"geoMatchConstraint"
{-# INLINEABLE gmsuGeoMatchConstraint #-}
{-# DEPRECATED geoMatchConstraint "Use generic-lens or generic-optics with 'geoMatchConstraint' instead"  #-}

instance Core.FromJSON GeoMatchSetUpdate where
        toJSON GeoMatchSetUpdate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Action" Core..= action),
                  Core.Just ("GeoMatchConstraint" Core..= geoMatchConstraint)])
