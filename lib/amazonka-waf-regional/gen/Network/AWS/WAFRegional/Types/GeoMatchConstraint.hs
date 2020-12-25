{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.GeoMatchConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.GeoMatchConstraint
  ( GeoMatchConstraint (..),

    -- * Smart constructor
    mkGeoMatchConstraint,

    -- * Lenses
    gmcType,
    gmcValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.GeoMatchConstraintType as Types
import qualified Network.AWS.WAFRegional.Types.GeoMatchConstraintValue as Types

-- | The country from which web requests originate that you want AWS WAF to search for.
--
-- /See:/ 'mkGeoMatchConstraint' smart constructor.
data GeoMatchConstraint = GeoMatchConstraint'
  { -- | The type of geographical area you want AWS WAF to search for. Currently @Country@ is the only valid value.
    type' :: Types.GeoMatchConstraintType,
    -- | The country that you want AWS WAF to search for.
    value :: Types.GeoMatchConstraintValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GeoMatchConstraint' value with any optional fields omitted.
mkGeoMatchConstraint ::
  -- | 'type\''
  Types.GeoMatchConstraintType ->
  -- | 'value'
  Types.GeoMatchConstraintValue ->
  GeoMatchConstraint
mkGeoMatchConstraint type' value =
  GeoMatchConstraint' {type', value}

-- | The type of geographical area you want AWS WAF to search for. Currently @Country@ is the only valid value.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcType :: Lens.Lens' GeoMatchConstraint Types.GeoMatchConstraintType
gmcType = Lens.field @"type'"
{-# DEPRECATED gmcType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The country that you want AWS WAF to search for.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcValue :: Lens.Lens' GeoMatchConstraint Types.GeoMatchConstraintValue
gmcValue = Lens.field @"value"
{-# DEPRECATED gmcValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON GeoMatchConstraint where
  toJSON GeoMatchConstraint {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            Core.Just ("Value" Core..= value)
          ]
      )

instance Core.FromJSON GeoMatchConstraint where
  parseJSON =
    Core.withObject "GeoMatchConstraint" Core.$
      \x ->
        GeoMatchConstraint'
          Core.<$> (x Core..: "Type") Core.<*> (x Core..: "Value")
