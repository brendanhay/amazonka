{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationFilter
  ( AssociationFilter (..)
  -- * Smart constructor
  , mkAssociationFilter
  -- * Lenses
  , afKey
  , afValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationFilterKey as Types
import qualified Network.AWS.SSM.Types.Value as Types

-- | Describes a filter.
--
-- /See:/ 'mkAssociationFilter' smart constructor.
data AssociationFilter = AssociationFilter'
  { key :: Types.AssociationFilterKey
    -- ^ The name of the filter.
  , value :: Types.Value
    -- ^ The filter value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociationFilter' value with any optional fields omitted.
mkAssociationFilter
    :: Types.AssociationFilterKey -- ^ 'key'
    -> Types.Value -- ^ 'value'
    -> AssociationFilter
mkAssociationFilter key value = AssociationFilter'{key, value}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afKey :: Lens.Lens' AssociationFilter Types.AssociationFilterKey
afKey = Lens.field @"key"
{-# INLINEABLE afKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The filter value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afValue :: Lens.Lens' AssociationFilter Types.Value
afValue = Lens.field @"value"
{-# INLINEABLE afValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON AssociationFilter where
        toJSON AssociationFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("key" Core..= key), Core.Just ("value" Core..= value)])
