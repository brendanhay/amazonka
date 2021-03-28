{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PropertyPredicate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.PropertyPredicate
  ( PropertyPredicate (..)
  -- * Smart constructor
  , mkPropertyPredicate
  -- * Lenses
  , ppComparator
  , ppKey
  , ppValue
  ) where

import qualified Network.AWS.Glue.Types.Comparator as Types
import qualified Network.AWS.Glue.Types.Key as Types
import qualified Network.AWS.Glue.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines a property predicate.
--
-- /See:/ 'mkPropertyPredicate' smart constructor.
data PropertyPredicate = PropertyPredicate'
  { comparator :: Core.Maybe Types.Comparator
    -- ^ The comparator used to compare this property to others.
  , key :: Core.Maybe Types.Key
    -- ^ The key of the property.
  , value :: Core.Maybe Types.Value
    -- ^ The value of the property.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PropertyPredicate' value with any optional fields omitted.
mkPropertyPredicate
    :: PropertyPredicate
mkPropertyPredicate
  = PropertyPredicate'{comparator = Core.Nothing, key = Core.Nothing,
                       value = Core.Nothing}

-- | The comparator used to compare this property to others.
--
-- /Note:/ Consider using 'comparator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppComparator :: Lens.Lens' PropertyPredicate (Core.Maybe Types.Comparator)
ppComparator = Lens.field @"comparator"
{-# INLINEABLE ppComparator #-}
{-# DEPRECATED comparator "Use generic-lens or generic-optics with 'comparator' instead"  #-}

-- | The key of the property.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppKey :: Lens.Lens' PropertyPredicate (Core.Maybe Types.Key)
ppKey = Lens.field @"key"
{-# INLINEABLE ppKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value of the property.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValue :: Lens.Lens' PropertyPredicate (Core.Maybe Types.Value)
ppValue = Lens.field @"value"
{-# INLINEABLE ppValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON PropertyPredicate where
        toJSON PropertyPredicate{..}
          = Core.object
              (Core.catMaybes
                 [("Comparator" Core..=) Core.<$> comparator,
                  ("Key" Core..=) Core.<$> key, ("Value" Core..=) Core.<$> value])
