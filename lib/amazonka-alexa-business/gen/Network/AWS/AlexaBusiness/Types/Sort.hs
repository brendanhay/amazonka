{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Sort
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Sort
  ( Sort (..)
  -- * Smart constructor
  , mkSort
  -- * Lenses
  , sfKey
  , sfValue
  ) where

import qualified Network.AWS.AlexaBusiness.Types.SortKey as Types
import qualified Network.AWS.AlexaBusiness.Types.SortValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a sort criteria. 
--
-- /See:/ 'mkSort' smart constructor.
data Sort = Sort'
  { key :: Types.SortKey
    -- ^ The sort key of a sort object.
  , value :: Types.SortValue
    -- ^ The sort value of a sort object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Sort' value with any optional fields omitted.
mkSort
    :: Types.SortKey -- ^ 'key'
    -> Types.SortValue -- ^ 'value'
    -> Sort
mkSort key value = Sort'{key, value}

-- | The sort key of a sort object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfKey :: Lens.Lens' Sort Types.SortKey
sfKey = Lens.field @"key"
{-# INLINEABLE sfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The sort value of a sort object.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValue :: Lens.Lens' Sort Types.SortValue
sfValue = Lens.field @"value"
{-# INLINEABLE sfValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Sort where
        toJSON Sort{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key), Core.Just ("Value" Core..= value)])
