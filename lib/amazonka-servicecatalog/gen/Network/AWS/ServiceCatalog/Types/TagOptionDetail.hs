{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.TagOptionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.TagOptionDetail
  ( TagOptionDetail (..)
  -- * Smart constructor
  , mkTagOptionDetail
  -- * Lenses
  , todActive
  , todId
  , todKey
  , todValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Key as Types
import qualified Network.AWS.ServiceCatalog.Types.TagOptionId as Types
import qualified Network.AWS.ServiceCatalog.Types.TagOptionValue as Types

-- | Information about a TagOption.
--
-- /See:/ 'mkTagOptionDetail' smart constructor.
data TagOptionDetail = TagOptionDetail'
  { active :: Core.Maybe Core.Bool
    -- ^ The TagOption active state.
  , id :: Core.Maybe Types.TagOptionId
    -- ^ The TagOption identifier.
  , key :: Core.Maybe Types.Key
    -- ^ The TagOption key.
  , value :: Core.Maybe Types.TagOptionValue
    -- ^ The TagOption value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagOptionDetail' value with any optional fields omitted.
mkTagOptionDetail
    :: TagOptionDetail
mkTagOptionDetail
  = TagOptionDetail'{active = Core.Nothing, id = Core.Nothing,
                     key = Core.Nothing, value = Core.Nothing}

-- | The TagOption active state.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
todActive :: Lens.Lens' TagOptionDetail (Core.Maybe Core.Bool)
todActive = Lens.field @"active"
{-# INLINEABLE todActive #-}
{-# DEPRECATED active "Use generic-lens or generic-optics with 'active' instead"  #-}

-- | The TagOption identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
todId :: Lens.Lens' TagOptionDetail (Core.Maybe Types.TagOptionId)
todId = Lens.field @"id"
{-# INLINEABLE todId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The TagOption key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
todKey :: Lens.Lens' TagOptionDetail (Core.Maybe Types.Key)
todKey = Lens.field @"key"
{-# INLINEABLE todKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The TagOption value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
todValue :: Lens.Lens' TagOptionDetail (Core.Maybe Types.TagOptionValue)
todValue = Lens.field @"value"
{-# INLINEABLE todValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON TagOptionDetail where
        parseJSON
          = Core.withObject "TagOptionDetail" Core.$
              \ x ->
                TagOptionDetail' Core.<$>
                  (x Core..:? "Active") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Key"
                    Core.<*> x Core..:? "Value"
