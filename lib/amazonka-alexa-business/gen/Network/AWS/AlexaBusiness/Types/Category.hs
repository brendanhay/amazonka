{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Category
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Category
  ( Category (..)
  -- * Smart constructor
  , mkCategory
  -- * Lenses
  , cCategoryId
  , cCategoryName
  ) where

import qualified Network.AWS.AlexaBusiness.Types.CategoryName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The skill store category that is shown. Alexa skills are assigned a specific skill category during creation, such as News, Social, and Sports.
--
-- /See:/ 'mkCategory' smart constructor.
data Category = Category'
  { categoryId :: Core.Maybe Core.Natural
    -- ^ The ID of the skill store category.
  , categoryName :: Core.Maybe Types.CategoryName
    -- ^ The name of the skill store category.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Category' value with any optional fields omitted.
mkCategory
    :: Category
mkCategory
  = Category'{categoryId = Core.Nothing, categoryName = Core.Nothing}

-- | The ID of the skill store category.
--
-- /Note:/ Consider using 'categoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCategoryId :: Lens.Lens' Category (Core.Maybe Core.Natural)
cCategoryId = Lens.field @"categoryId"
{-# INLINEABLE cCategoryId #-}
{-# DEPRECATED categoryId "Use generic-lens or generic-optics with 'categoryId' instead"  #-}

-- | The name of the skill store category.
--
-- /Note:/ Consider using 'categoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCategoryName :: Lens.Lens' Category (Core.Maybe Types.CategoryName)
cCategoryName = Lens.field @"categoryName"
{-# INLINEABLE cCategoryName #-}
{-# DEPRECATED categoryName "Use generic-lens or generic-optics with 'categoryName' instead"  #-}

instance Core.FromJSON Category where
        parseJSON
          = Core.withObject "Category" Core.$
              \ x ->
                Category' Core.<$>
                  (x Core..:? "CategoryId") Core.<*> x Core..:? "CategoryName"
