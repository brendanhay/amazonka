{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Category
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.Category
  ( Category (..)
  -- * Smart constructor
  , mkCategory
  -- * Lenses
  , cCode
  , cName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.Code as Types
import qualified Network.AWS.Support.Types.Name as Types

-- | A JSON-formatted name/value pair that represents the category name and category code of the problem, selected from the 'DescribeServices' response for each AWS service.
--
-- /See:/ 'mkCategory' smart constructor.
data Category = Category'
  { code :: Core.Maybe Types.Code
    -- ^ The category code for the support case.
  , name :: Core.Maybe Types.Name
    -- ^ The category name for the support case.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Category' value with any optional fields omitted.
mkCategory
    :: Category
mkCategory = Category'{code = Core.Nothing, name = Core.Nothing}

-- | The category code for the support case.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCode :: Lens.Lens' Category (Core.Maybe Types.Code)
cCode = Lens.field @"code"
{-# INLINEABLE cCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The category name for the support case.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Category (Core.Maybe Types.Name)
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON Category where
        parseJSON
          = Core.withObject "Category" Core.$
              \ x ->
                Category' Core.<$> (x Core..:? "code") Core.<*> x Core..:? "name"
