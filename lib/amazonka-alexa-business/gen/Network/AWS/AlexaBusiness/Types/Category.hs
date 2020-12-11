-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Category
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Category
  ( Category (..),

    -- * Smart constructor
    mkCategory,

    -- * Lenses
    cCategoryName,
    cCategoryId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The skill store category that is shown. Alexa skills are assigned a specific skill category during creation, such as News, Social, and Sports.
--
-- /See:/ 'mkCategory' smart constructor.
data Category = Category'
  { categoryName :: Lude.Maybe Lude.Text,
    categoryId :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Category' with the minimum fields required to make a request.
--
-- * 'categoryId' - The ID of the skill store category.
-- * 'categoryName' - The name of the skill store category.
mkCategory ::
  Category
mkCategory =
  Category' {categoryName = Lude.Nothing, categoryId = Lude.Nothing}

-- | The name of the skill store category.
--
-- /Note:/ Consider using 'categoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCategoryName :: Lens.Lens' Category (Lude.Maybe Lude.Text)
cCategoryName = Lens.lens (categoryName :: Category -> Lude.Maybe Lude.Text) (\s a -> s {categoryName = a} :: Category)
{-# DEPRECATED cCategoryName "Use generic-lens or generic-optics with 'categoryName' instead." #-}

-- | The ID of the skill store category.
--
-- /Note:/ Consider using 'categoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCategoryId :: Lens.Lens' Category (Lude.Maybe Lude.Natural)
cCategoryId = Lens.lens (categoryId :: Category -> Lude.Maybe Lude.Natural) (\s a -> s {categoryId = a} :: Category)
{-# DEPRECATED cCategoryId "Use generic-lens or generic-optics with 'categoryId' instead." #-}

instance Lude.FromJSON Category where
  parseJSON =
    Lude.withObject
      "Category"
      ( \x ->
          Category'
            Lude.<$> (x Lude..:? "CategoryName") Lude.<*> (x Lude..:? "CategoryId")
      )
