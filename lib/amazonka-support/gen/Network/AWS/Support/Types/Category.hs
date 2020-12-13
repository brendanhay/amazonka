{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Category
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.Category
  ( Category (..),

    -- * Smart constructor
    mkCategory,

    -- * Lenses
    cName,
    cCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A JSON-formatted name/value pair that represents the category name and category code of the problem, selected from the 'DescribeServices' response for each AWS service.
--
-- /See:/ 'mkCategory' smart constructor.
data Category = Category'
  { -- | The category name for the support case.
    name :: Lude.Maybe Lude.Text,
    -- | The category code for the support case.
    code :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Category' with the minimum fields required to make a request.
--
-- * 'name' - The category name for the support case.
-- * 'code' - The category code for the support case.
mkCategory ::
  Category
mkCategory = Category' {name = Lude.Nothing, code = Lude.Nothing}

-- | The category name for the support case.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Category (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: Category -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Category)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The category code for the support case.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCode :: Lens.Lens' Category (Lude.Maybe Lude.Text)
cCode = Lens.lens (code :: Category -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: Category)
{-# DEPRECATED cCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromJSON Category where
  parseJSON =
    Lude.withObject
      "Category"
      ( \x ->
          Category'
            Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "code")
      )
