{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Category
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Category where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The skill store category that is shown. Alexa skills are assigned a
-- specific skill category during creation, such as News, Social, and
-- Sports.
--
-- /See:/ 'newCategory' smart constructor.
data Category = Category'
  { -- | The ID of the skill store category.
    categoryId :: Core.Maybe Core.Natural,
    -- | The name of the skill store category.
    categoryName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Category' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryId', 'category_categoryId' - The ID of the skill store category.
--
-- 'categoryName', 'category_categoryName' - The name of the skill store category.
newCategory ::
  Category
newCategory =
  Category'
    { categoryId = Core.Nothing,
      categoryName = Core.Nothing
    }

-- | The ID of the skill store category.
category_categoryId :: Lens.Lens' Category (Core.Maybe Core.Natural)
category_categoryId = Lens.lens (\Category' {categoryId} -> categoryId) (\s@Category' {} a -> s {categoryId = a} :: Category)

-- | The name of the skill store category.
category_categoryName :: Lens.Lens' Category (Core.Maybe Core.Text)
category_categoryName = Lens.lens (\Category' {categoryName} -> categoryName) (\s@Category' {} a -> s {categoryName = a} :: Category)

instance Core.FromJSON Category where
  parseJSON =
    Core.withObject
      "Category"
      ( \x ->
          Category'
            Core.<$> (x Core..:? "CategoryId")
            Core.<*> (x Core..:? "CategoryName")
      )

instance Core.Hashable Category

instance Core.NFData Category
