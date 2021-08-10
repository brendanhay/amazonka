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
import qualified Network.AWS.Prelude as Prelude

-- | The skill store category that is shown. Alexa skills are assigned a
-- specific skill category during creation, such as News, Social, and
-- Sports.
--
-- /See:/ 'newCategory' smart constructor.
data Category = Category'
  { -- | The ID of the skill store category.
    categoryId :: Prelude.Maybe Prelude.Natural,
    -- | The name of the skill store category.
    categoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { categoryId = Prelude.Nothing,
      categoryName = Prelude.Nothing
    }

-- | The ID of the skill store category.
category_categoryId :: Lens.Lens' Category (Prelude.Maybe Prelude.Natural)
category_categoryId = Lens.lens (\Category' {categoryId} -> categoryId) (\s@Category' {} a -> s {categoryId = a} :: Category)

-- | The name of the skill store category.
category_categoryName :: Lens.Lens' Category (Prelude.Maybe Prelude.Text)
category_categoryName = Lens.lens (\Category' {categoryName} -> categoryName) (\s@Category' {} a -> s {categoryName = a} :: Category)

instance Core.FromJSON Category where
  parseJSON =
    Core.withObject
      "Category"
      ( \x ->
          Category'
            Prelude.<$> (x Core..:? "CategoryId")
            Prelude.<*> (x Core..:? "CategoryName")
      )

instance Prelude.Hashable Category

instance Prelude.NFData Category
