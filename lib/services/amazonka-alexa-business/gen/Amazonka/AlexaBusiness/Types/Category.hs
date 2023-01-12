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
-- Module      : Amazonka.AlexaBusiness.Types.Category
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Category where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON Category where
  parseJSON =
    Data.withObject
      "Category"
      ( \x ->
          Category'
            Prelude.<$> (x Data..:? "CategoryId")
            Prelude.<*> (x Data..:? "CategoryName")
      )

instance Prelude.Hashable Category where
  hashWithSalt _salt Category' {..} =
    _salt `Prelude.hashWithSalt` categoryId
      `Prelude.hashWithSalt` categoryName

instance Prelude.NFData Category where
  rnf Category' {..} =
    Prelude.rnf categoryId
      `Prelude.seq` Prelude.rnf categoryName
