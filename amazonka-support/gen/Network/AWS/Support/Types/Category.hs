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
-- Module      : Network.AWS.Support.Types.Category
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.Category where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A JSON-formatted name\/value pair that represents the category name and
-- category code of the problem, selected from the DescribeServices
-- response for each AWS service.
--
-- /See:/ 'newCategory' smart constructor.
data Category = Category'
  { -- | The category code for the support case.
    code :: Core.Maybe Core.Text,
    -- | The category name for the support case.
    name :: Core.Maybe Core.Text
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
-- 'code', 'category_code' - The category code for the support case.
--
-- 'name', 'category_name' - The category name for the support case.
newCategory ::
  Category
newCategory =
  Category' {code = Core.Nothing, name = Core.Nothing}

-- | The category code for the support case.
category_code :: Lens.Lens' Category (Core.Maybe Core.Text)
category_code = Lens.lens (\Category' {code} -> code) (\s@Category' {} a -> s {code = a} :: Category)

-- | The category name for the support case.
category_name :: Lens.Lens' Category (Core.Maybe Core.Text)
category_name = Lens.lens (\Category' {name} -> name) (\s@Category' {} a -> s {name = a} :: Category)

instance Core.FromJSON Category where
  parseJSON =
    Core.withObject
      "Category"
      ( \x ->
          Category'
            Core.<$> (x Core..:? "code") Core.<*> (x Core..:? "name")
      )

instance Core.Hashable Category

instance Core.NFData Category
