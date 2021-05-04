{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A JSON-formatted name\/value pair that represents the category name and
-- category code of the problem, selected from the DescribeServices
-- response for each AWS service.
--
-- /See:/ 'newCategory' smart constructor.
data Category = Category'
  { -- | The category code for the support case.
    code :: Prelude.Maybe Prelude.Text,
    -- | The category name for the support case.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Category'
    { code = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The category code for the support case.
category_code :: Lens.Lens' Category (Prelude.Maybe Prelude.Text)
category_code = Lens.lens (\Category' {code} -> code) (\s@Category' {} a -> s {code = a} :: Category)

-- | The category name for the support case.
category_name :: Lens.Lens' Category (Prelude.Maybe Prelude.Text)
category_name = Lens.lens (\Category' {name} -> name) (\s@Category' {} a -> s {name = a} :: Category)

instance Prelude.FromJSON Category where
  parseJSON =
    Prelude.withObject
      "Category"
      ( \x ->
          Category'
            Prelude.<$> (x Prelude..:? "code")
            Prelude.<*> (x Prelude..:? "name")
      )

instance Prelude.Hashable Category

instance Prelude.NFData Category
