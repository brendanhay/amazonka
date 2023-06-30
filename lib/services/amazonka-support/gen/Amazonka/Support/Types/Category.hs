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
-- Module      : Amazonka.Support.Types.Category
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.Category where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A JSON-formatted name\/value pair that represents the category name and
-- category code of the problem, selected from the DescribeServices
-- response for each Amazon Web Services service.
--
-- /See:/ 'newCategory' smart constructor.
data Category = Category'
  { -- | The category code for the support case.
    code :: Prelude.Maybe Prelude.Text,
    -- | The category name for the support case.
    name :: Prelude.Maybe Prelude.Text
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

instance Data.FromJSON Category where
  parseJSON =
    Data.withObject
      "Category"
      ( \x ->
          Category'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable Category where
  hashWithSalt _salt Category' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` name

instance Prelude.NFData Category where
  rnf Category' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf name
