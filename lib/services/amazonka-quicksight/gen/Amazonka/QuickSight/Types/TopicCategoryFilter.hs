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
-- Module      : Amazonka.QuickSight.Types.TopicCategoryFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicCategoryFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoryFilterFunction
import Amazonka.QuickSight.Types.CategoryFilterType
import Amazonka.QuickSight.Types.TopicCategoryFilterConstant

-- | A structure that represents a category filter.
--
-- /See:/ 'newTopicCategoryFilter' smart constructor.
data TopicCategoryFilter = TopicCategoryFilter'
  { -- | The category filter function. Valid values for this structure are
    -- @EXACT@ and @CONTAINS@.
    categoryFilterFunction :: Prelude.Maybe CategoryFilterFunction,
    -- | The category filter type. This element is used to specify whether a
    -- filter is a simple category filter or an inverse category filter.
    categoryFilterType :: Prelude.Maybe CategoryFilterType,
    -- | The constant used in a category filter.
    constant :: Prelude.Maybe (Data.Sensitive TopicCategoryFilterConstant),
    -- | A Boolean value that indicates if the filter is inverse.
    inverse :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicCategoryFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryFilterFunction', 'topicCategoryFilter_categoryFilterFunction' - The category filter function. Valid values for this structure are
-- @EXACT@ and @CONTAINS@.
--
-- 'categoryFilterType', 'topicCategoryFilter_categoryFilterType' - The category filter type. This element is used to specify whether a
-- filter is a simple category filter or an inverse category filter.
--
-- 'constant', 'topicCategoryFilter_constant' - The constant used in a category filter.
--
-- 'inverse', 'topicCategoryFilter_inverse' - A Boolean value that indicates if the filter is inverse.
newTopicCategoryFilter ::
  TopicCategoryFilter
newTopicCategoryFilter =
  TopicCategoryFilter'
    { categoryFilterFunction =
        Prelude.Nothing,
      categoryFilterType = Prelude.Nothing,
      constant = Prelude.Nothing,
      inverse = Prelude.Nothing
    }

-- | The category filter function. Valid values for this structure are
-- @EXACT@ and @CONTAINS@.
topicCategoryFilter_categoryFilterFunction :: Lens.Lens' TopicCategoryFilter (Prelude.Maybe CategoryFilterFunction)
topicCategoryFilter_categoryFilterFunction = Lens.lens (\TopicCategoryFilter' {categoryFilterFunction} -> categoryFilterFunction) (\s@TopicCategoryFilter' {} a -> s {categoryFilterFunction = a} :: TopicCategoryFilter)

-- | The category filter type. This element is used to specify whether a
-- filter is a simple category filter or an inverse category filter.
topicCategoryFilter_categoryFilterType :: Lens.Lens' TopicCategoryFilter (Prelude.Maybe CategoryFilterType)
topicCategoryFilter_categoryFilterType = Lens.lens (\TopicCategoryFilter' {categoryFilterType} -> categoryFilterType) (\s@TopicCategoryFilter' {} a -> s {categoryFilterType = a} :: TopicCategoryFilter)

-- | The constant used in a category filter.
topicCategoryFilter_constant :: Lens.Lens' TopicCategoryFilter (Prelude.Maybe TopicCategoryFilterConstant)
topicCategoryFilter_constant = Lens.lens (\TopicCategoryFilter' {constant} -> constant) (\s@TopicCategoryFilter' {} a -> s {constant = a} :: TopicCategoryFilter) Prelude.. Lens.mapping Data._Sensitive

-- | A Boolean value that indicates if the filter is inverse.
topicCategoryFilter_inverse :: Lens.Lens' TopicCategoryFilter (Prelude.Maybe Prelude.Bool)
topicCategoryFilter_inverse = Lens.lens (\TopicCategoryFilter' {inverse} -> inverse) (\s@TopicCategoryFilter' {} a -> s {inverse = a} :: TopicCategoryFilter)

instance Data.FromJSON TopicCategoryFilter where
  parseJSON =
    Data.withObject
      "TopicCategoryFilter"
      ( \x ->
          TopicCategoryFilter'
            Prelude.<$> (x Data..:? "CategoryFilterFunction")
            Prelude.<*> (x Data..:? "CategoryFilterType")
            Prelude.<*> (x Data..:? "Constant")
            Prelude.<*> (x Data..:? "Inverse")
      )

instance Prelude.Hashable TopicCategoryFilter where
  hashWithSalt _salt TopicCategoryFilter' {..} =
    _salt
      `Prelude.hashWithSalt` categoryFilterFunction
      `Prelude.hashWithSalt` categoryFilterType
      `Prelude.hashWithSalt` constant
      `Prelude.hashWithSalt` inverse

instance Prelude.NFData TopicCategoryFilter where
  rnf TopicCategoryFilter' {..} =
    Prelude.rnf categoryFilterFunction
      `Prelude.seq` Prelude.rnf categoryFilterType
      `Prelude.seq` Prelude.rnf constant
      `Prelude.seq` Prelude.rnf inverse

instance Data.ToJSON TopicCategoryFilter where
  toJSON TopicCategoryFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryFilterFunction" Data..=)
              Prelude.<$> categoryFilterFunction,
            ("CategoryFilterType" Data..=)
              Prelude.<$> categoryFilterType,
            ("Constant" Data..=) Prelude.<$> constant,
            ("Inverse" Data..=) Prelude.<$> inverse
          ]
      )
