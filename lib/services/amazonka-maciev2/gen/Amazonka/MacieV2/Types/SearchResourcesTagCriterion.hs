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
-- Module      : Amazonka.MacieV2.Types.SearchResourcesTagCriterion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SearchResourcesTagCriterion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.SearchResourcesComparator
import Amazonka.MacieV2.Types.SearchResourcesTagCriterionPair
import qualified Amazonka.Prelude as Prelude

-- | Specifies a tag-based filter condition that determines which Amazon Web
-- Services resources are included or excluded from the query results.
--
-- /See:/ 'newSearchResourcesTagCriterion' smart constructor.
data SearchResourcesTagCriterion = SearchResourcesTagCriterion'
  { -- | The tag keys, tag values, or tag key and value pairs to use in the
    -- condition.
    tagValues :: Prelude.Maybe [SearchResourcesTagCriterionPair],
    -- | The operator to use in the condition. Valid values are EQ (equals) and
    -- NE (not equals).
    comparator :: Prelude.Maybe SearchResourcesComparator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesTagCriterion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagValues', 'searchResourcesTagCriterion_tagValues' - The tag keys, tag values, or tag key and value pairs to use in the
-- condition.
--
-- 'comparator', 'searchResourcesTagCriterion_comparator' - The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
newSearchResourcesTagCriterion ::
  SearchResourcesTagCriterion
newSearchResourcesTagCriterion =
  SearchResourcesTagCriterion'
    { tagValues =
        Prelude.Nothing,
      comparator = Prelude.Nothing
    }

-- | The tag keys, tag values, or tag key and value pairs to use in the
-- condition.
searchResourcesTagCriterion_tagValues :: Lens.Lens' SearchResourcesTagCriterion (Prelude.Maybe [SearchResourcesTagCriterionPair])
searchResourcesTagCriterion_tagValues = Lens.lens (\SearchResourcesTagCriterion' {tagValues} -> tagValues) (\s@SearchResourcesTagCriterion' {} a -> s {tagValues = a} :: SearchResourcesTagCriterion) Prelude.. Lens.mapping Lens.coerced

-- | The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
searchResourcesTagCriterion_comparator :: Lens.Lens' SearchResourcesTagCriterion (Prelude.Maybe SearchResourcesComparator)
searchResourcesTagCriterion_comparator = Lens.lens (\SearchResourcesTagCriterion' {comparator} -> comparator) (\s@SearchResourcesTagCriterion' {} a -> s {comparator = a} :: SearchResourcesTagCriterion)

instance Prelude.Hashable SearchResourcesTagCriterion where
  hashWithSalt _salt SearchResourcesTagCriterion' {..} =
    _salt `Prelude.hashWithSalt` tagValues
      `Prelude.hashWithSalt` comparator

instance Prelude.NFData SearchResourcesTagCriterion where
  rnf SearchResourcesTagCriterion' {..} =
    Prelude.rnf tagValues
      `Prelude.seq` Prelude.rnf comparator

instance Core.ToJSON SearchResourcesTagCriterion where
  toJSON SearchResourcesTagCriterion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tagValues" Core..=) Prelude.<$> tagValues,
            ("comparator" Core..=) Prelude.<$> comparator
          ]
      )
