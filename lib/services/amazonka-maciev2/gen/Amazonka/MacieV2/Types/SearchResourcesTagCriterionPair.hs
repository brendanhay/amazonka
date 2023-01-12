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
-- Module      : Amazonka.MacieV2.Types.SearchResourcesTagCriterionPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SearchResourcesTagCriterionPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a tag key, a tag value, or a tag key and value (as a pair) to
-- use in a tag-based filter condition for a query. Tag keys and values are
-- case sensitive. Also, Amazon Macie doesn\'t support use of partial
-- values or wildcard characters in tag-based filter conditions.
--
-- /See:/ 'newSearchResourcesTagCriterionPair' smart constructor.
data SearchResourcesTagCriterionPair = SearchResourcesTagCriterionPair'
  { -- | The value for the tag key to use in the condition.
    key :: Prelude.Maybe Prelude.Text,
    -- | The tag value to use in the condition.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesTagCriterionPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'searchResourcesTagCriterionPair_key' - The value for the tag key to use in the condition.
--
-- 'value', 'searchResourcesTagCriterionPair_value' - The tag value to use in the condition.
newSearchResourcesTagCriterionPair ::
  SearchResourcesTagCriterionPair
newSearchResourcesTagCriterionPair =
  SearchResourcesTagCriterionPair'
    { key =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The value for the tag key to use in the condition.
searchResourcesTagCriterionPair_key :: Lens.Lens' SearchResourcesTagCriterionPair (Prelude.Maybe Prelude.Text)
searchResourcesTagCriterionPair_key = Lens.lens (\SearchResourcesTagCriterionPair' {key} -> key) (\s@SearchResourcesTagCriterionPair' {} a -> s {key = a} :: SearchResourcesTagCriterionPair)

-- | The tag value to use in the condition.
searchResourcesTagCriterionPair_value :: Lens.Lens' SearchResourcesTagCriterionPair (Prelude.Maybe Prelude.Text)
searchResourcesTagCriterionPair_value = Lens.lens (\SearchResourcesTagCriterionPair' {value} -> value) (\s@SearchResourcesTagCriterionPair' {} a -> s {value = a} :: SearchResourcesTagCriterionPair)

instance
  Prelude.Hashable
    SearchResourcesTagCriterionPair
  where
  hashWithSalt
    _salt
    SearchResourcesTagCriterionPair' {..} =
      _salt `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    SearchResourcesTagCriterionPair
  where
  rnf SearchResourcesTagCriterionPair' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON SearchResourcesTagCriterionPair where
  toJSON SearchResourcesTagCriterionPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("key" Data..=) Prelude.<$> key,
            ("value" Data..=) Prelude.<$> value
          ]
      )
