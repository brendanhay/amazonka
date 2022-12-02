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
-- Module      : Amazonka.ElasticBeanstalk.Types.SearchFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.SearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes criteria to restrict a list of results.
--
-- For operators that apply a single value to the attribute, the filter is
-- evaluated as follows: @Attribute Operator Values[1]@
--
-- Some operators, e.g. @in@, can apply multiple values. In this case, the
-- filter is evaluated as a logical union (OR) of applications of the
-- operator to the attribute with each one of the values:
-- @(Attribute Operator Values[1]) OR (Attribute Operator Values[2]) OR ...@
--
-- The valid values for attributes of @SearchFilter@ depend on the API
-- action. For valid values, see the reference page for the API action
-- you\'re calling that takes a @SearchFilter@ parameter.
--
-- /See:/ 'newSearchFilter' smart constructor.
data SearchFilter = SearchFilter'
  { -- | The result attribute to which the filter values are applied. Valid
    -- values vary by API action.
    attribute :: Prelude.Maybe Prelude.Text,
    -- | The operator to apply to the @Attribute@ with each of the @Values@.
    -- Valid values vary by @Attribute@.
    operator :: Prelude.Maybe Prelude.Text,
    -- | The list of values applied to the @Attribute@ and @Operator@ attributes.
    -- Number of values and valid values vary by @Attribute@.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'searchFilter_attribute' - The result attribute to which the filter values are applied. Valid
-- values vary by API action.
--
-- 'operator', 'searchFilter_operator' - The operator to apply to the @Attribute@ with each of the @Values@.
-- Valid values vary by @Attribute@.
--
-- 'values', 'searchFilter_values' - The list of values applied to the @Attribute@ and @Operator@ attributes.
-- Number of values and valid values vary by @Attribute@.
newSearchFilter ::
  SearchFilter
newSearchFilter =
  SearchFilter'
    { attribute = Prelude.Nothing,
      operator = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The result attribute to which the filter values are applied. Valid
-- values vary by API action.
searchFilter_attribute :: Lens.Lens' SearchFilter (Prelude.Maybe Prelude.Text)
searchFilter_attribute = Lens.lens (\SearchFilter' {attribute} -> attribute) (\s@SearchFilter' {} a -> s {attribute = a} :: SearchFilter)

-- | The operator to apply to the @Attribute@ with each of the @Values@.
-- Valid values vary by @Attribute@.
searchFilter_operator :: Lens.Lens' SearchFilter (Prelude.Maybe Prelude.Text)
searchFilter_operator = Lens.lens (\SearchFilter' {operator} -> operator) (\s@SearchFilter' {} a -> s {operator = a} :: SearchFilter)

-- | The list of values applied to the @Attribute@ and @Operator@ attributes.
-- Number of values and valid values vary by @Attribute@.
searchFilter_values :: Lens.Lens' SearchFilter (Prelude.Maybe [Prelude.Text])
searchFilter_values = Lens.lens (\SearchFilter' {values} -> values) (\s@SearchFilter' {} a -> s {values = a} :: SearchFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable SearchFilter where
  hashWithSalt _salt SearchFilter' {..} =
    _salt `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` values

instance Prelude.NFData SearchFilter where
  rnf SearchFilter' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf values

instance Data.ToQuery SearchFilter where
  toQuery SearchFilter' {..} =
    Prelude.mconcat
      [ "Attribute" Data.=: attribute,
        "Operator" Data.=: operator,
        "Values"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> values)
      ]
