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
-- Module      : Network.AWS.ElasticBeanstalk.Types.SearchFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SearchFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  { -- | The list of values applied to the @Attribute@ and @Operator@ attributes.
    -- Number of values and valid values vary by @Attribute@.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The operator to apply to the @Attribute@ with each of the @Values@.
    -- Valid values vary by @Attribute@.
    operator :: Prelude.Maybe Prelude.Text,
    -- | The result attribute to which the filter values are applied. Valid
    -- values vary by API action.
    attribute :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'searchFilter_values' - The list of values applied to the @Attribute@ and @Operator@ attributes.
-- Number of values and valid values vary by @Attribute@.
--
-- 'operator', 'searchFilter_operator' - The operator to apply to the @Attribute@ with each of the @Values@.
-- Valid values vary by @Attribute@.
--
-- 'attribute', 'searchFilter_attribute' - The result attribute to which the filter values are applied. Valid
-- values vary by API action.
newSearchFilter ::
  SearchFilter
newSearchFilter =
  SearchFilter'
    { values = Prelude.Nothing,
      operator = Prelude.Nothing,
      attribute = Prelude.Nothing
    }

-- | The list of values applied to the @Attribute@ and @Operator@ attributes.
-- Number of values and valid values vary by @Attribute@.
searchFilter_values :: Lens.Lens' SearchFilter (Prelude.Maybe [Prelude.Text])
searchFilter_values = Lens.lens (\SearchFilter' {values} -> values) (\s@SearchFilter' {} a -> s {values = a} :: SearchFilter) Prelude.. Lens.mapping Prelude._Coerce

-- | The operator to apply to the @Attribute@ with each of the @Values@.
-- Valid values vary by @Attribute@.
searchFilter_operator :: Lens.Lens' SearchFilter (Prelude.Maybe Prelude.Text)
searchFilter_operator = Lens.lens (\SearchFilter' {operator} -> operator) (\s@SearchFilter' {} a -> s {operator = a} :: SearchFilter)

-- | The result attribute to which the filter values are applied. Valid
-- values vary by API action.
searchFilter_attribute :: Lens.Lens' SearchFilter (Prelude.Maybe Prelude.Text)
searchFilter_attribute = Lens.lens (\SearchFilter' {attribute} -> attribute) (\s@SearchFilter' {} a -> s {attribute = a} :: SearchFilter)

instance Prelude.Hashable SearchFilter

instance Prelude.NFData SearchFilter

instance Prelude.ToQuery SearchFilter where
  toQuery SearchFilter' {..} =
    Prelude.mconcat
      [ "Values"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> values),
        "Operator" Prelude.=: operator,
        "Attribute" Prelude.=: attribute
      ]
