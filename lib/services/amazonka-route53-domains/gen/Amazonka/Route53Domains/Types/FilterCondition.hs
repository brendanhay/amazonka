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
-- Module      : Amazonka.Route53Domains.Types.FilterCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.FilterCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Domains.Types.ListDomainsAttributeName
import Amazonka.Route53Domains.Types.Operator

-- | Information for the filtering of a list of domains returned by
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains__ListDomains.html ListDomains>.
--
-- /See:/ 'newFilterCondition' smart constructor.
data FilterCondition = FilterCondition'
  { -- | Name of the field which should be used for filtering the list of
    -- domains.
    name :: ListDomainsAttributeName,
    -- | The operator values for filtering domain names. The values can be:
    --
    -- -   @LE@: Less than, or equal to
    --
    -- -   @GE@: Greater than, or equal to
    --
    -- -   @BEGINS_WITH@: Begins with
    operator :: Operator,
    -- | An array of strings presenting values to compare. Only 1 item in the
    -- list is currently supported.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'filterCondition_name' - Name of the field which should be used for filtering the list of
-- domains.
--
-- 'operator', 'filterCondition_operator' - The operator values for filtering domain names. The values can be:
--
-- -   @LE@: Less than, or equal to
--
-- -   @GE@: Greater than, or equal to
--
-- -   @BEGINS_WITH@: Begins with
--
-- 'values', 'filterCondition_values' - An array of strings presenting values to compare. Only 1 item in the
-- list is currently supported.
newFilterCondition ::
  -- | 'name'
  ListDomainsAttributeName ->
  -- | 'operator'
  Operator ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  FilterCondition
newFilterCondition pName_ pOperator_ pValues_ =
  FilterCondition'
    { name = pName_,
      operator = pOperator_,
      values = Lens.coerced Lens.# pValues_
    }

-- | Name of the field which should be used for filtering the list of
-- domains.
filterCondition_name :: Lens.Lens' FilterCondition ListDomainsAttributeName
filterCondition_name = Lens.lens (\FilterCondition' {name} -> name) (\s@FilterCondition' {} a -> s {name = a} :: FilterCondition)

-- | The operator values for filtering domain names. The values can be:
--
-- -   @LE@: Less than, or equal to
--
-- -   @GE@: Greater than, or equal to
--
-- -   @BEGINS_WITH@: Begins with
filterCondition_operator :: Lens.Lens' FilterCondition Operator
filterCondition_operator = Lens.lens (\FilterCondition' {operator} -> operator) (\s@FilterCondition' {} a -> s {operator = a} :: FilterCondition)

-- | An array of strings presenting values to compare. Only 1 item in the
-- list is currently supported.
filterCondition_values :: Lens.Lens' FilterCondition (Prelude.NonEmpty Prelude.Text)
filterCondition_values = Lens.lens (\FilterCondition' {values} -> values) (\s@FilterCondition' {} a -> s {values = a} :: FilterCondition) Prelude.. Lens.coerced

instance Prelude.Hashable FilterCondition where
  hashWithSalt _salt FilterCondition' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` values

instance Prelude.NFData FilterCondition where
  rnf FilterCondition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON FilterCondition where
  toJSON FilterCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Operator" Data..= operator),
            Prelude.Just ("Values" Data..= values)
          ]
      )
