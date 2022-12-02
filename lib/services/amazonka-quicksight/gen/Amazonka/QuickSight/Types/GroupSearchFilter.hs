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
-- Module      : Amazonka.QuickSight.Types.GroupSearchFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GroupSearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GroupFilterAttribute
import Amazonka.QuickSight.Types.GroupFilterOperator

-- | A @GroupSearchFilter@ object that you want to apply to your search.
--
-- /See:/ 'newGroupSearchFilter' smart constructor.
data GroupSearchFilter = GroupSearchFilter'
  { -- | The comparison operator that you want to use as a filter, for example
    -- @\"Operator\": \"StartsWith\"@. Currently, the only supported operator
    -- is @StartsWith@.
    operator :: GroupFilterOperator,
    -- | The name of the value that you want to use as a filter, for example
    -- @\"Name\": \"GROUP_NAME\"@. Currently, the only supported name is
    -- @GROUP_NAME@.
    name :: GroupFilterAttribute,
    -- | The value of the named item, in this case @GROUP_NAME@, that you want to
    -- use as a filter.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operator', 'groupSearchFilter_operator' - The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StartsWith\"@. Currently, the only supported operator
-- is @StartsWith@.
--
-- 'name', 'groupSearchFilter_name' - The name of the value that you want to use as a filter, for example
-- @\"Name\": \"GROUP_NAME\"@. Currently, the only supported name is
-- @GROUP_NAME@.
--
-- 'value', 'groupSearchFilter_value' - The value of the named item, in this case @GROUP_NAME@, that you want to
-- use as a filter.
newGroupSearchFilter ::
  -- | 'operator'
  GroupFilterOperator ->
  -- | 'name'
  GroupFilterAttribute ->
  -- | 'value'
  Prelude.Text ->
  GroupSearchFilter
newGroupSearchFilter pOperator_ pName_ pValue_ =
  GroupSearchFilter'
    { operator = pOperator_,
      name = pName_,
      value = pValue_
    }

-- | The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StartsWith\"@. Currently, the only supported operator
-- is @StartsWith@.
groupSearchFilter_operator :: Lens.Lens' GroupSearchFilter GroupFilterOperator
groupSearchFilter_operator = Lens.lens (\GroupSearchFilter' {operator} -> operator) (\s@GroupSearchFilter' {} a -> s {operator = a} :: GroupSearchFilter)

-- | The name of the value that you want to use as a filter, for example
-- @\"Name\": \"GROUP_NAME\"@. Currently, the only supported name is
-- @GROUP_NAME@.
groupSearchFilter_name :: Lens.Lens' GroupSearchFilter GroupFilterAttribute
groupSearchFilter_name = Lens.lens (\GroupSearchFilter' {name} -> name) (\s@GroupSearchFilter' {} a -> s {name = a} :: GroupSearchFilter)

-- | The value of the named item, in this case @GROUP_NAME@, that you want to
-- use as a filter.
groupSearchFilter_value :: Lens.Lens' GroupSearchFilter Prelude.Text
groupSearchFilter_value = Lens.lens (\GroupSearchFilter' {value} -> value) (\s@GroupSearchFilter' {} a -> s {value = a} :: GroupSearchFilter)

instance Prelude.Hashable GroupSearchFilter where
  hashWithSalt _salt GroupSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData GroupSearchFilter where
  rnf GroupSearchFilter' {..} =
    Prelude.rnf operator
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON GroupSearchFilter where
  toJSON GroupSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Operator" Data..= operator),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
