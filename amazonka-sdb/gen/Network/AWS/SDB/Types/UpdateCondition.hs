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
-- Module      : Network.AWS.SDB.Types.UpdateCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.UpdateCondition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the conditions under which data should be updated. If an
-- update condition is specified for a request, the data will only be
-- updated if the condition is satisfied. For example, if an attribute with
-- a specific name and value exists, or if a specific attribute doesn\'t
-- exist.
--
-- /See:/ 'newUpdateCondition' smart constructor.
data UpdateCondition = UpdateCondition'
  { -- | A value specifying whether or not the specified attribute must exist
    -- with the specified value in order for the update condition to be
    -- satisfied. Specify @true@ if the attribute must exist for the update
    -- condition to be satisfied. Specify @false@ if the attribute should not
    -- exist in order for the update condition to be satisfied.
    exists :: Core.Maybe Core.Bool,
    -- | The name of the attribute involved in the condition.
    name :: Core.Maybe Core.Text,
    -- | The value of an attribute. This value can only be specified when the
    -- @Exists@ parameter is equal to @true@.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exists', 'updateCondition_exists' - A value specifying whether or not the specified attribute must exist
-- with the specified value in order for the update condition to be
-- satisfied. Specify @true@ if the attribute must exist for the update
-- condition to be satisfied. Specify @false@ if the attribute should not
-- exist in order for the update condition to be satisfied.
--
-- 'name', 'updateCondition_name' - The name of the attribute involved in the condition.
--
-- 'value', 'updateCondition_value' - The value of an attribute. This value can only be specified when the
-- @Exists@ parameter is equal to @true@.
newUpdateCondition ::
  UpdateCondition
newUpdateCondition =
  UpdateCondition'
    { exists = Core.Nothing,
      name = Core.Nothing,
      value = Core.Nothing
    }

-- | A value specifying whether or not the specified attribute must exist
-- with the specified value in order for the update condition to be
-- satisfied. Specify @true@ if the attribute must exist for the update
-- condition to be satisfied. Specify @false@ if the attribute should not
-- exist in order for the update condition to be satisfied.
updateCondition_exists :: Lens.Lens' UpdateCondition (Core.Maybe Core.Bool)
updateCondition_exists = Lens.lens (\UpdateCondition' {exists} -> exists) (\s@UpdateCondition' {} a -> s {exists = a} :: UpdateCondition)

-- | The name of the attribute involved in the condition.
updateCondition_name :: Lens.Lens' UpdateCondition (Core.Maybe Core.Text)
updateCondition_name = Lens.lens (\UpdateCondition' {name} -> name) (\s@UpdateCondition' {} a -> s {name = a} :: UpdateCondition)

-- | The value of an attribute. This value can only be specified when the
-- @Exists@ parameter is equal to @true@.
updateCondition_value :: Lens.Lens' UpdateCondition (Core.Maybe Core.Text)
updateCondition_value = Lens.lens (\UpdateCondition' {value} -> value) (\s@UpdateCondition' {} a -> s {value = a} :: UpdateCondition)

instance Core.Hashable UpdateCondition

instance Core.NFData UpdateCondition

instance Core.ToQuery UpdateCondition where
  toQuery UpdateCondition' {..} =
    Core.mconcat
      [ "Exists" Core.=: exists,
        "Name" Core.=: name,
        "Value" Core.=: value
      ]
