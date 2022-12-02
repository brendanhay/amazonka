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
-- Module      : Amazonka.SDB.Types.UpdateCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SDB.Types.UpdateCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the conditions under which data should be updated. If an
-- update condition is specified for a request, the data will only be
-- updated if the condition is satisfied. For example, if an attribute with
-- a specific name and value exists, or if a specific attribute doesn\'t
-- exist.
--
-- /See:/ 'newUpdateCondition' smart constructor.
data UpdateCondition = UpdateCondition'
  { -- | The name of the attribute involved in the condition.
    name :: Prelude.Maybe Prelude.Text,
    -- | A value specifying whether or not the specified attribute must exist
    -- with the specified value in order for the update condition to be
    -- satisfied. Specify @true@ if the attribute must exist for the update
    -- condition to be satisfied. Specify @false@ if the attribute should not
    -- exist in order for the update condition to be satisfied.
    exists :: Prelude.Maybe Prelude.Bool,
    -- | The value of an attribute. This value can only be specified when the
    -- @Exists@ parameter is equal to @true@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateCondition_name' - The name of the attribute involved in the condition.
--
-- 'exists', 'updateCondition_exists' - A value specifying whether or not the specified attribute must exist
-- with the specified value in order for the update condition to be
-- satisfied. Specify @true@ if the attribute must exist for the update
-- condition to be satisfied. Specify @false@ if the attribute should not
-- exist in order for the update condition to be satisfied.
--
-- 'value', 'updateCondition_value' - The value of an attribute. This value can only be specified when the
-- @Exists@ parameter is equal to @true@.
newUpdateCondition ::
  UpdateCondition
newUpdateCondition =
  UpdateCondition'
    { name = Prelude.Nothing,
      exists = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the attribute involved in the condition.
updateCondition_name :: Lens.Lens' UpdateCondition (Prelude.Maybe Prelude.Text)
updateCondition_name = Lens.lens (\UpdateCondition' {name} -> name) (\s@UpdateCondition' {} a -> s {name = a} :: UpdateCondition)

-- | A value specifying whether or not the specified attribute must exist
-- with the specified value in order for the update condition to be
-- satisfied. Specify @true@ if the attribute must exist for the update
-- condition to be satisfied. Specify @false@ if the attribute should not
-- exist in order for the update condition to be satisfied.
updateCondition_exists :: Lens.Lens' UpdateCondition (Prelude.Maybe Prelude.Bool)
updateCondition_exists = Lens.lens (\UpdateCondition' {exists} -> exists) (\s@UpdateCondition' {} a -> s {exists = a} :: UpdateCondition)

-- | The value of an attribute. This value can only be specified when the
-- @Exists@ parameter is equal to @true@.
updateCondition_value :: Lens.Lens' UpdateCondition (Prelude.Maybe Prelude.Text)
updateCondition_value = Lens.lens (\UpdateCondition' {value} -> value) (\s@UpdateCondition' {} a -> s {value = a} :: UpdateCondition)

instance Prelude.Hashable UpdateCondition where
  hashWithSalt _salt UpdateCondition' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` exists
      `Prelude.hashWithSalt` value

instance Prelude.NFData UpdateCondition where
  rnf UpdateCondition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf exists
      `Prelude.seq` Prelude.rnf value

instance Data.ToQuery UpdateCondition where
  toQuery UpdateCondition' {..} =
    Prelude.mconcat
      [ "Name" Data.=: name,
        "Exists" Data.=: exists,
        "Value" Data.=: value
      ]
