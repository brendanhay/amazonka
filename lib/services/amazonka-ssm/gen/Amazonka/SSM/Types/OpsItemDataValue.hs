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
-- Module      : Amazonka.SSM.Types.OpsItemDataValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemDataValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OpsItemDataType

-- | An object that defines the value of the key and its type in the
-- OperationalData map.
--
-- /See:/ 'newOpsItemDataValue' smart constructor.
data OpsItemDataValue = OpsItemDataValue'
  { -- | The type of key-value pair. Valid types include @SearchableString@ and
    -- @String@.
    type' :: Prelude.Maybe OpsItemDataType,
    -- | The value of the OperationalData key.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsItemDataValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'opsItemDataValue_type' - The type of key-value pair. Valid types include @SearchableString@ and
-- @String@.
--
-- 'value', 'opsItemDataValue_value' - The value of the OperationalData key.
newOpsItemDataValue ::
  OpsItemDataValue
newOpsItemDataValue =
  OpsItemDataValue'
    { type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The type of key-value pair. Valid types include @SearchableString@ and
-- @String@.
opsItemDataValue_type :: Lens.Lens' OpsItemDataValue (Prelude.Maybe OpsItemDataType)
opsItemDataValue_type = Lens.lens (\OpsItemDataValue' {type'} -> type') (\s@OpsItemDataValue' {} a -> s {type' = a} :: OpsItemDataValue)

-- | The value of the OperationalData key.
opsItemDataValue_value :: Lens.Lens' OpsItemDataValue (Prelude.Maybe Prelude.Text)
opsItemDataValue_value = Lens.lens (\OpsItemDataValue' {value} -> value) (\s@OpsItemDataValue' {} a -> s {value = a} :: OpsItemDataValue)

instance Data.FromJSON OpsItemDataValue where
  parseJSON =
    Data.withObject
      "OpsItemDataValue"
      ( \x ->
          OpsItemDataValue'
            Prelude.<$> (x Data..:? "Type") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable OpsItemDataValue where
  hashWithSalt _salt OpsItemDataValue' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData OpsItemDataValue where
  rnf OpsItemDataValue' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Data.ToJSON OpsItemDataValue where
  toJSON OpsItemDataValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            ("Value" Data..=) Prelude.<$> value
          ]
      )
