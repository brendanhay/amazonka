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
-- Module      : Network.AWS.SSM.Types.OpsItemDataValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemDataValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.OpsItemDataType

-- | An object that defines the value of the key and its type in the
-- OperationalData map.
--
-- /See:/ 'newOpsItemDataValue' smart constructor.
data OpsItemDataValue = OpsItemDataValue'
  { -- | The value of the OperationalData key.
    value :: Prelude.Maybe Prelude.Text,
    -- | The type of key-value pair. Valid types include @SearchableString@ and
    -- @String@.
    type' :: Prelude.Maybe OpsItemDataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OpsItemDataValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'opsItemDataValue_value' - The value of the OperationalData key.
--
-- 'type'', 'opsItemDataValue_type' - The type of key-value pair. Valid types include @SearchableString@ and
-- @String@.
newOpsItemDataValue ::
  OpsItemDataValue
newOpsItemDataValue =
  OpsItemDataValue'
    { value = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The value of the OperationalData key.
opsItemDataValue_value :: Lens.Lens' OpsItemDataValue (Prelude.Maybe Prelude.Text)
opsItemDataValue_value = Lens.lens (\OpsItemDataValue' {value} -> value) (\s@OpsItemDataValue' {} a -> s {value = a} :: OpsItemDataValue)

-- | The type of key-value pair. Valid types include @SearchableString@ and
-- @String@.
opsItemDataValue_type :: Lens.Lens' OpsItemDataValue (Prelude.Maybe OpsItemDataType)
opsItemDataValue_type = Lens.lens (\OpsItemDataValue' {type'} -> type') (\s@OpsItemDataValue' {} a -> s {type' = a} :: OpsItemDataValue)

instance Prelude.FromJSON OpsItemDataValue where
  parseJSON =
    Prelude.withObject
      "OpsItemDataValue"
      ( \x ->
          OpsItemDataValue'
            Prelude.<$> (x Prelude..:? "Value")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable OpsItemDataValue

instance Prelude.NFData OpsItemDataValue

instance Prelude.ToJSON OpsItemDataValue where
  toJSON OpsItemDataValue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Value" Prelude..=) Prelude.<$> value,
            ("Type" Prelude..=) Prelude.<$> type'
          ]
      )
