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
-- Module      : Amazonka.EMR.Types.KeyValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.KeyValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair.
--
-- /See:/ 'newKeyValue' smart constructor.
data KeyValue = KeyValue'
  { -- | The unique identifier of a key-value pair.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value part of the identified key.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'keyValue_key' - The unique identifier of a key-value pair.
--
-- 'value', 'keyValue_value' - The value part of the identified key.
newKeyValue ::
  KeyValue
newKeyValue =
  KeyValue'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The unique identifier of a key-value pair.
keyValue_key :: Lens.Lens' KeyValue (Prelude.Maybe Prelude.Text)
keyValue_key = Lens.lens (\KeyValue' {key} -> key) (\s@KeyValue' {} a -> s {key = a} :: KeyValue)

-- | The value part of the identified key.
keyValue_value :: Lens.Lens' KeyValue (Prelude.Maybe Prelude.Text)
keyValue_value = Lens.lens (\KeyValue' {value} -> value) (\s@KeyValue' {} a -> s {value = a} :: KeyValue)

instance Prelude.Hashable KeyValue where
  hashWithSalt _salt KeyValue' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData KeyValue where
  rnf KeyValue' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON KeyValue where
  toJSON KeyValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
