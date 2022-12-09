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
-- Module      : Amazonka.CloudWatchEvents.Types.ConnectionBodyParameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ConnectionBodyParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Additional parameter included in the body. You can include up to 100
-- additional body parameters per request. An event payload cannot exceed
-- 64 KB.
--
-- /See:/ 'newConnectionBodyParameter' smart constructor.
data ConnectionBodyParameter = ConnectionBodyParameter'
  { -- | Specified whether the value is secret.
    isValueSecret :: Prelude.Maybe Prelude.Bool,
    -- | The key for the parameter.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value associated with the key.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionBodyParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isValueSecret', 'connectionBodyParameter_isValueSecret' - Specified whether the value is secret.
--
-- 'key', 'connectionBodyParameter_key' - The key for the parameter.
--
-- 'value', 'connectionBodyParameter_value' - The value associated with the key.
newConnectionBodyParameter ::
  ConnectionBodyParameter
newConnectionBodyParameter =
  ConnectionBodyParameter'
    { isValueSecret =
        Prelude.Nothing,
      key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Specified whether the value is secret.
connectionBodyParameter_isValueSecret :: Lens.Lens' ConnectionBodyParameter (Prelude.Maybe Prelude.Bool)
connectionBodyParameter_isValueSecret = Lens.lens (\ConnectionBodyParameter' {isValueSecret} -> isValueSecret) (\s@ConnectionBodyParameter' {} a -> s {isValueSecret = a} :: ConnectionBodyParameter)

-- | The key for the parameter.
connectionBodyParameter_key :: Lens.Lens' ConnectionBodyParameter (Prelude.Maybe Prelude.Text)
connectionBodyParameter_key = Lens.lens (\ConnectionBodyParameter' {key} -> key) (\s@ConnectionBodyParameter' {} a -> s {key = a} :: ConnectionBodyParameter)

-- | The value associated with the key.
connectionBodyParameter_value :: Lens.Lens' ConnectionBodyParameter (Prelude.Maybe Prelude.Text)
connectionBodyParameter_value = Lens.lens (\ConnectionBodyParameter' {value} -> value) (\s@ConnectionBodyParameter' {} a -> s {value = a} :: ConnectionBodyParameter)

instance Data.FromJSON ConnectionBodyParameter where
  parseJSON =
    Data.withObject
      "ConnectionBodyParameter"
      ( \x ->
          ConnectionBodyParameter'
            Prelude.<$> (x Data..:? "IsValueSecret")
            Prelude.<*> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable ConnectionBodyParameter where
  hashWithSalt _salt ConnectionBodyParameter' {..} =
    _salt `Prelude.hashWithSalt` isValueSecret
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ConnectionBodyParameter where
  rnf ConnectionBodyParameter' {..} =
    Prelude.rnf isValueSecret
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ConnectionBodyParameter where
  toJSON ConnectionBodyParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsValueSecret" Data..=) Prelude.<$> isValueSecret,
            ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
