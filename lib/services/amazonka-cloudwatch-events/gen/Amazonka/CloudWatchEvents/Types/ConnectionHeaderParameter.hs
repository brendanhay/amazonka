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
-- Module      : Amazonka.CloudWatchEvents.Types.ConnectionHeaderParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ConnectionHeaderParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Additional parameter included in the header. You can include up to 100
-- additional header parameters per request. An event payload cannot exceed
-- 64 KB.
--
-- /See:/ 'newConnectionHeaderParameter' smart constructor.
data ConnectionHeaderParameter = ConnectionHeaderParameter'
  { -- | Specified whether the value is a secret.
    isValueSecret :: Prelude.Maybe Prelude.Bool,
    -- | The key for the parameter.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value associated with the key.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionHeaderParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isValueSecret', 'connectionHeaderParameter_isValueSecret' - Specified whether the value is a secret.
--
-- 'key', 'connectionHeaderParameter_key' - The key for the parameter.
--
-- 'value', 'connectionHeaderParameter_value' - The value associated with the key.
newConnectionHeaderParameter ::
  ConnectionHeaderParameter
newConnectionHeaderParameter =
  ConnectionHeaderParameter'
    { isValueSecret =
        Prelude.Nothing,
      key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Specified whether the value is a secret.
connectionHeaderParameter_isValueSecret :: Lens.Lens' ConnectionHeaderParameter (Prelude.Maybe Prelude.Bool)
connectionHeaderParameter_isValueSecret = Lens.lens (\ConnectionHeaderParameter' {isValueSecret} -> isValueSecret) (\s@ConnectionHeaderParameter' {} a -> s {isValueSecret = a} :: ConnectionHeaderParameter)

-- | The key for the parameter.
connectionHeaderParameter_key :: Lens.Lens' ConnectionHeaderParameter (Prelude.Maybe Prelude.Text)
connectionHeaderParameter_key = Lens.lens (\ConnectionHeaderParameter' {key} -> key) (\s@ConnectionHeaderParameter' {} a -> s {key = a} :: ConnectionHeaderParameter)

-- | The value associated with the key.
connectionHeaderParameter_value :: Lens.Lens' ConnectionHeaderParameter (Prelude.Maybe Prelude.Text)
connectionHeaderParameter_value = Lens.lens (\ConnectionHeaderParameter' {value} -> value) (\s@ConnectionHeaderParameter' {} a -> s {value = a} :: ConnectionHeaderParameter)

instance Data.FromJSON ConnectionHeaderParameter where
  parseJSON =
    Data.withObject
      "ConnectionHeaderParameter"
      ( \x ->
          ConnectionHeaderParameter'
            Prelude.<$> (x Data..:? "IsValueSecret")
            Prelude.<*> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable ConnectionHeaderParameter where
  hashWithSalt _salt ConnectionHeaderParameter' {..} =
    _salt `Prelude.hashWithSalt` isValueSecret
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ConnectionHeaderParameter where
  rnf ConnectionHeaderParameter' {..} =
    Prelude.rnf isValueSecret
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ConnectionHeaderParameter where
  toJSON ConnectionHeaderParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsValueSecret" Data..=) Prelude.<$> isValueSecret,
            ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
