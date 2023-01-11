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
-- Module      : Amazonka.ECS.Types.KeyValuePair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.KeyValuePair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair object.
--
-- /See:/ 'newKeyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
  { -- | The name of the key-value pair. For environment variables, this is the
    -- name of the environment variable.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the key-value pair. For environment variables, this is the
    -- value of the environment variable.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'keyValuePair_name' - The name of the key-value pair. For environment variables, this is the
-- name of the environment variable.
--
-- 'value', 'keyValuePair_value' - The value of the key-value pair. For environment variables, this is the
-- value of the environment variable.
newKeyValuePair ::
  KeyValuePair
newKeyValuePair =
  KeyValuePair'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the key-value pair. For environment variables, this is the
-- name of the environment variable.
keyValuePair_name :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Text)
keyValuePair_name = Lens.lens (\KeyValuePair' {name} -> name) (\s@KeyValuePair' {} a -> s {name = a} :: KeyValuePair)

-- | The value of the key-value pair. For environment variables, this is the
-- value of the environment variable.
keyValuePair_value :: Lens.Lens' KeyValuePair (Prelude.Maybe Prelude.Text)
keyValuePair_value = Lens.lens (\KeyValuePair' {value} -> value) (\s@KeyValuePair' {} a -> s {value = a} :: KeyValuePair)

instance Data.FromJSON KeyValuePair where
  parseJSON =
    Data.withObject
      "KeyValuePair"
      ( \x ->
          KeyValuePair'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable KeyValuePair where
  hashWithSalt _salt KeyValuePair' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData KeyValuePair where
  rnf KeyValuePair' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON KeyValuePair where
  toJSON KeyValuePair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("value" Data..=) Prelude.<$> value
          ]
      )
