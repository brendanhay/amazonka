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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ConnectionBodyParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Additional parameter included in the body. You can include up to 100
-- additional body parameters per request. An event payload cannot exceed
-- 64 KB.
--
-- /See:/ 'newConnectionBodyParameter' smart constructor.
data ConnectionBodyParameter = ConnectionBodyParameter'
  { -- | Specified whether the value is secret.
    isValueSecret :: Prelude.Maybe Prelude.Bool,
    -- | The value associated with the key.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key for the parameter.
    key :: Prelude.Maybe Prelude.Text
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
-- 'value', 'connectionBodyParameter_value' - The value associated with the key.
--
-- 'key', 'connectionBodyParameter_key' - The key for the parameter.
newConnectionBodyParameter ::
  ConnectionBodyParameter
newConnectionBodyParameter =
  ConnectionBodyParameter'
    { isValueSecret =
        Prelude.Nothing,
      value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | Specified whether the value is secret.
connectionBodyParameter_isValueSecret :: Lens.Lens' ConnectionBodyParameter (Prelude.Maybe Prelude.Bool)
connectionBodyParameter_isValueSecret = Lens.lens (\ConnectionBodyParameter' {isValueSecret} -> isValueSecret) (\s@ConnectionBodyParameter' {} a -> s {isValueSecret = a} :: ConnectionBodyParameter)

-- | The value associated with the key.
connectionBodyParameter_value :: Lens.Lens' ConnectionBodyParameter (Prelude.Maybe Prelude.Text)
connectionBodyParameter_value = Lens.lens (\ConnectionBodyParameter' {value} -> value) (\s@ConnectionBodyParameter' {} a -> s {value = a} :: ConnectionBodyParameter)

-- | The key for the parameter.
connectionBodyParameter_key :: Lens.Lens' ConnectionBodyParameter (Prelude.Maybe Prelude.Text)
connectionBodyParameter_key = Lens.lens (\ConnectionBodyParameter' {key} -> key) (\s@ConnectionBodyParameter' {} a -> s {key = a} :: ConnectionBodyParameter)

instance Core.FromJSON ConnectionBodyParameter where
  parseJSON =
    Core.withObject
      "ConnectionBodyParameter"
      ( \x ->
          ConnectionBodyParameter'
            Prelude.<$> (x Core..:? "IsValueSecret")
            Prelude.<*> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Key")
      )

instance Prelude.Hashable ConnectionBodyParameter

instance Prelude.NFData ConnectionBodyParameter

instance Core.ToJSON ConnectionBodyParameter where
  toJSON ConnectionBodyParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IsValueSecret" Core..=) Prelude.<$> isValueSecret,
            ("Value" Core..=) Prelude.<$> value,
            ("Key" Core..=) Prelude.<$> key
          ]
      )
