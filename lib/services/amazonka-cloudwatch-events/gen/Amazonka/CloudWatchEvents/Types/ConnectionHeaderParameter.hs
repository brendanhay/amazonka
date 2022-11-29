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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ConnectionHeaderParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Additional parameter included in the header. You can include up to 100
-- additional header parameters per request. An event payload cannot exceed
-- 64 KB.
--
-- /See:/ 'newConnectionHeaderParameter' smart constructor.
data ConnectionHeaderParameter = ConnectionHeaderParameter'
  { -- | The key for the parameter.
    key :: Prelude.Maybe Prelude.Text,
    -- | Specified whether the value is a secret.
    isValueSecret :: Prelude.Maybe Prelude.Bool,
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
-- 'key', 'connectionHeaderParameter_key' - The key for the parameter.
--
-- 'isValueSecret', 'connectionHeaderParameter_isValueSecret' - Specified whether the value is a secret.
--
-- 'value', 'connectionHeaderParameter_value' - The value associated with the key.
newConnectionHeaderParameter ::
  ConnectionHeaderParameter
newConnectionHeaderParameter =
  ConnectionHeaderParameter'
    { key = Prelude.Nothing,
      isValueSecret = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key for the parameter.
connectionHeaderParameter_key :: Lens.Lens' ConnectionHeaderParameter (Prelude.Maybe Prelude.Text)
connectionHeaderParameter_key = Lens.lens (\ConnectionHeaderParameter' {key} -> key) (\s@ConnectionHeaderParameter' {} a -> s {key = a} :: ConnectionHeaderParameter)

-- | Specified whether the value is a secret.
connectionHeaderParameter_isValueSecret :: Lens.Lens' ConnectionHeaderParameter (Prelude.Maybe Prelude.Bool)
connectionHeaderParameter_isValueSecret = Lens.lens (\ConnectionHeaderParameter' {isValueSecret} -> isValueSecret) (\s@ConnectionHeaderParameter' {} a -> s {isValueSecret = a} :: ConnectionHeaderParameter)

-- | The value associated with the key.
connectionHeaderParameter_value :: Lens.Lens' ConnectionHeaderParameter (Prelude.Maybe Prelude.Text)
connectionHeaderParameter_value = Lens.lens (\ConnectionHeaderParameter' {value} -> value) (\s@ConnectionHeaderParameter' {} a -> s {value = a} :: ConnectionHeaderParameter)

instance Core.FromJSON ConnectionHeaderParameter where
  parseJSON =
    Core.withObject
      "ConnectionHeaderParameter"
      ( \x ->
          ConnectionHeaderParameter'
            Prelude.<$> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "IsValueSecret")
            Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable ConnectionHeaderParameter where
  hashWithSalt _salt ConnectionHeaderParameter' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` isValueSecret
      `Prelude.hashWithSalt` value

instance Prelude.NFData ConnectionHeaderParameter where
  rnf ConnectionHeaderParameter' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf isValueSecret
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON ConnectionHeaderParameter where
  toJSON ConnectionHeaderParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("IsValueSecret" Core..=) Prelude.<$> isValueSecret,
            ("Value" Core..=) Prelude.<$> value
          ]
      )
