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
-- Module      : Amazonka.CloudWatchEvents.Types.ConnectionQueryStringParameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ConnectionQueryStringParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Additional query string parameter for the connection. You can include up
-- to 100 additional query string parameters per request. Each additional
-- parameter counts towards the event payload size, which cannot exceed 64
-- KB.
--
-- /See:/ 'newConnectionQueryStringParameter' smart constructor.
data ConnectionQueryStringParameter = ConnectionQueryStringParameter'
  { -- | The key for a query string parameter.
    key :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the value is secret.
    isValueSecret :: Prelude.Maybe Prelude.Bool,
    -- | The value associated with the key for the query string parameter.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionQueryStringParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'connectionQueryStringParameter_key' - The key for a query string parameter.
--
-- 'isValueSecret', 'connectionQueryStringParameter_isValueSecret' - Specifies whether the value is secret.
--
-- 'value', 'connectionQueryStringParameter_value' - The value associated with the key for the query string parameter.
newConnectionQueryStringParameter ::
  ConnectionQueryStringParameter
newConnectionQueryStringParameter =
  ConnectionQueryStringParameter'
    { key =
        Prelude.Nothing,
      isValueSecret = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key for a query string parameter.
connectionQueryStringParameter_key :: Lens.Lens' ConnectionQueryStringParameter (Prelude.Maybe Prelude.Text)
connectionQueryStringParameter_key = Lens.lens (\ConnectionQueryStringParameter' {key} -> key) (\s@ConnectionQueryStringParameter' {} a -> s {key = a} :: ConnectionQueryStringParameter)

-- | Specifies whether the value is secret.
connectionQueryStringParameter_isValueSecret :: Lens.Lens' ConnectionQueryStringParameter (Prelude.Maybe Prelude.Bool)
connectionQueryStringParameter_isValueSecret = Lens.lens (\ConnectionQueryStringParameter' {isValueSecret} -> isValueSecret) (\s@ConnectionQueryStringParameter' {} a -> s {isValueSecret = a} :: ConnectionQueryStringParameter)

-- | The value associated with the key for the query string parameter.
connectionQueryStringParameter_value :: Lens.Lens' ConnectionQueryStringParameter (Prelude.Maybe Prelude.Text)
connectionQueryStringParameter_value = Lens.lens (\ConnectionQueryStringParameter' {value} -> value) (\s@ConnectionQueryStringParameter' {} a -> s {value = a} :: ConnectionQueryStringParameter)

instance Core.FromJSON ConnectionQueryStringParameter where
  parseJSON =
    Core.withObject
      "ConnectionQueryStringParameter"
      ( \x ->
          ConnectionQueryStringParameter'
            Prelude.<$> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "IsValueSecret")
            Prelude.<*> (x Core..:? "Value")
      )

instance
  Prelude.Hashable
    ConnectionQueryStringParameter
  where
  hashWithSalt
    _salt
    ConnectionQueryStringParameter' {..} =
      _salt `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` isValueSecret
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    ConnectionQueryStringParameter
  where
  rnf ConnectionQueryStringParameter' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf isValueSecret
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON ConnectionQueryStringParameter where
  toJSON ConnectionQueryStringParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("IsValueSecret" Core..=) Prelude.<$> isValueSecret,
            ("Value" Core..=) Prelude.<$> value
          ]
      )
