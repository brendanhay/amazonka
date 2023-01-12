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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ConnectionQueryStringParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Additional query string parameter for the connection. You can include up
-- to 100 additional query string parameters per request. Each additional
-- parameter counts towards the event payload size, which cannot exceed 64
-- KB.
--
-- /See:/ 'newConnectionQueryStringParameter' smart constructor.
data ConnectionQueryStringParameter = ConnectionQueryStringParameter'
  { -- | Specifies whether the value is secret.
    isValueSecret :: Prelude.Maybe Prelude.Bool,
    -- | The key for a query string parameter.
    key :: Prelude.Maybe Prelude.Text,
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
-- 'isValueSecret', 'connectionQueryStringParameter_isValueSecret' - Specifies whether the value is secret.
--
-- 'key', 'connectionQueryStringParameter_key' - The key for a query string parameter.
--
-- 'value', 'connectionQueryStringParameter_value' - The value associated with the key for the query string parameter.
newConnectionQueryStringParameter ::
  ConnectionQueryStringParameter
newConnectionQueryStringParameter =
  ConnectionQueryStringParameter'
    { isValueSecret =
        Prelude.Nothing,
      key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Specifies whether the value is secret.
connectionQueryStringParameter_isValueSecret :: Lens.Lens' ConnectionQueryStringParameter (Prelude.Maybe Prelude.Bool)
connectionQueryStringParameter_isValueSecret = Lens.lens (\ConnectionQueryStringParameter' {isValueSecret} -> isValueSecret) (\s@ConnectionQueryStringParameter' {} a -> s {isValueSecret = a} :: ConnectionQueryStringParameter)

-- | The key for a query string parameter.
connectionQueryStringParameter_key :: Lens.Lens' ConnectionQueryStringParameter (Prelude.Maybe Prelude.Text)
connectionQueryStringParameter_key = Lens.lens (\ConnectionQueryStringParameter' {key} -> key) (\s@ConnectionQueryStringParameter' {} a -> s {key = a} :: ConnectionQueryStringParameter)

-- | The value associated with the key for the query string parameter.
connectionQueryStringParameter_value :: Lens.Lens' ConnectionQueryStringParameter (Prelude.Maybe Prelude.Text)
connectionQueryStringParameter_value = Lens.lens (\ConnectionQueryStringParameter' {value} -> value) (\s@ConnectionQueryStringParameter' {} a -> s {value = a} :: ConnectionQueryStringParameter)

instance Data.FromJSON ConnectionQueryStringParameter where
  parseJSON =
    Data.withObject
      "ConnectionQueryStringParameter"
      ( \x ->
          ConnectionQueryStringParameter'
            Prelude.<$> (x Data..:? "IsValueSecret")
            Prelude.<*> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    ConnectionQueryStringParameter
  where
  hashWithSalt
    _salt
    ConnectionQueryStringParameter' {..} =
      _salt `Prelude.hashWithSalt` isValueSecret
        `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    ConnectionQueryStringParameter
  where
  rnf ConnectionQueryStringParameter' {..} =
    Prelude.rnf isValueSecret
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ConnectionQueryStringParameter where
  toJSON ConnectionQueryStringParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsValueSecret" Data..=) Prelude.<$> isValueSecret,
            ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
