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
-- Module      : Amazonka.Batch.Types.KeyValuesPair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.KeyValuesPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter name and value pair that\'s used to return a more specific list
-- of results from a @ListJobs@ API operation.
--
-- /See:/ 'newKeyValuesPair' smart constructor.
data KeyValuesPair = KeyValuesPair'
  { -- | The name of the filter. Filter names are case sensitive.
    name :: Prelude.Maybe Prelude.Text,
    -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyValuesPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'keyValuesPair_name' - The name of the filter. Filter names are case sensitive.
--
-- 'values', 'keyValuesPair_values' - The filter values.
newKeyValuesPair ::
  KeyValuesPair
newKeyValuesPair =
  KeyValuesPair'
    { name = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the filter. Filter names are case sensitive.
keyValuesPair_name :: Lens.Lens' KeyValuesPair (Prelude.Maybe Prelude.Text)
keyValuesPair_name = Lens.lens (\KeyValuesPair' {name} -> name) (\s@KeyValuesPair' {} a -> s {name = a} :: KeyValuesPair)

-- | The filter values.
keyValuesPair_values :: Lens.Lens' KeyValuesPair (Prelude.Maybe [Prelude.Text])
keyValuesPair_values = Lens.lens (\KeyValuesPair' {values} -> values) (\s@KeyValuesPair' {} a -> s {values = a} :: KeyValuesPair) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable KeyValuesPair where
  hashWithSalt _salt KeyValuesPair' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData KeyValuesPair where
  rnf KeyValuesPair' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON KeyValuesPair where
  toJSON KeyValuesPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("values" Data..=) Prelude.<$> values
          ]
      )
