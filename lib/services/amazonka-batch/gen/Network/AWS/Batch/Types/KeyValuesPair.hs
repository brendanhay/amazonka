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
-- Module      : Network.AWS.Batch.Types.KeyValuesPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.KeyValuesPair where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A filter name and value pair that\'s used to return a more specific list
-- of results from a @ListJobs@ API operation.
--
-- /See:/ 'newKeyValuesPair' smart constructor.
data KeyValuesPair = KeyValuesPair'
  { -- | The filter values.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the filter. Filter names are case sensitive.
    name :: Prelude.Maybe Prelude.Text
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
-- 'values', 'keyValuesPair_values' - The filter values.
--
-- 'name', 'keyValuesPair_name' - The name of the filter. Filter names are case sensitive.
newKeyValuesPair ::
  KeyValuesPair
newKeyValuesPair =
  KeyValuesPair'
    { values = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The filter values.
keyValuesPair_values :: Lens.Lens' KeyValuesPair (Prelude.Maybe [Prelude.Text])
keyValuesPair_values = Lens.lens (\KeyValuesPair' {values} -> values) (\s@KeyValuesPair' {} a -> s {values = a} :: KeyValuesPair) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the filter. Filter names are case sensitive.
keyValuesPair_name :: Lens.Lens' KeyValuesPair (Prelude.Maybe Prelude.Text)
keyValuesPair_name = Lens.lens (\KeyValuesPair' {name} -> name) (\s@KeyValuesPair' {} a -> s {name = a} :: KeyValuesPair)

instance Prelude.Hashable KeyValuesPair

instance Prelude.NFData KeyValuesPair

instance Core.ToJSON KeyValuesPair where
  toJSON KeyValuesPair' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("values" Core..=) Prelude.<$> values,
            ("name" Core..=) Prelude.<$> name
          ]
      )
