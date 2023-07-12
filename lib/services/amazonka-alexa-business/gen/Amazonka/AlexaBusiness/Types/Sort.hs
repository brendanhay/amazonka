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
-- Module      : Amazonka.AlexaBusiness.Types.Sort
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Sort where

import Amazonka.AlexaBusiness.Types.SortValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a sort criteria.
--
-- /See:/ 'newSort' smart constructor.
data Sort = Sort'
  { -- | The sort key of a sort object.
    key :: Prelude.Text,
    -- | The sort value of a sort object.
    value :: SortValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Sort' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'sort_key' - The sort key of a sort object.
--
-- 'value', 'sort_value' - The sort value of a sort object.
newSort ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  SortValue ->
  Sort
newSort pKey_ pValue_ =
  Sort' {key = pKey_, value = pValue_}

-- | The sort key of a sort object.
sort_key :: Lens.Lens' Sort Prelude.Text
sort_key = Lens.lens (\Sort' {key} -> key) (\s@Sort' {} a -> s {key = a} :: Sort)

-- | The sort value of a sort object.
sort_value :: Lens.Lens' Sort SortValue
sort_value = Lens.lens (\Sort' {value} -> value) (\s@Sort' {} a -> s {value = a} :: Sort)

instance Prelude.Hashable Sort where
  hashWithSalt _salt Sort' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Sort where
  rnf Sort' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Sort where
  toJSON Sort' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Value" Data..= value)
          ]
      )
