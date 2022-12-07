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
-- Module      : Amazonka.KeySpaces.Types.StaticColumn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.StaticColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The static columns of the table. Static columns store values that are
-- shared by all rows in the same partition.
--
-- /See:/ 'newStaticColumn' smart constructor.
data StaticColumn = StaticColumn'
  { -- | The name of the static column.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StaticColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'staticColumn_name' - The name of the static column.
newStaticColumn ::
  -- | 'name'
  Prelude.Text ->
  StaticColumn
newStaticColumn pName_ = StaticColumn' {name = pName_}

-- | The name of the static column.
staticColumn_name :: Lens.Lens' StaticColumn Prelude.Text
staticColumn_name = Lens.lens (\StaticColumn' {name} -> name) (\s@StaticColumn' {} a -> s {name = a} :: StaticColumn)

instance Data.FromJSON StaticColumn where
  parseJSON =
    Data.withObject
      "StaticColumn"
      (\x -> StaticColumn' Prelude.<$> (x Data..: "name"))

instance Prelude.Hashable StaticColumn where
  hashWithSalt _salt StaticColumn' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StaticColumn where
  rnf StaticColumn' {..} = Prelude.rnf name

instance Data.ToJSON StaticColumn where
  toJSON StaticColumn' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )
