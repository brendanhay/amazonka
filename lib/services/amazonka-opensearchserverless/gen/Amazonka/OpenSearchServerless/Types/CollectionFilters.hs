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
-- Module      : Amazonka.OpenSearchServerless.Types.CollectionFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.CollectionFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types.CollectionStatus
import qualified Amazonka.Prelude as Prelude

-- | List of filter keys that you can use for LIST, UPDATE, and DELETE
-- requests to OpenSearch Serverless collections.
--
-- /See:/ 'newCollectionFilters' smart constructor.
data CollectionFilters = CollectionFilters'
  { -- | The name of the collection.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the collection.
    status :: Prelude.Maybe CollectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollectionFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'collectionFilters_name' - The name of the collection.
--
-- 'status', 'collectionFilters_status' - The current status of the collection.
newCollectionFilters ::
  CollectionFilters
newCollectionFilters =
  CollectionFilters'
    { name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the collection.
collectionFilters_name :: Lens.Lens' CollectionFilters (Prelude.Maybe Prelude.Text)
collectionFilters_name = Lens.lens (\CollectionFilters' {name} -> name) (\s@CollectionFilters' {} a -> s {name = a} :: CollectionFilters)

-- | The current status of the collection.
collectionFilters_status :: Lens.Lens' CollectionFilters (Prelude.Maybe CollectionStatus)
collectionFilters_status = Lens.lens (\CollectionFilters' {status} -> status) (\s@CollectionFilters' {} a -> s {status = a} :: CollectionFilters)

instance Prelude.Hashable CollectionFilters where
  hashWithSalt _salt CollectionFilters' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData CollectionFilters where
  rnf CollectionFilters' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf status

instance Data.ToJSON CollectionFilters where
  toJSON CollectionFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("status" Data..=) Prelude.<$> status
          ]
      )
