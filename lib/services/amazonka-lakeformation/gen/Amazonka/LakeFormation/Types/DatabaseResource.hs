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
-- Module      : Amazonka.LakeFormation.Types.DatabaseResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.DatabaseResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure for the database object.
--
-- /See:/ 'newDatabaseResource' smart constructor.
data DatabaseResource = DatabaseResource'
  { -- | The identifier for the Data Catalog. By default, it is the account ID of
    -- the caller.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database resource. Unique to the Data Catalog.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'databaseResource_catalogId' - The identifier for the Data Catalog. By default, it is the account ID of
-- the caller.
--
-- 'name', 'databaseResource_name' - The name of the database resource. Unique to the Data Catalog.
newDatabaseResource ::
  -- | 'name'
  Prelude.Text ->
  DatabaseResource
newDatabaseResource pName_ =
  DatabaseResource'
    { catalogId = Prelude.Nothing,
      name = pName_
    }

-- | The identifier for the Data Catalog. By default, it is the account ID of
-- the caller.
databaseResource_catalogId :: Lens.Lens' DatabaseResource (Prelude.Maybe Prelude.Text)
databaseResource_catalogId = Lens.lens (\DatabaseResource' {catalogId} -> catalogId) (\s@DatabaseResource' {} a -> s {catalogId = a} :: DatabaseResource)

-- | The name of the database resource. Unique to the Data Catalog.
databaseResource_name :: Lens.Lens' DatabaseResource Prelude.Text
databaseResource_name = Lens.lens (\DatabaseResource' {name} -> name) (\s@DatabaseResource' {} a -> s {name = a} :: DatabaseResource)

instance Data.FromJSON DatabaseResource where
  parseJSON =
    Data.withObject
      "DatabaseResource"
      ( \x ->
          DatabaseResource'
            Prelude.<$> (x Data..:? "CatalogId")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable DatabaseResource where
  hashWithSalt _salt DatabaseResource' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` name

instance Prelude.NFData DatabaseResource where
  rnf DatabaseResource' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON DatabaseResource where
  toJSON DatabaseResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("Name" Data..= name)
          ]
      )
