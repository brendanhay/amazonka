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
-- Module      : Amazonka.LakeFormation.Types.Resource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.CatalogResource
import Amazonka.LakeFormation.Types.DataCellsFilterResource
import Amazonka.LakeFormation.Types.DataLocationResource
import Amazonka.LakeFormation.Types.DatabaseResource
import Amazonka.LakeFormation.Types.LFTagKeyResource
import Amazonka.LakeFormation.Types.LFTagPolicyResource
import Amazonka.LakeFormation.Types.TableResource
import Amazonka.LakeFormation.Types.TableWithColumnsResource
import qualified Amazonka.Prelude as Prelude

-- | A structure for the resource.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalog :: Prelude.Maybe CatalogResource,
    -- | A data cell filter.
    dataCellsFilter :: Prelude.Maybe DataCellsFilterResource,
    -- | The location of an Amazon S3 path where permissions are granted or
    -- revoked.
    dataLocation :: Prelude.Maybe DataLocationResource,
    -- | The database for the resource. Unique to the Data Catalog. A database is
    -- a set of associated table definitions organized into a logical group.
    -- You can Grant and Revoke database permissions to a principal.
    database :: Prelude.Maybe DatabaseResource,
    -- | The LF-tag key and values attached to a resource.
    lFTag :: Prelude.Maybe LFTagKeyResource,
    -- | A list of LF-tag conditions that define a resource\'s LF-tag policy.
    lFTagPolicy :: Prelude.Maybe LFTagPolicyResource,
    -- | The table for the resource. A table is a metadata definition that
    -- represents your data. You can Grant and Revoke table privileges to a
    -- principal.
    table :: Prelude.Maybe TableResource,
    -- | The table with columns for the resource. A principal with permissions to
    -- this resource can select metadata from the columns of a table in the
    -- Data Catalog and the underlying data in Amazon S3.
    tableWithColumns :: Prelude.Maybe TableWithColumnsResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalog', 'resource_catalog' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'dataCellsFilter', 'resource_dataCellsFilter' - A data cell filter.
--
-- 'dataLocation', 'resource_dataLocation' - The location of an Amazon S3 path where permissions are granted or
-- revoked.
--
-- 'database', 'resource_database' - The database for the resource. Unique to the Data Catalog. A database is
-- a set of associated table definitions organized into a logical group.
-- You can Grant and Revoke database permissions to a principal.
--
-- 'lFTag', 'resource_lFTag' - The LF-tag key and values attached to a resource.
--
-- 'lFTagPolicy', 'resource_lFTagPolicy' - A list of LF-tag conditions that define a resource\'s LF-tag policy.
--
-- 'table', 'resource_table' - The table for the resource. A table is a metadata definition that
-- represents your data. You can Grant and Revoke table privileges to a
-- principal.
--
-- 'tableWithColumns', 'resource_tableWithColumns' - The table with columns for the resource. A principal with permissions to
-- this resource can select metadata from the columns of a table in the
-- Data Catalog and the underlying data in Amazon S3.
newResource ::
  Resource
newResource =
  Resource'
    { catalog = Prelude.Nothing,
      dataCellsFilter = Prelude.Nothing,
      dataLocation = Prelude.Nothing,
      database = Prelude.Nothing,
      lFTag = Prelude.Nothing,
      lFTagPolicy = Prelude.Nothing,
      table = Prelude.Nothing,
      tableWithColumns = Prelude.Nothing
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
resource_catalog :: Lens.Lens' Resource (Prelude.Maybe CatalogResource)
resource_catalog = Lens.lens (\Resource' {catalog} -> catalog) (\s@Resource' {} a -> s {catalog = a} :: Resource)

-- | A data cell filter.
resource_dataCellsFilter :: Lens.Lens' Resource (Prelude.Maybe DataCellsFilterResource)
resource_dataCellsFilter = Lens.lens (\Resource' {dataCellsFilter} -> dataCellsFilter) (\s@Resource' {} a -> s {dataCellsFilter = a} :: Resource)

-- | The location of an Amazon S3 path where permissions are granted or
-- revoked.
resource_dataLocation :: Lens.Lens' Resource (Prelude.Maybe DataLocationResource)
resource_dataLocation = Lens.lens (\Resource' {dataLocation} -> dataLocation) (\s@Resource' {} a -> s {dataLocation = a} :: Resource)

-- | The database for the resource. Unique to the Data Catalog. A database is
-- a set of associated table definitions organized into a logical group.
-- You can Grant and Revoke database permissions to a principal.
resource_database :: Lens.Lens' Resource (Prelude.Maybe DatabaseResource)
resource_database = Lens.lens (\Resource' {database} -> database) (\s@Resource' {} a -> s {database = a} :: Resource)

-- | The LF-tag key and values attached to a resource.
resource_lFTag :: Lens.Lens' Resource (Prelude.Maybe LFTagKeyResource)
resource_lFTag = Lens.lens (\Resource' {lFTag} -> lFTag) (\s@Resource' {} a -> s {lFTag = a} :: Resource)

-- | A list of LF-tag conditions that define a resource\'s LF-tag policy.
resource_lFTagPolicy :: Lens.Lens' Resource (Prelude.Maybe LFTagPolicyResource)
resource_lFTagPolicy = Lens.lens (\Resource' {lFTagPolicy} -> lFTagPolicy) (\s@Resource' {} a -> s {lFTagPolicy = a} :: Resource)

-- | The table for the resource. A table is a metadata definition that
-- represents your data. You can Grant and Revoke table privileges to a
-- principal.
resource_table :: Lens.Lens' Resource (Prelude.Maybe TableResource)
resource_table = Lens.lens (\Resource' {table} -> table) (\s@Resource' {} a -> s {table = a} :: Resource)

-- | The table with columns for the resource. A principal with permissions to
-- this resource can select metadata from the columns of a table in the
-- Data Catalog and the underlying data in Amazon S3.
resource_tableWithColumns :: Lens.Lens' Resource (Prelude.Maybe TableWithColumnsResource)
resource_tableWithColumns = Lens.lens (\Resource' {tableWithColumns} -> tableWithColumns) (\s@Resource' {} a -> s {tableWithColumns = a} :: Resource)

instance Data.FromJSON Resource where
  parseJSON =
    Data.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Data..:? "Catalog")
            Prelude.<*> (x Data..:? "DataCellsFilter")
            Prelude.<*> (x Data..:? "DataLocation")
            Prelude.<*> (x Data..:? "Database")
            Prelude.<*> (x Data..:? "LFTag")
            Prelude.<*> (x Data..:? "LFTagPolicy")
            Prelude.<*> (x Data..:? "Table")
            Prelude.<*> (x Data..:? "TableWithColumns")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt
      `Prelude.hashWithSalt` catalog
      `Prelude.hashWithSalt` dataCellsFilter
      `Prelude.hashWithSalt` dataLocation
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` lFTag
      `Prelude.hashWithSalt` lFTagPolicy
      `Prelude.hashWithSalt` table
      `Prelude.hashWithSalt` tableWithColumns

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf catalog
      `Prelude.seq` Prelude.rnf dataCellsFilter
      `Prelude.seq` Prelude.rnf dataLocation
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf lFTag
      `Prelude.seq` Prelude.rnf lFTagPolicy
      `Prelude.seq` Prelude.rnf table
      `Prelude.seq` Prelude.rnf tableWithColumns

instance Data.ToJSON Resource where
  toJSON Resource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Catalog" Data..=) Prelude.<$> catalog,
            ("DataCellsFilter" Data..=)
              Prelude.<$> dataCellsFilter,
            ("DataLocation" Data..=) Prelude.<$> dataLocation,
            ("Database" Data..=) Prelude.<$> database,
            ("LFTag" Data..=) Prelude.<$> lFTag,
            ("LFTagPolicy" Data..=) Prelude.<$> lFTagPolicy,
            ("Table" Data..=) Prelude.<$> table,
            ("TableWithColumns" Data..=)
              Prelude.<$> tableWithColumns
          ]
      )
