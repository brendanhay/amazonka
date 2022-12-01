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
-- Module      : Amazonka.AppFlow.Types.MetadataCatalogDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.MetadataCatalogDetail where

import Amazonka.AppFlow.Types.CatalogType
import Amazonka.AppFlow.Types.RegistrationOutput
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the metadata catalog, metadata table, and data partitions that
-- Amazon AppFlow used for the associated flow run.
--
-- /See:/ 'newMetadataCatalogDetail' smart constructor.
data MetadataCatalogDetail = MetadataCatalogDetail'
  { -- | The name of the table that stores the metadata for the associated flow
    -- run. The table stores metadata that represents the data that the flow
    -- transferred. Amazon AppFlow stores the table in the metadata catalog.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | Describes the status of the attempt from Amazon AppFlow to register the
    -- metadata table with the metadata catalog. Amazon AppFlow creates or
    -- updates this table for the associated flow run.
    tableRegistrationOutput :: Prelude.Maybe RegistrationOutput,
    -- | Describes the status of the attempt from Amazon AppFlow to register the
    -- data partitions with the metadata catalog. The data partitions organize
    -- the flow output into a hierarchical path, such as a folder path in an S3
    -- bucket. Amazon AppFlow creates the partitions (if they don\'t already
    -- exist) based on your flow configuration.
    partitionRegistrationOutput :: Prelude.Maybe RegistrationOutput,
    -- | The type of metadata catalog that Amazon AppFlow used for the associated
    -- flow run. This parameter returns the following value:
    --
    -- [GLUE]
    --     The metadata catalog is provided by the Glue Data Catalog. Glue
    --     includes the Glue Data Catalog as a component.
    catalogType :: Prelude.Maybe CatalogType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetadataCatalogDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'metadataCatalogDetail_tableName' - The name of the table that stores the metadata for the associated flow
-- run. The table stores metadata that represents the data that the flow
-- transferred. Amazon AppFlow stores the table in the metadata catalog.
--
-- 'tableRegistrationOutput', 'metadataCatalogDetail_tableRegistrationOutput' - Describes the status of the attempt from Amazon AppFlow to register the
-- metadata table with the metadata catalog. Amazon AppFlow creates or
-- updates this table for the associated flow run.
--
-- 'partitionRegistrationOutput', 'metadataCatalogDetail_partitionRegistrationOutput' - Describes the status of the attempt from Amazon AppFlow to register the
-- data partitions with the metadata catalog. The data partitions organize
-- the flow output into a hierarchical path, such as a folder path in an S3
-- bucket. Amazon AppFlow creates the partitions (if they don\'t already
-- exist) based on your flow configuration.
--
-- 'catalogType', 'metadataCatalogDetail_catalogType' - The type of metadata catalog that Amazon AppFlow used for the associated
-- flow run. This parameter returns the following value:
--
-- [GLUE]
--     The metadata catalog is provided by the Glue Data Catalog. Glue
--     includes the Glue Data Catalog as a component.
newMetadataCatalogDetail ::
  MetadataCatalogDetail
newMetadataCatalogDetail =
  MetadataCatalogDetail'
    { tableName = Prelude.Nothing,
      tableRegistrationOutput = Prelude.Nothing,
      partitionRegistrationOutput = Prelude.Nothing,
      catalogType = Prelude.Nothing
    }

-- | The name of the table that stores the metadata for the associated flow
-- run. The table stores metadata that represents the data that the flow
-- transferred. Amazon AppFlow stores the table in the metadata catalog.
metadataCatalogDetail_tableName :: Lens.Lens' MetadataCatalogDetail (Prelude.Maybe Prelude.Text)
metadataCatalogDetail_tableName = Lens.lens (\MetadataCatalogDetail' {tableName} -> tableName) (\s@MetadataCatalogDetail' {} a -> s {tableName = a} :: MetadataCatalogDetail)

-- | Describes the status of the attempt from Amazon AppFlow to register the
-- metadata table with the metadata catalog. Amazon AppFlow creates or
-- updates this table for the associated flow run.
metadataCatalogDetail_tableRegistrationOutput :: Lens.Lens' MetadataCatalogDetail (Prelude.Maybe RegistrationOutput)
metadataCatalogDetail_tableRegistrationOutput = Lens.lens (\MetadataCatalogDetail' {tableRegistrationOutput} -> tableRegistrationOutput) (\s@MetadataCatalogDetail' {} a -> s {tableRegistrationOutput = a} :: MetadataCatalogDetail)

-- | Describes the status of the attempt from Amazon AppFlow to register the
-- data partitions with the metadata catalog. The data partitions organize
-- the flow output into a hierarchical path, such as a folder path in an S3
-- bucket. Amazon AppFlow creates the partitions (if they don\'t already
-- exist) based on your flow configuration.
metadataCatalogDetail_partitionRegistrationOutput :: Lens.Lens' MetadataCatalogDetail (Prelude.Maybe RegistrationOutput)
metadataCatalogDetail_partitionRegistrationOutput = Lens.lens (\MetadataCatalogDetail' {partitionRegistrationOutput} -> partitionRegistrationOutput) (\s@MetadataCatalogDetail' {} a -> s {partitionRegistrationOutput = a} :: MetadataCatalogDetail)

-- | The type of metadata catalog that Amazon AppFlow used for the associated
-- flow run. This parameter returns the following value:
--
-- [GLUE]
--     The metadata catalog is provided by the Glue Data Catalog. Glue
--     includes the Glue Data Catalog as a component.
metadataCatalogDetail_catalogType :: Lens.Lens' MetadataCatalogDetail (Prelude.Maybe CatalogType)
metadataCatalogDetail_catalogType = Lens.lens (\MetadataCatalogDetail' {catalogType} -> catalogType) (\s@MetadataCatalogDetail' {} a -> s {catalogType = a} :: MetadataCatalogDetail)

instance Core.FromJSON MetadataCatalogDetail where
  parseJSON =
    Core.withObject
      "MetadataCatalogDetail"
      ( \x ->
          MetadataCatalogDetail'
            Prelude.<$> (x Core..:? "tableName")
            Prelude.<*> (x Core..:? "tableRegistrationOutput")
            Prelude.<*> (x Core..:? "partitionRegistrationOutput")
            Prelude.<*> (x Core..:? "catalogType")
      )

instance Prelude.Hashable MetadataCatalogDetail where
  hashWithSalt _salt MetadataCatalogDetail' {..} =
    _salt `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` tableRegistrationOutput
      `Prelude.hashWithSalt` partitionRegistrationOutput
      `Prelude.hashWithSalt` catalogType

instance Prelude.NFData MetadataCatalogDetail where
  rnf MetadataCatalogDetail' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf tableRegistrationOutput
      `Prelude.seq` Prelude.rnf partitionRegistrationOutput
      `Prelude.seq` Prelude.rnf catalogType
