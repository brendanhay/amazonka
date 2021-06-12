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
-- Module      : Network.AWS.Firehose.Types.SchemaConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SchemaConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the schema to which you want Kinesis Data Firehose to
-- configure your data before it writes it to Amazon S3. This parameter is
-- required if @Enabled@ is set to true.
--
-- /See:/ 'newSchemaConfiguration' smart constructor.
data SchemaConfiguration = SchemaConfiguration'
  { -- | The role that Kinesis Data Firehose can use to access AWS Glue. This
    -- role must be in the same account you use for Kinesis Data Firehose.
    -- Cross-account roles aren\'t allowed.
    roleARN :: Core.Maybe Core.Text,
    -- | Specifies the AWS Glue table that contains the column information that
    -- constitutes your data schema.
    tableName :: Core.Maybe Core.Text,
    -- | The ID of the AWS Glue Data Catalog. If you don\'t supply this, the AWS
    -- account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | Specifies the table version for the output data schema. If you don\'t
    -- specify this version ID, or if you set it to @LATEST@, Kinesis Data
    -- Firehose uses the most recent version. This means that any updates to
    -- the table are automatically picked up.
    versionId :: Core.Maybe Core.Text,
    -- | If you don\'t specify an AWS Region, the default is the current Region.
    region :: Core.Maybe Core.Text,
    -- | Specifies the name of the AWS Glue database that contains the schema for
    -- the output data.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SchemaConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARN', 'schemaConfiguration_roleARN' - The role that Kinesis Data Firehose can use to access AWS Glue. This
-- role must be in the same account you use for Kinesis Data Firehose.
-- Cross-account roles aren\'t allowed.
--
-- 'tableName', 'schemaConfiguration_tableName' - Specifies the AWS Glue table that contains the column information that
-- constitutes your data schema.
--
-- 'catalogId', 'schemaConfiguration_catalogId' - The ID of the AWS Glue Data Catalog. If you don\'t supply this, the AWS
-- account ID is used by default.
--
-- 'versionId', 'schemaConfiguration_versionId' - Specifies the table version for the output data schema. If you don\'t
-- specify this version ID, or if you set it to @LATEST@, Kinesis Data
-- Firehose uses the most recent version. This means that any updates to
-- the table are automatically picked up.
--
-- 'region', 'schemaConfiguration_region' - If you don\'t specify an AWS Region, the default is the current Region.
--
-- 'databaseName', 'schemaConfiguration_databaseName' - Specifies the name of the AWS Glue database that contains the schema for
-- the output data.
newSchemaConfiguration ::
  SchemaConfiguration
newSchemaConfiguration =
  SchemaConfiguration'
    { roleARN = Core.Nothing,
      tableName = Core.Nothing,
      catalogId = Core.Nothing,
      versionId = Core.Nothing,
      region = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | The role that Kinesis Data Firehose can use to access AWS Glue. This
-- role must be in the same account you use for Kinesis Data Firehose.
-- Cross-account roles aren\'t allowed.
schemaConfiguration_roleARN :: Lens.Lens' SchemaConfiguration (Core.Maybe Core.Text)
schemaConfiguration_roleARN = Lens.lens (\SchemaConfiguration' {roleARN} -> roleARN) (\s@SchemaConfiguration' {} a -> s {roleARN = a} :: SchemaConfiguration)

-- | Specifies the AWS Glue table that contains the column information that
-- constitutes your data schema.
schemaConfiguration_tableName :: Lens.Lens' SchemaConfiguration (Core.Maybe Core.Text)
schemaConfiguration_tableName = Lens.lens (\SchemaConfiguration' {tableName} -> tableName) (\s@SchemaConfiguration' {} a -> s {tableName = a} :: SchemaConfiguration)

-- | The ID of the AWS Glue Data Catalog. If you don\'t supply this, the AWS
-- account ID is used by default.
schemaConfiguration_catalogId :: Lens.Lens' SchemaConfiguration (Core.Maybe Core.Text)
schemaConfiguration_catalogId = Lens.lens (\SchemaConfiguration' {catalogId} -> catalogId) (\s@SchemaConfiguration' {} a -> s {catalogId = a} :: SchemaConfiguration)

-- | Specifies the table version for the output data schema. If you don\'t
-- specify this version ID, or if you set it to @LATEST@, Kinesis Data
-- Firehose uses the most recent version. This means that any updates to
-- the table are automatically picked up.
schemaConfiguration_versionId :: Lens.Lens' SchemaConfiguration (Core.Maybe Core.Text)
schemaConfiguration_versionId = Lens.lens (\SchemaConfiguration' {versionId} -> versionId) (\s@SchemaConfiguration' {} a -> s {versionId = a} :: SchemaConfiguration)

-- | If you don\'t specify an AWS Region, the default is the current Region.
schemaConfiguration_region :: Lens.Lens' SchemaConfiguration (Core.Maybe Core.Text)
schemaConfiguration_region = Lens.lens (\SchemaConfiguration' {region} -> region) (\s@SchemaConfiguration' {} a -> s {region = a} :: SchemaConfiguration)

-- | Specifies the name of the AWS Glue database that contains the schema for
-- the output data.
schemaConfiguration_databaseName :: Lens.Lens' SchemaConfiguration (Core.Maybe Core.Text)
schemaConfiguration_databaseName = Lens.lens (\SchemaConfiguration' {databaseName} -> databaseName) (\s@SchemaConfiguration' {} a -> s {databaseName = a} :: SchemaConfiguration)

instance Core.FromJSON SchemaConfiguration where
  parseJSON =
    Core.withObject
      "SchemaConfiguration"
      ( \x ->
          SchemaConfiguration'
            Core.<$> (x Core..:? "RoleARN")
            Core.<*> (x Core..:? "TableName")
            Core.<*> (x Core..:? "CatalogId")
            Core.<*> (x Core..:? "VersionId")
            Core.<*> (x Core..:? "Region")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable SchemaConfiguration

instance Core.NFData SchemaConfiguration

instance Core.ToJSON SchemaConfiguration where
  toJSON SchemaConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoleARN" Core..=) Core.<$> roleARN,
            ("TableName" Core..=) Core.<$> tableName,
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("VersionId" Core..=) Core.<$> versionId,
            ("Region" Core..=) Core.<$> region,
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )
