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
-- Module      : Amazonka.Firehose.Types.SchemaConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.SchemaConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the schema to which you want Kinesis Data Firehose to
-- configure your data before it writes it to Amazon S3. This parameter is
-- required if @Enabled@ is set to true.
--
-- /See:/ 'newSchemaConfiguration' smart constructor.
data SchemaConfiguration = SchemaConfiguration'
  { -- | The ID of the Amazon Web Services Glue Data Catalog. If you don\'t
    -- supply this, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the Amazon Web Services Glue database that
    -- contains the schema for the output data.
    --
    -- If the @SchemaConfiguration@ request parameter is used as part of
    -- invoking the @CreateDeliveryStream@ API, then the @DatabaseName@
    -- property is required and its value must be specified.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | If you don\'t specify an Amazon Web Services Region, the default is the
    -- current Region.
    region :: Prelude.Maybe Prelude.Text,
    -- | The role that Kinesis Data Firehose can use to access Amazon Web
    -- Services Glue. This role must be in the same account you use for Kinesis
    -- Data Firehose. Cross-account roles aren\'t allowed.
    --
    -- If the @SchemaConfiguration@ request parameter is used as part of
    -- invoking the @CreateDeliveryStream@ API, then the @RoleARN@ property is
    -- required and its value must be specified.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon Web Services Glue table that contains the column
    -- information that constitutes your data schema.
    --
    -- If the @SchemaConfiguration@ request parameter is used as part of
    -- invoking the @CreateDeliveryStream@ API, then the @TableName@ property
    -- is required and its value must be specified.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the table version for the output data schema. If you don\'t
    -- specify this version ID, or if you set it to @LATEST@, Kinesis Data
    -- Firehose uses the most recent version. This means that any updates to
    -- the table are automatically picked up.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'schemaConfiguration_catalogId' - The ID of the Amazon Web Services Glue Data Catalog. If you don\'t
-- supply this, the Amazon Web Services account ID is used by default.
--
-- 'databaseName', 'schemaConfiguration_databaseName' - Specifies the name of the Amazon Web Services Glue database that
-- contains the schema for the output data.
--
-- If the @SchemaConfiguration@ request parameter is used as part of
-- invoking the @CreateDeliveryStream@ API, then the @DatabaseName@
-- property is required and its value must be specified.
--
-- 'region', 'schemaConfiguration_region' - If you don\'t specify an Amazon Web Services Region, the default is the
-- current Region.
--
-- 'roleARN', 'schemaConfiguration_roleARN' - The role that Kinesis Data Firehose can use to access Amazon Web
-- Services Glue. This role must be in the same account you use for Kinesis
-- Data Firehose. Cross-account roles aren\'t allowed.
--
-- If the @SchemaConfiguration@ request parameter is used as part of
-- invoking the @CreateDeliveryStream@ API, then the @RoleARN@ property is
-- required and its value must be specified.
--
-- 'tableName', 'schemaConfiguration_tableName' - Specifies the Amazon Web Services Glue table that contains the column
-- information that constitutes your data schema.
--
-- If the @SchemaConfiguration@ request parameter is used as part of
-- invoking the @CreateDeliveryStream@ API, then the @TableName@ property
-- is required and its value must be specified.
--
-- 'versionId', 'schemaConfiguration_versionId' - Specifies the table version for the output data schema. If you don\'t
-- specify this version ID, or if you set it to @LATEST@, Kinesis Data
-- Firehose uses the most recent version. This means that any updates to
-- the table are automatically picked up.
newSchemaConfiguration ::
  SchemaConfiguration
newSchemaConfiguration =
  SchemaConfiguration'
    { catalogId = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      region = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      tableName = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The ID of the Amazon Web Services Glue Data Catalog. If you don\'t
-- supply this, the Amazon Web Services account ID is used by default.
schemaConfiguration_catalogId :: Lens.Lens' SchemaConfiguration (Prelude.Maybe Prelude.Text)
schemaConfiguration_catalogId = Lens.lens (\SchemaConfiguration' {catalogId} -> catalogId) (\s@SchemaConfiguration' {} a -> s {catalogId = a} :: SchemaConfiguration)

-- | Specifies the name of the Amazon Web Services Glue database that
-- contains the schema for the output data.
--
-- If the @SchemaConfiguration@ request parameter is used as part of
-- invoking the @CreateDeliveryStream@ API, then the @DatabaseName@
-- property is required and its value must be specified.
schemaConfiguration_databaseName :: Lens.Lens' SchemaConfiguration (Prelude.Maybe Prelude.Text)
schemaConfiguration_databaseName = Lens.lens (\SchemaConfiguration' {databaseName} -> databaseName) (\s@SchemaConfiguration' {} a -> s {databaseName = a} :: SchemaConfiguration)

-- | If you don\'t specify an Amazon Web Services Region, the default is the
-- current Region.
schemaConfiguration_region :: Lens.Lens' SchemaConfiguration (Prelude.Maybe Prelude.Text)
schemaConfiguration_region = Lens.lens (\SchemaConfiguration' {region} -> region) (\s@SchemaConfiguration' {} a -> s {region = a} :: SchemaConfiguration)

-- | The role that Kinesis Data Firehose can use to access Amazon Web
-- Services Glue. This role must be in the same account you use for Kinesis
-- Data Firehose. Cross-account roles aren\'t allowed.
--
-- If the @SchemaConfiguration@ request parameter is used as part of
-- invoking the @CreateDeliveryStream@ API, then the @RoleARN@ property is
-- required and its value must be specified.
schemaConfiguration_roleARN :: Lens.Lens' SchemaConfiguration (Prelude.Maybe Prelude.Text)
schemaConfiguration_roleARN = Lens.lens (\SchemaConfiguration' {roleARN} -> roleARN) (\s@SchemaConfiguration' {} a -> s {roleARN = a} :: SchemaConfiguration)

-- | Specifies the Amazon Web Services Glue table that contains the column
-- information that constitutes your data schema.
--
-- If the @SchemaConfiguration@ request parameter is used as part of
-- invoking the @CreateDeliveryStream@ API, then the @TableName@ property
-- is required and its value must be specified.
schemaConfiguration_tableName :: Lens.Lens' SchemaConfiguration (Prelude.Maybe Prelude.Text)
schemaConfiguration_tableName = Lens.lens (\SchemaConfiguration' {tableName} -> tableName) (\s@SchemaConfiguration' {} a -> s {tableName = a} :: SchemaConfiguration)

-- | Specifies the table version for the output data schema. If you don\'t
-- specify this version ID, or if you set it to @LATEST@, Kinesis Data
-- Firehose uses the most recent version. This means that any updates to
-- the table are automatically picked up.
schemaConfiguration_versionId :: Lens.Lens' SchemaConfiguration (Prelude.Maybe Prelude.Text)
schemaConfiguration_versionId = Lens.lens (\SchemaConfiguration' {versionId} -> versionId) (\s@SchemaConfiguration' {} a -> s {versionId = a} :: SchemaConfiguration)

instance Data.FromJSON SchemaConfiguration where
  parseJSON =
    Data.withObject
      "SchemaConfiguration"
      ( \x ->
          SchemaConfiguration'
            Prelude.<$> (x Data..:? "CatalogId")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "RoleARN")
            Prelude.<*> (x Data..:? "TableName")
            Prelude.<*> (x Data..:? "VersionId")
      )

instance Prelude.Hashable SchemaConfiguration where
  hashWithSalt _salt SchemaConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData SchemaConfiguration where
  rnf SchemaConfiguration' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf versionId

instance Data.ToJSON SchemaConfiguration where
  toJSON SchemaConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("Region" Data..=) Prelude.<$> region,
            ("RoleARN" Data..=) Prelude.<$> roleARN,
            ("TableName" Data..=) Prelude.<$> tableName,
            ("VersionId" Data..=) Prelude.<$> versionId
          ]
      )
