{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SchemaConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SchemaConfiguration
  ( SchemaConfiguration (..),

    -- * Smart constructor
    mkSchemaConfiguration,

    -- * Lenses
    scVersionId,
    scCatalogId,
    scDatabaseName,
    scRegion,
    scTableName,
    scRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the schema to which you want Kinesis Data Firehose to configure your data before it writes it to Amazon S3. This parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'mkSchemaConfiguration' smart constructor.
data SchemaConfiguration = SchemaConfiguration'
  { versionId ::
      Lude.Maybe Lude.Text,
    catalogId :: Lude.Maybe Lude.Text,
    databaseName :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    tableName :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaConfiguration' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the AWS Glue Data Catalog. If you don't supply this, the AWS account ID is used by default.
-- * 'databaseName' - Specifies the name of the AWS Glue database that contains the schema for the output data.
-- * 'region' - If you don't specify an AWS Region, the default is the current Region.
-- * 'roleARN' - The role that Kinesis Data Firehose can use to access AWS Glue. This role must be in the same account you use for Kinesis Data Firehose. Cross-account roles aren't allowed.
-- * 'tableName' - Specifies the AWS Glue table that contains the column information that constitutes your data schema.
-- * 'versionId' - Specifies the table version for the output data schema. If you don't specify this version ID, or if you set it to @LATEST@ , Kinesis Data Firehose uses the most recent version. This means that any updates to the table are automatically picked up.
mkSchemaConfiguration ::
  SchemaConfiguration
mkSchemaConfiguration =
  SchemaConfiguration'
    { versionId = Lude.Nothing,
      catalogId = Lude.Nothing,
      databaseName = Lude.Nothing,
      region = Lude.Nothing,
      tableName = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Specifies the table version for the output data schema. If you don't specify this version ID, or if you set it to @LATEST@ , Kinesis Data Firehose uses the most recent version. This means that any updates to the table are automatically picked up.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scVersionId :: Lens.Lens' SchemaConfiguration (Lude.Maybe Lude.Text)
scVersionId = Lens.lens (versionId :: SchemaConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: SchemaConfiguration)
{-# DEPRECATED scVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ID of the AWS Glue Data Catalog. If you don't supply this, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scCatalogId :: Lens.Lens' SchemaConfiguration (Lude.Maybe Lude.Text)
scCatalogId = Lens.lens (catalogId :: SchemaConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: SchemaConfiguration)
{-# DEPRECATED scCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | Specifies the name of the AWS Glue database that contains the schema for the output data.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDatabaseName :: Lens.Lens' SchemaConfiguration (Lude.Maybe Lude.Text)
scDatabaseName = Lens.lens (databaseName :: SchemaConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: SchemaConfiguration)
{-# DEPRECATED scDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | If you don't specify an AWS Region, the default is the current Region.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scRegion :: Lens.Lens' SchemaConfiguration (Lude.Maybe Lude.Text)
scRegion = Lens.lens (region :: SchemaConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: SchemaConfiguration)
{-# DEPRECATED scRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Specifies the AWS Glue table that contains the column information that constitutes your data schema.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTableName :: Lens.Lens' SchemaConfiguration (Lude.Maybe Lude.Text)
scTableName = Lens.lens (tableName :: SchemaConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: SchemaConfiguration)
{-# DEPRECATED scTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The role that Kinesis Data Firehose can use to access AWS Glue. This role must be in the same account you use for Kinesis Data Firehose. Cross-account roles aren't allowed.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scRoleARN :: Lens.Lens' SchemaConfiguration (Lude.Maybe Lude.Text)
scRoleARN = Lens.lens (roleARN :: SchemaConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: SchemaConfiguration)
{-# DEPRECATED scRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON SchemaConfiguration where
  parseJSON =
    Lude.withObject
      "SchemaConfiguration"
      ( \x ->
          SchemaConfiguration'
            Lude.<$> (x Lude..:? "VersionId")
            Lude.<*> (x Lude..:? "CatalogId")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "TableName")
            Lude.<*> (x Lude..:? "RoleARN")
      )

instance Lude.ToJSON SchemaConfiguration where
  toJSON SchemaConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VersionId" Lude..=) Lude.<$> versionId,
            ("CatalogId" Lude..=) Lude.<$> catalogId,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("Region" Lude..=) Lude.<$> region,
            ("TableName" Lude..=) Lude.<$> tableName,
            ("RoleARN" Lude..=) Lude.<$> roleARN
          ]
      )
