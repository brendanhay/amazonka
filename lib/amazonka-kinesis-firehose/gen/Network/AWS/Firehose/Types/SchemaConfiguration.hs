{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SchemaConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.SchemaConfiguration
  ( SchemaConfiguration (..)
  -- * Smart constructor
  , mkSchemaConfiguration
  -- * Lenses
  , scCatalogId
  , scDatabaseName
  , scRegion
  , scRoleARN
  , scTableName
  , scVersionId
  ) where

import qualified Network.AWS.Firehose.Types.CatalogId as Types
import qualified Network.AWS.Firehose.Types.DatabaseName as Types
import qualified Network.AWS.Firehose.Types.NonEmptyStringWithoutWhitespace as Types
import qualified Network.AWS.Firehose.Types.Region as Types
import qualified Network.AWS.Firehose.Types.TableName as Types
import qualified Network.AWS.Firehose.Types.VersionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the schema to which you want Kinesis Data Firehose to configure your data before it writes it to Amazon S3. This parameter is required if @Enabled@ is set to true.
--
-- /See:/ 'mkSchemaConfiguration' smart constructor.
data SchemaConfiguration = SchemaConfiguration'
  { catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the AWS Glue Data Catalog. If you don't supply this, the AWS account ID is used by default.
  , databaseName :: Core.Maybe Types.DatabaseName
    -- ^ Specifies the name of the AWS Glue database that contains the schema for the output data.
  , region :: Core.Maybe Types.Region
    -- ^ If you don't specify an AWS Region, the default is the current Region.
  , roleARN :: Core.Maybe Types.NonEmptyStringWithoutWhitespace
    -- ^ The role that Kinesis Data Firehose can use to access AWS Glue. This role must be in the same account you use for Kinesis Data Firehose. Cross-account roles aren't allowed.
  , tableName :: Core.Maybe Types.TableName
    -- ^ Specifies the AWS Glue table that contains the column information that constitutes your data schema.
  , versionId :: Core.Maybe Types.VersionId
    -- ^ Specifies the table version for the output data schema. If you don't specify this version ID, or if you set it to @LATEST@ , Kinesis Data Firehose uses the most recent version. This means that any updates to the table are automatically picked up.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaConfiguration' value with any optional fields omitted.
mkSchemaConfiguration
    :: SchemaConfiguration
mkSchemaConfiguration
  = SchemaConfiguration'{catalogId = Core.Nothing,
                         databaseName = Core.Nothing, region = Core.Nothing,
                         roleARN = Core.Nothing, tableName = Core.Nothing,
                         versionId = Core.Nothing}

-- | The ID of the AWS Glue Data Catalog. If you don't supply this, the AWS account ID is used by default.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scCatalogId :: Lens.Lens' SchemaConfiguration (Core.Maybe Types.CatalogId)
scCatalogId = Lens.field @"catalogId"
{-# INLINEABLE scCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | Specifies the name of the AWS Glue database that contains the schema for the output data.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scDatabaseName :: Lens.Lens' SchemaConfiguration (Core.Maybe Types.DatabaseName)
scDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE scDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | If you don't specify an AWS Region, the default is the current Region.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scRegion :: Lens.Lens' SchemaConfiguration (Core.Maybe Types.Region)
scRegion = Lens.field @"region"
{-# INLINEABLE scRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The role that Kinesis Data Firehose can use to access AWS Glue. This role must be in the same account you use for Kinesis Data Firehose. Cross-account roles aren't allowed.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scRoleARN :: Lens.Lens' SchemaConfiguration (Core.Maybe Types.NonEmptyStringWithoutWhitespace)
scRoleARN = Lens.field @"roleARN"
{-# INLINEABLE scRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

-- | Specifies the AWS Glue table that contains the column information that constitutes your data schema.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scTableName :: Lens.Lens' SchemaConfiguration (Core.Maybe Types.TableName)
scTableName = Lens.field @"tableName"
{-# INLINEABLE scTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Specifies the table version for the output data schema. If you don't specify this version ID, or if you set it to @LATEST@ , Kinesis Data Firehose uses the most recent version. This means that any updates to the table are automatically picked up.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scVersionId :: Lens.Lens' SchemaConfiguration (Core.Maybe Types.VersionId)
scVersionId = Lens.field @"versionId"
{-# INLINEABLE scVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.FromJSON SchemaConfiguration where
        toJSON SchemaConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("CatalogId" Core..=) Core.<$> catalogId,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("Region" Core..=) Core.<$> region,
                  ("RoleARN" Core..=) Core.<$> roleARN,
                  ("TableName" Core..=) Core.<$> tableName,
                  ("VersionId" Core..=) Core.<$> versionId])

instance Core.FromJSON SchemaConfiguration where
        parseJSON
          = Core.withObject "SchemaConfiguration" Core.$
              \ x ->
                SchemaConfiguration' Core.<$>
                  (x Core..:? "CatalogId") Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "Region"
                    Core.<*> x Core..:? "RoleARN"
                    Core.<*> x Core..:? "TableName"
                    Core.<*> x Core..:? "VersionId"
