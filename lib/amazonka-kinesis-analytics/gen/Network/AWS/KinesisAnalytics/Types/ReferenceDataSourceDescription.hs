{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
  ( ReferenceDataSourceDescription (..)
  -- * Smart constructor
  , mkReferenceDataSourceDescription
  -- * Lenses
  , rdsdReferenceId
  , rdsdTableName
  , rdsdS3ReferenceDataSourceDescription
  , rdsdReferenceSchema
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.Id as Types
import qualified Network.AWS.KinesisAnalytics.Types.InAppTableName as Types
import qualified Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription as Types
import qualified Network.AWS.KinesisAnalytics.Types.SourceSchema as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the reference data source configured for an application.
--
-- /See:/ 'mkReferenceDataSourceDescription' smart constructor.
data ReferenceDataSourceDescription = ReferenceDataSourceDescription'
  { referenceId :: Types.Id
    -- ^ ID of the reference data source. This is the ID that Amazon Kinesis Analytics assigns when you add the reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> operation.
  , tableName :: Types.InAppTableName
    -- ^ The in-application table name created by the specific reference data source configuration.
  , s3ReferenceDataSourceDescription :: Types.S3ReferenceDataSourceDescription
    -- ^ Provides the S3 bucket name, the object key name that contains the reference data. It also provides the Amazon Resource Name (ARN) of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application reference table.
  , referenceSchema :: Core.Maybe Types.SourceSchema
    -- ^ Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReferenceDataSourceDescription' value with any optional fields omitted.
mkReferenceDataSourceDescription
    :: Types.Id -- ^ 'referenceId'
    -> Types.InAppTableName -- ^ 'tableName'
    -> Types.S3ReferenceDataSourceDescription -- ^ 's3ReferenceDataSourceDescription'
    -> ReferenceDataSourceDescription
mkReferenceDataSourceDescription referenceId tableName
  s3ReferenceDataSourceDescription
  = ReferenceDataSourceDescription'{referenceId, tableName,
                                    s3ReferenceDataSourceDescription,
                                    referenceSchema = Core.Nothing}

-- | ID of the reference data source. This is the ID that Amazon Kinesis Analytics assigns when you add the reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> operation.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdReferenceId :: Lens.Lens' ReferenceDataSourceDescription Types.Id
rdsdReferenceId = Lens.field @"referenceId"
{-# INLINEABLE rdsdReferenceId #-}
{-# DEPRECATED referenceId "Use generic-lens or generic-optics with 'referenceId' instead"  #-}

-- | The in-application table name created by the specific reference data source configuration.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdTableName :: Lens.Lens' ReferenceDataSourceDescription Types.InAppTableName
rdsdTableName = Lens.field @"tableName"
{-# INLINEABLE rdsdTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Provides the S3 bucket name, the object key name that contains the reference data. It also provides the Amazon Resource Name (ARN) of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application reference table.
--
-- /Note:/ Consider using 's3ReferenceDataSourceDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdS3ReferenceDataSourceDescription :: Lens.Lens' ReferenceDataSourceDescription Types.S3ReferenceDataSourceDescription
rdsdS3ReferenceDataSourceDescription = Lens.field @"s3ReferenceDataSourceDescription"
{-# INLINEABLE rdsdS3ReferenceDataSourceDescription #-}
{-# DEPRECATED s3ReferenceDataSourceDescription "Use generic-lens or generic-optics with 's3ReferenceDataSourceDescription' instead"  #-}

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- /Note:/ Consider using 'referenceSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdReferenceSchema :: Lens.Lens' ReferenceDataSourceDescription (Core.Maybe Types.SourceSchema)
rdsdReferenceSchema = Lens.field @"referenceSchema"
{-# INLINEABLE rdsdReferenceSchema #-}
{-# DEPRECATED referenceSchema "Use generic-lens or generic-optics with 'referenceSchema' instead"  #-}

instance Core.FromJSON ReferenceDataSourceDescription where
        parseJSON
          = Core.withObject "ReferenceDataSourceDescription" Core.$
              \ x ->
                ReferenceDataSourceDescription' Core.<$>
                  (x Core..: "ReferenceId") Core.<*> x Core..: "TableName" Core.<*>
                    x Core..: "S3ReferenceDataSourceDescription"
                    Core.<*> x Core..:? "ReferenceSchema"
