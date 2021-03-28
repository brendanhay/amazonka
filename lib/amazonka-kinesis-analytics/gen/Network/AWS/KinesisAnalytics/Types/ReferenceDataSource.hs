{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
  ( ReferenceDataSource (..)
  -- * Smart constructor
  , mkReferenceDataSource
  -- * Lenses
  , rdsTableName
  , rdsReferenceSchema
  , rdsS3ReferenceDataSource
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource as Types
import qualified Network.AWS.KinesisAnalytics.Types.SourceSchema as Types
import qualified Network.AWS.KinesisAnalytics.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the reference data source by providing the source information (S3 bucket name and object key name), the resulting in-application table name that is created, and the necessary schema to map the data elements in the Amazon S3 object to the in-application table.
--
-- /See:/ 'mkReferenceDataSource' smart constructor.
data ReferenceDataSource = ReferenceDataSource'
  { tableName :: Types.TableName
    -- ^ Name of the in-application table to create.
  , referenceSchema :: Types.SourceSchema
    -- ^ Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
  , s3ReferenceDataSource :: Core.Maybe Types.S3ReferenceDataSource
    -- ^ Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf. An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the @UpdateApplication@ operation to trigger reloading of data into your application. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReferenceDataSource' value with any optional fields omitted.
mkReferenceDataSource
    :: Types.TableName -- ^ 'tableName'
    -> Types.SourceSchema -- ^ 'referenceSchema'
    -> ReferenceDataSource
mkReferenceDataSource tableName referenceSchema
  = ReferenceDataSource'{tableName, referenceSchema,
                         s3ReferenceDataSource = Core.Nothing}

-- | Name of the in-application table to create.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsTableName :: Lens.Lens' ReferenceDataSource Types.TableName
rdsTableName = Lens.field @"tableName"
{-# INLINEABLE rdsTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- /Note:/ Consider using 'referenceSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsReferenceSchema :: Lens.Lens' ReferenceDataSource Types.SourceSchema
rdsReferenceSchema = Lens.field @"referenceSchema"
{-# INLINEABLE rdsReferenceSchema #-}
{-# DEPRECATED referenceSchema "Use generic-lens or generic-optics with 'referenceSchema' instead"  #-}

-- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf. An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the @UpdateApplication@ operation to trigger reloading of data into your application. 
--
-- /Note:/ Consider using 's3ReferenceDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsS3ReferenceDataSource :: Lens.Lens' ReferenceDataSource (Core.Maybe Types.S3ReferenceDataSource)
rdsS3ReferenceDataSource = Lens.field @"s3ReferenceDataSource"
{-# INLINEABLE rdsS3ReferenceDataSource #-}
{-# DEPRECATED s3ReferenceDataSource "Use generic-lens or generic-optics with 's3ReferenceDataSource' instead"  #-}

instance Core.FromJSON ReferenceDataSource where
        toJSON ReferenceDataSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TableName" Core..= tableName),
                  Core.Just ("ReferenceSchema" Core..= referenceSchema),
                  ("S3ReferenceDataSource" Core..=) Core.<$> s3ReferenceDataSource])
