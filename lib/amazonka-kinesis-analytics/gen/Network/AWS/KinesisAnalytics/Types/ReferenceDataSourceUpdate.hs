{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
  ( ReferenceDataSourceUpdate (..),

    -- * Smart constructor
    mkReferenceDataSourceUpdate,

    -- * Lenses
    rdsuReferenceId,
    rdsuReferenceSchemaUpdate,
    rdsuS3ReferenceDataSourceUpdate,
    rdsuTableNameUpdate,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.ReferenceId as Types
import qualified Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.SourceSchema as Types
import qualified Network.AWS.KinesisAnalytics.Types.TableNameUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When you update a reference data source configuration for an application, this object provides all the updated values (such as the source bucket name and object key name), the in-application table name that is created, and updated mapping information that maps the data in the Amazon S3 object to the in-application reference table that is created.
--
-- /See:/ 'mkReferenceDataSourceUpdate' smart constructor.
data ReferenceDataSourceUpdate = ReferenceDataSourceUpdate'
  { -- | ID of the reference data source being updated. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
    referenceId :: Types.ReferenceId,
    -- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
    referenceSchemaUpdate :: Core.Maybe Types.SourceSchema,
    -- | Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
    s3ReferenceDataSourceUpdate :: Core.Maybe Types.S3ReferenceDataSourceUpdate,
    -- | In-application table name that is created by this update.
    tableNameUpdate :: Core.Maybe Types.TableNameUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReferenceDataSourceUpdate' value with any optional fields omitted.
mkReferenceDataSourceUpdate ::
  -- | 'referenceId'
  Types.ReferenceId ->
  ReferenceDataSourceUpdate
mkReferenceDataSourceUpdate referenceId =
  ReferenceDataSourceUpdate'
    { referenceId,
      referenceSchemaUpdate = Core.Nothing,
      s3ReferenceDataSourceUpdate = Core.Nothing,
      tableNameUpdate = Core.Nothing
    }

-- | ID of the reference data source being updated. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsuReferenceId :: Lens.Lens' ReferenceDataSourceUpdate Types.ReferenceId
rdsuReferenceId = Lens.field @"referenceId"
{-# DEPRECATED rdsuReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- /Note:/ Consider using 'referenceSchemaUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsuReferenceSchemaUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Core.Maybe Types.SourceSchema)
rdsuReferenceSchemaUpdate = Lens.field @"referenceSchemaUpdate"
{-# DEPRECATED rdsuReferenceSchemaUpdate "Use generic-lens or generic-optics with 'referenceSchemaUpdate' instead." #-}

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
--
-- /Note:/ Consider using 's3ReferenceDataSourceUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsuS3ReferenceDataSourceUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Core.Maybe Types.S3ReferenceDataSourceUpdate)
rdsuS3ReferenceDataSourceUpdate = Lens.field @"s3ReferenceDataSourceUpdate"
{-# DEPRECATED rdsuS3ReferenceDataSourceUpdate "Use generic-lens or generic-optics with 's3ReferenceDataSourceUpdate' instead." #-}

-- | In-application table name that is created by this update.
--
-- /Note:/ Consider using 'tableNameUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsuTableNameUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Core.Maybe Types.TableNameUpdate)
rdsuTableNameUpdate = Lens.field @"tableNameUpdate"
{-# DEPRECATED rdsuTableNameUpdate "Use generic-lens or generic-optics with 'tableNameUpdate' instead." #-}

instance Core.FromJSON ReferenceDataSourceUpdate where
  toJSON ReferenceDataSourceUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReferenceId" Core..= referenceId),
            ("ReferenceSchemaUpdate" Core..=) Core.<$> referenceSchemaUpdate,
            ("S3ReferenceDataSourceUpdate" Core..=)
              Core.<$> s3ReferenceDataSourceUpdate,
            ("TableNameUpdate" Core..=) Core.<$> tableNameUpdate
          ]
      )
