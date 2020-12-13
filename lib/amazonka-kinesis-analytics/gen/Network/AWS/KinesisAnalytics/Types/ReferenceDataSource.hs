{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
  ( ReferenceDataSource (..),

    -- * Smart constructor
    mkReferenceDataSource,

    -- * Lenses
    rdsReferenceSchema,
    rdsS3ReferenceDataSource,
    rdsTableName,
  )
where

import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the reference data source by providing the source information (S3 bucket name and object key name), the resulting in-application table name that is created, and the necessary schema to map the data elements in the Amazon S3 object to the in-application table.
--
-- /See:/ 'mkReferenceDataSource' smart constructor.
data ReferenceDataSource = ReferenceDataSource'
  { -- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
    referenceSchema :: SourceSchema,
    -- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf. An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the @UpdateApplication@ operation to trigger reloading of data into your application.
    s3ReferenceDataSource :: Lude.Maybe S3ReferenceDataSource,
    -- | Name of the in-application table to create.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReferenceDataSource' with the minimum fields required to make a request.
--
-- * 'referenceSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
-- * 's3ReferenceDataSource' - Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf. An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the @UpdateApplication@ operation to trigger reloading of data into your application.
-- * 'tableName' - Name of the in-application table to create.
mkReferenceDataSource ::
  -- | 'referenceSchema'
  SourceSchema ->
  -- | 'tableName'
  Lude.Text ->
  ReferenceDataSource
mkReferenceDataSource pReferenceSchema_ pTableName_ =
  ReferenceDataSource'
    { referenceSchema = pReferenceSchema_,
      s3ReferenceDataSource = Lude.Nothing,
      tableName = pTableName_
    }

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- /Note:/ Consider using 'referenceSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsReferenceSchema :: Lens.Lens' ReferenceDataSource SourceSchema
rdsReferenceSchema = Lens.lens (referenceSchema :: ReferenceDataSource -> SourceSchema) (\s a -> s {referenceSchema = a} :: ReferenceDataSource)
{-# DEPRECATED rdsReferenceSchema "Use generic-lens or generic-optics with 'referenceSchema' instead." #-}

-- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf. An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the @UpdateApplication@ operation to trigger reloading of data into your application.
--
-- /Note:/ Consider using 's3ReferenceDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsS3ReferenceDataSource :: Lens.Lens' ReferenceDataSource (Lude.Maybe S3ReferenceDataSource)
rdsS3ReferenceDataSource = Lens.lens (s3ReferenceDataSource :: ReferenceDataSource -> Lude.Maybe S3ReferenceDataSource) (\s a -> s {s3ReferenceDataSource = a} :: ReferenceDataSource)
{-# DEPRECATED rdsS3ReferenceDataSource "Use generic-lens or generic-optics with 's3ReferenceDataSource' instead." #-}

-- | Name of the in-application table to create.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsTableName :: Lens.Lens' ReferenceDataSource Lude.Text
rdsTableName = Lens.lens (tableName :: ReferenceDataSource -> Lude.Text) (\s a -> s {tableName = a} :: ReferenceDataSource)
{-# DEPRECATED rdsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.ToJSON ReferenceDataSource where
  toJSON ReferenceDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ReferenceSchema" Lude..= referenceSchema),
            ("S3ReferenceDataSource" Lude..=) Lude.<$> s3ReferenceDataSource,
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )
