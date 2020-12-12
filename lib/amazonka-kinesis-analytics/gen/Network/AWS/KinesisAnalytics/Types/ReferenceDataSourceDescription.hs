{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
  ( ReferenceDataSourceDescription (..),

    -- * Smart constructor
    mkReferenceDataSourceDescription,

    -- * Lenses
    rdsdReferenceSchema,
    rdsdReferenceId,
    rdsdTableName,
    rdsdS3ReferenceDataSourceDescription,
  )
where

import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the reference data source configured for an application.
--
-- /See:/ 'mkReferenceDataSourceDescription' smart constructor.
data ReferenceDataSourceDescription = ReferenceDataSourceDescription'
  { referenceSchema ::
      Lude.Maybe SourceSchema,
    referenceId :: Lude.Text,
    tableName :: Lude.Text,
    s3ReferenceDataSourceDescription ::
      S3ReferenceDataSourceDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReferenceDataSourceDescription' with the minimum fields required to make a request.
--
-- * 'referenceId' - ID of the reference data source. This is the ID that Amazon Kinesis Analytics assigns when you add the reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> operation.
-- * 'referenceSchema' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
-- * 's3ReferenceDataSourceDescription' - Provides the S3 bucket name, the object key name that contains the reference data. It also provides the Amazon Resource Name (ARN) of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application reference table.
-- * 'tableName' - The in-application table name created by the specific reference data source configuration.
mkReferenceDataSourceDescription ::
  -- | 'referenceId'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  -- | 's3ReferenceDataSourceDescription'
  S3ReferenceDataSourceDescription ->
  ReferenceDataSourceDescription
mkReferenceDataSourceDescription
  pReferenceId_
  pTableName_
  pS3ReferenceDataSourceDescription_ =
    ReferenceDataSourceDescription'
      { referenceSchema = Lude.Nothing,
        referenceId = pReferenceId_,
        tableName = pTableName_,
        s3ReferenceDataSourceDescription =
          pS3ReferenceDataSourceDescription_
      }

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- /Note:/ Consider using 'referenceSchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdReferenceSchema :: Lens.Lens' ReferenceDataSourceDescription (Lude.Maybe SourceSchema)
rdsdReferenceSchema = Lens.lens (referenceSchema :: ReferenceDataSourceDescription -> Lude.Maybe SourceSchema) (\s a -> s {referenceSchema = a} :: ReferenceDataSourceDescription)
{-# DEPRECATED rdsdReferenceSchema "Use generic-lens or generic-optics with 'referenceSchema' instead." #-}

-- | ID of the reference data source. This is the ID that Amazon Kinesis Analytics assigns when you add the reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> operation.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdReferenceId :: Lens.Lens' ReferenceDataSourceDescription Lude.Text
rdsdReferenceId = Lens.lens (referenceId :: ReferenceDataSourceDescription -> Lude.Text) (\s a -> s {referenceId = a} :: ReferenceDataSourceDescription)
{-# DEPRECATED rdsdReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

-- | The in-application table name created by the specific reference data source configuration.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdTableName :: Lens.Lens' ReferenceDataSourceDescription Lude.Text
rdsdTableName = Lens.lens (tableName :: ReferenceDataSourceDescription -> Lude.Text) (\s a -> s {tableName = a} :: ReferenceDataSourceDescription)
{-# DEPRECATED rdsdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Provides the S3 bucket name, the object key name that contains the reference data. It also provides the Amazon Resource Name (ARN) of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application reference table.
--
-- /Note:/ Consider using 's3ReferenceDataSourceDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsdS3ReferenceDataSourceDescription :: Lens.Lens' ReferenceDataSourceDescription S3ReferenceDataSourceDescription
rdsdS3ReferenceDataSourceDescription = Lens.lens (s3ReferenceDataSourceDescription :: ReferenceDataSourceDescription -> S3ReferenceDataSourceDescription) (\s a -> s {s3ReferenceDataSourceDescription = a} :: ReferenceDataSourceDescription)
{-# DEPRECATED rdsdS3ReferenceDataSourceDescription "Use generic-lens or generic-optics with 's3ReferenceDataSourceDescription' instead." #-}

instance Lude.FromJSON ReferenceDataSourceDescription where
  parseJSON =
    Lude.withObject
      "ReferenceDataSourceDescription"
      ( \x ->
          ReferenceDataSourceDescription'
            Lude.<$> (x Lude..:? "ReferenceSchema")
            Lude.<*> (x Lude..: "ReferenceId")
            Lude.<*> (x Lude..: "TableName")
            Lude.<*> (x Lude..: "S3ReferenceDataSourceDescription")
      )
