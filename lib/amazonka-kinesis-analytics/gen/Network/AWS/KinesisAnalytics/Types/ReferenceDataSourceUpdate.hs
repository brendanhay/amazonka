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
    rdsuTableNameUpdate,
    rdsuS3ReferenceDataSourceUpdate,
    rdsuReferenceSchemaUpdate,
    rdsuReferenceId,
  )
where

import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When you update a reference data source configuration for an application, this object provides all the updated values (such as the source bucket name and object key name), the in-application table name that is created, and updated mapping information that maps the data in the Amazon S3 object to the in-application reference table that is created.
--
-- /See:/ 'mkReferenceDataSourceUpdate' smart constructor.
data ReferenceDataSourceUpdate = ReferenceDataSourceUpdate'
  { tableNameUpdate ::
      Lude.Maybe Lude.Text,
    s3ReferenceDataSourceUpdate ::
      Lude.Maybe S3ReferenceDataSourceUpdate,
    referenceSchemaUpdate ::
      Lude.Maybe SourceSchema,
    referenceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReferenceDataSourceUpdate' with the minimum fields required to make a request.
--
-- * 'referenceId' - ID of the reference data source being updated. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
-- * 'referenceSchemaUpdate' - Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
-- * 's3ReferenceDataSourceUpdate' - Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
-- * 'tableNameUpdate' - In-application table name that is created by this update.
mkReferenceDataSourceUpdate ::
  -- | 'referenceId'
  Lude.Text ->
  ReferenceDataSourceUpdate
mkReferenceDataSourceUpdate pReferenceId_ =
  ReferenceDataSourceUpdate'
    { tableNameUpdate = Lude.Nothing,
      s3ReferenceDataSourceUpdate = Lude.Nothing,
      referenceSchemaUpdate = Lude.Nothing,
      referenceId = pReferenceId_
    }

-- | In-application table name that is created by this update.
--
-- /Note:/ Consider using 'tableNameUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsuTableNameUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Lude.Maybe Lude.Text)
rdsuTableNameUpdate = Lens.lens (tableNameUpdate :: ReferenceDataSourceUpdate -> Lude.Maybe Lude.Text) (\s a -> s {tableNameUpdate = a} :: ReferenceDataSourceUpdate)
{-# DEPRECATED rdsuTableNameUpdate "Use generic-lens or generic-optics with 'tableNameUpdate' instead." #-}

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
--
-- /Note:/ Consider using 's3ReferenceDataSourceUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsuS3ReferenceDataSourceUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Lude.Maybe S3ReferenceDataSourceUpdate)
rdsuS3ReferenceDataSourceUpdate = Lens.lens (s3ReferenceDataSourceUpdate :: ReferenceDataSourceUpdate -> Lude.Maybe S3ReferenceDataSourceUpdate) (\s a -> s {s3ReferenceDataSourceUpdate = a} :: ReferenceDataSourceUpdate)
{-# DEPRECATED rdsuS3ReferenceDataSourceUpdate "Use generic-lens or generic-optics with 's3ReferenceDataSourceUpdate' instead." #-}

-- | Describes the format of the data in the streaming source, and how each data element maps to corresponding columns created in the in-application stream.
--
-- /Note:/ Consider using 'referenceSchemaUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsuReferenceSchemaUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Lude.Maybe SourceSchema)
rdsuReferenceSchemaUpdate = Lens.lens (referenceSchemaUpdate :: ReferenceDataSourceUpdate -> Lude.Maybe SourceSchema) (\s a -> s {referenceSchemaUpdate = a} :: ReferenceDataSourceUpdate)
{-# DEPRECATED rdsuReferenceSchemaUpdate "Use generic-lens or generic-optics with 'referenceSchemaUpdate' instead." #-}

-- | ID of the reference data source being updated. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get this value.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsuReferenceId :: Lens.Lens' ReferenceDataSourceUpdate Lude.Text
rdsuReferenceId = Lens.lens (referenceId :: ReferenceDataSourceUpdate -> Lude.Text) (\s a -> s {referenceId = a} :: ReferenceDataSourceUpdate)
{-# DEPRECATED rdsuReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

instance Lude.ToJSON ReferenceDataSourceUpdate where
  toJSON ReferenceDataSourceUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TableNameUpdate" Lude..=) Lude.<$> tableNameUpdate,
            ("S3ReferenceDataSourceUpdate" Lude..=)
              Lude.<$> s3ReferenceDataSourceUpdate,
            ("ReferenceSchemaUpdate" Lude..=) Lude.<$> referenceSchemaUpdate,
            Lude.Just ("ReferenceId" Lude..= referenceId)
          ]
      )
