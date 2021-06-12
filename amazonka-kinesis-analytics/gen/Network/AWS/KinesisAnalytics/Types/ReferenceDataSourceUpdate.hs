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
-- Module      : Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import qualified Network.AWS.Lens as Lens

-- | When you update a reference data source configuration for an
-- application, this object provides all the updated values (such as the
-- source bucket name and object key name), the in-application table name
-- that is created, and updated mapping information that maps the data in
-- the Amazon S3 object to the in-application reference table that is
-- created.
--
-- /See:/ 'newReferenceDataSourceUpdate' smart constructor.
data ReferenceDataSourceUpdate = ReferenceDataSourceUpdate'
  { -- | Describes the S3 bucket name, object key name, and IAM role that Amazon
    -- Kinesis Analytics can assume to read the Amazon S3 object on your behalf
    -- and populate the in-application reference table.
    s3ReferenceDataSourceUpdate :: Core.Maybe S3ReferenceDataSourceUpdate,
    -- | Describes the format of the data in the streaming source, and how each
    -- data element maps to corresponding columns created in the in-application
    -- stream.
    referenceSchemaUpdate :: Core.Maybe SourceSchema,
    -- | In-application table name that is created by this update.
    tableNameUpdate :: Core.Maybe Core.Text,
    -- | ID of the reference data source being updated. You can use the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
    -- operation to get this value.
    referenceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReferenceDataSourceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ReferenceDataSourceUpdate', 'referenceDataSourceUpdate_s3ReferenceDataSourceUpdate' - Describes the S3 bucket name, object key name, and IAM role that Amazon
-- Kinesis Analytics can assume to read the Amazon S3 object on your behalf
-- and populate the in-application reference table.
--
-- 'referenceSchemaUpdate', 'referenceDataSourceUpdate_referenceSchemaUpdate' - Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
--
-- 'tableNameUpdate', 'referenceDataSourceUpdate_tableNameUpdate' - In-application table name that is created by this update.
--
-- 'referenceId', 'referenceDataSourceUpdate_referenceId' - ID of the reference data source being updated. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get this value.
newReferenceDataSourceUpdate ::
  -- | 'referenceId'
  Core.Text ->
  ReferenceDataSourceUpdate
newReferenceDataSourceUpdate pReferenceId_ =
  ReferenceDataSourceUpdate'
    { s3ReferenceDataSourceUpdate =
        Core.Nothing,
      referenceSchemaUpdate = Core.Nothing,
      tableNameUpdate = Core.Nothing,
      referenceId = pReferenceId_
    }

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon
-- Kinesis Analytics can assume to read the Amazon S3 object on your behalf
-- and populate the in-application reference table.
referenceDataSourceUpdate_s3ReferenceDataSourceUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Core.Maybe S3ReferenceDataSourceUpdate)
referenceDataSourceUpdate_s3ReferenceDataSourceUpdate = Lens.lens (\ReferenceDataSourceUpdate' {s3ReferenceDataSourceUpdate} -> s3ReferenceDataSourceUpdate) (\s@ReferenceDataSourceUpdate' {} a -> s {s3ReferenceDataSourceUpdate = a} :: ReferenceDataSourceUpdate)

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
referenceDataSourceUpdate_referenceSchemaUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Core.Maybe SourceSchema)
referenceDataSourceUpdate_referenceSchemaUpdate = Lens.lens (\ReferenceDataSourceUpdate' {referenceSchemaUpdate} -> referenceSchemaUpdate) (\s@ReferenceDataSourceUpdate' {} a -> s {referenceSchemaUpdate = a} :: ReferenceDataSourceUpdate)

-- | In-application table name that is created by this update.
referenceDataSourceUpdate_tableNameUpdate :: Lens.Lens' ReferenceDataSourceUpdate (Core.Maybe Core.Text)
referenceDataSourceUpdate_tableNameUpdate = Lens.lens (\ReferenceDataSourceUpdate' {tableNameUpdate} -> tableNameUpdate) (\s@ReferenceDataSourceUpdate' {} a -> s {tableNameUpdate = a} :: ReferenceDataSourceUpdate)

-- | ID of the reference data source being updated. You can use the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication>
-- operation to get this value.
referenceDataSourceUpdate_referenceId :: Lens.Lens' ReferenceDataSourceUpdate Core.Text
referenceDataSourceUpdate_referenceId = Lens.lens (\ReferenceDataSourceUpdate' {referenceId} -> referenceId) (\s@ReferenceDataSourceUpdate' {} a -> s {referenceId = a} :: ReferenceDataSourceUpdate)

instance Core.Hashable ReferenceDataSourceUpdate

instance Core.NFData ReferenceDataSourceUpdate

instance Core.ToJSON ReferenceDataSourceUpdate where
  toJSON ReferenceDataSourceUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3ReferenceDataSourceUpdate" Core..=)
              Core.<$> s3ReferenceDataSourceUpdate,
            ("ReferenceSchemaUpdate" Core..=)
              Core.<$> referenceSchemaUpdate,
            ("TableNameUpdate" Core..=) Core.<$> tableNameUpdate,
            Core.Just ("ReferenceId" Core..= referenceId)
          ]
      )
