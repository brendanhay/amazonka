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
-- Module      : Amazonka.KinesisAnalytics.Types.ReferenceDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.ReferenceDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.S3ReferenceDataSource
import Amazonka.KinesisAnalytics.Types.SourceSchema
import qualified Amazonka.Prelude as Prelude

-- | Describes the reference data source by providing the source information
-- (S3 bucket name and object key name), the resulting in-application table
-- name that is created, and the necessary schema to map the data elements
-- in the Amazon S3 object to the in-application table.
--
-- /See:/ 'newReferenceDataSource' smart constructor.
data ReferenceDataSource = ReferenceDataSource'
  { -- | Identifies the S3 bucket and object that contains the reference data.
    -- Also identifies the IAM role Amazon Kinesis Analytics can assume to read
    -- this object on your behalf. An Amazon Kinesis Analytics application
    -- loads reference data only once. If the data changes, you call the
    -- @UpdateApplication@ operation to trigger reloading of data into your
    -- application.
    s3ReferenceDataSource :: Prelude.Maybe S3ReferenceDataSource,
    -- | Name of the in-application table to create.
    tableName :: Prelude.Text,
    -- | Describes the format of the data in the streaming source, and how each
    -- data element maps to corresponding columns created in the in-application
    -- stream.
    referenceSchema :: SourceSchema
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ReferenceDataSource', 'referenceDataSource_s3ReferenceDataSource' - Identifies the S3 bucket and object that contains the reference data.
-- Also identifies the IAM role Amazon Kinesis Analytics can assume to read
-- this object on your behalf. An Amazon Kinesis Analytics application
-- loads reference data only once. If the data changes, you call the
-- @UpdateApplication@ operation to trigger reloading of data into your
-- application.
--
-- 'tableName', 'referenceDataSource_tableName' - Name of the in-application table to create.
--
-- 'referenceSchema', 'referenceDataSource_referenceSchema' - Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
newReferenceDataSource ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'referenceSchema'
  SourceSchema ->
  ReferenceDataSource
newReferenceDataSource pTableName_ pReferenceSchema_ =
  ReferenceDataSource'
    { s3ReferenceDataSource =
        Prelude.Nothing,
      tableName = pTableName_,
      referenceSchema = pReferenceSchema_
    }

-- | Identifies the S3 bucket and object that contains the reference data.
-- Also identifies the IAM role Amazon Kinesis Analytics can assume to read
-- this object on your behalf. An Amazon Kinesis Analytics application
-- loads reference data only once. If the data changes, you call the
-- @UpdateApplication@ operation to trigger reloading of data into your
-- application.
referenceDataSource_s3ReferenceDataSource :: Lens.Lens' ReferenceDataSource (Prelude.Maybe S3ReferenceDataSource)
referenceDataSource_s3ReferenceDataSource = Lens.lens (\ReferenceDataSource' {s3ReferenceDataSource} -> s3ReferenceDataSource) (\s@ReferenceDataSource' {} a -> s {s3ReferenceDataSource = a} :: ReferenceDataSource)

-- | Name of the in-application table to create.
referenceDataSource_tableName :: Lens.Lens' ReferenceDataSource Prelude.Text
referenceDataSource_tableName = Lens.lens (\ReferenceDataSource' {tableName} -> tableName) (\s@ReferenceDataSource' {} a -> s {tableName = a} :: ReferenceDataSource)

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
referenceDataSource_referenceSchema :: Lens.Lens' ReferenceDataSource SourceSchema
referenceDataSource_referenceSchema = Lens.lens (\ReferenceDataSource' {referenceSchema} -> referenceSchema) (\s@ReferenceDataSource' {} a -> s {referenceSchema = a} :: ReferenceDataSource)

instance Prelude.Hashable ReferenceDataSource where
  hashWithSalt _salt ReferenceDataSource' {..} =
    _salt `Prelude.hashWithSalt` s3ReferenceDataSource
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` referenceSchema

instance Prelude.NFData ReferenceDataSource where
  rnf ReferenceDataSource' {..} =
    Prelude.rnf s3ReferenceDataSource
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf referenceSchema

instance Data.ToJSON ReferenceDataSource where
  toJSON ReferenceDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3ReferenceDataSource" Data..=)
              Prelude.<$> s3ReferenceDataSource,
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("ReferenceSchema" Data..= referenceSchema)
          ]
      )
