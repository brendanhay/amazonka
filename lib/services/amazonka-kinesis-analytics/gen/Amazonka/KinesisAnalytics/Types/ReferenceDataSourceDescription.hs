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
-- Module      : Amazonka.KinesisAnalytics.Types.ReferenceDataSourceDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.ReferenceDataSourceDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
import Amazonka.KinesisAnalytics.Types.SourceSchema
import qualified Amazonka.Prelude as Prelude

-- | Describes the reference data source configured for an application.
--
-- /See:/ 'newReferenceDataSourceDescription' smart constructor.
data ReferenceDataSourceDescription = ReferenceDataSourceDescription'
  { -- | Describes the format of the data in the streaming source, and how each
    -- data element maps to corresponding columns created in the in-application
    -- stream.
    referenceSchema :: Prelude.Maybe SourceSchema,
    -- | ID of the reference data source. This is the ID that Amazon Kinesis
    -- Analytics assigns when you add the reference data source to your
    -- application using the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource>
    -- operation.
    referenceId :: Prelude.Text,
    -- | The in-application table name created by the specific reference data
    -- source configuration.
    tableName :: Prelude.Text,
    -- | Provides the S3 bucket name, the object key name that contains the
    -- reference data. It also provides the Amazon Resource Name (ARN) of the
    -- IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3
    -- object and populate the in-application reference table.
    s3ReferenceDataSourceDescription :: S3ReferenceDataSourceDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceDataSourceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceSchema', 'referenceDataSourceDescription_referenceSchema' - Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
--
-- 'referenceId', 'referenceDataSourceDescription_referenceId' - ID of the reference data source. This is the ID that Amazon Kinesis
-- Analytics assigns when you add the reference data source to your
-- application using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource>
-- operation.
--
-- 'tableName', 'referenceDataSourceDescription_tableName' - The in-application table name created by the specific reference data
-- source configuration.
--
-- 's3ReferenceDataSourceDescription', 'referenceDataSourceDescription_s3ReferenceDataSourceDescription' - Provides the S3 bucket name, the object key name that contains the
-- reference data. It also provides the Amazon Resource Name (ARN) of the
-- IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3
-- object and populate the in-application reference table.
newReferenceDataSourceDescription ::
  -- | 'referenceId'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 's3ReferenceDataSourceDescription'
  S3ReferenceDataSourceDescription ->
  ReferenceDataSourceDescription
newReferenceDataSourceDescription
  pReferenceId_
  pTableName_
  pS3ReferenceDataSourceDescription_ =
    ReferenceDataSourceDescription'
      { referenceSchema =
          Prelude.Nothing,
        referenceId = pReferenceId_,
        tableName = pTableName_,
        s3ReferenceDataSourceDescription =
          pS3ReferenceDataSourceDescription_
      }

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
referenceDataSourceDescription_referenceSchema :: Lens.Lens' ReferenceDataSourceDescription (Prelude.Maybe SourceSchema)
referenceDataSourceDescription_referenceSchema = Lens.lens (\ReferenceDataSourceDescription' {referenceSchema} -> referenceSchema) (\s@ReferenceDataSourceDescription' {} a -> s {referenceSchema = a} :: ReferenceDataSourceDescription)

-- | ID of the reference data source. This is the ID that Amazon Kinesis
-- Analytics assigns when you add the reference data source to your
-- application using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource>
-- operation.
referenceDataSourceDescription_referenceId :: Lens.Lens' ReferenceDataSourceDescription Prelude.Text
referenceDataSourceDescription_referenceId = Lens.lens (\ReferenceDataSourceDescription' {referenceId} -> referenceId) (\s@ReferenceDataSourceDescription' {} a -> s {referenceId = a} :: ReferenceDataSourceDescription)

-- | The in-application table name created by the specific reference data
-- source configuration.
referenceDataSourceDescription_tableName :: Lens.Lens' ReferenceDataSourceDescription Prelude.Text
referenceDataSourceDescription_tableName = Lens.lens (\ReferenceDataSourceDescription' {tableName} -> tableName) (\s@ReferenceDataSourceDescription' {} a -> s {tableName = a} :: ReferenceDataSourceDescription)

-- | Provides the S3 bucket name, the object key name that contains the
-- reference data. It also provides the Amazon Resource Name (ARN) of the
-- IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3
-- object and populate the in-application reference table.
referenceDataSourceDescription_s3ReferenceDataSourceDescription :: Lens.Lens' ReferenceDataSourceDescription S3ReferenceDataSourceDescription
referenceDataSourceDescription_s3ReferenceDataSourceDescription = Lens.lens (\ReferenceDataSourceDescription' {s3ReferenceDataSourceDescription} -> s3ReferenceDataSourceDescription) (\s@ReferenceDataSourceDescription' {} a -> s {s3ReferenceDataSourceDescription = a} :: ReferenceDataSourceDescription)

instance Data.FromJSON ReferenceDataSourceDescription where
  parseJSON =
    Data.withObject
      "ReferenceDataSourceDescription"
      ( \x ->
          ReferenceDataSourceDescription'
            Prelude.<$> (x Data..:? "ReferenceSchema")
            Prelude.<*> (x Data..: "ReferenceId")
            Prelude.<*> (x Data..: "TableName")
            Prelude.<*> (x Data..: "S3ReferenceDataSourceDescription")
      )

instance
  Prelude.Hashable
    ReferenceDataSourceDescription
  where
  hashWithSalt
    _salt
    ReferenceDataSourceDescription' {..} =
      _salt
        `Prelude.hashWithSalt` referenceSchema
        `Prelude.hashWithSalt` referenceId
        `Prelude.hashWithSalt` tableName
        `Prelude.hashWithSalt` s3ReferenceDataSourceDescription

instance
  Prelude.NFData
    ReferenceDataSourceDescription
  where
  rnf ReferenceDataSourceDescription' {..} =
    Prelude.rnf referenceSchema
      `Prelude.seq` Prelude.rnf referenceId
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf s3ReferenceDataSourceDescription
