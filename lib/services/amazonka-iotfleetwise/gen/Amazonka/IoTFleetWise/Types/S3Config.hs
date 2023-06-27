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
-- Module      : Amazonka.IoTFleetWise.Types.S3Config
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.S3Config where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.DataFormat
import Amazonka.IoTFleetWise.Types.StorageCompressionFormat
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 bucket where the Amazon Web Services IoT FleetWise
-- campaign sends data. Amazon S3 is an object storage service that stores
-- data as objects within buckets. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/creating-buckets-s3.html Creating, configuring, and working with Amazon S3 buckets>
-- in the /Amazon Simple Storage Service User Guide/.
--
-- /See:/ 'newS3Config' smart constructor.
data S3Config = S3Config'
  { -- | Specify the format that files are saved in the Amazon S3 bucket. You can
    -- save files in an Apache Parquet or JSON format.
    --
    -- -   Parquet - Store data in a columnar storage file format. Parquet is
    --     optimal for fast data retrieval and can reduce costs. This option is
    --     selected by default.
    --
    -- -   JSON - Store data in a standard text-based JSON file format.
    dataFormat :: Prelude.Maybe DataFormat,
    -- | (Optional) Enter an S3 bucket prefix. The prefix is the string of
    -- characters after the bucket name and before the object name. You can use
    -- the prefix to organize data stored in Amazon S3 buckets. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-prefixes.html Organizing objects using prefixes>
    -- in the /Amazon Simple Storage Service User Guide/.
    --
    -- By default, Amazon Web Services IoT FleetWise sets the prefix
    -- @processed-data\/year=YY\/month=MM\/date=DD\/hour=HH\/@ (in UTC) to data
    -- it delivers to Amazon S3. You can enter a prefix to append it to this
    -- default prefix. For example, if you enter the prefix @vehicles@, the
    -- prefix will be
    -- @vehicles\/processed-data\/year=YY\/month=MM\/date=DD\/hour=HH\/@.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | By default, stored data is compressed as a .gzip file. Compressed files
    -- have a reduced file size, which can optimize the cost of data storage.
    storageCompressionFormat :: Prelude.Maybe StorageCompressionFormat,
    -- | The Amazon Resource Name (ARN) of the Amazon S3 bucket.
    bucketArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Config' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataFormat', 's3Config_dataFormat' - Specify the format that files are saved in the Amazon S3 bucket. You can
-- save files in an Apache Parquet or JSON format.
--
-- -   Parquet - Store data in a columnar storage file format. Parquet is
--     optimal for fast data retrieval and can reduce costs. This option is
--     selected by default.
--
-- -   JSON - Store data in a standard text-based JSON file format.
--
-- 'prefix', 's3Config_prefix' - (Optional) Enter an S3 bucket prefix. The prefix is the string of
-- characters after the bucket name and before the object name. You can use
-- the prefix to organize data stored in Amazon S3 buckets. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-prefixes.html Organizing objects using prefixes>
-- in the /Amazon Simple Storage Service User Guide/.
--
-- By default, Amazon Web Services IoT FleetWise sets the prefix
-- @processed-data\/year=YY\/month=MM\/date=DD\/hour=HH\/@ (in UTC) to data
-- it delivers to Amazon S3. You can enter a prefix to append it to this
-- default prefix. For example, if you enter the prefix @vehicles@, the
-- prefix will be
-- @vehicles\/processed-data\/year=YY\/month=MM\/date=DD\/hour=HH\/@.
--
-- 'storageCompressionFormat', 's3Config_storageCompressionFormat' - By default, stored data is compressed as a .gzip file. Compressed files
-- have a reduced file size, which can optimize the cost of data storage.
--
-- 'bucketArn', 's3Config_bucketArn' - The Amazon Resource Name (ARN) of the Amazon S3 bucket.
newS3Config ::
  -- | 'bucketArn'
  Prelude.Text ->
  S3Config
newS3Config pBucketArn_ =
  S3Config'
    { dataFormat = Prelude.Nothing,
      prefix = Prelude.Nothing,
      storageCompressionFormat = Prelude.Nothing,
      bucketArn = pBucketArn_
    }

-- | Specify the format that files are saved in the Amazon S3 bucket. You can
-- save files in an Apache Parquet or JSON format.
--
-- -   Parquet - Store data in a columnar storage file format. Parquet is
--     optimal for fast data retrieval and can reduce costs. This option is
--     selected by default.
--
-- -   JSON - Store data in a standard text-based JSON file format.
s3Config_dataFormat :: Lens.Lens' S3Config (Prelude.Maybe DataFormat)
s3Config_dataFormat = Lens.lens (\S3Config' {dataFormat} -> dataFormat) (\s@S3Config' {} a -> s {dataFormat = a} :: S3Config)

-- | (Optional) Enter an S3 bucket prefix. The prefix is the string of
-- characters after the bucket name and before the object name. You can use
-- the prefix to organize data stored in Amazon S3 buckets. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-prefixes.html Organizing objects using prefixes>
-- in the /Amazon Simple Storage Service User Guide/.
--
-- By default, Amazon Web Services IoT FleetWise sets the prefix
-- @processed-data\/year=YY\/month=MM\/date=DD\/hour=HH\/@ (in UTC) to data
-- it delivers to Amazon S3. You can enter a prefix to append it to this
-- default prefix. For example, if you enter the prefix @vehicles@, the
-- prefix will be
-- @vehicles\/processed-data\/year=YY\/month=MM\/date=DD\/hour=HH\/@.
s3Config_prefix :: Lens.Lens' S3Config (Prelude.Maybe Prelude.Text)
s3Config_prefix = Lens.lens (\S3Config' {prefix} -> prefix) (\s@S3Config' {} a -> s {prefix = a} :: S3Config)

-- | By default, stored data is compressed as a .gzip file. Compressed files
-- have a reduced file size, which can optimize the cost of data storage.
s3Config_storageCompressionFormat :: Lens.Lens' S3Config (Prelude.Maybe StorageCompressionFormat)
s3Config_storageCompressionFormat = Lens.lens (\S3Config' {storageCompressionFormat} -> storageCompressionFormat) (\s@S3Config' {} a -> s {storageCompressionFormat = a} :: S3Config)

-- | The Amazon Resource Name (ARN) of the Amazon S3 bucket.
s3Config_bucketArn :: Lens.Lens' S3Config Prelude.Text
s3Config_bucketArn = Lens.lens (\S3Config' {bucketArn} -> bucketArn) (\s@S3Config' {} a -> s {bucketArn = a} :: S3Config)

instance Data.FromJSON S3Config where
  parseJSON =
    Data.withObject
      "S3Config"
      ( \x ->
          S3Config'
            Prelude.<$> (x Data..:? "dataFormat")
            Prelude.<*> (x Data..:? "prefix")
            Prelude.<*> (x Data..:? "storageCompressionFormat")
            Prelude.<*> (x Data..: "bucketArn")
      )

instance Prelude.Hashable S3Config where
  hashWithSalt _salt S3Config' {..} =
    _salt
      `Prelude.hashWithSalt` dataFormat
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` storageCompressionFormat
      `Prelude.hashWithSalt` bucketArn

instance Prelude.NFData S3Config where
  rnf S3Config' {..} =
    Prelude.rnf dataFormat
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf storageCompressionFormat
      `Prelude.seq` Prelude.rnf bucketArn

instance Data.ToJSON S3Config where
  toJSON S3Config' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataFormat" Data..=) Prelude.<$> dataFormat,
            ("prefix" Data..=) Prelude.<$> prefix,
            ("storageCompressionFormat" Data..=)
              Prelude.<$> storageCompressionFormat,
            Prelude.Just ("bucketArn" Data..= bucketArn)
          ]
      )
