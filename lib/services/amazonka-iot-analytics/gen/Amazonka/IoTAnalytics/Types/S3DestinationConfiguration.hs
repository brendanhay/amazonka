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
-- Module      : Amazonka.IoTAnalytics.Types.S3DestinationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.S3DestinationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.GlueConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for delivery of dataset contents to Amazon
-- Simple Storage Service (Amazon S3).
--
-- /See:/ 'newS3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'
  { -- | Configuration information for coordination with Glue, a fully managed
    -- extract, transform and load (ETL) service.
    glueConfiguration :: Prelude.Maybe GlueConfiguration,
    -- | The name of the S3 bucket to which dataset contents are delivered.
    bucket :: Prelude.Text,
    -- | The key of the dataset contents object in an S3 bucket. Each object has
    -- a key that is a unique identifier. Each object has exactly one key.
    --
    -- You can create a unique key with the following options:
    --
    -- -   Use @!{iotanalytics:scheduleTime}@ to insert the time of a scheduled
    --     SQL query run.
    --
    -- -   Use @!{iotanalytics:versionId}@ to insert a unique hash that
    --     identifies a dataset content.
    --
    -- -   Use @!{iotanalytics:creationTime}@ to insert the creation time of a
    --     dataset content.
    --
    -- The following example creates a unique key for a CSV file:
    -- @dataset\/mydataset\/!{iotanalytics:scheduleTime}\/!{iotanalytics:versionId}.csv@
    --
    -- If you don\'t use @!{iotanalytics:versionId}@ to specify the key, you
    -- might get duplicate keys. For example, you might have two dataset
    -- contents with the same @scheduleTime@ but different @versionId@s. This
    -- means that one dataset content overwrites the other.
    key :: Prelude.Text,
    -- | The ARN of the role that grants IoT Analytics permission to interact
    -- with your Amazon S3 and Glue resources.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DestinationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'glueConfiguration', 's3DestinationConfiguration_glueConfiguration' - Configuration information for coordination with Glue, a fully managed
-- extract, transform and load (ETL) service.
--
-- 'bucket', 's3DestinationConfiguration_bucket' - The name of the S3 bucket to which dataset contents are delivered.
--
-- 'key', 's3DestinationConfiguration_key' - The key of the dataset contents object in an S3 bucket. Each object has
-- a key that is a unique identifier. Each object has exactly one key.
--
-- You can create a unique key with the following options:
--
-- -   Use @!{iotanalytics:scheduleTime}@ to insert the time of a scheduled
--     SQL query run.
--
-- -   Use @!{iotanalytics:versionId}@ to insert a unique hash that
--     identifies a dataset content.
--
-- -   Use @!{iotanalytics:creationTime}@ to insert the creation time of a
--     dataset content.
--
-- The following example creates a unique key for a CSV file:
-- @dataset\/mydataset\/!{iotanalytics:scheduleTime}\/!{iotanalytics:versionId}.csv@
--
-- If you don\'t use @!{iotanalytics:versionId}@ to specify the key, you
-- might get duplicate keys. For example, you might have two dataset
-- contents with the same @scheduleTime@ but different @versionId@s. This
-- means that one dataset content overwrites the other.
--
-- 'roleArn', 's3DestinationConfiguration_roleArn' - The ARN of the role that grants IoT Analytics permission to interact
-- with your Amazon S3 and Glue resources.
newS3DestinationConfiguration ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  S3DestinationConfiguration
newS3DestinationConfiguration
  pBucket_
  pKey_
  pRoleArn_ =
    S3DestinationConfiguration'
      { glueConfiguration =
          Prelude.Nothing,
        bucket = pBucket_,
        key = pKey_,
        roleArn = pRoleArn_
      }

-- | Configuration information for coordination with Glue, a fully managed
-- extract, transform and load (ETL) service.
s3DestinationConfiguration_glueConfiguration :: Lens.Lens' S3DestinationConfiguration (Prelude.Maybe GlueConfiguration)
s3DestinationConfiguration_glueConfiguration = Lens.lens (\S3DestinationConfiguration' {glueConfiguration} -> glueConfiguration) (\s@S3DestinationConfiguration' {} a -> s {glueConfiguration = a} :: S3DestinationConfiguration)

-- | The name of the S3 bucket to which dataset contents are delivered.
s3DestinationConfiguration_bucket :: Lens.Lens' S3DestinationConfiguration Prelude.Text
s3DestinationConfiguration_bucket = Lens.lens (\S3DestinationConfiguration' {bucket} -> bucket) (\s@S3DestinationConfiguration' {} a -> s {bucket = a} :: S3DestinationConfiguration)

-- | The key of the dataset contents object in an S3 bucket. Each object has
-- a key that is a unique identifier. Each object has exactly one key.
--
-- You can create a unique key with the following options:
--
-- -   Use @!{iotanalytics:scheduleTime}@ to insert the time of a scheduled
--     SQL query run.
--
-- -   Use @!{iotanalytics:versionId}@ to insert a unique hash that
--     identifies a dataset content.
--
-- -   Use @!{iotanalytics:creationTime}@ to insert the creation time of a
--     dataset content.
--
-- The following example creates a unique key for a CSV file:
-- @dataset\/mydataset\/!{iotanalytics:scheduleTime}\/!{iotanalytics:versionId}.csv@
--
-- If you don\'t use @!{iotanalytics:versionId}@ to specify the key, you
-- might get duplicate keys. For example, you might have two dataset
-- contents with the same @scheduleTime@ but different @versionId@s. This
-- means that one dataset content overwrites the other.
s3DestinationConfiguration_key :: Lens.Lens' S3DestinationConfiguration Prelude.Text
s3DestinationConfiguration_key = Lens.lens (\S3DestinationConfiguration' {key} -> key) (\s@S3DestinationConfiguration' {} a -> s {key = a} :: S3DestinationConfiguration)

-- | The ARN of the role that grants IoT Analytics permission to interact
-- with your Amazon S3 and Glue resources.
s3DestinationConfiguration_roleArn :: Lens.Lens' S3DestinationConfiguration Prelude.Text
s3DestinationConfiguration_roleArn = Lens.lens (\S3DestinationConfiguration' {roleArn} -> roleArn) (\s@S3DestinationConfiguration' {} a -> s {roleArn = a} :: S3DestinationConfiguration)

instance Data.FromJSON S3DestinationConfiguration where
  parseJSON =
    Data.withObject
      "S3DestinationConfiguration"
      ( \x ->
          S3DestinationConfiguration'
            Prelude.<$> (x Data..:? "glueConfiguration")
            Prelude.<*> (x Data..: "bucket")
            Prelude.<*> (x Data..: "key")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable S3DestinationConfiguration where
  hashWithSalt _salt S3DestinationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` glueConfiguration
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData S3DestinationConfiguration where
  rnf S3DestinationConfiguration' {..} =
    Prelude.rnf glueConfiguration
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON S3DestinationConfiguration where
  toJSON S3DestinationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("glueConfiguration" Data..=)
              Prelude.<$> glueConfiguration,
            Prelude.Just ("bucket" Data..= bucket),
            Prelude.Just ("key" Data..= key),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
