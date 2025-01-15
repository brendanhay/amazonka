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
-- Module      : Amazonka.RobOMaker.Types.DataSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.DataSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.DataSourceType

-- | Information about a data source.
--
-- /See:/ 'newDataSourceConfig' smart constructor.
data DataSourceConfig = DataSourceConfig'
  { -- | The location where your files are mounted in the container image.
    --
    -- If you\'ve specified the @type@ of the data source as an @Archive@, you
    -- must provide an Amazon S3 object key to your archive. The object key
    -- must point to either a @.zip@ or @.tar.gz@ file.
    --
    -- If you\'ve specified the @type@ of the data source as a @Prefix@, you
    -- provide the Amazon S3 prefix that points to the files that you are using
    -- for your data source.
    --
    -- If you\'ve specified the @type@ of the data source as a @File@, you
    -- provide the Amazon S3 path to the file that you\'re using as your data
    -- source.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The data type for the data source that you\'re using for your container
    -- image or simulation job. You can use this field to specify whether your
    -- data source is an Archive, an Amazon S3 prefix, or a file.
    --
    -- If you don\'t specify a field, the default value is @File@.
    type' :: Prelude.Maybe DataSourceType,
    -- | The name of the data source.
    name :: Prelude.Text,
    -- | The S3 bucket where the data files are located.
    s3Bucket :: Prelude.Text,
    -- | The list of S3 keys identifying the data source files.
    s3Keys :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'dataSourceConfig_destination' - The location where your files are mounted in the container image.
--
-- If you\'ve specified the @type@ of the data source as an @Archive@, you
-- must provide an Amazon S3 object key to your archive. The object key
-- must point to either a @.zip@ or @.tar.gz@ file.
--
-- If you\'ve specified the @type@ of the data source as a @Prefix@, you
-- provide the Amazon S3 prefix that points to the files that you are using
-- for your data source.
--
-- If you\'ve specified the @type@ of the data source as a @File@, you
-- provide the Amazon S3 path to the file that you\'re using as your data
-- source.
--
-- 'type'', 'dataSourceConfig_type' - The data type for the data source that you\'re using for your container
-- image or simulation job. You can use this field to specify whether your
-- data source is an Archive, an Amazon S3 prefix, or a file.
--
-- If you don\'t specify a field, the default value is @File@.
--
-- 'name', 'dataSourceConfig_name' - The name of the data source.
--
-- 's3Bucket', 'dataSourceConfig_s3Bucket' - The S3 bucket where the data files are located.
--
-- 's3Keys', 'dataSourceConfig_s3Keys' - The list of S3 keys identifying the data source files.
newDataSourceConfig ::
  -- | 'name'
  Prelude.Text ->
  -- | 's3Bucket'
  Prelude.Text ->
  -- | 's3Keys'
  Prelude.NonEmpty Prelude.Text ->
  DataSourceConfig
newDataSourceConfig pName_ pS3Bucket_ pS3Keys_ =
  DataSourceConfig'
    { destination = Prelude.Nothing,
      type' = Prelude.Nothing,
      name = pName_,
      s3Bucket = pS3Bucket_,
      s3Keys = Lens.coerced Lens.# pS3Keys_
    }

-- | The location where your files are mounted in the container image.
--
-- If you\'ve specified the @type@ of the data source as an @Archive@, you
-- must provide an Amazon S3 object key to your archive. The object key
-- must point to either a @.zip@ or @.tar.gz@ file.
--
-- If you\'ve specified the @type@ of the data source as a @Prefix@, you
-- provide the Amazon S3 prefix that points to the files that you are using
-- for your data source.
--
-- If you\'ve specified the @type@ of the data source as a @File@, you
-- provide the Amazon S3 path to the file that you\'re using as your data
-- source.
dataSourceConfig_destination :: Lens.Lens' DataSourceConfig (Prelude.Maybe Prelude.Text)
dataSourceConfig_destination = Lens.lens (\DataSourceConfig' {destination} -> destination) (\s@DataSourceConfig' {} a -> s {destination = a} :: DataSourceConfig)

-- | The data type for the data source that you\'re using for your container
-- image or simulation job. You can use this field to specify whether your
-- data source is an Archive, an Amazon S3 prefix, or a file.
--
-- If you don\'t specify a field, the default value is @File@.
dataSourceConfig_type :: Lens.Lens' DataSourceConfig (Prelude.Maybe DataSourceType)
dataSourceConfig_type = Lens.lens (\DataSourceConfig' {type'} -> type') (\s@DataSourceConfig' {} a -> s {type' = a} :: DataSourceConfig)

-- | The name of the data source.
dataSourceConfig_name :: Lens.Lens' DataSourceConfig Prelude.Text
dataSourceConfig_name = Lens.lens (\DataSourceConfig' {name} -> name) (\s@DataSourceConfig' {} a -> s {name = a} :: DataSourceConfig)

-- | The S3 bucket where the data files are located.
dataSourceConfig_s3Bucket :: Lens.Lens' DataSourceConfig Prelude.Text
dataSourceConfig_s3Bucket = Lens.lens (\DataSourceConfig' {s3Bucket} -> s3Bucket) (\s@DataSourceConfig' {} a -> s {s3Bucket = a} :: DataSourceConfig)

-- | The list of S3 keys identifying the data source files.
dataSourceConfig_s3Keys :: Lens.Lens' DataSourceConfig (Prelude.NonEmpty Prelude.Text)
dataSourceConfig_s3Keys = Lens.lens (\DataSourceConfig' {s3Keys} -> s3Keys) (\s@DataSourceConfig' {} a -> s {s3Keys = a} :: DataSourceConfig) Prelude.. Lens.coerced

instance Data.FromJSON DataSourceConfig where
  parseJSON =
    Data.withObject
      "DataSourceConfig"
      ( \x ->
          DataSourceConfig'
            Prelude.<$> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "s3Bucket")
            Prelude.<*> (x Data..: "s3Keys")
      )

instance Prelude.Hashable DataSourceConfig where
  hashWithSalt _salt DataSourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Keys

instance Prelude.NFData DataSourceConfig where
  rnf DataSourceConfig' {..} =
    Prelude.rnf destination `Prelude.seq`
      Prelude.rnf type' `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf s3Bucket `Prelude.seq`
            Prelude.rnf s3Keys

instance Data.ToJSON DataSourceConfig where
  toJSON DataSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destination" Data..=) Prelude.<$> destination,
            ("type" Data..=) Prelude.<$> type',
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("s3Bucket" Data..= s3Bucket),
            Prelude.Just ("s3Keys" Data..= s3Keys)
          ]
      )
