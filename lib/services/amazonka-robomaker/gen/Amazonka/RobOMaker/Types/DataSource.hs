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
-- Module      : Amazonka.RobOMaker.Types.DataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.DataSourceType
import Amazonka.RobOMaker.Types.S3KeyOutput

-- | Information about a data source.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
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
    -- | The name of the data source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The S3 bucket where the data files are located.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The list of S3 keys identifying the data source files.
    s3Keys :: Prelude.Maybe [S3KeyOutput],
    -- | The data type for the data source that you\'re using for your container
    -- image or simulation job. You can use this field to specify whether your
    -- data source is an Archive, an Amazon S3 prefix, or a file.
    --
    -- If you don\'t specify a field, the default value is @File@.
    type' :: Prelude.Maybe DataSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'dataSource_destination' - The location where your files are mounted in the container image.
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
-- 'name', 'dataSource_name' - The name of the data source.
--
-- 's3Bucket', 'dataSource_s3Bucket' - The S3 bucket where the data files are located.
--
-- 's3Keys', 'dataSource_s3Keys' - The list of S3 keys identifying the data source files.
--
-- 'type'', 'dataSource_type' - The data type for the data source that you\'re using for your container
-- image or simulation job. You can use this field to specify whether your
-- data source is an Archive, an Amazon S3 prefix, or a file.
--
-- If you don\'t specify a field, the default value is @File@.
newDataSource ::
  DataSource
newDataSource =
  DataSource'
    { destination = Prelude.Nothing,
      name = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      s3Keys = Prelude.Nothing,
      type' = Prelude.Nothing
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
dataSource_destination :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_destination = Lens.lens (\DataSource' {destination} -> destination) (\s@DataSource' {} a -> s {destination = a} :: DataSource)

-- | The name of the data source.
dataSource_name :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_name = Lens.lens (\DataSource' {name} -> name) (\s@DataSource' {} a -> s {name = a} :: DataSource)

-- | The S3 bucket where the data files are located.
dataSource_s3Bucket :: Lens.Lens' DataSource (Prelude.Maybe Prelude.Text)
dataSource_s3Bucket = Lens.lens (\DataSource' {s3Bucket} -> s3Bucket) (\s@DataSource' {} a -> s {s3Bucket = a} :: DataSource)

-- | The list of S3 keys identifying the data source files.
dataSource_s3Keys :: Lens.Lens' DataSource (Prelude.Maybe [S3KeyOutput])
dataSource_s3Keys = Lens.lens (\DataSource' {s3Keys} -> s3Keys) (\s@DataSource' {} a -> s {s3Keys = a} :: DataSource) Prelude.. Lens.mapping Lens.coerced

-- | The data type for the data source that you\'re using for your container
-- image or simulation job. You can use this field to specify whether your
-- data source is an Archive, an Amazon S3 prefix, or a file.
--
-- If you don\'t specify a field, the default value is @File@.
dataSource_type :: Lens.Lens' DataSource (Prelude.Maybe DataSourceType)
dataSource_type = Lens.lens (\DataSource' {type'} -> type') (\s@DataSource' {} a -> s {type' = a} :: DataSource)

instance Data.FromJSON DataSource where
  parseJSON =
    Data.withObject
      "DataSource"
      ( \x ->
          DataSource'
            Prelude.<$> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "s3Bucket")
            Prelude.<*> (x Data..:? "s3Keys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Keys
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DataSource where
  rnf DataSource' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Keys
      `Prelude.seq` Prelude.rnf type'
