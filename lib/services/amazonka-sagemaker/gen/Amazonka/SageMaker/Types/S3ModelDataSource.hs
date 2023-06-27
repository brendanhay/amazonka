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
-- Module      : Amazonka.SageMaker.Types.S3ModelDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.S3ModelDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelCompressionType
import Amazonka.SageMaker.Types.S3ModelDataType

-- | Specifies the S3 location of ML model data to deploy.
--
-- /See:/ 'newS3ModelDataSource' smart constructor.
data S3ModelDataSource = S3ModelDataSource'
  { -- | Specifies the S3 path of ML model data to deploy.
    s3Uri :: Prelude.Text,
    -- | Specifies the type of ML model data to deploy.
    --
    -- If you choose @S3Prefix@, @S3Uri@ identifies a key name prefix.
    -- SageMaker uses all objects that match the specified key name prefix as
    -- part of the ML model data to deploy. A valid key name prefix identified
    -- by @S3Uri@ always ends with a forward slash (\/).
    --
    -- If you choose S3Object, S3Uri identifies an object that is the ML model
    -- data to deploy.
    s3DataType :: S3ModelDataType,
    -- | Specifies how the ML model data is prepared.
    --
    -- If you choose @Gzip@ and choose @S3Object@ as the value of @S3DataType@,
    -- @S3Uri@ identifies an object that is a gzip-compressed TAR archive.
    -- SageMaker will attempt to decompress and untar the object during model
    -- deployment.
    --
    -- If you choose @None@ and chooose @S3Object@ as the value of
    -- @S3DataType@, @S3Uri@ identifies an object that represents an
    -- uncompressed ML model to deploy.
    --
    -- If you choose None and choose @S3Prefix@ as the value of @S3DataType@,
    -- @S3Uri@ identifies a key name prefix, under which all objects represents
    -- the uncompressed ML model to deploy.
    --
    -- If you choose None, then SageMaker will follow rules below when creating
    -- model data files under \/opt\/ml\/model directory for use by your
    -- inference code:
    --
    -- -   If you choose @S3Object@ as the value of @S3DataType@, then
    --     SageMaker will split the key of the S3 object referenced by @S3Uri@
    --     by slash (\/), and use the last part as the filename of the file
    --     holding the content of the S3 object.
    --
    -- -   If you choose @S3Prefix@ as the value of @S3DataType@, then for each
    --     S3 object under the key name pefix referenced by @S3Uri@, SageMaker
    --     will trim its key by the prefix, and use the remainder as the path
    --     (relative to @\/opt\/ml\/model@) of the file holding the content of
    --     the S3 object. SageMaker will split the remainder by slash (\/),
    --     using intermediate parts as directory names and the last part as
    --     filename of the file holding the content of the S3 object.
    --
    -- -   Do not use any of the following as file names or directory names:
    --
    --     -   An empty or blank string
    --
    --     -   A string which contains null bytes
    --
    --     -   A string longer than 255 bytes
    --
    --     -   A single dot (@.@)
    --
    --     -   A double dot (@..@)
    --
    -- -   Ambiguous file names will result in model deployment failure. For
    --     example, if your uncompressed ML model consists of two S3 objects
    --     @s3:\/\/mybucket\/model\/weights@ and
    --     @s3:\/\/mybucket\/model\/weights\/part1@ and you specify
    --     @s3:\/\/mybucket\/model\/@ as the value of @S3Uri@ and @S3Prefix@ as
    --     the value of S3DataType, then it will result in name clash between
    --     @\/opt\/ml\/model\/weights@ (a regular file) and
    --     @\/opt\/ml\/model\/weights\/@ (a directory).
    --
    -- -   Do not organize the model artifacts in
    --     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-folders.html S3 console using folders>.
    --     When you create a folder in S3 console, S3 creates a 0-byte object
    --     with a key set to the folder name you provide. They key of the
    --     0-byte object ends with a slash (\/) which violates SageMaker
    --     restrictions on model artifact file names, leading to model
    --     deployment failure.
    compressionType :: ModelCompressionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ModelDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 's3ModelDataSource_s3Uri' - Specifies the S3 path of ML model data to deploy.
--
-- 's3DataType', 's3ModelDataSource_s3DataType' - Specifies the type of ML model data to deploy.
--
-- If you choose @S3Prefix@, @S3Uri@ identifies a key name prefix.
-- SageMaker uses all objects that match the specified key name prefix as
-- part of the ML model data to deploy. A valid key name prefix identified
-- by @S3Uri@ always ends with a forward slash (\/).
--
-- If you choose S3Object, S3Uri identifies an object that is the ML model
-- data to deploy.
--
-- 'compressionType', 's3ModelDataSource_compressionType' - Specifies how the ML model data is prepared.
--
-- If you choose @Gzip@ and choose @S3Object@ as the value of @S3DataType@,
-- @S3Uri@ identifies an object that is a gzip-compressed TAR archive.
-- SageMaker will attempt to decompress and untar the object during model
-- deployment.
--
-- If you choose @None@ and chooose @S3Object@ as the value of
-- @S3DataType@, @S3Uri@ identifies an object that represents an
-- uncompressed ML model to deploy.
--
-- If you choose None and choose @S3Prefix@ as the value of @S3DataType@,
-- @S3Uri@ identifies a key name prefix, under which all objects represents
-- the uncompressed ML model to deploy.
--
-- If you choose None, then SageMaker will follow rules below when creating
-- model data files under \/opt\/ml\/model directory for use by your
-- inference code:
--
-- -   If you choose @S3Object@ as the value of @S3DataType@, then
--     SageMaker will split the key of the S3 object referenced by @S3Uri@
--     by slash (\/), and use the last part as the filename of the file
--     holding the content of the S3 object.
--
-- -   If you choose @S3Prefix@ as the value of @S3DataType@, then for each
--     S3 object under the key name pefix referenced by @S3Uri@, SageMaker
--     will trim its key by the prefix, and use the remainder as the path
--     (relative to @\/opt\/ml\/model@) of the file holding the content of
--     the S3 object. SageMaker will split the remainder by slash (\/),
--     using intermediate parts as directory names and the last part as
--     filename of the file holding the content of the S3 object.
--
-- -   Do not use any of the following as file names or directory names:
--
--     -   An empty or blank string
--
--     -   A string which contains null bytes
--
--     -   A string longer than 255 bytes
--
--     -   A single dot (@.@)
--
--     -   A double dot (@..@)
--
-- -   Ambiguous file names will result in model deployment failure. For
--     example, if your uncompressed ML model consists of two S3 objects
--     @s3:\/\/mybucket\/model\/weights@ and
--     @s3:\/\/mybucket\/model\/weights\/part1@ and you specify
--     @s3:\/\/mybucket\/model\/@ as the value of @S3Uri@ and @S3Prefix@ as
--     the value of S3DataType, then it will result in name clash between
--     @\/opt\/ml\/model\/weights@ (a regular file) and
--     @\/opt\/ml\/model\/weights\/@ (a directory).
--
-- -   Do not organize the model artifacts in
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-folders.html S3 console using folders>.
--     When you create a folder in S3 console, S3 creates a 0-byte object
--     with a key set to the folder name you provide. They key of the
--     0-byte object ends with a slash (\/) which violates SageMaker
--     restrictions on model artifact file names, leading to model
--     deployment failure.
newS3ModelDataSource ::
  -- | 's3Uri'
  Prelude.Text ->
  -- | 's3DataType'
  S3ModelDataType ->
  -- | 'compressionType'
  ModelCompressionType ->
  S3ModelDataSource
newS3ModelDataSource
  pS3Uri_
  pS3DataType_
  pCompressionType_ =
    S3ModelDataSource'
      { s3Uri = pS3Uri_,
        s3DataType = pS3DataType_,
        compressionType = pCompressionType_
      }

-- | Specifies the S3 path of ML model data to deploy.
s3ModelDataSource_s3Uri :: Lens.Lens' S3ModelDataSource Prelude.Text
s3ModelDataSource_s3Uri = Lens.lens (\S3ModelDataSource' {s3Uri} -> s3Uri) (\s@S3ModelDataSource' {} a -> s {s3Uri = a} :: S3ModelDataSource)

-- | Specifies the type of ML model data to deploy.
--
-- If you choose @S3Prefix@, @S3Uri@ identifies a key name prefix.
-- SageMaker uses all objects that match the specified key name prefix as
-- part of the ML model data to deploy. A valid key name prefix identified
-- by @S3Uri@ always ends with a forward slash (\/).
--
-- If you choose S3Object, S3Uri identifies an object that is the ML model
-- data to deploy.
s3ModelDataSource_s3DataType :: Lens.Lens' S3ModelDataSource S3ModelDataType
s3ModelDataSource_s3DataType = Lens.lens (\S3ModelDataSource' {s3DataType} -> s3DataType) (\s@S3ModelDataSource' {} a -> s {s3DataType = a} :: S3ModelDataSource)

-- | Specifies how the ML model data is prepared.
--
-- If you choose @Gzip@ and choose @S3Object@ as the value of @S3DataType@,
-- @S3Uri@ identifies an object that is a gzip-compressed TAR archive.
-- SageMaker will attempt to decompress and untar the object during model
-- deployment.
--
-- If you choose @None@ and chooose @S3Object@ as the value of
-- @S3DataType@, @S3Uri@ identifies an object that represents an
-- uncompressed ML model to deploy.
--
-- If you choose None and choose @S3Prefix@ as the value of @S3DataType@,
-- @S3Uri@ identifies a key name prefix, under which all objects represents
-- the uncompressed ML model to deploy.
--
-- If you choose None, then SageMaker will follow rules below when creating
-- model data files under \/opt\/ml\/model directory for use by your
-- inference code:
--
-- -   If you choose @S3Object@ as the value of @S3DataType@, then
--     SageMaker will split the key of the S3 object referenced by @S3Uri@
--     by slash (\/), and use the last part as the filename of the file
--     holding the content of the S3 object.
--
-- -   If you choose @S3Prefix@ as the value of @S3DataType@, then for each
--     S3 object under the key name pefix referenced by @S3Uri@, SageMaker
--     will trim its key by the prefix, and use the remainder as the path
--     (relative to @\/opt\/ml\/model@) of the file holding the content of
--     the S3 object. SageMaker will split the remainder by slash (\/),
--     using intermediate parts as directory names and the last part as
--     filename of the file holding the content of the S3 object.
--
-- -   Do not use any of the following as file names or directory names:
--
--     -   An empty or blank string
--
--     -   A string which contains null bytes
--
--     -   A string longer than 255 bytes
--
--     -   A single dot (@.@)
--
--     -   A double dot (@..@)
--
-- -   Ambiguous file names will result in model deployment failure. For
--     example, if your uncompressed ML model consists of two S3 objects
--     @s3:\/\/mybucket\/model\/weights@ and
--     @s3:\/\/mybucket\/model\/weights\/part1@ and you specify
--     @s3:\/\/mybucket\/model\/@ as the value of @S3Uri@ and @S3Prefix@ as
--     the value of S3DataType, then it will result in name clash between
--     @\/opt\/ml\/model\/weights@ (a regular file) and
--     @\/opt\/ml\/model\/weights\/@ (a directory).
--
-- -   Do not organize the model artifacts in
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-folders.html S3 console using folders>.
--     When you create a folder in S3 console, S3 creates a 0-byte object
--     with a key set to the folder name you provide. They key of the
--     0-byte object ends with a slash (\/) which violates SageMaker
--     restrictions on model artifact file names, leading to model
--     deployment failure.
s3ModelDataSource_compressionType :: Lens.Lens' S3ModelDataSource ModelCompressionType
s3ModelDataSource_compressionType = Lens.lens (\S3ModelDataSource' {compressionType} -> compressionType) (\s@S3ModelDataSource' {} a -> s {compressionType = a} :: S3ModelDataSource)

instance Data.FromJSON S3ModelDataSource where
  parseJSON =
    Data.withObject
      "S3ModelDataSource"
      ( \x ->
          S3ModelDataSource'
            Prelude.<$> (x Data..: "S3Uri")
            Prelude.<*> (x Data..: "S3DataType")
            Prelude.<*> (x Data..: "CompressionType")
      )

instance Prelude.Hashable S3ModelDataSource where
  hashWithSalt _salt S3ModelDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` s3Uri
      `Prelude.hashWithSalt` s3DataType
      `Prelude.hashWithSalt` compressionType

instance Prelude.NFData S3ModelDataSource where
  rnf S3ModelDataSource' {..} =
    Prelude.rnf s3Uri
      `Prelude.seq` Prelude.rnf s3DataType
      `Prelude.seq` Prelude.rnf compressionType

instance Data.ToJSON S3ModelDataSource where
  toJSON S3ModelDataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3Uri" Data..= s3Uri),
            Prelude.Just ("S3DataType" Data..= s3DataType),
            Prelude.Just
              ("CompressionType" Data..= compressionType)
          ]
      )
