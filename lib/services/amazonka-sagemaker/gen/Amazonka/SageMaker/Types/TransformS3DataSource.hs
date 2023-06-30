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
-- Module      : Amazonka.SageMaker.Types.TransformS3DataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TransformS3DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.S3DataType

-- | Describes the S3 data source.
--
-- /See:/ 'newTransformS3DataSource' smart constructor.
data TransformS3DataSource = TransformS3DataSource'
  { -- | If you choose @S3Prefix@, @S3Uri@ identifies a key name prefix. Amazon
    -- SageMaker uses all objects with the specified key name prefix for batch
    -- transform.
    --
    -- If you choose @ManifestFile@, @S3Uri@ identifies an object that is a
    -- manifest file containing a list of object keys that you want Amazon
    -- SageMaker to use for batch transform.
    --
    -- The following values are compatible: @ManifestFile@, @S3Prefix@
    --
    -- The following value is not compatible: @AugmentedManifestFile@
    s3DataType :: S3DataType,
    -- | Depending on the value specified for the @S3DataType@, identifies either
    -- a key name prefix or a manifest. For example:
    --
    -- -   A key name prefix might look like this:
    --     @s3:\/\/bucketname\/exampleprefix@.
    --
    -- -   A manifest might look like this:
    --     @s3:\/\/bucketname\/example.manifest@
    --
    --     The manifest is an S3 object which is a JSON file with the following
    --     format:
    --
    --     @[ {\"prefix\": \"s3:\/\/customer_bucket\/some\/prefix\/\"},@
    --
    --     @\"relative\/path\/to\/custdata-1\",@
    --
    --     @\"relative\/path\/custdata-2\",@
    --
    --     @...@
    --
    --     @\"relative\/path\/custdata-N\"@
    --
    --     @]@
    --
    --     The preceding JSON matches the following @S3Uris@:
    --
    --     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/to\/custdata-1@
    --
    --     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-2@
    --
    --     @...@
    --
    --     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-N@
    --
    --     The complete set of @S3Uris@ in this manifest constitutes the input
    --     data for the channel for this datasource. The object that each
    --     @S3Uris@ points to must be readable by the IAM role that Amazon
    --     SageMaker uses to perform tasks on your behalf.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformS3DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataType', 'transformS3DataSource_s3DataType' - If you choose @S3Prefix@, @S3Uri@ identifies a key name prefix. Amazon
-- SageMaker uses all objects with the specified key name prefix for batch
-- transform.
--
-- If you choose @ManifestFile@, @S3Uri@ identifies an object that is a
-- manifest file containing a list of object keys that you want Amazon
-- SageMaker to use for batch transform.
--
-- The following values are compatible: @ManifestFile@, @S3Prefix@
--
-- The following value is not compatible: @AugmentedManifestFile@
--
-- 's3Uri', 'transformS3DataSource_s3Uri' - Depending on the value specified for the @S3DataType@, identifies either
-- a key name prefix or a manifest. For example:
--
-- -   A key name prefix might look like this:
--     @s3:\/\/bucketname\/exampleprefix@.
--
-- -   A manifest might look like this:
--     @s3:\/\/bucketname\/example.manifest@
--
--     The manifest is an S3 object which is a JSON file with the following
--     format:
--
--     @[ {\"prefix\": \"s3:\/\/customer_bucket\/some\/prefix\/\"},@
--
--     @\"relative\/path\/to\/custdata-1\",@
--
--     @\"relative\/path\/custdata-2\",@
--
--     @...@
--
--     @\"relative\/path\/custdata-N\"@
--
--     @]@
--
--     The preceding JSON matches the following @S3Uris@:
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/to\/custdata-1@
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-2@
--
--     @...@
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-N@
--
--     The complete set of @S3Uris@ in this manifest constitutes the input
--     data for the channel for this datasource. The object that each
--     @S3Uris@ points to must be readable by the IAM role that Amazon
--     SageMaker uses to perform tasks on your behalf.
newTransformS3DataSource ::
  -- | 's3DataType'
  S3DataType ->
  -- | 's3Uri'
  Prelude.Text ->
  TransformS3DataSource
newTransformS3DataSource pS3DataType_ pS3Uri_ =
  TransformS3DataSource'
    { s3DataType = pS3DataType_,
      s3Uri = pS3Uri_
    }

-- | If you choose @S3Prefix@, @S3Uri@ identifies a key name prefix. Amazon
-- SageMaker uses all objects with the specified key name prefix for batch
-- transform.
--
-- If you choose @ManifestFile@, @S3Uri@ identifies an object that is a
-- manifest file containing a list of object keys that you want Amazon
-- SageMaker to use for batch transform.
--
-- The following values are compatible: @ManifestFile@, @S3Prefix@
--
-- The following value is not compatible: @AugmentedManifestFile@
transformS3DataSource_s3DataType :: Lens.Lens' TransformS3DataSource S3DataType
transformS3DataSource_s3DataType = Lens.lens (\TransformS3DataSource' {s3DataType} -> s3DataType) (\s@TransformS3DataSource' {} a -> s {s3DataType = a} :: TransformS3DataSource)

-- | Depending on the value specified for the @S3DataType@, identifies either
-- a key name prefix or a manifest. For example:
--
-- -   A key name prefix might look like this:
--     @s3:\/\/bucketname\/exampleprefix@.
--
-- -   A manifest might look like this:
--     @s3:\/\/bucketname\/example.manifest@
--
--     The manifest is an S3 object which is a JSON file with the following
--     format:
--
--     @[ {\"prefix\": \"s3:\/\/customer_bucket\/some\/prefix\/\"},@
--
--     @\"relative\/path\/to\/custdata-1\",@
--
--     @\"relative\/path\/custdata-2\",@
--
--     @...@
--
--     @\"relative\/path\/custdata-N\"@
--
--     @]@
--
--     The preceding JSON matches the following @S3Uris@:
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/to\/custdata-1@
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-2@
--
--     @...@
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-N@
--
--     The complete set of @S3Uris@ in this manifest constitutes the input
--     data for the channel for this datasource. The object that each
--     @S3Uris@ points to must be readable by the IAM role that Amazon
--     SageMaker uses to perform tasks on your behalf.
transformS3DataSource_s3Uri :: Lens.Lens' TransformS3DataSource Prelude.Text
transformS3DataSource_s3Uri = Lens.lens (\TransformS3DataSource' {s3Uri} -> s3Uri) (\s@TransformS3DataSource' {} a -> s {s3Uri = a} :: TransformS3DataSource)

instance Data.FromJSON TransformS3DataSource where
  parseJSON =
    Data.withObject
      "TransformS3DataSource"
      ( \x ->
          TransformS3DataSource'
            Prelude.<$> (x Data..: "S3DataType")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable TransformS3DataSource where
  hashWithSalt _salt TransformS3DataSource' {..} =
    _salt
      `Prelude.hashWithSalt` s3DataType
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData TransformS3DataSource where
  rnf TransformS3DataSource' {..} =
    Prelude.rnf s3DataType
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON TransformS3DataSource where
  toJSON TransformS3DataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3DataType" Data..= s3DataType),
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
