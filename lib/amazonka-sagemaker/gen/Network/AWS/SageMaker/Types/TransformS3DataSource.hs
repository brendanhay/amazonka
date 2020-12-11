-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformS3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformS3DataSource
  ( TransformS3DataSource (..),

    -- * Smart constructor
    mkTransformS3DataSource,

    -- * Lenses
    tsdsS3DataType,
    tsdsS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.S3DataType

-- | Describes the S3 data source.
--
-- /See:/ 'mkTransformS3DataSource' smart constructor.
data TransformS3DataSource = TransformS3DataSource'
  { s3DataType ::
      S3DataType,
    s3URI :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformS3DataSource' with the minimum fields required to make a request.
--
-- * 's3DataType' - If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for batch transform.
--
-- If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for batch transform.
-- The following values are compatible: @ManifestFile@ , @S3Prefix@
-- The following value is not compatible: @AugmentedManifestFile@
-- * 's3URI' - Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:
--
--
--     * A key name prefix might look like this: @s3://bucketname/exampleprefix@ .
--
--
--     * A manifest might look like this: @s3://bucketname/example.manifest@
-- The manifest is an S3 object which is a JSON file with the following format:
-- @[ {"prefix": "s3://customer_bucket/some/prefix/"},@
-- @"relative/path/to/custdata-1",@
-- @"relative/path/custdata-2",@
-- @...@
-- @"relative/path/custdata-N"@
-- @]@
-- The preceding JSON matches the following @S3Uris@ :
-- @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@
-- @s3://customer_bucket/some/prefix/relative/path/custdata-2@
-- @...@
-- @s3://customer_bucket/some/prefix/relative/path/custdata-N@
-- The complete set of @S3Uris@ in this manifest constitutes the input data for the channel for this datasource. The object that each @S3Uris@ points to must be readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
mkTransformS3DataSource ::
  -- | 's3DataType'
  S3DataType ->
  -- | 's3URI'
  Lude.Text ->
  TransformS3DataSource
mkTransformS3DataSource pS3DataType_ pS3URI_ =
  TransformS3DataSource'
    { s3DataType = pS3DataType_,
      s3URI = pS3URI_
    }

-- | If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for batch transform.
--
-- If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for batch transform.
-- The following values are compatible: @ManifestFile@ , @S3Prefix@
-- The following value is not compatible: @AugmentedManifestFile@
--
-- /Note:/ Consider using 's3DataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsdsS3DataType :: Lens.Lens' TransformS3DataSource S3DataType
tsdsS3DataType = Lens.lens (s3DataType :: TransformS3DataSource -> S3DataType) (\s a -> s {s3DataType = a} :: TransformS3DataSource)
{-# DEPRECATED tsdsS3DataType "Use generic-lens or generic-optics with 's3DataType' instead." #-}

-- | Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:
--
--
--     * A key name prefix might look like this: @s3://bucketname/exampleprefix@ .
--
--
--     * A manifest might look like this: @s3://bucketname/example.manifest@
-- The manifest is an S3 object which is a JSON file with the following format:
-- @[ {"prefix": "s3://customer_bucket/some/prefix/"},@
-- @"relative/path/to/custdata-1",@
-- @"relative/path/custdata-2",@
-- @...@
-- @"relative/path/custdata-N"@
-- @]@
-- The preceding JSON matches the following @S3Uris@ :
-- @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@
-- @s3://customer_bucket/some/prefix/relative/path/custdata-2@
-- @...@
-- @s3://customer_bucket/some/prefix/relative/path/custdata-N@
-- The complete set of @S3Uris@ in this manifest constitutes the input data for the channel for this datasource. The object that each @S3Uris@ points to must be readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
--
--
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsdsS3URI :: Lens.Lens' TransformS3DataSource Lude.Text
tsdsS3URI = Lens.lens (s3URI :: TransformS3DataSource -> Lude.Text) (\s a -> s {s3URI = a} :: TransformS3DataSource)
{-# DEPRECATED tsdsS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON TransformS3DataSource where
  parseJSON =
    Lude.withObject
      "TransformS3DataSource"
      ( \x ->
          TransformS3DataSource'
            Lude.<$> (x Lude..: "S3DataType") Lude.<*> (x Lude..: "S3Uri")
      )

instance Lude.ToJSON TransformS3DataSource where
  toJSON TransformS3DataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3DataType" Lude..= s3DataType),
            Lude.Just ("S3Uri" Lude..= s3URI)
          ]
      )
