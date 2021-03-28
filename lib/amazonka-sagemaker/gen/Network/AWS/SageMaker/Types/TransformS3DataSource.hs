{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformS3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TransformS3DataSource
  ( TransformS3DataSource (..)
  -- * Smart constructor
  , mkTransformS3DataSource
  -- * Lenses
  , tsdsS3DataType
  , tsdsS3Uri
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.S3DataType as Types
import qualified Network.AWS.SageMaker.Types.S3Uri as Types

-- | Describes the S3 data source.
--
-- /See:/ 'mkTransformS3DataSource' smart constructor.
data TransformS3DataSource = TransformS3DataSource'
  { s3DataType :: Types.S3DataType
    -- ^ If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for batch transform. 
--
-- If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for batch transform. 
-- The following values are compatible: @ManifestFile@ , @S3Prefix@ 
-- The following value is not compatible: @AugmentedManifestFile@ 
  , s3Uri :: Types.S3Uri
    -- ^ Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransformS3DataSource' value with any optional fields omitted.
mkTransformS3DataSource
    :: Types.S3DataType -- ^ 's3DataType'
    -> Types.S3Uri -- ^ 's3Uri'
    -> TransformS3DataSource
mkTransformS3DataSource s3DataType s3Uri
  = TransformS3DataSource'{s3DataType, s3Uri}

-- | If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for batch transform. 
--
-- If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for batch transform. 
-- The following values are compatible: @ManifestFile@ , @S3Prefix@ 
-- The following value is not compatible: @AugmentedManifestFile@ 
--
-- /Note:/ Consider using 's3DataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsdsS3DataType :: Lens.Lens' TransformS3DataSource Types.S3DataType
tsdsS3DataType = Lens.field @"s3DataType"
{-# INLINEABLE tsdsS3DataType #-}
{-# DEPRECATED s3DataType "Use generic-lens or generic-optics with 's3DataType' instead"  #-}

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
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsdsS3Uri :: Lens.Lens' TransformS3DataSource Types.S3Uri
tsdsS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE tsdsS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

instance Core.FromJSON TransformS3DataSource where
        toJSON TransformS3DataSource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("S3DataType" Core..= s3DataType),
                  Core.Just ("S3Uri" Core..= s3Uri)])

instance Core.FromJSON TransformS3DataSource where
        parseJSON
          = Core.withObject "TransformS3DataSource" Core.$
              \ x ->
                TransformS3DataSource' Core.<$>
                  (x Core..: "S3DataType") Core.<*> x Core..: "S3Uri"
