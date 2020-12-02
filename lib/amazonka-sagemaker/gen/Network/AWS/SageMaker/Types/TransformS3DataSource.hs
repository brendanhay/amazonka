{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformS3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformS3DataSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.S3DataType

-- | Describes the S3 data source.
--
--
--
-- /See:/ 'transformS3DataSource' smart constructor.
data TransformS3DataSource = TransformS3DataSource'
  { _tsdsS3DataType ::
      !S3DataType,
    _tsdsS3URI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformS3DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsdsS3DataType' - If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for batch transform.  If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for batch transform.  The following values are compatible: @ManifestFile@ , @S3Prefix@  The following value is not compatible: @AugmentedManifestFile@
--
-- * 'tsdsS3URI' - Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:     * A key name prefix might look like this: @s3://bucketname/exampleprefix@ .      * A manifest might look like this: @s3://bucketname/example.manifest@  The manifest is an S3 object which is a JSON file with the following format:  @[ {"prefix": "s3://customer_bucket/some/prefix/"},@  @"relative/path/to/custdata-1",@  @"relative/path/custdata-2",@  @...@  @"relative/path/custdata-N"@  @]@  The preceding JSON matches the following @S3Uris@ :  @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@  @s3://customer_bucket/some/prefix/relative/path/custdata-2@  @...@  @s3://customer_bucket/some/prefix/relative/path/custdata-N@  The complete set of @S3Uris@ in this manifest constitutes the input data for the channel for this datasource. The object that each @S3Uris@ points to must be readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
transformS3DataSource ::
  -- | 'tsdsS3DataType'
  S3DataType ->
  -- | 'tsdsS3URI'
  Text ->
  TransformS3DataSource
transformS3DataSource pS3DataType_ pS3URI_ =
  TransformS3DataSource'
    { _tsdsS3DataType = pS3DataType_,
      _tsdsS3URI = pS3URI_
    }

-- | If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects with the specified key name prefix for batch transform.  If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for batch transform.  The following values are compatible: @ManifestFile@ , @S3Prefix@  The following value is not compatible: @AugmentedManifestFile@
tsdsS3DataType :: Lens' TransformS3DataSource S3DataType
tsdsS3DataType = lens _tsdsS3DataType (\s a -> s {_tsdsS3DataType = a})

-- | Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:     * A key name prefix might look like this: @s3://bucketname/exampleprefix@ .      * A manifest might look like this: @s3://bucketname/example.manifest@  The manifest is an S3 object which is a JSON file with the following format:  @[ {"prefix": "s3://customer_bucket/some/prefix/"},@  @"relative/path/to/custdata-1",@  @"relative/path/custdata-2",@  @...@  @"relative/path/custdata-N"@  @]@  The preceding JSON matches the following @S3Uris@ :  @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@  @s3://customer_bucket/some/prefix/relative/path/custdata-2@  @...@  @s3://customer_bucket/some/prefix/relative/path/custdata-N@  The complete set of @S3Uris@ in this manifest constitutes the input data for the channel for this datasource. The object that each @S3Uris@ points to must be readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
tsdsS3URI :: Lens' TransformS3DataSource Text
tsdsS3URI = lens _tsdsS3URI (\s a -> s {_tsdsS3URI = a})

instance FromJSON TransformS3DataSource where
  parseJSON =
    withObject
      "TransformS3DataSource"
      ( \x ->
          TransformS3DataSource' <$> (x .: "S3DataType") <*> (x .: "S3Uri")
      )

instance Hashable TransformS3DataSource

instance NFData TransformS3DataSource

instance ToJSON TransformS3DataSource where
  toJSON TransformS3DataSource' {..} =
    object
      ( catMaybes
          [ Just ("S3DataType" .= _tsdsS3DataType),
            Just ("S3Uri" .= _tsdsS3URI)
          ]
      )
