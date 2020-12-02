{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.S3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.S3DataSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.S3DataDistribution
import Network.AWS.SageMaker.Types.S3DataType

-- | Describes the S3 data source.
--
--
--
-- /See:/ 's3DataSource' smart constructor.
data S3DataSource = S3DataSource'
  { _sdsS3DataDistributionType ::
      !(Maybe S3DataDistribution),
    _sdsAttributeNames :: !(Maybe [Text]),
    _sdsS3DataType :: !S3DataType,
    _sdsS3URI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsS3DataDistributionType' - If you want Amazon SageMaker to replicate the entire dataset on each ML compute instance that is launched for model training, specify @FullyReplicated@ .  If you want Amazon SageMaker to replicate a subset of data on each ML compute instance that is launched for model training, specify @ShardedByS3Key@ . If there are /n/ ML compute instances launched for a training job, each instance gets approximately 1//n/ of the number of S3 objects. In this case, model training on each machine uses only the subset of training data.  Don't choose more ML compute instances for training than available S3 objects. If you do, some nodes won't get any data and you will pay for nodes that aren't getting any training data. This applies in both File and Pipe modes. Keep this in mind when developing algorithms.  In distributed training, where you use multiple ML compute EC2 instances, you might choose @ShardedByS3Key@ . If the algorithm requires copying training data to the ML storage volume (when @TrainingInputMode@ is set to @File@ ), this copies 1//n/ of the number of objects.
--
-- * 'sdsAttributeNames' - A list of one or more attribute names to use that are found in a specified augmented manifest file.
--
-- * 'sdsS3DataType' - If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects that match the specified key name prefix for model training.  If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for model training.  If you choose @AugmentedManifestFile@ , S3Uri identifies an object that is an augmented manifest file in JSON lines format. This file contains the data you want to use for model training. @AugmentedManifestFile@ can only be used if the Channel's input mode is @Pipe@ .
--
-- * 'sdsS3URI' - Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:      * A key name prefix might look like this: @s3://bucketname/exampleprefix@      * A manifest might look like this: @s3://bucketname/example.manifest@  A manifest is an S3 object which is a JSON file consisting of an array of elements. The first element is a prefix which is followed by one or more suffixes. SageMaker appends the suffix elements to the prefix to get a full set of @S3Uri@ . Note that the prefix must be a valid non-empty @S3Uri@ that precludes users from specifying a manifest whose individual @S3Uri@ is sourced from different S3 buckets. The following code example shows a valid manifest format:  @[ {"prefix": "s3://customer_bucket/some/prefix/"},@  @"relative/path/to/custdata-1",@  @"relative/path/custdata-2",@  @...@  @"relative/path/custdata-N"@  @]@  This JSON is equivalent to the following @S3Uri@ list: @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@  @s3://customer_bucket/some/prefix/relative/path/custdata-2@  @...@  @s3://customer_bucket/some/prefix/relative/path/custdata-N@  The complete set of @S3Uri@ in this manifest is the input data for the channel for this data source. The object that each @S3Uri@ points to must be readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
s3DataSource ::
  -- | 'sdsS3DataType'
  S3DataType ->
  -- | 'sdsS3URI'
  Text ->
  S3DataSource
s3DataSource pS3DataType_ pS3URI_ =
  S3DataSource'
    { _sdsS3DataDistributionType = Nothing,
      _sdsAttributeNames = Nothing,
      _sdsS3DataType = pS3DataType_,
      _sdsS3URI = pS3URI_
    }

-- | If you want Amazon SageMaker to replicate the entire dataset on each ML compute instance that is launched for model training, specify @FullyReplicated@ .  If you want Amazon SageMaker to replicate a subset of data on each ML compute instance that is launched for model training, specify @ShardedByS3Key@ . If there are /n/ ML compute instances launched for a training job, each instance gets approximately 1//n/ of the number of S3 objects. In this case, model training on each machine uses only the subset of training data.  Don't choose more ML compute instances for training than available S3 objects. If you do, some nodes won't get any data and you will pay for nodes that aren't getting any training data. This applies in both File and Pipe modes. Keep this in mind when developing algorithms.  In distributed training, where you use multiple ML compute EC2 instances, you might choose @ShardedByS3Key@ . If the algorithm requires copying training data to the ML storage volume (when @TrainingInputMode@ is set to @File@ ), this copies 1//n/ of the number of objects.
sdsS3DataDistributionType :: Lens' S3DataSource (Maybe S3DataDistribution)
sdsS3DataDistributionType = lens _sdsS3DataDistributionType (\s a -> s {_sdsS3DataDistributionType = a})

-- | A list of one or more attribute names to use that are found in a specified augmented manifest file.
sdsAttributeNames :: Lens' S3DataSource [Text]
sdsAttributeNames = lens _sdsAttributeNames (\s a -> s {_sdsAttributeNames = a}) . _Default . _Coerce

-- | If you choose @S3Prefix@ , @S3Uri@ identifies a key name prefix. Amazon SageMaker uses all objects that match the specified key name prefix for model training.  If you choose @ManifestFile@ , @S3Uri@ identifies an object that is a manifest file containing a list of object keys that you want Amazon SageMaker to use for model training.  If you choose @AugmentedManifestFile@ , S3Uri identifies an object that is an augmented manifest file in JSON lines format. This file contains the data you want to use for model training. @AugmentedManifestFile@ can only be used if the Channel's input mode is @Pipe@ .
sdsS3DataType :: Lens' S3DataSource S3DataType
sdsS3DataType = lens _sdsS3DataType (\s a -> s {_sdsS3DataType = a})

-- | Depending on the value specified for the @S3DataType@ , identifies either a key name prefix or a manifest. For example:      * A key name prefix might look like this: @s3://bucketname/exampleprefix@      * A manifest might look like this: @s3://bucketname/example.manifest@  A manifest is an S3 object which is a JSON file consisting of an array of elements. The first element is a prefix which is followed by one or more suffixes. SageMaker appends the suffix elements to the prefix to get a full set of @S3Uri@ . Note that the prefix must be a valid non-empty @S3Uri@ that precludes users from specifying a manifest whose individual @S3Uri@ is sourced from different S3 buckets. The following code example shows a valid manifest format:  @[ {"prefix": "s3://customer_bucket/some/prefix/"},@  @"relative/path/to/custdata-1",@  @"relative/path/custdata-2",@  @...@  @"relative/path/custdata-N"@  @]@  This JSON is equivalent to the following @S3Uri@ list: @s3://customer_bucket/some/prefix/relative/path/to/custdata-1@  @s3://customer_bucket/some/prefix/relative/path/custdata-2@  @...@  @s3://customer_bucket/some/prefix/relative/path/custdata-N@  The complete set of @S3Uri@ in this manifest is the input data for the channel for this data source. The object that each @S3Uri@ points to must be readable by the IAM role that Amazon SageMaker uses to perform tasks on your behalf.
sdsS3URI :: Lens' S3DataSource Text
sdsS3URI = lens _sdsS3URI (\s a -> s {_sdsS3URI = a})

instance FromJSON S3DataSource where
  parseJSON =
    withObject
      "S3DataSource"
      ( \x ->
          S3DataSource'
            <$> (x .:? "S3DataDistributionType")
            <*> (x .:? "AttributeNames" .!= mempty)
            <*> (x .: "S3DataType")
            <*> (x .: "S3Uri")
      )

instance Hashable S3DataSource

instance NFData S3DataSource

instance ToJSON S3DataSource where
  toJSON S3DataSource' {..} =
    object
      ( catMaybes
          [ ("S3DataDistributionType" .=) <$> _sdsS3DataDistributionType,
            ("AttributeNames" .=) <$> _sdsAttributeNames,
            Just ("S3DataType" .= _sdsS3DataType),
            Just ("S3Uri" .= _sdsS3URI)
          ]
      )
