{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.S3DataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.S3DataSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.S3DataDistribution
import Network.AWS.SageMaker.Types.S3DataType

-- | Describes the S3 data source.
--
-- /See:/ 'newS3DataSource' smart constructor.
data S3DataSource = S3DataSource'
  { -- | If you want Amazon SageMaker to replicate the entire dataset on each ML
    -- compute instance that is launched for model training, specify
    -- @FullyReplicated@.
    --
    -- If you want Amazon SageMaker to replicate a subset of data on each ML
    -- compute instance that is launched for model training, specify
    -- @ShardedByS3Key@. If there are /n/ ML compute instances launched for a
    -- training job, each instance gets approximately 1\//n/ of the number of
    -- S3 objects. In this case, model training on each machine uses only the
    -- subset of training data.
    --
    -- Don\'t choose more ML compute instances for training than available S3
    -- objects. If you do, some nodes won\'t get any data and you will pay for
    -- nodes that aren\'t getting any training data. This applies in both File
    -- and Pipe modes. Keep this in mind when developing algorithms.
    --
    -- In distributed training, where you use multiple ML compute EC2
    -- instances, you might choose @ShardedByS3Key@. If the algorithm requires
    -- copying training data to the ML storage volume (when @TrainingInputMode@
    -- is set to @File@), this copies 1\//n/ of the number of objects.
    s3DataDistributionType :: Prelude.Maybe S3DataDistribution,
    -- | A list of one or more attribute names to use that are found in a
    -- specified augmented manifest file.
    attributeNames :: Prelude.Maybe [Prelude.Text],
    -- | If you choose @S3Prefix@, @S3Uri@ identifies a key name prefix. Amazon
    -- SageMaker uses all objects that match the specified key name prefix for
    -- model training.
    --
    -- If you choose @ManifestFile@, @S3Uri@ identifies an object that is a
    -- manifest file containing a list of object keys that you want Amazon
    -- SageMaker to use for model training.
    --
    -- If you choose @AugmentedManifestFile@, S3Uri identifies an object that
    -- is an augmented manifest file in JSON lines format. This file contains
    -- the data you want to use for model training. @AugmentedManifestFile@ can
    -- only be used if the Channel\'s input mode is @Pipe@.
    s3DataType :: S3DataType,
    -- | Depending on the value specified for the @S3DataType@, identifies either
    -- a key name prefix or a manifest. For example:
    --
    -- -   A key name prefix might look like this:
    --     @s3:\/\/bucketname\/exampleprefix@
    --
    -- -   A manifest might look like this:
    --     @s3:\/\/bucketname\/example.manifest@
    --
    --     A manifest is an S3 object which is a JSON file consisting of an
    --     array of elements. The first element is a prefix which is followed
    --     by one or more suffixes. SageMaker appends the suffix elements to
    --     the prefix to get a full set of @S3Uri@. Note that the prefix must
    --     be a valid non-empty @S3Uri@ that precludes users from specifying a
    --     manifest whose individual @S3Uri@ is sourced from different S3
    --     buckets.
    --
    --     The following code example shows a valid manifest format:
    --
    --     @[ {\"prefix\": \"s3:\/\/customer_bucket\/some\/prefix\/\"},@
    --
    --     @ \"relative\/path\/to\/custdata-1\",@
    --
    --     @ \"relative\/path\/custdata-2\",@
    --
    --     @ ...@
    --
    --     @ \"relative\/path\/custdata-N\"@
    --
    --     @]@
    --
    --     This JSON is equivalent to the following @S3Uri@ list:
    --
    --     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/to\/custdata-1@
    --
    --     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-2@
    --
    --     @...@
    --
    --     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-N@
    --
    --     The complete set of @S3Uri@ in this manifest is the input data for
    --     the channel for this data source. The object that each @S3Uri@
    --     points to must be readable by the IAM role that Amazon SageMaker
    --     uses to perform tasks on your behalf.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DataDistributionType', 's3DataSource_s3DataDistributionType' - If you want Amazon SageMaker to replicate the entire dataset on each ML
-- compute instance that is launched for model training, specify
-- @FullyReplicated@.
--
-- If you want Amazon SageMaker to replicate a subset of data on each ML
-- compute instance that is launched for model training, specify
-- @ShardedByS3Key@. If there are /n/ ML compute instances launched for a
-- training job, each instance gets approximately 1\//n/ of the number of
-- S3 objects. In this case, model training on each machine uses only the
-- subset of training data.
--
-- Don\'t choose more ML compute instances for training than available S3
-- objects. If you do, some nodes won\'t get any data and you will pay for
-- nodes that aren\'t getting any training data. This applies in both File
-- and Pipe modes. Keep this in mind when developing algorithms.
--
-- In distributed training, where you use multiple ML compute EC2
-- instances, you might choose @ShardedByS3Key@. If the algorithm requires
-- copying training data to the ML storage volume (when @TrainingInputMode@
-- is set to @File@), this copies 1\//n/ of the number of objects.
--
-- 'attributeNames', 's3DataSource_attributeNames' - A list of one or more attribute names to use that are found in a
-- specified augmented manifest file.
--
-- 's3DataType', 's3DataSource_s3DataType' - If you choose @S3Prefix@, @S3Uri@ identifies a key name prefix. Amazon
-- SageMaker uses all objects that match the specified key name prefix for
-- model training.
--
-- If you choose @ManifestFile@, @S3Uri@ identifies an object that is a
-- manifest file containing a list of object keys that you want Amazon
-- SageMaker to use for model training.
--
-- If you choose @AugmentedManifestFile@, S3Uri identifies an object that
-- is an augmented manifest file in JSON lines format. This file contains
-- the data you want to use for model training. @AugmentedManifestFile@ can
-- only be used if the Channel\'s input mode is @Pipe@.
--
-- 's3Uri', 's3DataSource_s3Uri' - Depending on the value specified for the @S3DataType@, identifies either
-- a key name prefix or a manifest. For example:
--
-- -   A key name prefix might look like this:
--     @s3:\/\/bucketname\/exampleprefix@
--
-- -   A manifest might look like this:
--     @s3:\/\/bucketname\/example.manifest@
--
--     A manifest is an S3 object which is a JSON file consisting of an
--     array of elements. The first element is a prefix which is followed
--     by one or more suffixes. SageMaker appends the suffix elements to
--     the prefix to get a full set of @S3Uri@. Note that the prefix must
--     be a valid non-empty @S3Uri@ that precludes users from specifying a
--     manifest whose individual @S3Uri@ is sourced from different S3
--     buckets.
--
--     The following code example shows a valid manifest format:
--
--     @[ {\"prefix\": \"s3:\/\/customer_bucket\/some\/prefix\/\"},@
--
--     @ \"relative\/path\/to\/custdata-1\",@
--
--     @ \"relative\/path\/custdata-2\",@
--
--     @ ...@
--
--     @ \"relative\/path\/custdata-N\"@
--
--     @]@
--
--     This JSON is equivalent to the following @S3Uri@ list:
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/to\/custdata-1@
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-2@
--
--     @...@
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-N@
--
--     The complete set of @S3Uri@ in this manifest is the input data for
--     the channel for this data source. The object that each @S3Uri@
--     points to must be readable by the IAM role that Amazon SageMaker
--     uses to perform tasks on your behalf.
newS3DataSource ::
  -- | 's3DataType'
  S3DataType ->
  -- | 's3Uri'
  Prelude.Text ->
  S3DataSource
newS3DataSource pS3DataType_ pS3Uri_ =
  S3DataSource'
    { s3DataDistributionType =
        Prelude.Nothing,
      attributeNames = Prelude.Nothing,
      s3DataType = pS3DataType_,
      s3Uri = pS3Uri_
    }

-- | If you want Amazon SageMaker to replicate the entire dataset on each ML
-- compute instance that is launched for model training, specify
-- @FullyReplicated@.
--
-- If you want Amazon SageMaker to replicate a subset of data on each ML
-- compute instance that is launched for model training, specify
-- @ShardedByS3Key@. If there are /n/ ML compute instances launched for a
-- training job, each instance gets approximately 1\//n/ of the number of
-- S3 objects. In this case, model training on each machine uses only the
-- subset of training data.
--
-- Don\'t choose more ML compute instances for training than available S3
-- objects. If you do, some nodes won\'t get any data and you will pay for
-- nodes that aren\'t getting any training data. This applies in both File
-- and Pipe modes. Keep this in mind when developing algorithms.
--
-- In distributed training, where you use multiple ML compute EC2
-- instances, you might choose @ShardedByS3Key@. If the algorithm requires
-- copying training data to the ML storage volume (when @TrainingInputMode@
-- is set to @File@), this copies 1\//n/ of the number of objects.
s3DataSource_s3DataDistributionType :: Lens.Lens' S3DataSource (Prelude.Maybe S3DataDistribution)
s3DataSource_s3DataDistributionType = Lens.lens (\S3DataSource' {s3DataDistributionType} -> s3DataDistributionType) (\s@S3DataSource' {} a -> s {s3DataDistributionType = a} :: S3DataSource)

-- | A list of one or more attribute names to use that are found in a
-- specified augmented manifest file.
s3DataSource_attributeNames :: Lens.Lens' S3DataSource (Prelude.Maybe [Prelude.Text])
s3DataSource_attributeNames = Lens.lens (\S3DataSource' {attributeNames} -> attributeNames) (\s@S3DataSource' {} a -> s {attributeNames = a} :: S3DataSource) Prelude.. Lens.mapping Prelude._Coerce

-- | If you choose @S3Prefix@, @S3Uri@ identifies a key name prefix. Amazon
-- SageMaker uses all objects that match the specified key name prefix for
-- model training.
--
-- If you choose @ManifestFile@, @S3Uri@ identifies an object that is a
-- manifest file containing a list of object keys that you want Amazon
-- SageMaker to use for model training.
--
-- If you choose @AugmentedManifestFile@, S3Uri identifies an object that
-- is an augmented manifest file in JSON lines format. This file contains
-- the data you want to use for model training. @AugmentedManifestFile@ can
-- only be used if the Channel\'s input mode is @Pipe@.
s3DataSource_s3DataType :: Lens.Lens' S3DataSource S3DataType
s3DataSource_s3DataType = Lens.lens (\S3DataSource' {s3DataType} -> s3DataType) (\s@S3DataSource' {} a -> s {s3DataType = a} :: S3DataSource)

-- | Depending on the value specified for the @S3DataType@, identifies either
-- a key name prefix or a manifest. For example:
--
-- -   A key name prefix might look like this:
--     @s3:\/\/bucketname\/exampleprefix@
--
-- -   A manifest might look like this:
--     @s3:\/\/bucketname\/example.manifest@
--
--     A manifest is an S3 object which is a JSON file consisting of an
--     array of elements. The first element is a prefix which is followed
--     by one or more suffixes. SageMaker appends the suffix elements to
--     the prefix to get a full set of @S3Uri@. Note that the prefix must
--     be a valid non-empty @S3Uri@ that precludes users from specifying a
--     manifest whose individual @S3Uri@ is sourced from different S3
--     buckets.
--
--     The following code example shows a valid manifest format:
--
--     @[ {\"prefix\": \"s3:\/\/customer_bucket\/some\/prefix\/\"},@
--
--     @ \"relative\/path\/to\/custdata-1\",@
--
--     @ \"relative\/path\/custdata-2\",@
--
--     @ ...@
--
--     @ \"relative\/path\/custdata-N\"@
--
--     @]@
--
--     This JSON is equivalent to the following @S3Uri@ list:
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/to\/custdata-1@
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-2@
--
--     @...@
--
--     @s3:\/\/customer_bucket\/some\/prefix\/relative\/path\/custdata-N@
--
--     The complete set of @S3Uri@ in this manifest is the input data for
--     the channel for this data source. The object that each @S3Uri@
--     points to must be readable by the IAM role that Amazon SageMaker
--     uses to perform tasks on your behalf.
s3DataSource_s3Uri :: Lens.Lens' S3DataSource Prelude.Text
s3DataSource_s3Uri = Lens.lens (\S3DataSource' {s3Uri} -> s3Uri) (\s@S3DataSource' {} a -> s {s3Uri = a} :: S3DataSource)

instance Prelude.FromJSON S3DataSource where
  parseJSON =
    Prelude.withObject
      "S3DataSource"
      ( \x ->
          S3DataSource'
            Prelude.<$> (x Prelude..:? "S3DataDistributionType")
            Prelude.<*> ( x Prelude..:? "AttributeNames"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "S3DataType")
            Prelude.<*> (x Prelude..: "S3Uri")
      )

instance Prelude.Hashable S3DataSource

instance Prelude.NFData S3DataSource

instance Prelude.ToJSON S3DataSource where
  toJSON S3DataSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("S3DataDistributionType" Prelude..=)
              Prelude.<$> s3DataDistributionType,
            ("AttributeNames" Prelude..=)
              Prelude.<$> attributeNames,
            Prelude.Just ("S3DataType" Prelude..= s3DataType),
            Prelude.Just ("S3Uri" Prelude..= s3Uri)
          ]
      )
