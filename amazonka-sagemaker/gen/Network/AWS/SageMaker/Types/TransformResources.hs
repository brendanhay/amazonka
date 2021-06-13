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
-- Module      : Network.AWS.SageMaker.Types.TransformResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformResources where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TransformInstanceType

-- | Describes the resources, including ML instance types and ML instance
-- count, to use for transform job.
--
-- /See:/ 'newTransformResources' smart constructor.
data TransformResources = TransformResources'
  { -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt model data on the storage volume attached to the ML compute
    -- instance(s) that run the batch transform job. The @VolumeKmsKeyId@ can
    -- be any of the following formats:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias name ARN:
    --     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The ML compute instance type for the transform job. If you are using
    -- built-in algorithms to transform moderately sized datasets, we recommend
    -- using ml.m4.xlarge or @ml.m5.large@ instance types.
    instanceType :: TransformInstanceType,
    -- | The number of ML compute instances to use in the transform job. For
    -- distributed transform jobs, specify a value greater than 1. The default
    -- value is @1@.
    instanceCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeKmsKeyId', 'transformResources_volumeKmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt model data on the storage volume attached to the ML compute
-- instance(s) that run the batch transform job. The @VolumeKmsKeyId@ can
-- be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
--
-- 'instanceType', 'transformResources_instanceType' - The ML compute instance type for the transform job. If you are using
-- built-in algorithms to transform moderately sized datasets, we recommend
-- using ml.m4.xlarge or @ml.m5.large@ instance types.
--
-- 'instanceCount', 'transformResources_instanceCount' - The number of ML compute instances to use in the transform job. For
-- distributed transform jobs, specify a value greater than 1. The default
-- value is @1@.
newTransformResources ::
  -- | 'instanceType'
  TransformInstanceType ->
  -- | 'instanceCount'
  Prelude.Natural ->
  TransformResources
newTransformResources pInstanceType_ pInstanceCount_ =
  TransformResources'
    { volumeKmsKeyId =
        Prelude.Nothing,
      instanceType = pInstanceType_,
      instanceCount = pInstanceCount_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt model data on the storage volume attached to the ML compute
-- instance(s) that run the batch transform job. The @VolumeKmsKeyId@ can
-- be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
transformResources_volumeKmsKeyId :: Lens.Lens' TransformResources (Prelude.Maybe Prelude.Text)
transformResources_volumeKmsKeyId = Lens.lens (\TransformResources' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@TransformResources' {} a -> s {volumeKmsKeyId = a} :: TransformResources)

-- | The ML compute instance type for the transform job. If you are using
-- built-in algorithms to transform moderately sized datasets, we recommend
-- using ml.m4.xlarge or @ml.m5.large@ instance types.
transformResources_instanceType :: Lens.Lens' TransformResources TransformInstanceType
transformResources_instanceType = Lens.lens (\TransformResources' {instanceType} -> instanceType) (\s@TransformResources' {} a -> s {instanceType = a} :: TransformResources)

-- | The number of ML compute instances to use in the transform job. For
-- distributed transform jobs, specify a value greater than 1. The default
-- value is @1@.
transformResources_instanceCount :: Lens.Lens' TransformResources Prelude.Natural
transformResources_instanceCount = Lens.lens (\TransformResources' {instanceCount} -> instanceCount) (\s@TransformResources' {} a -> s {instanceCount = a} :: TransformResources)

instance Core.FromJSON TransformResources where
  parseJSON =
    Core.withObject
      "TransformResources"
      ( \x ->
          TransformResources'
            Prelude.<$> (x Core..:? "VolumeKmsKeyId")
            Prelude.<*> (x Core..: "InstanceType")
            Prelude.<*> (x Core..: "InstanceCount")
      )

instance Prelude.Hashable TransformResources

instance Prelude.NFData TransformResources

instance Core.ToJSON TransformResources where
  toJSON TransformResources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            Prelude.Just ("InstanceType" Core..= instanceType),
            Prelude.Just
              ("InstanceCount" Core..= instanceCount)
          ]
      )
