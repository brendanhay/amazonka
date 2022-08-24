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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobInputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobInputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointInputConfiguration
import Amazonka.SageMaker.Types.RecommendationJobResourceLimit
import Amazonka.SageMaker.Types.TrafficPattern

-- | The input configuration of the recommendation job.
--
-- /See:/ 'newRecommendationJobInputConfig' smart constructor.
data RecommendationJobInputConfig = RecommendationJobInputConfig'
  { -- | Specifies the traffic pattern of the job.
    trafficPattern :: Prelude.Maybe TrafficPattern,
    -- | Specifies the maximum duration of the job, in seconds.>
    jobDurationInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
    -- Service (Amazon Web Services KMS) key that Amazon SageMaker uses to
    -- encrypt data on the storage volume attached to the ML compute instance
    -- that hosts the endpoint. This key will be passed to SageMaker Hosting
    -- for endpoint creation.
    --
    -- The SageMaker execution role must have @kms:CreateGrant@ permission in
    -- order to encrypt data on the storage volume of the endpoints created for
    -- inference recommendation. The inference recommendation job will fail
    -- asynchronously during endpoint configuration creation if the role passed
    -- does not have @kms:CreateGrant@ permission.
    --
    -- The @KmsKeyId@ can be any of the following formats:
    --
    -- -   \/\/ KMS Key ID
    --
    --     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
    --
    --     @\"arn:aws:kms:\<region>:\<account>:key\/\<key-id-12ab-34cd-56ef-1234567890ab>\"@
    --
    -- -   \/\/ KMS Key Alias
    --
    --     @\"alias\/ExampleAlias\"@
    --
    -- -   \/\/ Amazon Resource Name (ARN) of a KMS Key Alias
    --
    --     @\"arn:aws:kms:\<region>:\<account>:alias\/\<ExampleAlias>\"@
    --
    -- For more information about key identifiers, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id Key identifiers (KeyID)>
    -- in the Amazon Web Services Key Management Service (Amazon Web Services
    -- KMS) documentation.
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the endpoint configuration to use for a job.
    endpointConfigurations :: Prelude.Maybe (Prelude.NonEmpty EndpointInputConfiguration),
    -- | Defines the resource limit of the job.
    resourceLimit :: Prelude.Maybe RecommendationJobResourceLimit,
    -- | The Amazon Resource Name (ARN) of a versioned model package.
    modelPackageVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationJobInputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficPattern', 'recommendationJobInputConfig_trafficPattern' - Specifies the traffic pattern of the job.
--
-- 'jobDurationInSeconds', 'recommendationJobInputConfig_jobDurationInSeconds' - Specifies the maximum duration of the job, in seconds.>
--
-- 'volumeKmsKeyId', 'recommendationJobInputConfig_volumeKmsKeyId' - The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) key that Amazon SageMaker uses to
-- encrypt data on the storage volume attached to the ML compute instance
-- that hosts the endpoint. This key will be passed to SageMaker Hosting
-- for endpoint creation.
--
-- The SageMaker execution role must have @kms:CreateGrant@ permission in
-- order to encrypt data on the storage volume of the endpoints created for
-- inference recommendation. The inference recommendation job will fail
-- asynchronously during endpoint configuration creation if the role passed
-- does not have @kms:CreateGrant@ permission.
--
-- The @KmsKeyId@ can be any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:\<region>:\<account>:key\/\<key-id-12ab-34cd-56ef-1234567890ab>\"@
--
-- -   \/\/ KMS Key Alias
--
--     @\"alias\/ExampleAlias\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key Alias
--
--     @\"arn:aws:kms:\<region>:\<account>:alias\/\<ExampleAlias>\"@
--
-- For more information about key identifiers, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id Key identifiers (KeyID)>
-- in the Amazon Web Services Key Management Service (Amazon Web Services
-- KMS) documentation.
--
-- 'endpointConfigurations', 'recommendationJobInputConfig_endpointConfigurations' - Specifies the endpoint configuration to use for a job.
--
-- 'resourceLimit', 'recommendationJobInputConfig_resourceLimit' - Defines the resource limit of the job.
--
-- 'modelPackageVersionArn', 'recommendationJobInputConfig_modelPackageVersionArn' - The Amazon Resource Name (ARN) of a versioned model package.
newRecommendationJobInputConfig ::
  -- | 'modelPackageVersionArn'
  Prelude.Text ->
  RecommendationJobInputConfig
newRecommendationJobInputConfig
  pModelPackageVersionArn_ =
    RecommendationJobInputConfig'
      { trafficPattern =
          Prelude.Nothing,
        jobDurationInSeconds = Prelude.Nothing,
        volumeKmsKeyId = Prelude.Nothing,
        endpointConfigurations = Prelude.Nothing,
        resourceLimit = Prelude.Nothing,
        modelPackageVersionArn =
          pModelPackageVersionArn_
      }

-- | Specifies the traffic pattern of the job.
recommendationJobInputConfig_trafficPattern :: Lens.Lens' RecommendationJobInputConfig (Prelude.Maybe TrafficPattern)
recommendationJobInputConfig_trafficPattern = Lens.lens (\RecommendationJobInputConfig' {trafficPattern} -> trafficPattern) (\s@RecommendationJobInputConfig' {} a -> s {trafficPattern = a} :: RecommendationJobInputConfig)

-- | Specifies the maximum duration of the job, in seconds.>
recommendationJobInputConfig_jobDurationInSeconds :: Lens.Lens' RecommendationJobInputConfig (Prelude.Maybe Prelude.Natural)
recommendationJobInputConfig_jobDurationInSeconds = Lens.lens (\RecommendationJobInputConfig' {jobDurationInSeconds} -> jobDurationInSeconds) (\s@RecommendationJobInputConfig' {} a -> s {jobDurationInSeconds = a} :: RecommendationJobInputConfig)

-- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service (Amazon Web Services KMS) key that Amazon SageMaker uses to
-- encrypt data on the storage volume attached to the ML compute instance
-- that hosts the endpoint. This key will be passed to SageMaker Hosting
-- for endpoint creation.
--
-- The SageMaker execution role must have @kms:CreateGrant@ permission in
-- order to encrypt data on the storage volume of the endpoints created for
-- inference recommendation. The inference recommendation job will fail
-- asynchronously during endpoint configuration creation if the role passed
-- does not have @kms:CreateGrant@ permission.
--
-- The @KmsKeyId@ can be any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:\<region>:\<account>:key\/\<key-id-12ab-34cd-56ef-1234567890ab>\"@
--
-- -   \/\/ KMS Key Alias
--
--     @\"alias\/ExampleAlias\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key Alias
--
--     @\"arn:aws:kms:\<region>:\<account>:alias\/\<ExampleAlias>\"@
--
-- For more information about key identifiers, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#key-id-key-id Key identifiers (KeyID)>
-- in the Amazon Web Services Key Management Service (Amazon Web Services
-- KMS) documentation.
recommendationJobInputConfig_volumeKmsKeyId :: Lens.Lens' RecommendationJobInputConfig (Prelude.Maybe Prelude.Text)
recommendationJobInputConfig_volumeKmsKeyId = Lens.lens (\RecommendationJobInputConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@RecommendationJobInputConfig' {} a -> s {volumeKmsKeyId = a} :: RecommendationJobInputConfig)

-- | Specifies the endpoint configuration to use for a job.
recommendationJobInputConfig_endpointConfigurations :: Lens.Lens' RecommendationJobInputConfig (Prelude.Maybe (Prelude.NonEmpty EndpointInputConfiguration))
recommendationJobInputConfig_endpointConfigurations = Lens.lens (\RecommendationJobInputConfig' {endpointConfigurations} -> endpointConfigurations) (\s@RecommendationJobInputConfig' {} a -> s {endpointConfigurations = a} :: RecommendationJobInputConfig) Prelude.. Lens.mapping Lens.coerced

-- | Defines the resource limit of the job.
recommendationJobInputConfig_resourceLimit :: Lens.Lens' RecommendationJobInputConfig (Prelude.Maybe RecommendationJobResourceLimit)
recommendationJobInputConfig_resourceLimit = Lens.lens (\RecommendationJobInputConfig' {resourceLimit} -> resourceLimit) (\s@RecommendationJobInputConfig' {} a -> s {resourceLimit = a} :: RecommendationJobInputConfig)

-- | The Amazon Resource Name (ARN) of a versioned model package.
recommendationJobInputConfig_modelPackageVersionArn :: Lens.Lens' RecommendationJobInputConfig Prelude.Text
recommendationJobInputConfig_modelPackageVersionArn = Lens.lens (\RecommendationJobInputConfig' {modelPackageVersionArn} -> modelPackageVersionArn) (\s@RecommendationJobInputConfig' {} a -> s {modelPackageVersionArn = a} :: RecommendationJobInputConfig)

instance Core.FromJSON RecommendationJobInputConfig where
  parseJSON =
    Core.withObject
      "RecommendationJobInputConfig"
      ( \x ->
          RecommendationJobInputConfig'
            Prelude.<$> (x Core..:? "TrafficPattern")
            Prelude.<*> (x Core..:? "JobDurationInSeconds")
            Prelude.<*> (x Core..:? "VolumeKmsKeyId")
            Prelude.<*> (x Core..:? "EndpointConfigurations")
            Prelude.<*> (x Core..:? "ResourceLimit")
            Prelude.<*> (x Core..: "ModelPackageVersionArn")
      )

instance
  Prelude.Hashable
    RecommendationJobInputConfig
  where
  hashWithSalt _salt RecommendationJobInputConfig' {..} =
    _salt `Prelude.hashWithSalt` trafficPattern
      `Prelude.hashWithSalt` jobDurationInSeconds
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` endpointConfigurations
      `Prelude.hashWithSalt` resourceLimit
      `Prelude.hashWithSalt` modelPackageVersionArn

instance Prelude.NFData RecommendationJobInputConfig where
  rnf RecommendationJobInputConfig' {..} =
    Prelude.rnf trafficPattern
      `Prelude.seq` Prelude.rnf jobDurationInSeconds
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf endpointConfigurations
      `Prelude.seq` Prelude.rnf resourceLimit
      `Prelude.seq` Prelude.rnf modelPackageVersionArn

instance Core.ToJSON RecommendationJobInputConfig where
  toJSON RecommendationJobInputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TrafficPattern" Core..=)
              Prelude.<$> trafficPattern,
            ("JobDurationInSeconds" Core..=)
              Prelude.<$> jobDurationInSeconds,
            ("VolumeKmsKeyId" Core..=)
              Prelude.<$> volumeKmsKeyId,
            ("EndpointConfigurations" Core..=)
              Prelude.<$> endpointConfigurations,
            ("ResourceLimit" Core..=) Prelude.<$> resourceLimit,
            Prelude.Just
              ( "ModelPackageVersionArn"
                  Core..= modelPackageVersionArn
              )
          ]
      )
