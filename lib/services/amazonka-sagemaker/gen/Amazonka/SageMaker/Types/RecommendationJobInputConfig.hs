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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobInputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointInfo
import Amazonka.SageMaker.Types.EndpointInputConfiguration
import Amazonka.SageMaker.Types.RecommendationJobContainerConfig
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
    -- | Existing customer endpoints on which to run an Inference Recommender
    -- job.
    endpoints :: Prelude.Maybe [EndpointInfo],
    -- | Specifies the endpoint configuration to use for a job.
    endpointConfigurations :: Prelude.Maybe (Prelude.NonEmpty EndpointInputConfiguration),
    -- | Defines the resource limit of the job.
    resourceLimit :: Prelude.Maybe RecommendationJobResourceLimit,
    -- | Specifies mandatory fields for running an Inference Recommender job. The
    -- fields specified in @ContainerConfig@ override the corresponding fields
    -- in the model package.
    containerConfig :: Prelude.Maybe RecommendationJobContainerConfig,
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
-- 'endpoints', 'recommendationJobInputConfig_endpoints' - Existing customer endpoints on which to run an Inference Recommender
-- job.
--
-- 'endpointConfigurations', 'recommendationJobInputConfig_endpointConfigurations' - Specifies the endpoint configuration to use for a job.
--
-- 'resourceLimit', 'recommendationJobInputConfig_resourceLimit' - Defines the resource limit of the job.
--
-- 'containerConfig', 'recommendationJobInputConfig_containerConfig' - Specifies mandatory fields for running an Inference Recommender job. The
-- fields specified in @ContainerConfig@ override the corresponding fields
-- in the model package.
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
        endpoints = Prelude.Nothing,
        endpointConfigurations = Prelude.Nothing,
        resourceLimit = Prelude.Nothing,
        containerConfig = Prelude.Nothing,
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

-- | Existing customer endpoints on which to run an Inference Recommender
-- job.
recommendationJobInputConfig_endpoints :: Lens.Lens' RecommendationJobInputConfig (Prelude.Maybe [EndpointInfo])
recommendationJobInputConfig_endpoints = Lens.lens (\RecommendationJobInputConfig' {endpoints} -> endpoints) (\s@RecommendationJobInputConfig' {} a -> s {endpoints = a} :: RecommendationJobInputConfig) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the endpoint configuration to use for a job.
recommendationJobInputConfig_endpointConfigurations :: Lens.Lens' RecommendationJobInputConfig (Prelude.Maybe (Prelude.NonEmpty EndpointInputConfiguration))
recommendationJobInputConfig_endpointConfigurations = Lens.lens (\RecommendationJobInputConfig' {endpointConfigurations} -> endpointConfigurations) (\s@RecommendationJobInputConfig' {} a -> s {endpointConfigurations = a} :: RecommendationJobInputConfig) Prelude.. Lens.mapping Lens.coerced

-- | Defines the resource limit of the job.
recommendationJobInputConfig_resourceLimit :: Lens.Lens' RecommendationJobInputConfig (Prelude.Maybe RecommendationJobResourceLimit)
recommendationJobInputConfig_resourceLimit = Lens.lens (\RecommendationJobInputConfig' {resourceLimit} -> resourceLimit) (\s@RecommendationJobInputConfig' {} a -> s {resourceLimit = a} :: RecommendationJobInputConfig)

-- | Specifies mandatory fields for running an Inference Recommender job. The
-- fields specified in @ContainerConfig@ override the corresponding fields
-- in the model package.
recommendationJobInputConfig_containerConfig :: Lens.Lens' RecommendationJobInputConfig (Prelude.Maybe RecommendationJobContainerConfig)
recommendationJobInputConfig_containerConfig = Lens.lens (\RecommendationJobInputConfig' {containerConfig} -> containerConfig) (\s@RecommendationJobInputConfig' {} a -> s {containerConfig = a} :: RecommendationJobInputConfig)

-- | The Amazon Resource Name (ARN) of a versioned model package.
recommendationJobInputConfig_modelPackageVersionArn :: Lens.Lens' RecommendationJobInputConfig Prelude.Text
recommendationJobInputConfig_modelPackageVersionArn = Lens.lens (\RecommendationJobInputConfig' {modelPackageVersionArn} -> modelPackageVersionArn) (\s@RecommendationJobInputConfig' {} a -> s {modelPackageVersionArn = a} :: RecommendationJobInputConfig)

instance Data.FromJSON RecommendationJobInputConfig where
  parseJSON =
    Data.withObject
      "RecommendationJobInputConfig"
      ( \x ->
          RecommendationJobInputConfig'
            Prelude.<$> (x Data..:? "TrafficPattern")
            Prelude.<*> (x Data..:? "JobDurationInSeconds")
            Prelude.<*> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "Endpoints" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EndpointConfigurations")
            Prelude.<*> (x Data..:? "ResourceLimit")
            Prelude.<*> (x Data..:? "ContainerConfig")
            Prelude.<*> (x Data..: "ModelPackageVersionArn")
      )

instance
  Prelude.Hashable
    RecommendationJobInputConfig
  where
  hashWithSalt _salt RecommendationJobInputConfig' {..} =
    _salt `Prelude.hashWithSalt` trafficPattern
      `Prelude.hashWithSalt` jobDurationInSeconds
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` endpointConfigurations
      `Prelude.hashWithSalt` resourceLimit
      `Prelude.hashWithSalt` containerConfig
      `Prelude.hashWithSalt` modelPackageVersionArn

instance Prelude.NFData RecommendationJobInputConfig where
  rnf RecommendationJobInputConfig' {..} =
    Prelude.rnf trafficPattern
      `Prelude.seq` Prelude.rnf jobDurationInSeconds
      `Prelude.seq` Prelude.rnf volumeKmsKeyId
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf endpointConfigurations
      `Prelude.seq` Prelude.rnf resourceLimit
      `Prelude.seq` Prelude.rnf containerConfig
      `Prelude.seq` Prelude.rnf modelPackageVersionArn

instance Data.ToJSON RecommendationJobInputConfig where
  toJSON RecommendationJobInputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TrafficPattern" Data..=)
              Prelude.<$> trafficPattern,
            ("JobDurationInSeconds" Data..=)
              Prelude.<$> jobDurationInSeconds,
            ("VolumeKmsKeyId" Data..=)
              Prelude.<$> volumeKmsKeyId,
            ("Endpoints" Data..=) Prelude.<$> endpoints,
            ("EndpointConfigurations" Data..=)
              Prelude.<$> endpointConfigurations,
            ("ResourceLimit" Data..=) Prelude.<$> resourceLimit,
            ("ContainerConfig" Data..=)
              Prelude.<$> containerConfig,
            Prelude.Just
              ( "ModelPackageVersionArn"
                  Data..= modelPackageVersionArn
              )
          ]
      )
