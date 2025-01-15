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
-- Module      : Amazonka.SageMaker.Types.InferenceSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelPackageContainerDefinition
import Amazonka.SageMaker.Types.ProductionVariantInstanceType
import Amazonka.SageMaker.Types.TransformInstanceType

-- | Defines how to perform inference generation after a training job is run.
--
-- /See:/ 'newInferenceSpecification' smart constructor.
data InferenceSpecification = InferenceSpecification'
  { -- | A list of the instance types that are used to generate inferences in
    -- real-time.
    --
    -- This parameter is required for unversioned models, and optional for
    -- versioned models.
    supportedRealtimeInferenceInstanceTypes :: Prelude.Maybe [ProductionVariantInstanceType],
    -- | A list of the instance types on which a transformation job can be run or
    -- on which an endpoint can be deployed.
    --
    -- This parameter is required for unversioned models, and optional for
    -- versioned models.
    supportedTransformInstanceTypes :: Prelude.Maybe (Prelude.NonEmpty TransformInstanceType),
    -- | The Amazon ECR registry path of the Docker image that contains the
    -- inference code.
    containers :: Prelude.NonEmpty ModelPackageContainerDefinition,
    -- | The supported MIME types for the input data.
    supportedContentTypes :: [Prelude.Text],
    -- | The supported MIME types for the output data.
    supportedResponseMIMETypes :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'supportedRealtimeInferenceInstanceTypes', 'inferenceSpecification_supportedRealtimeInferenceInstanceTypes' - A list of the instance types that are used to generate inferences in
-- real-time.
--
-- This parameter is required for unversioned models, and optional for
-- versioned models.
--
-- 'supportedTransformInstanceTypes', 'inferenceSpecification_supportedTransformInstanceTypes' - A list of the instance types on which a transformation job can be run or
-- on which an endpoint can be deployed.
--
-- This parameter is required for unversioned models, and optional for
-- versioned models.
--
-- 'containers', 'inferenceSpecification_containers' - The Amazon ECR registry path of the Docker image that contains the
-- inference code.
--
-- 'supportedContentTypes', 'inferenceSpecification_supportedContentTypes' - The supported MIME types for the input data.
--
-- 'supportedResponseMIMETypes', 'inferenceSpecification_supportedResponseMIMETypes' - The supported MIME types for the output data.
newInferenceSpecification ::
  -- | 'containers'
  Prelude.NonEmpty ModelPackageContainerDefinition ->
  InferenceSpecification
newInferenceSpecification pContainers_ =
  InferenceSpecification'
    { supportedRealtimeInferenceInstanceTypes =
        Prelude.Nothing,
      supportedTransformInstanceTypes = Prelude.Nothing,
      containers = Lens.coerced Lens.# pContainers_,
      supportedContentTypes = Prelude.mempty,
      supportedResponseMIMETypes = Prelude.mempty
    }

-- | A list of the instance types that are used to generate inferences in
-- real-time.
--
-- This parameter is required for unversioned models, and optional for
-- versioned models.
inferenceSpecification_supportedRealtimeInferenceInstanceTypes :: Lens.Lens' InferenceSpecification (Prelude.Maybe [ProductionVariantInstanceType])
inferenceSpecification_supportedRealtimeInferenceInstanceTypes = Lens.lens (\InferenceSpecification' {supportedRealtimeInferenceInstanceTypes} -> supportedRealtimeInferenceInstanceTypes) (\s@InferenceSpecification' {} a -> s {supportedRealtimeInferenceInstanceTypes = a} :: InferenceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | A list of the instance types on which a transformation job can be run or
-- on which an endpoint can be deployed.
--
-- This parameter is required for unversioned models, and optional for
-- versioned models.
inferenceSpecification_supportedTransformInstanceTypes :: Lens.Lens' InferenceSpecification (Prelude.Maybe (Prelude.NonEmpty TransformInstanceType))
inferenceSpecification_supportedTransformInstanceTypes = Lens.lens (\InferenceSpecification' {supportedTransformInstanceTypes} -> supportedTransformInstanceTypes) (\s@InferenceSpecification' {} a -> s {supportedTransformInstanceTypes = a} :: InferenceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon ECR registry path of the Docker image that contains the
-- inference code.
inferenceSpecification_containers :: Lens.Lens' InferenceSpecification (Prelude.NonEmpty ModelPackageContainerDefinition)
inferenceSpecification_containers = Lens.lens (\InferenceSpecification' {containers} -> containers) (\s@InferenceSpecification' {} a -> s {containers = a} :: InferenceSpecification) Prelude.. Lens.coerced

-- | The supported MIME types for the input data.
inferenceSpecification_supportedContentTypes :: Lens.Lens' InferenceSpecification [Prelude.Text]
inferenceSpecification_supportedContentTypes = Lens.lens (\InferenceSpecification' {supportedContentTypes} -> supportedContentTypes) (\s@InferenceSpecification' {} a -> s {supportedContentTypes = a} :: InferenceSpecification) Prelude.. Lens.coerced

-- | The supported MIME types for the output data.
inferenceSpecification_supportedResponseMIMETypes :: Lens.Lens' InferenceSpecification [Prelude.Text]
inferenceSpecification_supportedResponseMIMETypes = Lens.lens (\InferenceSpecification' {supportedResponseMIMETypes} -> supportedResponseMIMETypes) (\s@InferenceSpecification' {} a -> s {supportedResponseMIMETypes = a} :: InferenceSpecification) Prelude.. Lens.coerced

instance Data.FromJSON InferenceSpecification where
  parseJSON =
    Data.withObject
      "InferenceSpecification"
      ( \x ->
          InferenceSpecification'
            Prelude.<$> ( x
                            Data..:? "SupportedRealtimeInferenceInstanceTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SupportedTransformInstanceTypes")
            Prelude.<*> (x Data..: "Containers")
            Prelude.<*> ( x
                            Data..:? "SupportedContentTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SupportedResponseMIMETypes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InferenceSpecification where
  hashWithSalt _salt InferenceSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` supportedRealtimeInferenceInstanceTypes
      `Prelude.hashWithSalt` supportedTransformInstanceTypes
      `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` supportedContentTypes
      `Prelude.hashWithSalt` supportedResponseMIMETypes

instance Prelude.NFData InferenceSpecification where
  rnf InferenceSpecification' {..} =
    Prelude.rnf supportedRealtimeInferenceInstanceTypes `Prelude.seq`
      Prelude.rnf supportedTransformInstanceTypes `Prelude.seq`
        Prelude.rnf containers `Prelude.seq`
          Prelude.rnf supportedContentTypes `Prelude.seq`
            Prelude.rnf supportedResponseMIMETypes

instance Data.ToJSON InferenceSpecification where
  toJSON InferenceSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SupportedRealtimeInferenceInstanceTypes" Data..=)
              Prelude.<$> supportedRealtimeInferenceInstanceTypes,
            ("SupportedTransformInstanceTypes" Data..=)
              Prelude.<$> supportedTransformInstanceTypes,
            Prelude.Just ("Containers" Data..= containers),
            Prelude.Just
              ( "SupportedContentTypes"
                  Data..= supportedContentTypes
              ),
            Prelude.Just
              ( "SupportedResponseMIMETypes"
                  Data..= supportedResponseMIMETypes
              )
          ]
      )
