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
-- Module      : Network.AWS.SageMaker.Types.InferenceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.InferenceSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
import Network.AWS.SageMaker.Types.ProductionVariantInstanceType
import Network.AWS.SageMaker.Types.TransformInstanceType

-- | Defines how to perform inference generation after a training job is run.
--
-- /See:/ 'newInferenceSpecification' smart constructor.
data InferenceSpecification = InferenceSpecification'
  { -- | A list of the instance types on which a transformation job can be run or
    -- on which an endpoint can be deployed.
    --
    -- This parameter is required for unversioned models, and optional for
    -- versioned models.
    supportedTransformInstanceTypes :: Prelude.Maybe (Prelude.NonEmpty TransformInstanceType),
    -- | A list of the instance types that are used to generate inferences in
    -- real-time.
    --
    -- This parameter is required for unversioned models, and optional for
    -- versioned models.
    supportedRealtimeInferenceInstanceTypes :: Prelude.Maybe [ProductionVariantInstanceType],
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
-- 'supportedTransformInstanceTypes', 'inferenceSpecification_supportedTransformInstanceTypes' - A list of the instance types on which a transformation job can be run or
-- on which an endpoint can be deployed.
--
-- This parameter is required for unversioned models, and optional for
-- versioned models.
--
-- 'supportedRealtimeInferenceInstanceTypes', 'inferenceSpecification_supportedRealtimeInferenceInstanceTypes' - A list of the instance types that are used to generate inferences in
-- real-time.
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
    { supportedTransformInstanceTypes =
        Prelude.Nothing,
      supportedRealtimeInferenceInstanceTypes =
        Prelude.Nothing,
      containers = Lens._Coerce Lens.# pContainers_,
      supportedContentTypes = Prelude.mempty,
      supportedResponseMIMETypes = Prelude.mempty
    }

-- | A list of the instance types on which a transformation job can be run or
-- on which an endpoint can be deployed.
--
-- This parameter is required for unversioned models, and optional for
-- versioned models.
inferenceSpecification_supportedTransformInstanceTypes :: Lens.Lens' InferenceSpecification (Prelude.Maybe (Prelude.NonEmpty TransformInstanceType))
inferenceSpecification_supportedTransformInstanceTypes = Lens.lens (\InferenceSpecification' {supportedTransformInstanceTypes} -> supportedTransformInstanceTypes) (\s@InferenceSpecification' {} a -> s {supportedTransformInstanceTypes = a} :: InferenceSpecification) Prelude.. Lens.mapping Lens._Coerce

-- | A list of the instance types that are used to generate inferences in
-- real-time.
--
-- This parameter is required for unversioned models, and optional for
-- versioned models.
inferenceSpecification_supportedRealtimeInferenceInstanceTypes :: Lens.Lens' InferenceSpecification (Prelude.Maybe [ProductionVariantInstanceType])
inferenceSpecification_supportedRealtimeInferenceInstanceTypes = Lens.lens (\InferenceSpecification' {supportedRealtimeInferenceInstanceTypes} -> supportedRealtimeInferenceInstanceTypes) (\s@InferenceSpecification' {} a -> s {supportedRealtimeInferenceInstanceTypes = a} :: InferenceSpecification) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon ECR registry path of the Docker image that contains the
-- inference code.
inferenceSpecification_containers :: Lens.Lens' InferenceSpecification (Prelude.NonEmpty ModelPackageContainerDefinition)
inferenceSpecification_containers = Lens.lens (\InferenceSpecification' {containers} -> containers) (\s@InferenceSpecification' {} a -> s {containers = a} :: InferenceSpecification) Prelude.. Lens._Coerce

-- | The supported MIME types for the input data.
inferenceSpecification_supportedContentTypes :: Lens.Lens' InferenceSpecification [Prelude.Text]
inferenceSpecification_supportedContentTypes = Lens.lens (\InferenceSpecification' {supportedContentTypes} -> supportedContentTypes) (\s@InferenceSpecification' {} a -> s {supportedContentTypes = a} :: InferenceSpecification) Prelude.. Lens._Coerce

-- | The supported MIME types for the output data.
inferenceSpecification_supportedResponseMIMETypes :: Lens.Lens' InferenceSpecification [Prelude.Text]
inferenceSpecification_supportedResponseMIMETypes = Lens.lens (\InferenceSpecification' {supportedResponseMIMETypes} -> supportedResponseMIMETypes) (\s@InferenceSpecification' {} a -> s {supportedResponseMIMETypes = a} :: InferenceSpecification) Prelude.. Lens._Coerce

instance Core.FromJSON InferenceSpecification where
  parseJSON =
    Core.withObject
      "InferenceSpecification"
      ( \x ->
          InferenceSpecification'
            Prelude.<$> (x Core..:? "SupportedTransformInstanceTypes")
            Prelude.<*> ( x Core..:? "SupportedRealtimeInferenceInstanceTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "Containers")
            Prelude.<*> ( x Core..:? "SupportedContentTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "SupportedResponseMIMETypes"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InferenceSpecification

instance Prelude.NFData InferenceSpecification

instance Core.ToJSON InferenceSpecification where
  toJSON InferenceSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SupportedTransformInstanceTypes" Core..=)
              Prelude.<$> supportedTransformInstanceTypes,
            ("SupportedRealtimeInferenceInstanceTypes" Core..=)
              Prelude.<$> supportedRealtimeInferenceInstanceTypes,
            Prelude.Just ("Containers" Core..= containers),
            Prelude.Just
              ( "SupportedContentTypes"
                  Core..= supportedContentTypes
              ),
            Prelude.Just
              ( "SupportedResponseMIMETypes"
                  Core..= supportedResponseMIMETypes
              )
          ]
      )
