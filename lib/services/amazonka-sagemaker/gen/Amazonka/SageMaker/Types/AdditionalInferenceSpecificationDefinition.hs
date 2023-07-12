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
-- Module      : Amazonka.SageMaker.Types.AdditionalInferenceSpecificationDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AdditionalInferenceSpecificationDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelPackageContainerDefinition
import Amazonka.SageMaker.Types.ProductionVariantInstanceType
import Amazonka.SageMaker.Types.TransformInstanceType

-- | A structure of additional Inference Specification. Additional Inference
-- Specification specifies details about inference jobs that can be run
-- with models based on this model package
--
-- /See:/ 'newAdditionalInferenceSpecificationDefinition' smart constructor.
data AdditionalInferenceSpecificationDefinition = AdditionalInferenceSpecificationDefinition'
  { -- | A description of the additional Inference specification
    description :: Prelude.Maybe Prelude.Text,
    -- | The supported MIME types for the input data.
    supportedContentTypes :: Prelude.Maybe [Prelude.Text],
    -- | A list of the instance types that are used to generate inferences in
    -- real-time.
    supportedRealtimeInferenceInstanceTypes :: Prelude.Maybe [ProductionVariantInstanceType],
    -- | The supported MIME types for the output data.
    supportedResponseMIMETypes :: Prelude.Maybe [Prelude.Text],
    -- | A list of the instance types on which a transformation job can be run or
    -- on which an endpoint can be deployed.
    supportedTransformInstanceTypes :: Prelude.Maybe (Prelude.NonEmpty TransformInstanceType),
    -- | A unique name to identify the additional inference specification. The
    -- name must be unique within the list of your additional inference
    -- specifications for a particular model package.
    name :: Prelude.Text,
    -- | The Amazon ECR registry path of the Docker image that contains the
    -- inference code.
    containers :: Prelude.NonEmpty ModelPackageContainerDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdditionalInferenceSpecificationDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'additionalInferenceSpecificationDefinition_description' - A description of the additional Inference specification
--
-- 'supportedContentTypes', 'additionalInferenceSpecificationDefinition_supportedContentTypes' - The supported MIME types for the input data.
--
-- 'supportedRealtimeInferenceInstanceTypes', 'additionalInferenceSpecificationDefinition_supportedRealtimeInferenceInstanceTypes' - A list of the instance types that are used to generate inferences in
-- real-time.
--
-- 'supportedResponseMIMETypes', 'additionalInferenceSpecificationDefinition_supportedResponseMIMETypes' - The supported MIME types for the output data.
--
-- 'supportedTransformInstanceTypes', 'additionalInferenceSpecificationDefinition_supportedTransformInstanceTypes' - A list of the instance types on which a transformation job can be run or
-- on which an endpoint can be deployed.
--
-- 'name', 'additionalInferenceSpecificationDefinition_name' - A unique name to identify the additional inference specification. The
-- name must be unique within the list of your additional inference
-- specifications for a particular model package.
--
-- 'containers', 'additionalInferenceSpecificationDefinition_containers' - The Amazon ECR registry path of the Docker image that contains the
-- inference code.
newAdditionalInferenceSpecificationDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'containers'
  Prelude.NonEmpty ModelPackageContainerDefinition ->
  AdditionalInferenceSpecificationDefinition
newAdditionalInferenceSpecificationDefinition
  pName_
  pContainers_ =
    AdditionalInferenceSpecificationDefinition'
      { description =
          Prelude.Nothing,
        supportedContentTypes =
          Prelude.Nothing,
        supportedRealtimeInferenceInstanceTypes =
          Prelude.Nothing,
        supportedResponseMIMETypes =
          Prelude.Nothing,
        supportedTransformInstanceTypes =
          Prelude.Nothing,
        name = pName_,
        containers =
          Lens.coerced
            Lens.# pContainers_
      }

-- | A description of the additional Inference specification
additionalInferenceSpecificationDefinition_description :: Lens.Lens' AdditionalInferenceSpecificationDefinition (Prelude.Maybe Prelude.Text)
additionalInferenceSpecificationDefinition_description = Lens.lens (\AdditionalInferenceSpecificationDefinition' {description} -> description) (\s@AdditionalInferenceSpecificationDefinition' {} a -> s {description = a} :: AdditionalInferenceSpecificationDefinition)

-- | The supported MIME types for the input data.
additionalInferenceSpecificationDefinition_supportedContentTypes :: Lens.Lens' AdditionalInferenceSpecificationDefinition (Prelude.Maybe [Prelude.Text])
additionalInferenceSpecificationDefinition_supportedContentTypes = Lens.lens (\AdditionalInferenceSpecificationDefinition' {supportedContentTypes} -> supportedContentTypes) (\s@AdditionalInferenceSpecificationDefinition' {} a -> s {supportedContentTypes = a} :: AdditionalInferenceSpecificationDefinition) Prelude.. Lens.mapping Lens.coerced

-- | A list of the instance types that are used to generate inferences in
-- real-time.
additionalInferenceSpecificationDefinition_supportedRealtimeInferenceInstanceTypes :: Lens.Lens' AdditionalInferenceSpecificationDefinition (Prelude.Maybe [ProductionVariantInstanceType])
additionalInferenceSpecificationDefinition_supportedRealtimeInferenceInstanceTypes = Lens.lens (\AdditionalInferenceSpecificationDefinition' {supportedRealtimeInferenceInstanceTypes} -> supportedRealtimeInferenceInstanceTypes) (\s@AdditionalInferenceSpecificationDefinition' {} a -> s {supportedRealtimeInferenceInstanceTypes = a} :: AdditionalInferenceSpecificationDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The supported MIME types for the output data.
additionalInferenceSpecificationDefinition_supportedResponseMIMETypes :: Lens.Lens' AdditionalInferenceSpecificationDefinition (Prelude.Maybe [Prelude.Text])
additionalInferenceSpecificationDefinition_supportedResponseMIMETypes = Lens.lens (\AdditionalInferenceSpecificationDefinition' {supportedResponseMIMETypes} -> supportedResponseMIMETypes) (\s@AdditionalInferenceSpecificationDefinition' {} a -> s {supportedResponseMIMETypes = a} :: AdditionalInferenceSpecificationDefinition) Prelude.. Lens.mapping Lens.coerced

-- | A list of the instance types on which a transformation job can be run or
-- on which an endpoint can be deployed.
additionalInferenceSpecificationDefinition_supportedTransformInstanceTypes :: Lens.Lens' AdditionalInferenceSpecificationDefinition (Prelude.Maybe (Prelude.NonEmpty TransformInstanceType))
additionalInferenceSpecificationDefinition_supportedTransformInstanceTypes = Lens.lens (\AdditionalInferenceSpecificationDefinition' {supportedTransformInstanceTypes} -> supportedTransformInstanceTypes) (\s@AdditionalInferenceSpecificationDefinition' {} a -> s {supportedTransformInstanceTypes = a} :: AdditionalInferenceSpecificationDefinition) Prelude.. Lens.mapping Lens.coerced

-- | A unique name to identify the additional inference specification. The
-- name must be unique within the list of your additional inference
-- specifications for a particular model package.
additionalInferenceSpecificationDefinition_name :: Lens.Lens' AdditionalInferenceSpecificationDefinition Prelude.Text
additionalInferenceSpecificationDefinition_name = Lens.lens (\AdditionalInferenceSpecificationDefinition' {name} -> name) (\s@AdditionalInferenceSpecificationDefinition' {} a -> s {name = a} :: AdditionalInferenceSpecificationDefinition)

-- | The Amazon ECR registry path of the Docker image that contains the
-- inference code.
additionalInferenceSpecificationDefinition_containers :: Lens.Lens' AdditionalInferenceSpecificationDefinition (Prelude.NonEmpty ModelPackageContainerDefinition)
additionalInferenceSpecificationDefinition_containers = Lens.lens (\AdditionalInferenceSpecificationDefinition' {containers} -> containers) (\s@AdditionalInferenceSpecificationDefinition' {} a -> s {containers = a} :: AdditionalInferenceSpecificationDefinition) Prelude.. Lens.coerced

instance
  Data.FromJSON
    AdditionalInferenceSpecificationDefinition
  where
  parseJSON =
    Data.withObject
      "AdditionalInferenceSpecificationDefinition"
      ( \x ->
          AdditionalInferenceSpecificationDefinition'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> ( x
                            Data..:? "SupportedContentTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SupportedRealtimeInferenceInstanceTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SupportedResponseMIMETypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SupportedTransformInstanceTypes")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Containers")
      )

instance
  Prelude.Hashable
    AdditionalInferenceSpecificationDefinition
  where
  hashWithSalt
    _salt
    AdditionalInferenceSpecificationDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` supportedContentTypes
        `Prelude.hashWithSalt` supportedRealtimeInferenceInstanceTypes
        `Prelude.hashWithSalt` supportedResponseMIMETypes
        `Prelude.hashWithSalt` supportedTransformInstanceTypes
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` containers

instance
  Prelude.NFData
    AdditionalInferenceSpecificationDefinition
  where
  rnf AdditionalInferenceSpecificationDefinition' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf supportedContentTypes
      `Prelude.seq` Prelude.rnf supportedRealtimeInferenceInstanceTypes
      `Prelude.seq` Prelude.rnf supportedResponseMIMETypes
      `Prelude.seq` Prelude.rnf supportedTransformInstanceTypes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf containers

instance
  Data.ToJSON
    AdditionalInferenceSpecificationDefinition
  where
  toJSON
    AdditionalInferenceSpecificationDefinition' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Description" Data..=) Prelude.<$> description,
              ("SupportedContentTypes" Data..=)
                Prelude.<$> supportedContentTypes,
              ("SupportedRealtimeInferenceInstanceTypes" Data..=)
                Prelude.<$> supportedRealtimeInferenceInstanceTypes,
              ("SupportedResponseMIMETypes" Data..=)
                Prelude.<$> supportedResponseMIMETypes,
              ("SupportedTransformInstanceTypes" Data..=)
                Prelude.<$> supportedTransformInstanceTypes,
              Prelude.Just ("Name" Data..= name),
              Prelude.Just ("Containers" Data..= containers)
            ]
        )
