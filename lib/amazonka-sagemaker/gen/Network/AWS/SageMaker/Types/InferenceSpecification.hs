{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.InferenceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.InferenceSpecification
  ( InferenceSpecification (..),

    -- * Smart constructor
    mkInferenceSpecification,

    -- * Lenses
    isContainers,
    isSupportedTransformInstanceTypes,
    isSupportedRealtimeInferenceInstanceTypes,
    isSupportedContentTypes,
    isSupportedResponseMIMETypes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ContentType as Types
import qualified Network.AWS.SageMaker.Types.ModelPackageContainerDefinition as Types
import qualified Network.AWS.SageMaker.Types.ProductionVariantInstanceType as Types
import qualified Network.AWS.SageMaker.Types.ResponseMIMEType as Types
import qualified Network.AWS.SageMaker.Types.TransformInstanceType as Types

-- | Defines how to perform inference generation after a training job is run.
--
-- /See:/ 'mkInferenceSpecification' smart constructor.
data InferenceSpecification = InferenceSpecification'
  { -- | The Amazon ECR registry path of the Docker image that contains the inference code.
    containers :: Core.NonEmpty Types.ModelPackageContainerDefinition,
    -- | A list of the instance types on which a transformation job can be run or on which an endpoint can be deployed.
    supportedTransformInstanceTypes :: Core.NonEmpty Types.TransformInstanceType,
    -- | A list of the instance types that are used to generate inferences in real-time.
    supportedRealtimeInferenceInstanceTypes :: [Types.ProductionVariantInstanceType],
    -- | The supported MIME types for the input data.
    supportedContentTypes :: [Types.ContentType],
    -- | The supported MIME types for the output data.
    supportedResponseMIMETypes :: [Types.ResponseMIMEType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InferenceSpecification' value with any optional fields omitted.
mkInferenceSpecification ::
  -- | 'containers'
  Core.NonEmpty Types.ModelPackageContainerDefinition ->
  -- | 'supportedTransformInstanceTypes'
  Core.NonEmpty Types.TransformInstanceType ->
  InferenceSpecification
mkInferenceSpecification containers supportedTransformInstanceTypes =
  InferenceSpecification'
    { containers,
      supportedTransformInstanceTypes,
      supportedRealtimeInferenceInstanceTypes = Core.mempty,
      supportedContentTypes = Core.mempty,
      supportedResponseMIMETypes = Core.mempty
    }

-- | The Amazon ECR registry path of the Docker image that contains the inference code.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isContainers :: Lens.Lens' InferenceSpecification (Core.NonEmpty Types.ModelPackageContainerDefinition)
isContainers = Lens.field @"containers"
{-# DEPRECATED isContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | A list of the instance types on which a transformation job can be run or on which an endpoint can be deployed.
--
-- /Note:/ Consider using 'supportedTransformInstanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportedTransformInstanceTypes :: Lens.Lens' InferenceSpecification (Core.NonEmpty Types.TransformInstanceType)
isSupportedTransformInstanceTypes = Lens.field @"supportedTransformInstanceTypes"
{-# DEPRECATED isSupportedTransformInstanceTypes "Use generic-lens or generic-optics with 'supportedTransformInstanceTypes' instead." #-}

-- | A list of the instance types that are used to generate inferences in real-time.
--
-- /Note:/ Consider using 'supportedRealtimeInferenceInstanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportedRealtimeInferenceInstanceTypes :: Lens.Lens' InferenceSpecification [Types.ProductionVariantInstanceType]
isSupportedRealtimeInferenceInstanceTypes = Lens.field @"supportedRealtimeInferenceInstanceTypes"
{-# DEPRECATED isSupportedRealtimeInferenceInstanceTypes "Use generic-lens or generic-optics with 'supportedRealtimeInferenceInstanceTypes' instead." #-}

-- | The supported MIME types for the input data.
--
-- /Note:/ Consider using 'supportedContentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportedContentTypes :: Lens.Lens' InferenceSpecification [Types.ContentType]
isSupportedContentTypes = Lens.field @"supportedContentTypes"
{-# DEPRECATED isSupportedContentTypes "Use generic-lens or generic-optics with 'supportedContentTypes' instead." #-}

-- | The supported MIME types for the output data.
--
-- /Note:/ Consider using 'supportedResponseMIMETypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportedResponseMIMETypes :: Lens.Lens' InferenceSpecification [Types.ResponseMIMEType]
isSupportedResponseMIMETypes = Lens.field @"supportedResponseMIMETypes"
{-# DEPRECATED isSupportedResponseMIMETypes "Use generic-lens or generic-optics with 'supportedResponseMIMETypes' instead." #-}

instance Core.FromJSON InferenceSpecification where
  toJSON InferenceSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Containers" Core..= containers),
            Core.Just
              ( "SupportedTransformInstanceTypes"
                  Core..= supportedTransformInstanceTypes
              ),
            Core.Just
              ( "SupportedRealtimeInferenceInstanceTypes"
                  Core..= supportedRealtimeInferenceInstanceTypes
              ),
            Core.Just ("SupportedContentTypes" Core..= supportedContentTypes),
            Core.Just
              ("SupportedResponseMIMETypes" Core..= supportedResponseMIMETypes)
          ]
      )

instance Core.FromJSON InferenceSpecification where
  parseJSON =
    Core.withObject "InferenceSpecification" Core.$
      \x ->
        InferenceSpecification'
          Core.<$> (x Core..: "Containers")
          Core.<*> (x Core..: "SupportedTransformInstanceTypes")
          Core.<*> ( x Core..:? "SupportedRealtimeInferenceInstanceTypes"
                       Core..!= Core.mempty
                   )
          Core.<*> (x Core..:? "SupportedContentTypes" Core..!= Core.mempty)
          Core.<*> (x Core..:? "SupportedResponseMIMETypes" Core..!= Core.mempty)
