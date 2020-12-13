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
    isSupportedContentTypes,
    isSupportedResponseMIMETypes,
    isSupportedRealtimeInferenceInstanceTypes,
    isContainers,
    isSupportedTransformInstanceTypes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
import Network.AWS.SageMaker.Types.ProductionVariantInstanceType
import Network.AWS.SageMaker.Types.TransformInstanceType

-- | Defines how to perform inference generation after a training job is run.
--
-- /See:/ 'mkInferenceSpecification' smart constructor.
data InferenceSpecification = InferenceSpecification'
  { -- | The supported MIME types for the input data.
    supportedContentTypes :: [Lude.Text],
    -- | The supported MIME types for the output data.
    supportedResponseMIMETypes :: [Lude.Text],
    -- | A list of the instance types that are used to generate inferences in real-time.
    supportedRealtimeInferenceInstanceTypes :: [ProductionVariantInstanceType],
    -- | The Amazon ECR registry path of the Docker image that contains the inference code.
    containers :: Lude.NonEmpty ModelPackageContainerDefinition,
    -- | A list of the instance types on which a transformation job can be run or on which an endpoint can be deployed.
    supportedTransformInstanceTypes :: Lude.NonEmpty TransformInstanceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InferenceSpecification' with the minimum fields required to make a request.
--
-- * 'supportedContentTypes' - The supported MIME types for the input data.
-- * 'supportedResponseMIMETypes' - The supported MIME types for the output data.
-- * 'supportedRealtimeInferenceInstanceTypes' - A list of the instance types that are used to generate inferences in real-time.
-- * 'containers' - The Amazon ECR registry path of the Docker image that contains the inference code.
-- * 'supportedTransformInstanceTypes' - A list of the instance types on which a transformation job can be run or on which an endpoint can be deployed.
mkInferenceSpecification ::
  -- | 'containers'
  Lude.NonEmpty ModelPackageContainerDefinition ->
  -- | 'supportedTransformInstanceTypes'
  Lude.NonEmpty TransformInstanceType ->
  InferenceSpecification
mkInferenceSpecification
  pContainers_
  pSupportedTransformInstanceTypes_ =
    InferenceSpecification'
      { supportedContentTypes = Lude.mempty,
        supportedResponseMIMETypes = Lude.mempty,
        supportedRealtimeInferenceInstanceTypes = Lude.mempty,
        containers = pContainers_,
        supportedTransformInstanceTypes =
          pSupportedTransformInstanceTypes_
      }

-- | The supported MIME types for the input data.
--
-- /Note:/ Consider using 'supportedContentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportedContentTypes :: Lens.Lens' InferenceSpecification [Lude.Text]
isSupportedContentTypes = Lens.lens (supportedContentTypes :: InferenceSpecification -> [Lude.Text]) (\s a -> s {supportedContentTypes = a} :: InferenceSpecification)
{-# DEPRECATED isSupportedContentTypes "Use generic-lens or generic-optics with 'supportedContentTypes' instead." #-}

-- | The supported MIME types for the output data.
--
-- /Note:/ Consider using 'supportedResponseMIMETypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportedResponseMIMETypes :: Lens.Lens' InferenceSpecification [Lude.Text]
isSupportedResponseMIMETypes = Lens.lens (supportedResponseMIMETypes :: InferenceSpecification -> [Lude.Text]) (\s a -> s {supportedResponseMIMETypes = a} :: InferenceSpecification)
{-# DEPRECATED isSupportedResponseMIMETypes "Use generic-lens or generic-optics with 'supportedResponseMIMETypes' instead." #-}

-- | A list of the instance types that are used to generate inferences in real-time.
--
-- /Note:/ Consider using 'supportedRealtimeInferenceInstanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportedRealtimeInferenceInstanceTypes :: Lens.Lens' InferenceSpecification [ProductionVariantInstanceType]
isSupportedRealtimeInferenceInstanceTypes = Lens.lens (supportedRealtimeInferenceInstanceTypes :: InferenceSpecification -> [ProductionVariantInstanceType]) (\s a -> s {supportedRealtimeInferenceInstanceTypes = a} :: InferenceSpecification)
{-# DEPRECATED isSupportedRealtimeInferenceInstanceTypes "Use generic-lens or generic-optics with 'supportedRealtimeInferenceInstanceTypes' instead." #-}

-- | The Amazon ECR registry path of the Docker image that contains the inference code.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isContainers :: Lens.Lens' InferenceSpecification (Lude.NonEmpty ModelPackageContainerDefinition)
isContainers = Lens.lens (containers :: InferenceSpecification -> Lude.NonEmpty ModelPackageContainerDefinition) (\s a -> s {containers = a} :: InferenceSpecification)
{-# DEPRECATED isContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | A list of the instance types on which a transformation job can be run or on which an endpoint can be deployed.
--
-- /Note:/ Consider using 'supportedTransformInstanceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportedTransformInstanceTypes :: Lens.Lens' InferenceSpecification (Lude.NonEmpty TransformInstanceType)
isSupportedTransformInstanceTypes = Lens.lens (supportedTransformInstanceTypes :: InferenceSpecification -> Lude.NonEmpty TransformInstanceType) (\s a -> s {supportedTransformInstanceTypes = a} :: InferenceSpecification)
{-# DEPRECATED isSupportedTransformInstanceTypes "Use generic-lens or generic-optics with 'supportedTransformInstanceTypes' instead." #-}

instance Lude.FromJSON InferenceSpecification where
  parseJSON =
    Lude.withObject
      "InferenceSpecification"
      ( \x ->
          InferenceSpecification'
            Lude.<$> (x Lude..:? "SupportedContentTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SupportedResponseMIMETypes" Lude..!= Lude.mempty)
            Lude.<*> ( x Lude..:? "SupportedRealtimeInferenceInstanceTypes"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..: "Containers")
            Lude.<*> (x Lude..: "SupportedTransformInstanceTypes")
      )

instance Lude.ToJSON InferenceSpecification where
  toJSON InferenceSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SupportedContentTypes" Lude..= supportedContentTypes),
            Lude.Just
              ("SupportedResponseMIMETypes" Lude..= supportedResponseMIMETypes),
            Lude.Just
              ( "SupportedRealtimeInferenceInstanceTypes"
                  Lude..= supportedRealtimeInferenceInstanceTypes
              ),
            Lude.Just ("Containers" Lude..= containers),
            Lude.Just
              ( "SupportedTransformInstanceTypes"
                  Lude..= supportedTransformInstanceTypes
              )
          ]
      )
