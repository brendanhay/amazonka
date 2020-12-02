{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.InferenceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.InferenceSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
import Network.AWS.SageMaker.Types.ProductionVariantInstanceType
import Network.AWS.SageMaker.Types.TransformInstanceType

-- | Defines how to perform inference generation after a training job is run.
--
--
--
-- /See:/ 'inferenceSpecification' smart constructor.
data InferenceSpecification = InferenceSpecification'
  { _isContainers ::
      !(List1 ModelPackageContainerDefinition),
    _isSupportedTransformInstanceTypes ::
      !(List1 TransformInstanceType),
    _isSupportedRealtimeInferenceInstanceTypes ::
      ![ProductionVariantInstanceType],
    _isSupportedContentTypes :: ![Text],
    _isSupportedResponseMIMETypes :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InferenceSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isContainers' - The Amazon ECR registry path of the Docker image that contains the inference code.
--
-- * 'isSupportedTransformInstanceTypes' - A list of the instance types on which a transformation job can be run or on which an endpoint can be deployed.
--
-- * 'isSupportedRealtimeInferenceInstanceTypes' - A list of the instance types that are used to generate inferences in real-time.
--
-- * 'isSupportedContentTypes' - The supported MIME types for the input data.
--
-- * 'isSupportedResponseMIMETypes' - The supported MIME types for the output data.
inferenceSpecification ::
  -- | 'isContainers'
  NonEmpty ModelPackageContainerDefinition ->
  -- | 'isSupportedTransformInstanceTypes'
  NonEmpty TransformInstanceType ->
  InferenceSpecification
inferenceSpecification
  pContainers_
  pSupportedTransformInstanceTypes_ =
    InferenceSpecification'
      { _isContainers = _List1 # pContainers_,
        _isSupportedTransformInstanceTypes =
          _List1 # pSupportedTransformInstanceTypes_,
        _isSupportedRealtimeInferenceInstanceTypes = mempty,
        _isSupportedContentTypes = mempty,
        _isSupportedResponseMIMETypes = mempty
      }

-- | The Amazon ECR registry path of the Docker image that contains the inference code.
isContainers :: Lens' InferenceSpecification (NonEmpty ModelPackageContainerDefinition)
isContainers = lens _isContainers (\s a -> s {_isContainers = a}) . _List1

-- | A list of the instance types on which a transformation job can be run or on which an endpoint can be deployed.
isSupportedTransformInstanceTypes :: Lens' InferenceSpecification (NonEmpty TransformInstanceType)
isSupportedTransformInstanceTypes = lens _isSupportedTransformInstanceTypes (\s a -> s {_isSupportedTransformInstanceTypes = a}) . _List1

-- | A list of the instance types that are used to generate inferences in real-time.
isSupportedRealtimeInferenceInstanceTypes :: Lens' InferenceSpecification [ProductionVariantInstanceType]
isSupportedRealtimeInferenceInstanceTypes = lens _isSupportedRealtimeInferenceInstanceTypes (\s a -> s {_isSupportedRealtimeInferenceInstanceTypes = a}) . _Coerce

-- | The supported MIME types for the input data.
isSupportedContentTypes :: Lens' InferenceSpecification [Text]
isSupportedContentTypes = lens _isSupportedContentTypes (\s a -> s {_isSupportedContentTypes = a}) . _Coerce

-- | The supported MIME types for the output data.
isSupportedResponseMIMETypes :: Lens' InferenceSpecification [Text]
isSupportedResponseMIMETypes = lens _isSupportedResponseMIMETypes (\s a -> s {_isSupportedResponseMIMETypes = a}) . _Coerce

instance FromJSON InferenceSpecification where
  parseJSON =
    withObject
      "InferenceSpecification"
      ( \x ->
          InferenceSpecification'
            <$> (x .: "Containers")
            <*> (x .: "SupportedTransformInstanceTypes")
            <*> (x .:? "SupportedRealtimeInferenceInstanceTypes" .!= mempty)
            <*> (x .:? "SupportedContentTypes" .!= mempty)
            <*> (x .:? "SupportedResponseMIMETypes" .!= mempty)
      )

instance Hashable InferenceSpecification

instance NFData InferenceSpecification

instance ToJSON InferenceSpecification where
  toJSON InferenceSpecification' {..} =
    object
      ( catMaybes
          [ Just ("Containers" .= _isContainers),
            Just
              ( "SupportedTransformInstanceTypes"
                  .= _isSupportedTransformInstanceTypes
              ),
            Just
              ( "SupportedRealtimeInferenceInstanceTypes"
                  .= _isSupportedRealtimeInferenceInstanceTypes
              ),
            Just ("SupportedContentTypes" .= _isSupportedContentTypes),
            Just
              ("SupportedResponseMIMETypes" .= _isSupportedResponseMIMETypes)
          ]
      )
