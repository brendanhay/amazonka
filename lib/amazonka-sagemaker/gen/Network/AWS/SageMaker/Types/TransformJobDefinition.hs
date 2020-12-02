{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJobDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.BatchStrategy
import Network.AWS.SageMaker.Types.TransformInput
import Network.AWS.SageMaker.Types.TransformOutput
import Network.AWS.SageMaker.Types.TransformResources

-- | Defines the input needed to run a transform job using the inference specification specified in the algorithm.
--
--
--
-- /See:/ 'transformJobDefinition' smart constructor.
data TransformJobDefinition = TransformJobDefinition'
  { _tjdBatchStrategy ::
      !(Maybe BatchStrategy),
    _tjdMaxPayloadInMB :: !(Maybe Nat),
    _tjdEnvironment :: !(Maybe (Map Text (Text))),
    _tjdMaxConcurrentTransforms :: !(Maybe Nat),
    _tjdTransformInput :: !TransformInput,
    _tjdTransformOutput :: !TransformOutput,
    _tjdTransformResources :: !TransformResources
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformJobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjdBatchStrategy' - A string that determines the number of records included in a single mini-batch. @SingleRecord@ means only one record is used per mini-batch. @MultiRecord@ means a mini-batch is set to contain as many records that can fit within the @MaxPayloadInMB@ limit.
--
-- * 'tjdMaxPayloadInMB' - The maximum payload size allowed, in MB. A payload is the data portion of a record (without metadata).
--
-- * 'tjdEnvironment' - The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- * 'tjdMaxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each instance in a transform job. The default value is 1.
--
-- * 'tjdTransformInput' - A description of the input source and the way the transform job consumes it.
--
-- * 'tjdTransformOutput' - Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
--
-- * 'tjdTransformResources' - Identifies the ML compute instances for the transform job.
transformJobDefinition ::
  -- | 'tjdTransformInput'
  TransformInput ->
  -- | 'tjdTransformOutput'
  TransformOutput ->
  -- | 'tjdTransformResources'
  TransformResources ->
  TransformJobDefinition
transformJobDefinition
  pTransformInput_
  pTransformOutput_
  pTransformResources_ =
    TransformJobDefinition'
      { _tjdBatchStrategy = Nothing,
        _tjdMaxPayloadInMB = Nothing,
        _tjdEnvironment = Nothing,
        _tjdMaxConcurrentTransforms = Nothing,
        _tjdTransformInput = pTransformInput_,
        _tjdTransformOutput = pTransformOutput_,
        _tjdTransformResources = pTransformResources_
      }

-- | A string that determines the number of records included in a single mini-batch. @SingleRecord@ means only one record is used per mini-batch. @MultiRecord@ means a mini-batch is set to contain as many records that can fit within the @MaxPayloadInMB@ limit.
tjdBatchStrategy :: Lens' TransformJobDefinition (Maybe BatchStrategy)
tjdBatchStrategy = lens _tjdBatchStrategy (\s a -> s {_tjdBatchStrategy = a})

-- | The maximum payload size allowed, in MB. A payload is the data portion of a record (without metadata).
tjdMaxPayloadInMB :: Lens' TransformJobDefinition (Maybe Natural)
tjdMaxPayloadInMB = lens _tjdMaxPayloadInMB (\s a -> s {_tjdMaxPayloadInMB = a}) . mapping _Nat

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
tjdEnvironment :: Lens' TransformJobDefinition (HashMap Text (Text))
tjdEnvironment = lens _tjdEnvironment (\s a -> s {_tjdEnvironment = a}) . _Default . _Map

-- | The maximum number of parallel requests that can be sent to each instance in a transform job. The default value is 1.
tjdMaxConcurrentTransforms :: Lens' TransformJobDefinition (Maybe Natural)
tjdMaxConcurrentTransforms = lens _tjdMaxConcurrentTransforms (\s a -> s {_tjdMaxConcurrentTransforms = a}) . mapping _Nat

-- | A description of the input source and the way the transform job consumes it.
tjdTransformInput :: Lens' TransformJobDefinition TransformInput
tjdTransformInput = lens _tjdTransformInput (\s a -> s {_tjdTransformInput = a})

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
tjdTransformOutput :: Lens' TransformJobDefinition TransformOutput
tjdTransformOutput = lens _tjdTransformOutput (\s a -> s {_tjdTransformOutput = a})

-- | Identifies the ML compute instances for the transform job.
tjdTransformResources :: Lens' TransformJobDefinition TransformResources
tjdTransformResources = lens _tjdTransformResources (\s a -> s {_tjdTransformResources = a})

instance FromJSON TransformJobDefinition where
  parseJSON =
    withObject
      "TransformJobDefinition"
      ( \x ->
          TransformJobDefinition'
            <$> (x .:? "BatchStrategy")
            <*> (x .:? "MaxPayloadInMB")
            <*> (x .:? "Environment" .!= mempty)
            <*> (x .:? "MaxConcurrentTransforms")
            <*> (x .: "TransformInput")
            <*> (x .: "TransformOutput")
            <*> (x .: "TransformResources")
      )

instance Hashable TransformJobDefinition

instance NFData TransformJobDefinition

instance ToJSON TransformJobDefinition where
  toJSON TransformJobDefinition' {..} =
    object
      ( catMaybes
          [ ("BatchStrategy" .=) <$> _tjdBatchStrategy,
            ("MaxPayloadInMB" .=) <$> _tjdMaxPayloadInMB,
            ("Environment" .=) <$> _tjdEnvironment,
            ("MaxConcurrentTransforms" .=) <$> _tjdMaxConcurrentTransforms,
            Just ("TransformInput" .= _tjdTransformInput),
            Just ("TransformOutput" .= _tjdTransformOutput),
            Just ("TransformResources" .= _tjdTransformResources)
          ]
      )
