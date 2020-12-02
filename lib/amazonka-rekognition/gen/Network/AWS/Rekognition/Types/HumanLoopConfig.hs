{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.HumanLoopConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.HumanLoopDataAttributes

-- | Sets up the flow definition the image will be sent to if one of the conditions is met. You can also set certain attributes of the image before review.
--
--
--
-- /See:/ 'humanLoopConfig' smart constructor.
data HumanLoopConfig = HumanLoopConfig'
  { _hlcDataAttributes ::
      !(Maybe HumanLoopDataAttributes),
    _hlcHumanLoopName :: !Text,
    _hlcFlowDefinitionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HumanLoopConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlcDataAttributes' - Sets attributes of the input data.
--
-- * 'hlcHumanLoopName' - The name of the human review used for this image. This should be kept unique within a region.
--
-- * 'hlcFlowDefinitionARN' - The Amazon Resource Name (ARN) of the flow definition. You can create a flow definition by using the Amazon Sagemaker <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition> Operation.
humanLoopConfig ::
  -- | 'hlcHumanLoopName'
  Text ->
  -- | 'hlcFlowDefinitionARN'
  Text ->
  HumanLoopConfig
humanLoopConfig pHumanLoopName_ pFlowDefinitionARN_ =
  HumanLoopConfig'
    { _hlcDataAttributes = Nothing,
      _hlcHumanLoopName = pHumanLoopName_,
      _hlcFlowDefinitionARN = pFlowDefinitionARN_
    }

-- | Sets attributes of the input data.
hlcDataAttributes :: Lens' HumanLoopConfig (Maybe HumanLoopDataAttributes)
hlcDataAttributes = lens _hlcDataAttributes (\s a -> s {_hlcDataAttributes = a})

-- | The name of the human review used for this image. This should be kept unique within a region.
hlcHumanLoopName :: Lens' HumanLoopConfig Text
hlcHumanLoopName = lens _hlcHumanLoopName (\s a -> s {_hlcHumanLoopName = a})

-- | The Amazon Resource Name (ARN) of the flow definition. You can create a flow definition by using the Amazon Sagemaker <https://docs.aws.amazon.com/sagemaker/latest/dg/API_CreateFlowDefinition.html CreateFlowDefinition> Operation.
hlcFlowDefinitionARN :: Lens' HumanLoopConfig Text
hlcFlowDefinitionARN = lens _hlcFlowDefinitionARN (\s a -> s {_hlcFlowDefinitionARN = a})

instance Hashable HumanLoopConfig

instance NFData HumanLoopConfig

instance ToJSON HumanLoopConfig where
  toJSON HumanLoopConfig' {..} =
    object
      ( catMaybes
          [ ("DataAttributes" .=) <$> _hlcDataAttributes,
            Just ("HumanLoopName" .= _hlcHumanLoopName),
            Just ("FlowDefinitionArn" .= _hlcFlowDefinitionARN)
          ]
      )
