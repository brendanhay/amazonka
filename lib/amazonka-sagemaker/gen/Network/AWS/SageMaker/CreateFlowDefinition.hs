{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a flow definition.
module Network.AWS.SageMaker.CreateFlowDefinition
  ( -- * Creating a Request
    createFlowDefinition,
    CreateFlowDefinition,

    -- * Request Lenses
    cfdHumanLoopRequestSource,
    cfdHumanLoopActivationConfig,
    cfdTags,
    cfdFlowDefinitionName,
    cfdHumanLoopConfig,
    cfdOutputConfig,
    cfdRoleARN,

    -- * Destructuring the Response
    createFlowDefinitionResponse,
    CreateFlowDefinitionResponse,

    -- * Response Lenses
    cfdrsResponseStatus,
    cfdrsFlowDefinitionARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createFlowDefinition' smart constructor.
data CreateFlowDefinition = CreateFlowDefinition'
  { _cfdHumanLoopRequestSource ::
      !(Maybe HumanLoopRequestSource),
    _cfdHumanLoopActivationConfig ::
      !(Maybe HumanLoopActivationConfig),
    _cfdTags :: !(Maybe [Tag]),
    _cfdFlowDefinitionName :: !Text,
    _cfdHumanLoopConfig :: !HumanLoopConfig,
    _cfdOutputConfig :: !FlowDefinitionOutputConfig,
    _cfdRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFlowDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfdHumanLoopRequestSource' - Container for configuring the source of human task requests. Use to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
--
-- * 'cfdHumanLoopActivationConfig' - An object containing information about the events that trigger a human workflow.
--
-- * 'cfdTags' - An array of key-value pairs that contain metadata to help you categorize and organize a flow definition. Each tag consists of a key and a value, both of which you define.
--
-- * 'cfdFlowDefinitionName' - The name of your flow definition.
--
-- * 'cfdHumanLoopConfig' - An object containing information about the tasks the human reviewers will perform.
--
-- * 'cfdOutputConfig' - An object containing information about where the human review results will be uploaded.
--
-- * 'cfdRoleARN' - The Amazon Resource Name (ARN) of the role needed to call other services on your behalf. For example, @arn:aws:iam::1234567890:role/service-role/AmazonSageMaker-ExecutionRole-20180111T151298@ .
createFlowDefinition ::
  -- | 'cfdFlowDefinitionName'
  Text ->
  -- | 'cfdHumanLoopConfig'
  HumanLoopConfig ->
  -- | 'cfdOutputConfig'
  FlowDefinitionOutputConfig ->
  -- | 'cfdRoleARN'
  Text ->
  CreateFlowDefinition
createFlowDefinition
  pFlowDefinitionName_
  pHumanLoopConfig_
  pOutputConfig_
  pRoleARN_ =
    CreateFlowDefinition'
      { _cfdHumanLoopRequestSource = Nothing,
        _cfdHumanLoopActivationConfig = Nothing,
        _cfdTags = Nothing,
        _cfdFlowDefinitionName = pFlowDefinitionName_,
        _cfdHumanLoopConfig = pHumanLoopConfig_,
        _cfdOutputConfig = pOutputConfig_,
        _cfdRoleARN = pRoleARN_
      }

-- | Container for configuring the source of human task requests. Use to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
cfdHumanLoopRequestSource :: Lens' CreateFlowDefinition (Maybe HumanLoopRequestSource)
cfdHumanLoopRequestSource = lens _cfdHumanLoopRequestSource (\s a -> s {_cfdHumanLoopRequestSource = a})

-- | An object containing information about the events that trigger a human workflow.
cfdHumanLoopActivationConfig :: Lens' CreateFlowDefinition (Maybe HumanLoopActivationConfig)
cfdHumanLoopActivationConfig = lens _cfdHumanLoopActivationConfig (\s a -> s {_cfdHumanLoopActivationConfig = a})

-- | An array of key-value pairs that contain metadata to help you categorize and organize a flow definition. Each tag consists of a key and a value, both of which you define.
cfdTags :: Lens' CreateFlowDefinition [Tag]
cfdTags = lens _cfdTags (\s a -> s {_cfdTags = a}) . _Default . _Coerce

-- | The name of your flow definition.
cfdFlowDefinitionName :: Lens' CreateFlowDefinition Text
cfdFlowDefinitionName = lens _cfdFlowDefinitionName (\s a -> s {_cfdFlowDefinitionName = a})

-- | An object containing information about the tasks the human reviewers will perform.
cfdHumanLoopConfig :: Lens' CreateFlowDefinition HumanLoopConfig
cfdHumanLoopConfig = lens _cfdHumanLoopConfig (\s a -> s {_cfdHumanLoopConfig = a})

-- | An object containing information about where the human review results will be uploaded.
cfdOutputConfig :: Lens' CreateFlowDefinition FlowDefinitionOutputConfig
cfdOutputConfig = lens _cfdOutputConfig (\s a -> s {_cfdOutputConfig = a})

-- | The Amazon Resource Name (ARN) of the role needed to call other services on your behalf. For example, @arn:aws:iam::1234567890:role/service-role/AmazonSageMaker-ExecutionRole-20180111T151298@ .
cfdRoleARN :: Lens' CreateFlowDefinition Text
cfdRoleARN = lens _cfdRoleARN (\s a -> s {_cfdRoleARN = a})

instance AWSRequest CreateFlowDefinition where
  type Rs CreateFlowDefinition = CreateFlowDefinitionResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateFlowDefinitionResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "FlowDefinitionArn")
      )

instance Hashable CreateFlowDefinition

instance NFData CreateFlowDefinition

instance ToHeaders CreateFlowDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.CreateFlowDefinition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateFlowDefinition where
  toJSON CreateFlowDefinition' {..} =
    object
      ( catMaybes
          [ ("HumanLoopRequestSource" .=) <$> _cfdHumanLoopRequestSource,
            ("HumanLoopActivationConfig" .=) <$> _cfdHumanLoopActivationConfig,
            ("Tags" .=) <$> _cfdTags,
            Just ("FlowDefinitionName" .= _cfdFlowDefinitionName),
            Just ("HumanLoopConfig" .= _cfdHumanLoopConfig),
            Just ("OutputConfig" .= _cfdOutputConfig),
            Just ("RoleArn" .= _cfdRoleARN)
          ]
      )

instance ToPath CreateFlowDefinition where
  toPath = const "/"

instance ToQuery CreateFlowDefinition where
  toQuery = const mempty

-- | /See:/ 'createFlowDefinitionResponse' smart constructor.
data CreateFlowDefinitionResponse = CreateFlowDefinitionResponse'
  { _cfdrsResponseStatus ::
      !Int,
    _cfdrsFlowDefinitionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFlowDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfdrsResponseStatus' - -- | The response status code.
--
-- * 'cfdrsFlowDefinitionARN' - The Amazon Resource Name (ARN) of the flow definition you create.
createFlowDefinitionResponse ::
  -- | 'cfdrsResponseStatus'
  Int ->
  -- | 'cfdrsFlowDefinitionARN'
  Text ->
  CreateFlowDefinitionResponse
createFlowDefinitionResponse pResponseStatus_ pFlowDefinitionARN_ =
  CreateFlowDefinitionResponse'
    { _cfdrsResponseStatus =
        pResponseStatus_,
      _cfdrsFlowDefinitionARN = pFlowDefinitionARN_
    }

-- | -- | The response status code.
cfdrsResponseStatus :: Lens' CreateFlowDefinitionResponse Int
cfdrsResponseStatus = lens _cfdrsResponseStatus (\s a -> s {_cfdrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the flow definition you create.
cfdrsFlowDefinitionARN :: Lens' CreateFlowDefinitionResponse Text
cfdrsFlowDefinitionARN = lens _cfdrsFlowDefinitionARN (\s a -> s {_cfdrsFlowDefinitionARN = a})

instance NFData CreateFlowDefinitionResponse
