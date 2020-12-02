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
-- Module      : Network.AWS.SageMaker.DescribeFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified flow definition.
module Network.AWS.SageMaker.DescribeFlowDefinition
  ( -- * Creating a Request
    describeFlowDefinition,
    DescribeFlowDefinition,

    -- * Request Lenses
    dFlowDefinitionName,

    -- * Destructuring the Response
    describeFlowDefinitionResponse,
    DescribeFlowDefinitionResponse,

    -- * Response Lenses
    dfdfrsFailureReason,
    dfdfrsHumanLoopRequestSource,
    dfdfrsHumanLoopActivationConfig,
    dfdfrsResponseStatus,
    dfdfrsFlowDefinitionARN,
    dfdfrsFlowDefinitionName,
    dfdfrsFlowDefinitionStatus,
    dfdfrsCreationTime,
    dfdfrsHumanLoopConfig,
    dfdfrsOutputConfig,
    dfdfrsRoleARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'describeFlowDefinition' smart constructor.
newtype DescribeFlowDefinition = DescribeFlowDefinition'
  { _dFlowDefinitionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFlowDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dFlowDefinitionName' - The name of the flow definition.
describeFlowDefinition ::
  -- | 'dFlowDefinitionName'
  Text ->
  DescribeFlowDefinition
describeFlowDefinition pFlowDefinitionName_ =
  DescribeFlowDefinition'
    { _dFlowDefinitionName =
        pFlowDefinitionName_
    }

-- | The name of the flow definition.
dFlowDefinitionName :: Lens' DescribeFlowDefinition Text
dFlowDefinitionName = lens _dFlowDefinitionName (\s a -> s {_dFlowDefinitionName = a})

instance AWSRequest DescribeFlowDefinition where
  type Rs DescribeFlowDefinition = DescribeFlowDefinitionResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          DescribeFlowDefinitionResponse'
            <$> (x .?> "FailureReason")
            <*> (x .?> "HumanLoopRequestSource")
            <*> (x .?> "HumanLoopActivationConfig")
            <*> (pure (fromEnum s))
            <*> (x .:> "FlowDefinitionArn")
            <*> (x .:> "FlowDefinitionName")
            <*> (x .:> "FlowDefinitionStatus")
            <*> (x .:> "CreationTime")
            <*> (x .:> "HumanLoopConfig")
            <*> (x .:> "OutputConfig")
            <*> (x .:> "RoleArn")
      )

instance Hashable DescribeFlowDefinition

instance NFData DescribeFlowDefinition

instance ToHeaders DescribeFlowDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DescribeFlowDefinition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeFlowDefinition where
  toJSON DescribeFlowDefinition' {..} =
    object
      (catMaybes [Just ("FlowDefinitionName" .= _dFlowDefinitionName)])

instance ToPath DescribeFlowDefinition where
  toPath = const "/"

instance ToQuery DescribeFlowDefinition where
  toQuery = const mempty

-- | /See:/ 'describeFlowDefinitionResponse' smart constructor.
data DescribeFlowDefinitionResponse = DescribeFlowDefinitionResponse'
  { _dfdfrsFailureReason ::
      !(Maybe Text),
    _dfdfrsHumanLoopRequestSource ::
      !( Maybe
           HumanLoopRequestSource
       ),
    _dfdfrsHumanLoopActivationConfig ::
      !( Maybe
           HumanLoopActivationConfig
       ),
    _dfdfrsResponseStatus :: !Int,
    _dfdfrsFlowDefinitionARN ::
      !Text,
    _dfdfrsFlowDefinitionName ::
      !Text,
    _dfdfrsFlowDefinitionStatus ::
      !FlowDefinitionStatus,
    _dfdfrsCreationTime :: !POSIX,
    _dfdfrsHumanLoopConfig ::
      !HumanLoopConfig,
    _dfdfrsOutputConfig ::
      !FlowDefinitionOutputConfig,
    _dfdfrsRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFlowDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfdfrsFailureReason' - The reason your flow definition failed.
--
-- * 'dfdfrsHumanLoopRequestSource' - Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
--
-- * 'dfdfrsHumanLoopActivationConfig' - An object containing information about what triggers a human review workflow.
--
-- * 'dfdfrsResponseStatus' - -- | The response status code.
--
-- * 'dfdfrsFlowDefinitionARN' - The Amazon Resource Name (ARN) of the flow defintion.
--
-- * 'dfdfrsFlowDefinitionName' - The Amazon Resource Name (ARN) of the flow definition.
--
-- * 'dfdfrsFlowDefinitionStatus' - The status of the flow definition. Valid values are listed below.
--
-- * 'dfdfrsCreationTime' - The timestamp when the flow definition was created.
--
-- * 'dfdfrsHumanLoopConfig' - An object containing information about who works on the task, the workforce task price, and other task details.
--
-- * 'dfdfrsOutputConfig' - An object containing information about the output file.
--
-- * 'dfdfrsRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
describeFlowDefinitionResponse ::
  -- | 'dfdfrsResponseStatus'
  Int ->
  -- | 'dfdfrsFlowDefinitionARN'
  Text ->
  -- | 'dfdfrsFlowDefinitionName'
  Text ->
  -- | 'dfdfrsFlowDefinitionStatus'
  FlowDefinitionStatus ->
  -- | 'dfdfrsCreationTime'
  UTCTime ->
  -- | 'dfdfrsHumanLoopConfig'
  HumanLoopConfig ->
  -- | 'dfdfrsOutputConfig'
  FlowDefinitionOutputConfig ->
  -- | 'dfdfrsRoleARN'
  Text ->
  DescribeFlowDefinitionResponse
describeFlowDefinitionResponse
  pResponseStatus_
  pFlowDefinitionARN_
  pFlowDefinitionName_
  pFlowDefinitionStatus_
  pCreationTime_
  pHumanLoopConfig_
  pOutputConfig_
  pRoleARN_ =
    DescribeFlowDefinitionResponse'
      { _dfdfrsFailureReason = Nothing,
        _dfdfrsHumanLoopRequestSource = Nothing,
        _dfdfrsHumanLoopActivationConfig = Nothing,
        _dfdfrsResponseStatus = pResponseStatus_,
        _dfdfrsFlowDefinitionARN = pFlowDefinitionARN_,
        _dfdfrsFlowDefinitionName = pFlowDefinitionName_,
        _dfdfrsFlowDefinitionStatus = pFlowDefinitionStatus_,
        _dfdfrsCreationTime = _Time # pCreationTime_,
        _dfdfrsHumanLoopConfig = pHumanLoopConfig_,
        _dfdfrsOutputConfig = pOutputConfig_,
        _dfdfrsRoleARN = pRoleARN_
      }

-- | The reason your flow definition failed.
dfdfrsFailureReason :: Lens' DescribeFlowDefinitionResponse (Maybe Text)
dfdfrsFailureReason = lens _dfdfrsFailureReason (\s a -> s {_dfdfrsFailureReason = a})

-- | Container for configuring the source of human task requests. Used to specify if Amazon Rekognition or Amazon Textract is used as an integration source.
dfdfrsHumanLoopRequestSource :: Lens' DescribeFlowDefinitionResponse (Maybe HumanLoopRequestSource)
dfdfrsHumanLoopRequestSource = lens _dfdfrsHumanLoopRequestSource (\s a -> s {_dfdfrsHumanLoopRequestSource = a})

-- | An object containing information about what triggers a human review workflow.
dfdfrsHumanLoopActivationConfig :: Lens' DescribeFlowDefinitionResponse (Maybe HumanLoopActivationConfig)
dfdfrsHumanLoopActivationConfig = lens _dfdfrsHumanLoopActivationConfig (\s a -> s {_dfdfrsHumanLoopActivationConfig = a})

-- | -- | The response status code.
dfdfrsResponseStatus :: Lens' DescribeFlowDefinitionResponse Int
dfdfrsResponseStatus = lens _dfdfrsResponseStatus (\s a -> s {_dfdfrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the flow defintion.
dfdfrsFlowDefinitionARN :: Lens' DescribeFlowDefinitionResponse Text
dfdfrsFlowDefinitionARN = lens _dfdfrsFlowDefinitionARN (\s a -> s {_dfdfrsFlowDefinitionARN = a})

-- | The Amazon Resource Name (ARN) of the flow definition.
dfdfrsFlowDefinitionName :: Lens' DescribeFlowDefinitionResponse Text
dfdfrsFlowDefinitionName = lens _dfdfrsFlowDefinitionName (\s a -> s {_dfdfrsFlowDefinitionName = a})

-- | The status of the flow definition. Valid values are listed below.
dfdfrsFlowDefinitionStatus :: Lens' DescribeFlowDefinitionResponse FlowDefinitionStatus
dfdfrsFlowDefinitionStatus = lens _dfdfrsFlowDefinitionStatus (\s a -> s {_dfdfrsFlowDefinitionStatus = a})

-- | The timestamp when the flow definition was created.
dfdfrsCreationTime :: Lens' DescribeFlowDefinitionResponse UTCTime
dfdfrsCreationTime = lens _dfdfrsCreationTime (\s a -> s {_dfdfrsCreationTime = a}) . _Time

-- | An object containing information about who works on the task, the workforce task price, and other task details.
dfdfrsHumanLoopConfig :: Lens' DescribeFlowDefinitionResponse HumanLoopConfig
dfdfrsHumanLoopConfig = lens _dfdfrsHumanLoopConfig (\s a -> s {_dfdfrsHumanLoopConfig = a})

-- | An object containing information about the output file.
dfdfrsOutputConfig :: Lens' DescribeFlowDefinitionResponse FlowDefinitionOutputConfig
dfdfrsOutputConfig = lens _dfdfrsOutputConfig (\s a -> s {_dfdfrsOutputConfig = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) execution role for the flow definition.
dfdfrsRoleARN :: Lens' DescribeFlowDefinitionResponse Text
dfdfrsRoleARN = lens _dfdfrsRoleARN (\s a -> s {_dfdfrsRoleARN = a})

instance NFData DescribeFlowDefinitionResponse
