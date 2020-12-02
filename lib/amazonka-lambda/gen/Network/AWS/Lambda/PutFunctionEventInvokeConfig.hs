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
-- Module      : Network.AWS.Lambda.PutFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures options for <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html asynchronous invocation> on a function, version, or alias. If a configuration already exists for a function, version, or alias, this operation overwrites it. If you exclude any settings, they are removed. To set one option without affecting existing settings for other options, use 'UpdateFunctionEventInvokeConfig' .
--
--
-- By default, Lambda retries an asynchronous invocation twice if the function returns an error. It retains events in a queue for up to six hours. When an event fails all processing attempts or stays in the asynchronous invocation queue for too long, Lambda discards it. To retain discarded events, configure a dead-letter queue with 'UpdateFunctionConfiguration' .
--
-- To send an invocation record to a queue, topic, function, or event bus, specify a <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#invocation-async-destinations destination> . You can configure separate destinations for successful invocations (on-success) and events that fail all processing attempts (on-failure). You can configure destinations in addition to or instead of a dead-letter queue.
module Network.AWS.Lambda.PutFunctionEventInvokeConfig
  ( -- * Creating a Request
    putFunctionEventInvokeConfig,
    PutFunctionEventInvokeConfig,

    -- * Request Lenses
    pfeicMaximumEventAgeInSeconds,
    pfeicMaximumRetryAttempts,
    pfeicQualifier,
    pfeicDestinationConfig,
    pfeicFunctionName,

    -- * Destructuring the Response
    functionEventInvokeConfig,
    FunctionEventInvokeConfig,

    -- * Response Lenses
    feicFunctionARN,
    feicMaximumEventAgeInSeconds,
    feicMaximumRetryAttempts,
    feicLastModified,
    feicDestinationConfig,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putFunctionEventInvokeConfig' smart constructor.
data PutFunctionEventInvokeConfig = PutFunctionEventInvokeConfig'
  { _pfeicMaximumEventAgeInSeconds ::
      !(Maybe Nat),
    _pfeicMaximumRetryAttempts ::
      !(Maybe Nat),
    _pfeicQualifier :: !(Maybe Text),
    _pfeicDestinationConfig ::
      !(Maybe DestinationConfig),
    _pfeicFunctionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutFunctionEventInvokeConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfeicMaximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for processing.
--
-- * 'pfeicMaximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
--
-- * 'pfeicQualifier' - A version number or alias name.
--
-- * 'pfeicDestinationConfig' - A destination for events after they have been sent to a function for processing. __Destinations__      * __Function__ - The Amazon Resource Name (ARN) of a Lambda function.     * __Queue__ - The ARN of an SQS queue.     * __Topic__ - The ARN of an SNS topic.     * __Event Bus__ - The ARN of an Amazon EventBridge event bus.
--
-- * 'pfeicFunctionName' - The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
putFunctionEventInvokeConfig ::
  -- | 'pfeicFunctionName'
  Text ->
  PutFunctionEventInvokeConfig
putFunctionEventInvokeConfig pFunctionName_ =
  PutFunctionEventInvokeConfig'
    { _pfeicMaximumEventAgeInSeconds =
        Nothing,
      _pfeicMaximumRetryAttempts = Nothing,
      _pfeicQualifier = Nothing,
      _pfeicDestinationConfig = Nothing,
      _pfeicFunctionName = pFunctionName_
    }

-- | The maximum age of a request that Lambda sends to a function for processing.
pfeicMaximumEventAgeInSeconds :: Lens' PutFunctionEventInvokeConfig (Maybe Natural)
pfeicMaximumEventAgeInSeconds = lens _pfeicMaximumEventAgeInSeconds (\s a -> s {_pfeicMaximumEventAgeInSeconds = a}) . mapping _Nat

-- | The maximum number of times to retry when the function returns an error.
pfeicMaximumRetryAttempts :: Lens' PutFunctionEventInvokeConfig (Maybe Natural)
pfeicMaximumRetryAttempts = lens _pfeicMaximumRetryAttempts (\s a -> s {_pfeicMaximumRetryAttempts = a}) . mapping _Nat

-- | A version number or alias name.
pfeicQualifier :: Lens' PutFunctionEventInvokeConfig (Maybe Text)
pfeicQualifier = lens _pfeicQualifier (\s a -> s {_pfeicQualifier = a})

-- | A destination for events after they have been sent to a function for processing. __Destinations__      * __Function__ - The Amazon Resource Name (ARN) of a Lambda function.     * __Queue__ - The ARN of an SQS queue.     * __Topic__ - The ARN of an SNS topic.     * __Event Bus__ - The ARN of an Amazon EventBridge event bus.
pfeicDestinationConfig :: Lens' PutFunctionEventInvokeConfig (Maybe DestinationConfig)
pfeicDestinationConfig = lens _pfeicDestinationConfig (\s a -> s {_pfeicDestinationConfig = a})

-- | The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
pfeicFunctionName :: Lens' PutFunctionEventInvokeConfig Text
pfeicFunctionName = lens _pfeicFunctionName (\s a -> s {_pfeicFunctionName = a})

instance AWSRequest PutFunctionEventInvokeConfig where
  type Rs PutFunctionEventInvokeConfig = FunctionEventInvokeConfig
  request = putJSON lambda
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable PutFunctionEventInvokeConfig

instance NFData PutFunctionEventInvokeConfig

instance ToHeaders PutFunctionEventInvokeConfig where
  toHeaders = const mempty

instance ToJSON PutFunctionEventInvokeConfig where
  toJSON PutFunctionEventInvokeConfig' {..} =
    object
      ( catMaybes
          [ ("MaximumEventAgeInSeconds" .=)
              <$> _pfeicMaximumEventAgeInSeconds,
            ("MaximumRetryAttempts" .=) <$> _pfeicMaximumRetryAttempts,
            ("DestinationConfig" .=) <$> _pfeicDestinationConfig
          ]
      )

instance ToPath PutFunctionEventInvokeConfig where
  toPath PutFunctionEventInvokeConfig' {..} =
    mconcat
      [ "/2019-09-25/functions/",
        toBS _pfeicFunctionName,
        "/event-invoke-config"
      ]

instance ToQuery PutFunctionEventInvokeConfig where
  toQuery PutFunctionEventInvokeConfig' {..} =
    mconcat ["Qualifier" =: _pfeicQualifier]
