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
-- Module      : Network.AWS.Lambda.UpdateFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for asynchronous invocation for a function, version, or alias.
--
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
module Network.AWS.Lambda.UpdateFunctionEventInvokeConfig
  ( -- * Creating a Request
    updateFunctionEventInvokeConfig,
    UpdateFunctionEventInvokeConfig,

    -- * Request Lenses
    ufeicMaximumEventAgeInSeconds,
    ufeicMaximumRetryAttempts,
    ufeicQualifier,
    ufeicDestinationConfig,
    ufeicFunctionName,

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

-- | /See:/ 'updateFunctionEventInvokeConfig' smart constructor.
data UpdateFunctionEventInvokeConfig = UpdateFunctionEventInvokeConfig'
  { _ufeicMaximumEventAgeInSeconds ::
      !(Maybe Nat),
    _ufeicMaximumRetryAttempts ::
      !(Maybe Nat),
    _ufeicQualifier ::
      !(Maybe Text),
    _ufeicDestinationConfig ::
      !(Maybe DestinationConfig),
    _ufeicFunctionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFunctionEventInvokeConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufeicMaximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for processing.
--
-- * 'ufeicMaximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
--
-- * 'ufeicQualifier' - A version number or alias name.
--
-- * 'ufeicDestinationConfig' - A destination for events after they have been sent to a function for processing. __Destinations__      * __Function__ - The Amazon Resource Name (ARN) of a Lambda function.     * __Queue__ - The ARN of an SQS queue.     * __Topic__ - The ARN of an SNS topic.     * __Event Bus__ - The ARN of an Amazon EventBridge event bus.
--
-- * 'ufeicFunctionName' - The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
updateFunctionEventInvokeConfig ::
  -- | 'ufeicFunctionName'
  Text ->
  UpdateFunctionEventInvokeConfig
updateFunctionEventInvokeConfig pFunctionName_ =
  UpdateFunctionEventInvokeConfig'
    { _ufeicMaximumEventAgeInSeconds =
        Nothing,
      _ufeicMaximumRetryAttempts = Nothing,
      _ufeicQualifier = Nothing,
      _ufeicDestinationConfig = Nothing,
      _ufeicFunctionName = pFunctionName_
    }

-- | The maximum age of a request that Lambda sends to a function for processing.
ufeicMaximumEventAgeInSeconds :: Lens' UpdateFunctionEventInvokeConfig (Maybe Natural)
ufeicMaximumEventAgeInSeconds = lens _ufeicMaximumEventAgeInSeconds (\s a -> s {_ufeicMaximumEventAgeInSeconds = a}) . mapping _Nat

-- | The maximum number of times to retry when the function returns an error.
ufeicMaximumRetryAttempts :: Lens' UpdateFunctionEventInvokeConfig (Maybe Natural)
ufeicMaximumRetryAttempts = lens _ufeicMaximumRetryAttempts (\s a -> s {_ufeicMaximumRetryAttempts = a}) . mapping _Nat

-- | A version number or alias name.
ufeicQualifier :: Lens' UpdateFunctionEventInvokeConfig (Maybe Text)
ufeicQualifier = lens _ufeicQualifier (\s a -> s {_ufeicQualifier = a})

-- | A destination for events after they have been sent to a function for processing. __Destinations__      * __Function__ - The Amazon Resource Name (ARN) of a Lambda function.     * __Queue__ - The ARN of an SQS queue.     * __Topic__ - The ARN of an SNS topic.     * __Event Bus__ - The ARN of an Amazon EventBridge event bus.
ufeicDestinationConfig :: Lens' UpdateFunctionEventInvokeConfig (Maybe DestinationConfig)
ufeicDestinationConfig = lens _ufeicDestinationConfig (\s a -> s {_ufeicDestinationConfig = a})

-- | The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
ufeicFunctionName :: Lens' UpdateFunctionEventInvokeConfig Text
ufeicFunctionName = lens _ufeicFunctionName (\s a -> s {_ufeicFunctionName = a})

instance AWSRequest UpdateFunctionEventInvokeConfig where
  type Rs UpdateFunctionEventInvokeConfig = FunctionEventInvokeConfig
  request = postJSON lambda
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable UpdateFunctionEventInvokeConfig

instance NFData UpdateFunctionEventInvokeConfig

instance ToHeaders UpdateFunctionEventInvokeConfig where
  toHeaders = const mempty

instance ToJSON UpdateFunctionEventInvokeConfig where
  toJSON UpdateFunctionEventInvokeConfig' {..} =
    object
      ( catMaybes
          [ ("MaximumEventAgeInSeconds" .=)
              <$> _ufeicMaximumEventAgeInSeconds,
            ("MaximumRetryAttempts" .=) <$> _ufeicMaximumRetryAttempts,
            ("DestinationConfig" .=) <$> _ufeicDestinationConfig
          ]
      )

instance ToPath UpdateFunctionEventInvokeConfig where
  toPath UpdateFunctionEventInvokeConfig' {..} =
    mconcat
      [ "/2019-09-25/functions/",
        toBS _ufeicFunctionName,
        "/event-invoke-config"
      ]

instance ToQuery UpdateFunctionEventInvokeConfig where
  toQuery UpdateFunctionEventInvokeConfig' {..} =
    mconcat ["Qualifier" =: _ufeicQualifier]
