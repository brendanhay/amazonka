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
-- Module      : Network.AWS.Lambda.GetFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration for asynchronous invocation for a function, version, or alias.
--
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
module Network.AWS.Lambda.GetFunctionEventInvokeConfig
  ( -- * Creating a Request
    getFunctionEventInvokeConfig,
    GetFunctionEventInvokeConfig,

    -- * Request Lenses
    gfeicQualifier,
    gfeicFunctionName,

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

-- | /See:/ 'getFunctionEventInvokeConfig' smart constructor.
data GetFunctionEventInvokeConfig = GetFunctionEventInvokeConfig'
  { _gfeicQualifier ::
      !(Maybe Text),
    _gfeicFunctionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFunctionEventInvokeConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfeicQualifier' - A version number or alias name.
--
-- * 'gfeicFunctionName' - The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
getFunctionEventInvokeConfig ::
  -- | 'gfeicFunctionName'
  Text ->
  GetFunctionEventInvokeConfig
getFunctionEventInvokeConfig pFunctionName_ =
  GetFunctionEventInvokeConfig'
    { _gfeicQualifier = Nothing,
      _gfeicFunctionName = pFunctionName_
    }

-- | A version number or alias name.
gfeicQualifier :: Lens' GetFunctionEventInvokeConfig (Maybe Text)
gfeicQualifier = lens _gfeicQualifier (\s a -> s {_gfeicQualifier = a})

-- | The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
gfeicFunctionName :: Lens' GetFunctionEventInvokeConfig Text
gfeicFunctionName = lens _gfeicFunctionName (\s a -> s {_gfeicFunctionName = a})

instance AWSRequest GetFunctionEventInvokeConfig where
  type Rs GetFunctionEventInvokeConfig = FunctionEventInvokeConfig
  request = get lambda
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable GetFunctionEventInvokeConfig

instance NFData GetFunctionEventInvokeConfig

instance ToHeaders GetFunctionEventInvokeConfig where
  toHeaders = const mempty

instance ToPath GetFunctionEventInvokeConfig where
  toPath GetFunctionEventInvokeConfig' {..} =
    mconcat
      [ "/2019-09-25/functions/",
        toBS _gfeicFunctionName,
        "/event-invoke-config"
      ]

instance ToQuery GetFunctionEventInvokeConfig where
  toQuery GetFunctionEventInvokeConfig' {..} =
    mconcat ["Qualifier" =: _gfeicQualifier]
