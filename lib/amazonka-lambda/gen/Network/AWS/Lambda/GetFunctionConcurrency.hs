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
-- Module      : Network.AWS.Lambda.GetFunctionConcurrency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the reserved concurrency configuration for a function. To set a concurrency limit for a function, use 'PutFunctionConcurrency' .
module Network.AWS.Lambda.GetFunctionConcurrency
  ( -- * Creating a Request
    getFunctionConcurrency,
    GetFunctionConcurrency,

    -- * Request Lenses
    gFunctionName,

    -- * Destructuring the Response
    getFunctionConcurrencyResponse,
    GetFunctionConcurrencyResponse,

    -- * Response Lenses
    gfcrsReservedConcurrentExecutions,
    gfcrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFunctionConcurrency' smart constructor.
newtype GetFunctionConcurrency = GetFunctionConcurrency'
  { _gFunctionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFunctionConcurrency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
getFunctionConcurrency ::
  -- | 'gFunctionName'
  Text ->
  GetFunctionConcurrency
getFunctionConcurrency pFunctionName_ =
  GetFunctionConcurrency' {_gFunctionName = pFunctionName_}

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
gFunctionName :: Lens' GetFunctionConcurrency Text
gFunctionName = lens _gFunctionName (\s a -> s {_gFunctionName = a})

instance AWSRequest GetFunctionConcurrency where
  type Rs GetFunctionConcurrency = GetFunctionConcurrencyResponse
  request = get lambda
  response =
    receiveJSON
      ( \s h x ->
          GetFunctionConcurrencyResponse'
            <$> (x .?> "ReservedConcurrentExecutions") <*> (pure (fromEnum s))
      )

instance Hashable GetFunctionConcurrency

instance NFData GetFunctionConcurrency

instance ToHeaders GetFunctionConcurrency where
  toHeaders = const mempty

instance ToPath GetFunctionConcurrency where
  toPath GetFunctionConcurrency' {..} =
    mconcat
      ["/2019-09-30/functions/", toBS _gFunctionName, "/concurrency"]

instance ToQuery GetFunctionConcurrency where
  toQuery = const mempty

-- | /See:/ 'getFunctionConcurrencyResponse' smart constructor.
data GetFunctionConcurrencyResponse = GetFunctionConcurrencyResponse'
  { _gfcrsReservedConcurrentExecutions ::
      !(Maybe Nat),
    _gfcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFunctionConcurrencyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfcrsReservedConcurrentExecutions' - The number of simultaneous executions that are reserved for the function.
--
-- * 'gfcrsResponseStatus' - -- | The response status code.
getFunctionConcurrencyResponse ::
  -- | 'gfcrsResponseStatus'
  Int ->
  GetFunctionConcurrencyResponse
getFunctionConcurrencyResponse pResponseStatus_ =
  GetFunctionConcurrencyResponse'
    { _gfcrsReservedConcurrentExecutions =
        Nothing,
      _gfcrsResponseStatus = pResponseStatus_
    }

-- | The number of simultaneous executions that are reserved for the function.
gfcrsReservedConcurrentExecutions :: Lens' GetFunctionConcurrencyResponse (Maybe Natural)
gfcrsReservedConcurrentExecutions = lens _gfcrsReservedConcurrentExecutions (\s a -> s {_gfcrsReservedConcurrentExecutions = a}) . mapping _Nat

-- | -- | The response status code.
gfcrsResponseStatus :: Lens' GetFunctionConcurrencyResponse Int
gfcrsResponseStatus = lens _gfcrsResponseStatus (\s a -> s {_gfcrsResponseStatus = a})

instance NFData GetFunctionConcurrencyResponse
