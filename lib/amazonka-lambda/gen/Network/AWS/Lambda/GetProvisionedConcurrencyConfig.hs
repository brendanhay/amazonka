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
-- Module      : Network.AWS.Lambda.GetProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the provisioned concurrency configuration for a function's alias or version.
module Network.AWS.Lambda.GetProvisionedConcurrencyConfig
  ( -- * Creating a Request
    getProvisionedConcurrencyConfig,
    GetProvisionedConcurrencyConfig,

    -- * Request Lenses
    gpccFunctionName,
    gpccQualifier,

    -- * Destructuring the Response
    getProvisionedConcurrencyConfigResponse,
    GetProvisionedConcurrencyConfigResponse,

    -- * Response Lenses
    gpccrsStatus,
    gpccrsRequestedProvisionedConcurrentExecutions,
    gpccrsAvailableProvisionedConcurrentExecutions,
    gpccrsStatusReason,
    gpccrsAllocatedProvisionedConcurrentExecutions,
    gpccrsLastModified,
    gpccrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getProvisionedConcurrencyConfig' smart constructor.
data GetProvisionedConcurrencyConfig = GetProvisionedConcurrencyConfig'
  { _gpccFunctionName ::
      !Text,
    _gpccQualifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetProvisionedConcurrencyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpccFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'gpccQualifier' - The version number or alias name.
getProvisionedConcurrencyConfig ::
  -- | 'gpccFunctionName'
  Text ->
  -- | 'gpccQualifier'
  Text ->
  GetProvisionedConcurrencyConfig
getProvisionedConcurrencyConfig pFunctionName_ pQualifier_ =
  GetProvisionedConcurrencyConfig'
    { _gpccFunctionName =
        pFunctionName_,
      _gpccQualifier = pQualifier_
    }

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
gpccFunctionName :: Lens' GetProvisionedConcurrencyConfig Text
gpccFunctionName = lens _gpccFunctionName (\s a -> s {_gpccFunctionName = a})

-- | The version number or alias name.
gpccQualifier :: Lens' GetProvisionedConcurrencyConfig Text
gpccQualifier = lens _gpccQualifier (\s a -> s {_gpccQualifier = a})

instance AWSRequest GetProvisionedConcurrencyConfig where
  type
    Rs GetProvisionedConcurrencyConfig =
      GetProvisionedConcurrencyConfigResponse
  request = get lambda
  response =
    receiveJSON
      ( \s h x ->
          GetProvisionedConcurrencyConfigResponse'
            <$> (x .?> "Status")
            <*> (x .?> "RequestedProvisionedConcurrentExecutions")
            <*> (x .?> "AvailableProvisionedConcurrentExecutions")
            <*> (x .?> "StatusReason")
            <*> (x .?> "AllocatedProvisionedConcurrentExecutions")
            <*> (x .?> "LastModified")
            <*> (pure (fromEnum s))
      )

instance Hashable GetProvisionedConcurrencyConfig

instance NFData GetProvisionedConcurrencyConfig

instance ToHeaders GetProvisionedConcurrencyConfig where
  toHeaders = const mempty

instance ToPath GetProvisionedConcurrencyConfig where
  toPath GetProvisionedConcurrencyConfig' {..} =
    mconcat
      [ "/2019-09-30/functions/",
        toBS _gpccFunctionName,
        "/provisioned-concurrency"
      ]

instance ToQuery GetProvisionedConcurrencyConfig where
  toQuery GetProvisionedConcurrencyConfig' {..} =
    mconcat ["Qualifier" =: _gpccQualifier]

-- | /See:/ 'getProvisionedConcurrencyConfigResponse' smart constructor.
data GetProvisionedConcurrencyConfigResponse = GetProvisionedConcurrencyConfigResponse'
  { _gpccrsStatus ::
      !( Maybe
           ProvisionedConcurrencyStatusEnum
       ),
    _gpccrsRequestedProvisionedConcurrentExecutions ::
      !( Maybe
           Nat
       ),
    _gpccrsAvailableProvisionedConcurrentExecutions ::
      !( Maybe
           Nat
       ),
    _gpccrsStatusReason ::
      !( Maybe
           Text
       ),
    _gpccrsAllocatedProvisionedConcurrentExecutions ::
      !( Maybe
           Nat
       ),
    _gpccrsLastModified ::
      !( Maybe
           Text
       ),
    _gpccrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetProvisionedConcurrencyConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpccrsStatus' - The status of the allocation process.
--
-- * 'gpccrsRequestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
--
-- * 'gpccrsAvailableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
--
-- * 'gpccrsStatusReason' - For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- * 'gpccrsAllocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated.
--
-- * 'gpccrsLastModified' - The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- * 'gpccrsResponseStatus' - -- | The response status code.
getProvisionedConcurrencyConfigResponse ::
  -- | 'gpccrsResponseStatus'
  Int ->
  GetProvisionedConcurrencyConfigResponse
getProvisionedConcurrencyConfigResponse pResponseStatus_ =
  GetProvisionedConcurrencyConfigResponse'
    { _gpccrsStatus = Nothing,
      _gpccrsRequestedProvisionedConcurrentExecutions =
        Nothing,
      _gpccrsAvailableProvisionedConcurrentExecutions =
        Nothing,
      _gpccrsStatusReason = Nothing,
      _gpccrsAllocatedProvisionedConcurrentExecutions =
        Nothing,
      _gpccrsLastModified = Nothing,
      _gpccrsResponseStatus = pResponseStatus_
    }

-- | The status of the allocation process.
gpccrsStatus :: Lens' GetProvisionedConcurrencyConfigResponse (Maybe ProvisionedConcurrencyStatusEnum)
gpccrsStatus = lens _gpccrsStatus (\s a -> s {_gpccrsStatus = a})

-- | The amount of provisioned concurrency requested.
gpccrsRequestedProvisionedConcurrentExecutions :: Lens' GetProvisionedConcurrencyConfigResponse (Maybe Natural)
gpccrsRequestedProvisionedConcurrentExecutions = lens _gpccrsRequestedProvisionedConcurrentExecutions (\s a -> s {_gpccrsRequestedProvisionedConcurrentExecutions = a}) . mapping _Nat

-- | The amount of provisioned concurrency available.
gpccrsAvailableProvisionedConcurrentExecutions :: Lens' GetProvisionedConcurrencyConfigResponse (Maybe Natural)
gpccrsAvailableProvisionedConcurrentExecutions = lens _gpccrsAvailableProvisionedConcurrentExecutions (\s a -> s {_gpccrsAvailableProvisionedConcurrentExecutions = a}) . mapping _Nat

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
gpccrsStatusReason :: Lens' GetProvisionedConcurrencyConfigResponse (Maybe Text)
gpccrsStatusReason = lens _gpccrsStatusReason (\s a -> s {_gpccrsStatusReason = a})

-- | The amount of provisioned concurrency allocated.
gpccrsAllocatedProvisionedConcurrentExecutions :: Lens' GetProvisionedConcurrencyConfigResponse (Maybe Natural)
gpccrsAllocatedProvisionedConcurrentExecutions = lens _gpccrsAllocatedProvisionedConcurrentExecutions (\s a -> s {_gpccrsAllocatedProvisionedConcurrentExecutions = a}) . mapping _Nat

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
gpccrsLastModified :: Lens' GetProvisionedConcurrencyConfigResponse (Maybe Text)
gpccrsLastModified = lens _gpccrsLastModified (\s a -> s {_gpccrsLastModified = a})

-- | -- | The response status code.
gpccrsResponseStatus :: Lens' GetProvisionedConcurrencyConfigResponse Int
gpccrsResponseStatus = lens _gpccrsResponseStatus (\s a -> s {_gpccrsResponseStatus = a})

instance NFData GetProvisionedConcurrencyConfigResponse
