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
-- Module      : Network.AWS.Lambda.PutProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a provisioned concurrency configuration to a function's alias or version.
module Network.AWS.Lambda.PutProvisionedConcurrencyConfig
  ( -- * Creating a Request
    putProvisionedConcurrencyConfig,
    PutProvisionedConcurrencyConfig,

    -- * Request Lenses
    ppccFunctionName,
    ppccQualifier,
    ppccProvisionedConcurrentExecutions,

    -- * Destructuring the Response
    putProvisionedConcurrencyConfigResponse,
    PutProvisionedConcurrencyConfigResponse,

    -- * Response Lenses
    ppccrsStatus,
    ppccrsRequestedProvisionedConcurrentExecutions,
    ppccrsAvailableProvisionedConcurrentExecutions,
    ppccrsStatusReason,
    ppccrsAllocatedProvisionedConcurrentExecutions,
    ppccrsLastModified,
    ppccrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putProvisionedConcurrencyConfig' smart constructor.
data PutProvisionedConcurrencyConfig = PutProvisionedConcurrencyConfig'
  { _ppccFunctionName ::
      !Text,
    _ppccQualifier :: !Text,
    _ppccProvisionedConcurrentExecutions ::
      !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutProvisionedConcurrencyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppccFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'ppccQualifier' - The version number or alias name.
--
-- * 'ppccProvisionedConcurrentExecutions' - The amount of provisioned concurrency to allocate for the version or alias.
putProvisionedConcurrencyConfig ::
  -- | 'ppccFunctionName'
  Text ->
  -- | 'ppccQualifier'
  Text ->
  -- | 'ppccProvisionedConcurrentExecutions'
  Natural ->
  PutProvisionedConcurrencyConfig
putProvisionedConcurrencyConfig
  pFunctionName_
  pQualifier_
  pProvisionedConcurrentExecutions_ =
    PutProvisionedConcurrencyConfig'
      { _ppccFunctionName =
          pFunctionName_,
        _ppccQualifier = pQualifier_,
        _ppccProvisionedConcurrentExecutions =
          _Nat # pProvisionedConcurrentExecutions_
      }

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
ppccFunctionName :: Lens' PutProvisionedConcurrencyConfig Text
ppccFunctionName = lens _ppccFunctionName (\s a -> s {_ppccFunctionName = a})

-- | The version number or alias name.
ppccQualifier :: Lens' PutProvisionedConcurrencyConfig Text
ppccQualifier = lens _ppccQualifier (\s a -> s {_ppccQualifier = a})

-- | The amount of provisioned concurrency to allocate for the version or alias.
ppccProvisionedConcurrentExecutions :: Lens' PutProvisionedConcurrencyConfig Natural
ppccProvisionedConcurrentExecutions = lens _ppccProvisionedConcurrentExecutions (\s a -> s {_ppccProvisionedConcurrentExecutions = a}) . _Nat

instance AWSRequest PutProvisionedConcurrencyConfig where
  type
    Rs PutProvisionedConcurrencyConfig =
      PutProvisionedConcurrencyConfigResponse
  request = putJSON lambda
  response =
    receiveJSON
      ( \s h x ->
          PutProvisionedConcurrencyConfigResponse'
            <$> (x .?> "Status")
            <*> (x .?> "RequestedProvisionedConcurrentExecutions")
            <*> (x .?> "AvailableProvisionedConcurrentExecutions")
            <*> (x .?> "StatusReason")
            <*> (x .?> "AllocatedProvisionedConcurrentExecutions")
            <*> (x .?> "LastModified")
            <*> (pure (fromEnum s))
      )

instance Hashable PutProvisionedConcurrencyConfig

instance NFData PutProvisionedConcurrencyConfig

instance ToHeaders PutProvisionedConcurrencyConfig where
  toHeaders = const mempty

instance ToJSON PutProvisionedConcurrencyConfig where
  toJSON PutProvisionedConcurrencyConfig' {..} =
    object
      ( catMaybes
          [ Just
              ( "ProvisionedConcurrentExecutions"
                  .= _ppccProvisionedConcurrentExecutions
              )
          ]
      )

instance ToPath PutProvisionedConcurrencyConfig where
  toPath PutProvisionedConcurrencyConfig' {..} =
    mconcat
      [ "/2019-09-30/functions/",
        toBS _ppccFunctionName,
        "/provisioned-concurrency"
      ]

instance ToQuery PutProvisionedConcurrencyConfig where
  toQuery PutProvisionedConcurrencyConfig' {..} =
    mconcat ["Qualifier" =: _ppccQualifier]

-- | /See:/ 'putProvisionedConcurrencyConfigResponse' smart constructor.
data PutProvisionedConcurrencyConfigResponse = PutProvisionedConcurrencyConfigResponse'
  { _ppccrsStatus ::
      !( Maybe
           ProvisionedConcurrencyStatusEnum
       ),
    _ppccrsRequestedProvisionedConcurrentExecutions ::
      !( Maybe
           Nat
       ),
    _ppccrsAvailableProvisionedConcurrentExecutions ::
      !( Maybe
           Nat
       ),
    _ppccrsStatusReason ::
      !( Maybe
           Text
       ),
    _ppccrsAllocatedProvisionedConcurrentExecutions ::
      !( Maybe
           Nat
       ),
    _ppccrsLastModified ::
      !( Maybe
           Text
       ),
    _ppccrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutProvisionedConcurrencyConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppccrsStatus' - The status of the allocation process.
--
-- * 'ppccrsRequestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
--
-- * 'ppccrsAvailableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
--
-- * 'ppccrsStatusReason' - For failed allocations, the reason that provisioned concurrency could not be allocated.
--
-- * 'ppccrsAllocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated.
--
-- * 'ppccrsLastModified' - The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
--
-- * 'ppccrsResponseStatus' - -- | The response status code.
putProvisionedConcurrencyConfigResponse ::
  -- | 'ppccrsResponseStatus'
  Int ->
  PutProvisionedConcurrencyConfigResponse
putProvisionedConcurrencyConfigResponse pResponseStatus_ =
  PutProvisionedConcurrencyConfigResponse'
    { _ppccrsStatus = Nothing,
      _ppccrsRequestedProvisionedConcurrentExecutions =
        Nothing,
      _ppccrsAvailableProvisionedConcurrentExecutions =
        Nothing,
      _ppccrsStatusReason = Nothing,
      _ppccrsAllocatedProvisionedConcurrentExecutions =
        Nothing,
      _ppccrsLastModified = Nothing,
      _ppccrsResponseStatus = pResponseStatus_
    }

-- | The status of the allocation process.
ppccrsStatus :: Lens' PutProvisionedConcurrencyConfigResponse (Maybe ProvisionedConcurrencyStatusEnum)
ppccrsStatus = lens _ppccrsStatus (\s a -> s {_ppccrsStatus = a})

-- | The amount of provisioned concurrency requested.
ppccrsRequestedProvisionedConcurrentExecutions :: Lens' PutProvisionedConcurrencyConfigResponse (Maybe Natural)
ppccrsRequestedProvisionedConcurrentExecutions = lens _ppccrsRequestedProvisionedConcurrentExecutions (\s a -> s {_ppccrsRequestedProvisionedConcurrentExecutions = a}) . mapping _Nat

-- | The amount of provisioned concurrency available.
ppccrsAvailableProvisionedConcurrentExecutions :: Lens' PutProvisionedConcurrencyConfigResponse (Maybe Natural)
ppccrsAvailableProvisionedConcurrentExecutions = lens _ppccrsAvailableProvisionedConcurrentExecutions (\s a -> s {_ppccrsAvailableProvisionedConcurrentExecutions = a}) . mapping _Nat

-- | For failed allocations, the reason that provisioned concurrency could not be allocated.
ppccrsStatusReason :: Lens' PutProvisionedConcurrencyConfigResponse (Maybe Text)
ppccrsStatusReason = lens _ppccrsStatusReason (\s a -> s {_ppccrsStatusReason = a})

-- | The amount of provisioned concurrency allocated.
ppccrsAllocatedProvisionedConcurrentExecutions :: Lens' PutProvisionedConcurrencyConfigResponse (Maybe Natural)
ppccrsAllocatedProvisionedConcurrentExecutions = lens _ppccrsAllocatedProvisionedConcurrentExecutions (\s a -> s {_ppccrsAllocatedProvisionedConcurrentExecutions = a}) . mapping _Nat

-- | The date and time that a user last updated the configuration, in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format> .
ppccrsLastModified :: Lens' PutProvisionedConcurrencyConfigResponse (Maybe Text)
ppccrsLastModified = lens _ppccrsLastModified (\s a -> s {_ppccrsLastModified = a})

-- | -- | The response status code.
ppccrsResponseStatus :: Lens' PutProvisionedConcurrencyConfigResponse Int
ppccrsResponseStatus = lens _ppccrsResponseStatus (\s a -> s {_ppccrsResponseStatus = a})

instance NFData PutProvisionedConcurrencyConfigResponse
