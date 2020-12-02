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
-- Module      : Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration for asynchronous invocation for a function, version, or alias.
--
--
-- To configure options for asynchronous invocation, use 'PutFunctionEventInvokeConfig' .
module Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
  ( -- * Creating a Request
    deleteFunctionEventInvokeConfig,
    DeleteFunctionEventInvokeConfig,

    -- * Request Lenses
    dfeicQualifier,
    dfeicFunctionName,

    -- * Destructuring the Response
    deleteFunctionEventInvokeConfigResponse,
    DeleteFunctionEventInvokeConfigResponse,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFunctionEventInvokeConfig' smart constructor.
data DeleteFunctionEventInvokeConfig = DeleteFunctionEventInvokeConfig'
  { _dfeicQualifier ::
      !(Maybe Text),
    _dfeicFunctionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFunctionEventInvokeConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfeicQualifier' - A version number or alias name.
--
-- * 'dfeicFunctionName' - The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
deleteFunctionEventInvokeConfig ::
  -- | 'dfeicFunctionName'
  Text ->
  DeleteFunctionEventInvokeConfig
deleteFunctionEventInvokeConfig pFunctionName_ =
  DeleteFunctionEventInvokeConfig'
    { _dfeicQualifier = Nothing,
      _dfeicFunctionName = pFunctionName_
    }

-- | A version number or alias name.
dfeicQualifier :: Lens' DeleteFunctionEventInvokeConfig (Maybe Text)
dfeicQualifier = lens _dfeicQualifier (\s a -> s {_dfeicQualifier = a})

-- | The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
dfeicFunctionName :: Lens' DeleteFunctionEventInvokeConfig Text
dfeicFunctionName = lens _dfeicFunctionName (\s a -> s {_dfeicFunctionName = a})

instance AWSRequest DeleteFunctionEventInvokeConfig where
  type
    Rs DeleteFunctionEventInvokeConfig =
      DeleteFunctionEventInvokeConfigResponse
  request = delete lambda
  response = receiveNull DeleteFunctionEventInvokeConfigResponse'

instance Hashable DeleteFunctionEventInvokeConfig

instance NFData DeleteFunctionEventInvokeConfig

instance ToHeaders DeleteFunctionEventInvokeConfig where
  toHeaders = const mempty

instance ToPath DeleteFunctionEventInvokeConfig where
  toPath DeleteFunctionEventInvokeConfig' {..} =
    mconcat
      [ "/2019-09-25/functions/",
        toBS _dfeicFunctionName,
        "/event-invoke-config"
      ]

instance ToQuery DeleteFunctionEventInvokeConfig where
  toQuery DeleteFunctionEventInvokeConfig' {..} =
    mconcat ["Qualifier" =: _dfeicQualifier]

-- | /See:/ 'deleteFunctionEventInvokeConfigResponse' smart constructor.
data DeleteFunctionEventInvokeConfigResponse = DeleteFunctionEventInvokeConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFunctionEventInvokeConfigResponse' with the minimum fields required to make a request.
deleteFunctionEventInvokeConfigResponse ::
  DeleteFunctionEventInvokeConfigResponse
deleteFunctionEventInvokeConfigResponse =
  DeleteFunctionEventInvokeConfigResponse'

instance NFData DeleteFunctionEventInvokeConfigResponse
