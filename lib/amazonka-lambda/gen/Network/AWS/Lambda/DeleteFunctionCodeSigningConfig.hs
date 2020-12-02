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
-- Module      : Network.AWS.Lambda.DeleteFunctionCodeSigningConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the code signing configuration from the function.
module Network.AWS.Lambda.DeleteFunctionCodeSigningConfig
  ( -- * Creating a Request
    deleteFunctionCodeSigningConfig,
    DeleteFunctionCodeSigningConfig,

    -- * Request Lenses
    dfcscFunctionName,

    -- * Destructuring the Response
    deleteFunctionCodeSigningConfigResponse,
    DeleteFunctionCodeSigningConfigResponse,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFunctionCodeSigningConfig' smart constructor.
newtype DeleteFunctionCodeSigningConfig = DeleteFunctionCodeSigningConfig'
  { _dfcscFunctionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFunctionCodeSigningConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfcscFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
deleteFunctionCodeSigningConfig ::
  -- | 'dfcscFunctionName'
  Text ->
  DeleteFunctionCodeSigningConfig
deleteFunctionCodeSigningConfig pFunctionName_ =
  DeleteFunctionCodeSigningConfig'
    { _dfcscFunctionName =
        pFunctionName_
    }

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
dfcscFunctionName :: Lens' DeleteFunctionCodeSigningConfig Text
dfcscFunctionName = lens _dfcscFunctionName (\s a -> s {_dfcscFunctionName = a})

instance AWSRequest DeleteFunctionCodeSigningConfig where
  type
    Rs DeleteFunctionCodeSigningConfig =
      DeleteFunctionCodeSigningConfigResponse
  request = delete lambda
  response = receiveNull DeleteFunctionCodeSigningConfigResponse'

instance Hashable DeleteFunctionCodeSigningConfig

instance NFData DeleteFunctionCodeSigningConfig

instance ToHeaders DeleteFunctionCodeSigningConfig where
  toHeaders = const mempty

instance ToPath DeleteFunctionCodeSigningConfig where
  toPath DeleteFunctionCodeSigningConfig' {..} =
    mconcat
      [ "/2020-06-30/functions/",
        toBS _dfcscFunctionName,
        "/code-signing-config"
      ]

instance ToQuery DeleteFunctionCodeSigningConfig where
  toQuery = const mempty

-- | /See:/ 'deleteFunctionCodeSigningConfigResponse' smart constructor.
data DeleteFunctionCodeSigningConfigResponse = DeleteFunctionCodeSigningConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFunctionCodeSigningConfigResponse' with the minimum fields required to make a request.
deleteFunctionCodeSigningConfigResponse ::
  DeleteFunctionCodeSigningConfigResponse
deleteFunctionCodeSigningConfigResponse =
  DeleteFunctionCodeSigningConfigResponse'

instance NFData DeleteFunctionCodeSigningConfigResponse
