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
-- Module      : Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the provisioned concurrency configuration for a function.
module Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
  ( -- * Creating a Request
    deleteProvisionedConcurrencyConfig,
    DeleteProvisionedConcurrencyConfig,

    -- * Request Lenses
    dpccFunctionName,
    dpccQualifier,

    -- * Destructuring the Response
    deleteProvisionedConcurrencyConfigResponse,
    DeleteProvisionedConcurrencyConfigResponse,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteProvisionedConcurrencyConfig' smart constructor.
data DeleteProvisionedConcurrencyConfig = DeleteProvisionedConcurrencyConfig'
  { _dpccFunctionName ::
      !Text,
    _dpccQualifier ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProvisionedConcurrencyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpccFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'dpccQualifier' - The version number or alias name.
deleteProvisionedConcurrencyConfig ::
  -- | 'dpccFunctionName'
  Text ->
  -- | 'dpccQualifier'
  Text ->
  DeleteProvisionedConcurrencyConfig
deleteProvisionedConcurrencyConfig pFunctionName_ pQualifier_ =
  DeleteProvisionedConcurrencyConfig'
    { _dpccFunctionName =
        pFunctionName_,
      _dpccQualifier = pQualifier_
    }

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
dpccFunctionName :: Lens' DeleteProvisionedConcurrencyConfig Text
dpccFunctionName = lens _dpccFunctionName (\s a -> s {_dpccFunctionName = a})

-- | The version number or alias name.
dpccQualifier :: Lens' DeleteProvisionedConcurrencyConfig Text
dpccQualifier = lens _dpccQualifier (\s a -> s {_dpccQualifier = a})

instance AWSRequest DeleteProvisionedConcurrencyConfig where
  type
    Rs DeleteProvisionedConcurrencyConfig =
      DeleteProvisionedConcurrencyConfigResponse
  request = delete lambda
  response = receiveNull DeleteProvisionedConcurrencyConfigResponse'

instance Hashable DeleteProvisionedConcurrencyConfig

instance NFData DeleteProvisionedConcurrencyConfig

instance ToHeaders DeleteProvisionedConcurrencyConfig where
  toHeaders = const mempty

instance ToPath DeleteProvisionedConcurrencyConfig where
  toPath DeleteProvisionedConcurrencyConfig' {..} =
    mconcat
      [ "/2019-09-30/functions/",
        toBS _dpccFunctionName,
        "/provisioned-concurrency"
      ]

instance ToQuery DeleteProvisionedConcurrencyConfig where
  toQuery DeleteProvisionedConcurrencyConfig' {..} =
    mconcat ["Qualifier" =: _dpccQualifier]

-- | /See:/ 'deleteProvisionedConcurrencyConfigResponse' smart constructor.
data DeleteProvisionedConcurrencyConfigResponse = DeleteProvisionedConcurrencyConfigResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteProvisionedConcurrencyConfigResponse' with the minimum fields required to make a request.
deleteProvisionedConcurrencyConfigResponse ::
  DeleteProvisionedConcurrencyConfigResponse
deleteProvisionedConcurrencyConfigResponse =
  DeleteProvisionedConcurrencyConfigResponse'

instance NFData DeleteProvisionedConcurrencyConfigResponse
