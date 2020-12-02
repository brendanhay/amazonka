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
-- Module      : Network.AWS.Connect.AssociateLambdaFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the specified Amazon Connect instance to access the specified Lambda function.
module Network.AWS.Connect.AssociateLambdaFunction
  ( -- * Creating a Request
    associateLambdaFunction,
    AssociateLambdaFunction,

    -- * Request Lenses
    alfInstanceId,
    alfFunctionARN,

    -- * Destructuring the Response
    associateLambdaFunctionResponse,
    AssociateLambdaFunctionResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateLambdaFunction' smart constructor.
data AssociateLambdaFunction = AssociateLambdaFunction'
  { _alfInstanceId ::
      !Text,
    _alfFunctionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateLambdaFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alfInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'alfFunctionARN' - The Amazon Resource Name (ARN) for the Lambda function being associated. Maximum number of characters allowed is 140.
associateLambdaFunction ::
  -- | 'alfInstanceId'
  Text ->
  -- | 'alfFunctionARN'
  Text ->
  AssociateLambdaFunction
associateLambdaFunction pInstanceId_ pFunctionARN_ =
  AssociateLambdaFunction'
    { _alfInstanceId = pInstanceId_,
      _alfFunctionARN = pFunctionARN_
    }

-- | The identifier of the Amazon Connect instance.
alfInstanceId :: Lens' AssociateLambdaFunction Text
alfInstanceId = lens _alfInstanceId (\s a -> s {_alfInstanceId = a})

-- | The Amazon Resource Name (ARN) for the Lambda function being associated. Maximum number of characters allowed is 140.
alfFunctionARN :: Lens' AssociateLambdaFunction Text
alfFunctionARN = lens _alfFunctionARN (\s a -> s {_alfFunctionARN = a})

instance AWSRequest AssociateLambdaFunction where
  type Rs AssociateLambdaFunction = AssociateLambdaFunctionResponse
  request = putJSON connect
  response = receiveNull AssociateLambdaFunctionResponse'

instance Hashable AssociateLambdaFunction

instance NFData AssociateLambdaFunction

instance ToHeaders AssociateLambdaFunction where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON AssociateLambdaFunction where
  toJSON AssociateLambdaFunction' {..} =
    object (catMaybes [Just ("FunctionArn" .= _alfFunctionARN)])

instance ToPath AssociateLambdaFunction where
  toPath AssociateLambdaFunction' {..} =
    mconcat ["/instance/", toBS _alfInstanceId, "/lambda-function"]

instance ToQuery AssociateLambdaFunction where
  toQuery = const mempty

-- | /See:/ 'associateLambdaFunctionResponse' smart constructor.
data AssociateLambdaFunctionResponse = AssociateLambdaFunctionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociateLambdaFunctionResponse' with the minimum fields required to make a request.
associateLambdaFunctionResponse ::
  AssociateLambdaFunctionResponse
associateLambdaFunctionResponse = AssociateLambdaFunctionResponse'

instance NFData AssociateLambdaFunctionResponse
