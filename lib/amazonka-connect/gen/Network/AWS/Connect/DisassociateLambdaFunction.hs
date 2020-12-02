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
-- Module      : Network.AWS.Connect.DisassociateLambdaFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the Lambda function from the drop-down options available in the relevant contact flow blocks.
module Network.AWS.Connect.DisassociateLambdaFunction
  ( -- * Creating a Request
    disassociateLambdaFunction,
    DisassociateLambdaFunction,

    -- * Request Lenses
    dlfInstanceId,
    dlfFunctionARN,

    -- * Destructuring the Response
    disassociateLambdaFunctionResponse,
    DisassociateLambdaFunctionResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateLambdaFunction' smart constructor.
data DisassociateLambdaFunction = DisassociateLambdaFunction'
  { _dlfInstanceId ::
      !Text,
    _dlfFunctionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateLambdaFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlfInstanceId' - The identifier of the Amazon Connect instance..
--
-- * 'dlfFunctionARN' - The Amazon Resource Name (ARN) of the Lambda function being disassociated.
disassociateLambdaFunction ::
  -- | 'dlfInstanceId'
  Text ->
  -- | 'dlfFunctionARN'
  Text ->
  DisassociateLambdaFunction
disassociateLambdaFunction pInstanceId_ pFunctionARN_ =
  DisassociateLambdaFunction'
    { _dlfInstanceId = pInstanceId_,
      _dlfFunctionARN = pFunctionARN_
    }

-- | The identifier of the Amazon Connect instance..
dlfInstanceId :: Lens' DisassociateLambdaFunction Text
dlfInstanceId = lens _dlfInstanceId (\s a -> s {_dlfInstanceId = a})

-- | The Amazon Resource Name (ARN) of the Lambda function being disassociated.
dlfFunctionARN :: Lens' DisassociateLambdaFunction Text
dlfFunctionARN = lens _dlfFunctionARN (\s a -> s {_dlfFunctionARN = a})

instance AWSRequest DisassociateLambdaFunction where
  type
    Rs DisassociateLambdaFunction =
      DisassociateLambdaFunctionResponse
  request = delete connect
  response = receiveNull DisassociateLambdaFunctionResponse'

instance Hashable DisassociateLambdaFunction

instance NFData DisassociateLambdaFunction

instance ToHeaders DisassociateLambdaFunction where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DisassociateLambdaFunction where
  toPath DisassociateLambdaFunction' {..} =
    mconcat ["/instance/", toBS _dlfInstanceId, "/lambda-function"]

instance ToQuery DisassociateLambdaFunction where
  toQuery DisassociateLambdaFunction' {..} =
    mconcat ["functionArn" =: _dlfFunctionARN]

-- | /See:/ 'disassociateLambdaFunctionResponse' smart constructor.
data DisassociateLambdaFunctionResponse = DisassociateLambdaFunctionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateLambdaFunctionResponse' with the minimum fields required to make a request.
disassociateLambdaFunctionResponse ::
  DisassociateLambdaFunctionResponse
disassociateLambdaFunctionResponse =
  DisassociateLambdaFunctionResponse'

instance NFData DisassociateLambdaFunctionResponse
