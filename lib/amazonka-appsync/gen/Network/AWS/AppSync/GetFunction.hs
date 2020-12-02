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
-- Module      : Network.AWS.AppSync.GetFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a @Function@ .
module Network.AWS.AppSync.GetFunction
  ( -- * Creating a Request
    getFunction,
    GetFunction,

    -- * Request Lenses
    gfApiId,
    gfFunctionId,

    -- * Destructuring the Response
    getFunctionResponse,
    GetFunctionResponse,

    -- * Response Lenses
    gfrsFunctionConfiguration,
    gfrsResponseStatus,
  )
where

import Network.AWS.AppSync.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFunction' smart constructor.
data GetFunction = GetFunction'
  { _gfApiId :: !Text,
    _gfFunctionId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfApiId' - The GraphQL API ID.
--
-- * 'gfFunctionId' - The @Function@ ID.
getFunction ::
  -- | 'gfApiId'
  Text ->
  -- | 'gfFunctionId'
  Text ->
  GetFunction
getFunction pApiId_ pFunctionId_ =
  GetFunction' {_gfApiId = pApiId_, _gfFunctionId = pFunctionId_}

-- | The GraphQL API ID.
gfApiId :: Lens' GetFunction Text
gfApiId = lens _gfApiId (\s a -> s {_gfApiId = a})

-- | The @Function@ ID.
gfFunctionId :: Lens' GetFunction Text
gfFunctionId = lens _gfFunctionId (\s a -> s {_gfFunctionId = a})

instance AWSRequest GetFunction where
  type Rs GetFunction = GetFunctionResponse
  request = get appSync
  response =
    receiveJSON
      ( \s h x ->
          GetFunctionResponse'
            <$> (x .?> "functionConfiguration") <*> (pure (fromEnum s))
      )

instance Hashable GetFunction

instance NFData GetFunction

instance ToHeaders GetFunction where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetFunction where
  toPath GetFunction' {..} =
    mconcat
      ["/v1/apis/", toBS _gfApiId, "/functions/", toBS _gfFunctionId]

instance ToQuery GetFunction where
  toQuery = const mempty

-- | /See:/ 'getFunctionResponse' smart constructor.
data GetFunctionResponse = GetFunctionResponse'
  { _gfrsFunctionConfiguration ::
      !(Maybe FunctionConfiguration),
    _gfrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfrsFunctionConfiguration' - The @Function@ object.
--
-- * 'gfrsResponseStatus' - -- | The response status code.
getFunctionResponse ::
  -- | 'gfrsResponseStatus'
  Int ->
  GetFunctionResponse
getFunctionResponse pResponseStatus_ =
  GetFunctionResponse'
    { _gfrsFunctionConfiguration = Nothing,
      _gfrsResponseStatus = pResponseStatus_
    }

-- | The @Function@ object.
gfrsFunctionConfiguration :: Lens' GetFunctionResponse (Maybe FunctionConfiguration)
gfrsFunctionConfiguration = lens _gfrsFunctionConfiguration (\s a -> s {_gfrsFunctionConfiguration = a})

-- | -- | The response status code.
gfrsResponseStatus :: Lens' GetFunctionResponse Int
gfrsResponseStatus = lens _gfrsResponseStatus (\s a -> s {_gfrsResponseStatus = a})

instance NFData GetFunctionResponse
