{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.GetQueryExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single execution of a query. Each time a query executes, information about the query execution is saved with a unique ID.
--
--
module Network.AWS.Athena.GetQueryExecution
    (
    -- * Creating a Request
      getQueryExecution
    , GetQueryExecution
    -- * Request Lenses
    , gqeQueryExecutionId

    -- * Destructuring the Response
    , getQueryExecutionResponse
    , GetQueryExecutionResponse
    -- * Response Lenses
    , gqersQueryExecution
    , gqersResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getQueryExecution' smart constructor.
newtype GetQueryExecution = GetQueryExecution'
  { _gqeQueryExecutionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueryExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqeQueryExecutionId' - The unique ID of the query execution.
getQueryExecution
    :: Text -- ^ 'gqeQueryExecutionId'
    -> GetQueryExecution
getQueryExecution pQueryExecutionId_ =
  GetQueryExecution' {_gqeQueryExecutionId = pQueryExecutionId_}


-- | The unique ID of the query execution.
gqeQueryExecutionId :: Lens' GetQueryExecution Text
gqeQueryExecutionId = lens _gqeQueryExecutionId (\ s a -> s{_gqeQueryExecutionId = a})

instance AWSRequest GetQueryExecution where
        type Rs GetQueryExecution = GetQueryExecutionResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 GetQueryExecutionResponse' <$>
                   (x .?> "QueryExecution") <*> (pure (fromEnum s)))

instance Hashable GetQueryExecution where

instance NFData GetQueryExecution where

instance ToHeaders GetQueryExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.GetQueryExecution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetQueryExecution where
        toJSON GetQueryExecution'{..}
          = object
              (catMaybes
                 [Just ("QueryExecutionId" .= _gqeQueryExecutionId)])

instance ToPath GetQueryExecution where
        toPath = const "/"

instance ToQuery GetQueryExecution where
        toQuery = const mempty

-- | /See:/ 'getQueryExecutionResponse' smart constructor.
data GetQueryExecutionResponse = GetQueryExecutionResponse'
  { _gqersQueryExecution :: !(Maybe QueryExecution)
  , _gqersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueryExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqersQueryExecution' - Information about the query execution.
--
-- * 'gqersResponseStatus' - -- | The response status code.
getQueryExecutionResponse
    :: Int -- ^ 'gqersResponseStatus'
    -> GetQueryExecutionResponse
getQueryExecutionResponse pResponseStatus_ =
  GetQueryExecutionResponse'
    {_gqersQueryExecution = Nothing, _gqersResponseStatus = pResponseStatus_}


-- | Information about the query execution.
gqersQueryExecution :: Lens' GetQueryExecutionResponse (Maybe QueryExecution)
gqersQueryExecution = lens _gqersQueryExecution (\ s a -> s{_gqersQueryExecution = a})

-- | -- | The response status code.
gqersResponseStatus :: Lens' GetQueryExecutionResponse Int
gqersResponseStatus = lens _gqersResponseStatus (\ s a -> s{_gqersResponseStatus = a})

instance NFData GetQueryExecutionResponse where
