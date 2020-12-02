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
-- Module      : Network.AWS.Athena.GetNamedQuery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single query.
--
--
module Network.AWS.Athena.GetNamedQuery
    (
    -- * Creating a Request
      getNamedQuery
    , GetNamedQuery
    -- * Request Lenses
    , gnqNamedQueryId

    -- * Destructuring the Response
    , getNamedQueryResponse
    , GetNamedQueryResponse
    -- * Response Lenses
    , gnqrsNamedQuery
    , gnqrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getNamedQuery' smart constructor.
newtype GetNamedQuery = GetNamedQuery'
  { _gnqNamedQueryId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetNamedQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gnqNamedQueryId' - The unique ID of the query. Use 'ListNamedQueries' to get query IDs.
getNamedQuery
    :: Text -- ^ 'gnqNamedQueryId'
    -> GetNamedQuery
getNamedQuery pNamedQueryId_ =
  GetNamedQuery' {_gnqNamedQueryId = pNamedQueryId_}


-- | The unique ID of the query. Use 'ListNamedQueries' to get query IDs.
gnqNamedQueryId :: Lens' GetNamedQuery Text
gnqNamedQueryId = lens _gnqNamedQueryId (\ s a -> s{_gnqNamedQueryId = a})

instance AWSRequest GetNamedQuery where
        type Rs GetNamedQuery = GetNamedQueryResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 GetNamedQueryResponse' <$>
                   (x .?> "NamedQuery") <*> (pure (fromEnum s)))

instance Hashable GetNamedQuery where

instance NFData GetNamedQuery where

instance ToHeaders GetNamedQuery where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.GetNamedQuery" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetNamedQuery where
        toJSON GetNamedQuery'{..}
          = object
              (catMaybes
                 [Just ("NamedQueryId" .= _gnqNamedQueryId)])

instance ToPath GetNamedQuery where
        toPath = const "/"

instance ToQuery GetNamedQuery where
        toQuery = const mempty

-- | /See:/ 'getNamedQueryResponse' smart constructor.
data GetNamedQueryResponse = GetNamedQueryResponse'
  { _gnqrsNamedQuery     :: !(Maybe NamedQuery)
  , _gnqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetNamedQueryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gnqrsNamedQuery' - Information about the query.
--
-- * 'gnqrsResponseStatus' - -- | The response status code.
getNamedQueryResponse
    :: Int -- ^ 'gnqrsResponseStatus'
    -> GetNamedQueryResponse
getNamedQueryResponse pResponseStatus_ =
  GetNamedQueryResponse'
    {_gnqrsNamedQuery = Nothing, _gnqrsResponseStatus = pResponseStatus_}


-- | Information about the query.
gnqrsNamedQuery :: Lens' GetNamedQueryResponse (Maybe NamedQuery)
gnqrsNamedQuery = lens _gnqrsNamedQuery (\ s a -> s{_gnqrsNamedQuery = a})

-- | -- | The response status code.
gnqrsResponseStatus :: Lens' GetNamedQueryResponse Int
gnqrsResponseStatus = lens _gnqrsResponseStatus (\ s a -> s{_gnqrsResponseStatus = a})

instance NFData GetNamedQueryResponse where
