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
-- Module      : Network.AWS.AppSync.ListDataSources
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data sources for a given API.
--
--
module Network.AWS.AppSync.ListDataSources
    (
    -- * Creating a Request
      listDataSources
    , ListDataSources
    -- * Request Lenses
    , ldsNextToken
    , ldsMaxResults
    , ldsApiId

    -- * Destructuring the Response
    , listDataSourcesResponse
    , ListDataSourcesResponse
    -- * Response Lenses
    , ldsrsDataSources
    , ldsrsNextToken
    , ldsrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDataSources' smart constructor.
data ListDataSources = ListDataSources'
  { _ldsNextToken  :: !(Maybe Text)
  , _ldsMaxResults :: !(Maybe Nat)
  , _ldsApiId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDataSources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'ldsMaxResults' - The maximum number of results you want the request to return.
--
-- * 'ldsApiId' - The API ID.
listDataSources
    :: Text -- ^ 'ldsApiId'
    -> ListDataSources
listDataSources pApiId_ =
  ListDataSources'
    {_ldsNextToken = Nothing, _ldsMaxResults = Nothing, _ldsApiId = pApiId_}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
ldsNextToken :: Lens' ListDataSources (Maybe Text)
ldsNextToken = lens _ldsNextToken (\ s a -> s{_ldsNextToken = a})

-- | The maximum number of results you want the request to return.
ldsMaxResults :: Lens' ListDataSources (Maybe Natural)
ldsMaxResults = lens _ldsMaxResults (\ s a -> s{_ldsMaxResults = a}) . mapping _Nat

-- | The API ID.
ldsApiId :: Lens' ListDataSources Text
ldsApiId = lens _ldsApiId (\ s a -> s{_ldsApiId = a})

instance AWSRequest ListDataSources where
        type Rs ListDataSources = ListDataSourcesResponse
        request = get appSync
        response
          = receiveJSON
              (\ s h x ->
                 ListDataSourcesResponse' <$>
                   (x .?> "dataSources" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListDataSources where

instance NFData ListDataSources where

instance ToHeaders ListDataSources where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListDataSources where
        toPath ListDataSources'{..}
          = mconcat
              ["/v1/apis/", toBS _ldsApiId, "/datasources"]

instance ToQuery ListDataSources where
        toQuery ListDataSources'{..}
          = mconcat
              ["nextToken" =: _ldsNextToken,
               "maxResults" =: _ldsMaxResults]

-- | /See:/ 'listDataSourcesResponse' smart constructor.
data ListDataSourcesResponse = ListDataSourcesResponse'
  { _ldsrsDataSources    :: !(Maybe [DataSource])
  , _ldsrsNextToken      :: !(Maybe Text)
  , _ldsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDataSourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldsrsDataSources' - The @DataSource@ objects.
--
-- * 'ldsrsNextToken' - An identifier to be passed in the next request to this operation to return the next set of items in the list.
--
-- * 'ldsrsResponseStatus' - -- | The response status code.
listDataSourcesResponse
    :: Int -- ^ 'ldsrsResponseStatus'
    -> ListDataSourcesResponse
listDataSourcesResponse pResponseStatus_ =
  ListDataSourcesResponse'
    { _ldsrsDataSources = Nothing
    , _ldsrsNextToken = Nothing
    , _ldsrsResponseStatus = pResponseStatus_
    }


-- | The @DataSource@ objects.
ldsrsDataSources :: Lens' ListDataSourcesResponse [DataSource]
ldsrsDataSources = lens _ldsrsDataSources (\ s a -> s{_ldsrsDataSources = a}) . _Default . _Coerce

-- | An identifier to be passed in the next request to this operation to return the next set of items in the list.
ldsrsNextToken :: Lens' ListDataSourcesResponse (Maybe Text)
ldsrsNextToken = lens _ldsrsNextToken (\ s a -> s{_ldsrsNextToken = a})

-- | -- | The response status code.
ldsrsResponseStatus :: Lens' ListDataSourcesResponse Int
ldsrsResponseStatus = lens _ldsrsResponseStatus (\ s a -> s{_ldsrsResponseStatus = a})

instance NFData ListDataSourcesResponse where
