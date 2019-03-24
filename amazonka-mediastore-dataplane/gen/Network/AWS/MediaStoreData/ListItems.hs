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
-- Module      : Network.AWS.MediaStoreData.ListItems
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of metadata entries about folders and objects in the specified folder.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MediaStoreData.ListItems
    (
    -- * Creating a Request
      listItems
    , ListItems
    -- * Request Lenses
    , liPath
    , liNextToken
    , liMaxResults

    -- * Destructuring the Response
    , listItemsResponse
    , ListItemsResponse
    -- * Response Lenses
    , lirsItems
    , lirsNextToken
    , lirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStoreData.Types
import Network.AWS.MediaStoreData.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listItems' smart constructor.
data ListItems = ListItems'
  { _liPath       :: !(Maybe Text)
  , _liNextToken  :: !(Maybe Text)
  , _liMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListItems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liPath' - The path in the container from which to retrieve items. Format: <folder name>/<folder name>/<file name>
--
-- * 'liNextToken' - The token that identifies which batch of results that you want to see. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value. To see the next batch of results, you can submit the @ListItems@ request a second time and specify the @NextToken@ value. Tokens expire after 15 minutes.
--
-- * 'liMaxResults' - The maximum number of results to return per API request. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. Although 2,000 items match your request, the service returns no more than the first 500 items. (The service also returns a @NextToken@ value that you can use to fetch the next batch of results.) The service might return fewer results than the @MaxResults@ value. If @MaxResults@ is not included in the request, the service defaults to pagination with a maximum of 1,000 results per page.
listItems
    :: ListItems
listItems =
  ListItems'
    {_liPath = Nothing, _liNextToken = Nothing, _liMaxResults = Nothing}


-- | The path in the container from which to retrieve items. Format: <folder name>/<folder name>/<file name>
liPath :: Lens' ListItems (Maybe Text)
liPath = lens _liPath (\ s a -> s{_liPath = a})

-- | The token that identifies which batch of results that you want to see. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value. To see the next batch of results, you can submit the @ListItems@ request a second time and specify the @NextToken@ value. Tokens expire after 15 minutes.
liNextToken :: Lens' ListItems (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a})

-- | The maximum number of results to return per API request. For example, you submit a @ListItems@ request with @MaxResults@ set at 500. Although 2,000 items match your request, the service returns no more than the first 500 items. (The service also returns a @NextToken@ value that you can use to fetch the next batch of results.) The service might return fewer results than the @MaxResults@ value. If @MaxResults@ is not included in the request, the service defaults to pagination with a maximum of 1,000 results per page.
liMaxResults :: Lens' ListItems (Maybe Natural)
liMaxResults = lens _liMaxResults (\ s a -> s{_liMaxResults = a}) . mapping _Nat

instance AWSPager ListItems where
        page rq rs
          | stop (rs ^. lirsNextToken) = Nothing
          | stop (rs ^. lirsItems) = Nothing
          | otherwise =
            Just $ rq & liNextToken .~ rs ^. lirsNextToken

instance AWSRequest ListItems where
        type Rs ListItems = ListItemsResponse
        request = get mediaStoreData
        response
          = receiveJSON
              (\ s h x ->
                 ListItemsResponse' <$>
                   (x .?> "Items" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListItems where

instance NFData ListItems where

instance ToHeaders ListItems where
        toHeaders = const mempty

instance ToPath ListItems where
        toPath = const "/"

instance ToQuery ListItems where
        toQuery ListItems'{..}
          = mconcat
              ["Path" =: _liPath, "NextToken" =: _liNextToken,
               "MaxResults" =: _liMaxResults]

-- | /See:/ 'listItemsResponse' smart constructor.
data ListItemsResponse = ListItemsResponse'
  { _lirsItems          :: !(Maybe [Item])
  , _lirsNextToken      :: !(Maybe Text)
  , _lirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListItemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsItems' - The metadata entries for the folders and objects at the requested path.
--
-- * 'lirsNextToken' - The token that can be used in a request to view the next set of results. For example, you submit a @ListItems@ request that matches 2,000 items with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value that can be used to fetch the next batch of results.
--
-- * 'lirsResponseStatus' - -- | The response status code.
listItemsResponse
    :: Int -- ^ 'lirsResponseStatus'
    -> ListItemsResponse
listItemsResponse pResponseStatus_ =
  ListItemsResponse'
    { _lirsItems = Nothing
    , _lirsNextToken = Nothing
    , _lirsResponseStatus = pResponseStatus_
    }


-- | The metadata entries for the folders and objects at the requested path.
lirsItems :: Lens' ListItemsResponse [Item]
lirsItems = lens _lirsItems (\ s a -> s{_lirsItems = a}) . _Default . _Coerce

-- | The token that can be used in a request to view the next set of results. For example, you submit a @ListItems@ request that matches 2,000 items with @MaxResults@ set at 500. The service returns the first batch of results (up to 500) and a @NextToken@ value that can be used to fetch the next batch of results.
lirsNextToken :: Lens' ListItemsResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a})

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListItemsResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\ s a -> s{_lirsResponseStatus = a})

instance NFData ListItemsResponse where
