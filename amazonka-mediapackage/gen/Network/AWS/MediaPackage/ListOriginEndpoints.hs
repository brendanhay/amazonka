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
-- Module      : Network.AWS.MediaPackage.ListOriginEndpoints
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of OriginEndpoint records.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListOriginEndpoints
    (
    -- * Creating a Request
      listOriginEndpoints
    , ListOriginEndpoints
    -- * Request Lenses
    , loeChannelId
    , loeNextToken
    , loeMaxResults

    -- * Destructuring the Response
    , listOriginEndpointsResponse
    , ListOriginEndpointsResponse
    -- * Response Lenses
    , loersOriginEndpoints
    , loersNextToken
    , loersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types
import Network.AWS.MediaPackage.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listOriginEndpoints' smart constructor.
data ListOriginEndpoints = ListOriginEndpoints'
  { _loeChannelId  :: !(Maybe Text)
  , _loeNextToken  :: !(Maybe Text)
  , _loeMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOriginEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loeChannelId' - When specified, the request will return only OriginEndpoints associated with the given Channel ID.
--
-- * 'loeNextToken' - A token used to resume pagination from the end of a previous request.
--
-- * 'loeMaxResults' - The upper bound on the number of records to return.
listOriginEndpoints
    :: ListOriginEndpoints
listOriginEndpoints =
  ListOriginEndpoints'
    {_loeChannelId = Nothing, _loeNextToken = Nothing, _loeMaxResults = Nothing}


-- | When specified, the request will return only OriginEndpoints associated with the given Channel ID.
loeChannelId :: Lens' ListOriginEndpoints (Maybe Text)
loeChannelId = lens _loeChannelId (\ s a -> s{_loeChannelId = a})

-- | A token used to resume pagination from the end of a previous request.
loeNextToken :: Lens' ListOriginEndpoints (Maybe Text)
loeNextToken = lens _loeNextToken (\ s a -> s{_loeNextToken = a})

-- | The upper bound on the number of records to return.
loeMaxResults :: Lens' ListOriginEndpoints (Maybe Natural)
loeMaxResults = lens _loeMaxResults (\ s a -> s{_loeMaxResults = a}) . mapping _Nat

instance AWSPager ListOriginEndpoints where
        page rq rs
          | stop (rs ^. loersNextToken) = Nothing
          | stop (rs ^. loersOriginEndpoints) = Nothing
          | otherwise =
            Just $ rq & loeNextToken .~ rs ^. loersNextToken

instance AWSRequest ListOriginEndpoints where
        type Rs ListOriginEndpoints =
             ListOriginEndpointsResponse
        request = get mediaPackage
        response
          = receiveJSON
              (\ s h x ->
                 ListOriginEndpointsResponse' <$>
                   (x .?> "originEndpoints" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListOriginEndpoints where

instance NFData ListOriginEndpoints where

instance ToHeaders ListOriginEndpoints where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListOriginEndpoints where
        toPath = const "/origin_endpoints"

instance ToQuery ListOriginEndpoints where
        toQuery ListOriginEndpoints'{..}
          = mconcat
              ["channelId" =: _loeChannelId,
               "nextToken" =: _loeNextToken,
               "maxResults" =: _loeMaxResults]

-- | /See:/ 'listOriginEndpointsResponse' smart constructor.
data ListOriginEndpointsResponse = ListOriginEndpointsResponse'
  { _loersOriginEndpoints :: !(Maybe [OriginEndpoint])
  , _loersNextToken       :: !(Maybe Text)
  , _loersResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOriginEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loersOriginEndpoints' - A list of OriginEndpoint records.
--
-- * 'loersNextToken' - A token that can be used to resume pagination from the end of the collection.
--
-- * 'loersResponseStatus' - -- | The response status code.
listOriginEndpointsResponse
    :: Int -- ^ 'loersResponseStatus'
    -> ListOriginEndpointsResponse
listOriginEndpointsResponse pResponseStatus_ =
  ListOriginEndpointsResponse'
    { _loersOriginEndpoints = Nothing
    , _loersNextToken = Nothing
    , _loersResponseStatus = pResponseStatus_
    }


-- | A list of OriginEndpoint records.
loersOriginEndpoints :: Lens' ListOriginEndpointsResponse [OriginEndpoint]
loersOriginEndpoints = lens _loersOriginEndpoints (\ s a -> s{_loersOriginEndpoints = a}) . _Default . _Coerce

-- | A token that can be used to resume pagination from the end of the collection.
loersNextToken :: Lens' ListOriginEndpointsResponse (Maybe Text)
loersNextToken = lens _loersNextToken (\ s a -> s{_loersNextToken = a})

-- | -- | The response status code.
loersResponseStatus :: Lens' ListOriginEndpointsResponse Int
loersResponseStatus = lens _loersResponseStatus (\ s a -> s{_loersResponseStatus = a})

instance NFData ListOriginEndpointsResponse where
