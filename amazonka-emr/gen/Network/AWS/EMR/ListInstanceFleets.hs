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
-- Module      : Network.AWS.EMR.ListInstanceFleets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available details about the instance fleets in a cluster.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstanceFleets
    (
    -- * Creating a Request
      listInstanceFleets
    , ListInstanceFleets
    -- * Request Lenses
    , lifMarker
    , lifClusterId

    -- * Destructuring the Response
    , listInstanceFleetsResponse
    , ListInstanceFleetsResponse
    -- * Response Lenses
    , lifrsInstanceFleets
    , lifrsMarker
    , lifrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listInstanceFleets' smart constructor.
data ListInstanceFleets = ListInstanceFleets'
  { _lifMarker    :: !(Maybe Text)
  , _lifClusterId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInstanceFleets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lifMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lifClusterId' - The unique identifier of the cluster.
listInstanceFleets
    :: Text -- ^ 'lifClusterId'
    -> ListInstanceFleets
listInstanceFleets pClusterId_ =
  ListInstanceFleets' {_lifMarker = Nothing, _lifClusterId = pClusterId_}


-- | The pagination token that indicates the next set of results to retrieve.
lifMarker :: Lens' ListInstanceFleets (Maybe Text)
lifMarker = lens _lifMarker (\ s a -> s{_lifMarker = a})

-- | The unique identifier of the cluster.
lifClusterId :: Lens' ListInstanceFleets Text
lifClusterId = lens _lifClusterId (\ s a -> s{_lifClusterId = a})

instance AWSPager ListInstanceFleets where
        page rq rs
          | stop (rs ^. lifrsMarker) = Nothing
          | stop (rs ^. lifrsInstanceFleets) = Nothing
          | otherwise =
            Just $ rq & lifMarker .~ rs ^. lifrsMarker

instance AWSRequest ListInstanceFleets where
        type Rs ListInstanceFleets =
             ListInstanceFleetsResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 ListInstanceFleetsResponse' <$>
                   (x .?> "InstanceFleets" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable ListInstanceFleets where

instance NFData ListInstanceFleets where

instance ToHeaders ListInstanceFleets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.ListInstanceFleets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListInstanceFleets where
        toJSON ListInstanceFleets'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _lifMarker,
                  Just ("ClusterId" .= _lifClusterId)])

instance ToPath ListInstanceFleets where
        toPath = const "/"

instance ToQuery ListInstanceFleets where
        toQuery = const mempty

-- | /See:/ 'listInstanceFleetsResponse' smart constructor.
data ListInstanceFleetsResponse = ListInstanceFleetsResponse'
  { _lifrsInstanceFleets :: !(Maybe [InstanceFleet])
  , _lifrsMarker         :: !(Maybe Text)
  , _lifrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInstanceFleetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lifrsInstanceFleets' - The list of instance fleets for the cluster and given filters.
--
-- * 'lifrsMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lifrsResponseStatus' - -- | The response status code.
listInstanceFleetsResponse
    :: Int -- ^ 'lifrsResponseStatus'
    -> ListInstanceFleetsResponse
listInstanceFleetsResponse pResponseStatus_ =
  ListInstanceFleetsResponse'
    { _lifrsInstanceFleets = Nothing
    , _lifrsMarker = Nothing
    , _lifrsResponseStatus = pResponseStatus_
    }


-- | The list of instance fleets for the cluster and given filters.
lifrsInstanceFleets :: Lens' ListInstanceFleetsResponse [InstanceFleet]
lifrsInstanceFleets = lens _lifrsInstanceFleets (\ s a -> s{_lifrsInstanceFleets = a}) . _Default . _Coerce

-- | The pagination token that indicates the next set of results to retrieve.
lifrsMarker :: Lens' ListInstanceFleetsResponse (Maybe Text)
lifrsMarker = lens _lifrsMarker (\ s a -> s{_lifrsMarker = a})

-- | -- | The response status code.
lifrsResponseStatus :: Lens' ListInstanceFleetsResponse Int
lifrsResponseStatus = lens _lifrsResponseStatus (\ s a -> s{_lifrsResponseStatus = a})

instance NFData ListInstanceFleetsResponse where
