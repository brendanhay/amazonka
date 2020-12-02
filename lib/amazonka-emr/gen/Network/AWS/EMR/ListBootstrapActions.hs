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
-- Module      : Network.AWS.EMR.ListBootstrapActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the bootstrap actions associated with a cluster.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListBootstrapActions
    (
    -- * Creating a Request
      listBootstrapActions
    , ListBootstrapActions
    -- * Request Lenses
    , lbaMarker
    , lbaClusterId

    -- * Destructuring the Response
    , listBootstrapActionsResponse
    , ListBootstrapActionsResponse
    -- * Response Lenses
    , lbarsBootstrapActions
    , lbarsMarker
    , lbarsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | This input determines which bootstrap actions to retrieve.
--
--
--
-- /See:/ 'listBootstrapActions' smart constructor.
data ListBootstrapActions = ListBootstrapActions'
  { _lbaMarker    :: !(Maybe Text)
  , _lbaClusterId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBootstrapActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbaMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lbaClusterId' - The cluster identifier for the bootstrap actions to list.
listBootstrapActions
    :: Text -- ^ 'lbaClusterId'
    -> ListBootstrapActions
listBootstrapActions pClusterId_ =
  ListBootstrapActions' {_lbaMarker = Nothing, _lbaClusterId = pClusterId_}


-- | The pagination token that indicates the next set of results to retrieve.
lbaMarker :: Lens' ListBootstrapActions (Maybe Text)
lbaMarker = lens _lbaMarker (\ s a -> s{_lbaMarker = a})

-- | The cluster identifier for the bootstrap actions to list.
lbaClusterId :: Lens' ListBootstrapActions Text
lbaClusterId = lens _lbaClusterId (\ s a -> s{_lbaClusterId = a})

instance AWSPager ListBootstrapActions where
        page rq rs
          | stop (rs ^. lbarsMarker) = Nothing
          | stop (rs ^. lbarsBootstrapActions) = Nothing
          | otherwise =
            Just $ rq & lbaMarker .~ rs ^. lbarsMarker

instance AWSRequest ListBootstrapActions where
        type Rs ListBootstrapActions =
             ListBootstrapActionsResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 ListBootstrapActionsResponse' <$>
                   (x .?> "BootstrapActions" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable ListBootstrapActions where

instance NFData ListBootstrapActions where

instance ToHeaders ListBootstrapActions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.ListBootstrapActions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListBootstrapActions where
        toJSON ListBootstrapActions'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _lbaMarker,
                  Just ("ClusterId" .= _lbaClusterId)])

instance ToPath ListBootstrapActions where
        toPath = const "/"

instance ToQuery ListBootstrapActions where
        toQuery = const mempty

-- | This output contains the bootstrap actions detail.
--
--
--
-- /See:/ 'listBootstrapActionsResponse' smart constructor.
data ListBootstrapActionsResponse = ListBootstrapActionsResponse'
  { _lbarsBootstrapActions :: !(Maybe [Command])
  , _lbarsMarker           :: !(Maybe Text)
  , _lbarsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBootstrapActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbarsBootstrapActions' - The bootstrap actions associated with the cluster.
--
-- * 'lbarsMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lbarsResponseStatus' - -- | The response status code.
listBootstrapActionsResponse
    :: Int -- ^ 'lbarsResponseStatus'
    -> ListBootstrapActionsResponse
listBootstrapActionsResponse pResponseStatus_ =
  ListBootstrapActionsResponse'
    { _lbarsBootstrapActions = Nothing
    , _lbarsMarker = Nothing
    , _lbarsResponseStatus = pResponseStatus_
    }


-- | The bootstrap actions associated with the cluster.
lbarsBootstrapActions :: Lens' ListBootstrapActionsResponse [Command]
lbarsBootstrapActions = lens _lbarsBootstrapActions (\ s a -> s{_lbarsBootstrapActions = a}) . _Default . _Coerce

-- | The pagination token that indicates the next set of results to retrieve.
lbarsMarker :: Lens' ListBootstrapActionsResponse (Maybe Text)
lbarsMarker = lens _lbarsMarker (\ s a -> s{_lbarsMarker = a})

-- | -- | The response status code.
lbarsResponseStatus :: Lens' ListBootstrapActionsResponse Int
lbarsResponseStatus = lens _lbarsResponseStatus (\ s a -> s{_lbarsResponseStatus = a})

instance NFData ListBootstrapActionsResponse where
