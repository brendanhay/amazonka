{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListBootstrapActions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the bootstrap actions associated with a
-- cluster.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListBootstrapActions.html>
module Network.AWS.EMR.ListBootstrapActions
    (
    -- * Request
      ListBootstrapActions
    -- ** Request constructor
    , listBootstrapActions
    -- ** Request lenses
    , lbarqMarker
    , lbarqClusterId

    -- * Response
    , ListBootstrapActionsResponse
    -- ** Response constructor
    , listBootstrapActionsResponse
    -- ** Response lenses
    , lbarsBootstrapActions
    , lbarsMarker
    , lbarsStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines which bootstrap actions to retrieve.
--
-- /See:/ 'listBootstrapActions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbarqMarker'
--
-- * 'lbarqClusterId'
data ListBootstrapActions = ListBootstrapActions'
    { _lbarqMarker    :: !(Maybe Text)
    , _lbarqClusterId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListBootstrapActions' smart constructor.
listBootstrapActions :: Text -> ListBootstrapActions
listBootstrapActions pClusterId =
    ListBootstrapActions'
    { _lbarqMarker = Nothing
    , _lbarqClusterId = pClusterId
    }

-- | The pagination token that indicates the next set of results to retrieve
-- .
lbarqMarker :: Lens' ListBootstrapActions (Maybe Text)
lbarqMarker = lens _lbarqMarker (\ s a -> s{_lbarqMarker = a});

-- | The cluster identifier for the bootstrap actions to list .
lbarqClusterId :: Lens' ListBootstrapActions Text
lbarqClusterId = lens _lbarqClusterId (\ s a -> s{_lbarqClusterId = a});

instance AWSPager ListBootstrapActions where
        page rq rs
          | stop (rs ^. lbarsMarker) = Nothing
          | stop (rs ^. lbarsBootstrapActions) = Nothing
          | otherwise =
            Just $ rq & lbarqMarker .~ rs ^. lbarsMarker

instance AWSRequest ListBootstrapActions where
        type Sv ListBootstrapActions = EMR
        type Rs ListBootstrapActions =
             ListBootstrapActionsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListBootstrapActionsResponse' <$>
                   (x .?> "BootstrapActions" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

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
              ["Marker" .= _lbarqMarker,
               "ClusterId" .= _lbarqClusterId]

instance ToPath ListBootstrapActions where
        toPath = const "/"

instance ToQuery ListBootstrapActions where
        toQuery = const mempty

-- | This output contains the boostrap actions detail .
--
-- /See:/ 'listBootstrapActionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbarsBootstrapActions'
--
-- * 'lbarsMarker'
--
-- * 'lbarsStatus'
data ListBootstrapActionsResponse = ListBootstrapActionsResponse'
    { _lbarsBootstrapActions :: !(Maybe [Command])
    , _lbarsMarker           :: !(Maybe Text)
    , _lbarsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListBootstrapActionsResponse' smart constructor.
listBootstrapActionsResponse :: Int -> ListBootstrapActionsResponse
listBootstrapActionsResponse pStatus =
    ListBootstrapActionsResponse'
    { _lbarsBootstrapActions = Nothing
    , _lbarsMarker = Nothing
    , _lbarsStatus = pStatus
    }

-- | The bootstrap actions associated with the cluster .
lbarsBootstrapActions :: Lens' ListBootstrapActionsResponse [Command]
lbarsBootstrapActions = lens _lbarsBootstrapActions (\ s a -> s{_lbarsBootstrapActions = a}) . _Default;

-- | The pagination token that indicates the next set of results to retrieve
-- .
lbarsMarker :: Lens' ListBootstrapActionsResponse (Maybe Text)
lbarsMarker = lens _lbarsMarker (\ s a -> s{_lbarsMarker = a});

-- | FIXME: Undocumented member.
lbarsStatus :: Lens' ListBootstrapActionsResponse Int
lbarsStatus = lens _lbarsStatus (\ s a -> s{_lbarsStatus = a});
