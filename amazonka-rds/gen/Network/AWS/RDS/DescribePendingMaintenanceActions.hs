{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resources (for example, DB instances) that have at
-- least one pending maintenance action.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribePendingMaintenanceActions.html>
module Network.AWS.RDS.DescribePendingMaintenanceActions
    (
    -- * Request
      DescribePendingMaintenanceActions
    -- ** Request constructor
    , describePendingMaintenanceActions
    -- ** Request lenses
    , dpmarqFilters
    , dpmarqMaxRecords
    , dpmarqMarker
    , dpmarqResourceIdentifier

    -- * Response
    , DescribePendingMaintenanceActionsResponse
    -- ** Response constructor
    , describePendingMaintenanceActionsResponse
    -- ** Response lenses
    , dpmarsPendingMaintenanceActions
    , dpmarsMarker
    , dpmarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describePendingMaintenanceActions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpmarqFilters'
--
-- * 'dpmarqMaxRecords'
--
-- * 'dpmarqMarker'
--
-- * 'dpmarqResourceIdentifier'
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
    { _dpmarqFilters            :: !(Maybe [Filter])
    , _dpmarqMaxRecords         :: !(Maybe Int)
    , _dpmarqMarker             :: !(Maybe Text)
    , _dpmarqResourceIdentifier :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePendingMaintenanceActions' smart constructor.
describePendingMaintenanceActions :: DescribePendingMaintenanceActions
describePendingMaintenanceActions =
    DescribePendingMaintenanceActions'
    { _dpmarqFilters = Nothing
    , _dpmarqMaxRecords = Nothing
    , _dpmarqMarker = Nothing
    , _dpmarqResourceIdentifier = Nothing
    }

-- | A filter that specifies one or more resources to return pending
-- maintenance actions for.
--
-- Supported filters:
--
-- -   @db-instance-id@ - Accepts DB instance identifiers and DB instance
--     Amazon Resource Names (ARNs). The results list will only include
--     pending maintenance actions for the DB instances identified by these
--     ARNs.
dpmarqFilters :: Lens' DescribePendingMaintenanceActions [Filter]
dpmarqFilters = lens _dpmarqFilters (\ s a -> s{_dpmarqFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dpmarqMaxRecords :: Lens' DescribePendingMaintenanceActions (Maybe Int)
dpmarqMaxRecords = lens _dpmarqMaxRecords (\ s a -> s{_dpmarqMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
dpmarqMarker :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmarqMarker = lens _dpmarqMarker (\ s a -> s{_dpmarqMarker = a});

-- | The ARN of a resource to return pending maintenance actions for.
dpmarqResourceIdentifier :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmarqResourceIdentifier = lens _dpmarqResourceIdentifier (\ s a -> s{_dpmarqResourceIdentifier = a});

instance AWSRequest DescribePendingMaintenanceActions
         where
        type Sv DescribePendingMaintenanceActions = RDS
        type Rs DescribePendingMaintenanceActions =
             DescribePendingMaintenanceActionsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribePendingMaintenanceActionsResult"
              (\ s h x ->
                 DescribePendingMaintenanceActionsResponse' <$>
                   (x .@? "PendingMaintenanceActions" .!@ mempty >>=
                      may
                        (parseXMLList "ResourcePendingMaintenanceActions"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribePendingMaintenanceActions
         where
        toHeaders = const mempty

instance ToPath DescribePendingMaintenanceActions
         where
        toPath = const "/"

instance ToQuery DescribePendingMaintenanceActions
         where
        toQuery DescribePendingMaintenanceActions'{..}
          = mconcat
              ["Action" =:
                 ("DescribePendingMaintenanceActions" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dpmarqFilters),
               "MaxRecords" =: _dpmarqMaxRecords,
               "Marker" =: _dpmarqMarker,
               "ResourceIdentifier" =: _dpmarqResourceIdentifier]

-- | Data returned from the __DescribePendingMaintenanceActions__ action.
--
-- /See:/ 'describePendingMaintenanceActionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpmarsPendingMaintenanceActions'
--
-- * 'dpmarsMarker'
--
-- * 'dpmarsStatus'
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
    { _dpmarsPendingMaintenanceActions :: !(Maybe [ResourcePendingMaintenanceActions])
    , _dpmarsMarker                    :: !(Maybe Text)
    , _dpmarsStatus                    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePendingMaintenanceActionsResponse' smart constructor.
describePendingMaintenanceActionsResponse :: Int -> DescribePendingMaintenanceActionsResponse
describePendingMaintenanceActionsResponse pStatus_ =
    DescribePendingMaintenanceActionsResponse'
    { _dpmarsPendingMaintenanceActions = Nothing
    , _dpmarsMarker = Nothing
    , _dpmarsStatus = pStatus_
    }

-- | A list of the pending maintenance actions for the resource.
dpmarsPendingMaintenanceActions :: Lens' DescribePendingMaintenanceActionsResponse [ResourcePendingMaintenanceActions]
dpmarsPendingMaintenanceActions = lens _dpmarsPendingMaintenanceActions (\ s a -> s{_dpmarsPendingMaintenanceActions = a}) . _Default;

-- | An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
dpmarsMarker :: Lens' DescribePendingMaintenanceActionsResponse (Maybe Text)
dpmarsMarker = lens _dpmarsMarker (\ s a -> s{_dpmarsMarker = a});

-- | FIXME: Undocumented member.
dpmarsStatus :: Lens' DescribePendingMaintenanceActionsResponse Int
dpmarsStatus = lens _dpmarsStatus (\ s a -> s{_dpmarsStatus = a});
