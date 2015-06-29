{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of resources (for example, DB instances) that have at
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
    , dpmaFilters
    , dpmaMaxRecords
    , dpmaMarker
    , dpmaResourceIdentifier

    -- * Response
    , DescribePendingMaintenanceActionsResponse
    -- ** Response constructor
    , describePendingMaintenanceActionsResponse
    -- ** Response lenses
    , dpmarPendingMaintenanceActions
    , dpmarMarker
    , dpmarStatus
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
-- * 'dpmaFilters'
--
-- * 'dpmaMaxRecords'
--
-- * 'dpmaMarker'
--
-- * 'dpmaResourceIdentifier'
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
    { _dpmaFilters            :: !(Maybe [Filter])
    , _dpmaMaxRecords         :: !(Maybe Int)
    , _dpmaMarker             :: !(Maybe Text)
    , _dpmaResourceIdentifier :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribePendingMaintenanceActions' smart constructor.
describePendingMaintenanceActions :: DescribePendingMaintenanceActions
describePendingMaintenanceActions =
    DescribePendingMaintenanceActions'
    { _dpmaFilters = Nothing
    , _dpmaMaxRecords = Nothing
    , _dpmaMarker = Nothing
    , _dpmaResourceIdentifier = Nothing
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
dpmaFilters :: Lens' DescribePendingMaintenanceActions [Filter]
dpmaFilters = lens _dpmaFilters (\ s a -> s{_dpmaFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dpmaMaxRecords :: Lens' DescribePendingMaintenanceActions (Maybe Int)
dpmaMaxRecords = lens _dpmaMaxRecords (\ s a -> s{_dpmaMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
dpmaMarker :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmaMarker = lens _dpmaMarker (\ s a -> s{_dpmaMarker = a});

-- | The ARN of a resource to return pending maintenance actions for.
dpmaResourceIdentifier :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmaResourceIdentifier = lens _dpmaResourceIdentifier (\ s a -> s{_dpmaResourceIdentifier = a});

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
                 toQuery (toQueryList "Filter" <$> _dpmaFilters),
               "MaxRecords" =: _dpmaMaxRecords,
               "Marker" =: _dpmaMarker,
               "ResourceIdentifier" =: _dpmaResourceIdentifier]

-- | Data returned from the __DescribePendingMaintenanceActions__ action.
--
-- /See:/ 'describePendingMaintenanceActionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpmarPendingMaintenanceActions'
--
-- * 'dpmarMarker'
--
-- * 'dpmarStatus'
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
    { _dpmarPendingMaintenanceActions :: !(Maybe [ResourcePendingMaintenanceActions])
    , _dpmarMarker                    :: !(Maybe Text)
    , _dpmarStatus                    :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribePendingMaintenanceActionsResponse' smart constructor.
describePendingMaintenanceActionsResponse :: Int -> DescribePendingMaintenanceActionsResponse
describePendingMaintenanceActionsResponse pStatus =
    DescribePendingMaintenanceActionsResponse'
    { _dpmarPendingMaintenanceActions = Nothing
    , _dpmarMarker = Nothing
    , _dpmarStatus = pStatus
    }

-- | A list of the pending maintenance actions for the resource.
dpmarPendingMaintenanceActions :: Lens' DescribePendingMaintenanceActionsResponse [ResourcePendingMaintenanceActions]
dpmarPendingMaintenanceActions = lens _dpmarPendingMaintenanceActions (\ s a -> s{_dpmarPendingMaintenanceActions = a}) . _Default;

-- | An optional pagination token provided by a previous
-- @DescribePendingMaintenanceActions@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to a
-- number of records specified by @MaxRecords@.
dpmarMarker :: Lens' DescribePendingMaintenanceActionsResponse (Maybe Text)
dpmarMarker = lens _dpmarMarker (\ s a -> s{_dpmarMarker = a});

-- | FIXME: Undocumented member.
dpmarStatus :: Lens' DescribePendingMaintenanceActionsResponse Int
dpmarStatus = lens _dpmarStatus (\ s a -> s{_dpmarStatus = a});
