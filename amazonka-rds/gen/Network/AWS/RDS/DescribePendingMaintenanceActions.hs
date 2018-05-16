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
-- Module      : Network.AWS.RDS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resources (for example, DB instances) that have at least one pending maintenance action.
--
--
module Network.AWS.RDS.DescribePendingMaintenanceActions
    (
    -- * Creating a Request
      describePendingMaintenanceActions
    , DescribePendingMaintenanceActions
    -- * Request Lenses
    , dpmaFilters
    , dpmaMarker
    , dpmaMaxRecords
    , dpmaResourceIdentifier

    -- * Destructuring the Response
    , describePendingMaintenanceActionsResponse
    , DescribePendingMaintenanceActionsResponse
    -- * Response Lenses
    , dpmarsPendingMaintenanceActions
    , dpmarsMarker
    , dpmarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { _dpmaFilters            :: !(Maybe [Filter])
  , _dpmaMarker             :: !(Maybe Text)
  , _dpmaMaxRecords         :: !(Maybe Int)
  , _dpmaResourceIdentifier :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePendingMaintenanceActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpmaFilters' - A filter that specifies one or more resources to return pending maintenance actions for. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include pending maintenance actions for the DB clusters identified by these ARNs.     * @db-instance-id@ - Accepts DB instance identifiers and DB instance ARNs. The results list will only include pending maintenance actions for the DB instances identified by these ARNs.
--
-- * 'dpmaMarker' - An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
--
-- * 'dpmaMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'dpmaResourceIdentifier' - The ARN of a resource to return pending maintenance actions for.
describePendingMaintenanceActions
    :: DescribePendingMaintenanceActions
describePendingMaintenanceActions =
  DescribePendingMaintenanceActions'
    { _dpmaFilters = Nothing
    , _dpmaMarker = Nothing
    , _dpmaMaxRecords = Nothing
    , _dpmaResourceIdentifier = Nothing
    }


-- | A filter that specifies one or more resources to return pending maintenance actions for. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include pending maintenance actions for the DB clusters identified by these ARNs.     * @db-instance-id@ - Accepts DB instance identifiers and DB instance ARNs. The results list will only include pending maintenance actions for the DB instances identified by these ARNs.
dpmaFilters :: Lens' DescribePendingMaintenanceActions [Filter]
dpmaFilters = lens _dpmaFilters (\ s a -> s{_dpmaFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
dpmaMarker :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmaMarker = lens _dpmaMarker (\ s a -> s{_dpmaMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dpmaMaxRecords :: Lens' DescribePendingMaintenanceActions (Maybe Int)
dpmaMaxRecords = lens _dpmaMaxRecords (\ s a -> s{_dpmaMaxRecords = a})

-- | The ARN of a resource to return pending maintenance actions for.
dpmaResourceIdentifier :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmaResourceIdentifier = lens _dpmaResourceIdentifier (\ s a -> s{_dpmaResourceIdentifier = a})

instance AWSRequest DescribePendingMaintenanceActions
         where
        type Rs DescribePendingMaintenanceActions =
             DescribePendingMaintenanceActionsResponse
        request = postQuery rds
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

instance Hashable DescribePendingMaintenanceActions
         where

instance NFData DescribePendingMaintenanceActions
         where

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
               "Marker" =: _dpmaMarker,
               "MaxRecords" =: _dpmaMaxRecords,
               "ResourceIdentifier" =: _dpmaResourceIdentifier]

-- | Data returned from the __DescribePendingMaintenanceActions__ action.
--
--
--
-- /See:/ 'describePendingMaintenanceActionsResponse' smart constructor.
data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse'
  { _dpmarsPendingMaintenanceActions :: !(Maybe [ResourcePendingMaintenanceActions])
  , _dpmarsMarker :: !(Maybe Text)
  , _dpmarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePendingMaintenanceActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpmarsPendingMaintenanceActions' - A list of the pending maintenance actions for the resource.
--
-- * 'dpmarsMarker' - An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
--
-- * 'dpmarsResponseStatus' - -- | The response status code.
describePendingMaintenanceActionsResponse
    :: Int -- ^ 'dpmarsResponseStatus'
    -> DescribePendingMaintenanceActionsResponse
describePendingMaintenanceActionsResponse pResponseStatus_ =
  DescribePendingMaintenanceActionsResponse'
    { _dpmarsPendingMaintenanceActions = Nothing
    , _dpmarsMarker = Nothing
    , _dpmarsResponseStatus = pResponseStatus_
    }


-- | A list of the pending maintenance actions for the resource.
dpmarsPendingMaintenanceActions :: Lens' DescribePendingMaintenanceActionsResponse [ResourcePendingMaintenanceActions]
dpmarsPendingMaintenanceActions = lens _dpmarsPendingMaintenanceActions (\ s a -> s{_dpmarsPendingMaintenanceActions = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribePendingMaintenanceActions@ request. If this parameter is specified, the response includes only records beyond the marker, up to a number of records specified by @MaxRecords@ .
dpmarsMarker :: Lens' DescribePendingMaintenanceActionsResponse (Maybe Text)
dpmarsMarker = lens _dpmarsMarker (\ s a -> s{_dpmarsMarker = a})

-- | -- | The response status code.
dpmarsResponseStatus :: Lens' DescribePendingMaintenanceActionsResponse Int
dpmarsResponseStatus = lens _dpmarsResponseStatus (\ s a -> s{_dpmarsResponseStatus = a})

instance NFData
           DescribePendingMaintenanceActionsResponse
         where
