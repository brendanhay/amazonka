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
-- Module      : Network.AWS.DMS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For internal use only
--
--
module Network.AWS.DMS.DescribePendingMaintenanceActions
    (
    -- * Creating a Request
      describePendingMaintenanceActions
    , DescribePendingMaintenanceActions
    -- * Request Lenses
    , dpmaFilters
    , dpmaMarker
    , dpmaMaxRecords
    , dpmaReplicationInstanceARN

    -- * Destructuring the Response
    , describePendingMaintenanceActionsResponse
    , DescribePendingMaintenanceActionsResponse
    -- * Response Lenses
    , dpmarsPendingMaintenanceActions
    , dpmarsMarker
    , dpmarsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describePendingMaintenanceActions' smart constructor.
data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions'
  { _dpmaFilters                :: !(Maybe [Filter])
  , _dpmaMarker                 :: !(Maybe Text)
  , _dpmaMaxRecords             :: !(Maybe Int)
  , _dpmaReplicationInstanceARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePendingMaintenanceActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpmaFilters' -
--
-- * 'dpmaMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dpmaMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'dpmaReplicationInstanceARN' - The ARN of the replication instance.
describePendingMaintenanceActions
    :: DescribePendingMaintenanceActions
describePendingMaintenanceActions =
  DescribePendingMaintenanceActions'
    { _dpmaFilters = Nothing
    , _dpmaMarker = Nothing
    , _dpmaMaxRecords = Nothing
    , _dpmaReplicationInstanceARN = Nothing
    }


-- |
dpmaFilters :: Lens' DescribePendingMaintenanceActions [Filter]
dpmaFilters = lens _dpmaFilters (\ s a -> s{_dpmaFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dpmaMarker :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmaMarker = lens _dpmaMarker (\ s a -> s{_dpmaMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dpmaMaxRecords :: Lens' DescribePendingMaintenanceActions (Maybe Int)
dpmaMaxRecords = lens _dpmaMaxRecords (\ s a -> s{_dpmaMaxRecords = a})

-- | The ARN of the replication instance.
dpmaReplicationInstanceARN :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmaReplicationInstanceARN = lens _dpmaReplicationInstanceARN (\ s a -> s{_dpmaReplicationInstanceARN = a})

instance AWSRequest DescribePendingMaintenanceActions
         where
        type Rs DescribePendingMaintenanceActions =
             DescribePendingMaintenanceActionsResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribePendingMaintenanceActionsResponse' <$>
                   (x .?> "PendingMaintenanceActions" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribePendingMaintenanceActions
         where

instance NFData DescribePendingMaintenanceActions
         where

instance ToHeaders DescribePendingMaintenanceActions
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribePendingMaintenanceActions"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePendingMaintenanceActions
         where
        toJSON DescribePendingMaintenanceActions'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dpmaFilters,
                  ("Marker" .=) <$> _dpmaMarker,
                  ("MaxRecords" .=) <$> _dpmaMaxRecords,
                  ("ReplicationInstanceArn" .=) <$>
                    _dpmaReplicationInstanceARN])

instance ToPath DescribePendingMaintenanceActions
         where
        toPath = const "/"

instance ToQuery DescribePendingMaintenanceActions
         where
        toQuery = const mempty

-- |
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
-- * 'dpmarsPendingMaintenanceActions' - The pending maintenance action.
--
-- * 'dpmarsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
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


-- | The pending maintenance action.
dpmarsPendingMaintenanceActions :: Lens' DescribePendingMaintenanceActionsResponse [ResourcePendingMaintenanceActions]
dpmarsPendingMaintenanceActions = lens _dpmarsPendingMaintenanceActions (\ s a -> s{_dpmarsPendingMaintenanceActions = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dpmarsMarker :: Lens' DescribePendingMaintenanceActionsResponse (Maybe Text)
dpmarsMarker = lens _dpmarsMarker (\ s a -> s{_dpmarsMarker = a})

-- | -- | The response status code.
dpmarsResponseStatus :: Lens' DescribePendingMaintenanceActionsResponse Int
dpmarsResponseStatus = lens _dpmarsResponseStatus (\ s a -> s{_dpmarsResponseStatus = a})

instance NFData
           DescribePendingMaintenanceActionsResponse
         where
