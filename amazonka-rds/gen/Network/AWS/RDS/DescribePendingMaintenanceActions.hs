{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribePendingMaintenanceActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of resources (for example, DB Instances) that have at least
-- one pending maintenance action.
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
    , dpmaMarker
    , dpmaMaxRecords
    , dpmaResourceIdentifier

    -- * Response
    , DescribePendingMaintenanceActionsResponse
    -- ** Response constructor
    , describePendingMaintenanceActionsResponse
    -- ** Response lenses
    , dpmarMarker
    , dpmarPendingMaintenanceActions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribePendingMaintenanceActions = DescribePendingMaintenanceActions
    { _dpmaFilters            :: List "member" Filter
    , _dpmaMarker             :: Maybe Text
    , _dpmaMaxRecords         :: Maybe Int
    , _dpmaResourceIdentifier :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribePendingMaintenanceActions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpmaFilters' @::@ ['Filter']
--
-- * 'dpmaMarker' @::@ 'Maybe' 'Text'
--
-- * 'dpmaMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dpmaResourceIdentifier' @::@ 'Maybe' 'Text'
--
describePendingMaintenanceActions :: DescribePendingMaintenanceActions
describePendingMaintenanceActions = DescribePendingMaintenanceActions
    { _dpmaResourceIdentifier = Nothing
    , _dpmaFilters            = mempty
    , _dpmaMarker             = Nothing
    , _dpmaMaxRecords         = Nothing
    }

-- | Supported filters:
--
-- 'db-instance-id' - Accepts DB instance identifiers and DB instance ARNs. The
-- result list will only include maintenance actions for the specified DB
-- Instances.
dpmaFilters :: Lens' DescribePendingMaintenanceActions [Filter]
dpmaFilters = lens _dpmaFilters (\s a -> s { _dpmaFilters = a }) . _List

-- | An optional pagination token provided by a previous 'DescribePendingMaintenanceActions' request. If this parameter is specified, the response includes only records
-- beyond the marker, up to a number of records specified by 'MaxRecords' .
dpmaMarker :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmaMarker = lens _dpmaMarker (\s a -> s { _dpmaMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a pagination token called a marker
-- is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dpmaMaxRecords :: Lens' DescribePendingMaintenanceActions (Maybe Int)
dpmaMaxRecords = lens _dpmaMaxRecords (\s a -> s { _dpmaMaxRecords = a })

-- | The ARN of the resource to return pending maintenance actions for.
dpmaResourceIdentifier :: Lens' DescribePendingMaintenanceActions (Maybe Text)
dpmaResourceIdentifier =
    lens _dpmaResourceIdentifier (\s a -> s { _dpmaResourceIdentifier = a })

data DescribePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse
    { _dpmarMarker                    :: Maybe Text
    , _dpmarPendingMaintenanceActions :: List "member" ResourcePendingMaintenanceActions
    } deriving (Eq, Read, Show)

-- | 'DescribePendingMaintenanceActionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpmarMarker' @::@ 'Maybe' 'Text'
--
-- * 'dpmarPendingMaintenanceActions' @::@ ['ResourcePendingMaintenanceActions']
--
describePendingMaintenanceActionsResponse :: DescribePendingMaintenanceActionsResponse
describePendingMaintenanceActionsResponse = DescribePendingMaintenanceActionsResponse
    { _dpmarPendingMaintenanceActions = mempty
    , _dpmarMarker                    = Nothing
    }

-- | An optional pagination token provided by a previous 'DescribePendingMaintenanceActions' request. If this parameter is specified, the response includes only records
-- beyond the marker, up to a number of records specified by 'MaxRecords' .
dpmarMarker :: Lens' DescribePendingMaintenanceActionsResponse (Maybe Text)
dpmarMarker = lens _dpmarMarker (\s a -> s { _dpmarMarker = a })

-- | Provides a list of the pending maintenance actions for the resource.
dpmarPendingMaintenanceActions :: Lens' DescribePendingMaintenanceActionsResponse [ResourcePendingMaintenanceActions]
dpmarPendingMaintenanceActions =
    lens _dpmarPendingMaintenanceActions
        (\s a -> s { _dpmarPendingMaintenanceActions = a })
            . _List

instance ToPath DescribePendingMaintenanceActions where
    toPath = const "/"

instance ToQuery DescribePendingMaintenanceActions where
    toQuery DescribePendingMaintenanceActions{..} = mconcat
        [ "Filters"            =? _dpmaFilters
        , "Marker"             =? _dpmaMarker
        , "MaxRecords"         =? _dpmaMaxRecords
        , "ResourceIdentifier" =? _dpmaResourceIdentifier
        ]

instance ToHeaders DescribePendingMaintenanceActions

instance AWSRequest DescribePendingMaintenanceActions where
    type Sv DescribePendingMaintenanceActions = RDS
    type Rs DescribePendingMaintenanceActions = DescribePendingMaintenanceActionsResponse

    request  = post "DescribePendingMaintenanceActions"
    response = xmlResponse

instance FromXML DescribePendingMaintenanceActionsResponse where
    parseXML = withElement "DescribePendingMaintenanceActionsResult" $ \x -> DescribePendingMaintenanceActionsResponse
        <$> x .@? "Marker"
        <*> x .@? "PendingMaintenanceActions" .!@ mempty
