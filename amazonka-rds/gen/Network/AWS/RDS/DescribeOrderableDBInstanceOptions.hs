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

-- Module      : Network.AWS.RDS.DescribeOrderableDBInstanceOptions
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

-- | Returns a list of orderable DB instance options for the specified engine.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeOrderableDBInstanceOptions.html>
module Network.AWS.RDS.DescribeOrderableDBInstanceOptions
    (
    -- * Request
      DescribeOrderableDBInstanceOptions
    -- ** Request constructor
    , describeOrderableDBInstanceOptions
    -- ** Request lenses
    , dodbioDBInstanceClass
    , dodbioEngine
    , dodbioEngineVersion
    , dodbioFilters
    , dodbioLicenseModel
    , dodbioMarker
    , dodbioMaxRecords
    , dodbioVpc

    -- * Response
    , DescribeOrderableDBInstanceOptionsResponse
    -- ** Response constructor
    , describeOrderableDBInstanceOptionsResponse
    -- ** Response lenses
    , dodbiorMarker
    , dodbiorOrderableDBInstanceOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions
    { _dodbioDBInstanceClass :: Maybe Text
    , _dodbioEngine          :: Text
    , _dodbioEngineVersion   :: Maybe Text
    , _dodbioFilters         :: List "Filter" Filter
    , _dodbioLicenseModel    :: Maybe Text
    , _dodbioMarker          :: Maybe Text
    , _dodbioMaxRecords      :: Maybe Int
    , _dodbioVpc             :: Maybe Bool
    } deriving (Eq, Show)

-- | 'DescribeOrderableDBInstanceOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dodbioDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'dodbioEngine' @::@ 'Text'
--
-- * 'dodbioEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dodbioFilters' @::@ ['Filter']
--
-- * 'dodbioLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'dodbioMarker' @::@ 'Maybe' 'Text'
--
-- * 'dodbioMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dodbioVpc' @::@ 'Maybe' 'Bool'
--
describeOrderableDBInstanceOptions :: Text -- ^ 'dodbioEngine'
                                   -> DescribeOrderableDBInstanceOptions
describeOrderableDBInstanceOptions p1 = DescribeOrderableDBInstanceOptions
    { _dodbioEngine          = p1
    , _dodbioEngineVersion   = Nothing
    , _dodbioDBInstanceClass = Nothing
    , _dodbioLicenseModel    = Nothing
    , _dodbioVpc             = Nothing
    , _dodbioFilters         = mempty
    , _dodbioMaxRecords      = Nothing
    , _dodbioMarker          = Nothing
    }

-- | The DB instance class filter value. Specify this parameter to show only the
-- available offerings matching the specified DB instance class.
dodbioDBInstanceClass :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbioDBInstanceClass =
    lens _dodbioDBInstanceClass (\s a -> s { _dodbioDBInstanceClass = a })

-- | The name of the engine to retrieve DB instance options for.
dodbioEngine :: Lens' DescribeOrderableDBInstanceOptions Text
dodbioEngine = lens _dodbioEngine (\s a -> s { _dodbioEngine = a })

-- | The engine version filter value. Specify this parameter to show only the
-- available offerings matching the specified engine version.
dodbioEngineVersion :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbioEngineVersion =
    lens _dodbioEngineVersion (\s a -> s { _dodbioEngineVersion = a })

-- | This parameter is not currently supported.
dodbioFilters :: Lens' DescribeOrderableDBInstanceOptions [Filter]
dodbioFilters = lens _dodbioFilters (\s a -> s { _dodbioFilters = a }) . _List

-- | The license model filter value. Specify this parameter to show only the
-- available offerings matching the specified license model.
dodbioLicenseModel :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbioLicenseModel =
    lens _dodbioLicenseModel (\s a -> s { _dodbioLicenseModel = a })

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by 'MaxRecords' .
dodbioMarker :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbioMarker = lens _dodbioMarker (\s a -> s { _dodbioMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a pagination token called a marker
-- is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dodbioMaxRecords :: Lens' DescribeOrderableDBInstanceOptions (Maybe Int)
dodbioMaxRecords = lens _dodbioMaxRecords (\s a -> s { _dodbioMaxRecords = a })

-- | The VPC filter value. Specify this parameter to show only the available VPC
-- or non-VPC offerings.
dodbioVpc :: Lens' DescribeOrderableDBInstanceOptions (Maybe Bool)
dodbioVpc = lens _dodbioVpc (\s a -> s { _dodbioVpc = a })

data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse
    { _dodbiorMarker                     :: Maybe Text
    , _dodbiorOrderableDBInstanceOptions :: List "OrderableDBInstanceOption" OrderableDBInstanceOption
    } deriving (Eq, Show)

-- | 'DescribeOrderableDBInstanceOptionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dodbiorMarker' @::@ 'Maybe' 'Text'
--
-- * 'dodbiorOrderableDBInstanceOptions' @::@ ['OrderableDBInstanceOption']
--
describeOrderableDBInstanceOptionsResponse :: DescribeOrderableDBInstanceOptionsResponse
describeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse
    { _dodbiorOrderableDBInstanceOptions = mempty
    , _dodbiorMarker                     = Nothing
    }

-- | An optional pagination token provided by a previous
-- OrderableDBInstanceOptions request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value specified
-- by 'MaxRecords' .
dodbiorMarker :: Lens' DescribeOrderableDBInstanceOptionsResponse (Maybe Text)
dodbiorMarker = lens _dodbiorMarker (\s a -> s { _dodbiorMarker = a })

-- | An 'OrderableDBInstanceOption' structure containing information about orderable
-- options for the DB instance.
dodbiorOrderableDBInstanceOptions :: Lens' DescribeOrderableDBInstanceOptionsResponse [OrderableDBInstanceOption]
dodbiorOrderableDBInstanceOptions =
    lens _dodbiorOrderableDBInstanceOptions
        (\s a -> s { _dodbiorOrderableDBInstanceOptions = a })
            . _List

instance ToPath DescribeOrderableDBInstanceOptions where
    toPath = const "/"

instance ToQuery DescribeOrderableDBInstanceOptions where
    toQuery DescribeOrderableDBInstanceOptions{..} = mconcat
        [ "DBInstanceClass" =? _dodbioDBInstanceClass
        , "Engine"          =? _dodbioEngine
        , "EngineVersion"   =? _dodbioEngineVersion
        , "Filters"         =? _dodbioFilters
        , "LicenseModel"    =? _dodbioLicenseModel
        , "Marker"          =? _dodbioMarker
        , "MaxRecords"      =? _dodbioMaxRecords
        , "Vpc"             =? _dodbioVpc
        ]

instance ToHeaders DescribeOrderableDBInstanceOptions

instance AWSRequest DescribeOrderableDBInstanceOptions where
    type Sv DescribeOrderableDBInstanceOptions = RDS
    type Rs DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptionsResponse

    request  = post "DescribeOrderableDBInstanceOptions"
    response = xmlResponse

instance FromXML DescribeOrderableDBInstanceOptionsResponse where
    parseXML = withElement "DescribeOrderableDBInstanceOptionsResult" $ \x -> DescribeOrderableDBInstanceOptionsResponse
        <$> x .@? "Marker"
        <*> x .@  "OrderableDBInstanceOptions"

instance AWSPager DescribeOrderableDBInstanceOptions where
    page rq rs
        | stop (rq ^. dodbioMarker) = Nothing
        | otherwise = (\x -> rq & dodbioMarker ?~ x)
            <$> (rs ^. dodbiorMarker)
