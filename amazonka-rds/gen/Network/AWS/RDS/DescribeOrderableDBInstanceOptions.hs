{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.RDS.DescribeOrderableDBInstanceOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of orderable DB instance options for the specified engine.
module Network.AWS.RDS.DescribeOrderableDBInstanceOptions
    (
    -- * Request
      DescribeOrderableDBInstanceOptionsMessage
    -- ** Request constructor
    , describeOrderableDBInstanceOptionsMessage
    -- ** Request lenses
    , dodbiomDBInstanceClass
    , dodbiomEngine
    , dodbiomEngineVersion
    , dodbiomFilters
    , dodbiomLicenseModel
    , dodbiomMarker
    , dodbiomMaxRecords
    , dodbiomVpc

    -- * Response
    , OrderableDBInstanceOptionsMessage
    -- ** Response constructor
    , orderableDBInstanceOptionsMessage
    -- ** Response lenses
    , odbiomMarker
    , odbiomOrderableDBInstanceOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeOrderableDBInstanceOptionsMessage = DescribeOrderableDBInstanceOptionsMessage
    { _dodbiomDBInstanceClass :: Maybe Text
    , _dodbiomEngine          :: Text
    , _dodbiomEngineVersion   :: Maybe Text
    , _dodbiomFilters         :: [Filter]
    , _dodbiomLicenseModel    :: Maybe Text
    , _dodbiomMarker          :: Maybe Text
    , _dodbiomMaxRecords      :: Maybe Int
    , _dodbiomVpc             :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'DescribeOrderableDBInstanceOptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dodbiomDBInstanceClass' @::@ 'Maybe' 'Text'
--
-- * 'dodbiomEngine' @::@ 'Text'
--
-- * 'dodbiomEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'dodbiomFilters' @::@ ['Filter']
--
-- * 'dodbiomLicenseModel' @::@ 'Maybe' 'Text'
--
-- * 'dodbiomMarker' @::@ 'Maybe' 'Text'
--
-- * 'dodbiomMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dodbiomVpc' @::@ 'Maybe' 'Bool'
--
describeOrderableDBInstanceOptionsMessage :: Text -- ^ 'dodbiomEngine'
                                          -> DescribeOrderableDBInstanceOptionsMessage
describeOrderableDBInstanceOptionsMessage p1 = DescribeOrderableDBInstanceOptionsMessage
    { _dodbiomEngine          = p1
    , _dodbiomEngineVersion   = Nothing
    , _dodbiomDBInstanceClass = Nothing
    , _dodbiomLicenseModel    = Nothing
    , _dodbiomVpc             = Nothing
    , _dodbiomFilters         = mempty
    , _dodbiomMaxRecords      = Nothing
    , _dodbiomMarker          = Nothing
    }

-- | The DB instance class filter value. Specify this parameter to show only
-- the available offerings matching the specified DB instance class.
dodbiomDBInstanceClass :: Lens' DescribeOrderableDBInstanceOptionsMessage (Maybe Text)
dodbiomDBInstanceClass =
    lens _dodbiomDBInstanceClass (\s a -> s { _dodbiomDBInstanceClass = a })

-- | The name of the engine to retrieve DB instance options for.
dodbiomEngine :: Lens' DescribeOrderableDBInstanceOptionsMessage Text
dodbiomEngine = lens _dodbiomEngine (\s a -> s { _dodbiomEngine = a })

-- | The engine version filter value. Specify this parameter to show only the
-- available offerings matching the specified engine version.
dodbiomEngineVersion :: Lens' DescribeOrderableDBInstanceOptionsMessage (Maybe Text)
dodbiomEngineVersion =
    lens _dodbiomEngineVersion (\s a -> s { _dodbiomEngineVersion = a })

-- | This parameter is not currently supported.
dodbiomFilters :: Lens' DescribeOrderableDBInstanceOptionsMessage [Filter]
dodbiomFilters = lens _dodbiomFilters (\s a -> s { _dodbiomFilters = a })

-- | The license model filter value. Specify this parameter to show only the
-- available offerings matching the specified license model.
dodbiomLicenseModel :: Lens' DescribeOrderableDBInstanceOptionsMessage (Maybe Text)
dodbiomLicenseModel =
    lens _dodbiomLicenseModel (\s a -> s { _dodbiomLicenseModel = a })

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords .
dodbiomMarker :: Lens' DescribeOrderableDBInstanceOptionsMessage (Maybe Text)
dodbiomMarker = lens _dodbiomMarker (\s a -> s { _dodbiomMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dodbiomMaxRecords :: Lens' DescribeOrderableDBInstanceOptionsMessage (Maybe Int)
dodbiomMaxRecords =
    lens _dodbiomMaxRecords (\s a -> s { _dodbiomMaxRecords = a })

-- | The VPC filter value. Specify this parameter to show only the available
-- VPC or non-VPC offerings.
dodbiomVpc :: Lens' DescribeOrderableDBInstanceOptionsMessage (Maybe Bool)
dodbiomVpc = lens _dodbiomVpc (\s a -> s { _dodbiomVpc = a })
instance ToQuery DescribeOrderableDBInstanceOptionsMessage

instance ToPath DescribeOrderableDBInstanceOptionsMessage where
    toPath = const "/"

data OrderableDBInstanceOptionsMessage = OrderableDBInstanceOptionsMessage
    { _odbiomMarker                     :: Maybe Text
    , _odbiomOrderableDBInstanceOptions :: [OrderableDBInstanceOption]
    } deriving (Eq, Show, Generic)

-- | 'OrderableDBInstanceOptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'odbiomMarker' @::@ 'Maybe' 'Text'
--
-- * 'odbiomOrderableDBInstanceOptions' @::@ ['OrderableDBInstanceOption']
--
orderableDBInstanceOptionsMessage :: OrderableDBInstanceOptionsMessage
orderableDBInstanceOptionsMessage = OrderableDBInstanceOptionsMessage
    { _odbiomOrderableDBInstanceOptions = mempty
    , _odbiomMarker                     = Nothing
    }

-- | An optional pagination token provided by a previous
-- OrderableDBInstanceOptions request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by MaxRecords .
odbiomMarker :: Lens' OrderableDBInstanceOptionsMessage (Maybe Text)
odbiomMarker = lens _odbiomMarker (\s a -> s { _odbiomMarker = a })

-- | An OrderableDBInstanceOption structure containing information about
-- orderable options for the DB instance.
odbiomOrderableDBInstanceOptions :: Lens' OrderableDBInstanceOptionsMessage [OrderableDBInstanceOption]
odbiomOrderableDBInstanceOptions =
    lens _odbiomOrderableDBInstanceOptions
        (\s a -> s { _odbiomOrderableDBInstanceOptions = a })
instance FromXML OrderableDBInstanceOptionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OrderableDBInstanceOptionsMessage"

instance AWSRequest DescribeOrderableDBInstanceOptionsMessage where
    type Sv DescribeOrderableDBInstanceOptionsMessage = RDS
    type Rs DescribeOrderableDBInstanceOptionsMessage = OrderableDBInstanceOptionsMessage

    request  = post "DescribeOrderableDBInstanceOptions"
    response = xmlResponse $ \h x -> OrderableDBInstanceOptionsMessage
        <$> x %| "Marker"
        <*> x %| "OrderableDBInstanceOptions"
