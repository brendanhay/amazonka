{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeOrderableDBInstanceOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of orderable DB instance options for the specified engine.
-- https://rds.amazonaws.com/ ?Action=DescribeOrderableDBInstanceOptions
-- &Engine=mysql &MaxRecords=100 &Version=2013-05-15
-- &Timestamp=2011-05-23T07%3A49%3A17.749Z &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &AWSAccessKeyId= &Signature= true mysql
-- general-public-license true 5.1.45 db.m1.large us-east-1a yes us-east-1b no
-- us-east-1d yes true mysql general-public-license true 5.1.45 db.m1.small
-- us-east-1a yes us-east-1b yes us-east-1d yes true mysql
-- general-public-license true 5.1.45 db.m1.xlarge us-east-1a yes us-east-1b
-- yes us-east-1d yes true mysql general-public-license true 5.1.45
-- db.m2.2xlarge us-east-1a yes us-east-1b yes us-east-1d yes true mysql
-- general-public-license true 5.1.45 db.m2.4xlarge us-east-1a yes us-east-1b
-- no us-east-1d no 2a0406d7-8511-11e0-90aa-eb648410240d.
module Network.AWS.RDS.V2013_09_09.DescribeOrderableDBInstanceOptions
    (
    -- * Request
      DescribeOrderableDBInstanceOptions
    -- ** Request constructor
    , mkDescribeOrderableDBInstanceOptions
    -- ** Request lenses
    , dodbioEngine
    , dodbioEngineVersion
    , dodbioDBInstanceClass
    , dodbioLicenseModel
    , dodbioVpc
    , dodbioMaxRecords
    , dodbioMarker

    -- * Response
    , DescribeOrderableDBInstanceOptionsResponse
    -- ** Response lenses
    , dodbiorsOrderableDBInstanceOptions
    , dodbiorsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions
    { _dodbioEngine :: Text
    , _dodbioEngineVersion :: Maybe Text
    , _dodbioDBInstanceClass :: Maybe Text
    , _dodbioLicenseModel :: Maybe Text
    , _dodbioVpc :: Maybe Bool
    , _dodbioMaxRecords :: Maybe Integer
    , _dodbioMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeOrderableDBInstanceOptions' request.
mkDescribeOrderableDBInstanceOptions :: Text -- ^ 'dodbioEngine'
                                     -> DescribeOrderableDBInstanceOptions
mkDescribeOrderableDBInstanceOptions p1 = DescribeOrderableDBInstanceOptions
    { _dodbioEngine = p1
    , _dodbioEngineVersion = Nothing
    , _dodbioDBInstanceClass = Nothing
    , _dodbioLicenseModel = Nothing
    , _dodbioVpc = Nothing
    , _dodbioMaxRecords = Nothing
    , _dodbioMarker = Nothing
    }

-- | The name of the engine to retrieve DB instance options for.
dodbioEngine :: Lens' DescribeOrderableDBInstanceOptions Text
dodbioEngine = lens _dodbioEngine (\s a -> s { _dodbioEngine = a })

-- | The engine version filter value. Specify this parameter to show only the
-- available offerings matching the specified engine version.
dodbioEngineVersion :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbioEngineVersion =
    lens _dodbioEngineVersion (\s a -> s { _dodbioEngineVersion = a })

-- | The DB instance class filter value. Specify this parameter to show only the
-- available offerings matching the specified DB instance class.
dodbioDBInstanceClass :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbioDBInstanceClass =
    lens _dodbioDBInstanceClass (\s a -> s { _dodbioDBInstanceClass = a })

-- | The license model filter value. Specify this parameter to show only the
-- available offerings matching the specified license model.
dodbioLicenseModel :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbioLicenseModel =
    lens _dodbioLicenseModel (\s a -> s { _dodbioLicenseModel = a })

-- | The VPC filter value. Specify this parameter to show only the available VPC
-- or non-VPC offerings.
dodbioVpc :: Lens' DescribeOrderableDBInstanceOptions (Maybe Bool)
dodbioVpc = lens _dodbioVpc (\s a -> s { _dodbioVpc = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dodbioMaxRecords :: Lens' DescribeOrderableDBInstanceOptions (Maybe Integer)
dodbioMaxRecords =
    lens _dodbioMaxRecords (\s a -> s { _dodbioMaxRecords = a })

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords .
dodbioMarker :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbioMarker = lens _dodbioMarker (\s a -> s { _dodbioMarker = a })

instance ToQuery DescribeOrderableDBInstanceOptions where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- DescribeOrderableDBInstanceOptions action.
data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse
    { _dodbiorsOrderableDBInstanceOptions :: [OrderableDBInstanceOption]
    , _dodbiorsMarker :: Maybe Text
    } deriving (Show, Generic)

-- | An OrderableDBInstanceOption structure containing information about
-- orderable options for the DB instance.
dodbiorsOrderableDBInstanceOptions :: Lens' DescribeOrderableDBInstanceOptionsResponse [OrderableDBInstanceOption]
dodbiorsOrderableDBInstanceOptions =
    lens _dodbiorsOrderableDBInstanceOptions
         (\s a -> s { _dodbiorsOrderableDBInstanceOptions = a })

-- | An optional pagination token provided by a previous
-- OrderableDBInstanceOptions request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value specified
-- by MaxRecords .
dodbiorsMarker :: Lens' DescribeOrderableDBInstanceOptionsResponse (Maybe Text)
dodbiorsMarker = lens _dodbiorsMarker (\s a -> s { _dodbiorsMarker = a })

instance FromXML DescribeOrderableDBInstanceOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeOrderableDBInstanceOptions where
    type Sv DescribeOrderableDBInstanceOptions = RDS
    type Rs DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptionsResponse

    request = post "DescribeOrderableDBInstanceOptions"
    response _ = xmlResponse

instance AWSPager DescribeOrderableDBInstanceOptions where
    next rq rs = (\x -> rq & dodbioMarker ?~ x)
        <$> (rs ^. dodbiorsMarker)
