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
    , mkDescribeOrderableDBInstanceOptionsMessage
    -- ** Request lenses
    , dodbiomEngine
    , dodbiomEngineVersion
    , dodbiomDBInstanceClass
    , dodbiomLicenseModel
    , dodbiomVpc
    , dodbiomMaxRecords
    , dodbiomMarker

    -- * Response
    , DescribeOrderableDBInstanceOptionsResponse
    -- ** Response lenses
    , odbiomOrderableDBInstanceOptions
    , odbiomMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeOrderableDBInstanceOptions' request.
mkDescribeOrderableDBInstanceOptionsMessage :: Text -- ^ 'dodbiomEngine'
                                            -> DescribeOrderableDBInstanceOptions
mkDescribeOrderableDBInstanceOptionsMessage p1 = DescribeOrderableDBInstanceOptions
    { _dodbiomEngine = p1
    , _dodbiomEngineVersion = Nothing
    , _dodbiomDBInstanceClass = Nothing
    , _dodbiomLicenseModel = Nothing
    , _dodbiomVpc = Nothing
    , _dodbiomMaxRecords = Nothing
    , _dodbiomMarker = Nothing
    }
{-# INLINE mkDescribeOrderableDBInstanceOptionsMessage #-}

data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions
    { _dodbiomEngine :: Text
      -- ^ The name of the engine to retrieve DB instance options for.
    , _dodbiomEngineVersion :: Maybe Text
      -- ^ The engine version filter value. Specify this parameter to show
      -- only the available offerings matching the specified engine
      -- version.
    , _dodbiomDBInstanceClass :: Maybe Text
      -- ^ The DB instance class filter value. Specify this parameter to
      -- show only the available offerings matching the specified DB
      -- instance class.
    , _dodbiomLicenseModel :: Maybe Text
      -- ^ The license model filter value. Specify this parameter to show
      -- only the available offerings matching the specified license
      -- model.
    , _dodbiomVpc :: Maybe Bool
      -- ^ The VPC filter value. Specify this parameter to show only the
      -- available VPC or non-VPC offerings.
    , _dodbiomMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results can be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
    , _dodbiomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableDBInstanceOptions request. If this parameter is
      -- specified, the response includes only records beyond the marker,
      -- up to the value specified by MaxRecords .
    } deriving (Show, Generic)

-- | The name of the engine to retrieve DB instance options for.
dodbiomEngine :: Lens' DescribeOrderableDBInstanceOptions (Text)
dodbiomEngine = lens _dodbiomEngine (\s a -> s { _dodbiomEngine = a })
{-# INLINE dodbiomEngine #-}

-- | The engine version filter value. Specify this parameter to show only the
-- available offerings matching the specified engine version.
dodbiomEngineVersion :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbiomEngineVersion = lens _dodbiomEngineVersion (\s a -> s { _dodbiomEngineVersion = a })
{-# INLINE dodbiomEngineVersion #-}

-- | The DB instance class filter value. Specify this parameter to show only the
-- available offerings matching the specified DB instance class.
dodbiomDBInstanceClass :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbiomDBInstanceClass = lens _dodbiomDBInstanceClass (\s a -> s { _dodbiomDBInstanceClass = a })
{-# INLINE dodbiomDBInstanceClass #-}

-- | The license model filter value. Specify this parameter to show only the
-- available offerings matching the specified license model.
dodbiomLicenseModel :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbiomLicenseModel = lens _dodbiomLicenseModel (\s a -> s { _dodbiomLicenseModel = a })
{-# INLINE dodbiomLicenseModel #-}

-- | The VPC filter value. Specify this parameter to show only the available VPC
-- or non-VPC offerings.
dodbiomVpc :: Lens' DescribeOrderableDBInstanceOptions (Maybe Bool)
dodbiomVpc = lens _dodbiomVpc (\s a -> s { _dodbiomVpc = a })
{-# INLINE dodbiomVpc #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dodbiomMaxRecords :: Lens' DescribeOrderableDBInstanceOptions (Maybe Integer)
dodbiomMaxRecords = lens _dodbiomMaxRecords (\s a -> s { _dodbiomMaxRecords = a })
{-# INLINE dodbiomMaxRecords #-}

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords .
dodbiomMarker :: Lens' DescribeOrderableDBInstanceOptions (Maybe Text)
dodbiomMarker = lens _dodbiomMarker (\s a -> s { _dodbiomMarker = a })
{-# INLINE dodbiomMarker #-}

instance ToQuery DescribeOrderableDBInstanceOptions where
    toQuery = genericQuery def

data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse
    { _odbiomOrderableDBInstanceOptions :: [OrderableDBInstanceOption]
      -- ^ An OrderableDBInstanceOption structure containing information
      -- about orderable options for the DB instance.
    , _odbiomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- OrderableDBInstanceOptions request. If this parameter is
      -- specified, the response includes only records beyond the marker,
      -- up to the value specified by MaxRecords .
    } deriving (Show, Generic)

-- | An OrderableDBInstanceOption structure containing information about
-- orderable options for the DB instance.
odbiomOrderableDBInstanceOptions :: Lens' DescribeOrderableDBInstanceOptionsResponse ([OrderableDBInstanceOption])
odbiomOrderableDBInstanceOptions = lens _odbiomOrderableDBInstanceOptions (\s a -> s { _odbiomOrderableDBInstanceOptions = a })
{-# INLINE odbiomOrderableDBInstanceOptions #-}

-- | An optional pagination token provided by a previous
-- OrderableDBInstanceOptions request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value specified
-- by MaxRecords .
odbiomMarker :: Lens' DescribeOrderableDBInstanceOptionsResponse (Maybe Text)
odbiomMarker = lens _odbiomMarker (\s a -> s { _odbiomMarker = a })
{-# INLINE odbiomMarker #-}

instance FromXML DescribeOrderableDBInstanceOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeOrderableDBInstanceOptions where
    type Sv DescribeOrderableDBInstanceOptions = RDS
    type Rs DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptionsResponse

    request = post "DescribeOrderableDBInstanceOptions"
    response _ = xmlResponse

instance AWSPager DescribeOrderableDBInstanceOptions where
    next rq rs = (\x -> rq { _dodbiomMarker = Just x })
        <$> (_odbiomMarker rs)
