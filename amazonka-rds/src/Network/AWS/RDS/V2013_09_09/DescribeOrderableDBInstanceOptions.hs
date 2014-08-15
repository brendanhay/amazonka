{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.RDS.V2013_09_09.DescribeOrderableDBInstanceOptions where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeOrderableDBInstanceOptions' request.
describeOrderableDBInstanceOptions :: Text -- ^ '_dodbiomEngine'
                                   -> DescribeOrderableDBInstanceOptions
describeOrderableDBInstanceOptions p1 = DescribeOrderableDBInstanceOptions
    { _dodbiomEngine = p1
    , _dodbiomVpc = Nothing
    , _dodbiomMaxRecords = Nothing
    , _dodbiomEngineVersion = Nothing
    , _dodbiomDBInstanceClass = Nothing
    , _dodbiomLicenseModel = Nothing
    , _dodbiomMarker = Nothing
    }

data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions
    { _dodbiomEngine :: Text
      -- ^ The name of the engine to retrieve DB instance options for.
    , _dodbiomVpc :: Maybe Bool
      -- ^ The VPC filter value. Specify this parameter to show only the
      -- available VPC or non-VPC offerings.
    , _dodbiomMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a pagination
      -- token called a marker is included in the response so that the
      -- remaining results can be retrieved. Default: 100 Constraints:
      -- minimum 20, maximum 100.
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
    , _dodbiomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableDBInstanceOptions request. If this parameter is
      -- specified, the response includes only records beyond the marker,
      -- up to the value specified by MaxRecords .
    } deriving (Show, Generic)

makeLenses ''DescribeOrderableDBInstanceOptions

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

makeLenses ''DescribeOrderableDBInstanceOptionsResponse

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
