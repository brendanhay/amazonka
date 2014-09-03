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
    , describeOrderableDBInstanceOptions
    -- ** Request lenses
    , dodbiomEngine
    , dodbiomVpc
    , dodbiomMaxRecords
    , dodbiomEngineVersion
    , dodbiomDBInstanceClass
    , dodbiomLicenseModel
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

-- | Minimum specification for a 'DescribeOrderableDBInstanceOptions' request.
describeOrderableDBInstanceOptions :: Text -- ^ 'dodbiomEngine'
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

-- | The name of the engine to retrieve DB instance options for.
dodbiomEngine
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeOrderableDBInstanceOptions
    -> f DescribeOrderableDBInstanceOptions
dodbiomEngine f x =
    (\y -> x { _dodbiomEngine = y })
       <$> f (_dodbiomEngine x)
{-# INLINE dodbiomEngine #-}

-- | The VPC filter value. Specify this parameter to show only the available VPC
-- or non-VPC offerings.
dodbiomVpc
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DescribeOrderableDBInstanceOptions
    -> f DescribeOrderableDBInstanceOptions
dodbiomVpc f x =
    (\y -> x { _dodbiomVpc = y })
       <$> f (_dodbiomVpc x)
{-# INLINE dodbiomVpc #-}

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
dodbiomMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeOrderableDBInstanceOptions
    -> f DescribeOrderableDBInstanceOptions
dodbiomMaxRecords f x =
    (\y -> x { _dodbiomMaxRecords = y })
       <$> f (_dodbiomMaxRecords x)
{-# INLINE dodbiomMaxRecords #-}

-- | The engine version filter value. Specify this parameter to show only the
-- available offerings matching the specified engine version.
dodbiomEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOrderableDBInstanceOptions
    -> f DescribeOrderableDBInstanceOptions
dodbiomEngineVersion f x =
    (\y -> x { _dodbiomEngineVersion = y })
       <$> f (_dodbiomEngineVersion x)
{-# INLINE dodbiomEngineVersion #-}

-- | The DB instance class filter value. Specify this parameter to show only the
-- available offerings matching the specified DB instance class.
dodbiomDBInstanceClass
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOrderableDBInstanceOptions
    -> f DescribeOrderableDBInstanceOptions
dodbiomDBInstanceClass f x =
    (\y -> x { _dodbiomDBInstanceClass = y })
       <$> f (_dodbiomDBInstanceClass x)
{-# INLINE dodbiomDBInstanceClass #-}

-- | The license model filter value. Specify this parameter to show only the
-- available offerings matching the specified license model.
dodbiomLicenseModel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOrderableDBInstanceOptions
    -> f DescribeOrderableDBInstanceOptions
dodbiomLicenseModel f x =
    (\y -> x { _dodbiomLicenseModel = y })
       <$> f (_dodbiomLicenseModel x)
{-# INLINE dodbiomLicenseModel #-}

-- | An optional pagination token provided by a previous
-- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords .
dodbiomMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOrderableDBInstanceOptions
    -> f DescribeOrderableDBInstanceOptions
dodbiomMarker f x =
    (\y -> x { _dodbiomMarker = y })
       <$> f (_dodbiomMarker x)
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
odbiomOrderableDBInstanceOptions
    :: Functor f
    => ([OrderableDBInstanceOption]
    -> f ([OrderableDBInstanceOption]))
    -> DescribeOrderableDBInstanceOptionsResponse
    -> f DescribeOrderableDBInstanceOptionsResponse
odbiomOrderableDBInstanceOptions f x =
    (\y -> x { _odbiomOrderableDBInstanceOptions = y })
       <$> f (_odbiomOrderableDBInstanceOptions x)
{-# INLINE odbiomOrderableDBInstanceOptions #-}

-- | An optional pagination token provided by a previous
-- OrderableDBInstanceOptions request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value specified
-- by MaxRecords .
odbiomMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOrderableDBInstanceOptionsResponse
    -> f DescribeOrderableDBInstanceOptionsResponse
odbiomMarker f x =
    (\y -> x { _odbiomMarker = y })
       <$> f (_odbiomMarker x)
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
