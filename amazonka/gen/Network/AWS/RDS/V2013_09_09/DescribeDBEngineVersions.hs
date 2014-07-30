{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeDBEngineVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the available DB engines. https://rds.amazonaws.com/
-- ?Action=DescribeDBEngineVersions &MaxRecords=100 &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T07%3A34%3A17.435Z &AWSAccessKeyId= &Signature=
-- mysql5.1 mysql 5.1.42 mysql5.1 mysql Use instead of mysql5.1 5.1.45 yaSSL
-- Security Fixes mysql5.1 mysql Use instead of mysql5.1 5.1.47 MySQL
-- 5.1.47.R1 with InnoDB Plugin 1.0.8 mysql5.1 mysql Use instead of mysql5.1
-- 5.1.48 MySQL 5.1.47.R1 with InnoDB Plugin 1.0.8 mysql5.1 mysql Use instead
-- of mysql5.1 5.1.49 MySQL 5.1.49-R1 with innodb plugin mysql5.1 mysql Use
-- instead of mysql5.1 5.1.50 MySQL 5.1.50-R3 mysql5.5 mysql Use instead of
-- mysql5.1 5.5.7 MySQL 5.5.7.R1 oracle-ee-11.2 oracle-ee Oracle Database
-- Server EE 11.2.0.2 Oracle EE release AL32UTF8 Unicode 5.0 UTF-8 Universal
-- character set oracle-ee-11.2 oracle-ee Oracle Database Server EE
-- 11.2.0.2.v2 First Oracle Enterprise Edition One - DB Engine Version
-- 11.2.0.2.v2 AL32UTF8 Unicode 5.0 UTF-8 Universal character set
-- oracle-ee-11.2 oracle-ee Oracle Database Server EE 11.2.0.2.v3 Oracle EE
-- release AL32UTF8 Unicode 5.0 UTF-8 Universal character set oracle-se-11.2
-- oracle-se Oracle Database Server SE 11.2.0.2 Oracle SE release AL32UTF8
-- Unicode 5.0 UTF-8 Universal character set oracle-se-11.2 oracle-se Oracle
-- Database Server SE 11.2.0.2.v2 Oracle SE release AL32UTF8 Unicode 5.0 UTF-8
-- Universal character set oracle-se-11.2 oracle-se Oracle Database Server SE
-- 11.2.0.2.v3 Oracle SE release AL32UTF8 Unicode 5.0 UTF-8 Universal
-- character set oracle-se1-11.2 oracle-se1 Oracle Database Server SE1
-- 11.2.0.2 Oracle SE1 release AL32UTF8 Unicode 5.0 UTF-8 Universal character
-- set oracle-se1-11.2 oracle-se1 Oracle Database Server SE1 11.2.0.2.v2
-- Oracle SE1 release AL32UTF8 Unicode 5.0 UTF-8 Universal character set
-- oracle-se1-11.2 oracle-se1 Oracle Database Server SE1 11.2.0.2.v3 Oracle
-- SE1 release AL32UTF8 Unicode 5.0 UTF-8 Universal character set
-- 1162dc55-850f-11e0-90aa-eb648410240d.
module Network.AWS.RDS.V2013_09_09.DescribeDBEngineVersions where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.RDS.V2013_09_09.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeDBEngineVersions' request.
describeDBEngineVersions :: DescribeDBEngineVersions
describeDBEngineVersions = DescribeDBEngineVersions
    { _ddbevmDefaultOnly = Nothing
    , _ddbevmListSupportedCharacterSets = Nothing
    , _ddbevmMaxRecords = Nothing
    , _ddbevmEngineVersion = Nothing
    , _ddbevmEngine = Nothing
    , _ddbevmDBParameterGroupFamily = Nothing
    , _ddbevmMarker = Nothing
    }

data DescribeDBEngineVersions = DescribeDBEngineVersions
    { _ddbevmDefaultOnly :: Maybe Bool
      -- ^ Indicates that only the default version of the specified engine
      -- or engine and major version combination is returned.
    , _ddbevmListSupportedCharacterSets :: Maybe Bool
      -- ^ If this parameter is specified, and if the requested engine
      -- supports the CharacterSetName parameter for CreateDBInstance, the
      -- response includes a list of supported character sets for each
      -- engine version.
    , _ddbevmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- than the MaxRecords value is available, a pagination token called
      -- a marker is included in the response so that the following
      -- results can be retrieved. Default: 100 Constraints: minimum 20,
      -- maximum 100.
    , _ddbevmEngineVersion :: Maybe Text
      -- ^ The database engine version to return. Example: 5.1.49.
    , _ddbevmEngine :: Maybe Text
      -- ^ The database engine to return.
    , _ddbevmDBParameterGroupFamily :: Maybe Text
      -- ^ The name of a specific DB parameter group family to return
      -- details for. Constraints: Must be 1 to 255 alphanumeric
      -- characters First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens.
    , _ddbevmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Generic)

instance ToQuery DescribeDBEngineVersions where
    toQuery = genericToQuery def

instance AWSRequest DescribeDBEngineVersions where
    type Sv DescribeDBEngineVersions = RDS
    type Rs DescribeDBEngineVersions = DescribeDBEngineVersionsResponse

    request = post "DescribeDBEngineVersions"
    response _ = xmlResponse

instance AWSPager DescribeDBEngineVersions where
    next rq rs = (\x -> rq { _ddbevmMarker = Just x })
        <$> _dbevmMarker rs

data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse
    { _dbevmDBEngineVersions :: [DBEngineVersion]
      -- ^ A list of DBEngineVersion elements.
    , _dbevmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If
      -- this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    } deriving (Generic)

instance FromXML DescribeDBEngineVersionsResponse where
    fromXMLOptions = xmlOptions
