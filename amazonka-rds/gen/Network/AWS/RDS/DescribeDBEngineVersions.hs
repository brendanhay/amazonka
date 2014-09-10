{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS
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
module Network.AWS.RDS
    (
    -- * Request
      DescribeDBEngineVersions
    -- ** Request constructor
    , mkDescribeDBEngineVersions
    -- ** Request lenses
    , ddbevEngine
    , ddbevEngineVersion
    , ddbevDBParameterGroupFamily
    , ddbevMaxRecords
    , ddbevMarker
    , ddbevDefaultOnly
    , ddbevListSupportedCharacterSets

    -- * Response
    , DescribeDBEngineVersionsResponse
    -- ** Response constructor
    , mkDescribeDBEngineVersionsResponse
    -- ** Response lenses
    , ddbevrMarker
    , ddbevrDBEngineVersions
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

data DescribeDBEngineVersions = DescribeDBEngineVersions
    { _ddbevEngine :: !(Maybe Text)
    , _ddbevEngineVersion :: !(Maybe Text)
    , _ddbevDBParameterGroupFamily :: !(Maybe Text)
    , _ddbevMaxRecords :: !(Maybe Integer)
    , _ddbevMarker :: !(Maybe Text)
    , _ddbevDefaultOnly :: !(Maybe Bool)
    , _ddbevListSupportedCharacterSets :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBEngineVersions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Engine ::@ @Maybe Text@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @DBParameterGroupFamily ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @DefaultOnly ::@ @Maybe Bool@
--
-- * @ListSupportedCharacterSets ::@ @Maybe Bool@
--
mkDescribeDBEngineVersions :: DescribeDBEngineVersions
mkDescribeDBEngineVersions = DescribeDBEngineVersions
    { _ddbevEngine = Nothing
    , _ddbevEngineVersion = Nothing
    , _ddbevDBParameterGroupFamily = Nothing
    , _ddbevMaxRecords = Nothing
    , _ddbevMarker = Nothing
    , _ddbevDefaultOnly = Nothing
    , _ddbevListSupportedCharacterSets = Nothing
    }

-- | The database engine to return.
ddbevEngine :: Lens' DescribeDBEngineVersions (Maybe Text)
ddbevEngine = lens _ddbevEngine (\s a -> s { _ddbevEngine = a })

-- | The database engine version to return. Example: 5.1.49.
ddbevEngineVersion :: Lens' DescribeDBEngineVersions (Maybe Text)
ddbevEngineVersion =
    lens _ddbevEngineVersion (\s a -> s { _ddbevEngineVersion = a })

-- | The name of a specific DB parameter group family to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character must
-- be a letter Cannot end with a hyphen or contain two consecutive hyphens.
ddbevDBParameterGroupFamily :: Lens' DescribeDBEngineVersions (Maybe Text)
ddbevDBParameterGroupFamily =
    lens _ddbevDBParameterGroupFamily
         (\s a -> s { _ddbevDBParameterGroupFamily = a })

-- | The maximum number of records to include in the response. If more than the
-- MaxRecords value is available, a pagination token called a marker is
-- included in the response so that the following results can be retrieved.
-- Default: 100 Constraints: minimum 20, maximum 100.
ddbevMaxRecords :: Lens' DescribeDBEngineVersions (Maybe Integer)
ddbevMaxRecords = lens _ddbevMaxRecords (\s a -> s { _ddbevMaxRecords = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbevMarker :: Lens' DescribeDBEngineVersions (Maybe Text)
ddbevMarker = lens _ddbevMarker (\s a -> s { _ddbevMarker = a })

-- | Indicates that only the default version of the specified engine or engine
-- and major version combination is returned.
ddbevDefaultOnly :: Lens' DescribeDBEngineVersions (Maybe Bool)
ddbevDefaultOnly =
    lens _ddbevDefaultOnly (\s a -> s { _ddbevDefaultOnly = a })

-- | If this parameter is specified, and if the requested engine supports the
-- CharacterSetName parameter for CreateDBInstance, the response includes a
-- list of supported character sets for each engine version.
ddbevListSupportedCharacterSets :: Lens' DescribeDBEngineVersions (Maybe Bool)
ddbevListSupportedCharacterSets =
    lens _ddbevListSupportedCharacterSets
         (\s a -> s { _ddbevListSupportedCharacterSets = a })

instance ToQuery DescribeDBEngineVersions where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- DescribeDBEngineVersions action.
data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse
    { _ddbevrMarker :: !(Maybe Text)
    , _ddbevrDBEngineVersions :: [DBEngineVersion]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDBEngineVersionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @DBEngineVersions ::@ @[DBEngineVersion]@
--
mkDescribeDBEngineVersionsResponse :: DescribeDBEngineVersionsResponse
mkDescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse
    { _ddbevrMarker = Nothing
    , _ddbevrDBEngineVersions = mempty
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbevrMarker :: Lens' DescribeDBEngineVersionsResponse (Maybe Text)
ddbevrMarker = lens _ddbevrMarker (\s a -> s { _ddbevrMarker = a })

-- | A list of DBEngineVersion elements.
ddbevrDBEngineVersions :: Lens' DescribeDBEngineVersionsResponse [DBEngineVersion]
ddbevrDBEngineVersions =
    lens _ddbevrDBEngineVersions (\s a -> s { _ddbevrDBEngineVersions = a })

instance FromXML DescribeDBEngineVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeDBEngineVersions where
    type Sv DescribeDBEngineVersions = RDS
    type Rs DescribeDBEngineVersions = DescribeDBEngineVersionsResponse

    request = post "DescribeDBEngineVersions"
    response _ = xmlResponse

instance AWSPager DescribeDBEngineVersions where
    next rq rs = (\x -> rq & ddbevMarker ?~ x)
        <$> (rs ^. ddbevrMarker)
