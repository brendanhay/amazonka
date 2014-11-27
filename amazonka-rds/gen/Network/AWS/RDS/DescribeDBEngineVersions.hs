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

-- Module      : Network.AWS.RDS.DescribeDBEngineVersions
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

-- | Returns a list of the available DB engines.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBEngineVersions.html>
module Network.AWS.RDS.DescribeDBEngineVersions
    (
    -- * Request
      DescribeDBEngineVersions
    -- ** Request constructor
    , describeDBEngineVersions
    -- ** Request lenses
    , ddbevDBParameterGroupFamily
    , ddbevDefaultOnly
    , ddbevEngine
    , ddbevEngineVersion
    , ddbevFilters
    , ddbevListSupportedCharacterSets
    , ddbevMarker
    , ddbevMaxRecords

    -- * Response
    , DescribeDBEngineVersionsResponse
    -- ** Response constructor
    , describeDBEngineVersionsResponse
    -- ** Response lenses
    , ddbevrDBEngineVersions
    , ddbevrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeDBEngineVersions = DescribeDBEngineVersions
    { _ddbevDBParameterGroupFamily     :: Maybe Text
    , _ddbevDefaultOnly                :: Maybe Bool
    , _ddbevEngine                     :: Maybe Text
    , _ddbevEngineVersion              :: Maybe Text
    , _ddbevFilters                    :: List "Filter" Filter
    , _ddbevListSupportedCharacterSets :: Maybe Bool
    , _ddbevMarker                     :: Maybe Text
    , _ddbevMaxRecords                 :: Maybe Int
    } deriving (Eq, Show)

-- | 'DescribeDBEngineVersions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbevDBParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'ddbevDefaultOnly' @::@ 'Maybe' 'Bool'
--
-- * 'ddbevEngine' @::@ 'Maybe' 'Text'
--
-- * 'ddbevEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'ddbevFilters' @::@ ['Filter']
--
-- * 'ddbevListSupportedCharacterSets' @::@ 'Maybe' 'Bool'
--
-- * 'ddbevMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbevMaxRecords' @::@ 'Maybe' 'Int'
--
describeDBEngineVersions :: DescribeDBEngineVersions
describeDBEngineVersions = DescribeDBEngineVersions
    { _ddbevEngine                     = Nothing
    , _ddbevEngineVersion              = Nothing
    , _ddbevDBParameterGroupFamily     = Nothing
    , _ddbevFilters                    = mempty
    , _ddbevMaxRecords                 = Nothing
    , _ddbevMarker                     = Nothing
    , _ddbevDefaultOnly                = Nothing
    , _ddbevListSupportedCharacterSets = Nothing
    }

-- | The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
-- Must be 1 to 255 alphanumeric characters First character must be a letter Cannot end with a hyphen or contain two consecutive hyphens
--
ddbevDBParameterGroupFamily :: Lens' DescribeDBEngineVersions (Maybe Text)
ddbevDBParameterGroupFamily =
    lens _ddbevDBParameterGroupFamily
        (\s a -> s { _ddbevDBParameterGroupFamily = a })

-- | Indicates that only the default version of the specified engine or engine
-- and major version combination is returned.
ddbevDefaultOnly :: Lens' DescribeDBEngineVersions (Maybe Bool)
ddbevDefaultOnly = lens _ddbevDefaultOnly (\s a -> s { _ddbevDefaultOnly = a })

-- | The database engine to return.
ddbevEngine :: Lens' DescribeDBEngineVersions (Maybe Text)
ddbevEngine = lens _ddbevEngine (\s a -> s { _ddbevEngine = a })

-- | The database engine version to return.
--
-- Example: '5.1.49'
ddbevEngineVersion :: Lens' DescribeDBEngineVersions (Maybe Text)
ddbevEngineVersion =
    lens _ddbevEngineVersion (\s a -> s { _ddbevEngineVersion = a })

-- | Not currently supported.
ddbevFilters :: Lens' DescribeDBEngineVersions [Filter]
ddbevFilters = lens _ddbevFilters (\s a -> s { _ddbevFilters = a }) . _List

-- | If this parameter is specified, and if the requested engine supports the
-- CharacterSetName parameter for CreateDBInstance, the response includes a list
-- of supported character sets for each engine version.
ddbevListSupportedCharacterSets :: Lens' DescribeDBEngineVersions (Maybe Bool)
ddbevListSupportedCharacterSets =
    lens _ddbevListSupportedCharacterSets
        (\s a -> s { _ddbevListSupportedCharacterSets = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the marker,
-- up to the value specified by 'MaxRecords'.
ddbevMarker :: Lens' DescribeDBEngineVersions (Maybe Text)
ddbevMarker = lens _ddbevMarker (\s a -> s { _ddbevMarker = a })

-- | The maximum number of records to include in the response. If more than the 'MaxRecords' value is available, a pagination token called a marker is included in the
-- response so that the following results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddbevMaxRecords :: Lens' DescribeDBEngineVersions (Maybe Int)
ddbevMaxRecords = lens _ddbevMaxRecords (\s a -> s { _ddbevMaxRecords = a })

data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse
    { _ddbevrDBEngineVersions :: List "DBEngineVersion" DBEngineVersion
    , _ddbevrMarker           :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeDBEngineVersionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbevrDBEngineVersions' @::@ ['DBEngineVersion']
--
-- * 'ddbevrMarker' @::@ 'Maybe' 'Text'
--
describeDBEngineVersionsResponse :: DescribeDBEngineVersionsResponse
describeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse
    { _ddbevrMarker           = Nothing
    , _ddbevrDBEngineVersions = mempty
    }

-- | A list of 'DBEngineVersion' elements.
ddbevrDBEngineVersions :: Lens' DescribeDBEngineVersionsResponse [DBEngineVersion]
ddbevrDBEngineVersions =
    lens _ddbevrDBEngineVersions (\s a -> s { _ddbevrDBEngineVersions = a })
        . _List

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the marker,
-- up to the value specified by 'MaxRecords'.
ddbevrMarker :: Lens' DescribeDBEngineVersionsResponse (Maybe Text)
ddbevrMarker = lens _ddbevrMarker (\s a -> s { _ddbevrMarker = a })

instance ToPath DescribeDBEngineVersions where
    toPath = const "/"

instance ToQuery DescribeDBEngineVersions where
    toQuery DescribeDBEngineVersions{..} = mconcat
        [ "DBParameterGroupFamily"     =? _ddbevDBParameterGroupFamily
        , "DefaultOnly"                =? _ddbevDefaultOnly
        , "Engine"                     =? _ddbevEngine
        , "EngineVersion"              =? _ddbevEngineVersion
        , "Filters"                    =? _ddbevFilters
        , "ListSupportedCharacterSets" =? _ddbevListSupportedCharacterSets
        , "Marker"                     =? _ddbevMarker
        , "MaxRecords"                 =? _ddbevMaxRecords
        ]

instance ToHeaders DescribeDBEngineVersions

instance AWSRequest DescribeDBEngineVersions where
    type Sv DescribeDBEngineVersions = RDS
    type Rs DescribeDBEngineVersions = DescribeDBEngineVersionsResponse

    request  = post "DescribeDBEngineVersions"
    response = xmlResponse

instance FromXML DescribeDBEngineVersionsResponse where
    parseXML = withElement "DescribeDBEngineVersionsResult" $ \x -> DescribeDBEngineVersionsResponse
        <$> x .@  "DBEngineVersions"
        <*> x .@? "Marker"

instance AWSPager DescribeDBEngineVersions where
    page rq rs
        | stop (rq ^. ddbevMarker) = Nothing
        | otherwise = (\x -> rq & ddbevMarker ?~ x)
            <$> (rs ^. ddbevrMarker)
