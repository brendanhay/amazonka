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

-- Module      : Network.AWS.RDS.DescribeDBEngineVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the available DB engines.
module Network.AWS.RDS.DescribeDBEngineVersions
    (
    -- * Request
      DescribeDBEngineVersionsMessage
    -- ** Request constructor
    , describeDBEngineVersionsMessage
    -- ** Request lenses
    , ddbevmDBParameterGroupFamily
    , ddbevmDefaultOnly
    , ddbevmEngine
    , ddbevmEngineVersion
    , ddbevmFilters
    , ddbevmListSupportedCharacterSets
    , ddbevmMarker
    , ddbevmMaxRecords

    -- * Response
    , DBEngineVersionMessage
    -- ** Response constructor
    , dbengineVersionMessage
    -- ** Response lenses
    , dbevmDBEngineVersions
    , dbevmMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DescribeDBEngineVersionsMessage = DescribeDBEngineVersionsMessage
    { _ddbevmDBParameterGroupFamily     :: Maybe Text
    , _ddbevmDefaultOnly                :: Maybe Bool
    , _ddbevmEngine                     :: Maybe Text
    , _ddbevmEngineVersion              :: Maybe Text
    , _ddbevmFilters                    :: [Filter]
    , _ddbevmListSupportedCharacterSets :: Maybe Bool
    , _ddbevmMarker                     :: Maybe Text
    , _ddbevmMaxRecords                 :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'DescribeDBEngineVersionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbevmDBParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'ddbevmDefaultOnly' @::@ 'Maybe' 'Bool'
--
-- * 'ddbevmEngine' @::@ 'Maybe' 'Text'
--
-- * 'ddbevmEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'ddbevmFilters' @::@ ['Filter']
--
-- * 'ddbevmListSupportedCharacterSets' @::@ 'Maybe' 'Bool'
--
-- * 'ddbevmMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbevmMaxRecords' @::@ 'Maybe' 'Int'
--
describeDBEngineVersionsMessage :: DescribeDBEngineVersionsMessage
describeDBEngineVersionsMessage = DescribeDBEngineVersionsMessage
    { _ddbevmEngine                     = Nothing
    , _ddbevmEngineVersion              = Nothing
    , _ddbevmDBParameterGroupFamily     = Nothing
    , _ddbevmFilters                    = mempty
    , _ddbevmMaxRecords                 = Nothing
    , _ddbevmMarker                     = Nothing
    , _ddbevmDefaultOnly                = Nothing
    , _ddbevmListSupportedCharacterSets = Nothing
    }

-- | The name of a specific DB parameter group family to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
ddbevmDBParameterGroupFamily :: Lens' DescribeDBEngineVersionsMessage (Maybe Text)
ddbevmDBParameterGroupFamily =
    lens _ddbevmDBParameterGroupFamily
        (\s a -> s { _ddbevmDBParameterGroupFamily = a })

-- | Indicates that only the default version of the specified engine or engine
-- and major version combination is returned.
ddbevmDefaultOnly :: Lens' DescribeDBEngineVersionsMessage (Maybe Bool)
ddbevmDefaultOnly =
    lens _ddbevmDefaultOnly (\s a -> s { _ddbevmDefaultOnly = a })

-- | The database engine to return.
ddbevmEngine :: Lens' DescribeDBEngineVersionsMessage (Maybe Text)
ddbevmEngine = lens _ddbevmEngine (\s a -> s { _ddbevmEngine = a })

-- | The database engine version to return. Example: 5.1.49.
ddbevmEngineVersion :: Lens' DescribeDBEngineVersionsMessage (Maybe Text)
ddbevmEngineVersion =
    lens _ddbevmEngineVersion (\s a -> s { _ddbevmEngineVersion = a })

-- | Not currently supported.
ddbevmFilters :: Lens' DescribeDBEngineVersionsMessage [Filter]
ddbevmFilters = lens _ddbevmFilters (\s a -> s { _ddbevmFilters = a })

-- | If this parameter is specified, and if the requested engine supports the
-- CharacterSetName parameter for CreateDBInstance, the response includes a
-- list of supported character sets for each engine version.
ddbevmListSupportedCharacterSets :: Lens' DescribeDBEngineVersionsMessage (Maybe Bool)
ddbevmListSupportedCharacterSets =
    lens _ddbevmListSupportedCharacterSets
        (\s a -> s { _ddbevmListSupportedCharacterSets = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbevmMarker :: Lens' DescribeDBEngineVersionsMessage (Maybe Text)
ddbevmMarker = lens _ddbevmMarker (\s a -> s { _ddbevmMarker = a })

-- | The maximum number of records to include in the response. If more than
-- the MaxRecords value is available, a pagination token called a marker is
-- included in the response so that the following results can be retrieved.
-- Default: 100 Constraints: minimum 20, maximum 100.
ddbevmMaxRecords :: Lens' DescribeDBEngineVersionsMessage (Maybe Int)
ddbevmMaxRecords = lens _ddbevmMaxRecords (\s a -> s { _ddbevmMaxRecords = a })

instance ToQuery DescribeDBEngineVersionsMessage

instance ToPath DescribeDBEngineVersionsMessage where
    toPath = const "/"

data DBEngineVersionMessage = DBEngineVersionMessage
    { _dbevmDBEngineVersions :: [DBEngineVersion]
    , _dbevmMarker           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DBEngineVersionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbevmDBEngineVersions' @::@ ['DBEngineVersion']
--
-- * 'dbevmMarker' @::@ 'Maybe' 'Text'
--
dbengineVersionMessage :: DBEngineVersionMessage
dbengineVersionMessage = DBEngineVersionMessage
    { _dbevmMarker           = Nothing
    , _dbevmDBEngineVersions = mempty
    }

-- | A list of DBEngineVersion elements.
dbevmDBEngineVersions :: Lens' DBEngineVersionMessage [DBEngineVersion]
dbevmDBEngineVersions =
    lens _dbevmDBEngineVersions (\s a -> s { _dbevmDBEngineVersions = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
dbevmMarker :: Lens' DBEngineVersionMessage (Maybe Text)
dbevmMarker = lens _dbevmMarker (\s a -> s { _dbevmMarker = a })

instance FromXML DBEngineVersionMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBEngineVersionMessage"

instance AWSRequest DescribeDBEngineVersionsMessage where
    type Sv DescribeDBEngineVersionsMessage = RDS
    type Rs DescribeDBEngineVersionsMessage = DBEngineVersionMessage

    request  = post "DescribeDBEngineVersions"
    response = xmlResponse $ \h x -> DBEngineVersionMessage
        <$> x %| "DBEngineVersions"
        <*> x %| "Marker"
