{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.DescribeDBEngineVersions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    , ddevEngineVersion
    , ddevDefaultOnly
    , ddevFilters
    , ddevEngine
    , ddevDBParameterGroupFamily
    , ddevListSupportedCharacterSets
    , ddevMaxRecords
    , ddevMarker

    -- * Response
    , DescribeDBEngineVersionsResponse
    -- ** Response constructor
    , describeDBEngineVersionsResponse
    -- ** Response lenses
    , ddevrMarker
    , ddevrDBEngineVersions
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.RDS.Types

-- | /See:/ 'describeDBEngineVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddevEngineVersion'
--
-- * 'ddevDefaultOnly'
--
-- * 'ddevFilters'
--
-- * 'ddevEngine'
--
-- * 'ddevDBParameterGroupFamily'
--
-- * 'ddevListSupportedCharacterSets'
--
-- * 'ddevMaxRecords'
--
-- * 'ddevMarker'
data DescribeDBEngineVersions = DescribeDBEngineVersions'{_ddevEngineVersion :: Maybe Text, _ddevDefaultOnly :: Maybe Bool, _ddevFilters :: Maybe [Filter], _ddevEngine :: Maybe Text, _ddevDBParameterGroupFamily :: Maybe Text, _ddevListSupportedCharacterSets :: Maybe Bool, _ddevMaxRecords :: Maybe Int, _ddevMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeDBEngineVersions' smart constructor.
describeDBEngineVersions :: DescribeDBEngineVersions
describeDBEngineVersions = DescribeDBEngineVersions'{_ddevEngineVersion = Nothing, _ddevDefaultOnly = Nothing, _ddevFilters = Nothing, _ddevEngine = Nothing, _ddevDBParameterGroupFamily = Nothing, _ddevListSupportedCharacterSets = Nothing, _ddevMaxRecords = Nothing, _ddevMarker = Nothing};

-- | The database engine version to return.
--
-- Example: @5.1.49@
ddevEngineVersion :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevEngineVersion = lens _ddevEngineVersion (\ s a -> s{_ddevEngineVersion = a});

-- | Indicates that only the default version of the specified engine or
-- engine and major version combination is returned.
ddevDefaultOnly :: Lens' DescribeDBEngineVersions (Maybe Bool)
ddevDefaultOnly = lens _ddevDefaultOnly (\ s a -> s{_ddevDefaultOnly = a});

-- | Not currently supported.
ddevFilters :: Lens' DescribeDBEngineVersions (Maybe [Filter])
ddevFilters = lens _ddevFilters (\ s a -> s{_ddevFilters = a});

-- | The database engine to return.
ddevEngine :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevEngine = lens _ddevEngine (\ s a -> s{_ddevEngine = a});

-- | The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddevDBParameterGroupFamily :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevDBParameterGroupFamily = lens _ddevDBParameterGroupFamily (\ s a -> s{_ddevDBParameterGroupFamily = a});

-- | If this parameter is specified, and if the requested engine supports the
-- CharacterSetName parameter for CreateDBInstance, the response includes a
-- list of supported character sets for each engine version.
ddevListSupportedCharacterSets :: Lens' DescribeDBEngineVersions (Maybe Bool)
ddevListSupportedCharacterSets = lens _ddevListSupportedCharacterSets (\ s a -> s{_ddevListSupportedCharacterSets = a});

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so that the following results can be
-- retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddevMaxRecords :: Lens' DescribeDBEngineVersions (Maybe Int)
ddevMaxRecords = lens _ddevMaxRecords (\ s a -> s{_ddevMaxRecords = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddevMarker :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevMarker = lens _ddevMarker (\ s a -> s{_ddevMarker = a});

instance AWSRequest DescribeDBEngineVersions where
        type Sv DescribeDBEngineVersions = RDS
        type Rs DescribeDBEngineVersions =
             DescribeDBEngineVersionsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeDBEngineVersionsResult"
              (\ s h x ->
                 DescribeDBEngineVersionsResponse' <$>
                   x .@? "Marker" <*>
                     (x .@? "DBEngineVersions" .!@ mempty >>=
                        parseXMLList "DBEngineVersion"))

instance ToHeaders DescribeDBEngineVersions where
        toHeaders = const mempty

instance ToPath DescribeDBEngineVersions where
        toPath = const "/"

instance ToQuery DescribeDBEngineVersions where
        toQuery DescribeDBEngineVersions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDBEngineVersions" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _ddevEngineVersion,
               "DefaultOnly" =: _ddevDefaultOnly,
               "Filters" =: "Filter" =: _ddevFilters,
               "Engine" =: _ddevEngine,
               "DBParameterGroupFamily" =:
                 _ddevDBParameterGroupFamily,
               "ListSupportedCharacterSets" =:
                 _ddevListSupportedCharacterSets,
               "MaxRecords" =: _ddevMaxRecords,
               "Marker" =: _ddevMarker]

-- | /See:/ 'describeDBEngineVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddevrMarker'
--
-- * 'ddevrDBEngineVersions'
data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse'{_ddevrMarker :: Maybe Text, _ddevrDBEngineVersions :: Maybe [DBEngineVersion]} deriving (Eq, Read, Show)

-- | 'DescribeDBEngineVersionsResponse' smart constructor.
describeDBEngineVersionsResponse :: DescribeDBEngineVersionsResponse
describeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse'{_ddevrMarker = Nothing, _ddevrDBEngineVersions = Nothing};

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddevrMarker :: Lens' DescribeDBEngineVersionsResponse (Maybe Text)
ddevrMarker = lens _ddevrMarker (\ s a -> s{_ddevrMarker = a});

-- | A list of @DBEngineVersion@ elements.
ddevrDBEngineVersions :: Lens' DescribeDBEngineVersionsResponse (Maybe [DBEngineVersion])
ddevrDBEngineVersions = lens _ddevrDBEngineVersions (\ s a -> s{_ddevrDBEngineVersions = a});
