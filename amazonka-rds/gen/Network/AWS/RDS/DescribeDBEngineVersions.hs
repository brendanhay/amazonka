{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBEngineVersions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available DB engines.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBEngineVersions.html AWS API Reference> for DescribeDBEngineVersions.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBEngineVersions
    (
    -- * Creating a Request
      describeDBEngineVersions
    , DescribeDBEngineVersions
    -- * Request Lenses
    , ddevEngineVersion
    , ddevDefaultOnly
    , ddevFilters
    , ddevEngine
    , ddevDBParameterGroupFamily
    , ddevListSupportedCharacterSets
    , ddevMaxRecords
    , ddevMarker

    -- * Destructuring the Response
    , describeDBEngineVersionsResponse
    , DescribeDBEngineVersionsResponse
    -- * Response Lenses
    , ddevrsMarker
    , ddevrsDBEngineVersions
    , ddevrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDBEngineVersions' smart constructor.
data DescribeDBEngineVersions = DescribeDBEngineVersions'
    { _ddevEngineVersion              :: !(Maybe Text)
    , _ddevDefaultOnly                :: !(Maybe Bool)
    , _ddevFilters                    :: !(Maybe [Filter])
    , _ddevEngine                     :: !(Maybe Text)
    , _ddevDBParameterGroupFamily     :: !(Maybe Text)
    , _ddevListSupportedCharacterSets :: !(Maybe Bool)
    , _ddevMaxRecords                 :: !(Maybe Int)
    , _ddevMarker                     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBEngineVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
describeDBEngineVersions
    :: DescribeDBEngineVersions
describeDBEngineVersions =
    DescribeDBEngineVersions'
    { _ddevEngineVersion = Nothing
    , _ddevDefaultOnly = Nothing
    , _ddevFilters = Nothing
    , _ddevEngine = Nothing
    , _ddevDBParameterGroupFamily = Nothing
    , _ddevListSupportedCharacterSets = Nothing
    , _ddevMaxRecords = Nothing
    , _ddevMarker = Nothing
    }

-- | The database engine version to return.
--
-- Example: '5.1.49'
ddevEngineVersion :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevEngineVersion = lens _ddevEngineVersion (\ s a -> s{_ddevEngineVersion = a});

-- | Indicates that only the default version of the specified engine or
-- engine and major version combination is returned.
ddevDefaultOnly :: Lens' DescribeDBEngineVersions (Maybe Bool)
ddevDefaultOnly = lens _ddevDefaultOnly (\ s a -> s{_ddevDefaultOnly = a});

-- | Not currently supported.
ddevFilters :: Lens' DescribeDBEngineVersions [Filter]
ddevFilters = lens _ddevFilters (\ s a -> s{_ddevFilters = a}) . _Default . _Coerce;

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
-- the 'MaxRecords' value is available, a pagination token called a marker
-- is included in the response so that the following results can be
-- retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
ddevMaxRecords :: Lens' DescribeDBEngineVersions (Maybe Int)
ddevMaxRecords = lens _ddevMaxRecords (\ s a -> s{_ddevMaxRecords = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by 'MaxRecords'.
ddevMarker :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevMarker = lens _ddevMarker (\ s a -> s{_ddevMarker = a});

instance AWSPager DescribeDBEngineVersions where
        page rq rs
          | stop (rs ^. ddevrsMarker) = Nothing
          | stop (rs ^. ddevrsDBEngineVersions) = Nothing
          | otherwise =
            Just $ rq & ddevMarker .~ rs ^. ddevrsMarker

instance AWSRequest DescribeDBEngineVersions where
        type Rs DescribeDBEngineVersions =
             DescribeDBEngineVersionsResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper "DescribeDBEngineVersionsResult"
              (\ s h x ->
                 DescribeDBEngineVersionsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "DBEngineVersions" .!@ mempty >>=
                        may (parseXMLList "DBEngineVersion"))
                     <*> (pure (fromEnum s)))

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
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddevFilters),
               "Engine" =: _ddevEngine,
               "DBParameterGroupFamily" =:
                 _ddevDBParameterGroupFamily,
               "ListSupportedCharacterSets" =:
                 _ddevListSupportedCharacterSets,
               "MaxRecords" =: _ddevMaxRecords,
               "Marker" =: _ddevMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBEngineVersions action.
--
-- /See:/ 'describeDBEngineVersionsResponse' smart constructor.
data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse'
    { _ddevrsMarker           :: !(Maybe Text)
    , _ddevrsDBEngineVersions :: !(Maybe [DBEngineVersion])
    , _ddevrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBEngineVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddevrsMarker'
--
-- * 'ddevrsDBEngineVersions'
--
-- * 'ddevrsStatus'
describeDBEngineVersionsResponse
    :: Int -- ^ 'ddevrsStatus'
    -> DescribeDBEngineVersionsResponse
describeDBEngineVersionsResponse pStatus_ =
    DescribeDBEngineVersionsResponse'
    { _ddevrsMarker = Nothing
    , _ddevrsDBEngineVersions = Nothing
    , _ddevrsStatus = pStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by 'MaxRecords'.
ddevrsMarker :: Lens' DescribeDBEngineVersionsResponse (Maybe Text)
ddevrsMarker = lens _ddevrsMarker (\ s a -> s{_ddevrsMarker = a});

-- | A list of 'DBEngineVersion' elements.
ddevrsDBEngineVersions :: Lens' DescribeDBEngineVersionsResponse [DBEngineVersion]
ddevrsDBEngineVersions = lens _ddevrsDBEngineVersions (\ s a -> s{_ddevrsDBEngineVersions = a}) . _Default . _Coerce;

-- | The response status code.
ddevrsStatus :: Lens' DescribeDBEngineVersionsResponse Int
ddevrsStatus = lens _ddevrsStatus (\ s a -> s{_ddevrsStatus = a});
