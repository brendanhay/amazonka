{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBEngineVersions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available DB engines.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBEngineVersions.html>
module Network.AWS.RDS.DescribeDBEngineVersions
    (
    -- * Request
      DescribeDBEngineVersions
    -- ** Request constructor
    , describeDBEngineVersions
    -- ** Request lenses
    , ddevrqEngineVersion
    , ddevrqDefaultOnly
    , ddevrqFilters
    , ddevrqEngine
    , ddevrqDBParameterGroupFamily
    , ddevrqListSupportedCharacterSets
    , ddevrqMaxRecords
    , ddevrqMarker

    -- * Response
    , DescribeDBEngineVersionsResponse
    -- ** Response constructor
    , describeDBEngineVersionsResponse
    -- ** Response lenses
    , ddevrsMarker
    , ddevrsDBEngineVersions
    , ddevrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeDBEngineVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddevrqEngineVersion'
--
-- * 'ddevrqDefaultOnly'
--
-- * 'ddevrqFilters'
--
-- * 'ddevrqEngine'
--
-- * 'ddevrqDBParameterGroupFamily'
--
-- * 'ddevrqListSupportedCharacterSets'
--
-- * 'ddevrqMaxRecords'
--
-- * 'ddevrqMarker'
data DescribeDBEngineVersions = DescribeDBEngineVersions'
    { _ddevrqEngineVersion              :: !(Maybe Text)
    , _ddevrqDefaultOnly                :: !(Maybe Bool)
    , _ddevrqFilters                    :: !(Maybe [Filter])
    , _ddevrqEngine                     :: !(Maybe Text)
    , _ddevrqDBParameterGroupFamily     :: !(Maybe Text)
    , _ddevrqListSupportedCharacterSets :: !(Maybe Bool)
    , _ddevrqMaxRecords                 :: !(Maybe Int)
    , _ddevrqMarker                     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBEngineVersions' smart constructor.
describeDBEngineVersions :: DescribeDBEngineVersions
describeDBEngineVersions =
    DescribeDBEngineVersions'
    { _ddevrqEngineVersion = Nothing
    , _ddevrqDefaultOnly = Nothing
    , _ddevrqFilters = Nothing
    , _ddevrqEngine = Nothing
    , _ddevrqDBParameterGroupFamily = Nothing
    , _ddevrqListSupportedCharacterSets = Nothing
    , _ddevrqMaxRecords = Nothing
    , _ddevrqMarker = Nothing
    }

-- | The database engine version to return.
--
-- Example: @5.1.49@
ddevrqEngineVersion :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevrqEngineVersion = lens _ddevrqEngineVersion (\ s a -> s{_ddevrqEngineVersion = a});

-- | Indicates that only the default version of the specified engine or
-- engine and major version combination is returned.
ddevrqDefaultOnly :: Lens' DescribeDBEngineVersions (Maybe Bool)
ddevrqDefaultOnly = lens _ddevrqDefaultOnly (\ s a -> s{_ddevrqDefaultOnly = a});

-- | Not currently supported.
ddevrqFilters :: Lens' DescribeDBEngineVersions [Filter]
ddevrqFilters = lens _ddevrqFilters (\ s a -> s{_ddevrqFilters = a}) . _Default;

-- | The database engine to return.
ddevrqEngine :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevrqEngine = lens _ddevrqEngine (\ s a -> s{_ddevrqEngine = a});

-- | The name of a specific DB parameter group family to return details for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddevrqDBParameterGroupFamily :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevrqDBParameterGroupFamily = lens _ddevrqDBParameterGroupFamily (\ s a -> s{_ddevrqDBParameterGroupFamily = a});

-- | If this parameter is specified, and if the requested engine supports the
-- CharacterSetName parameter for CreateDBInstance, the response includes a
-- list of supported character sets for each engine version.
ddevrqListSupportedCharacterSets :: Lens' DescribeDBEngineVersions (Maybe Bool)
ddevrqListSupportedCharacterSets = lens _ddevrqListSupportedCharacterSets (\ s a -> s{_ddevrqListSupportedCharacterSets = a});

-- | The maximum number of records to include in the response. If more than
-- the @MaxRecords@ value is available, a pagination token called a marker
-- is included in the response so that the following results can be
-- retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddevrqMaxRecords :: Lens' DescribeDBEngineVersions (Maybe Int)
ddevrqMaxRecords = lens _ddevrqMaxRecords (\ s a -> s{_ddevrqMaxRecords = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddevrqMarker :: Lens' DescribeDBEngineVersions (Maybe Text)
ddevrqMarker = lens _ddevrqMarker (\ s a -> s{_ddevrqMarker = a});

instance AWSPager DescribeDBEngineVersions where
        page rq rs
          | stop (rs ^. ddevrsMarker) = Nothing
          | stop (rs ^. ddevrsDBEngineVersions) = Nothing
          | otherwise =
            Just $ rq & ddevrqMarker .~ rs ^. ddevrsMarker

instance AWSRequest DescribeDBEngineVersions where
        type Sv DescribeDBEngineVersions = RDS
        type Rs DescribeDBEngineVersions =
             DescribeDBEngineVersionsResponse
        request = post
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
               "EngineVersion" =: _ddevrqEngineVersion,
               "DefaultOnly" =: _ddevrqDefaultOnly,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddevrqFilters),
               "Engine" =: _ddevrqEngine,
               "DBParameterGroupFamily" =:
                 _ddevrqDBParameterGroupFamily,
               "ListSupportedCharacterSets" =:
                 _ddevrqListSupportedCharacterSets,
               "MaxRecords" =: _ddevrqMaxRecords,
               "Marker" =: _ddevrqMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBEngineVersions action.
--
-- /See:/ 'describeDBEngineVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddevrsMarker'
--
-- * 'ddevrsDBEngineVersions'
--
-- * 'ddevrsStatus'
data DescribeDBEngineVersionsResponse = DescribeDBEngineVersionsResponse'
    { _ddevrsMarker           :: !(Maybe Text)
    , _ddevrsDBEngineVersions :: !(Maybe [DBEngineVersion])
    , _ddevrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBEngineVersionsResponse' smart constructor.
describeDBEngineVersionsResponse :: Int -> DescribeDBEngineVersionsResponse
describeDBEngineVersionsResponse pStatus_ =
    DescribeDBEngineVersionsResponse'
    { _ddevrsMarker = Nothing
    , _ddevrsDBEngineVersions = Nothing
    , _ddevrsStatus = pStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddevrsMarker :: Lens' DescribeDBEngineVersionsResponse (Maybe Text)
ddevrsMarker = lens _ddevrsMarker (\ s a -> s{_ddevrsMarker = a});

-- | A list of @DBEngineVersion@ elements.
ddevrsDBEngineVersions :: Lens' DescribeDBEngineVersionsResponse [DBEngineVersion]
ddevrsDBEngineVersions = lens _ddevrsDBEngineVersions (\ s a -> s{_ddevrsDBEngineVersions = a}) . _Default;

-- | FIXME: Undocumented member.
ddevrsStatus :: Lens' DescribeDBEngineVersionsResponse Int
ddevrsStatus = lens _ddevrsStatus (\ s a -> s{_ddevrsStatus = a});
