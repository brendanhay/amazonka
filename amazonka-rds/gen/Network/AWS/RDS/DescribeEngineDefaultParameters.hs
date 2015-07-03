{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the default engine and system parameter information for the
-- specified database engine.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEngineDefaultParameters.html>
module Network.AWS.RDS.DescribeEngineDefaultParameters
    (
    -- * Request
      DescribeEngineDefaultParameters
    -- ** Request constructor
    , describeEngineDefaultParameters
    -- ** Request lenses
    , dedpFilters
    , dedpMaxRecords
    , dedpMarker
    , dedpDBParameterGroupFamily

    -- * Response
    , DescribeEngineDefaultParametersResponse
    -- ** Response constructor
    , describeEngineDefaultParametersResponse
    -- ** Response lenses
    , dedprStatus
    , dedprEngineDefaults
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeEngineDefaultParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedpFilters'
--
-- * 'dedpMaxRecords'
--
-- * 'dedpMarker'
--
-- * 'dedpDBParameterGroupFamily'
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
    { _dedpFilters                :: !(Maybe [Filter])
    , _dedpMaxRecords             :: !(Maybe Int)
    , _dedpMarker                 :: !(Maybe Text)
    , _dedpDBParameterGroupFamily :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeEngineDefaultParameters' smart constructor.
describeEngineDefaultParameters :: Text -> DescribeEngineDefaultParameters
describeEngineDefaultParameters pDBParameterGroupFamily =
    DescribeEngineDefaultParameters'
    { _dedpFilters = Nothing
    , _dedpMaxRecords = Nothing
    , _dedpMarker = Nothing
    , _dedpDBParameterGroupFamily = pDBParameterGroupFamily
    }

-- | Not currently supported.
dedpFilters :: Lens' DescribeEngineDefaultParameters [Filter]
dedpFilters = lens _dedpFilters (\ s a -> s{_dedpFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dedpMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Int)
dedpMaxRecords = lens _dedpMaxRecords (\ s a -> s{_dedpMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribeEngineDefaultParameters@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
dedpMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpMarker = lens _dedpMarker (\ s a -> s{_dedpMarker = a});

-- | The name of the DB parameter group family.
dedpDBParameterGroupFamily :: Lens' DescribeEngineDefaultParameters Text
dedpDBParameterGroupFamily = lens _dedpDBParameterGroupFamily (\ s a -> s{_dedpDBParameterGroupFamily = a});

instance AWSPager DescribeEngineDefaultParameters
         where
        page rq rs
          | stop (rs ^? dedprEngineDefaults . edMarker . _Just)
            = Nothing
          | stop (rs ^. dedprEngineDefaults . edParameters) =
            Nothing
          | otherwise =
            Just $ rq &
              dedpMarker .~
                rs ^? dedprEngineDefaults . edMarker . _Just

instance AWSRequest DescribeEngineDefaultParameters
         where
        type Sv DescribeEngineDefaultParameters = RDS
        type Rs DescribeEngineDefaultParameters =
             DescribeEngineDefaultParametersResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeEngineDefaultParametersResult"
              (\ s h x ->
                 DescribeEngineDefaultParametersResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "EngineDefaults"))

instance ToHeaders DescribeEngineDefaultParameters
         where
        toHeaders = const mempty

instance ToPath DescribeEngineDefaultParameters where
        toPath = const "/"

instance ToQuery DescribeEngineDefaultParameters
         where
        toQuery DescribeEngineDefaultParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEngineDefaultParameters" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dedpFilters),
               "MaxRecords" =: _dedpMaxRecords,
               "Marker" =: _dedpMarker,
               "DBParameterGroupFamily" =:
                 _dedpDBParameterGroupFamily]

-- | /See:/ 'describeEngineDefaultParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedprStatus'
--
-- * 'dedprEngineDefaults'
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
    { _dedprStatus         :: !Int
    , _dedprEngineDefaults :: !EngineDefaults
    } deriving (Eq,Read,Show)

-- | 'DescribeEngineDefaultParametersResponse' smart constructor.
describeEngineDefaultParametersResponse :: Int -> EngineDefaults -> DescribeEngineDefaultParametersResponse
describeEngineDefaultParametersResponse pStatus pEngineDefaults =
    DescribeEngineDefaultParametersResponse'
    { _dedprStatus = pStatus
    , _dedprEngineDefaults = pEngineDefaults
    }

-- | FIXME: Undocumented member.
dedprStatus :: Lens' DescribeEngineDefaultParametersResponse Int
dedprStatus = lens _dedprStatus (\ s a -> s{_dedprStatus = a});

-- | FIXME: Undocumented member.
dedprEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse EngineDefaults
dedprEngineDefaults = lens _dedprEngineDefaults (\ s a -> s{_dedprEngineDefaults = a});
