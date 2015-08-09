{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeEngineDefaultClusterParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the
-- cluster database engine.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeEngineDefaultClusterParameters.html AWS API Reference> for DescribeEngineDefaultClusterParameters.
module Network.AWS.RDS.DescribeEngineDefaultClusterParameters
    (
    -- * Creating a Request
      DescribeEngineDefaultClusterParameters
    , describeEngineDefaultClusterParameters
    -- * Request Lenses
    , dedcpFilters
    , dedcpMaxRecords
    , dedcpMarker
    , dedcpDBParameterGroupFamily

    -- * Destructuring the Response
    , DescribeEngineDefaultClusterParametersResponse
    , describeEngineDefaultClusterParametersResponse
    -- * Response Lenses
    , dedcprsEngineDefaults
    , dedcprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeEngineDefaultClusterParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedcpFilters'
--
-- * 'dedcpMaxRecords'
--
-- * 'dedcpMarker'
--
-- * 'dedcpDBParameterGroupFamily'
data DescribeEngineDefaultClusterParameters = DescribeEngineDefaultClusterParameters'
    { _dedcpFilters                :: !(Maybe [Filter])
    , _dedcpMaxRecords             :: !(Maybe Int)
    , _dedcpMarker                 :: !(Maybe Text)
    , _dedcpDBParameterGroupFamily :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEngineDefaultClusterParameters' smart constructor.
describeEngineDefaultClusterParameters :: Text -> DescribeEngineDefaultClusterParameters
describeEngineDefaultClusterParameters pDBParameterGroupFamily_ =
    DescribeEngineDefaultClusterParameters'
    { _dedcpFilters = Nothing
    , _dedcpMaxRecords = Nothing
    , _dedcpMarker = Nothing
    , _dedcpDBParameterGroupFamily = pDBParameterGroupFamily_
    }

-- | This parameter is not currently supported.
dedcpFilters :: Lens' DescribeEngineDefaultClusterParameters [Filter]
dedcpFilters = lens _dedcpFilters (\ s a -> s{_dedcpFilters = a}) . _Default . _Coerce;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
dedcpMaxRecords :: Lens' DescribeEngineDefaultClusterParameters (Maybe Int)
dedcpMaxRecords = lens _dedcpMaxRecords (\ s a -> s{_dedcpMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribeEngineDefaultClusterParameters@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
dedcpMarker :: Lens' DescribeEngineDefaultClusterParameters (Maybe Text)
dedcpMarker = lens _dedcpMarker (\ s a -> s{_dedcpMarker = a});

-- | The name of the DB cluster parameter group family to return engine
-- parameter information for.
dedcpDBParameterGroupFamily :: Lens' DescribeEngineDefaultClusterParameters Text
dedcpDBParameterGroupFamily = lens _dedcpDBParameterGroupFamily (\ s a -> s{_dedcpDBParameterGroupFamily = a});

instance AWSRequest
         DescribeEngineDefaultClusterParameters where
        type Sv DescribeEngineDefaultClusterParameters = RDS
        type Rs DescribeEngineDefaultClusterParameters =
             DescribeEngineDefaultClusterParametersResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeEngineDefaultClusterParametersResult"
              (\ s h x ->
                 DescribeEngineDefaultClusterParametersResponse' <$>
                   (x .@? "EngineDefaults") <*> (pure (fromEnum s)))

instance ToHeaders
         DescribeEngineDefaultClusterParameters where
        toHeaders = const mempty

instance ToPath
         DescribeEngineDefaultClusterParameters where
        toPath = const "/"

instance ToQuery
         DescribeEngineDefaultClusterParameters where
        toQuery DescribeEngineDefaultClusterParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEngineDefaultClusterParameters" ::
                    ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dedcpFilters),
               "MaxRecords" =: _dedcpMaxRecords,
               "Marker" =: _dedcpMarker,
               "DBParameterGroupFamily" =:
                 _dedcpDBParameterGroupFamily]

-- | /See:/ 'describeEngineDefaultClusterParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedcprsEngineDefaults'
--
-- * 'dedcprsStatus'
data DescribeEngineDefaultClusterParametersResponse = DescribeEngineDefaultClusterParametersResponse'
    { _dedcprsEngineDefaults :: !(Maybe EngineDefaults)
    , _dedcprsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEngineDefaultClusterParametersResponse' smart constructor.
describeEngineDefaultClusterParametersResponse :: Int -> DescribeEngineDefaultClusterParametersResponse
describeEngineDefaultClusterParametersResponse pStatus_ =
    DescribeEngineDefaultClusterParametersResponse'
    { _dedcprsEngineDefaults = Nothing
    , _dedcprsStatus = pStatus_
    }

-- | Undocumented member.
dedcprsEngineDefaults :: Lens' DescribeEngineDefaultClusterParametersResponse (Maybe EngineDefaults)
dedcprsEngineDefaults = lens _dedcprsEngineDefaults (\ s a -> s{_dedcprsEngineDefaults = a});

-- | Undocumented member.
dedcprsStatus :: Lens' DescribeEngineDefaultClusterParametersResponse Int
dedcprsStatus = lens _dedcprsStatus (\ s a -> s{_dedcprsStatus = a});
