{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB cluster
-- parameter group.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBClusterParameters.html AWS API Reference> for DescribeDBClusterParameters.
module Network.AWS.RDS.DescribeDBClusterParameters
    (
    -- * Creating a Request
      DescribeDBClusterParameters
    , describeDBClusterParameters
    -- * Request Lenses
    , ddcpFilters
    , ddcpMaxRecords
    , ddcpMarker
    , ddcpSource
    , ddcpDBClusterParameterGroupName

    -- * Destructuring the Response
    , DescribeDBClusterParametersResponse
    , describeDBClusterParametersResponse
    -- * Response Lenses
    , ddcprsParameters
    , ddcprsMarker
    , ddcprsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBClusterParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcpFilters'
--
-- * 'ddcpMaxRecords'
--
-- * 'ddcpMarker'
--
-- * 'ddcpSource'
--
-- * 'ddcpDBClusterParameterGroupName'
data DescribeDBClusterParameters = DescribeDBClusterParameters'
    { _ddcpFilters                     :: !(Maybe [Filter])
    , _ddcpMaxRecords                  :: !(Maybe Int)
    , _ddcpMarker                      :: !(Maybe Text)
    , _ddcpSource                      :: !(Maybe Text)
    , _ddcpDBClusterParameterGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBClusterParameters' smart constructor.
describeDBClusterParameters :: Text -> DescribeDBClusterParameters
describeDBClusterParameters pDBClusterParameterGroupName_ =
    DescribeDBClusterParameters'
    { _ddcpFilters = Nothing
    , _ddcpMaxRecords = Nothing
    , _ddcpMarker = Nothing
    , _ddcpSource = Nothing
    , _ddcpDBClusterParameterGroupName = pDBClusterParameterGroupName_
    }

-- | This parameter is not currently supported.
ddcpFilters :: Lens' DescribeDBClusterParameters [Filter]
ddcpFilters = lens _ddcpFilters (\ s a -> s{_ddcpFilters = a}) . _Default . _Coerce;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
ddcpMaxRecords :: Lens' DescribeDBClusterParameters (Maybe Int)
ddcpMaxRecords = lens _ddcpMaxRecords (\ s a -> s{_ddcpMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterParameters@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddcpMarker :: Lens' DescribeDBClusterParameters (Maybe Text)
ddcpMarker = lens _ddcpMarker (\ s a -> s{_ddcpMarker = a});

-- | A value that indicates to return only parameters for a specific source.
-- Parameter sources can be @engine@, @service@, or @customer@.
ddcpSource :: Lens' DescribeDBClusterParameters (Maybe Text)
ddcpSource = lens _ddcpSource (\ s a -> s{_ddcpSource = a});

-- | The name of a specific DB cluster parameter group to return parameter
-- details for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddcpDBClusterParameterGroupName :: Lens' DescribeDBClusterParameters Text
ddcpDBClusterParameterGroupName = lens _ddcpDBClusterParameterGroupName (\ s a -> s{_ddcpDBClusterParameterGroupName = a});

instance AWSRequest DescribeDBClusterParameters where
        type Sv DescribeDBClusterParameters = RDS
        type Rs DescribeDBClusterParameters =
             DescribeDBClusterParametersResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeDBClusterParametersResult"
              (\ s h x ->
                 DescribeDBClusterParametersResponse' <$>
                   (x .@? "Parameters" .!@ mempty >>=
                      may (parseXMLList "Parameter"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDBClusterParameters where
        toHeaders = const mempty

instance ToPath DescribeDBClusterParameters where
        toPath = const "/"

instance ToQuery DescribeDBClusterParameters where
        toQuery DescribeDBClusterParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDBClusterParameters" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddcpFilters),
               "MaxRecords" =: _ddcpMaxRecords,
               "Marker" =: _ddcpMarker, "Source" =: _ddcpSource,
               "DBClusterParameterGroupName" =:
                 _ddcpDBClusterParameterGroupName]

-- | Provides details about a DB cluster parameter group including the
-- parameters in the DB cluster parameter group.
--
-- /See:/ 'describeDBClusterParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcprsParameters'
--
-- * 'ddcprsMarker'
--
-- * 'ddcprsStatus'
data DescribeDBClusterParametersResponse = DescribeDBClusterParametersResponse'
    { _ddcprsParameters :: !(Maybe [Parameter])
    , _ddcprsMarker     :: !(Maybe Text)
    , _ddcprsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBClusterParametersResponse' smart constructor.
describeDBClusterParametersResponse :: Int -> DescribeDBClusterParametersResponse
describeDBClusterParametersResponse pStatus_ =
    DescribeDBClusterParametersResponse'
    { _ddcprsParameters = Nothing
    , _ddcprsMarker = Nothing
    , _ddcprsStatus = pStatus_
    }

-- | Provides a list of parameters for the DB cluster parameter group.
ddcprsParameters :: Lens' DescribeDBClusterParametersResponse [Parameter]
ddcprsParameters = lens _ddcprsParameters (\ s a -> s{_ddcprsParameters = a}) . _Default . _Coerce;

-- | An optional pagination token provided by a previous
-- DescribeDBClusterParameters request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
ddcprsMarker :: Lens' DescribeDBClusterParametersResponse (Maybe Text)
ddcprsMarker = lens _ddcprsMarker (\ s a -> s{_ddcprsMarker = a});

-- | Undocumented member.
ddcprsStatus :: Lens' DescribeDBClusterParametersResponse Int
ddcprsStatus = lens _ddcprsStatus (\ s a -> s{_ddcprsStatus = a});
