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
-- Module      : Network.AWS.RDS.DescribeDBClusterParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB cluster parameter group.
--
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
module Network.AWS.RDS.DescribeDBClusterParameters
    (
    -- * Creating a Request
      describeDBClusterParameters
    , DescribeDBClusterParameters
    -- * Request Lenses
    , ddcpFilters
    , ddcpMarker
    , ddcpMaxRecords
    , ddcpSource
    , ddcpDBClusterParameterGroupName

    -- * Destructuring the Response
    , describeDBClusterParametersResponse
    , DescribeDBClusterParametersResponse
    -- * Response Lenses
    , ddcprsMarker
    , ddcprsParameters
    , ddcprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeDBClusterParameters' smart constructor.
data DescribeDBClusterParameters = DescribeDBClusterParameters'
  { _ddcpFilters                     :: !(Maybe [Filter])
  , _ddcpMarker                      :: !(Maybe Text)
  , _ddcpMaxRecords                  :: !(Maybe Int)
  , _ddcpSource                      :: !(Maybe Text)
  , _ddcpDBClusterParameterGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClusterParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcpFilters' - This parameter is not currently supported.
--
-- * 'ddcpMarker' - An optional pagination token provided by a previous @DescribeDBClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddcpMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddcpSource' - A value that indicates to return only parameters for a specific source. Parameter sources can be @engine@ , @service@ , or @customer@ .
--
-- * 'ddcpDBClusterParameterGroupName' - The name of a specific DB cluster parameter group to return parameter details for. Constraints:     * If supplied, must match the name of an existing DBClusterParameterGroup.
describeDBClusterParameters
    :: Text -- ^ 'ddcpDBClusterParameterGroupName'
    -> DescribeDBClusterParameters
describeDBClusterParameters pDBClusterParameterGroupName_ =
  DescribeDBClusterParameters'
    { _ddcpFilters = Nothing
    , _ddcpMarker = Nothing
    , _ddcpMaxRecords = Nothing
    , _ddcpSource = Nothing
    , _ddcpDBClusterParameterGroupName = pDBClusterParameterGroupName_
    }


-- | This parameter is not currently supported.
ddcpFilters :: Lens' DescribeDBClusterParameters [Filter]
ddcpFilters = lens _ddcpFilters (\ s a -> s{_ddcpFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeDBClusterParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddcpMarker :: Lens' DescribeDBClusterParameters (Maybe Text)
ddcpMarker = lens _ddcpMarker (\ s a -> s{_ddcpMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddcpMaxRecords :: Lens' DescribeDBClusterParameters (Maybe Int)
ddcpMaxRecords = lens _ddcpMaxRecords (\ s a -> s{_ddcpMaxRecords = a})

-- | A value that indicates to return only parameters for a specific source. Parameter sources can be @engine@ , @service@ , or @customer@ .
ddcpSource :: Lens' DescribeDBClusterParameters (Maybe Text)
ddcpSource = lens _ddcpSource (\ s a -> s{_ddcpSource = a})

-- | The name of a specific DB cluster parameter group to return parameter details for. Constraints:     * If supplied, must match the name of an existing DBClusterParameterGroup.
ddcpDBClusterParameterGroupName :: Lens' DescribeDBClusterParameters Text
ddcpDBClusterParameterGroupName = lens _ddcpDBClusterParameterGroupName (\ s a -> s{_ddcpDBClusterParameterGroupName = a})

instance AWSRequest DescribeDBClusterParameters where
        type Rs DescribeDBClusterParameters =
             DescribeDBClusterParametersResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DescribeDBClusterParametersResult"
              (\ s h x ->
                 DescribeDBClusterParametersResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "Parameters" .!@ mempty >>=
                        may (parseXMLList "Parameter"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBClusterParameters where

instance NFData DescribeDBClusterParameters where

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
               "Marker" =: _ddcpMarker,
               "MaxRecords" =: _ddcpMaxRecords,
               "Source" =: _ddcpSource,
               "DBClusterParameterGroupName" =:
                 _ddcpDBClusterParameterGroupName]

-- | Provides details about a DB cluster parameter group including the parameters in the DB cluster parameter group.
--
--
--
-- /See:/ 'describeDBClusterParametersResponse' smart constructor.
data DescribeDBClusterParametersResponse = DescribeDBClusterParametersResponse'
  { _ddcprsMarker         :: !(Maybe Text)
  , _ddcprsParameters     :: !(Maybe [Parameter])
  , _ddcprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBClusterParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcprsMarker' - An optional pagination token provided by a previous DescribeDBClusterParameters request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddcprsParameters' - Provides a list of parameters for the DB cluster parameter group.
--
-- * 'ddcprsResponseStatus' - -- | The response status code.
describeDBClusterParametersResponse
    :: Int -- ^ 'ddcprsResponseStatus'
    -> DescribeDBClusterParametersResponse
describeDBClusterParametersResponse pResponseStatus_ =
  DescribeDBClusterParametersResponse'
    { _ddcprsMarker = Nothing
    , _ddcprsParameters = Nothing
    , _ddcprsResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous DescribeDBClusterParameters request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddcprsMarker :: Lens' DescribeDBClusterParametersResponse (Maybe Text)
ddcprsMarker = lens _ddcprsMarker (\ s a -> s{_ddcprsMarker = a})

-- | Provides a list of parameters for the DB cluster parameter group.
ddcprsParameters :: Lens' DescribeDBClusterParametersResponse [Parameter]
ddcprsParameters = lens _ddcprsParameters (\ s a -> s{_ddcprsParameters = a}) . _Default . _Coerce

-- | -- | The response status code.
ddcprsResponseStatus :: Lens' DescribeDBClusterParametersResponse Int
ddcprsResponseStatus = lens _ddcprsResponseStatus (\ s a -> s{_ddcprsResponseStatus = a})

instance NFData DescribeDBClusterParametersResponse
         where
