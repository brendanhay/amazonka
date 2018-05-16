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
-- Module      : Network.AWS.RDS.DescribeDBParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB parameter group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBParameters
    (
    -- * Creating a Request
      describeDBParameters
    , DescribeDBParameters
    -- * Request Lenses
    , ddpFilters
    , ddpMarker
    , ddpMaxRecords
    , ddpSource
    , ddpDBParameterGroupName

    -- * Destructuring the Response
    , describeDBParametersResponse
    , DescribeDBParametersResponse
    -- * Response Lenses
    , ddprsMarker
    , ddprsParameters
    , ddprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDBParameters' smart constructor.
data DescribeDBParameters = DescribeDBParameters'
  { _ddpFilters              :: !(Maybe [Filter])
  , _ddpMarker               :: !(Maybe Text)
  , _ddpMaxRecords           :: !(Maybe Int)
  , _ddpSource               :: !(Maybe Text)
  , _ddpDBParameterGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpFilters' - This parameter is not currently supported.
--
-- * 'ddpMarker' - An optional pagination token provided by a previous @DescribeDBParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddpMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddpSource' - The parameter types to return. Default: All parameter types returned Valid Values: @user | system | engine-default@
--
-- * 'ddpDBParameterGroupName' - The name of a specific DB parameter group to return details for. Constraints:     * If supplied, must match the name of an existing DBParameterGroup.
describeDBParameters
    :: Text -- ^ 'ddpDBParameterGroupName'
    -> DescribeDBParameters
describeDBParameters pDBParameterGroupName_ =
  DescribeDBParameters'
    { _ddpFilters = Nothing
    , _ddpMarker = Nothing
    , _ddpMaxRecords = Nothing
    , _ddpSource = Nothing
    , _ddpDBParameterGroupName = pDBParameterGroupName_
    }


-- | This parameter is not currently supported.
ddpFilters :: Lens' DescribeDBParameters [Filter]
ddpFilters = lens _ddpFilters (\ s a -> s{_ddpFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeDBParameters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddpMarker :: Lens' DescribeDBParameters (Maybe Text)
ddpMarker = lens _ddpMarker (\ s a -> s{_ddpMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddpMaxRecords :: Lens' DescribeDBParameters (Maybe Int)
ddpMaxRecords = lens _ddpMaxRecords (\ s a -> s{_ddpMaxRecords = a})

-- | The parameter types to return. Default: All parameter types returned Valid Values: @user | system | engine-default@
ddpSource :: Lens' DescribeDBParameters (Maybe Text)
ddpSource = lens _ddpSource (\ s a -> s{_ddpSource = a})

-- | The name of a specific DB parameter group to return details for. Constraints:     * If supplied, must match the name of an existing DBParameterGroup.
ddpDBParameterGroupName :: Lens' DescribeDBParameters Text
ddpDBParameterGroupName = lens _ddpDBParameterGroupName (\ s a -> s{_ddpDBParameterGroupName = a})

instance AWSPager DescribeDBParameters where
        page rq rs
          | stop (rs ^. ddprsMarker) = Nothing
          | stop (rs ^. ddprsParameters) = Nothing
          | otherwise =
            Just $ rq & ddpMarker .~ rs ^. ddprsMarker

instance AWSRequest DescribeDBParameters where
        type Rs DescribeDBParameters =
             DescribeDBParametersResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeDBParametersResult"
              (\ s h x ->
                 DescribeDBParametersResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "Parameters" .!@ mempty >>=
                        may (parseXMLList "Parameter"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBParameters where

instance NFData DescribeDBParameters where

instance ToHeaders DescribeDBParameters where
        toHeaders = const mempty

instance ToPath DescribeDBParameters where
        toPath = const "/"

instance ToQuery DescribeDBParameters where
        toQuery DescribeDBParameters'{..}
          = mconcat
              ["Action" =: ("DescribeDBParameters" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddpFilters),
               "Marker" =: _ddpMarker,
               "MaxRecords" =: _ddpMaxRecords,
               "Source" =: _ddpSource,
               "DBParameterGroupName" =: _ddpDBParameterGroupName]

-- | Contains the result of a successful invocation of the 'DescribeDBParameters' action.
--
--
--
-- /See:/ 'describeDBParametersResponse' smart constructor.
data DescribeDBParametersResponse = DescribeDBParametersResponse'
  { _ddprsMarker         :: !(Maybe Text)
  , _ddprsParameters     :: !(Maybe [Parameter])
  , _ddprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddprsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddprsParameters' - A list of 'Parameter' values.
--
-- * 'ddprsResponseStatus' - -- | The response status code.
describeDBParametersResponse
    :: Int -- ^ 'ddprsResponseStatus'
    -> DescribeDBParametersResponse
describeDBParametersResponse pResponseStatus_ =
  DescribeDBParametersResponse'
    { _ddprsMarker = Nothing
    , _ddprsParameters = Nothing
    , _ddprsResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddprsMarker :: Lens' DescribeDBParametersResponse (Maybe Text)
ddprsMarker = lens _ddprsMarker (\ s a -> s{_ddprsMarker = a})

-- | A list of 'Parameter' values.
ddprsParameters :: Lens' DescribeDBParametersResponse [Parameter]
ddprsParameters = lens _ddprsParameters (\ s a -> s{_ddprsParameters = a}) . _Default . _Coerce

-- | -- | The response status code.
ddprsResponseStatus :: Lens' DescribeDBParametersResponse Int
ddprsResponseStatus = lens _ddprsResponseStatus (\ s a -> s{_ddprsResponseStatus = a})

instance NFData DescribeDBParametersResponse where
