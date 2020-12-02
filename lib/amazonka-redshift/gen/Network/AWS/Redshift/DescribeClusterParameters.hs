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
-- Module      : Network.AWS.Redshift.DescribeClusterParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a detailed list of parameters contained within the specified Amazon Redshift parameter group. For each parameter the response includes information such as parameter name, description, data type, value, whether the parameter value is modifiable, and so on.
--
--
-- You can specify /source/ filter to retrieve parameters of only specific type. For example, to retrieve parameters that were modified by a user action such as from 'ModifyClusterParameterGroup' , you can specify /source/ equal to /user/ .
--
-- For more information about parameters and parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterParameters
    (
    -- * Creating a Request
      describeClusterParameters
    , DescribeClusterParameters
    -- * Request Lenses
    , dcpsMarker
    , dcpsMaxRecords
    , dcpsSource
    , dcpsParameterGroupName

    -- * Destructuring the Response
    , describeClusterParametersResponse
    , DescribeClusterParametersResponse
    -- * Response Lenses
    , dcprsMarker
    , dcprsParameters
    , dcprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeClusterParameters' smart constructor.
data DescribeClusterParameters = DescribeClusterParameters'
  { _dcpsMarker             :: !(Maybe Text)
  , _dcpsMaxRecords         :: !(Maybe Int)
  , _dcpsSource             :: !(Maybe Text)
  , _dcpsParameterGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpsMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'dcpsMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
--
-- * 'dcpsSource' - The parameter types to return. Specify @user@ to show parameters that are different form the default. Similarly, specify @engine-default@ to show parameters that are the same as the default parameter group.  Default: All parameter types returned. Valid Values: @user@ | @engine-default@
--
-- * 'dcpsParameterGroupName' - The name of a cluster parameter group for which to return details.
describeClusterParameters
    :: Text -- ^ 'dcpsParameterGroupName'
    -> DescribeClusterParameters
describeClusterParameters pParameterGroupName_ =
  DescribeClusterParameters'
    { _dcpsMarker = Nothing
    , _dcpsMaxRecords = Nothing
    , _dcpsSource = Nothing
    , _dcpsParameterGroupName = pParameterGroupName_
    }


-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
dcpsMarker :: Lens' DescribeClusterParameters (Maybe Text)
dcpsMarker = lens _dcpsMarker (\ s a -> s{_dcpsMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
dcpsMaxRecords :: Lens' DescribeClusterParameters (Maybe Int)
dcpsMaxRecords = lens _dcpsMaxRecords (\ s a -> s{_dcpsMaxRecords = a})

-- | The parameter types to return. Specify @user@ to show parameters that are different form the default. Similarly, specify @engine-default@ to show parameters that are the same as the default parameter group.  Default: All parameter types returned. Valid Values: @user@ | @engine-default@
dcpsSource :: Lens' DescribeClusterParameters (Maybe Text)
dcpsSource = lens _dcpsSource (\ s a -> s{_dcpsSource = a})

-- | The name of a cluster parameter group for which to return details.
dcpsParameterGroupName :: Lens' DescribeClusterParameters Text
dcpsParameterGroupName = lens _dcpsParameterGroupName (\ s a -> s{_dcpsParameterGroupName = a})

instance AWSPager DescribeClusterParameters where
        page rq rs
          | stop (rs ^. dcprsMarker) = Nothing
          | stop (rs ^. dcprsParameters) = Nothing
          | otherwise =
            Just $ rq & dcpsMarker .~ rs ^. dcprsMarker

instance AWSRequest DescribeClusterParameters where
        type Rs DescribeClusterParameters =
             DescribeClusterParametersResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "DescribeClusterParametersResult"
              (\ s h x ->
                 DescribeClusterParametersResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "Parameters" .!@ mempty >>=
                        may (parseXMLList "Parameter"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClusterParameters where

instance NFData DescribeClusterParameters where

instance ToHeaders DescribeClusterParameters where
        toHeaders = const mempty

instance ToPath DescribeClusterParameters where
        toPath = const "/"

instance ToQuery DescribeClusterParameters where
        toQuery DescribeClusterParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClusterParameters" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Marker" =: _dcpsMarker,
               "MaxRecords" =: _dcpsMaxRecords,
               "Source" =: _dcpsSource,
               "ParameterGroupName" =: _dcpsParameterGroupName]

-- | Contains the output from the 'DescribeClusterParameters' action.
--
--
--
-- /See:/ 'describeClusterParametersResponse' smart constructor.
data DescribeClusterParametersResponse = DescribeClusterParametersResponse'
  { _dcprsMarker         :: !(Maybe Text)
  , _dcprsParameters     :: !(Maybe [Parameter])
  , _dcprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcprsMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dcprsParameters' - A list of 'Parameter' instances. Each instance lists the parameters of one cluster parameter group.
--
-- * 'dcprsResponseStatus' - -- | The response status code.
describeClusterParametersResponse
    :: Int -- ^ 'dcprsResponseStatus'
    -> DescribeClusterParametersResponse
describeClusterParametersResponse pResponseStatus_ =
  DescribeClusterParametersResponse'
    { _dcprsMarker = Nothing
    , _dcprsParameters = Nothing
    , _dcprsResponseStatus = pResponseStatus_
    }


-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
dcprsMarker :: Lens' DescribeClusterParametersResponse (Maybe Text)
dcprsMarker = lens _dcprsMarker (\ s a -> s{_dcprsMarker = a})

-- | A list of 'Parameter' instances. Each instance lists the parameters of one cluster parameter group.
dcprsParameters :: Lens' DescribeClusterParametersResponse [Parameter]
dcprsParameters = lens _dcprsParameters (\ s a -> s{_dcprsParameters = a}) . _Default . _Coerce

-- | -- | The response status code.
dcprsResponseStatus :: Lens' DescribeClusterParametersResponse Int
dcprsResponseStatus = lens _dcprsResponseStatus (\ s a -> s{_dcprsResponseStatus = a})

instance NFData DescribeClusterParametersResponse
         where
