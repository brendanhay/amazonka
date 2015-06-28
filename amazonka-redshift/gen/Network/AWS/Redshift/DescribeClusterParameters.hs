{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.DescribeClusterParameters
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

-- | Returns a detailed list of parameters contained within the specified
-- Amazon Redshift parameter group. For each parameter the response
-- includes information such as parameter name, description, data type,
-- value, whether the parameter value is modifiable, and so on.
--
-- You can specify /source/ filter to retrieve parameters of only specific
-- type. For example, to retrieve parameters that were modified by a user
-- action such as from ModifyClusterParameterGroup, you can specify
-- /source/ equal to /user/.
--
-- For more information about parameters and parameter groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterParameters.html>
module Network.AWS.Redshift.DescribeClusterParameters
    (
    -- * Request
      DescribeClusterParameters
    -- ** Request constructor
    , describeClusterParameters
    -- ** Request lenses
    , dcp1MaxRecords
    , dcp1Marker
    , dcp1Source
    , dcp1ParameterGroupName

    -- * Response
    , DescribeClusterParametersResponse
    -- ** Response constructor
    , describeClusterParametersResponse
    -- ** Response lenses
    , dcprParameters
    , dcprMarker
    , dcprStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeClusterParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcp1MaxRecords'
--
-- * 'dcp1Marker'
--
-- * 'dcp1Source'
--
-- * 'dcp1ParameterGroupName'
data DescribeClusterParameters = DescribeClusterParameters'
    { _dcp1MaxRecords         :: !(Maybe Int)
    , _dcp1Marker             :: !(Maybe Text)
    , _dcp1Source             :: !(Maybe Text)
    , _dcp1ParameterGroupName :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeClusterParameters' smart constructor.
describeClusterParameters :: Text -> DescribeClusterParameters
describeClusterParameters pParameterGroupName =
    DescribeClusterParameters'
    { _dcp1MaxRecords = Nothing
    , _dcp1Marker = Nothing
    , _dcp1Source = Nothing
    , _dcp1ParameterGroupName = pParameterGroupName
    }

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dcp1MaxRecords :: Lens' DescribeClusterParameters (Maybe Int)
dcp1MaxRecords = lens _dcp1MaxRecords (\ s a -> s{_dcp1MaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterParameters
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dcp1Marker :: Lens' DescribeClusterParameters (Maybe Text)
dcp1Marker = lens _dcp1Marker (\ s a -> s{_dcp1Marker = a});

-- | The parameter types to return. Specify @user@ to show parameters that
-- are different form the default. Similarly, specify @engine-default@ to
-- show parameters that are the same as the default parameter group.
--
-- Default: All parameter types returned.
--
-- Valid Values: @user@ | @engine-default@
dcp1Source :: Lens' DescribeClusterParameters (Maybe Text)
dcp1Source = lens _dcp1Source (\ s a -> s{_dcp1Source = a});

-- | The name of a cluster parameter group for which to return details.
dcp1ParameterGroupName :: Lens' DescribeClusterParameters Text
dcp1ParameterGroupName = lens _dcp1ParameterGroupName (\ s a -> s{_dcp1ParameterGroupName = a});

instance AWSPager DescribeClusterParameters where
        page rq rs
          | stop (rs ^. dcprMarker) = Nothing
          | stop (rs ^. dcprParameters) = Nothing
          | otherwise =
            Just $ rq & dcp1Marker .~ rs ^. dcprMarker

instance AWSRequest DescribeClusterParameters where
        type Sv DescribeClusterParameters = Redshift
        type Rs DescribeClusterParameters =
             DescribeClusterParametersResponse
        request = post
        response
          = receiveXMLWrapper "DescribeClusterParametersResult"
              (\ s h x ->
                 DescribeClusterParametersResponse' <$>
                   (x .@? "Parameters" .!@ mempty >>=
                      may (parseXMLList "Parameter"))
                     <*> (x .@? "Marker")
                     <*> (pure s))

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
               "MaxRecords" =: _dcp1MaxRecords,
               "Marker" =: _dcp1Marker, "Source" =: _dcp1Source,
               "ParameterGroupName" =: _dcp1ParameterGroupName]

-- | Contains the output from the DescribeClusterParameters action.
--
-- /See:/ 'describeClusterParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcprParameters'
--
-- * 'dcprMarker'
--
-- * 'dcprStatus'
data DescribeClusterParametersResponse = DescribeClusterParametersResponse'
    { _dcprParameters :: !(Maybe [Parameter])
    , _dcprMarker     :: !(Maybe Text)
    , _dcprStatus     :: !Status
    } deriving (Eq,Show)

-- | 'DescribeClusterParametersResponse' smart constructor.
describeClusterParametersResponse :: Status -> DescribeClusterParametersResponse
describeClusterParametersResponse pStatus =
    DescribeClusterParametersResponse'
    { _dcprParameters = Nothing
    , _dcprMarker = Nothing
    , _dcprStatus = pStatus
    }

-- | A list of Parameter instances. Each instance lists the parameters of one
-- cluster parameter group.
dcprParameters :: Lens' DescribeClusterParametersResponse [Parameter]
dcprParameters = lens _dcprParameters (\ s a -> s{_dcprParameters = a}) . _Default;

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dcprMarker :: Lens' DescribeClusterParametersResponse (Maybe Text)
dcprMarker = lens _dcprMarker (\ s a -> s{_dcprMarker = a});

-- | FIXME: Undocumented member.
dcprStatus :: Lens' DescribeClusterParametersResponse Status
dcprStatus = lens _dcprStatus (\ s a -> s{_dcprStatus = a});
