{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.DescribeDefaultClusterParameters
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

-- | Returns a list of parameter settings for the specified parameter group
-- family.
--
-- For more information about managing parameter groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeDefaultClusterParameters.html>
module Network.AWS.Redshift.DescribeDefaultClusterParameters
    (
    -- * Request
      DescribeDefaultClusterParameters
    -- ** Request constructor
    , describeDefaultClusterParameters
    -- ** Request lenses
    , ddcpMaxRecords
    , ddcpMarker
    , ddcpParameterGroupFamily

    -- * Response
    , DescribeDefaultClusterParametersResponse
    -- ** Response constructor
    , describeDefaultClusterParametersResponse
    -- ** Response lenses
    , ddcprDefaultClusterParameters
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Redshift.Types

-- | /See:/ 'describeDefaultClusterParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcpMaxRecords'
--
-- * 'ddcpMarker'
--
-- * 'ddcpParameterGroupFamily'
data DescribeDefaultClusterParameters = DescribeDefaultClusterParameters'{_ddcpMaxRecords :: Maybe Int, _ddcpMarker :: Maybe Text, _ddcpParameterGroupFamily :: Text} deriving (Eq, Read, Show)

-- | 'DescribeDefaultClusterParameters' smart constructor.
describeDefaultClusterParameters :: Text -> DescribeDefaultClusterParameters
describeDefaultClusterParameters pParameterGroupFamily = DescribeDefaultClusterParameters'{_ddcpMaxRecords = Nothing, _ddcpMarker = Nothing, _ddcpParameterGroupFamily = pParameterGroupFamily};

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
ddcpMaxRecords :: Lens' DescribeDefaultClusterParameters (Maybe Int)
ddcpMaxRecords = lens _ddcpMaxRecords (\ s a -> s{_ddcpMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a
-- DescribeDefaultClusterParameters request exceed the value specified in
-- @MaxRecords@, AWS returns a value in the @Marker@ field of the response.
-- You can retrieve the next set of response records by providing the
-- returned marker value in the @Marker@ parameter and retrying the
-- request.
ddcpMarker :: Lens' DescribeDefaultClusterParameters (Maybe Text)
ddcpMarker = lens _ddcpMarker (\ s a -> s{_ddcpMarker = a});

-- | The name of the cluster parameter group family.
ddcpParameterGroupFamily :: Lens' DescribeDefaultClusterParameters Text
ddcpParameterGroupFamily = lens _ddcpParameterGroupFamily (\ s a -> s{_ddcpParameterGroupFamily = a});

instance AWSRequest DescribeDefaultClusterParameters
         where
        type Sv DescribeDefaultClusterParameters = Redshift
        type Rs DescribeDefaultClusterParameters =
             DescribeDefaultClusterParametersResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeDefaultClusterParametersResult"
              (\ s h x ->
                 DescribeDefaultClusterParametersResponse' <$>
                   x .@ "DefaultClusterParameters")

instance ToHeaders DescribeDefaultClusterParameters
         where
        toHeaders = const mempty

instance ToPath DescribeDefaultClusterParameters
         where
        toPath = const "/"

instance ToQuery DescribeDefaultClusterParameters
         where
        toQuery DescribeDefaultClusterParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDefaultClusterParameters" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "MaxRecords" =: _ddcpMaxRecords,
               "Marker" =: _ddcpMarker,
               "ParameterGroupFamily" =: _ddcpParameterGroupFamily]

-- | /See:/ 'describeDefaultClusterParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcprDefaultClusterParameters'
newtype DescribeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResponse'{_ddcprDefaultClusterParameters :: DefaultClusterParameters} deriving (Eq, Read, Show)

-- | 'DescribeDefaultClusterParametersResponse' smart constructor.
describeDefaultClusterParametersResponse :: DefaultClusterParameters -> DescribeDefaultClusterParametersResponse
describeDefaultClusterParametersResponse pDefaultClusterParameters = DescribeDefaultClusterParametersResponse'{_ddcprDefaultClusterParameters = pDefaultClusterParameters};

-- | FIXME: Undocumented member.
ddcprDefaultClusterParameters :: Lens' DescribeDefaultClusterParametersResponse DefaultClusterParameters
ddcprDefaultClusterParameters = lens _ddcprDefaultClusterParameters (\ s a -> s{_ddcprDefaultClusterParameters = a});
