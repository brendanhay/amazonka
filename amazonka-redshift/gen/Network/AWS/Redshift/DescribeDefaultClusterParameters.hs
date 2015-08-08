{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeDefaultClusterParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of parameter settings for the specified parameter group
-- family.
--
-- For more information about parameters and parameter groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeDefaultClusterParameters.html AWS API Reference> for DescribeDefaultClusterParameters.
module Network.AWS.Redshift.DescribeDefaultClusterParameters
    (
    -- * Creating a Request
      DescribeDefaultClusterParameters
    , describeDefaultClusterParameters
    -- * Request Lenses
    , ddcpMaxRecords
    , ddcpMarker
    , ddcpParameterGroupFamily

    -- * Destructuring the Response
    , DescribeDefaultClusterParametersResponse
    , describeDefaultClusterParametersResponse
    -- * Response Lenses
    , ddcprsStatus
    , ddcprsDefaultClusterParameters
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDefaultClusterParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcpMaxRecords'
--
-- * 'ddcpMarker'
--
-- * 'ddcpParameterGroupFamily'
data DescribeDefaultClusterParameters = DescribeDefaultClusterParameters'
    { _ddcpMaxRecords           :: !(Maybe Int)
    , _ddcpMarker               :: !(Maybe Text)
    , _ddcpParameterGroupFamily :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDefaultClusterParameters' smart constructor.
describeDefaultClusterParameters :: Text -> DescribeDefaultClusterParameters
describeDefaultClusterParameters pParameterGroupFamily_ =
    DescribeDefaultClusterParameters'
    { _ddcpMaxRecords = Nothing
    , _ddcpMarker = Nothing
    , _ddcpParameterGroupFamily = pParameterGroupFamily_
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

instance AWSPager DescribeDefaultClusterParameters
         where
        page rq rs
          | stop
              (rs ^?
                 ddcprsDefaultClusterParameters . dcpMarker . _Just)
            = Nothing
          | stop
              (rs ^.
                 ddcprsDefaultClusterParameters . dcpParameters)
            = Nothing
          | otherwise =
            Just $ rq &
              ddcpMarker .~
                rs ^?
                  ddcprsDefaultClusterParameters . dcpMarker . _Just

instance AWSRequest DescribeDefaultClusterParameters
         where
        type Sv DescribeDefaultClusterParameters = Redshift
        type Rs DescribeDefaultClusterParameters =
             DescribeDefaultClusterParametersResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeDefaultClusterParametersResult"
              (\ s h x ->
                 DescribeDefaultClusterParametersResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@ "DefaultClusterParameters"))

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
-- * 'ddcprsStatus'
--
-- * 'ddcprsDefaultClusterParameters'
data DescribeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResponse'
    { _ddcprsStatus                   :: !Int
    , _ddcprsDefaultClusterParameters :: !DefaultClusterParameters
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDefaultClusterParametersResponse' smart constructor.
describeDefaultClusterParametersResponse :: Int -> DefaultClusterParameters -> DescribeDefaultClusterParametersResponse
describeDefaultClusterParametersResponse pStatus_ pDefaultClusterParameters_ =
    DescribeDefaultClusterParametersResponse'
    { _ddcprsStatus = pStatus_
    , _ddcprsDefaultClusterParameters = pDefaultClusterParameters_
    }

-- | Undocumented member.
ddcprsStatus :: Lens' DescribeDefaultClusterParametersResponse Int
ddcprsStatus = lens _ddcprsStatus (\ s a -> s{_ddcprsStatus = a});

-- | Undocumented member.
ddcprsDefaultClusterParameters :: Lens' DescribeDefaultClusterParametersResponse DefaultClusterParameters
ddcprsDefaultClusterParameters = lens _ddcprsDefaultClusterParameters (\ s a -> s{_ddcprsDefaultClusterParameters = a});
