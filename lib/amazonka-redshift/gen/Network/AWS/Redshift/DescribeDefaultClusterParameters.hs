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
-- Module      : Network.AWS.Redshift.DescribeDefaultClusterParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of parameter settings for the specified parameter group family.
--
--
-- For more information about parameters and parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeDefaultClusterParameters
    (
    -- * Creating a Request
      describeDefaultClusterParameters
    , DescribeDefaultClusterParameters
    -- * Request Lenses
    , ddcpMarker
    , ddcpMaxRecords
    , ddcpParameterGroupFamily

    -- * Destructuring the Response
    , describeDefaultClusterParametersResponse
    , DescribeDefaultClusterParametersResponse
    -- * Response Lenses
    , ddcprsResponseStatus
    , ddcprsDefaultClusterParameters
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
-- /See:/ 'describeDefaultClusterParameters' smart constructor.
data DescribeDefaultClusterParameters = DescribeDefaultClusterParameters'
  { _ddcpMarker               :: !(Maybe Text)
  , _ddcpMaxRecords           :: !(Maybe Int)
  , _ddcpParameterGroupFamily :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDefaultClusterParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcpMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeDefaultClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'ddcpMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
--
-- * 'ddcpParameterGroupFamily' - The name of the cluster parameter group family.
describeDefaultClusterParameters
    :: Text -- ^ 'ddcpParameterGroupFamily'
    -> DescribeDefaultClusterParameters
describeDefaultClusterParameters pParameterGroupFamily_ =
  DescribeDefaultClusterParameters'
    { _ddcpMarker = Nothing
    , _ddcpMaxRecords = Nothing
    , _ddcpParameterGroupFamily = pParameterGroupFamily_
    }


-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeDefaultClusterParameters' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
ddcpMarker :: Lens' DescribeDefaultClusterParameters (Maybe Text)
ddcpMarker = lens _ddcpMarker (\ s a -> s{_ddcpMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
ddcpMaxRecords :: Lens' DescribeDefaultClusterParameters (Maybe Int)
ddcpMaxRecords = lens _ddcpMaxRecords (\ s a -> s{_ddcpMaxRecords = a})

-- | The name of the cluster parameter group family.
ddcpParameterGroupFamily :: Lens' DescribeDefaultClusterParameters Text
ddcpParameterGroupFamily = lens _ddcpParameterGroupFamily (\ s a -> s{_ddcpParameterGroupFamily = a})

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
        type Rs DescribeDefaultClusterParameters =
             DescribeDefaultClusterParametersResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "DescribeDefaultClusterParametersResult"
              (\ s h x ->
                 DescribeDefaultClusterParametersResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@ "DefaultClusterParameters"))

instance Hashable DescribeDefaultClusterParameters
         where

instance NFData DescribeDefaultClusterParameters
         where

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
               "Marker" =: _ddcpMarker,
               "MaxRecords" =: _ddcpMaxRecords,
               "ParameterGroupFamily" =: _ddcpParameterGroupFamily]

-- | /See:/ 'describeDefaultClusterParametersResponse' smart constructor.
data DescribeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResponse'
  { _ddcprsResponseStatus           :: !Int
  , _ddcprsDefaultClusterParameters :: !DefaultClusterParameters
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDefaultClusterParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcprsResponseStatus' - -- | The response status code.
--
-- * 'ddcprsDefaultClusterParameters' - Undocumented member.
describeDefaultClusterParametersResponse
    :: Int -- ^ 'ddcprsResponseStatus'
    -> DefaultClusterParameters -- ^ 'ddcprsDefaultClusterParameters'
    -> DescribeDefaultClusterParametersResponse
describeDefaultClusterParametersResponse pResponseStatus_ pDefaultClusterParameters_ =
  DescribeDefaultClusterParametersResponse'
    { _ddcprsResponseStatus = pResponseStatus_
    , _ddcprsDefaultClusterParameters = pDefaultClusterParameters_
    }


-- | -- | The response status code.
ddcprsResponseStatus :: Lens' DescribeDefaultClusterParametersResponse Int
ddcprsResponseStatus = lens _ddcprsResponseStatus (\ s a -> s{_ddcprsResponseStatus = a})

-- | Undocumented member.
ddcprsDefaultClusterParameters :: Lens' DescribeDefaultClusterParametersResponse DefaultClusterParameters
ddcprsDefaultClusterParameters = lens _ddcprsDefaultClusterParameters (\ s a -> s{_ddcprsDefaultClusterParameters = a})

instance NFData
           DescribeDefaultClusterParametersResponse
         where
