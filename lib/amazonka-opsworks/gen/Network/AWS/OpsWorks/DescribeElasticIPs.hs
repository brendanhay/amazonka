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
-- Module      : Network.AWS.OpsWorks.DescribeElasticIPs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP addresses> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeElasticIPs
    (
    -- * Creating a Request
      describeElasticIPs
    , DescribeElasticIPs
    -- * Request Lenses
    , deiInstanceId
    , deiIPs
    , deiStackId

    -- * Destructuring the Response
    , describeElasticIPsResponse
    , DescribeElasticIPsResponse
    -- * Response Lenses
    , deirsElasticIPs
    , deirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeElasticIPs' smart constructor.
data DescribeElasticIPs = DescribeElasticIPs'
  { _deiInstanceId :: !(Maybe Text)
  , _deiIPs        :: !(Maybe [Text])
  , _deiStackId    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticIPs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deiInstanceId' - The instance ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses associated with the specified instance.
--
-- * 'deiIPs' - An array of Elastic IP addresses to be described. If you include this parameter, @DescribeElasticIps@ returns a description of the specified Elastic IP addresses. Otherwise, it returns a description of every Elastic IP address.
--
-- * 'deiStackId' - A stack ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses that are registered with the specified stack.
describeElasticIPs
    :: DescribeElasticIPs
describeElasticIPs =
  DescribeElasticIPs'
    {_deiInstanceId = Nothing, _deiIPs = Nothing, _deiStackId = Nothing}


-- | The instance ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses associated with the specified instance.
deiInstanceId :: Lens' DescribeElasticIPs (Maybe Text)
deiInstanceId = lens _deiInstanceId (\ s a -> s{_deiInstanceId = a})

-- | An array of Elastic IP addresses to be described. If you include this parameter, @DescribeElasticIps@ returns a description of the specified Elastic IP addresses. Otherwise, it returns a description of every Elastic IP address.
deiIPs :: Lens' DescribeElasticIPs [Text]
deiIPs = lens _deiIPs (\ s a -> s{_deiIPs = a}) . _Default . _Coerce

-- | A stack ID. If you include this parameter, @DescribeElasticIps@ returns a description of the Elastic IP addresses that are registered with the specified stack.
deiStackId :: Lens' DescribeElasticIPs (Maybe Text)
deiStackId = lens _deiStackId (\ s a -> s{_deiStackId = a})

instance AWSRequest DescribeElasticIPs where
        type Rs DescribeElasticIPs =
             DescribeElasticIPsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeElasticIPsResponse' <$>
                   (x .?> "ElasticIps" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeElasticIPs where

instance NFData DescribeElasticIPs where

instance ToHeaders DescribeElasticIPs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeElasticIps" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeElasticIPs where
        toJSON DescribeElasticIPs'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _deiInstanceId,
                  ("Ips" .=) <$> _deiIPs,
                  ("StackId" .=) <$> _deiStackId])

instance ToPath DescribeElasticIPs where
        toPath = const "/"

instance ToQuery DescribeElasticIPs where
        toQuery = const mempty

-- | Contains the response to a @DescribeElasticIps@ request.
--
--
--
-- /See:/ 'describeElasticIPsResponse' smart constructor.
data DescribeElasticIPsResponse = DescribeElasticIPsResponse'
  { _deirsElasticIPs     :: !(Maybe [ElasticIP])
  , _deirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticIPsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deirsElasticIPs' - An @ElasticIps@ object that describes the specified Elastic IP addresses.
--
-- * 'deirsResponseStatus' - -- | The response status code.
describeElasticIPsResponse
    :: Int -- ^ 'deirsResponseStatus'
    -> DescribeElasticIPsResponse
describeElasticIPsResponse pResponseStatus_ =
  DescribeElasticIPsResponse'
    {_deirsElasticIPs = Nothing, _deirsResponseStatus = pResponseStatus_}


-- | An @ElasticIps@ object that describes the specified Elastic IP addresses.
deirsElasticIPs :: Lens' DescribeElasticIPsResponse [ElasticIP]
deirsElasticIPs = lens _deirsElasticIPs (\ s a -> s{_deirsElasticIPs = a}) . _Default . _Coerce

-- | -- | The response status code.
deirsResponseStatus :: Lens' DescribeElasticIPsResponse Int
deirsResponseStatus = lens _deirsResponseStatus (\ s a -> s{_deirsResponseStatus = a})

instance NFData DescribeElasticIPsResponse where
