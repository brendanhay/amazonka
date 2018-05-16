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
-- Module      : Network.AWS.OpsWorks.DescribeElasticLoadBalancers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a stack's Elastic Load Balancing instances.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeElasticLoadBalancers
    (
    -- * Creating a Request
      describeElasticLoadBalancers
    , DescribeElasticLoadBalancers
    -- * Request Lenses
    , delbLayerIds
    , delbStackId

    -- * Destructuring the Response
    , describeElasticLoadBalancersResponse
    , DescribeElasticLoadBalancersResponse
    -- * Response Lenses
    , delbrsElasticLoadBalancers
    , delbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeElasticLoadBalancers' smart constructor.
data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers'
  { _delbLayerIds :: !(Maybe [Text])
  , _delbStackId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticLoadBalancers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delbLayerIds' - A list of layer IDs. The action describes the Elastic Load Balancing instances for the specified layers.
--
-- * 'delbStackId' - A stack ID. The action describes the stack's Elastic Load Balancing instances.
describeElasticLoadBalancers
    :: DescribeElasticLoadBalancers
describeElasticLoadBalancers =
  DescribeElasticLoadBalancers'
    {_delbLayerIds = Nothing, _delbStackId = Nothing}


-- | A list of layer IDs. The action describes the Elastic Load Balancing instances for the specified layers.
delbLayerIds :: Lens' DescribeElasticLoadBalancers [Text]
delbLayerIds = lens _delbLayerIds (\ s a -> s{_delbLayerIds = a}) . _Default . _Coerce

-- | A stack ID. The action describes the stack's Elastic Load Balancing instances.
delbStackId :: Lens' DescribeElasticLoadBalancers (Maybe Text)
delbStackId = lens _delbStackId (\ s a -> s{_delbStackId = a})

instance AWSRequest DescribeElasticLoadBalancers
         where
        type Rs DescribeElasticLoadBalancers =
             DescribeElasticLoadBalancersResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeElasticLoadBalancersResponse' <$>
                   (x .?> "ElasticLoadBalancers" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeElasticLoadBalancers where

instance NFData DescribeElasticLoadBalancers where

instance ToHeaders DescribeElasticLoadBalancers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeElasticLoadBalancers" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeElasticLoadBalancers where
        toJSON DescribeElasticLoadBalancers'{..}
          = object
              (catMaybes
                 [("LayerIds" .=) <$> _delbLayerIds,
                  ("StackId" .=) <$> _delbStackId])

instance ToPath DescribeElasticLoadBalancers where
        toPath = const "/"

instance ToQuery DescribeElasticLoadBalancers where
        toQuery = const mempty

-- | Contains the response to a @DescribeElasticLoadBalancers@ request.
--
--
--
-- /See:/ 'describeElasticLoadBalancersResponse' smart constructor.
data DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse'
  { _delbrsElasticLoadBalancers :: !(Maybe [ElasticLoadBalancer])
  , _delbrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeElasticLoadBalancersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delbrsElasticLoadBalancers' - A list of @ElasticLoadBalancer@ objects that describe the specified Elastic Load Balancing instances.
--
-- * 'delbrsResponseStatus' - -- | The response status code.
describeElasticLoadBalancersResponse
    :: Int -- ^ 'delbrsResponseStatus'
    -> DescribeElasticLoadBalancersResponse
describeElasticLoadBalancersResponse pResponseStatus_ =
  DescribeElasticLoadBalancersResponse'
    { _delbrsElasticLoadBalancers = Nothing
    , _delbrsResponseStatus = pResponseStatus_
    }


-- | A list of @ElasticLoadBalancer@ objects that describe the specified Elastic Load Balancing instances.
delbrsElasticLoadBalancers :: Lens' DescribeElasticLoadBalancersResponse [ElasticLoadBalancer]
delbrsElasticLoadBalancers = lens _delbrsElasticLoadBalancers (\ s a -> s{_delbrsElasticLoadBalancers = a}) . _Default . _Coerce

-- | -- | The response status code.
delbrsResponseStatus :: Lens' DescribeElasticLoadBalancersResponse Int
delbrsResponseStatus = lens _delbrsResponseStatus (\ s a -> s{_delbrsResponseStatus = a})

instance NFData DescribeElasticLoadBalancersResponse
         where
