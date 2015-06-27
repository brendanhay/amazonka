{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.DescribeElasticLoadBalancers
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

-- | Describes a stack\'s Elastic Load Balancing instances.
--
-- You must specify at least one of the parameters.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeElasticLoadBalancers.html>
module Network.AWS.OpsWorks.DescribeElasticLoadBalancers
    (
    -- * Request
      DescribeElasticLoadBalancers
    -- ** Request constructor
    , describeElasticLoadBalancers
    -- ** Request lenses
    , delbLayerIds
    , delbStackId

    -- * Response
    , DescribeElasticLoadBalancersResponse
    -- ** Response constructor
    , describeElasticLoadBalancersResponse
    -- ** Response lenses
    , delbrElasticLoadBalancers
    , delbrStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeElasticLoadBalancers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delbLayerIds'
--
-- * 'delbStackId'
data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers'
    { _delbLayerIds :: Maybe [Text]
    , _delbStackId  :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'DescribeElasticLoadBalancers' smart constructor.
describeElasticLoadBalancers :: DescribeElasticLoadBalancers
describeElasticLoadBalancers =
    DescribeElasticLoadBalancers'
    { _delbLayerIds = Nothing
    , _delbStackId = Nothing
    }

-- | A list of layer IDs. The action describes the Elastic Load Balancing
-- instances for the specified layers.
delbLayerIds :: Lens' DescribeElasticLoadBalancers [Text]
delbLayerIds = lens _delbLayerIds (\ s a -> s{_delbLayerIds = a}) . _Default;

-- | A stack ID. The action describes the stack\'s Elastic Load Balancing
-- instances.
delbStackId :: Lens' DescribeElasticLoadBalancers (Maybe Text)
delbStackId = lens _delbStackId (\ s a -> s{_delbStackId = a});

instance AWSRequest DescribeElasticLoadBalancers
         where
        type Sv DescribeElasticLoadBalancers = OpsWorks
        type Rs DescribeElasticLoadBalancers =
             DescribeElasticLoadBalancersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeElasticLoadBalancersResponse' <$>
                   (x .?> "ElasticLoadBalancers" .!@ mempty) <*>
                     (pure (fromEnum s)))

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
              ["LayerIds" .= _delbLayerIds,
               "StackId" .= _delbStackId]

instance ToPath DescribeElasticLoadBalancers where
        toPath = const "/"

instance ToQuery DescribeElasticLoadBalancers where
        toQuery = const mempty

-- | Contains the response to a @DescribeElasticLoadBalancers@ request.
--
-- /See:/ 'describeElasticLoadBalancersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delbrElasticLoadBalancers'
--
-- * 'delbrStatus'
data DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse'
    { _delbrElasticLoadBalancers :: Maybe [ElasticLoadBalancer]
    , _delbrStatus               :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeElasticLoadBalancersResponse' smart constructor.
describeElasticLoadBalancersResponse :: Int -> DescribeElasticLoadBalancersResponse
describeElasticLoadBalancersResponse pStatus =
    DescribeElasticLoadBalancersResponse'
    { _delbrElasticLoadBalancers = Nothing
    , _delbrStatus = pStatus
    }

-- | A list of @ElasticLoadBalancer@ objects that describe the specified
-- Elastic Load Balancing instances.
delbrElasticLoadBalancers :: Lens' DescribeElasticLoadBalancersResponse [ElasticLoadBalancer]
delbrElasticLoadBalancers = lens _delbrElasticLoadBalancers (\ s a -> s{_delbrElasticLoadBalancers = a}) . _Default;

-- | FIXME: Undocumented member.
delbrStatus :: Lens' DescribeElasticLoadBalancersResponse Int
delbrStatus = lens _delbrStatus (\ s a -> s{_delbrStatus = a});
