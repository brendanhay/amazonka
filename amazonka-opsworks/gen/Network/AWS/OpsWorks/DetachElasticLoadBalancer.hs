{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DetachElasticLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Detaches a specified Elastic Load Balancing instance from its layer.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DetachElasticLoadBalancer.html>
module Network.AWS.OpsWorks.DetachElasticLoadBalancer
    (
    -- * Request
      DetachElasticLoadBalancer
    -- ** Request constructor
    , detachElasticLoadBalancer
    -- ** Request lenses
    , delbElasticLoadBalancerName
    , delbLayerId

    -- * Response
    , DetachElasticLoadBalancerResponse
    -- ** Response constructor
    , detachElasticLoadBalancerResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachElasticLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delbElasticLoadBalancerName'
--
-- * 'delbLayerId'
data DetachElasticLoadBalancer = DetachElasticLoadBalancer'
    { _delbElasticLoadBalancerName :: !Text
    , _delbLayerId                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachElasticLoadBalancer' smart constructor.
detachElasticLoadBalancer :: Text -> Text -> DetachElasticLoadBalancer
detachElasticLoadBalancer pElasticLoadBalancerName pLayerId =
    DetachElasticLoadBalancer'
    { _delbElasticLoadBalancerName = pElasticLoadBalancerName
    , _delbLayerId = pLayerId
    }

-- | The Elastic Load Balancing instance\'s name.
delbElasticLoadBalancerName :: Lens' DetachElasticLoadBalancer Text
delbElasticLoadBalancerName = lens _delbElasticLoadBalancerName (\ s a -> s{_delbElasticLoadBalancerName = a});

-- | The ID of the layer that the Elastic Load Balancing instance is attached
-- to.
delbLayerId :: Lens' DetachElasticLoadBalancer Text
delbLayerId = lens _delbLayerId (\ s a -> s{_delbLayerId = a});

instance AWSRequest DetachElasticLoadBalancer where
        type Sv DetachElasticLoadBalancer = OpsWorks
        type Rs DetachElasticLoadBalancer =
             DetachElasticLoadBalancerResponse
        request = postJSON
        response
          = receiveNull DetachElasticLoadBalancerResponse'

instance ToHeaders DetachElasticLoadBalancer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DetachElasticLoadBalancer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetachElasticLoadBalancer where
        toJSON DetachElasticLoadBalancer'{..}
          = object
              ["ElasticLoadBalancerName" .=
                 _delbElasticLoadBalancerName,
               "LayerId" .= _delbLayerId]

instance ToPath DetachElasticLoadBalancer where
        toPath = const "/"

instance ToQuery DetachElasticLoadBalancer where
        toQuery = const mempty

-- | /See:/ 'detachElasticLoadBalancerResponse' smart constructor.
data DetachElasticLoadBalancerResponse =
    DetachElasticLoadBalancerResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachElasticLoadBalancerResponse' smart constructor.
detachElasticLoadBalancerResponse :: DetachElasticLoadBalancerResponse
detachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse'
