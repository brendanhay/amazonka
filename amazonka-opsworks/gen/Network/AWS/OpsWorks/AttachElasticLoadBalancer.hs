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
-- Module      : Network.AWS.OpsWorks.AttachElasticLoadBalancer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an Elastic Load Balancing load balancer to a specified layer.
-- For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/load-balancer-elb.html Elastic Load Balancing>.
--
-- You must create the Elastic Load Balancing instance separately, by using
-- the Elastic Load Balancing console, API, or CLI. For more information,
-- see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/Welcome.html Elastic Load Balancing Developer Guide>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_AttachElasticLoadBalancer.html AWS API Reference> for AttachElasticLoadBalancer.
module Network.AWS.OpsWorks.AttachElasticLoadBalancer
    (
    -- * Creating a Request
      AttachElasticLoadBalancer
    , attachElasticLoadBalancer
    -- * Request Lenses
    , aelbElasticLoadBalancerName
    , aelbLayerId

    -- * Destructuring the Response
    , AttachElasticLoadBalancerResponse
    , attachElasticLoadBalancerResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachElasticLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aelbElasticLoadBalancerName'
--
-- * 'aelbLayerId'
data AttachElasticLoadBalancer = AttachElasticLoadBalancer'
    { _aelbElasticLoadBalancerName :: !Text
    , _aelbLayerId                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachElasticLoadBalancer' smart constructor.
attachElasticLoadBalancer :: Text -> Text -> AttachElasticLoadBalancer
attachElasticLoadBalancer pElasticLoadBalancerName_ pLayerId_ =
    AttachElasticLoadBalancer'
    { _aelbElasticLoadBalancerName = pElasticLoadBalancerName_
    , _aelbLayerId = pLayerId_
    }

-- | The Elastic Load Balancing instance\'s name.
aelbElasticLoadBalancerName :: Lens' AttachElasticLoadBalancer Text
aelbElasticLoadBalancerName = lens _aelbElasticLoadBalancerName (\ s a -> s{_aelbElasticLoadBalancerName = a});

-- | The ID of the layer that the Elastic Load Balancing instance is to be
-- attached to.
aelbLayerId :: Lens' AttachElasticLoadBalancer Text
aelbLayerId = lens _aelbLayerId (\ s a -> s{_aelbLayerId = a});

instance AWSRequest AttachElasticLoadBalancer where
        type Sv AttachElasticLoadBalancer = OpsWorks
        type Rs AttachElasticLoadBalancer =
             AttachElasticLoadBalancerResponse
        request = postJSON
        response
          = receiveNull AttachElasticLoadBalancerResponse'

instance ToHeaders AttachElasticLoadBalancer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.AttachElasticLoadBalancer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AttachElasticLoadBalancer where
        toJSON AttachElasticLoadBalancer'{..}
          = object
              ["ElasticLoadBalancerName" .=
                 _aelbElasticLoadBalancerName,
               "LayerId" .= _aelbLayerId]

instance ToPath AttachElasticLoadBalancer where
        toPath = const "/"

instance ToQuery AttachElasticLoadBalancer where
        toQuery = const mempty

-- | /See:/ 'attachElasticLoadBalancerResponse' smart constructor.
data AttachElasticLoadBalancerResponse =
    AttachElasticLoadBalancerResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AttachElasticLoadBalancerResponse' smart constructor.
attachElasticLoadBalancerResponse :: AttachElasticLoadBalancerResponse
attachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse'
