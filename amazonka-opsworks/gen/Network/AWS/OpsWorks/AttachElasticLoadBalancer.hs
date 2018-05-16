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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an Elastic Load Balancing load balancer to a specified layer. AWS OpsWorks Stacks does not support Application Load Balancer. You can only use Classic Load Balancer with AWS OpsWorks Stacks. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/layers-elb.html Elastic Load Balancing> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.AttachElasticLoadBalancer
    (
    -- * Creating a Request
      attachElasticLoadBalancer
    , AttachElasticLoadBalancer
    -- * Request Lenses
    , aelbElasticLoadBalancerName
    , aelbLayerId

    -- * Destructuring the Response
    , attachElasticLoadBalancerResponse
    , AttachElasticLoadBalancerResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachElasticLoadBalancer' smart constructor.
data AttachElasticLoadBalancer = AttachElasticLoadBalancer'
  { _aelbElasticLoadBalancerName :: !Text
  , _aelbLayerId                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachElasticLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aelbElasticLoadBalancerName' - The Elastic Load Balancing instance's name.
--
-- * 'aelbLayerId' - The ID of the layer that the Elastic Load Balancing instance is to be attached to.
attachElasticLoadBalancer
    :: Text -- ^ 'aelbElasticLoadBalancerName'
    -> Text -- ^ 'aelbLayerId'
    -> AttachElasticLoadBalancer
attachElasticLoadBalancer pElasticLoadBalancerName_ pLayerId_ =
  AttachElasticLoadBalancer'
    { _aelbElasticLoadBalancerName = pElasticLoadBalancerName_
    , _aelbLayerId = pLayerId_
    }


-- | The Elastic Load Balancing instance's name.
aelbElasticLoadBalancerName :: Lens' AttachElasticLoadBalancer Text
aelbElasticLoadBalancerName = lens _aelbElasticLoadBalancerName (\ s a -> s{_aelbElasticLoadBalancerName = a})

-- | The ID of the layer that the Elastic Load Balancing instance is to be attached to.
aelbLayerId :: Lens' AttachElasticLoadBalancer Text
aelbLayerId = lens _aelbLayerId (\ s a -> s{_aelbLayerId = a})

instance AWSRequest AttachElasticLoadBalancer where
        type Rs AttachElasticLoadBalancer =
             AttachElasticLoadBalancerResponse
        request = postJSON opsWorks
        response
          = receiveNull AttachElasticLoadBalancerResponse'

instance Hashable AttachElasticLoadBalancer where

instance NFData AttachElasticLoadBalancer where

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
              (catMaybes
                 [Just
                    ("ElasticLoadBalancerName" .=
                       _aelbElasticLoadBalancerName),
                  Just ("LayerId" .= _aelbLayerId)])

instance ToPath AttachElasticLoadBalancer where
        toPath = const "/"

instance ToQuery AttachElasticLoadBalancer where
        toQuery = const mempty

-- | /See:/ 'attachElasticLoadBalancerResponse' smart constructor.
data AttachElasticLoadBalancerResponse =
  AttachElasticLoadBalancerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachElasticLoadBalancerResponse' with the minimum fields required to make a request.
--
attachElasticLoadBalancerResponse
    :: AttachElasticLoadBalancerResponse
attachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse'


instance NFData AttachElasticLoadBalancerResponse
         where
