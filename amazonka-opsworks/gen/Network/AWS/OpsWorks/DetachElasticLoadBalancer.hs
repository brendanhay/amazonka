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
-- Module      : Network.AWS.OpsWorks.DetachElasticLoadBalancer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a specified Elastic Load Balancing instance from its layer.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DetachElasticLoadBalancer
    (
    -- * Creating a Request
      detachElasticLoadBalancer
    , DetachElasticLoadBalancer
    -- * Request Lenses
    , delbElasticLoadBalancerName
    , delbLayerId

    -- * Destructuring the Response
    , detachElasticLoadBalancerResponse
    , DetachElasticLoadBalancerResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachElasticLoadBalancer' smart constructor.
data DetachElasticLoadBalancer = DetachElasticLoadBalancer'
  { _delbElasticLoadBalancerName :: !Text
  , _delbLayerId                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachElasticLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delbElasticLoadBalancerName' - The Elastic Load Balancing instance's name.
--
-- * 'delbLayerId' - The ID of the layer that the Elastic Load Balancing instance is attached to.
detachElasticLoadBalancer
    :: Text -- ^ 'delbElasticLoadBalancerName'
    -> Text -- ^ 'delbLayerId'
    -> DetachElasticLoadBalancer
detachElasticLoadBalancer pElasticLoadBalancerName_ pLayerId_ =
  DetachElasticLoadBalancer'
    { _delbElasticLoadBalancerName = pElasticLoadBalancerName_
    , _delbLayerId = pLayerId_
    }


-- | The Elastic Load Balancing instance's name.
delbElasticLoadBalancerName :: Lens' DetachElasticLoadBalancer Text
delbElasticLoadBalancerName = lens _delbElasticLoadBalancerName (\ s a -> s{_delbElasticLoadBalancerName = a})

-- | The ID of the layer that the Elastic Load Balancing instance is attached to.
delbLayerId :: Lens' DetachElasticLoadBalancer Text
delbLayerId = lens _delbLayerId (\ s a -> s{_delbLayerId = a})

instance AWSRequest DetachElasticLoadBalancer where
        type Rs DetachElasticLoadBalancer =
             DetachElasticLoadBalancerResponse
        request = postJSON opsWorks
        response
          = receiveNull DetachElasticLoadBalancerResponse'

instance Hashable DetachElasticLoadBalancer where

instance NFData DetachElasticLoadBalancer where

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
              (catMaybes
                 [Just
                    ("ElasticLoadBalancerName" .=
                       _delbElasticLoadBalancerName),
                  Just ("LayerId" .= _delbLayerId)])

instance ToPath DetachElasticLoadBalancer where
        toPath = const "/"

instance ToQuery DetachElasticLoadBalancer where
        toQuery = const mempty

-- | /See:/ 'detachElasticLoadBalancerResponse' smart constructor.
data DetachElasticLoadBalancerResponse =
  DetachElasticLoadBalancerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachElasticLoadBalancerResponse' with the minimum fields required to make a request.
--
detachElasticLoadBalancerResponse
    :: DetachElasticLoadBalancerResponse
detachElasticLoadBalancerResponse = DetachElasticLoadBalancerResponse'


instance NFData DetachElasticLoadBalancerResponse
         where
