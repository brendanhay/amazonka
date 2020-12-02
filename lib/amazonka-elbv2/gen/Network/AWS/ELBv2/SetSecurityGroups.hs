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
-- Module      : Network.AWS.ELBv2.SetSecurityGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified security groups with the specified Application Load Balancer. The specified security groups override the previously associated security groups.
--
--
-- Note that you can't specify a security group for a Network Load Balancer.
--
module Network.AWS.ELBv2.SetSecurityGroups
    (
    -- * Creating a Request
      setSecurityGroups
    , SetSecurityGroups
    -- * Request Lenses
    , ssgLoadBalancerARN
    , ssgSecurityGroups

    -- * Destructuring the Response
    , setSecurityGroupsResponse
    , SetSecurityGroupsResponse
    -- * Response Lenses
    , ssgrsSecurityGroupIds
    , ssgrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setSecurityGroups' smart constructor.
data SetSecurityGroups = SetSecurityGroups'
  { _ssgLoadBalancerARN :: !Text
  , _ssgSecurityGroups  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetSecurityGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssgLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'ssgSecurityGroups' - The IDs of the security groups.
setSecurityGroups
    :: Text -- ^ 'ssgLoadBalancerARN'
    -> SetSecurityGroups
setSecurityGroups pLoadBalancerARN_ =
  SetSecurityGroups'
    {_ssgLoadBalancerARN = pLoadBalancerARN_, _ssgSecurityGroups = mempty}


-- | The Amazon Resource Name (ARN) of the load balancer.
ssgLoadBalancerARN :: Lens' SetSecurityGroups Text
ssgLoadBalancerARN = lens _ssgLoadBalancerARN (\ s a -> s{_ssgLoadBalancerARN = a})

-- | The IDs of the security groups.
ssgSecurityGroups :: Lens' SetSecurityGroups [Text]
ssgSecurityGroups = lens _ssgSecurityGroups (\ s a -> s{_ssgSecurityGroups = a}) . _Coerce

instance AWSRequest SetSecurityGroups where
        type Rs SetSecurityGroups = SetSecurityGroupsResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "SetSecurityGroupsResult"
              (\ s h x ->
                 SetSecurityGroupsResponse' <$>
                   (x .@? "SecurityGroupIds" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable SetSecurityGroups where

instance NFData SetSecurityGroups where

instance ToHeaders SetSecurityGroups where
        toHeaders = const mempty

instance ToPath SetSecurityGroups where
        toPath = const "/"

instance ToQuery SetSecurityGroups where
        toQuery SetSecurityGroups'{..}
          = mconcat
              ["Action" =: ("SetSecurityGroups" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "LoadBalancerArn" =: _ssgLoadBalancerARN,
               "SecurityGroups" =:
                 toQueryList "member" _ssgSecurityGroups]

-- | /See:/ 'setSecurityGroupsResponse' smart constructor.
data SetSecurityGroupsResponse = SetSecurityGroupsResponse'
  { _ssgrsSecurityGroupIds :: !(Maybe [Text])
  , _ssgrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssgrsSecurityGroupIds' - The IDs of the security groups associated with the load balancer.
--
-- * 'ssgrsResponseStatus' - -- | The response status code.
setSecurityGroupsResponse
    :: Int -- ^ 'ssgrsResponseStatus'
    -> SetSecurityGroupsResponse
setSecurityGroupsResponse pResponseStatus_ =
  SetSecurityGroupsResponse'
    {_ssgrsSecurityGroupIds = Nothing, _ssgrsResponseStatus = pResponseStatus_}


-- | The IDs of the security groups associated with the load balancer.
ssgrsSecurityGroupIds :: Lens' SetSecurityGroupsResponse [Text]
ssgrsSecurityGroupIds = lens _ssgrsSecurityGroupIds (\ s a -> s{_ssgrsSecurityGroupIds = a}) . _Default . _Coerce

-- | -- | The response status code.
ssgrsResponseStatus :: Lens' SetSecurityGroupsResponse Int
ssgrsResponseStatus = lens _ssgrsResponseStatus (\ s a -> s{_ssgrsResponseStatus = a})

instance NFData SetSecurityGroupsResponse where
