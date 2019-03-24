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
-- Module      : Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of an ingress (inbound) security group rule. You can replace an existing description, or add a description to a rule that did not have one previously.
--
--
-- You specify the description as part of the IP permissions structure. You can remove a description for a security group rule by omitting the description parameter in the request.
--
module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
    (
    -- * Creating a Request
      updateSecurityGroupRuleDescriptionsIngress
    , UpdateSecurityGroupRuleDescriptionsIngress
    -- * Request Lenses
    , usgrdiGroupId
    , usgrdiGroupName
    , usgrdiDryRun
    , usgrdiIPPermissions

    -- * Destructuring the Response
    , updateSecurityGroupRuleDescriptionsIngressResponse
    , UpdateSecurityGroupRuleDescriptionsIngressResponse
    -- * Response Lenses
    , usgrdirsReturn
    , usgrdirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSecurityGroupRuleDescriptionsIngress' smart constructor.
data UpdateSecurityGroupRuleDescriptionsIngress = UpdateSecurityGroupRuleDescriptionsIngress'
  { _usgrdiGroupId       :: !(Maybe Text)
  , _usgrdiGroupName     :: !(Maybe Text)
  , _usgrdiDryRun        :: !(Maybe Bool)
  , _usgrdiIPPermissions :: ![IPPermission]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSecurityGroupRuleDescriptionsIngress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgrdiGroupId' - The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- * 'usgrdiGroupName' - [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- * 'usgrdiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'usgrdiIPPermissions' - The IP permissions for the security group rule.
updateSecurityGroupRuleDescriptionsIngress
    :: UpdateSecurityGroupRuleDescriptionsIngress
updateSecurityGroupRuleDescriptionsIngress =
  UpdateSecurityGroupRuleDescriptionsIngress'
    { _usgrdiGroupId = Nothing
    , _usgrdiGroupName = Nothing
    , _usgrdiDryRun = Nothing
    , _usgrdiIPPermissions = mempty
    }


-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
usgrdiGroupId :: Lens' UpdateSecurityGroupRuleDescriptionsIngress (Maybe Text)
usgrdiGroupId = lens _usgrdiGroupId (\ s a -> s{_usgrdiGroupId = a})

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
usgrdiGroupName :: Lens' UpdateSecurityGroupRuleDescriptionsIngress (Maybe Text)
usgrdiGroupName = lens _usgrdiGroupName (\ s a -> s{_usgrdiGroupName = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
usgrdiDryRun :: Lens' UpdateSecurityGroupRuleDescriptionsIngress (Maybe Bool)
usgrdiDryRun = lens _usgrdiDryRun (\ s a -> s{_usgrdiDryRun = a})

-- | The IP permissions for the security group rule.
usgrdiIPPermissions :: Lens' UpdateSecurityGroupRuleDescriptionsIngress [IPPermission]
usgrdiIPPermissions = lens _usgrdiIPPermissions (\ s a -> s{_usgrdiIPPermissions = a}) . _Coerce

instance AWSRequest
           UpdateSecurityGroupRuleDescriptionsIngress
         where
        type Rs UpdateSecurityGroupRuleDescriptionsIngress =
             UpdateSecurityGroupRuleDescriptionsIngressResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 UpdateSecurityGroupRuleDescriptionsIngressResponse'
                   <$> (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable
           UpdateSecurityGroupRuleDescriptionsIngress
         where

instance NFData
           UpdateSecurityGroupRuleDescriptionsIngress
         where

instance ToHeaders
           UpdateSecurityGroupRuleDescriptionsIngress
         where
        toHeaders = const mempty

instance ToPath
           UpdateSecurityGroupRuleDescriptionsIngress
         where
        toPath = const "/"

instance ToQuery
           UpdateSecurityGroupRuleDescriptionsIngress
         where
        toQuery
          UpdateSecurityGroupRuleDescriptionsIngress'{..}
          = mconcat
              ["Action" =:
                 ("UpdateSecurityGroupRuleDescriptionsIngress" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "GroupId" =: _usgrdiGroupId,
               "GroupName" =: _usgrdiGroupName,
               "DryRun" =: _usgrdiDryRun,
               toQueryList "IpPermissions" _usgrdiIPPermissions]

-- | /See:/ 'updateSecurityGroupRuleDescriptionsIngressResponse' smart constructor.
data UpdateSecurityGroupRuleDescriptionsIngressResponse = UpdateSecurityGroupRuleDescriptionsIngressResponse'
  { _usgrdirsReturn         :: !(Maybe Bool)
  , _usgrdirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSecurityGroupRuleDescriptionsIngressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgrdirsReturn' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- * 'usgrdirsResponseStatus' - -- | The response status code.
updateSecurityGroupRuleDescriptionsIngressResponse
    :: Int -- ^ 'usgrdirsResponseStatus'
    -> UpdateSecurityGroupRuleDescriptionsIngressResponse
updateSecurityGroupRuleDescriptionsIngressResponse pResponseStatus_ =
  UpdateSecurityGroupRuleDescriptionsIngressResponse'
    {_usgrdirsReturn = Nothing, _usgrdirsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, returns an error.
usgrdirsReturn :: Lens' UpdateSecurityGroupRuleDescriptionsIngressResponse (Maybe Bool)
usgrdirsReturn = lens _usgrdirsReturn (\ s a -> s{_usgrdirsReturn = a})

-- | -- | The response status code.
usgrdirsResponseStatus :: Lens' UpdateSecurityGroupRuleDescriptionsIngressResponse Int
usgrdirsResponseStatus = lens _usgrdirsResponseStatus (\ s a -> s{_usgrdirsResponseStatus = a})

instance NFData
           UpdateSecurityGroupRuleDescriptionsIngressResponse
         where
