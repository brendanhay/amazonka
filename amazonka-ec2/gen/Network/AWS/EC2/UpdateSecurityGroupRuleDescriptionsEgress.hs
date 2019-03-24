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
-- Module      : Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [EC2-VPC only] Updates the description of an egress (outbound) security group rule. You can replace an existing description, or add a description to a rule that did not have one previously.
--
--
-- You specify the description as part of the IP permissions structure. You can remove a description for a security group rule by omitting the description parameter in the request.
--
module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
    (
    -- * Creating a Request
      updateSecurityGroupRuleDescriptionsEgress
    , UpdateSecurityGroupRuleDescriptionsEgress
    -- * Request Lenses
    , usgrdeGroupId
    , usgrdeGroupName
    , usgrdeDryRun
    , usgrdeIPPermissions

    -- * Destructuring the Response
    , updateSecurityGroupRuleDescriptionsEgressResponse
    , UpdateSecurityGroupRuleDescriptionsEgressResponse
    -- * Response Lenses
    , usgrdersReturn
    , usgrdersResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSecurityGroupRuleDescriptionsEgress' smart constructor.
data UpdateSecurityGroupRuleDescriptionsEgress = UpdateSecurityGroupRuleDescriptionsEgress'
  { _usgrdeGroupId       :: !(Maybe Text)
  , _usgrdeGroupName     :: !(Maybe Text)
  , _usgrdeDryRun        :: !(Maybe Bool)
  , _usgrdeIPPermissions :: ![IPPermission]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSecurityGroupRuleDescriptionsEgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgrdeGroupId' - The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- * 'usgrdeGroupName' - [Default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- * 'usgrdeDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'usgrdeIPPermissions' - The IP permissions for the security group rule.
updateSecurityGroupRuleDescriptionsEgress
    :: UpdateSecurityGroupRuleDescriptionsEgress
updateSecurityGroupRuleDescriptionsEgress =
  UpdateSecurityGroupRuleDescriptionsEgress'
    { _usgrdeGroupId = Nothing
    , _usgrdeGroupName = Nothing
    , _usgrdeDryRun = Nothing
    , _usgrdeIPPermissions = mempty
    }


-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
usgrdeGroupId :: Lens' UpdateSecurityGroupRuleDescriptionsEgress (Maybe Text)
usgrdeGroupId = lens _usgrdeGroupId (\ s a -> s{_usgrdeGroupId = a})

-- | [Default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
usgrdeGroupName :: Lens' UpdateSecurityGroupRuleDescriptionsEgress (Maybe Text)
usgrdeGroupName = lens _usgrdeGroupName (\ s a -> s{_usgrdeGroupName = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
usgrdeDryRun :: Lens' UpdateSecurityGroupRuleDescriptionsEgress (Maybe Bool)
usgrdeDryRun = lens _usgrdeDryRun (\ s a -> s{_usgrdeDryRun = a})

-- | The IP permissions for the security group rule.
usgrdeIPPermissions :: Lens' UpdateSecurityGroupRuleDescriptionsEgress [IPPermission]
usgrdeIPPermissions = lens _usgrdeIPPermissions (\ s a -> s{_usgrdeIPPermissions = a}) . _Coerce

instance AWSRequest
           UpdateSecurityGroupRuleDescriptionsEgress
         where
        type Rs UpdateSecurityGroupRuleDescriptionsEgress =
             UpdateSecurityGroupRuleDescriptionsEgressResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 UpdateSecurityGroupRuleDescriptionsEgressResponse'
                   <$> (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable
           UpdateSecurityGroupRuleDescriptionsEgress
         where

instance NFData
           UpdateSecurityGroupRuleDescriptionsEgress
         where

instance ToHeaders
           UpdateSecurityGroupRuleDescriptionsEgress
         where
        toHeaders = const mempty

instance ToPath
           UpdateSecurityGroupRuleDescriptionsEgress
         where
        toPath = const "/"

instance ToQuery
           UpdateSecurityGroupRuleDescriptionsEgress
         where
        toQuery
          UpdateSecurityGroupRuleDescriptionsEgress'{..}
          = mconcat
              ["Action" =:
                 ("UpdateSecurityGroupRuleDescriptionsEgress" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "GroupId" =: _usgrdeGroupId,
               "GroupName" =: _usgrdeGroupName,
               "DryRun" =: _usgrdeDryRun,
               toQueryList "IpPermissions" _usgrdeIPPermissions]

-- | /See:/ 'updateSecurityGroupRuleDescriptionsEgressResponse' smart constructor.
data UpdateSecurityGroupRuleDescriptionsEgressResponse = UpdateSecurityGroupRuleDescriptionsEgressResponse'
  { _usgrdersReturn         :: !(Maybe Bool)
  , _usgrdersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSecurityGroupRuleDescriptionsEgressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgrdersReturn' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- * 'usgrdersResponseStatus' - -- | The response status code.
updateSecurityGroupRuleDescriptionsEgressResponse
    :: Int -- ^ 'usgrdersResponseStatus'
    -> UpdateSecurityGroupRuleDescriptionsEgressResponse
updateSecurityGroupRuleDescriptionsEgressResponse pResponseStatus_ =
  UpdateSecurityGroupRuleDescriptionsEgressResponse'
    {_usgrdersReturn = Nothing, _usgrdersResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, returns an error.
usgrdersReturn :: Lens' UpdateSecurityGroupRuleDescriptionsEgressResponse (Maybe Bool)
usgrdersReturn = lens _usgrdersReturn (\ s a -> s{_usgrdersReturn = a})

-- | -- | The response status code.
usgrdersResponseStatus :: Lens' UpdateSecurityGroupRuleDescriptionsEgressResponse Int
usgrdersResponseStatus = lens _usgrdersResponseStatus (\ s a -> s{_usgrdersResponseStatus = a})

instance NFData
           UpdateSecurityGroupRuleDescriptionsEgressResponse
         where
