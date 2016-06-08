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
-- Module      : Network.AWS.IAM.GetAccountSummary
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about IAM entity usage and IAM quotas in the AWS account.
--
-- For information about limitations on IAM entities, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/.
module Network.AWS.IAM.GetAccountSummary
    (
    -- * Creating a Request
      getAccountSummary
    , GetAccountSummary

    -- * Destructuring the Response
    , getAccountSummaryResponse
    , GetAccountSummaryResponse
    -- * Response Lenses
    , gasrsSummaryMap
    , gasrsResponseStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getAccountSummary' smart constructor.
data GetAccountSummary =
    GetAccountSummary'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAccountSummary' with the minimum fields required to make a request.
--
getAccountSummary
    :: GetAccountSummary
getAccountSummary = GetAccountSummary'

instance AWSRequest GetAccountSummary where
        type Rs GetAccountSummary = GetAccountSummaryResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetAccountSummaryResult"
              (\ s h x ->
                 GetAccountSummaryResponse' <$>
                   (x .@? "SummaryMap" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance Hashable GetAccountSummary

instance NFData GetAccountSummary

instance ToHeaders GetAccountSummary where
        toHeaders = const mempty

instance ToPath GetAccountSummary where
        toPath = const "/"

instance ToQuery GetAccountSummary where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("GetAccountSummary" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | Contains the response to a successful < GetAccountSummary> request.
--
-- /See:/ 'getAccountSummaryResponse' smart constructor.
data GetAccountSummaryResponse = GetAccountSummaryResponse'
    { _gasrsSummaryMap     :: !(Maybe (Map SummaryKeyType Int))
    , _gasrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetAccountSummaryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasrsSummaryMap'
--
-- * 'gasrsResponseStatus'
getAccountSummaryResponse
    :: Int -- ^ 'gasrsResponseStatus'
    -> GetAccountSummaryResponse
getAccountSummaryResponse pResponseStatus_ =
    GetAccountSummaryResponse'
    { _gasrsSummaryMap = Nothing
    , _gasrsResponseStatus = pResponseStatus_
    }

-- | A set of key value pairs containing information about IAM entity usage and IAM quotas.
--
-- 'SummaryMap' contains the following keys:
--
-- -   __AccessKeysPerUserQuota__
--
--     The maximum number of active access keys allowed for each IAM user.
--
-- -   __AccountAccessKeysPresent__
--
--     This value is 1 if the AWS account (root) has an access key, otherwise it is 0.
--
-- -   __AccountMFAEnabled__
--
--     This value is 1 if the AWS account (root) has an MFA device assigned, otherwise it is 0.
--
-- -   __AccountSigningCertificatesPresent__
--
--     This value is 1 if the AWS account (root) has a signing certificate, otherwise it is 0.
--
-- -   __AssumeRolePolicySizeQuota__
--
--     The maximum allowed size for assume role policy documents (trust policies), in non-whitespace characters.
--
-- -   __AttachedPoliciesPerGroupQuota__
--
--     The maximum number of managed policies that can be attached to an IAM group.
--
-- -   __AttachedPoliciesPerRoleQuota__
--
--     The maximum number of managed policies that can be attached to an IAM role.
--
-- -   __AttachedPoliciesPerUserQuota__
--
--     The maximum number of managed policies that can be attached to an IAM user.
--
-- -   __GroupPolicySizeQuota__
--
--     The maximum allowed size for the aggregate of all inline policies embedded in an IAM group, in non-whitespace characters.
--
-- -   __Groups__
--
--     The number of IAM groups in the AWS account.
--
-- -   __GroupsPerUserQuota__
--
--     The maximum number of IAM groups each IAM user can belong to.
--
-- -   __GroupsQuota__
--
--     The maximum number of IAM groups allowed in the AWS account.
--
-- -   __InstanceProfiles__
--
--     The number of instance profiles in the AWS account.
--
-- -   __InstanceProfilesQuota__
--
--     The maximum number of instance profiles allowed in the AWS account.
--
-- -   __MFADevices__
--
--     The number of MFA devices in the AWS account, including those assigned and unassigned.
--
-- -   __MFADevicesInUse__
--
--     The number of MFA devices that have been assigned to an IAM user or to the AWS account (root).
--
-- -   __Policies__
--
--     The number of customer managed policies in the AWS account.
--
-- -   __PoliciesQuota__
--
--     The maximum number of customer managed policies allowed in the AWS account.
--
-- -   __PolicySizeQuota__
--
--     The maximum allowed size of a customer managed policy, in non-whitespace characters.
--
-- -   __PolicyVersionsInUse__
--
--     The number of managed policies that are attached to IAM users, groups, or roles in the AWS account.
--
-- -   __PolicyVersionsInUseQuota__
--
--     The maximum number of managed policies that can be attached to IAM users, groups, or roles in the AWS account.
--
-- -   __Providers__
--
--     The number of identity providers in the AWS account.
--
-- -   __RolePolicySizeQuota__
--
--     The maximum allowed size for the aggregate of all inline policies (access policies, not the trust policy) embedded in an IAM role, in non-whitespace characters.
--
-- -   __Roles__
--
--     The number of IAM roles in the AWS account.
--
-- -   __RolesQuota__
--
--     The maximum number of IAM roles allowed in the AWS account.
--
-- -   __ServerCertificates__
--
--     The number of server certificates in the AWS account.
--
-- -   __ServerCertificatesQuota__
--
--     The maximum number of server certificates allowed in the AWS account.
--
-- -   __SigningCertificatesPerUserQuota__
--
--     The maximum number of X.509 signing certificates allowed for each IAM user.
--
-- -   __UserPolicySizeQuota__
--
--     The maximum allowed size for the aggregate of all inline policies embedded in an IAM user, in non-whitespace characters.
--
-- -   __Users__
--
--     The number of IAM users in the AWS account.
--
-- -   __UsersQuota__
--
--     The maximum number of IAM users allowed in the AWS account.
--
-- -   __VersionsPerPolicyQuota__
--
--     The maximum number of policy versions allowed for each managed policy.
--
gasrsSummaryMap :: Lens' GetAccountSummaryResponse (HashMap SummaryKeyType Int)
gasrsSummaryMap = lens _gasrsSummaryMap (\ s a -> s{_gasrsSummaryMap = a}) . _Default . _Map;

-- | The response status code.
gasrsResponseStatus :: Lens' GetAccountSummaryResponse Int
gasrsResponseStatus = lens _gasrsResponseStatus (\ s a -> s{_gasrsResponseStatus = a});

instance NFData GetAccountSummaryResponse
