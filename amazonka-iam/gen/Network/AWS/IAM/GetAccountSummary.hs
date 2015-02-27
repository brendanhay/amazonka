{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetAccountSummary
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves information about IAM entity usage and IAM quotas in the AWS
-- account.
--
-- For information about limitations on IAM entities, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAMEntities> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountSummary.html>
module Network.AWS.IAM.GetAccountSummary
    (
    -- * Request
      GetAccountSummary
    -- ** Request constructor
    , getAccountSummary

    -- * Response
    , GetAccountSummaryResponse
    -- ** Response constructor
    , getAccountSummaryResponse
    -- ** Response lenses
    , gasrSummaryMap
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data GetAccountSummary = GetAccountSummary
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'GetAccountSummary' constructor.
getAccountSummary :: GetAccountSummary
getAccountSummary = GetAccountSummary

newtype GetAccountSummaryResponse = GetAccountSummaryResponse
    { _gasrSummaryMap :: EMap "entry" "key" "value" SummaryKeyType Int
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'GetAccountSummaryResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gasrSummaryMap' @::@ 'HashMap' 'SummaryKeyType' 'Int'
--
getAccountSummaryResponse :: GetAccountSummaryResponse
getAccountSummaryResponse = GetAccountSummaryResponse
    { _gasrSummaryMap = mempty
    }

-- | A set of key value pairs containing information about IAM entity usage and
-- IAM quotas.
--
-- 'SummaryMap' contains the following keys:   AccessKeysPerUserQuota
--
-- The maximum number of active access keys allowed for each IAM user.
--
-- AccountAccessKeysPresent
--
-- This value is 1 if the AWS account (root) has an access key, otherwise it is
-- 0.
--
-- AccountMFAEnabled
--
-- This value is 1 if the AWS account (root) has an MFA device assigned,
-- otherwise it is 0.
--
-- AccountSigningCertificatesPresent
--
-- This value is 1 if the AWS account (root) has a signing certificate,
-- otherwise it is 0.
--
-- AssumeRolePolicySizeQuota
--
-- The maximum allowed size for assume role policy documents (trust policies),
-- in non-whitespace characters.
--
-- AttachedPoliciesPerGroupQuota
--
-- The maximum number of managed policies that can be attached to an IAM group.
--
-- AttachedPoliciesPerRoleQuota
--
-- The maximum number of managed policies that can be attached to an IAM role.
--
-- AttachedPoliciesPerUserQuota
--
-- The maximum number of managed policies that can be attached to an IAM user.
--
-- GroupPolicySizeQuota
--
-- The maximum allowed size for the aggregate of all inline policies embedded
-- in an IAM group, in non-whitespace characters.
--
-- Groups
--
-- The number of IAM groups in the AWS account.
--
-- GroupsPerUserQuota
--
-- The maximum number of IAM groups each IAM user can belong to.
--
-- GroupsQuota
--
-- The maximum number of IAM groups allowed in the AWS account.
--
-- InstanceProfiles
--
-- The number of instance profiles in the AWS account.
--
-- InstanceProfilesQuota
--
-- The maximum number of instance profiles allowed in the AWS account.
--
-- MFADevices
--
-- The number of MFA devices in the AWS account, including those assigned and
-- unassigned.
--
-- MFADevicesInUse
--
-- The number of MFA devices that have been assigned to an IAM user or to the
-- AWS account (root).
--
-- Policies
--
-- The number of customer managed policies in the AWS account.
--
-- PoliciesQuota
--
-- The maximum number of customer managed policies allowed in the AWS account.
--
-- PolicySizeQuota
--
-- The maximum allowed size of a customer managed policy, in non-whitespace
-- characters.
--
-- PolicyVersionsInUse
--
-- The number of managed policies that are attached to IAM users, groups, or
-- roles in the AWS account.
--
-- PolicyVersionsInUseQuota
--
-- The maximum number of managed policies that can be attached to IAM users,
-- groups, or roles in the AWS account.
--
-- Providers
--
-- The number of identity providers in the AWS account.
--
-- RolePolicySizeQuota
--
-- The maximum allowed size for the aggregate of all inline policies (access
-- policies, not the trust policy) embedded in an IAM role, in non-whitespace
-- characters.
--
-- Roles
--
-- The number of IAM roles in the AWS account.
--
-- RolesQuota
--
-- The maximum number of IAM roles allowed in the AWS account.
--
-- ServerCertificates
--
-- The number of server certificates in the AWS account.
--
-- ServerCertificatesQuota
--
-- The maximum number of server certificates allowed in the AWS account.
--
-- SigningCertificatesPerUserQuota
--
-- The maximum number of X.509 signing certificates allowed for each IAM user.
--
-- UserPolicySizeQuota
--
-- The maximum allowed size for the aggregate of all inline policies embedded
-- in an IAM user, in non-whitespace characters.
--
-- Users
--
-- The number of IAM users in the AWS account.
--
-- UsersQuota
--
-- The maximum number of IAM users allowed in the AWS account.
--
-- VersionsPerPolicyQuota
--
-- The maximum number of policy versions allowed for each managed policy.
--
--
gasrSummaryMap :: Lens' GetAccountSummaryResponse (HashMap SummaryKeyType Int)
gasrSummaryMap = lens _gasrSummaryMap (\s a -> s { _gasrSummaryMap = a }) . _EMap

instance ToPath GetAccountSummary where
    toPath = const "/"

instance ToQuery GetAccountSummary where
    toQuery = const mempty

instance ToHeaders GetAccountSummary

instance AWSRequest GetAccountSummary where
    type Sv GetAccountSummary = IAM
    type Rs GetAccountSummary = GetAccountSummaryResponse

    request  = post "GetAccountSummary"
    response = xmlResponse

instance FromXML GetAccountSummaryResponse where
    parseXML = withElement "GetAccountSummaryResult" $ \x -> GetAccountSummaryResponse
        <$> x .@? "SummaryMap" .!@ mempty
