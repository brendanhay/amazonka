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

-- | Retrieves account level information about account entity usage and IAM quotas.
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
    deriving (Eq, Ord, Show, Generic)

-- | 'GetAccountSummary' constructor.
getAccountSummary :: GetAccountSummary
getAccountSummary = GetAccountSummary

newtype GetAccountSummaryResponse = GetAccountSummaryResponse
    { _gasrSummaryMap :: EMap "entry" "key" "value" SummaryKeyType Int
    } deriving (Eq, Show, Monoid, Semigroup)

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

-- | A set of key value pairs containing account-level information.
--
-- 'SummaryMap' contains the following keys:  'AccessKeysPerUserQuota' - Maximum
-- number of access keys that can be created per user
--
-- 'AccountMFAEnabled' - 1 if the root account has an MFA device assigned to it,
-- 0 otherwise
--
-- 'AssumeRolePolicySizeQuota' - Maximum allowed size for assume role policy
-- documents (in kilobytes)
--
-- 'GroupPolicySizeQuota' - Maximum allowed size for Group policy documents (in
-- kilobytes)
--
-- 'Groups' - Number of Groups for the AWS account
--
-- 'GroupsPerUserQuota' - Maximum number of groups an IAM user can belong to
--
-- 'GroupsQuota' - Maximum groups allowed for the AWS account
--
-- 'InstanceProfiles' - Number of instance profiles for the AWS account
--
-- 'InstanceProfilesQuota' - Maximum instance profiles allowed for the AWS account
--
-- 'MFADevices' - Number of MFA devices, either assigned or unassigned
--
-- 'MFADevicesInUse' - Number of MFA devices that have been assigned to an IAM
-- user or to the root account
--
-- 'RolePolicySizeQuota' - Maximum allowed size for role policy documents (in
-- kilobytes)
--
-- 'Roles' - Number of roles for the AWS account
--
-- 'RolesQuota' - Maximum roles allowed for the AWS account
--
-- 'ServerCertificates' - Number of server certificates for the AWS account
--
-- 'ServerCertificatesQuota' - Maximum server certificates allowed for the AWS
-- account
--
-- 'SigningCertificatesPerUserQuota' - Maximum number of X509 certificates
-- allowed for a user
--
-- 'UserPolicySizeQuota' - Maximum allowed size for user policy documents (in
-- kilobytes)
--
-- 'Users' - Number of users for the AWS account
--
-- 'UsersQuota' - Maximum users allowed for the AWS account
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
        <$> x .@  "SummaryMap"
