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

-- | A set of key value pairs containing account-level information.
--
-- 'SummaryMap' contains the following keys:   'AccessKeysPerUserQuota' - Maximum
-- number of active access keys allowed per IAM user
--
-- 'AccountAccessKeysPresent' - 1 if the root account has an access key, 0
-- otherwise
--
-- 'AccountMFAEnabled' - 1 if the root account has an MFA device assigned to
-- it, 0 otherwise
--
-- 'AccountSigningCertificatesPresent' - 1 if the root account has a signing
-- certificate, 0 otherwise
--
-- 'AssumeRolePolicySizeQuota' - Maximum allowed size for assume role policy
-- documents (trust policies), in non-whitespace characters
--
-- 'GroupPolicySizeQuota' - Maximum allowed size for IAM group policy
-- documents, in non-whitespace characters
--
-- 'Groups' - Number of IAM groups in the AWS account
--
-- 'GroupsPerUserQuota' - Maximum number of IAM groups each IAM user can belong
-- to
--
-- 'GroupsQuota' - Maximum number of IAM groups allowed in the AWS account
--
-- 'InstanceProfiles' - Number of instance profiles in the AWS account
--
-- 'InstanceProfilesQuota' - Maximum number of instance profiles allowed in the
-- AWS account
--
-- 'MFADevices' - Number of MFA devices, either assigned or unassigned
--
-- 'MFADevicesInUse' - Number of MFA devices that have been assigned to an IAM
-- user or to the root account
--
-- 'RolePolicySizeQuota' - Maximum allowed size for IAM role policy documents
-- (permissions policies), in non-whitespace characters
--
-- 'Roles' - Number of roles IAM in the AWS account
--
-- 'RolesQuota' - Maximum number of IAM roles allowed in the AWS account
--
-- 'UserPolicySizeQuota' - Maximum allowed size for IAM user policy documents,
-- in non-whitespace characters
--
-- 'Users' - Number of IAM users in the AWS account
--
-- 'UsersQuota' - Maximum number of IAM users allowed in the AWS account
--
-- 'ServerCertificates' - Number of server certificates in the AWS account
--
-- 'ServerCertificatesQuota' - Maximum number of server certificates allowed in
-- the AWS account
--
-- 'SigningCertificatesPerUserQuota' - Maximum number of X509 signing
-- certificates allowed per IAM user
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
