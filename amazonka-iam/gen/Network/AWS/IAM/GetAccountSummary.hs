{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves account level information about account entity usage and IAM
-- quotas. For information about limitations on IAM entities, see Limitations
-- on IAM Entities in the Using IAM guide. https://iam.amazonaws.com/
-- ?Action=GetAccountSummary &Version=2010-05-08 &AUTHPARAMS Groups 31
-- GroupsQuota 50 UsersQuota 150 Users 35 GroupPolicySizeQuota 10240
-- AccessKeysPerUserQuota 2 GroupsPerUserQuota 10 UserPolicySizeQuota 10240
-- SigningCertificatesPerUserQuota 2 ServerCertificates 0
-- ServerCertificatesQuota 10 AccountMFAEnabled 0 MFADevicesInUse 10
-- MFADevices 20 f1e38443-f1ad-11df-b1ef-a9265EXAMPLE.
module Network.AWS.IAM
    (
    -- * Request
      GetAccountSummary
    -- ** Request constructor
    , mkGetAccountSummary
    -- * Response
    , GetAccountSummaryResponse
    -- ** Response constructor
    , mkGetAccountSummaryResponse
    -- ** Response lenses
    , gasrSummaryMap
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data GetAccountSummary = GetAccountSummary
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetAccountSummary' request.
mkGetAccountSummary :: GetAccountSummary
mkGetAccountSummary = GetAccountSummary

instance ToQuery GetAccountSummary where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetAccountSummary
-- action.
newtype GetAccountSummaryResponse = GetAccountSummaryResponse
    { _gasrSummaryMap :: Map SummaryKeyType Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetAccountSummaryResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SummaryMap ::@ @Map SummaryKeyType Integer@
--
mkGetAccountSummaryResponse :: GetAccountSummaryResponse
mkGetAccountSummaryResponse = GetAccountSummaryResponse
    { _gasrSummaryMap = mempty
    }

-- | A set of key value pairs containing account-level information. SummaryMap
-- contains the following keys: AccessKeysPerUserQuota - Maximum number of
-- access keys that can be created per user AccountMFAEnabled - 1 if the root
-- account has an MFA device assigned to it, 0 otherwise
-- AssumeRolePolicySizeQuota - Maximum allowed size for assume role policy
-- documents (in kilobytes) GroupPolicySizeQuota - Maximum allowed size for
-- Group policy documents (in kilobytes) Groups - Number of Groups for the AWS
-- account GroupsPerUserQuota - Maximum number of groups a user can belong to
-- GroupsQuota - Maximum groups allowed for the AWS account InstanceProfiles -
-- Number of instance profiles for the AWS account InstanceProfilesQuota -
-- Maximum instance profiles allowed for the AWS account MFADevices - Number
-- of MFA devices, either assigned or unassigned MFADevicesInUse - Number of
-- MFA devices that have been assigned to an IAM user or to the root account
-- RolePolicySizeQuota - Maximum allowed size for role policy documents (in
-- kilobytes) Roles - Number of roles for the AWS account RolesQuota - Maximum
-- roles allowed for the AWS account ServerCertificates - Number of server
-- certificates for the AWS account ServerCertificatesQuota - Maximum server
-- certificates allowed for the AWS account SigningCertificatesPerUserQuota -
-- Maximum number of X509 certificates allowed for a user UserPolicySizeQuota
-- - Maximum allowed size for user policy documents (in kilobytes) Users -
-- Number of users for the AWS account UsersQuota - Maximum users allowed for
-- the AWS account.
gasrSummaryMap :: Lens' GetAccountSummaryResponse (Map SummaryKeyType Integer)
gasrSummaryMap = lens _gasrSummaryMap (\s a -> s { _gasrSummaryMap = a })

instance FromXML GetAccountSummaryResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetAccountSummary where
    type Sv GetAccountSummary = IAM
    type Rs GetAccountSummary = GetAccountSummaryResponse

    request = post "GetAccountSummary"
    response _ = xmlResponse
