{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.V2011_06_15.GetFederationToken
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of temporary security credentials (consisting of an access
-- key ID, a secret access key, and a security token) for a federated user. A
-- typical use is in a proxy application that gets temporary security
-- credentials on behalf of distributed applications inside a corporate
-- network. Because you must call the GetFederationToken action using the
-- long-term security credentials of an IAM user, this call is appropriate in
-- contexts where those credentials can be safely stored, usually in a
-- server-based application. Note: Do not use this call in mobile applications
-- or client-based web applications that directly get temporary security
-- credentials. For those types of applications, use
-- AssumeRoleWithWebIdentity. The GetFederationToken action must be called by
-- using the long-term AWS security credentials of an IAM user. You can also
-- call GetFederationToken using the security credentials of an AWS account
-- (root), but this is not recommended. Instead, we recommend that you create
-- an IAM user for the purpose of the proxy application and then attach a
-- policy to the IAM user that limits federated users to only the actions and
-- resources they need access to. For more information, see IAM Best Practices
-- in Using IAM. The temporary security credentials that are obtained by using
-- the long-term credentials of an IAM user are valid for the specified
-- duration, between 900 seconds (15 minutes) and 129600 seconds (36 hours).
-- Temporary credentials that are obtained by using AWS account (root)
-- credentials have a maximum duration of 3600 seconds (1 hour) Permissions
-- The permissions for the temporary security credentials returned by
-- GetFederationToken are determined by a combination of the following: The
-- policy or policies that are attached to the IAM user whose credentials are
-- used to call GetFederationToken. The policy that is passed as a parameter
-- in the call. The passed policy is attached to the temporary security
-- credentials that result from the GetFederationToken API call--that is, to
-- the federated user. When the federated user makes an AWS request, AWS
-- evaluates the policy attached to the federated user in combination with the
-- policy or policies attached to the IAM user whose credentials were used to
-- call GetFederationToken. AWS allows the federated user's request only when
-- both the federated user and the IAM user are explicitly allowed to perform
-- the requested action. The passed policy cannot grant more permissions than
-- those that are defined in the IAM user policy. A typical use case is that
-- the permissions of the IAM user whose credentials are used to call
-- GetFederationToken are designed to allow access to all the actions and
-- resources that any federated user will need. Then, for individual users,
-- you pass a policy to the operation that scopes down the permissions to a
-- level that's appropriate to that individual user, using a policy that
-- allows only a subset of permissions that are granted to the IAM user. If
-- you do not pass a policy, the resulting temporary security credentials have
-- no effective permissions. The only exception is when the temporary security
-- credentials are used to access a resource that has a resource-based policy
-- that specifically allows the federated user to access the resource. For
-- more information about how permissions work, see Permissions for
-- GetFederationToken in Using Temporary Security Credentials. For information
-- about using GetFederationToken to create temporary security credentials,
-- see Creating Temporary Credentials to Enable Access for Federated Users in
-- Using Temporary Security Credentials. https://sts.amazonaws.com/
-- ?Version=2011-06-15 &Action=GetFederationToken &Name=Bob
-- &Policy=%7B%22Version%22%3A%222012-10-17%22%2C%22Statement%22%3A%5B%7B%22Sid%22%3A%22Stmt1%22%2C%22Effect%22%
-- 3A%22Allow%22%2C%22Action%22%3A%22s3%3A*%22%2C%22Resource%22%3A%22*%22%7D
-- %5D%7D &DurationSeconds=3600 &AUTHPARAMS 2011-06-15/">
-- AQoDYXdzEPT//////////wEXAMPLEtc764bNrC9SAPBSM22wDOk4x4HIZ8j4FZTwdQW
-- LWsKWHGBuFqwAeMicRXmxfpSPfIeoIYRqTflfKD8YUuwthAx7mSEI/qkPpKPi/kMcGd
-- QrmGdeehM4IC1NtBmUpp2wUE8phUZampKsburEDy0KPkyQDYwT7WZ0wq5VSXDvp75YU
-- 9HFvlRd8Tx6q6fE8YQcHNVXAkiY9q6d+xo0rKwT38xVqr7ZD0u0iPPkUL64lIZbqBAz
-- +scqKmlzm8FDrypNC9Yjc8fPOLn9FX9KSYvKTr4rvx3iSIlTJabIQwj2ICCR/oLxBA==
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2011-07-15T23:28:33.359Z
-- AKIAIOSFODNN7EXAMPLE arn:aws:sts::123456789012:federated-user/Bob
-- 123456789012:Bob 6 c6104cbe-af31-11e0-8154-cbc7ccf896c7.
module Network.AWS.STS.V2011_06_15.GetFederationToken
    (
    -- * Request
      GetFederationToken
    -- ** Request constructor
    , getFederationToken
    -- ** Request lenses
    , gftrName
    , gftrDurationSeconds
    , gftrPolicy

    -- * Response
    , GetFederationTokenResponse
    -- ** Response lenses
    , gftsCredentials
    , gftsFederatedUser
    , gftsPackedPolicySize
    ) where

import Network.AWS.Request.Query
import Network.AWS.STS.V2011_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetFederationToken' request.
getFederationToken :: Text -- ^ 'gftrName'
                   -> GetFederationToken
getFederationToken p1 = GetFederationToken
    { _gftrName = p1
    , _gftrDurationSeconds = Nothing
    , _gftrPolicy = Nothing
    }
{-# INLINE getFederationToken #-}

data GetFederationToken = GetFederationToken
    { _gftrName :: Text
      -- ^ The name of the federated user. The name is used as an identifier
      -- for the temporary security credentials (such as Bob). For
      -- example, you can reference the federated user name in a
      -- resource-based policy, such as in an Amazon S3 bucket policy.
    , _gftrDurationSeconds :: Maybe Integer
      -- ^ The duration, in seconds, that the session should last.
      -- Acceptable durations for federation sessions range from 900
      -- seconds (15 minutes) to 129600 seconds (36 hours), with 43200
      -- seconds (12 hours) as the default. Sessions obtained using AWS
      -- account (root) credentials are restricted to a maximum of 3600
      -- seconds (one hour). If the specified duration is longer than one
      -- hour, the session obtained by using AWS account (root)
      -- credentials defaults to one hour.
    , _gftrPolicy :: Maybe Text
      -- ^ An IAM policy in JSON format that is passed with the
      -- GetFederationToken call and evaluated along with the policy or
      -- policies that are attached to the IAM user whose credentials are
      -- used to call GetFederationToken. The passed policy is used to
      -- scope down the permissions that are available to the IAM user, by
      -- allowing only a subset of the permissions that are granted to the
      -- IAM user. The passed policy cannot grant more permissions than
      -- those granted to the IAM user. The final permissions for the
      -- federated user are the most restrictive set based on the
      -- intersection of the passed policy and the IAM user policy. If you
      -- do not pass a policy, the resulting temporary security
      -- credentials have no effective permissions. The only exception is
      -- when the temporary security credentials are used to access a
      -- resource that has a resource-based policy that specifically
      -- allows the federated user to access the resource. For more
      -- information about how permissions work, see Permissions for
      -- GetFederationToken in Using Temporary Security Credentials.
    } deriving (Show, Generic)

-- | The name of the federated user. The name is used as an identifier for the
-- temporary security credentials (such as Bob). For example, you can
-- reference the federated user name in a resource-based policy, such as in an
-- Amazon S3 bucket policy.
gftrName :: Lens' GetFederationToken (Text)
gftrName f x =
    f (_gftrName x)
        <&> \y -> x { _gftrName = y }
{-# INLINE gftrName #-}

-- | The duration, in seconds, that the session should last. Acceptable
-- durations for federation sessions range from 900 seconds (15 minutes) to
-- 129600 seconds (36 hours), with 43200 seconds (12 hours) as the default.
-- Sessions obtained using AWS account (root) credentials are restricted to a
-- maximum of 3600 seconds (one hour). If the specified duration is longer
-- than one hour, the session obtained by using AWS account (root) credentials
-- defaults to one hour.
gftrDurationSeconds :: Lens' GetFederationToken (Maybe Integer)
gftrDurationSeconds f x =
    f (_gftrDurationSeconds x)
        <&> \y -> x { _gftrDurationSeconds = y }
{-# INLINE gftrDurationSeconds #-}

-- | An IAM policy in JSON format that is passed with the GetFederationToken
-- call and evaluated along with the policy or policies that are attached to
-- the IAM user whose credentials are used to call GetFederationToken. The
-- passed policy is used to scope down the permissions that are available to
-- the IAM user, by allowing only a subset of the permissions that are granted
-- to the IAM user. The passed policy cannot grant more permissions than those
-- granted to the IAM user. The final permissions for the federated user are
-- the most restrictive set based on the intersection of the passed policy and
-- the IAM user policy. If you do not pass a policy, the resulting temporary
-- security credentials have no effective permissions. The only exception is
-- when the temporary security credentials are used to access a resource that
-- has a resource-based policy that specifically allows the federated user to
-- access the resource. For more information about how permissions work, see
-- Permissions for GetFederationToken in Using Temporary Security Credentials.
gftrPolicy :: Lens' GetFederationToken (Maybe Text)
gftrPolicy f x =
    f (_gftrPolicy x)
        <&> \y -> x { _gftrPolicy = y }
{-# INLINE gftrPolicy #-}

instance ToQuery GetFederationToken where
    toQuery = genericQuery def

data GetFederationTokenResponse = GetFederationTokenResponse
    { _gftsCredentials :: Maybe Credentials
      -- ^ Credentials for the service API authentication.
    , _gftsFederatedUser :: Maybe FederatedUser
      -- ^ Identifiers for the federated user associated with the
      -- credentials (such as arn:aws:sts::123456789012:federated-user/Bob
      -- or 123456789012:Bob). You can use the federated user's ARN in
      -- your resource-based policies, such as an Amazon S3 bucket policy.
    , _gftsPackedPolicySize :: Maybe Integer
      -- ^ A percentage value indicating the size of the policy in packed
      -- form. The service rejects policies for which the packed size is
      -- greater than 100 percent of the allowed value.
    } deriving (Show, Generic)

-- | Credentials for the service API authentication.
gftsCredentials :: Lens' GetFederationTokenResponse (Maybe Credentials)
gftsCredentials f x =
    f (_gftsCredentials x)
        <&> \y -> x { _gftsCredentials = y }
{-# INLINE gftsCredentials #-}

-- | Identifiers for the federated user associated with the credentials (such as
-- arn:aws:sts::123456789012:federated-user/Bob or 123456789012:Bob). You can
-- use the federated user's ARN in your resource-based policies, such as an
-- Amazon S3 bucket policy.
gftsFederatedUser :: Lens' GetFederationTokenResponse (Maybe FederatedUser)
gftsFederatedUser f x =
    f (_gftsFederatedUser x)
        <&> \y -> x { _gftsFederatedUser = y }
{-# INLINE gftsFederatedUser #-}

-- | A percentage value indicating the size of the policy in packed form. The
-- service rejects policies for which the packed size is greater than 100
-- percent of the allowed value.
gftsPackedPolicySize :: Lens' GetFederationTokenResponse (Maybe Integer)
gftsPackedPolicySize f x =
    f (_gftsPackedPolicySize x)
        <&> \y -> x { _gftsPackedPolicySize = y }
{-# INLINE gftsPackedPolicySize #-}

instance FromXML GetFederationTokenResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetFederationToken where
    type Sv GetFederationToken = STS
    type Rs GetFederationToken = GetFederationTokenResponse

    request = post "GetFederationToken"
    response _ = xmlResponse
