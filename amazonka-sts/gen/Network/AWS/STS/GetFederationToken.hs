{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.GetFederationToken
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials (consisting of an access
-- key ID, a secret access key, and a security token) for a federated user.
-- A typical use is in a proxy application that gets temporary security
-- credentials on behalf of distributed applications inside a corporate
-- network. Because you must call the @GetFederationToken@ action using the
-- long-term security credentials of an IAM user, this call is appropriate
-- in contexts where those credentials can be safely stored, usually in a
-- server-based application.
--
-- If you are creating a mobile-based or browser-based app that can
-- authenticate users using a web identity provider like Login with Amazon,
-- Facebook, Google, or an OpenID Connect-compatible identity provider, we
-- recommend that you use <http://aws.amazon.com/cognito/ Amazon Cognito>
-- or @AssumeRoleWithWebIdentity@. For more information, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/CreatingWIF.html Creating Temporary Security Credentials for Mobile Apps Using Identity Providers>.
--
-- The @GetFederationToken@ action must be called by using the long-term
-- AWS security credentials of an IAM user. You can also call
-- @GetFederationToken@ using the security credentials of an AWS account
-- (root), but this is not recommended. Instead, we recommend that you
-- create an IAM user for the purpose of the proxy application and then
-- attach a policy to the IAM user that limits federated users to only the
-- actions and resources they need access to. For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAMBestPractices.html IAM Best Practices>
-- in /Using IAM/.
--
-- The temporary security credentials that are obtained by using the
-- long-term credentials of an IAM user are valid for the specified
-- duration, between 900 seconds (15 minutes) and 129600 seconds (36
-- hours). Temporary credentials that are obtained by using AWS account
-- (root) credentials have a maximum duration of 3600 seconds (1 hour)
--
-- __Permissions__
--
-- The permissions for the temporary security credentials returned by
-- @GetFederationToken@ are determined by a combination of the following:
--
-- -   The policy or policies that are attached to the IAM user whose
--     credentials are used to call @GetFederationToken@.
-- -   The policy that is passed as a parameter in the call.
--
-- The passed policy is attached to the temporary security credentials that
-- result from the @GetFederationToken@ API call--that is, to the
-- /federated user/. When the federated user makes an AWS request, AWS
-- evaluates the policy attached to the federated user in combination with
-- the policy or policies attached to the IAM user whose credentials were
-- used to call @GetFederationToken@. AWS allows the federated user\'s
-- request only when both the federated user /__and__/ the IAM user are
-- explicitly allowed to perform the requested action. The passed policy
-- cannot grant more permissions than those that are defined in the IAM
-- user policy.
--
-- A typical use case is that the permissions of the IAM user whose
-- credentials are used to call @GetFederationToken@ are designed to allow
-- access to all the actions and resources that any federated user will
-- need. Then, for individual users, you pass a policy to the operation
-- that scopes down the permissions to a level that\'s appropriate to that
-- individual user, using a policy that allows only a subset of permissions
-- that are granted to the IAM user.
--
-- If you do not pass a policy, the resulting temporary security
-- credentials have no effective permissions. The only exception is when
-- the temporary security credentials are used to access a resource that
-- has a resource-based policy that specifically allows the federated user
-- to access the resource.
--
-- For more information about how permissions work, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-get-federation-token.html Permissions for GetFederationToken>.
-- For information about using @GetFederationToken@ to create temporary
-- security credentials, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/CreatingFedTokens.html Creating Temporary Credentials to Enable Access for Federated Users>.
--
-- <http://docs.aws.amazon.com/STS/latest/APIReference/API_GetFederationToken.html>
module Network.AWS.STS.GetFederationToken
    (
    -- * Request
      GetFederationToken
    -- ** Request constructor
    , getFederationToken
    -- ** Request lenses
    , gftDurationSeconds
    , gftPolicy
    , gftName

    -- * Response
    , GetFederationTokenResponse
    -- ** Response constructor
    , getFederationTokenResponse
    -- ** Response lenses
    , gftrPackedPolicySize
    , gftrCredentials
    , gftrFederatedUser
    , gftrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.STS.Types

-- | /See:/ 'getFederationToken' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gftDurationSeconds'
--
-- * 'gftPolicy'
--
-- * 'gftName'
data GetFederationToken = GetFederationToken'
    { _gftDurationSeconds :: !(Maybe Nat)
    , _gftPolicy          :: !(Maybe Text)
    , _gftName            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetFederationToken' smart constructor.
getFederationToken :: Text -> GetFederationToken
getFederationToken pName =
    GetFederationToken'
    { _gftDurationSeconds = Nothing
    , _gftPolicy = Nothing
    , _gftName = pName
    }

-- | The duration, in seconds, that the session should last. Acceptable
-- durations for federation sessions range from 900 seconds (15 minutes) to
-- 129600 seconds (36 hours), with 43200 seconds (12 hours) as the default.
-- Sessions obtained using AWS account (root) credentials are restricted to
-- a maximum of 3600 seconds (one hour). If the specified duration is
-- longer than one hour, the session obtained by using AWS account (root)
-- credentials defaults to one hour.
gftDurationSeconds :: Lens' GetFederationToken (Maybe Natural)
gftDurationSeconds = lens _gftDurationSeconds (\ s a -> s{_gftDurationSeconds = a}) . mapping _Nat;

-- | An IAM policy in JSON format that is passed with the
-- @GetFederationToken@ call and evaluated along with the policy or
-- policies that are attached to the IAM user whose credentials are used to
-- call @GetFederationToken@. The passed policy is used to scope down the
-- permissions that are available to the IAM user, by allowing only a
-- subset of the permissions that are granted to the IAM user. The passed
-- policy cannot grant more permissions than those granted to the IAM user.
-- The final permissions for the federated user are the most restrictive
-- set based on the intersection of the passed policy and the IAM user
-- policy.
--
-- If you do not pass a policy, the resulting temporary security
-- credentials have no effective permissions. The only exception is when
-- the temporary security credentials are used to access a resource that
-- has a resource-based policy that specifically allows the federated user
-- to access the resource.
--
-- The policy plain text must be 2048 bytes or shorter. However, an
-- internal conversion compresses it into a packed binary format with a
-- separate limit. The PackedPolicySize response element indicates by
-- percentage how close to the upper size limit the policy is, with 100%
-- equaling the maximum allowed size.
--
-- For more information about how permissions work, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-get-federation-token.html Permissions for GetFederationToken>.
gftPolicy :: Lens' GetFederationToken (Maybe Text)
gftPolicy = lens _gftPolicy (\ s a -> s{_gftPolicy = a});

-- | The name of the federated user. The name is used as an identifier for
-- the temporary security credentials (such as @Bob@). For example, you can
-- reference the federated user name in a resource-based policy, such as in
-- an Amazon S3 bucket policy.
gftName :: Lens' GetFederationToken Text
gftName = lens _gftName (\ s a -> s{_gftName = a});

instance AWSRequest GetFederationToken where
        type Sv GetFederationToken = STS
        type Rs GetFederationToken =
             GetFederationTokenResponse
        request = post
        response
          = receiveXMLWrapper "GetFederationTokenResult"
              (\ s h x ->
                 GetFederationTokenResponse' <$>
                   (x .@? "PackedPolicySize") <*> (x .@? "Credentials")
                     <*> (x .@? "FederatedUser")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetFederationToken where
        toHeaders = const mempty

instance ToPath GetFederationToken where
        toPath = const "/"

instance ToQuery GetFederationToken where
        toQuery GetFederationToken'{..}
          = mconcat
              ["Action" =: ("GetFederationToken" :: ByteString),
               "Version" =: ("2011-06-15" :: ByteString),
               "DurationSeconds" =: _gftDurationSeconds,
               "Policy" =: _gftPolicy, "Name" =: _gftName]

-- | Contains the response to a successful GetFederationToken request,
-- including temporary AWS credentials that can be used to make AWS
-- requests.
--
-- /See:/ 'getFederationTokenResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gftrPackedPolicySize'
--
-- * 'gftrCredentials'
--
-- * 'gftrFederatedUser'
--
-- * 'gftrStatus'
data GetFederationTokenResponse = GetFederationTokenResponse'
    { _gftrPackedPolicySize :: !(Maybe Nat)
    , _gftrCredentials      :: !(Maybe Credentials)
    , _gftrFederatedUser    :: !(Maybe FederatedUser)
    , _gftrStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetFederationTokenResponse' smart constructor.
getFederationTokenResponse :: Int -> GetFederationTokenResponse
getFederationTokenResponse pStatus =
    GetFederationTokenResponse'
    { _gftrPackedPolicySize = Nothing
    , _gftrCredentials = Nothing
    , _gftrFederatedUser = Nothing
    , _gftrStatus = pStatus
    }

-- | A percentage value indicating the size of the policy in packed form. The
-- service rejects policies for which the packed size is greater than 100
-- percent of the allowed value.
gftrPackedPolicySize :: Lens' GetFederationTokenResponse (Maybe Natural)
gftrPackedPolicySize = lens _gftrPackedPolicySize (\ s a -> s{_gftrPackedPolicySize = a}) . mapping _Nat;

-- | Credentials for the service API authentication.
gftrCredentials :: Lens' GetFederationTokenResponse (Maybe Credentials)
gftrCredentials = lens _gftrCredentials (\ s a -> s{_gftrCredentials = a});

-- | Identifiers for the federated user associated with the credentials (such
-- as @arn:aws:sts::123456789012:federated-user\/Bob@ or
-- @123456789012:Bob@). You can use the federated user\'s ARN in your
-- resource-based policies, such as an Amazon S3 bucket policy.
gftrFederatedUser :: Lens' GetFederationTokenResponse (Maybe FederatedUser)
gftrFederatedUser = lens _gftrFederatedUser (\ s a -> s{_gftrFederatedUser = a});

-- | FIXME: Undocumented member.
gftrStatus :: Lens' GetFederationTokenResponse Int
gftrStatus = lens _gftrStatus (\ s a -> s{_gftrStatus = a});
