{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SecurityToken.AssumeRoleWithWebIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a set of temporary security credentials for users who have been
-- authenticated in a mobile or web application with a web identity provider,
-- such as Login with Amazon, Amazon Cognito, Facebook, or Google. Calling
-- AssumeRoleWithWebIdentity does not require the use of AWS security
-- credentials. Therefore, you can distribute an application (for example, on
-- mobile devices) that requests temporary security credentials without
-- including long-term AWS credentials in the application, and without
-- deploying server-based proxy services that use long-term AWS credentials.
-- Instead, the identity of the caller is validated by using a token from the
-- web identity provider. The temporary security credentials returned by this
-- API consist of an access key ID, a secret access key, and a security token.
-- Applications can use these temporary security credentials to sign calls to
-- AWS service APIs. The credentials are valid for the duration that you
-- specified when calling AssumeRoleWithWebIdentity, which can be from 900
-- seconds (15 minutes) to 3600 seconds (1 hour). By default, the temporary
-- security credentials are valid for 1 hour. Optionally, you can pass an IAM
-- access policy to this operation. If you choose not to pass a policy, the
-- temporary security credentials that are returned by the operation have the
-- permissions that are defined in the access policy of the role that is being
-- assumed. If you pass a policy to this operation, the temporary security
-- credentials that are returned by the operation have the permissions that
-- are allowed by both the access policy of the role that is being assumed,
-- and the policy that you pass. This gives you a way to further restrict the
-- permissions for the resulting temporary security credentials. You cannot
-- use the passed policy to grant permissions that are in excess of those
-- allowed by the access policy of the role that is being assumed. For more
-- information, see Permissions for AssumeRoleWithWebIdentity in Using
-- Temporary Security Credentials. Before your application can call
-- AssumeRoleWithWebIdentity, you must have an identity token from a supported
-- identity provider and create a role that the application can assume. The
-- role that your application assumes must trust the identity provider that is
-- associated with the identity token. In other words, the identity provider
-- must be specified in the role's trust policy. For more information about
-- how to use web identity federation and the AssumeRoleWithWebIdentity, see
-- the following resources: Creating a Mobile Application with Third-Party
-- Sign-In and Creating Temporary Security Credentials for Mobile Apps Using
-- Third-Party Identity Providers in Using Temporary Security Credentials. Web
-- Identity Federation Playground. This interactive website lets you walk
-- through the process of authenticating via Login with Amazon, Facebook, or
-- Google, getting temporary security credentials, and then using those
-- credentials to make a request to AWS. AWS SDK for iOS and AWS SDK for
-- Android. These toolkits contain sample apps that show how to invoke the
-- identity providers, and then how to use the information from these
-- providers to get and use temporary security credentials. Web Identity
-- Federation with Mobile Applications. This article discusses web identity
-- federation and shows an example of how to use web identity federation to
-- get access to content in Amazon S3.
module Network.AWS.SecurityToken.AssumeRoleWithWebIdentity
    (
    -- * Request
      AssumeRoleWithWebIdentity
    -- ** Request constructor
    , assumeRoleWithWebIdentity
    -- ** Request lenses
    , arwwiDurationSeconds
    , arwwiPolicy
    , arwwiProviderId
    , arwwiRoleArn
    , arwwiRoleSessionName
    , arwwiWebIdentityToken

    -- * Response
    , AssumeRoleWithWebIdentityResponse
    -- ** Response constructor
    , assumeRoleWithWebIdentityResponse
    -- ** Response lenses
    , arwwirAssumedRoleUser
    , arwwirAudience
    , arwwirCredentials
    , arwwirPackedPolicySize
    , arwwirProvider
    , arwwirSubjectFromWebIdentityToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SecurityToken.Types
import qualified GHC.Exts

data AssumeRoleWithWebIdentity = AssumeRoleWithWebIdentity
    { _arwwiDurationSeconds  :: Maybe Natural
    , _arwwiPolicy           :: Maybe Text
    , _arwwiProviderId       :: Maybe Text
    , _arwwiRoleArn          :: Text
    , _arwwiRoleSessionName  :: Text
    , _arwwiWebIdentityToken :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AssumeRoleWithWebIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arwwiDurationSeconds' @::@ 'Maybe' 'Natural'
--
-- * 'arwwiPolicy' @::@ 'Maybe' 'Text'
--
-- * 'arwwiProviderId' @::@ 'Maybe' 'Text'
--
-- * 'arwwiRoleArn' @::@ 'Text'
--
-- * 'arwwiRoleSessionName' @::@ 'Text'
--
-- * 'arwwiWebIdentityToken' @::@ 'Text'
--
assumeRoleWithWebIdentity :: Text -- ^ 'arwwiRoleArn'
                          -> Text -- ^ 'arwwiRoleSessionName'
                          -> Text -- ^ 'arwwiWebIdentityToken'
                          -> AssumeRoleWithWebIdentity
assumeRoleWithWebIdentity p1 p2 p3 = AssumeRoleWithWebIdentity
    { _arwwiRoleArn          = p1
    , _arwwiRoleSessionName  = p2
    , _arwwiWebIdentityToken = p3
    , _arwwiProviderId       = Nothing
    , _arwwiPolicy           = Nothing
    , _arwwiDurationSeconds  = Nothing
    }

-- | The duration, in seconds, of the role session. The value can range from
-- 900 seconds (15 minutes) to 3600 seconds (1 hour). By default, the value
-- is set to 3600 seconds.
arwwiDurationSeconds :: Lens' AssumeRoleWithWebIdentity (Maybe Natural)
arwwiDurationSeconds =
    lens _arwwiDurationSeconds (\s a -> s { _arwwiDurationSeconds = a })

-- | An IAM policy in JSON format. The policy parameter is optional. If you
-- pass a policy, the temporary security credentials that are returned by
-- the operation have the permissions that are allowed by both the access
-- policy of the role that is being assumed, and the policy that you pass.
-- This gives you a way to further restrict the permissions for the
-- resulting temporary security credentials. You cannot use the passed
-- policy to grant permissions that are in excess of those allowed by the
-- access policy of the role that is being assumed. For more information,
-- see Permissions for AssumeRoleWithWebIdentity in Using Temporary Security
-- Credentials.
arwwiPolicy :: Lens' AssumeRoleWithWebIdentity (Maybe Text)
arwwiPolicy = lens _arwwiPolicy (\s a -> s { _arwwiPolicy = a })

-- | The fully-qualified host component of the domain name of the identity
-- provider. Specify this value only for OAuth access tokens. Do not specify
-- this value for OpenID Connect ID tokens, such as accounts.google.com. Do
-- not include URL schemes and port numbers. Currently, www.amazon.com and
-- graph.facebook.com are supported.
arwwiProviderId :: Lens' AssumeRoleWithWebIdentity (Maybe Text)
arwwiProviderId = lens _arwwiProviderId (\s a -> s { _arwwiProviderId = a })

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
arwwiRoleArn :: Lens' AssumeRoleWithWebIdentity Text
arwwiRoleArn = lens _arwwiRoleArn (\s a -> s { _arwwiRoleArn = a })

-- | An identifier for the assumed role session. Typically, you pass the name
-- or identifier that is associated with the user who is using your
-- application. That way, the temporary security credentials that your
-- application will use are associated with that user. This session name is
-- included as part of the ARN and assumed role ID in the AssumedRoleUser
-- response element.
arwwiRoleSessionName :: Lens' AssumeRoleWithWebIdentity Text
arwwiRoleSessionName =
    lens _arwwiRoleSessionName (\s a -> s { _arwwiRoleSessionName = a })

-- | The OAuth 2.0 access token or OpenID Connect ID token that is provided by
-- the identity provider. Your application must get this token by
-- authenticating the user who is using your application with a web identity
-- provider before the application makes an AssumeRoleWithWebIdentity call.
arwwiWebIdentityToken :: Lens' AssumeRoleWithWebIdentity Text
arwwiWebIdentityToken =
    lens _arwwiWebIdentityToken (\s a -> s { _arwwiWebIdentityToken = a })

instance ToQuery AssumeRoleWithWebIdentity

instance ToPath AssumeRoleWithWebIdentity where
    toPath = const "/"

data AssumeRoleWithWebIdentityResponse = AssumeRoleWithWebIdentityResponse
    { _arwwirAssumedRoleUser             :: Maybe AssumedRoleUser
    , _arwwirAudience                    :: Maybe Text
    , _arwwirCredentials                 :: Maybe Credentials
    , _arwwirPackedPolicySize            :: Maybe Natural
    , _arwwirProvider                    :: Maybe Text
    , _arwwirSubjectFromWebIdentityToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'AssumeRoleWithWebIdentityResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arwwirAssumedRoleUser' @::@ 'Maybe' 'AssumedRoleUser'
--
-- * 'arwwirAudience' @::@ 'Maybe' 'Text'
--
-- * 'arwwirCredentials' @::@ 'Maybe' 'Credentials'
--
-- * 'arwwirPackedPolicySize' @::@ 'Maybe' 'Natural'
--
-- * 'arwwirProvider' @::@ 'Maybe' 'Text'
--
-- * 'arwwirSubjectFromWebIdentityToken' @::@ 'Maybe' 'Text'
--
assumeRoleWithWebIdentityResponse :: AssumeRoleWithWebIdentityResponse
assumeRoleWithWebIdentityResponse = AssumeRoleWithWebIdentityResponse
    { _arwwirCredentials                 = Nothing
    , _arwwirSubjectFromWebIdentityToken = Nothing
    , _arwwirAssumedRoleUser             = Nothing
    , _arwwirPackedPolicySize            = Nothing
    , _arwwirProvider                    = Nothing
    , _arwwirAudience                    = Nothing
    }

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary security
-- credentials. For example, you can reference these credentials as a
-- principal in a resource-based policy by using the ARN or assumed role ID.
-- The ARN and ID include the RoleSessionName that you specified when you
-- called AssumeRole.
arwwirAssumedRoleUser :: Lens' AssumeRoleWithWebIdentityResponse (Maybe AssumedRoleUser)
arwwirAssumedRoleUser =
    lens _arwwirAssumedRoleUser (\s a -> s { _arwwirAssumedRoleUser = a })

-- | The intended audience of the web identity token. This is traditionally
-- the client identifier issued to the application that requested the web
-- identity token.
arwwirAudience :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirAudience = lens _arwwirAudience (\s a -> s { _arwwirAudience = a })

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security token.
arwwirCredentials :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Credentials)
arwwirCredentials =
    lens _arwwirCredentials (\s a -> s { _arwwirCredentials = a })

-- | A percentage value that indicates the size of the policy in packed form.
-- The service rejects any policy with a packed size greater than 100
-- percent, which means the policy exceeded the allowed space.
arwwirPackedPolicySize :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Natural)
arwwirPackedPolicySize =
    lens _arwwirPackedPolicySize (\s a -> s { _arwwirPackedPolicySize = a })

-- | The issuing authority of the web identity token presented. For OpenID
-- Connect ID Tokens this contains the value of the iss field. For OAuth 2.0
-- Access Tokens, this contains the value of the ProviderId parameter that
-- was passed in the AssumeRoleWithWebIdentity request.
arwwirProvider :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirProvider = lens _arwwirProvider (\s a -> s { _arwwirProvider = a })

-- | The unique user identifier that is returned by the identity provider.
-- This identifier is associated with the WebIdentityToken that was
-- submitted with the AssumeRoleWithWebIdentity call. The identifier is
-- typically unique to the user and the application that acquired the
-- WebIdentityToken (pairwise identifier). If an OpenID Connect ID token was
-- submitted in the WebIdentityToken, this value is returned by the identity
-- provider as the token's sub (Subject) claim.
arwwirSubjectFromWebIdentityToken :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirSubjectFromWebIdentityToken =
    lens _arwwirSubjectFromWebIdentityToken
        (\s a -> s { _arwwirSubjectFromWebIdentityToken = a })

instance AWSRequest AssumeRoleWithWebIdentity where
    type Sv AssumeRoleWithWebIdentity = SecurityToken
    type Rs AssumeRoleWithWebIdentity = AssumeRoleWithWebIdentityResponse

    request  = post "AssumeRoleWithWebIdentity"
    response = xmlResponse $ \h x -> AssumeRoleWithWebIdentityResponse
        <$> x %| "AssumedRoleUser"
        <*> x %| "Audience"
        <*> x %| "Credentials"
        <*> x %| "PackedPolicySize"
        <*> x %| "Provider"
        <*> x %| "SubjectFromWebIdentityToken"
