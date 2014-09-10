{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.AssumeRoleWithWebIdentity
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
-- such as Login with Amazon, Facebook, or Google. Calling
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
-- get access to content in Amazon S3. https://sts.amazonaws.com/
-- ?Action=AssumeRoleWithWebIdentity &DurationSeconds=3600
-- &ProviderId=www.amazon.com &RoleSessionName=app1 &Version=2011-06-15
-- &RoleArn=arn%3Aaws%3Aiam%3A%3A000240903217%3Arole%2FFederatedWebIdentityRole
-- &WebIdentityToken=Atza%7CIQEBLjAsAhRFiXuWpUXuRvQ9PZL3GMFcYevydwIUFAHZwXZXX
-- XXXXXXJnrulxKDHwy87oGKPznh0D6bEQZTSCzyoCtL_8S07pLpr0zMbn6w1lfVZKNTBdDansFB
-- mtGnIsIapjI6xKR02Yc_2bQ8LZbUXSGm6Ry6_BG7PrtLZtj_dfCTj92xNGed-CrKqjG7nPBjNI
-- L016GGvuS5gSvPRUxWES3VYfm1wl7WTI7jn-Pcb6M-buCgHhFOzTQxod27L9CqnOLio7N3gZAG
-- psp6n1-AJBOCJckcyXe2c6uD0srOJeZlKUm2eTDVMf8IehDVI0r1QOnTV6KzzAI3OY87Vd_cVMQ
-- amzn1.account.AF6RHO7KZU5XRVQJGXK6HB56KR2A
-- arn:aws:sts::000240903217:assumed-role/FederatedWebIdentityRole/app1
-- AROACLKWSDQRAOFQC3IDI:app1 AQoDYXdzEE0a8ANXXXXXXXXNO1ewxE5TijQyp+IPfnyowF
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY 2013-05-14T23:00:23Z
-- AKIAIOSFODNN7EXAMPLE apps.example.com
-- client.5498841531868486423.1548@apps.example.com
-- ad4156e9-bce1-11e2-82e6-6b6ef249e618.
module Network.AWS.STS
    (
    -- * Request
      AssumeRoleWithWebIdentity
    -- ** Request constructor
    , mkAssumeRoleWithWebIdentity
    -- ** Request lenses
    , arwwiRoleArn
    , arwwiRoleSessionName
    , arwwiWebIdentityToken
    , arwwiProviderId
    , arwwiPolicy
    , arwwiDurationSeconds

    -- * Response
    , AssumeRoleWithWebIdentityResponse
    -- ** Response constructor
    , mkAssumeRoleWithWebIdentityResponse
    -- ** Response lenses
    , arwwirCredentials
    , arwwirSubjectFromWebIdentityToken
    , arwwirAssumedRoleUser
    , arwwirPackedPolicySize
    , arwwirProvider
    , arwwirAudience
    ) where

import Network.AWS.Request.Query
import Network.AWS.STS.Types
import Network.AWS.Prelude

data AssumeRoleWithWebIdentity = AssumeRoleWithWebIdentity
    { _arwwiRoleArn :: !Text
    , _arwwiRoleSessionName :: !Text
    , _arwwiWebIdentityToken :: !Text
    , _arwwiProviderId :: !(Maybe Text)
    , _arwwiPolicy :: !(Maybe Text)
    , _arwwiDurationSeconds :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssumeRoleWithWebIdentity' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RoleArn ::@ @Text@
--
-- * @RoleSessionName ::@ @Text@
--
-- * @WebIdentityToken ::@ @Text@
--
-- * @ProviderId ::@ @Maybe Text@
--
-- * @Policy ::@ @Maybe Text@
--
-- * @DurationSeconds ::@ @Maybe Integer@
--
mkAssumeRoleWithWebIdentity :: Text -- ^ 'arwwiRoleArn'
                            -> Text -- ^ 'arwwiRoleSessionName'
                            -> Text -- ^ 'arwwiWebIdentityToken'
                            -> AssumeRoleWithWebIdentity
mkAssumeRoleWithWebIdentity p1 p2 p3 = AssumeRoleWithWebIdentity
    { _arwwiRoleArn = p1
    , _arwwiRoleSessionName = p2
    , _arwwiWebIdentityToken = p3
    , _arwwiProviderId = Nothing
    , _arwwiPolicy = Nothing
    , _arwwiDurationSeconds = Nothing
    }

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
arwwiRoleArn :: Lens' AssumeRoleWithWebIdentity Text
arwwiRoleArn = lens _arwwiRoleArn (\s a -> s { _arwwiRoleArn = a })

-- | An identifier for the assumed role session. Typically, you pass the name or
-- identifier that is associated with the user who is using your application.
-- That way, the temporary security credentials that your application will use
-- are associated with that user. This session name is included as part of the
-- ARN and assumed role ID in the AssumedRoleUser response element.
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

-- | The fully-qualified host component of the domain name of the identity
-- provider. Specify this value only for OAuth access tokens. Do not specify
-- this value for OpenID Connect ID tokens, such as accounts.google.com. Do
-- not include URL schemes and port numbers. Currently, www.amazon.com and
-- graph.facebook.com are supported.
arwwiProviderId :: Lens' AssumeRoleWithWebIdentity (Maybe Text)
arwwiProviderId = lens _arwwiProviderId (\s a -> s { _arwwiProviderId = a })

-- | An IAM policy in JSON format. The policy parameter is optional. If you pass
-- a policy, the temporary security credentials that are returned by the
-- operation have the permissions that are allowed by both the access policy
-- of the role that is being assumed, and the policy that you pass. This gives
-- you a way to further restrict the permissions for the resulting temporary
-- security credentials. You cannot use the passed policy to grant permissions
-- that are in excess of those allowed by the access policy of the role that
-- is being assumed. For more information, see Permissions for
-- AssumeRoleWithWebIdentity in Using Temporary Security Credentials.
arwwiPolicy :: Lens' AssumeRoleWithWebIdentity (Maybe Text)
arwwiPolicy = lens _arwwiPolicy (\s a -> s { _arwwiPolicy = a })

-- | The duration, in seconds, of the role session. The value can range from 900
-- seconds (15 minutes) to 3600 seconds (1 hour). By default, the value is set
-- to 3600 seconds.
arwwiDurationSeconds :: Lens' AssumeRoleWithWebIdentity (Maybe Integer)
arwwiDurationSeconds =
    lens _arwwiDurationSeconds (\s a -> s { _arwwiDurationSeconds = a })

instance ToQuery AssumeRoleWithWebIdentity where
    toQuery = genericQuery def

-- | Contains the result of a successful call to the AssumeRoleWithWebIdentity
-- action, including temporary AWS credentials that can be used to make AWS
-- requests.
data AssumeRoleWithWebIdentityResponse = AssumeRoleWithWebIdentityResponse
    { _arwwirCredentials :: Maybe Credentials
    , _arwwirSubjectFromWebIdentityToken :: !(Maybe Text)
    , _arwwirAssumedRoleUser :: Maybe AssumedRoleUser
    , _arwwirPackedPolicySize :: !(Maybe Integer)
    , _arwwirProvider :: !(Maybe Text)
    , _arwwirAudience :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssumeRoleWithWebIdentityResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Credentials ::@ @Maybe Credentials@
--
-- * @SubjectFromWebIdentityToken ::@ @Maybe Text@
--
-- * @AssumedRoleUser ::@ @Maybe AssumedRoleUser@
--
-- * @PackedPolicySize ::@ @Maybe Integer@
--
-- * @Provider ::@ @Maybe Text@
--
-- * @Audience ::@ @Maybe Text@
--
mkAssumeRoleWithWebIdentityResponse :: AssumeRoleWithWebIdentityResponse
mkAssumeRoleWithWebIdentityResponse = AssumeRoleWithWebIdentityResponse
    { _arwwirCredentials = Nothing
    , _arwwirSubjectFromWebIdentityToken = Nothing
    , _arwwirAssumedRoleUser = Nothing
    , _arwwirPackedPolicySize = Nothing
    , _arwwirProvider = Nothing
    , _arwwirAudience = Nothing
    }

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security token.
arwwirCredentials :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Credentials)
arwwirCredentials =
    lens _arwwirCredentials (\s a -> s { _arwwirCredentials = a })

-- | The unique user identifier that is returned by the identity provider. This
-- identifier is associated with the WebIdentityToken that was submitted with
-- the AssumeRoleWithWebIdentity call. The identifier is typically unique to
-- the user and the application that acquired the WebIdentityToken (pairwise
-- identifier). If an OpenID Connect ID token was submitted in the
-- WebIdentityToken, this value is returned by the identity provider as the
-- token's sub (Subject) claim.
arwwirSubjectFromWebIdentityToken :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirSubjectFromWebIdentityToken =
    lens _arwwirSubjectFromWebIdentityToken
         (\s a -> s { _arwwirSubjectFromWebIdentityToken = a })

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary security
-- credentials. For example, you can reference these credentials as a
-- principal in a resource-based policy by using the ARN or assumed role ID.
-- The ARN and ID include the RoleSessionName that you specified when you
-- called AssumeRole.
arwwirAssumedRoleUser :: Lens' AssumeRoleWithWebIdentityResponse (Maybe AssumedRoleUser)
arwwirAssumedRoleUser =
    lens _arwwirAssumedRoleUser (\s a -> s { _arwwirAssumedRoleUser = a })

-- | A percentage value that indicates the size of the policy in packed form.
-- The service rejects any policy with a packed size greater than 100 percent,
-- which means the policy exceeded the allowed space.
arwwirPackedPolicySize :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Integer)
arwwirPackedPolicySize =
    lens _arwwirPackedPolicySize (\s a -> s { _arwwirPackedPolicySize = a })

-- | The issuing authority of the web identity token presented. For OpenID
-- Connect ID Tokens this contains the value of the iss field. For OAuth 2.0
-- Access Tokens, this contains the value of the ProviderId parameter that was
-- passed in the AssumeRoleWithWebIdentity request.
arwwirProvider :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirProvider = lens _arwwirProvider (\s a -> s { _arwwirProvider = a })

-- | The intended audience of the web identity token. This is traditionally the
-- client identifier issued to the application that requested the web identity
-- token.
arwwirAudience :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirAudience = lens _arwwirAudience (\s a -> s { _arwwirAudience = a })

instance FromXML AssumeRoleWithWebIdentityResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AssumeRoleWithWebIdentity where
    type Sv AssumeRoleWithWebIdentity = STS
    type Rs AssumeRoleWithWebIdentity = AssumeRoleWithWebIdentityResponse

    request = post "AssumeRoleWithWebIdentity"
    response _ = xmlResponse
