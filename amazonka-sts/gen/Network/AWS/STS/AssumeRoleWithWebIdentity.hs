{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.AssumeRoleWithWebIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials for users who have been
-- authenticated in a mobile or web application with a web identity
-- provider, such as Amazon Cognito, Login with Amazon, Facebook, Google,
-- or any OpenID Connect-compatible identity provider.
--
-- For mobile applications, we recommend that you use Amazon Cognito. You
-- can use Amazon Cognito with the
-- <http://aws.amazon.com/sdkforios/ AWS SDK for iOS> and the
-- <http://aws.amazon.com/sdkforandroid/ AWS SDK for Android> to uniquely
-- identify a user and supply the user with a consistent identity
-- throughout the lifetime of an application.
--
-- To learn more about Amazon Cognito, see
-- <http://docs.aws.amazon.com/mobile/sdkforandroid/developerguide/cognito-auth.html#d0e840 Amazon Cognito Overview>
-- in the /AWS SDK for Android Developer Guide/ guide and
-- <http://docs.aws.amazon.com/mobile/sdkforios/developerguide/cognito-auth.html#d0e664 Amazon Cognito Overview>
-- in the /AWS SDK for iOS Developer Guide/.
--
-- Calling @AssumeRoleWithWebIdentity@ does not require the use of AWS
-- security credentials. Therefore, you can distribute an application (for
-- example, on mobile devices) that requests temporary security credentials
-- without including long-term AWS credentials in the application, and
-- without deploying server-based proxy services that use long-term AWS
-- credentials. Instead, the identity of the caller is validated by using a
-- token from the web identity provider.
--
-- The temporary security credentials returned by this API consist of an
-- access key ID, a secret access key, and a security token. Applications
-- can use these temporary security credentials to sign calls to AWS
-- service APIs. The credentials are valid for the duration that you
-- specified when calling @AssumeRoleWithWebIdentity@, which can be from
-- 900 seconds (15 minutes) to 3600 seconds (1 hour). By default, the
-- temporary security credentials are valid for 1 hour.
--
-- Optionally, you can pass an IAM access policy to this operation. If you
-- choose not to pass a policy, the temporary security credentials that are
-- returned by the operation have the permissions that are defined in the
-- access policy of the role that is being assumed. If you pass a policy to
-- this operation, the temporary security credentials that are returned by
-- the operation have the permissions that are allowed by both the access
-- policy of the role that is being assumed, /__and__/ the policy that you
-- pass. This gives you a way to further restrict the permissions for the
-- resulting temporary security credentials. You cannot use the passed
-- policy to grant permissions that are in excess of those allowed by the
-- access policy of the role that is being assumed. For more information,
-- see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-assume-role.html Permissions for AssumeRoleWithWebIdentity>.
--
-- Before your application can call @AssumeRoleWithWebIdentity@, you must
-- have an identity token from a supported identity provider and create a
-- role that the application can assume. The role that your application
-- assumes must trust the identity provider that is associated with the
-- identity token. In other words, the identity provider must be specified
-- in the role\'s trust policy.
--
-- For more information about how to use web identity federation and the
-- @AssumeRoleWithWebIdentity@ API, see the following resources:
--
-- -   <http://docs.aws.amazon.com/STS/latest/UsingSTS/STSUseCases.html#MobileApplication-KnownProvider Creating a Mobile Application with Third-Party Sign-In>
--     and
--     <http://docs.aws.amazon.com/STS/latest/UsingSTS/CreatingWIF.html Creating Temporary Security Credentials for Mobile Apps Using Third-Party Identity Providers>.
-- -   <https://web-identity-federation-playground.s3.amazonaws.com/index.html Web Identity Federation Playground>.
--     This interactive website lets you walk through the process of
--     authenticating via Login with Amazon, Facebook, or Google, getting
--     temporary security credentials, and then using those credentials to
--     make a request to AWS.
-- -   <http://aws.amazon.com/sdkforios/ AWS SDK for iOS> and
--     <http://aws.amazon.com/sdkforandroid/ AWS SDK for Android>. These
--     toolkits contain sample apps that show how to invoke the identity
--     providers, and then how to use the information from these providers
--     to get and use temporary security credentials.
-- -   <http://aws.amazon.com/articles/4617974389850313 Web Identity Federation with Mobile Applications>.
--     This article discusses web identity federation and shows an example
--     of how to use web identity federation to get access to content in
--     Amazon S3.
--
-- <http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html>
module Network.AWS.STS.AssumeRoleWithWebIdentity
    (
    -- * Request
      AssumeRoleWithWebIdentity
    -- ** Request constructor
    , assumeRoleWithWebIdentity
    -- ** Request lenses
    , arwwiProviderId
    , arwwiDurationSeconds
    , arwwiPolicy
    , arwwiRoleARN
    , arwwiRoleSessionName
    , arwwiWebIdentityToken

    -- * Response
    , AssumeRoleWithWebIdentityResponse
    -- ** Response constructor
    , assumeRoleWithWebIdentityResponse
    -- ** Response lenses
    , arwwirsAudience
    , arwwirsSubjectFromWebIdentityToken
    , arwwirsPackedPolicySize
    , arwwirsCredentials
    , arwwirsAssumedRoleUser
    , arwwirsProvider
    , arwwirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.STS.Types

-- | /See:/ 'assumeRoleWithWebIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arwwiProviderId'
--
-- * 'arwwiDurationSeconds'
--
-- * 'arwwiPolicy'
--
-- * 'arwwiRoleARN'
--
-- * 'arwwiRoleSessionName'
--
-- * 'arwwiWebIdentityToken'
data AssumeRoleWithWebIdentity = AssumeRoleWithWebIdentity'
    { _arwwiProviderId       :: !(Maybe Text)
    , _arwwiDurationSeconds  :: !(Maybe Nat)
    , _arwwiPolicy           :: !(Maybe Text)
    , _arwwiRoleARN          :: !Text
    , _arwwiRoleSessionName  :: !Text
    , _arwwiWebIdentityToken :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssumeRoleWithWebIdentity' smart constructor.
assumeRoleWithWebIdentity :: Text -> Text -> Text -> AssumeRoleWithWebIdentity
assumeRoleWithWebIdentity pRoleARN_ pRoleSessionName_ pWebIdentityToken_ =
    AssumeRoleWithWebIdentity'
    { _arwwiProviderId = Nothing
    , _arwwiDurationSeconds = Nothing
    , _arwwiPolicy = Nothing
    , _arwwiRoleARN = pRoleARN_
    , _arwwiRoleSessionName = pRoleSessionName_
    , _arwwiWebIdentityToken = pWebIdentityToken_
    }

-- | The fully qualified host component of the domain name of the identity
-- provider.
--
-- Specify this value only for OAuth 2.0 access tokens. Currently
-- @www.amazon.com@ and @graph.facebook.com@ are the only supported
-- identity providers for OAuth 2.0 access tokens. Do not include URL
-- schemes and port numbers.
--
-- Do not specify this value for OpenID Connect ID tokens.
arwwiProviderId :: Lens' AssumeRoleWithWebIdentity (Maybe Text)
arwwiProviderId = lens _arwwiProviderId (\ s a -> s{_arwwiProviderId = a});

-- | The duration, in seconds, of the role session. The value can range from
-- 900 seconds (15 minutes) to 3600 seconds (1 hour). By default, the value
-- is set to 3600 seconds.
arwwiDurationSeconds :: Lens' AssumeRoleWithWebIdentity (Maybe Natural)
arwwiDurationSeconds = lens _arwwiDurationSeconds (\ s a -> s{_arwwiDurationSeconds = a}) . mapping _Nat;

-- | An IAM policy in JSON format.
--
-- The policy parameter is optional. If you pass a policy, the temporary
-- security credentials that are returned by the operation have the
-- permissions that are allowed by both the access policy of the role that
-- is being assumed, /__and__/ the policy that you pass. This gives you a
-- way to further restrict the permissions for the resulting temporary
-- security credentials. You cannot use the passed policy to grant
-- permissions that are in excess of those allowed by the access policy of
-- the role that is being assumed. For more information, see
-- <http://docs.aws.amazon.com/STS/latest/UsingSTS/permissions-assume-role.html Permissions for AssumeRoleWithWebIdentity>.
--
-- The policy plain text must be 2048 bytes or shorter. However, an
-- internal conversion compresses it into a packed binary format with a
-- separate limit. The PackedPolicySize response element indicates by
-- percentage how close to the upper size limit the policy is, with 100%
-- equaling the maximum allowed size.
arwwiPolicy :: Lens' AssumeRoleWithWebIdentity (Maybe Text)
arwwiPolicy = lens _arwwiPolicy (\ s a -> s{_arwwiPolicy = a});

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
arwwiRoleARN :: Lens' AssumeRoleWithWebIdentity Text
arwwiRoleARN = lens _arwwiRoleARN (\ s a -> s{_arwwiRoleARN = a});

-- | An identifier for the assumed role session. Typically, you pass the name
-- or identifier that is associated with the user who is using your
-- application. That way, the temporary security credentials that your
-- application will use are associated with that user. This session name is
-- included as part of the ARN and assumed role ID in the @AssumedRoleUser@
-- response element.
arwwiRoleSessionName :: Lens' AssumeRoleWithWebIdentity Text
arwwiRoleSessionName = lens _arwwiRoleSessionName (\ s a -> s{_arwwiRoleSessionName = a});

-- | The OAuth 2.0 access token or OpenID Connect ID token that is provided
-- by the identity provider. Your application must get this token by
-- authenticating the user who is using your application with a web
-- identity provider before the application makes an
-- @AssumeRoleWithWebIdentity@ call.
arwwiWebIdentityToken :: Lens' AssumeRoleWithWebIdentity Text
arwwiWebIdentityToken = lens _arwwiWebIdentityToken (\ s a -> s{_arwwiWebIdentityToken = a});

instance AWSRequest AssumeRoleWithWebIdentity where
        type Sv AssumeRoleWithWebIdentity = STS
        type Rs AssumeRoleWithWebIdentity =
             AssumeRoleWithWebIdentityResponse
        request = post "AssumeRoleWithWebIdentity"
        response
          = receiveXMLWrapper "AssumeRoleWithWebIdentityResult"
              (\ s h x ->
                 AssumeRoleWithWebIdentityResponse' <$>
                   (x .@? "Audience") <*>
                     (x .@? "SubjectFromWebIdentityToken")
                     <*> (x .@? "PackedPolicySize")
                     <*> (x .@? "Credentials")
                     <*> (x .@? "AssumedRoleUser")
                     <*> (x .@? "Provider")
                     <*> (pure (fromEnum s)))

instance ToHeaders AssumeRoleWithWebIdentity where
        toHeaders = const mempty

instance ToPath AssumeRoleWithWebIdentity where
        toPath = const "/"

instance ToQuery AssumeRoleWithWebIdentity where
        toQuery AssumeRoleWithWebIdentity'{..}
          = mconcat
              ["Action" =:
                 ("AssumeRoleWithWebIdentity" :: ByteString),
               "Version" =: ("2011-06-15" :: ByteString),
               "ProviderId" =: _arwwiProviderId,
               "DurationSeconds" =: _arwwiDurationSeconds,
               "Policy" =: _arwwiPolicy, "RoleArn" =: _arwwiRoleARN,
               "RoleSessionName" =: _arwwiRoleSessionName,
               "WebIdentityToken" =: _arwwiWebIdentityToken]

-- | Contains the response to a successful AssumeRoleWithWebIdentity request,
-- including temporary AWS credentials that can be used to make AWS
-- requests.
--
-- /See:/ 'assumeRoleWithWebIdentityResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'arwwirsAudience'
--
-- * 'arwwirsSubjectFromWebIdentityToken'
--
-- * 'arwwirsPackedPolicySize'
--
-- * 'arwwirsCredentials'
--
-- * 'arwwirsAssumedRoleUser'
--
-- * 'arwwirsProvider'
--
-- * 'arwwirsStatus'
data AssumeRoleWithWebIdentityResponse = AssumeRoleWithWebIdentityResponse'
    { _arwwirsAudience                    :: !(Maybe Text)
    , _arwwirsSubjectFromWebIdentityToken :: !(Maybe Text)
    , _arwwirsPackedPolicySize            :: !(Maybe Nat)
    , _arwwirsCredentials                 :: !(Maybe Credentials)
    , _arwwirsAssumedRoleUser             :: !(Maybe AssumedRoleUser)
    , _arwwirsProvider                    :: !(Maybe Text)
    , _arwwirsStatus                      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssumeRoleWithWebIdentityResponse' smart constructor.
assumeRoleWithWebIdentityResponse :: Int -> AssumeRoleWithWebIdentityResponse
assumeRoleWithWebIdentityResponse pStatus_ =
    AssumeRoleWithWebIdentityResponse'
    { _arwwirsAudience = Nothing
    , _arwwirsSubjectFromWebIdentityToken = Nothing
    , _arwwirsPackedPolicySize = Nothing
    , _arwwirsCredentials = Nothing
    , _arwwirsAssumedRoleUser = Nothing
    , _arwwirsProvider = Nothing
    , _arwwirsStatus = pStatus_
    }

-- | The intended audience (also known as client ID) of the web identity
-- token. This is traditionally the client identifier issued to the
-- application that requested the web identity token.
arwwirsAudience :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirsAudience = lens _arwwirsAudience (\ s a -> s{_arwwirsAudience = a});

-- | The unique user identifier that is returned by the identity provider.
-- This identifier is associated with the @WebIdentityToken@ that was
-- submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is
-- typically unique to the user and the application that acquired the
-- @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens,
-- this field contains the value returned by the identity provider as the
-- token\'s @sub@ (Subject) claim.
arwwirsSubjectFromWebIdentityToken :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirsSubjectFromWebIdentityToken = lens _arwwirsSubjectFromWebIdentityToken (\ s a -> s{_arwwirsSubjectFromWebIdentityToken = a});

-- | A percentage value that indicates the size of the policy in packed form.
-- The service rejects any policy with a packed size greater than 100
-- percent, which means the policy exceeded the allowed space.
arwwirsPackedPolicySize :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Natural)
arwwirsPackedPolicySize = lens _arwwirsPackedPolicySize (\ s a -> s{_arwwirsPackedPolicySize = a}) . mapping _Nat;

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security token.
arwwirsCredentials :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Credentials)
arwwirsCredentials = lens _arwwirsCredentials (\ s a -> s{_arwwirsCredentials = a});

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary
-- security credentials. For example, you can reference these credentials
-- as a principal in a resource-based policy by using the ARN or assumed
-- role ID. The ARN and ID include the @RoleSessionName@ that you specified
-- when you called @AssumeRole@.
arwwirsAssumedRoleUser :: Lens' AssumeRoleWithWebIdentityResponse (Maybe AssumedRoleUser)
arwwirsAssumedRoleUser = lens _arwwirsAssumedRoleUser (\ s a -> s{_arwwirsAssumedRoleUser = a});

-- | The issuing authority of the web identity token presented. For OpenID
-- Connect ID Tokens this contains the value of the @iss@ field. For OAuth
-- 2.0 access tokens, this contains the value of the @ProviderId@ parameter
-- that was passed in the @AssumeRoleWithWebIdentity@ request.
arwwirsProvider :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirsProvider = lens _arwwirsProvider (\ s a -> s{_arwwirsProvider = a});

-- | FIXME: Undocumented member.
arwwirsStatus :: Lens' AssumeRoleWithWebIdentityResponse Int
arwwirsStatus = lens _arwwirsStatus (\ s a -> s{_arwwirsStatus = a});
