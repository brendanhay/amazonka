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
-- Module      : Network.AWS.STS.AssumeRoleWithWebIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials for users who have been authenticated in a mobile or web application with a web identity provider, such as Amazon Cognito, Login with Amazon, Facebook, Google, or any OpenID Connect-compatible identity provider.
--
--
-- Calling @AssumeRoleWithWebIdentity@ does not require the use of AWS security credentials. Therefore, you can distribute an application (for example, on mobile devices) that requests temporary security credentials without including long-term AWS credentials in the application, and without deploying server-based proxy services that use long-term AWS credentials. Instead, the identity of the caller is validated by using a token from the web identity provider. For a comparison of @AssumeRoleWithWebIdentity@ with the other APIs that produce temporary credentials, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials> and <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS APIs> in the /IAM User Guide/ .
--
-- The temporary security credentials returned by this API consist of an access key ID, a secret access key, and a security token. Applications can use these temporary security credentials to sign calls to AWS service APIs.
--
-- By default, the temporary security credentials created by @AssumeRoleWithWebIdentity@ last for one hour. However, you can use the optional @DurationSeconds@ parameter to specify the duration of your session. You can provide a value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. To learn how to view the maximum value for your role, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . The maximum session duration limit applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
--
-- The temporary security credentials created by @AssumeRoleWithWebIdentity@ can be used to make API calls to any AWS service with the following exception: you cannot call the STS service's @GetFederationToken@ or @GetSessionToken@ APIs.
--
-- Optionally, you can pass an IAM access policy to this operation. If you choose not to pass a policy, the temporary security credentials that are returned by the operation have the permissions that are defined in the access policy of the role that is being assumed. If you pass a policy to this operation, the temporary security credentials that are returned by the operation have the permissions that are allowed by both the access policy of the role that is being assumed, /__and__ / the policy that you pass. This gives you a way to further restrict the permissions for the resulting temporary security credentials. You cannot use the passed policy to grant permissions that are in excess of those allowed by the access policy of the role that is being assumed. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_assumerole.html Permissions for AssumeRole, AssumeRoleWithSAML, and AssumeRoleWithWebIdentity> in the /IAM User Guide/ .
--
-- Before your application can call @AssumeRoleWithWebIdentity@ , you must have an identity token from a supported identity provider and create a role that the application can assume. The role that your application assumes must trust the identity provider that is associated with the identity token. In other words, the identity provider must be specified in the role's trust policy.
--
-- /Important:/ Calling @AssumeRoleWithWebIdentity@ can result in an entry in your AWS CloudTrail logs. The entry includes the <http://openid.net/specs/openid-connect-core-1_0.html#Claims Subject> of the provided Web Identity Token. We recommend that you avoid using any personally identifiable information (PII) in this field. For example, you could instead use a GUID or a pairwise identifier, as <http://openid.net/specs/openid-connect-core-1_0.html#SubjectIDTypes suggested in the OIDC specification> .
--
-- For more information about how to use web identity federation and the @AssumeRoleWithWebIdentity@ API, see the following resources:
--
--     * <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_oidc_manual.html Using Web Identity Federation APIs for Mobile Apps> and <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_assumerolewithwebidentity Federation Through a Web-based Identity Provider> .
--
--     * <https://web-identity-federation-playground.s3.amazonaws.com/index.html Web Identity Federation Playground> . This interactive website lets you walk through the process of authenticating via Login with Amazon, Facebook, or Google, getting temporary security credentials, and then using those credentials to make a request to AWS.
--
--     * <http://aws.amazon.com/sdkforios/ AWS SDK for iOS> and <http://aws.amazon.com/sdkforandroid/ AWS SDK for Android> . These toolkits contain sample apps that show how to invoke the identity providers, and then how to use the information from these providers to get and use temporary security credentials.
--
--     * <http://aws.amazon.com/articles/web-identity-federation-with-mobile-applications Web Identity Federation with Mobile Applications> . This article discusses web identity federation and shows an example of how to use web identity federation to get access to content in Amazon S3.
--
--
--
module Network.AWS.STS.AssumeRoleWithWebIdentity
    (
    -- * Creating a Request
      assumeRoleWithWebIdentity
    , AssumeRoleWithWebIdentity
    -- * Request Lenses
    , arwwiProviderId
    , arwwiDurationSeconds
    , arwwiPolicy
    , arwwiRoleARN
    , arwwiRoleSessionName
    , arwwiWebIdentityToken

    -- * Destructuring the Response
    , assumeRoleWithWebIdentityResponse
    , AssumeRoleWithWebIdentityResponse
    -- * Response Lenses
    , arwwirsAudience
    , arwwirsSubjectFromWebIdentityToken
    , arwwirsPackedPolicySize
    , arwwirsCredentials
    , arwwirsAssumedRoleUser
    , arwwirsProvider
    , arwwirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.STS.Types
import Network.AWS.STS.Types.Product

-- | /See:/ 'assumeRoleWithWebIdentity' smart constructor.
data AssumeRoleWithWebIdentity = AssumeRoleWithWebIdentity'
  { _arwwiProviderId       :: !(Maybe Text)
  , _arwwiDurationSeconds  :: !(Maybe Nat)
  , _arwwiPolicy           :: !(Maybe Text)
  , _arwwiRoleARN          :: !Text
  , _arwwiRoleSessionName  :: !Text
  , _arwwiWebIdentityToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssumeRoleWithWebIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arwwiProviderId' - The fully qualified host component of the domain name of the identity provider. Specify this value only for OAuth 2.0 access tokens. Currently @www.amazon.com@ and @graph.facebook.com@ are the only supported identity providers for OAuth 2.0 access tokens. Do not include URL schemes and port numbers. Do not specify this value for OpenID Connect ID tokens.
--
-- * 'arwwiDurationSeconds' - The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . By default, the value is set to 3600 seconds.
--
-- * 'arwwiPolicy' - An IAM policy in JSON format. The policy parameter is optional. If you pass a policy, the temporary security credentials that are returned by the operation have the permissions that are allowed by both the access policy of the role that is being assumed, /__and__ / the policy that you pass. This gives you a way to further restrict the permissions for the resulting temporary security credentials. You cannot use the passed policy to grant permissions that are in excess of those allowed by the access policy of the role that is being assumed. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_assumerole.html Permissions for AssumeRoleWithWebIdentity> in the /IAM User Guide/ .  The format for this parameter, as described by its regex pattern, is a string of characters up to 2048 characters in length. The characters can be any ASCII character from the space character to the end of the valid character list (\u0020-\u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- * 'arwwiRoleARN' - The Amazon Resource Name (ARN) of the role that the caller is assuming.
--
-- * 'arwwiRoleSessionName' - An identifier for the assumed role session. Typically, you pass the name or identifier that is associated with the user who is using your application. That way, the temporary security credentials that your application will use are associated with that user. This session name is included as part of the ARN and assumed role ID in the @AssumedRoleUser@ response element. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
--
-- * 'arwwiWebIdentityToken' - The OAuth 2.0 access token or OpenID Connect ID token that is provided by the identity provider. Your application must get this token by authenticating the user who is using your application with a web identity provider before the application makes an @AssumeRoleWithWebIdentity@ call.
assumeRoleWithWebIdentity
    :: Text -- ^ 'arwwiRoleARN'
    -> Text -- ^ 'arwwiRoleSessionName'
    -> Text -- ^ 'arwwiWebIdentityToken'
    -> AssumeRoleWithWebIdentity
assumeRoleWithWebIdentity pRoleARN_ pRoleSessionName_ pWebIdentityToken_ =
  AssumeRoleWithWebIdentity'
    { _arwwiProviderId = Nothing
    , _arwwiDurationSeconds = Nothing
    , _arwwiPolicy = Nothing
    , _arwwiRoleARN = pRoleARN_
    , _arwwiRoleSessionName = pRoleSessionName_
    , _arwwiWebIdentityToken = pWebIdentityToken_
    }


-- | The fully qualified host component of the domain name of the identity provider. Specify this value only for OAuth 2.0 access tokens. Currently @www.amazon.com@ and @graph.facebook.com@ are the only supported identity providers for OAuth 2.0 access tokens. Do not include URL schemes and port numbers. Do not specify this value for OpenID Connect ID tokens.
arwwiProviderId :: Lens' AssumeRoleWithWebIdentity (Maybe Text)
arwwiProviderId = lens _arwwiProviderId (\ s a -> s{_arwwiProviderId = a})

-- | The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . By default, the value is set to 3600 seconds.
arwwiDurationSeconds :: Lens' AssumeRoleWithWebIdentity (Maybe Natural)
arwwiDurationSeconds = lens _arwwiDurationSeconds (\ s a -> s{_arwwiDurationSeconds = a}) . mapping _Nat

-- | An IAM policy in JSON format. The policy parameter is optional. If you pass a policy, the temporary security credentials that are returned by the operation have the permissions that are allowed by both the access policy of the role that is being assumed, /__and__ / the policy that you pass. This gives you a way to further restrict the permissions for the resulting temporary security credentials. You cannot use the passed policy to grant permissions that are in excess of those allowed by the access policy of the role that is being assumed. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_assumerole.html Permissions for AssumeRoleWithWebIdentity> in the /IAM User Guide/ .  The format for this parameter, as described by its regex pattern, is a string of characters up to 2048 characters in length. The characters can be any ASCII character from the space character to the end of the valid character list (\u0020-\u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
arwwiPolicy :: Lens' AssumeRoleWithWebIdentity (Maybe Text)
arwwiPolicy = lens _arwwiPolicy (\ s a -> s{_arwwiPolicy = a})

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
arwwiRoleARN :: Lens' AssumeRoleWithWebIdentity Text
arwwiRoleARN = lens _arwwiRoleARN (\ s a -> s{_arwwiRoleARN = a})

-- | An identifier for the assumed role session. Typically, you pass the name or identifier that is associated with the user who is using your application. That way, the temporary security credentials that your application will use are associated with that user. This session name is included as part of the ARN and assumed role ID in the @AssumedRoleUser@ response element. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
arwwiRoleSessionName :: Lens' AssumeRoleWithWebIdentity Text
arwwiRoleSessionName = lens _arwwiRoleSessionName (\ s a -> s{_arwwiRoleSessionName = a})

-- | The OAuth 2.0 access token or OpenID Connect ID token that is provided by the identity provider. Your application must get this token by authenticating the user who is using your application with a web identity provider before the application makes an @AssumeRoleWithWebIdentity@ call.
arwwiWebIdentityToken :: Lens' AssumeRoleWithWebIdentity Text
arwwiWebIdentityToken = lens _arwwiWebIdentityToken (\ s a -> s{_arwwiWebIdentityToken = a})

instance AWSRequest AssumeRoleWithWebIdentity where
        type Rs AssumeRoleWithWebIdentity =
             AssumeRoleWithWebIdentityResponse
        request = postQuery sts
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

instance Hashable AssumeRoleWithWebIdentity where

instance NFData AssumeRoleWithWebIdentity where

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

-- | Contains the response to a successful 'AssumeRoleWithWebIdentity' request, including temporary AWS credentials that can be used to make AWS requests.
--
--
--
-- /See:/ 'assumeRoleWithWebIdentityResponse' smart constructor.
data AssumeRoleWithWebIdentityResponse = AssumeRoleWithWebIdentityResponse'
  { _arwwirsAudience                    :: !(Maybe Text)
  , _arwwirsSubjectFromWebIdentityToken :: !(Maybe Text)
  , _arwwirsPackedPolicySize            :: !(Maybe Nat)
  , _arwwirsCredentials                 :: !(Maybe AuthEnv)
  , _arwwirsAssumedRoleUser             :: !(Maybe AssumedRoleUser)
  , _arwwirsProvider                    :: !(Maybe Text)
  , _arwwirsResponseStatus              :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssumeRoleWithWebIdentityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arwwirsAudience' - The intended audience (also known as client ID) of the web identity token. This is traditionally the client identifier issued to the application that requested the web identity token.
--
-- * 'arwwirsSubjectFromWebIdentityToken' - The unique user identifier that is returned by the identity provider. This identifier is associated with the @WebIdentityToken@ that was submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is typically unique to the user and the application that acquired the @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens, this field contains the value returned by the identity provider as the token's @sub@ (Subject) claim.
--
-- * 'arwwirsPackedPolicySize' - A percentage value that indicates the size of the policy in packed form. The service rejects any policy with a packed size greater than 100 percent, which means the policy exceeded the allowed space.
--
-- * 'arwwirsCredentials' - The temporary security credentials, which include an access key ID, a secret access key, and a security token. __Note:__ The size of the security token that STS APIs return is not fixed. We strongly recommend that you make no assumptions about the maximum size. As of this writing, the typical size is less than 4096 bytes, but that can vary. Also, future updates to AWS might require larger sizes.
--
-- * 'arwwirsAssumedRoleUser' - The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
--
-- * 'arwwirsProvider' - The issuing authority of the web identity token presented. For OpenID Connect ID Tokens this contains the value of the @iss@ field. For OAuth 2.0 access tokens, this contains the value of the @ProviderId@ parameter that was passed in the @AssumeRoleWithWebIdentity@ request.
--
-- * 'arwwirsResponseStatus' - -- | The response status code.
assumeRoleWithWebIdentityResponse
    :: Int -- ^ 'arwwirsResponseStatus'
    -> AssumeRoleWithWebIdentityResponse
assumeRoleWithWebIdentityResponse pResponseStatus_ =
  AssumeRoleWithWebIdentityResponse'
    { _arwwirsAudience = Nothing
    , _arwwirsSubjectFromWebIdentityToken = Nothing
    , _arwwirsPackedPolicySize = Nothing
    , _arwwirsCredentials = Nothing
    , _arwwirsAssumedRoleUser = Nothing
    , _arwwirsProvider = Nothing
    , _arwwirsResponseStatus = pResponseStatus_
    }


-- | The intended audience (also known as client ID) of the web identity token. This is traditionally the client identifier issued to the application that requested the web identity token.
arwwirsAudience :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirsAudience = lens _arwwirsAudience (\ s a -> s{_arwwirsAudience = a})

-- | The unique user identifier that is returned by the identity provider. This identifier is associated with the @WebIdentityToken@ that was submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is typically unique to the user and the application that acquired the @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens, this field contains the value returned by the identity provider as the token's @sub@ (Subject) claim.
arwwirsSubjectFromWebIdentityToken :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirsSubjectFromWebIdentityToken = lens _arwwirsSubjectFromWebIdentityToken (\ s a -> s{_arwwirsSubjectFromWebIdentityToken = a})

-- | A percentage value that indicates the size of the policy in packed form. The service rejects any policy with a packed size greater than 100 percent, which means the policy exceeded the allowed space.
arwwirsPackedPolicySize :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Natural)
arwwirsPackedPolicySize = lens _arwwirsPackedPolicySize (\ s a -> s{_arwwirsPackedPolicySize = a}) . mapping _Nat

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security token. __Note:__ The size of the security token that STS APIs return is not fixed. We strongly recommend that you make no assumptions about the maximum size. As of this writing, the typical size is less than 4096 bytes, but that can vary. Also, future updates to AWS might require larger sizes.
arwwirsCredentials :: Lens' AssumeRoleWithWebIdentityResponse (Maybe AuthEnv)
arwwirsCredentials = lens _arwwirsCredentials (\ s a -> s{_arwwirsCredentials = a})

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
arwwirsAssumedRoleUser :: Lens' AssumeRoleWithWebIdentityResponse (Maybe AssumedRoleUser)
arwwirsAssumedRoleUser = lens _arwwirsAssumedRoleUser (\ s a -> s{_arwwirsAssumedRoleUser = a})

-- | The issuing authority of the web identity token presented. For OpenID Connect ID Tokens this contains the value of the @iss@ field. For OAuth 2.0 access tokens, this contains the value of the @ProviderId@ parameter that was passed in the @AssumeRoleWithWebIdentity@ request.
arwwirsProvider :: Lens' AssumeRoleWithWebIdentityResponse (Maybe Text)
arwwirsProvider = lens _arwwirsProvider (\ s a -> s{_arwwirsProvider = a})

-- | -- | The response status code.
arwwirsResponseStatus :: Lens' AssumeRoleWithWebIdentityResponse Int
arwwirsResponseStatus = lens _arwwirsResponseStatus (\ s a -> s{_arwwirsResponseStatus = a})

instance NFData AssumeRoleWithWebIdentityResponse
         where
