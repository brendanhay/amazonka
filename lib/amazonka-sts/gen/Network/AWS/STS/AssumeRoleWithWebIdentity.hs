{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.AssumeRoleWithWebIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials for users who have been authenticated in a mobile or web application with a web identity provider. Example providers include Amazon Cognito, Login with Amazon, Facebook, Google, or any OpenID Connect-compatible identity provider.
--
-- Calling @AssumeRoleWithWebIdentity@ does not require the use of AWS security credentials. Therefore, you can distribute an application (for example, on mobile devices) that requests temporary security credentials without including long-term AWS credentials in the application. You also don't need to deploy server-based proxy services that use long-term AWS credentials. Instead, the identity of the caller is validated by using a token from the web identity provider. For a comparison of @AssumeRoleWithWebIdentity@ with the other API operations that produce temporary credentials, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials> and <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS API operations> in the /IAM User Guide/ .
-- The temporary security credentials returned by this API consist of an access key ID, a secret access key, and a security token. Applications can use these temporary security credentials to sign calls to AWS service API operations.
-- __Session Duration__
-- By default, the temporary security credentials created by @AssumeRoleWithWebIdentity@ last for one hour. However, you can use the optional @DurationSeconds@ parameter to specify the duration of your session. You can provide a value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . The maximum session duration limit applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI commands. However the limit does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
-- __Permissions__
-- The temporary security credentials created by @AssumeRoleWithWebIdentity@ can be used to make API calls to any AWS service with the following exception: you cannot call the STS @GetFederationToken@ or @GetSessionToken@ API operations.
-- (Optional) You can pass inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policies> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- __Tags__
-- (Optional) You can configure your IdP to pass attributes into your web identity token as session tags. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
-- You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ .
-- You can pass a session tag with the same key as a tag that is attached to the role. When you do, the session tag overrides the role tag with the same key.
-- An administrator must grant you the permissions necessary to pass session tags. The administrator can also create granular permissions to allow you to pass only specific session tags. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_attribute-based-access-control.html Tutorial: Using Tags for Attribute-Based Access Control> in the /IAM User Guide/ .
-- You can set the session tags as transitive. Transitive tags persist during role chaining. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ .
-- __Identities__
-- Before your application can call @AssumeRoleWithWebIdentity@ , you must have an identity token from a supported identity provider and create a role that the application can assume. The role that your application assumes must trust the identity provider that is associated with the identity token. In other words, the identity provider must be specified in the role's trust policy.
-- /Important:/ Calling @AssumeRoleWithWebIdentity@ can result in an entry in your AWS CloudTrail logs. The entry includes the <http://openid.net/specs/openid-connect-core-1_0.html#Claims Subject> of the provided Web Identity Token. We recommend that you avoid using any personally identifiable information (PII) in this field. For example, you could instead use a GUID or a pairwise identifier, as <http://openid.net/specs/openid-connect-core-1_0.html#SubjectIDTypes suggested in the OIDC specification> .
-- For more information about how to use web identity federation and the @AssumeRoleWithWebIdentity@ API, see the following resources:
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_oidc_manual.html Using Web Identity Federation API Operations for Mobile Apps> and <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_assumerolewithwebidentity Federation Through a Web-based Identity Provider> .
--
--
--     * <https://aws.amazon.com/blogs/aws/the-aws-web-identity-federation-playground/ Web Identity Federation Playground> . Walk through the process of authenticating through Login with Amazon, Facebook, or Google, getting temporary security credentials, and then using those credentials to make a request to AWS.
--
--
--     * <http://aws.amazon.com/sdkforios/ AWS SDK for iOS Developer Guide> and <http://aws.amazon.com/sdkforandroid/ AWS SDK for Android Developer Guide> . These toolkits contain sample apps that show how to invoke the identity providers. The toolkits then show how to use the information from these providers to get and use temporary security credentials.
--
--
--     * <http://aws.amazon.com/articles/web-identity-federation-with-mobile-applications Web Identity Federation with Mobile Applications> . This article discusses web identity federation and shows an example of how to use web identity federation to get access to content in Amazon S3.
module Network.AWS.STS.AssumeRoleWithWebIdentity
  ( -- * Creating a request
    AssumeRoleWithWebIdentity (..),
    mkAssumeRoleWithWebIdentity,

    -- ** Request lenses
    arwwiProviderId,
    arwwiRoleSessionName,
    arwwiPolicyARNs,
    arwwiWebIdentityToken,
    arwwiDurationSeconds,
    arwwiPolicy,
    arwwiRoleARN,

    -- * Destructuring the response
    AssumeRoleWithWebIdentityResponse (..),
    mkAssumeRoleWithWebIdentityResponse,

    -- ** Response lenses
    arwwirsAudience,
    arwwirsSubjectFromWebIdentityToken,
    arwwirsPackedPolicySize,
    arwwirsCredentials,
    arwwirsAssumedRoleUser,
    arwwirsProvider,
    arwwirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.STS.Types

-- | /See:/ 'mkAssumeRoleWithWebIdentity' smart constructor.
data AssumeRoleWithWebIdentity = AssumeRoleWithWebIdentity'
  { -- | The fully qualified host component of the domain name of the identity provider.
    --
    -- Specify this value only for OAuth 2.0 access tokens. Currently @www.amazon.com@ and @graph.facebook.com@ are the only supported identity providers for OAuth 2.0 access tokens. Do not include URL schemes and port numbers.
    -- Do not specify this value for OpenID Connect ID tokens.
    providerId :: Lude.Maybe Lude.Text,
    -- | An identifier for the assumed role session. Typically, you pass the name or identifier that is associated with the user who is using your application. That way, the temporary security credentials that your application will use are associated with that user. This session name is included as part of the ARN and assumed role ID in the @AssumedRoleUser@ response element.
    --
    -- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
    roleSessionName :: Lude.Text,
    -- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
    --
    -- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
    -- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
    policyARNs :: Lude.Maybe [PolicyDescriptorType],
    -- | The OAuth 2.0 access token or OpenID Connect ID token that is provided by the identity provider. Your application must get this token by authenticating the user who is using your application with a web identity provider before the application makes an @AssumeRoleWithWebIdentity@ call.
    webIdentityToken :: Lude.Text,
    -- | The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
    --
    -- By default, the value is set to @3600@ seconds.
    durationSeconds :: Lude.Maybe Lude.Natural,
    -- | An IAM policy in JSON format that you want to use as an inline session policy.
    --
    -- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
    -- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
    policy :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssumeRoleWithWebIdentity' with the minimum fields required to make a request.
--
-- * 'providerId' - The fully qualified host component of the domain name of the identity provider.
--
-- Specify this value only for OAuth 2.0 access tokens. Currently @www.amazon.com@ and @graph.facebook.com@ are the only supported identity providers for OAuth 2.0 access tokens. Do not include URL schemes and port numbers.
-- Do not specify this value for OpenID Connect ID tokens.
-- * 'roleSessionName' - An identifier for the assumed role session. Typically, you pass the name or identifier that is associated with the user who is using your application. That way, the temporary security credentials that your application will use are associated with that user. This session name is included as part of the ARN and assumed role ID in the @AssumedRoleUser@ response element.
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
-- * 'policyARNs' - The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
-- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- * 'webIdentityToken' - The OAuth 2.0 access token or OpenID Connect ID token that is provided by the identity provider. Your application must get this token by authenticating the user who is using your application with a web identity provider before the application makes an @AssumeRoleWithWebIdentity@ call.
-- * 'durationSeconds' - The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
--
-- By default, the value is set to @3600@ seconds.
-- * 'policy' - An IAM policy in JSON format that you want to use as an inline session policy.
--
-- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role that the caller is assuming.
mkAssumeRoleWithWebIdentity ::
  -- | 'roleSessionName'
  Lude.Text ->
  -- | 'webIdentityToken'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  AssumeRoleWithWebIdentity
mkAssumeRoleWithWebIdentity
  pRoleSessionName_
  pWebIdentityToken_
  pRoleARN_ =
    AssumeRoleWithWebIdentity'
      { providerId = Lude.Nothing,
        roleSessionName = pRoleSessionName_,
        policyARNs = Lude.Nothing,
        webIdentityToken = pWebIdentityToken_,
        durationSeconds = Lude.Nothing,
        policy = Lude.Nothing,
        roleARN = pRoleARN_
      }

-- | The fully qualified host component of the domain name of the identity provider.
--
-- Specify this value only for OAuth 2.0 access tokens. Currently @www.amazon.com@ and @graph.facebook.com@ are the only supported identity providers for OAuth 2.0 access tokens. Do not include URL schemes and port numbers.
-- Do not specify this value for OpenID Connect ID tokens.
--
-- /Note:/ Consider using 'providerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiProviderId :: Lens.Lens' AssumeRoleWithWebIdentity (Lude.Maybe Lude.Text)
arwwiProviderId = Lens.lens (providerId :: AssumeRoleWithWebIdentity -> Lude.Maybe Lude.Text) (\s a -> s {providerId = a} :: AssumeRoleWithWebIdentity)
{-# DEPRECATED arwwiProviderId "Use generic-lens or generic-optics with 'providerId' instead." #-}

-- | An identifier for the assumed role session. Typically, you pass the name or identifier that is associated with the user who is using your application. That way, the temporary security credentials that your application will use are associated with that user. This session name is included as part of the ARN and assumed role ID in the @AssumedRoleUser@ response element.
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
--
-- /Note:/ Consider using 'roleSessionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiRoleSessionName :: Lens.Lens' AssumeRoleWithWebIdentity Lude.Text
arwwiRoleSessionName = Lens.lens (roleSessionName :: AssumeRoleWithWebIdentity -> Lude.Text) (\s a -> s {roleSessionName = a} :: AssumeRoleWithWebIdentity)
{-# DEPRECATED arwwiRoleSessionName "Use generic-lens or generic-optics with 'roleSessionName' instead." #-}

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
-- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiPolicyARNs :: Lens.Lens' AssumeRoleWithWebIdentity (Lude.Maybe [PolicyDescriptorType])
arwwiPolicyARNs = Lens.lens (policyARNs :: AssumeRoleWithWebIdentity -> Lude.Maybe [PolicyDescriptorType]) (\s a -> s {policyARNs = a} :: AssumeRoleWithWebIdentity)
{-# DEPRECATED arwwiPolicyARNs "Use generic-lens or generic-optics with 'policyARNs' instead." #-}

-- | The OAuth 2.0 access token or OpenID Connect ID token that is provided by the identity provider. Your application must get this token by authenticating the user who is using your application with a web identity provider before the application makes an @AssumeRoleWithWebIdentity@ call.
--
-- /Note:/ Consider using 'webIdentityToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiWebIdentityToken :: Lens.Lens' AssumeRoleWithWebIdentity Lude.Text
arwwiWebIdentityToken = Lens.lens (webIdentityToken :: AssumeRoleWithWebIdentity -> Lude.Text) (\s a -> s {webIdentityToken = a} :: AssumeRoleWithWebIdentity)
{-# DEPRECATED arwwiWebIdentityToken "Use generic-lens or generic-optics with 'webIdentityToken' instead." #-}

-- | The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
--
-- By default, the value is set to @3600@ seconds.
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiDurationSeconds :: Lens.Lens' AssumeRoleWithWebIdentity (Lude.Maybe Lude.Natural)
arwwiDurationSeconds = Lens.lens (durationSeconds :: AssumeRoleWithWebIdentity -> Lude.Maybe Lude.Natural) (\s a -> s {durationSeconds = a} :: AssumeRoleWithWebIdentity)
{-# DEPRECATED arwwiDurationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead." #-}

-- | An IAM policy in JSON format that you want to use as an inline session policy.
--
-- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiPolicy :: Lens.Lens' AssumeRoleWithWebIdentity (Lude.Maybe Lude.Text)
arwwiPolicy = Lens.lens (policy :: AssumeRoleWithWebIdentity -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: AssumeRoleWithWebIdentity)
{-# DEPRECATED arwwiPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiRoleARN :: Lens.Lens' AssumeRoleWithWebIdentity Lude.Text
arwwiRoleARN = Lens.lens (roleARN :: AssumeRoleWithWebIdentity -> Lude.Text) (\s a -> s {roleARN = a} :: AssumeRoleWithWebIdentity)
{-# DEPRECATED arwwiRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest AssumeRoleWithWebIdentity where
  type
    Rs AssumeRoleWithWebIdentity =
      AssumeRoleWithWebIdentityResponse
  request = Req.postQuery stsService
  response =
    Res.receiveXMLWrapper
      "AssumeRoleWithWebIdentityResult"
      ( \s h x ->
          AssumeRoleWithWebIdentityResponse'
            Lude.<$> (x Lude..@? "Audience")
            Lude.<*> (x Lude..@? "SubjectFromWebIdentityToken")
            Lude.<*> (x Lude..@? "PackedPolicySize")
            Lude.<*> (x Lude..@? "Credentials")
            Lude.<*> (x Lude..@? "AssumedRoleUser")
            Lude.<*> (x Lude..@? "Provider")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssumeRoleWithWebIdentity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssumeRoleWithWebIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery AssumeRoleWithWebIdentity where
  toQuery AssumeRoleWithWebIdentity' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssumeRoleWithWebIdentity" :: Lude.ByteString),
        "Version" Lude.=: ("2011-06-15" :: Lude.ByteString),
        "ProviderId" Lude.=: providerId,
        "RoleSessionName" Lude.=: roleSessionName,
        "PolicyArns"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyARNs),
        "WebIdentityToken" Lude.=: webIdentityToken,
        "DurationSeconds" Lude.=: durationSeconds,
        "Policy" Lude.=: policy,
        "RoleArn" Lude.=: roleARN
      ]

-- | Contains the response to a successful 'AssumeRoleWithWebIdentity' request, including temporary AWS credentials that can be used to make AWS requests.
--
-- /See:/ 'mkAssumeRoleWithWebIdentityResponse' smart constructor.
data AssumeRoleWithWebIdentityResponse = AssumeRoleWithWebIdentityResponse'
  { -- | The intended audience (also known as client ID) of the web identity token. This is traditionally the client identifier issued to the application that requested the web identity token.
    audience :: Lude.Maybe Lude.Text,
    -- | The unique user identifier that is returned by the identity provider. This identifier is associated with the @WebIdentityToken@ that was submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is typically unique to the user and the application that acquired the @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens, this field contains the value returned by the identity provider as the token's @sub@ (Subject) claim.
    subjectFromWebIdentityToken :: Lude.Maybe Lude.Text,
    -- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
    packedPolicySize :: Lude.Maybe Lude.Natural,
    -- | The temporary security credentials, which include an access key ID, a secret access key, and a security token.
    credentials :: Lude.Maybe AuthEnv,
    -- | The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
    assumedRoleUser :: Lude.Maybe AssumedRoleUser,
    -- | The issuing authority of the web identity token presented. For OpenID Connect ID tokens, this contains the value of the @iss@ field. For OAuth 2.0 access tokens, this contains the value of the @ProviderId@ parameter that was passed in the @AssumeRoleWithWebIdentity@ request.
    provider :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssumeRoleWithWebIdentityResponse' with the minimum fields required to make a request.
--
-- * 'audience' - The intended audience (also known as client ID) of the web identity token. This is traditionally the client identifier issued to the application that requested the web identity token.
-- * 'subjectFromWebIdentityToken' - The unique user identifier that is returned by the identity provider. This identifier is associated with the @WebIdentityToken@ that was submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is typically unique to the user and the application that acquired the @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens, this field contains the value returned by the identity provider as the token's @sub@ (Subject) claim.
-- * 'packedPolicySize' - A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
-- * 'credentials' - The temporary security credentials, which include an access key ID, a secret access key, and a security token.
-- * 'assumedRoleUser' - The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
-- * 'provider' - The issuing authority of the web identity token presented. For OpenID Connect ID tokens, this contains the value of the @iss@ field. For OAuth 2.0 access tokens, this contains the value of the @ProviderId@ parameter that was passed in the @AssumeRoleWithWebIdentity@ request.
-- * 'responseStatus' - The response status code.
mkAssumeRoleWithWebIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssumeRoleWithWebIdentityResponse
mkAssumeRoleWithWebIdentityResponse pResponseStatus_ =
  AssumeRoleWithWebIdentityResponse'
    { audience = Lude.Nothing,
      subjectFromWebIdentityToken = Lude.Nothing,
      packedPolicySize = Lude.Nothing,
      credentials = Lude.Nothing,
      assumedRoleUser = Lude.Nothing,
      provider = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The intended audience (also known as client ID) of the web identity token. This is traditionally the client identifier issued to the application that requested the web identity token.
--
-- /Note:/ Consider using 'audience' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirsAudience :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Lude.Maybe Lude.Text)
arwwirsAudience = Lens.lens (audience :: AssumeRoleWithWebIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {audience = a} :: AssumeRoleWithWebIdentityResponse)
{-# DEPRECATED arwwirsAudience "Use generic-lens or generic-optics with 'audience' instead." #-}

-- | The unique user identifier that is returned by the identity provider. This identifier is associated with the @WebIdentityToken@ that was submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is typically unique to the user and the application that acquired the @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens, this field contains the value returned by the identity provider as the token's @sub@ (Subject) claim.
--
-- /Note:/ Consider using 'subjectFromWebIdentityToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirsSubjectFromWebIdentityToken :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Lude.Maybe Lude.Text)
arwwirsSubjectFromWebIdentityToken = Lens.lens (subjectFromWebIdentityToken :: AssumeRoleWithWebIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {subjectFromWebIdentityToken = a} :: AssumeRoleWithWebIdentityResponse)
{-# DEPRECATED arwwirsSubjectFromWebIdentityToken "Use generic-lens or generic-optics with 'subjectFromWebIdentityToken' instead." #-}

-- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
--
-- /Note:/ Consider using 'packedPolicySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirsPackedPolicySize :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Lude.Maybe Lude.Natural)
arwwirsPackedPolicySize = Lens.lens (packedPolicySize :: AssumeRoleWithWebIdentityResponse -> Lude.Maybe Lude.Natural) (\s a -> s {packedPolicySize = a} :: AssumeRoleWithWebIdentityResponse)
{-# DEPRECATED arwwirsPackedPolicySize "Use generic-lens or generic-optics with 'packedPolicySize' instead." #-}

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security token.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirsCredentials :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Lude.Maybe AuthEnv)
arwwirsCredentials = Lens.lens (credentials :: AssumeRoleWithWebIdentityResponse -> Lude.Maybe AuthEnv) (\s a -> s {credentials = a} :: AssumeRoleWithWebIdentityResponse)
{-# DEPRECATED arwwirsCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
--
-- /Note:/ Consider using 'assumedRoleUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirsAssumedRoleUser :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Lude.Maybe AssumedRoleUser)
arwwirsAssumedRoleUser = Lens.lens (assumedRoleUser :: AssumeRoleWithWebIdentityResponse -> Lude.Maybe AssumedRoleUser) (\s a -> s {assumedRoleUser = a} :: AssumeRoleWithWebIdentityResponse)
{-# DEPRECATED arwwirsAssumedRoleUser "Use generic-lens or generic-optics with 'assumedRoleUser' instead." #-}

-- | The issuing authority of the web identity token presented. For OpenID Connect ID tokens, this contains the value of the @iss@ field. For OAuth 2.0 access tokens, this contains the value of the @ProviderId@ parameter that was passed in the @AssumeRoleWithWebIdentity@ request.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirsProvider :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Lude.Maybe Lude.Text)
arwwirsProvider = Lens.lens (provider :: AssumeRoleWithWebIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {provider = a} :: AssumeRoleWithWebIdentityResponse)
{-# DEPRECATED arwwirsProvider "Use generic-lens or generic-optics with 'provider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirsResponseStatus :: Lens.Lens' AssumeRoleWithWebIdentityResponse Lude.Int
arwwirsResponseStatus = Lens.lens (responseStatus :: AssumeRoleWithWebIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssumeRoleWithWebIdentityResponse)
{-# DEPRECATED arwwirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
