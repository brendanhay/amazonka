{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.STS.AssumeRoleWithWebIdentity
    (
    -- * Creating a request
      AssumeRoleWithWebIdentity (..)
    , mkAssumeRoleWithWebIdentity
    -- ** Request lenses
    , arwwiRoleArn
    , arwwiRoleSessionName
    , arwwiWebIdentityToken
    , arwwiDurationSeconds
    , arwwiPolicy
    , arwwiPolicyArns
    , arwwiProviderId

    -- * Destructuring the response
    , AssumeRoleWithWebIdentityResponse (..)
    , mkAssumeRoleWithWebIdentityResponse
    -- ** Response lenses
    , arwwirrsAssumedRoleUser
    , arwwirrsAudience
    , arwwirrsCredentials
    , arwwirrsPackedPolicySize
    , arwwirrsProvider
    , arwwirrsSubjectFromWebIdentityToken
    , arwwirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.STS.Types as Types

-- | /See:/ 'mkAssumeRoleWithWebIdentity' smart constructor.
data AssumeRoleWithWebIdentity = AssumeRoleWithWebIdentity'
  { roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the role that the caller is assuming.
  , roleSessionName :: Types.RoleSessionName
    -- ^ An identifier for the assumed role session. Typically, you pass the name or identifier that is associated with the user who is using your application. That way, the temporary security credentials that your application will use are associated with that user. This session name is included as part of the ARN and assumed role ID in the @AssumedRoleUser@ response element.
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
  , webIdentityToken :: Types.WebIdentityToken
    -- ^ The OAuth 2.0 access token or OpenID Connect ID token that is provided by the identity provider. Your application must get this token by authenticating the user who is using your application with a web identity provider before the application makes an @AssumeRoleWithWebIdentity@ call. 
  , durationSeconds :: Core.Maybe Core.Natural
    -- ^ The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
--
-- By default, the value is set to @3600@ seconds. 
  , policy :: Core.Maybe Types.Policy
    -- ^ An IAM policy in JSON format that you want to use as an inline session policy.
--
-- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
  , policyArns :: Core.Maybe [Types.PolicyDescriptorType]
    -- ^ The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
-- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
  , providerId :: Core.Maybe Types.ProviderId
    -- ^ The fully qualified host component of the domain name of the identity provider.
--
-- Specify this value only for OAuth 2.0 access tokens. Currently @www.amazon.com@ and @graph.facebook.com@ are the only supported identity providers for OAuth 2.0 access tokens. Do not include URL schemes and port numbers.
-- Do not specify this value for OpenID Connect ID tokens.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssumeRoleWithWebIdentity' value with any optional fields omitted.
mkAssumeRoleWithWebIdentity
    :: Types.RoleArn -- ^ 'roleArn'
    -> Types.RoleSessionName -- ^ 'roleSessionName'
    -> Types.WebIdentityToken -- ^ 'webIdentityToken'
    -> AssumeRoleWithWebIdentity
mkAssumeRoleWithWebIdentity roleArn roleSessionName
  webIdentityToken
  = AssumeRoleWithWebIdentity'{roleArn, roleSessionName,
                               webIdentityToken, durationSeconds = Core.Nothing,
                               policy = Core.Nothing, policyArns = Core.Nothing,
                               providerId = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiRoleArn :: Lens.Lens' AssumeRoleWithWebIdentity Types.RoleArn
arwwiRoleArn = Lens.field @"roleArn"
{-# INLINEABLE arwwiRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | An identifier for the assumed role session. Typically, you pass the name or identifier that is associated with the user who is using your application. That way, the temporary security credentials that your application will use are associated with that user. This session name is included as part of the ARN and assumed role ID in the @AssumedRoleUser@ response element.
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
--
-- /Note:/ Consider using 'roleSessionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiRoleSessionName :: Lens.Lens' AssumeRoleWithWebIdentity Types.RoleSessionName
arwwiRoleSessionName = Lens.field @"roleSessionName"
{-# INLINEABLE arwwiRoleSessionName #-}
{-# DEPRECATED roleSessionName "Use generic-lens or generic-optics with 'roleSessionName' instead"  #-}

-- | The OAuth 2.0 access token or OpenID Connect ID token that is provided by the identity provider. Your application must get this token by authenticating the user who is using your application with a web identity provider before the application makes an @AssumeRoleWithWebIdentity@ call. 
--
-- /Note:/ Consider using 'webIdentityToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiWebIdentityToken :: Lens.Lens' AssumeRoleWithWebIdentity Types.WebIdentityToken
arwwiWebIdentityToken = Lens.field @"webIdentityToken"
{-# INLINEABLE arwwiWebIdentityToken #-}
{-# DEPRECATED webIdentityToken "Use generic-lens or generic-optics with 'webIdentityToken' instead"  #-}

-- | The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
--
-- By default, the value is set to @3600@ seconds. 
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiDurationSeconds :: Lens.Lens' AssumeRoleWithWebIdentity (Core.Maybe Core.Natural)
arwwiDurationSeconds = Lens.field @"durationSeconds"
{-# INLINEABLE arwwiDurationSeconds #-}
{-# DEPRECATED durationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead"  #-}

-- | An IAM policy in JSON format that you want to use as an inline session policy.
--
-- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiPolicy :: Lens.Lens' AssumeRoleWithWebIdentity (Core.Maybe Types.Policy)
arwwiPolicy = Lens.field @"policy"
{-# INLINEABLE arwwiPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
-- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiPolicyArns :: Lens.Lens' AssumeRoleWithWebIdentity (Core.Maybe [Types.PolicyDescriptorType])
arwwiPolicyArns = Lens.field @"policyArns"
{-# INLINEABLE arwwiPolicyArns #-}
{-# DEPRECATED policyArns "Use generic-lens or generic-optics with 'policyArns' instead"  #-}

-- | The fully qualified host component of the domain name of the identity provider.
--
-- Specify this value only for OAuth 2.0 access tokens. Currently @www.amazon.com@ and @graph.facebook.com@ are the only supported identity providers for OAuth 2.0 access tokens. Do not include URL schemes and port numbers.
-- Do not specify this value for OpenID Connect ID tokens.
--
-- /Note:/ Consider using 'providerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwiProviderId :: Lens.Lens' AssumeRoleWithWebIdentity (Core.Maybe Types.ProviderId)
arwwiProviderId = Lens.field @"providerId"
{-# INLINEABLE arwwiProviderId #-}
{-# DEPRECATED providerId "Use generic-lens or generic-optics with 'providerId' instead"  #-}

instance Core.ToQuery AssumeRoleWithWebIdentity where
        toQuery AssumeRoleWithWebIdentity{..}
          = Core.toQueryPair "Action"
              ("AssumeRoleWithWebIdentity" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-06-15" :: Core.Text)
              Core.<> Core.toQueryPair "RoleArn" roleArn
              Core.<> Core.toQueryPair "RoleSessionName" roleSessionName
              Core.<> Core.toQueryPair "WebIdentityToken" webIdentityToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DurationSeconds")
                durationSeconds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Policy") policy
              Core.<>
              Core.toQueryPair "PolicyArns"
                (Core.maybe Core.mempty (Core.toQueryList "member") policyArns)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ProviderId") providerId

instance Core.ToHeaders AssumeRoleWithWebIdentity where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AssumeRoleWithWebIdentity where
        type Rs AssumeRoleWithWebIdentity =
             AssumeRoleWithWebIdentityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "AssumeRoleWithWebIdentityResult"
              (\ s h x ->
                 AssumeRoleWithWebIdentityResponse' Core.<$>
                   (x Core..@? "AssumedRoleUser") Core.<*> x Core..@? "Audience"
                     Core.<*> x Core..@? "Credentials"
                     Core.<*> x Core..@? "PackedPolicySize"
                     Core.<*> x Core..@? "Provider"
                     Core.<*> x Core..@? "SubjectFromWebIdentityToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'AssumeRoleWithWebIdentity' request, including temporary AWS credentials that can be used to make AWS requests. 
--
-- /See:/ 'mkAssumeRoleWithWebIdentityResponse' smart constructor.
data AssumeRoleWithWebIdentityResponse = AssumeRoleWithWebIdentityResponse'
  { assumedRoleUser :: Core.Maybe Types.AssumedRoleUser
    -- ^ The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ . 
  , audience :: Core.Maybe Types.Audience
    -- ^ The intended audience (also known as client ID) of the web identity token. This is traditionally the client identifier issued to the application that requested the web identity token.
  , credentials :: Core.Maybe Types.AuthEnv
    -- ^ The temporary security credentials, which include an access key ID, a secret access key, and a security token.
  , packedPolicySize :: Core.Maybe Core.Natural
    -- ^ A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
  , provider :: Core.Maybe Types.Provider
    -- ^ The issuing authority of the web identity token presented. For OpenID Connect ID tokens, this contains the value of the @iss@ field. For OAuth 2.0 access tokens, this contains the value of the @ProviderId@ parameter that was passed in the @AssumeRoleWithWebIdentity@ request.
  , subjectFromWebIdentityToken :: Core.Maybe Types.WebIdentitySubjectType
    -- ^ The unique user identifier that is returned by the identity provider. This identifier is associated with the @WebIdentityToken@ that was submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is typically unique to the user and the application that acquired the @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens, this field contains the value returned by the identity provider as the token's @sub@ (Subject) claim. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssumeRoleWithWebIdentityResponse' value with any optional fields omitted.
mkAssumeRoleWithWebIdentityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssumeRoleWithWebIdentityResponse
mkAssumeRoleWithWebIdentityResponse responseStatus
  = AssumeRoleWithWebIdentityResponse'{assumedRoleUser =
                                         Core.Nothing,
                                       audience = Core.Nothing, credentials = Core.Nothing,
                                       packedPolicySize = Core.Nothing, provider = Core.Nothing,
                                       subjectFromWebIdentityToken = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ . 
--
-- /Note:/ Consider using 'assumedRoleUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirrsAssumedRoleUser :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Core.Maybe Types.AssumedRoleUser)
arwwirrsAssumedRoleUser = Lens.field @"assumedRoleUser"
{-# INLINEABLE arwwirrsAssumedRoleUser #-}
{-# DEPRECATED assumedRoleUser "Use generic-lens or generic-optics with 'assumedRoleUser' instead"  #-}

-- | The intended audience (also known as client ID) of the web identity token. This is traditionally the client identifier issued to the application that requested the web identity token.
--
-- /Note:/ Consider using 'audience' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirrsAudience :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Core.Maybe Types.Audience)
arwwirrsAudience = Lens.field @"audience"
{-# INLINEABLE arwwirrsAudience #-}
{-# DEPRECATED audience "Use generic-lens or generic-optics with 'audience' instead"  #-}

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security token.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirrsCredentials :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Core.Maybe Types.AuthEnv)
arwwirrsCredentials = Lens.field @"credentials"
{-# INLINEABLE arwwirrsCredentials #-}
{-# DEPRECATED credentials "Use generic-lens or generic-optics with 'credentials' instead"  #-}

-- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
--
-- /Note:/ Consider using 'packedPolicySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirrsPackedPolicySize :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Core.Maybe Core.Natural)
arwwirrsPackedPolicySize = Lens.field @"packedPolicySize"
{-# INLINEABLE arwwirrsPackedPolicySize #-}
{-# DEPRECATED packedPolicySize "Use generic-lens or generic-optics with 'packedPolicySize' instead"  #-}

-- | The issuing authority of the web identity token presented. For OpenID Connect ID tokens, this contains the value of the @iss@ field. For OAuth 2.0 access tokens, this contains the value of the @ProviderId@ parameter that was passed in the @AssumeRoleWithWebIdentity@ request.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirrsProvider :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Core.Maybe Types.Provider)
arwwirrsProvider = Lens.field @"provider"
{-# INLINEABLE arwwirrsProvider #-}
{-# DEPRECATED provider "Use generic-lens or generic-optics with 'provider' instead"  #-}

-- | The unique user identifier that is returned by the identity provider. This identifier is associated with the @WebIdentityToken@ that was submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is typically unique to the user and the application that acquired the @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens, this field contains the value returned by the identity provider as the token's @sub@ (Subject) claim. 
--
-- /Note:/ Consider using 'subjectFromWebIdentityToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirrsSubjectFromWebIdentityToken :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Core.Maybe Types.WebIdentitySubjectType)
arwwirrsSubjectFromWebIdentityToken = Lens.field @"subjectFromWebIdentityToken"
{-# INLINEABLE arwwirrsSubjectFromWebIdentityToken #-}
{-# DEPRECATED subjectFromWebIdentityToken "Use generic-lens or generic-optics with 'subjectFromWebIdentityToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwwirrsResponseStatus :: Lens.Lens' AssumeRoleWithWebIdentityResponse Core.Int
arwwirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE arwwirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
