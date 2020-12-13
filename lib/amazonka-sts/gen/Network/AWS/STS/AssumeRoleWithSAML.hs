{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.AssumeRoleWithSAML
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials for users who have been authenticated via a SAML authentication response. This operation provides a mechanism for tying an enterprise identity store or directory to role-based AWS access without user-specific credentials or configuration. For a comparison of @AssumeRoleWithSAML@ with the other API operations that produce temporary credentials, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials> and <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS API operations> in the /IAM User Guide/ .
--
-- The temporary security credentials returned by this operation consist of an access key ID, a secret access key, and a security token. Applications can use these temporary security credentials to sign calls to AWS services.
-- __Session Duration__
-- By default, the temporary security credentials created by @AssumeRoleWithSAML@ last for one hour. However, you can use the optional @DurationSeconds@ parameter to specify the duration of your session. Your role session lasts for the duration that you specify, or until the time specified in the SAML authentication response's @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . The maximum session duration limit applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI commands. However the limit does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
-- __Permissions__
-- The temporary security credentials created by @AssumeRoleWithSAML@ can be used to make API calls to any AWS service with the following exception: you cannot call the STS @GetFederationToken@ or @GetSessionToken@ API operations.
-- (Optional) You can pass inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policies> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- Calling @AssumeRoleWithSAML@ does not require the use of AWS security credentials. The identity of the caller is validated by using keys in the metadata document that is uploaded for the SAML provider entity for your identity provider.
-- /Important:/ Calling @AssumeRoleWithSAML@ can result in an entry in your AWS CloudTrail logs. The entry includes the value in the @NameID@ element of the SAML assertion. We recommend that you use a @NameIDType@ that is not associated with any personally identifiable information (PII). For example, you could instead use the persistent identifier (@urn:oasis:names:tc:SAML:2.0:nameid-format:persistent@ ).
-- __Tags__
-- (Optional) You can configure your IdP to pass attributes into your SAML assertion as session tags. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
-- You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ .
-- You can pass a session tag with the same key as a tag that is attached to the role. When you do, session tags override the role's tags with the same key.
-- An administrator must grant you the permissions necessary to pass session tags. The administrator can also create granular permissions to allow you to pass only specific session tags. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_attribute-based-access-control.html Tutorial: Using Tags for Attribute-Based Access Control> in the /IAM User Guide/ .
-- You can set the session tags as transitive. Transitive tags persist during role chaining. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ .
-- __SAML Configuration__
-- Before your application can call @AssumeRoleWithSAML@ , you must configure your SAML identity provider (IdP) to issue the claims required by AWS. Additionally, you must use AWS Identity and Access Management (IAM) to create a SAML provider entity in your AWS account that represents your identity provider. You must also create an IAM role that specifies this SAML provider in its trust policy.
-- For more information, see the following resources:
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based Federation> in the /IAM User Guide/ .
--
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_create_saml.html Creating SAML Identity Providers> in the /IAM User Guide/ .
--
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_create_saml_relying-party.html Configuring a Relying Party and Claims> in the /IAM User Guide/ .
--
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-idp_saml.html Creating a Role for SAML 2.0 Federation> in the /IAM User Guide/ .
module Network.AWS.STS.AssumeRoleWithSAML
  ( -- * Creating a request
    AssumeRoleWithSAML (..),
    mkAssumeRoleWithSAML,

    -- ** Request lenses
    arwsamlPolicyARNs,
    arwsamlPrincipalARN,
    arwsamlDurationSeconds,
    arwsamlPolicy,
    arwsamlSAMLAssertion,
    arwsamlRoleARN,

    -- * Destructuring the response
    AssumeRoleWithSAMLResponse (..),
    mkAssumeRoleWithSAMLResponse,

    -- ** Response lenses
    arwsamlrsSubject,
    arwsamlrsAudience,
    arwsamlrsPackedPolicySize,
    arwsamlrsCredentials,
    arwsamlrsSubjectType,
    arwsamlrsNameQualifier,
    arwsamlrsAssumedRoleUser,
    arwsamlrsIssuer,
    arwsamlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.STS.Types

-- | /See:/ 'mkAssumeRoleWithSAML' smart constructor.
data AssumeRoleWithSAML = AssumeRoleWithSAML'
  { -- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
    --
    -- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
    -- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
    policyARNs :: Lude.Maybe [PolicyDescriptorType],
    -- | The Amazon Resource Name (ARN) of the SAML provider in IAM that describes the IdP.
    principalARN :: Lude.Text,
    -- | The duration, in seconds, of the role session. Your role session lasts for the duration that you specify for the @DurationSeconds@ parameter, or until the time specified in the SAML authentication response's @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
    --
    -- By default, the value is set to @3600@ seconds.
    durationSeconds :: Lude.Maybe Lude.Natural,
    -- | An IAM policy in JSON format that you want to use as an inline session policy.
    --
    -- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
    -- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
    policy :: Lude.Maybe Lude.Text,
    -- | The base-64 encoded SAML authentication response provided by the IdP.
    --
    -- For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Adding Claims> in the /IAM User Guide/ .
    sAMLAssertion :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssumeRoleWithSAML' with the minimum fields required to make a request.
--
-- * 'policyARNs' - The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
-- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- * 'principalARN' - The Amazon Resource Name (ARN) of the SAML provider in IAM that describes the IdP.
-- * 'durationSeconds' - The duration, in seconds, of the role session. Your role session lasts for the duration that you specify for the @DurationSeconds@ parameter, or until the time specified in the SAML authentication response's @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
--
-- By default, the value is set to @3600@ seconds.
-- * 'policy' - An IAM policy in JSON format that you want to use as an inline session policy.
--
-- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
-- * 'sAMLAssertion' - The base-64 encoded SAML authentication response provided by the IdP.
--
-- For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Adding Claims> in the /IAM User Guide/ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role that the caller is assuming.
mkAssumeRoleWithSAML ::
  -- | 'principalARN'
  Lude.Text ->
  -- | 'sAMLAssertion'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  AssumeRoleWithSAML
mkAssumeRoleWithSAML pPrincipalARN_ pSAMLAssertion_ pRoleARN_ =
  AssumeRoleWithSAML'
    { policyARNs = Lude.Nothing,
      principalARN = pPrincipalARN_,
      durationSeconds = Lude.Nothing,
      policy = Lude.Nothing,
      sAMLAssertion = pSAMLAssertion_,
      roleARN = pRoleARN_
    }

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
-- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlPolicyARNs :: Lens.Lens' AssumeRoleWithSAML (Lude.Maybe [PolicyDescriptorType])
arwsamlPolicyARNs = Lens.lens (policyARNs :: AssumeRoleWithSAML -> Lude.Maybe [PolicyDescriptorType]) (\s a -> s {policyARNs = a} :: AssumeRoleWithSAML)
{-# DEPRECATED arwsamlPolicyARNs "Use generic-lens or generic-optics with 'policyARNs' instead." #-}

-- | The Amazon Resource Name (ARN) of the SAML provider in IAM that describes the IdP.
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlPrincipalARN :: Lens.Lens' AssumeRoleWithSAML Lude.Text
arwsamlPrincipalARN = Lens.lens (principalARN :: AssumeRoleWithSAML -> Lude.Text) (\s a -> s {principalARN = a} :: AssumeRoleWithSAML)
{-# DEPRECATED arwsamlPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | The duration, in seconds, of the role session. Your role session lasts for the duration that you specify for the @DurationSeconds@ parameter, or until the time specified in the SAML authentication response's @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
--
-- By default, the value is set to @3600@ seconds.
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlDurationSeconds :: Lens.Lens' AssumeRoleWithSAML (Lude.Maybe Lude.Natural)
arwsamlDurationSeconds = Lens.lens (durationSeconds :: AssumeRoleWithSAML -> Lude.Maybe Lude.Natural) (\s a -> s {durationSeconds = a} :: AssumeRoleWithSAML)
{-# DEPRECATED arwsamlDurationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead." #-}

-- | An IAM policy in JSON format that you want to use as an inline session policy.
--
-- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlPolicy :: Lens.Lens' AssumeRoleWithSAML (Lude.Maybe Lude.Text)
arwsamlPolicy = Lens.lens (policy :: AssumeRoleWithSAML -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: AssumeRoleWithSAML)
{-# DEPRECATED arwsamlPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The base-64 encoded SAML authentication response provided by the IdP.
--
-- For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Adding Claims> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'sAMLAssertion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlSAMLAssertion :: Lens.Lens' AssumeRoleWithSAML Lude.Text
arwsamlSAMLAssertion = Lens.lens (sAMLAssertion :: AssumeRoleWithSAML -> Lude.Text) (\s a -> s {sAMLAssertion = a} :: AssumeRoleWithSAML)
{-# DEPRECATED arwsamlSAMLAssertion "Use generic-lens or generic-optics with 'sAMLAssertion' instead." #-}

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlRoleARN :: Lens.Lens' AssumeRoleWithSAML Lude.Text
arwsamlRoleARN = Lens.lens (roleARN :: AssumeRoleWithSAML -> Lude.Text) (\s a -> s {roleARN = a} :: AssumeRoleWithSAML)
{-# DEPRECATED arwsamlRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest AssumeRoleWithSAML where
  type Rs AssumeRoleWithSAML = AssumeRoleWithSAMLResponse
  request = Req.postQuery stsService
  response =
    Res.receiveXMLWrapper
      "AssumeRoleWithSAMLResult"
      ( \s h x ->
          AssumeRoleWithSAMLResponse'
            Lude.<$> (x Lude..@? "Subject")
            Lude.<*> (x Lude..@? "Audience")
            Lude.<*> (x Lude..@? "PackedPolicySize")
            Lude.<*> (x Lude..@? "Credentials")
            Lude.<*> (x Lude..@? "SubjectType")
            Lude.<*> (x Lude..@? "NameQualifier")
            Lude.<*> (x Lude..@? "AssumedRoleUser")
            Lude.<*> (x Lude..@? "Issuer")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssumeRoleWithSAML where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssumeRoleWithSAML where
  toPath = Lude.const "/"

instance Lude.ToQuery AssumeRoleWithSAML where
  toQuery AssumeRoleWithSAML' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssumeRoleWithSAML" :: Lude.ByteString),
        "Version" Lude.=: ("2011-06-15" :: Lude.ByteString),
        "PolicyArns"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyARNs),
        "PrincipalArn" Lude.=: principalARN,
        "DurationSeconds" Lude.=: durationSeconds,
        "Policy" Lude.=: policy,
        "SAMLAssertion" Lude.=: sAMLAssertion,
        "RoleArn" Lude.=: roleARN
      ]

-- | Contains the response to a successful 'AssumeRoleWithSAML' request, including temporary AWS credentials that can be used to make AWS requests.
--
-- /See:/ 'mkAssumeRoleWithSAMLResponse' smart constructor.
data AssumeRoleWithSAMLResponse = AssumeRoleWithSAMLResponse'
  { -- | The value of the @NameID@ element in the @Subject@ element of the SAML assertion.
    subject :: Lude.Maybe Lude.Text,
    -- | The value of the @Recipient@ attribute of the @SubjectConfirmationData@ element of the SAML assertion.
    audience :: Lude.Maybe Lude.Text,
    -- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
    packedPolicySize :: Lude.Maybe Lude.Natural,
    -- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
    credentials :: Lude.Maybe AuthEnv,
    -- | The format of the name ID, as defined by the @Format@ attribute in the @NameID@ element of the SAML assertion. Typical examples of the format are @transient@ or @persistent@ .
    --
    -- If the format includes the prefix @urn:oasis:names:tc:SAML:2.0:nameid-format@ , that prefix is removed. For example, @urn:oasis:names:tc:SAML:2.0:nameid-format:transient@ is returned as @transient@ . If the format includes any other prefix, the format is returned with no modifications.
    subjectType :: Lude.Maybe Lude.Text,
    -- | A hash value based on the concatenation of the @Issuer@ response value, the AWS account ID, and the friendly name (the last part of the ARN) of the SAML provider in IAM. The combination of @NameQualifier@ and @Subject@ can be used to uniquely identify a federated user.
    --
    -- The following pseudocode shows how the hash value is calculated:
    -- @BASE64 ( SHA1 ( "https://example.com/saml" + "123456789012" + "/MySAMLIdP" ) )@
    nameQualifier :: Lude.Maybe Lude.Text,
    -- | The identifiers for the temporary security credentials that the operation returns.
    assumedRoleUser :: Lude.Maybe AssumedRoleUser,
    -- | The value of the @Issuer@ element of the SAML assertion.
    issuer :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssumeRoleWithSAMLResponse' with the minimum fields required to make a request.
--
-- * 'subject' - The value of the @NameID@ element in the @Subject@ element of the SAML assertion.
-- * 'audience' - The value of the @Recipient@ attribute of the @SubjectConfirmationData@ element of the SAML assertion.
-- * 'packedPolicySize' - A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
-- * 'credentials' - The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
-- * 'subjectType' - The format of the name ID, as defined by the @Format@ attribute in the @NameID@ element of the SAML assertion. Typical examples of the format are @transient@ or @persistent@ .
--
-- If the format includes the prefix @urn:oasis:names:tc:SAML:2.0:nameid-format@ , that prefix is removed. For example, @urn:oasis:names:tc:SAML:2.0:nameid-format:transient@ is returned as @transient@ . If the format includes any other prefix, the format is returned with no modifications.
-- * 'nameQualifier' - A hash value based on the concatenation of the @Issuer@ response value, the AWS account ID, and the friendly name (the last part of the ARN) of the SAML provider in IAM. The combination of @NameQualifier@ and @Subject@ can be used to uniquely identify a federated user.
--
-- The following pseudocode shows how the hash value is calculated:
-- @BASE64 ( SHA1 ( "https://example.com/saml" + "123456789012" + "/MySAMLIdP" ) )@
-- * 'assumedRoleUser' - The identifiers for the temporary security credentials that the operation returns.
-- * 'issuer' - The value of the @Issuer@ element of the SAML assertion.
-- * 'responseStatus' - The response status code.
mkAssumeRoleWithSAMLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssumeRoleWithSAMLResponse
mkAssumeRoleWithSAMLResponse pResponseStatus_ =
  AssumeRoleWithSAMLResponse'
    { subject = Lude.Nothing,
      audience = Lude.Nothing,
      packedPolicySize = Lude.Nothing,
      credentials = Lude.Nothing,
      subjectType = Lude.Nothing,
      nameQualifier = Lude.Nothing,
      assumedRoleUser = Lude.Nothing,
      issuer = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The value of the @NameID@ element in the @Subject@ element of the SAML assertion.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlrsSubject :: Lens.Lens' AssumeRoleWithSAMLResponse (Lude.Maybe Lude.Text)
arwsamlrsSubject = Lens.lens (subject :: AssumeRoleWithSAMLResponse -> Lude.Maybe Lude.Text) (\s a -> s {subject = a} :: AssumeRoleWithSAMLResponse)
{-# DEPRECATED arwsamlrsSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The value of the @Recipient@ attribute of the @SubjectConfirmationData@ element of the SAML assertion.
--
-- /Note:/ Consider using 'audience' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlrsAudience :: Lens.Lens' AssumeRoleWithSAMLResponse (Lude.Maybe Lude.Text)
arwsamlrsAudience = Lens.lens (audience :: AssumeRoleWithSAMLResponse -> Lude.Maybe Lude.Text) (\s a -> s {audience = a} :: AssumeRoleWithSAMLResponse)
{-# DEPRECATED arwsamlrsAudience "Use generic-lens or generic-optics with 'audience' instead." #-}

-- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
--
-- /Note:/ Consider using 'packedPolicySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlrsPackedPolicySize :: Lens.Lens' AssumeRoleWithSAMLResponse (Lude.Maybe Lude.Natural)
arwsamlrsPackedPolicySize = Lens.lens (packedPolicySize :: AssumeRoleWithSAMLResponse -> Lude.Maybe Lude.Natural) (\s a -> s {packedPolicySize = a} :: AssumeRoleWithSAMLResponse)
{-# DEPRECATED arwsamlrsPackedPolicySize "Use generic-lens or generic-optics with 'packedPolicySize' instead." #-}

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlrsCredentials :: Lens.Lens' AssumeRoleWithSAMLResponse (Lude.Maybe AuthEnv)
arwsamlrsCredentials = Lens.lens (credentials :: AssumeRoleWithSAMLResponse -> Lude.Maybe AuthEnv) (\s a -> s {credentials = a} :: AssumeRoleWithSAMLResponse)
{-# DEPRECATED arwsamlrsCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | The format of the name ID, as defined by the @Format@ attribute in the @NameID@ element of the SAML assertion. Typical examples of the format are @transient@ or @persistent@ .
--
-- If the format includes the prefix @urn:oasis:names:tc:SAML:2.0:nameid-format@ , that prefix is removed. For example, @urn:oasis:names:tc:SAML:2.0:nameid-format:transient@ is returned as @transient@ . If the format includes any other prefix, the format is returned with no modifications.
--
-- /Note:/ Consider using 'subjectType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlrsSubjectType :: Lens.Lens' AssumeRoleWithSAMLResponse (Lude.Maybe Lude.Text)
arwsamlrsSubjectType = Lens.lens (subjectType :: AssumeRoleWithSAMLResponse -> Lude.Maybe Lude.Text) (\s a -> s {subjectType = a} :: AssumeRoleWithSAMLResponse)
{-# DEPRECATED arwsamlrsSubjectType "Use generic-lens or generic-optics with 'subjectType' instead." #-}

-- | A hash value based on the concatenation of the @Issuer@ response value, the AWS account ID, and the friendly name (the last part of the ARN) of the SAML provider in IAM. The combination of @NameQualifier@ and @Subject@ can be used to uniquely identify a federated user.
--
-- The following pseudocode shows how the hash value is calculated:
-- @BASE64 ( SHA1 ( "https://example.com/saml" + "123456789012" + "/MySAMLIdP" ) )@
--
-- /Note:/ Consider using 'nameQualifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlrsNameQualifier :: Lens.Lens' AssumeRoleWithSAMLResponse (Lude.Maybe Lude.Text)
arwsamlrsNameQualifier = Lens.lens (nameQualifier :: AssumeRoleWithSAMLResponse -> Lude.Maybe Lude.Text) (\s a -> s {nameQualifier = a} :: AssumeRoleWithSAMLResponse)
{-# DEPRECATED arwsamlrsNameQualifier "Use generic-lens or generic-optics with 'nameQualifier' instead." #-}

-- | The identifiers for the temporary security credentials that the operation returns.
--
-- /Note:/ Consider using 'assumedRoleUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlrsAssumedRoleUser :: Lens.Lens' AssumeRoleWithSAMLResponse (Lude.Maybe AssumedRoleUser)
arwsamlrsAssumedRoleUser = Lens.lens (assumedRoleUser :: AssumeRoleWithSAMLResponse -> Lude.Maybe AssumedRoleUser) (\s a -> s {assumedRoleUser = a} :: AssumeRoleWithSAMLResponse)
{-# DEPRECATED arwsamlrsAssumedRoleUser "Use generic-lens or generic-optics with 'assumedRoleUser' instead." #-}

-- | The value of the @Issuer@ element of the SAML assertion.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlrsIssuer :: Lens.Lens' AssumeRoleWithSAMLResponse (Lude.Maybe Lude.Text)
arwsamlrsIssuer = Lens.lens (issuer :: AssumeRoleWithSAMLResponse -> Lude.Maybe Lude.Text) (\s a -> s {issuer = a} :: AssumeRoleWithSAMLResponse)
{-# DEPRECATED arwsamlrsIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arwsamlrsResponseStatus :: Lens.Lens' AssumeRoleWithSAMLResponse Lude.Int
arwsamlrsResponseStatus = Lens.lens (responseStatus :: AssumeRoleWithSAMLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssumeRoleWithSAMLResponse)
{-# DEPRECATED arwsamlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
