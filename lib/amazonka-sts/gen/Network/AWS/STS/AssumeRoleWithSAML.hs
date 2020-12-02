{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- The temporary security credentials returned by this operation consist of an access key ID, a secret access key, and a security token. Applications can use these temporary security credentials to sign calls to AWS services.
--
-- __Session Duration__
--
-- By default, the temporary security credentials created by @AssumeRoleWithSAML@ last for one hour. However, you can use the optional @DurationSeconds@ parameter to specify the duration of your session. Your role session lasts for the duration that you specify, or until the time specified in the SAML authentication response's @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . The maximum session duration limit applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI commands. However the limit does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
--
-- __Permissions__
--
-- The temporary security credentials created by @AssumeRoleWithSAML@ can be used to make API calls to any AWS service with the following exception: you cannot call the STS @GetFederationToken@ or @GetSessionToken@ API operations.
--
-- (Optional) You can pass inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policies> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
--
-- Calling @AssumeRoleWithSAML@ does not require the use of AWS security credentials. The identity of the caller is validated by using keys in the metadata document that is uploaded for the SAML provider entity for your identity provider.
--
-- /Important:/ Calling @AssumeRoleWithSAML@ can result in an entry in your AWS CloudTrail logs. The entry includes the value in the @NameID@ element of the SAML assertion. We recommend that you use a @NameIDType@ that is not associated with any personally identifiable information (PII). For example, you could instead use the persistent identifier (@urn:oasis:names:tc:SAML:2.0:nameid-format:persistent@ ).
--
-- __Tags__
--
-- (Optional) You can configure your IdP to pass attributes into your SAML assertion as session tags. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
--
-- You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ .
--
-- You can pass a session tag with the same key as a tag that is attached to the role. When you do, session tags override the role's tags with the same key.
--
-- An administrator must grant you the permissions necessary to pass session tags. The administrator can also create granular permissions to allow you to pass only specific session tags. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_attribute-based-access-control.html Tutorial: Using Tags for Attribute-Based Access Control> in the /IAM User Guide/ .
--
-- You can set the session tags as transitive. Transitive tags persist during role chaining. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ .
--
-- __SAML Configuration__
--
-- Before your application can call @AssumeRoleWithSAML@ , you must configure your SAML identity provider (IdP) to issue the claims required by AWS. Additionally, you must use AWS Identity and Access Management (IAM) to create a SAML provider entity in your AWS account that represents your identity provider. You must also create an IAM role that specifies this SAML provider in its trust policy.
--
-- For more information, see the following resources:
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based Federation> in the /IAM User Guide/ .
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_create_saml.html Creating SAML Identity Providers> in the /IAM User Guide/ .
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_create_saml_relying-party.html Configuring a Relying Party and Claims> in the /IAM User Guide/ .
--
--     * <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-idp_saml.html Creating a Role for SAML 2.0 Federation> in the /IAM User Guide/ .
module Network.AWS.STS.AssumeRoleWithSAML
  ( -- * Creating a Request
    assumeRoleWithSAML,
    AssumeRoleWithSAML,

    -- * Request Lenses
    arwsamlPolicyARNs,
    arwsamlDurationSeconds,
    arwsamlPolicy,
    arwsamlRoleARN,
    arwsamlPrincipalARN,
    arwsamlSAMLAssertion,

    -- * Destructuring the Response
    assumeRoleWithSAMLResponse,
    AssumeRoleWithSAMLResponse,

    -- * Response Lenses
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.STS.Types

-- | /See:/ 'assumeRoleWithSAML' smart constructor.
data AssumeRoleWithSAML = AssumeRoleWithSAML'
  { _arwsamlPolicyARNs ::
      !(Maybe [PolicyDescriptorType]),
    _arwsamlDurationSeconds :: !(Maybe Nat),
    _arwsamlPolicy :: !(Maybe Text),
    _arwsamlRoleARN :: !Text,
    _arwsamlPrincipalARN :: !Text,
    _arwsamlSAMLAssertion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssumeRoleWithSAML' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arwsamlPolicyARNs' - The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role. This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
--
-- * 'arwsamlDurationSeconds' - The duration, in seconds, of the role session. Your role session lasts for the duration that you specify for the @DurationSeconds@ parameter, or until the time specified in the SAML authentication response's @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . By default, the value is set to @3600@ seconds.
--
-- * 'arwsamlPolicy' - An IAM policy in JSON format that you want to use as an inline session policy. This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .  The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- * 'arwsamlRoleARN' - The Amazon Resource Name (ARN) of the role that the caller is assuming.
--
-- * 'arwsamlPrincipalARN' - The Amazon Resource Name (ARN) of the SAML provider in IAM that describes the IdP.
--
-- * 'arwsamlSAMLAssertion' - The base-64 encoded SAML authentication response provided by the IdP. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Adding Claims> in the /IAM User Guide/ .
assumeRoleWithSAML ::
  -- | 'arwsamlRoleARN'
  Text ->
  -- | 'arwsamlPrincipalARN'
  Text ->
  -- | 'arwsamlSAMLAssertion'
  Text ->
  AssumeRoleWithSAML
assumeRoleWithSAML pRoleARN_ pPrincipalARN_ pSAMLAssertion_ =
  AssumeRoleWithSAML'
    { _arwsamlPolicyARNs = Nothing,
      _arwsamlDurationSeconds = Nothing,
      _arwsamlPolicy = Nothing,
      _arwsamlRoleARN = pRoleARN_,
      _arwsamlPrincipalARN = pPrincipalARN_,
      _arwsamlSAMLAssertion = pSAMLAssertion_
    }

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role. This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
arwsamlPolicyARNs :: Lens' AssumeRoleWithSAML [PolicyDescriptorType]
arwsamlPolicyARNs = lens _arwsamlPolicyARNs (\s a -> s {_arwsamlPolicyARNs = a}) . _Default . _Coerce

-- | The duration, in seconds, of the role session. Your role session lasts for the duration that you specify for the @DurationSeconds@ parameter, or until the time specified in the SAML authentication response's @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . By default, the value is set to @3600@ seconds.
arwsamlDurationSeconds :: Lens' AssumeRoleWithSAML (Maybe Natural)
arwsamlDurationSeconds = lens _arwsamlDurationSeconds (\s a -> s {_arwsamlDurationSeconds = a}) . mapping _Nat

-- | An IAM policy in JSON format that you want to use as an inline session policy. This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .  The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
arwsamlPolicy :: Lens' AssumeRoleWithSAML (Maybe Text)
arwsamlPolicy = lens _arwsamlPolicy (\s a -> s {_arwsamlPolicy = a})

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
arwsamlRoleARN :: Lens' AssumeRoleWithSAML Text
arwsamlRoleARN = lens _arwsamlRoleARN (\s a -> s {_arwsamlRoleARN = a})

-- | The Amazon Resource Name (ARN) of the SAML provider in IAM that describes the IdP.
arwsamlPrincipalARN :: Lens' AssumeRoleWithSAML Text
arwsamlPrincipalARN = lens _arwsamlPrincipalARN (\s a -> s {_arwsamlPrincipalARN = a})

-- | The base-64 encoded SAML authentication response provided by the IdP. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Adding Claims> in the /IAM User Guide/ .
arwsamlSAMLAssertion :: Lens' AssumeRoleWithSAML Text
arwsamlSAMLAssertion = lens _arwsamlSAMLAssertion (\s a -> s {_arwsamlSAMLAssertion = a})

instance AWSRequest AssumeRoleWithSAML where
  type Rs AssumeRoleWithSAML = AssumeRoleWithSAMLResponse
  request = postQuery sts
  response =
    receiveXMLWrapper
      "AssumeRoleWithSAMLResult"
      ( \s h x ->
          AssumeRoleWithSAMLResponse'
            <$> (x .@? "Subject")
            <*> (x .@? "Audience")
            <*> (x .@? "PackedPolicySize")
            <*> (x .@? "Credentials")
            <*> (x .@? "SubjectType")
            <*> (x .@? "NameQualifier")
            <*> (x .@? "AssumedRoleUser")
            <*> (x .@? "Issuer")
            <*> (pure (fromEnum s))
      )

instance Hashable AssumeRoleWithSAML

instance NFData AssumeRoleWithSAML

instance ToHeaders AssumeRoleWithSAML where
  toHeaders = const mempty

instance ToPath AssumeRoleWithSAML where
  toPath = const "/"

instance ToQuery AssumeRoleWithSAML where
  toQuery AssumeRoleWithSAML' {..} =
    mconcat
      [ "Action" =: ("AssumeRoleWithSAML" :: ByteString),
        "Version" =: ("2011-06-15" :: ByteString),
        "PolicyArns"
          =: toQuery (toQueryList "member" <$> _arwsamlPolicyARNs),
        "DurationSeconds" =: _arwsamlDurationSeconds,
        "Policy" =: _arwsamlPolicy,
        "RoleArn" =: _arwsamlRoleARN,
        "PrincipalArn" =: _arwsamlPrincipalARN,
        "SAMLAssertion" =: _arwsamlSAMLAssertion
      ]

-- | Contains the response to a successful 'AssumeRoleWithSAML' request, including temporary AWS credentials that can be used to make AWS requests.
--
--
--
-- /See:/ 'assumeRoleWithSAMLResponse' smart constructor.
data AssumeRoleWithSAMLResponse = AssumeRoleWithSAMLResponse'
  { _arwsamlrsSubject ::
      !(Maybe Text),
    _arwsamlrsAudience :: !(Maybe Text),
    _arwsamlrsPackedPolicySize ::
      !(Maybe Nat),
    _arwsamlrsCredentials ::
      !(Maybe AuthEnv),
    _arwsamlrsSubjectType ::
      !(Maybe Text),
    _arwsamlrsNameQualifier ::
      !(Maybe Text),
    _arwsamlrsAssumedRoleUser ::
      !(Maybe AssumedRoleUser),
    _arwsamlrsIssuer :: !(Maybe Text),
    _arwsamlrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssumeRoleWithSAMLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arwsamlrsSubject' - The value of the @NameID@ element in the @Subject@ element of the SAML assertion.
--
-- * 'arwsamlrsAudience' - The value of the @Recipient@ attribute of the @SubjectConfirmationData@ element of the SAML assertion.
--
-- * 'arwsamlrsPackedPolicySize' - A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
--
-- * 'arwsamlrsCredentials' - The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
--
-- * 'arwsamlrsSubjectType' - The format of the name ID, as defined by the @Format@ attribute in the @NameID@ element of the SAML assertion. Typical examples of the format are @transient@ or @persistent@ .  If the format includes the prefix @urn:oasis:names:tc:SAML:2.0:nameid-format@ , that prefix is removed. For example, @urn:oasis:names:tc:SAML:2.0:nameid-format:transient@ is returned as @transient@ . If the format includes any other prefix, the format is returned with no modifications.
--
-- * 'arwsamlrsNameQualifier' - A hash value based on the concatenation of the @Issuer@ response value, the AWS account ID, and the friendly name (the last part of the ARN) of the SAML provider in IAM. The combination of @NameQualifier@ and @Subject@ can be used to uniquely identify a federated user.  The following pseudocode shows how the hash value is calculated: @BASE64 ( SHA1 ( "https://example.com/saml" + "123456789012" + "/MySAMLIdP" ) )@
--
-- * 'arwsamlrsAssumedRoleUser' - The identifiers for the temporary security credentials that the operation returns.
--
-- * 'arwsamlrsIssuer' - The value of the @Issuer@ element of the SAML assertion.
--
-- * 'arwsamlrsResponseStatus' - -- | The response status code.
assumeRoleWithSAMLResponse ::
  -- | 'arwsamlrsResponseStatus'
  Int ->
  AssumeRoleWithSAMLResponse
assumeRoleWithSAMLResponse pResponseStatus_ =
  AssumeRoleWithSAMLResponse'
    { _arwsamlrsSubject = Nothing,
      _arwsamlrsAudience = Nothing,
      _arwsamlrsPackedPolicySize = Nothing,
      _arwsamlrsCredentials = Nothing,
      _arwsamlrsSubjectType = Nothing,
      _arwsamlrsNameQualifier = Nothing,
      _arwsamlrsAssumedRoleUser = Nothing,
      _arwsamlrsIssuer = Nothing,
      _arwsamlrsResponseStatus = pResponseStatus_
    }

-- | The value of the @NameID@ element in the @Subject@ element of the SAML assertion.
arwsamlrsSubject :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsSubject = lens _arwsamlrsSubject (\s a -> s {_arwsamlrsSubject = a})

-- | The value of the @Recipient@ attribute of the @SubjectConfirmationData@ element of the SAML assertion.
arwsamlrsAudience :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsAudience = lens _arwsamlrsAudience (\s a -> s {_arwsamlrsAudience = a})

-- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
arwsamlrsPackedPolicySize :: Lens' AssumeRoleWithSAMLResponse (Maybe Natural)
arwsamlrsPackedPolicySize = lens _arwsamlrsPackedPolicySize (\s a -> s {_arwsamlrsPackedPolicySize = a}) . mapping _Nat

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
arwsamlrsCredentials :: Lens' AssumeRoleWithSAMLResponse (Maybe AuthEnv)
arwsamlrsCredentials = lens _arwsamlrsCredentials (\s a -> s {_arwsamlrsCredentials = a})

-- | The format of the name ID, as defined by the @Format@ attribute in the @NameID@ element of the SAML assertion. Typical examples of the format are @transient@ or @persistent@ .  If the format includes the prefix @urn:oasis:names:tc:SAML:2.0:nameid-format@ , that prefix is removed. For example, @urn:oasis:names:tc:SAML:2.0:nameid-format:transient@ is returned as @transient@ . If the format includes any other prefix, the format is returned with no modifications.
arwsamlrsSubjectType :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsSubjectType = lens _arwsamlrsSubjectType (\s a -> s {_arwsamlrsSubjectType = a})

-- | A hash value based on the concatenation of the @Issuer@ response value, the AWS account ID, and the friendly name (the last part of the ARN) of the SAML provider in IAM. The combination of @NameQualifier@ and @Subject@ can be used to uniquely identify a federated user.  The following pseudocode shows how the hash value is calculated: @BASE64 ( SHA1 ( "https://example.com/saml" + "123456789012" + "/MySAMLIdP" ) )@
arwsamlrsNameQualifier :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsNameQualifier = lens _arwsamlrsNameQualifier (\s a -> s {_arwsamlrsNameQualifier = a})

-- | The identifiers for the temporary security credentials that the operation returns.
arwsamlrsAssumedRoleUser :: Lens' AssumeRoleWithSAMLResponse (Maybe AssumedRoleUser)
arwsamlrsAssumedRoleUser = lens _arwsamlrsAssumedRoleUser (\s a -> s {_arwsamlrsAssumedRoleUser = a})

-- | The value of the @Issuer@ element of the SAML assertion.
arwsamlrsIssuer :: Lens' AssumeRoleWithSAMLResponse (Maybe Text)
arwsamlrsIssuer = lens _arwsamlrsIssuer (\s a -> s {_arwsamlrsIssuer = a})

-- | -- | The response status code.
arwsamlrsResponseStatus :: Lens' AssumeRoleWithSAMLResponse Int
arwsamlrsResponseStatus = lens _arwsamlrsResponseStatus (\s a -> s {_arwsamlrsResponseStatus = a})

instance NFData AssumeRoleWithSAMLResponse
