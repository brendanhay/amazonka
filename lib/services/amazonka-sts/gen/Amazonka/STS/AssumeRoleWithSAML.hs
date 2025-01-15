{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.STS.AssumeRoleWithSAML
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials for users who have been
-- authenticated via a SAML authentication response. This operation
-- provides a mechanism for tying an enterprise identity store or directory
-- to role-based Amazon Web Services access without user-specific
-- credentials or configuration. For a comparison of @AssumeRoleWithSAML@
-- with the other API operations that produce temporary credentials, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the Amazon Web Services STS API operations>
-- in the /IAM User Guide/.
--
-- The temporary security credentials returned by this operation consist of
-- an access key ID, a secret access key, and a security token.
-- Applications can use these temporary security credentials to sign calls
-- to Amazon Web Services services.
--
-- __Session Duration__
--
-- By default, the temporary security credentials created by
-- @AssumeRoleWithSAML@ last for one hour. However, you can use the
-- optional @DurationSeconds@ parameter to specify the duration of your
-- session. Your role session lasts for the duration that you specify, or
-- until the time specified in the SAML authentication response\'s
-- @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a
-- @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum
-- session duration setting for the role. This setting can have a value
-- from 1 hour to 12 hours. To learn how to view the maximum value for your
-- role, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role>
-- in the /IAM User Guide/. The maximum session duration limit applies when
-- you use the @AssumeRole*@ API operations or the @assume-role*@ CLI
-- commands. However the limit does not apply when you use those operations
-- to create a console URL. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles>
-- in the /IAM User Guide/.
--
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts.html#iam-term-role-chaining Role chaining>
-- limits your CLI or Amazon Web Services API role session to a maximum of
-- one hour. When you use the @AssumeRole@ API operation to assume a role,
-- you can specify the duration of your role session with the
-- @DurationSeconds@ parameter. You can specify a parameter value of up to
-- 43200 seconds (12 hours), depending on the maximum session duration
-- setting for your role. However, if you assume a role using role chaining
-- and provide a @DurationSeconds@ parameter value greater than one hour,
-- the operation fails.
--
-- __Permissions__
--
-- The temporary security credentials created by @AssumeRoleWithSAML@ can
-- be used to make API calls to any Amazon Web Services service with the
-- following exception: you cannot call the STS @GetFederationToken@ or
-- @GetSessionToken@ API operations.
--
-- (Optional) You can pass inline or managed
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policies>
-- to this operation. You can pass a single JSON policy document to use as
-- an inline session policy. You can also specify up to 10 managed policy
-- Amazon Resource Names (ARNs) to use as managed session policies. The
-- plaintext that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters. Passing policies to this operation
-- returns new temporary credentials. The resulting session\'s permissions
-- are the intersection of the role\'s identity-based policy and the
-- session policies. You can use the role\'s temporary credentials in
-- subsequent Amazon Web Services API calls to access resources in the
-- account that owns the role. You cannot use session policies to grant
-- more permissions than those allowed by the identity-based policy of the
-- role that is being assumed. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- Calling @AssumeRoleWithSAML@ does not require the use of Amazon Web
-- Services security credentials. The identity of the caller is validated
-- by using keys in the metadata document that is uploaded for the SAML
-- provider entity for your identity provider.
--
-- Calling @AssumeRoleWithSAML@ can result in an entry in your CloudTrail
-- logs. The entry includes the value in the @NameID@ element of the SAML
-- assertion. We recommend that you use a @NameIDType@ that is not
-- associated with any personally identifiable information (PII). For
-- example, you could instead use the persistent identifier
-- (@urn:oasis:names:tc:SAML:2.0:nameid-format:persistent@).
--
-- __Tags__
--
-- (Optional) You can configure your IdP to pass attributes into your SAML
-- assertion as session tags. Each session tag consists of a key name and
-- an associated value. For more information about session tags, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS>
-- in the /IAM User Guide/.
--
-- You can pass up to 50 session tags. The plaintext session tag keys can’t
-- exceed 128 characters and the values can’t exceed 256 characters. For
-- these and additional limits, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
-- in the /IAM User Guide/.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
--
-- You can pass a session tag with the same key as a tag that is attached
-- to the role. When you do, session tags override the role\'s tags with
-- the same key.
--
-- An administrator must grant you the permissions necessary to pass
-- session tags. The administrator can also create granular permissions to
-- allow you to pass only specific session tags. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_attribute-based-access-control.html Tutorial: Using Tags for Attribute-Based Access Control>
-- in the /IAM User Guide/.
--
-- You can set the session tags as transitive. Transitive tags persist
-- during role chaining. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags>
-- in the /IAM User Guide/.
--
-- __SAML Configuration__
--
-- Before your application can call @AssumeRoleWithSAML@, you must
-- configure your SAML identity provider (IdP) to issue the claims required
-- by Amazon Web Services. Additionally, you must use Identity and Access
-- Management (IAM) to create a SAML provider entity in your Amazon Web
-- Services account that represents your identity provider. You must also
-- create an IAM role that specifies this SAML provider in its trust
-- policy.
--
-- For more information, see the following resources:
--
-- -   <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based Federation>
--     in the /IAM User Guide/.
--
-- -   <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_create_saml.html Creating SAML Identity Providers>
--     in the /IAM User Guide/.
--
-- -   <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_create_saml_relying-party.html Configuring a Relying Party and Claims>
--     in the /IAM User Guide/.
--
-- -   <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-idp_saml.html Creating a Role for SAML 2.0 Federation>
--     in the /IAM User Guide/.
module Amazonka.STS.AssumeRoleWithSAML
  ( -- * Creating a Request
    AssumeRoleWithSAML (..),
    newAssumeRoleWithSAML,

    -- * Request Lenses
    assumeRoleWithSAML_durationSeconds,
    assumeRoleWithSAML_policy,
    assumeRoleWithSAML_policyArns,
    assumeRoleWithSAML_roleArn,
    assumeRoleWithSAML_principalArn,
    assumeRoleWithSAML_sAMLAssertion,

    -- * Destructuring the Response
    AssumeRoleWithSAMLResponse (..),
    newAssumeRoleWithSAMLResponse,

    -- * Response Lenses
    assumeRoleWithSAMLResponse_assumedRoleUser,
    assumeRoleWithSAMLResponse_audience,
    assumeRoleWithSAMLResponse_credentials,
    assumeRoleWithSAMLResponse_issuer,
    assumeRoleWithSAMLResponse_nameQualifier,
    assumeRoleWithSAMLResponse_packedPolicySize,
    assumeRoleWithSAMLResponse_sourceIdentity,
    assumeRoleWithSAMLResponse_subject,
    assumeRoleWithSAMLResponse_subjectType,
    assumeRoleWithSAMLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.STS.Types

-- | /See:/ 'newAssumeRoleWithSAML' smart constructor.
data AssumeRoleWithSAML = AssumeRoleWithSAML'
  { -- | The duration, in seconds, of the role session. Your role session lasts
    -- for the duration that you specify for the @DurationSeconds@ parameter,
    -- or until the time specified in the SAML authentication response\'s
    -- @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a
    -- @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum
    -- session duration setting for the role. This setting can have a value
    -- from 1 hour to 12 hours. If you specify a value higher than this
    -- setting, the operation fails. For example, if you specify a session
    -- duration of 12 hours, but your administrator set the maximum session
    -- duration to 6 hours, your operation fails. To learn how to view the
    -- maximum value for your role, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role>
    -- in the /IAM User Guide/.
    --
    -- By default, the value is set to @3600@ seconds.
    --
    -- The @DurationSeconds@ parameter is separate from the duration of a
    -- console session that you might request using the returned credentials.
    -- The request to the federation endpoint for a console sign-in token takes
    -- a @SessionDuration@ parameter that specifies the maximum length of the
    -- console session. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-custom-url.html Creating a URL that Enables Federated Users to Access the Amazon Web Services Management Console>
    -- in the /IAM User Guide/.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | An IAM policy in JSON format that you want to use as an inline session
    -- policy.
    --
    -- This parameter is optional. Passing policies to this operation returns
    -- new temporary credentials. The resulting session\'s permissions are the
    -- intersection of the role\'s identity-based policy and the session
    -- policies. You can use the role\'s temporary credentials in subsequent
    -- Amazon Web Services API calls to access resources in the account that
    -- owns the role. You cannot use session policies to grant more permissions
    -- than those allowed by the identity-based policy of the role that is
    -- being assumed. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
    -- in the /IAM User Guide/.
    --
    -- The plaintext that you use for both inline and managed session policies
    -- can\'t exceed 2,048 characters. The JSON policy characters can be any
    -- ASCII character from the space character to the end of the valid
    -- character list (\\u0020 through \\u00FF). It can also include the tab
    -- (\\u0009), linefeed (\\u000A), and carriage return (\\u000D) characters.
    --
    -- An Amazon Web Services conversion compresses the passed inline session
    -- policy, managed policy ARNs, and session tags into a packed binary
    -- format that has a separate limit. Your request can fail for this limit
    -- even if your plaintext meets the other requirements. The
    -- @PackedPolicySize@ response element indicates by percentage how close
    -- the policies and tags for your request are to the upper size limit.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the IAM managed policies that you
    -- want to use as managed session policies. The policies must exist in the
    -- same account as the role.
    --
    -- This parameter is optional. You can provide up to 10 managed policy
    -- ARNs. However, the plaintext that you use for both inline and managed
    -- session policies can\'t exceed 2,048 characters. For more information
    -- about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the Amazon Web Services General Reference.
    --
    -- An Amazon Web Services conversion compresses the passed inline session
    -- policy, managed policy ARNs, and session tags into a packed binary
    -- format that has a separate limit. Your request can fail for this limit
    -- even if your plaintext meets the other requirements. The
    -- @PackedPolicySize@ response element indicates by percentage how close
    -- the policies and tags for your request are to the upper size limit.
    --
    -- Passing policies to this operation returns new temporary credentials.
    -- The resulting session\'s permissions are the intersection of the role\'s
    -- identity-based policy and the session policies. You can use the role\'s
    -- temporary credentials in subsequent Amazon Web Services API calls to
    -- access resources in the account that owns the role. You cannot use
    -- session policies to grant more permissions than those allowed by the
    -- identity-based policy of the role that is being assumed. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
    -- in the /IAM User Guide/.
    policyArns :: Prelude.Maybe [PolicyDescriptorType],
    -- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
    roleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the SAML provider in IAM that
    -- describes the IdP.
    principalArn :: Prelude.Text,
    -- | The base64 encoded SAML authentication response provided by the IdP.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Adding Claims>
    -- in the /IAM User Guide/.
    sAMLAssertion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeRoleWithSAML' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationSeconds', 'assumeRoleWithSAML_durationSeconds' - The duration, in seconds, of the role session. Your role session lasts
-- for the duration that you specify for the @DurationSeconds@ parameter,
-- or until the time specified in the SAML authentication response\'s
-- @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a
-- @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum
-- session duration setting for the role. This setting can have a value
-- from 1 hour to 12 hours. If you specify a value higher than this
-- setting, the operation fails. For example, if you specify a session
-- duration of 12 hours, but your administrator set the maximum session
-- duration to 6 hours, your operation fails. To learn how to view the
-- maximum value for your role, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role>
-- in the /IAM User Guide/.
--
-- By default, the value is set to @3600@ seconds.
--
-- The @DurationSeconds@ parameter is separate from the duration of a
-- console session that you might request using the returned credentials.
-- The request to the federation endpoint for a console sign-in token takes
-- a @SessionDuration@ parameter that specifies the maximum length of the
-- console session. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-custom-url.html Creating a URL that Enables Federated Users to Access the Amazon Web Services Management Console>
-- in the /IAM User Guide/.
--
-- 'policy', 'assumeRoleWithSAML_policy' - An IAM policy in JSON format that you want to use as an inline session
-- policy.
--
-- This parameter is optional. Passing policies to this operation returns
-- new temporary credentials. The resulting session\'s permissions are the
-- intersection of the role\'s identity-based policy and the session
-- policies. You can use the role\'s temporary credentials in subsequent
-- Amazon Web Services API calls to access resources in the account that
-- owns the role. You cannot use session policies to grant more permissions
-- than those allowed by the identity-based policy of the role that is
-- being assumed. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- The plaintext that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters. The JSON policy characters can be any
-- ASCII character from the space character to the end of the valid
-- character list (\\u0020 through \\u00FF). It can also include the tab
-- (\\u0009), linefeed (\\u000A), and carriage return (\\u000D) characters.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
--
-- 'policyArns', 'assumeRoleWithSAML_policyArns' - The Amazon Resource Names (ARNs) of the IAM managed policies that you
-- want to use as managed session policies. The policies must exist in the
-- same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy
-- ARNs. However, the plaintext that you use for both inline and managed
-- session policies can\'t exceed 2,048 characters. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the Amazon Web Services General Reference.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
--
-- Passing policies to this operation returns new temporary credentials.
-- The resulting session\'s permissions are the intersection of the role\'s
-- identity-based policy and the session policies. You can use the role\'s
-- temporary credentials in subsequent Amazon Web Services API calls to
-- access resources in the account that owns the role. You cannot use
-- session policies to grant more permissions than those allowed by the
-- identity-based policy of the role that is being assumed. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- 'roleArn', 'assumeRoleWithSAML_roleArn' - The Amazon Resource Name (ARN) of the role that the caller is assuming.
--
-- 'principalArn', 'assumeRoleWithSAML_principalArn' - The Amazon Resource Name (ARN) of the SAML provider in IAM that
-- describes the IdP.
--
-- 'sAMLAssertion', 'assumeRoleWithSAML_sAMLAssertion' - The base64 encoded SAML authentication response provided by the IdP.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Adding Claims>
-- in the /IAM User Guide/.
newAssumeRoleWithSAML ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'principalArn'
  Prelude.Text ->
  -- | 'sAMLAssertion'
  Prelude.Text ->
  AssumeRoleWithSAML
newAssumeRoleWithSAML
  pRoleArn_
  pPrincipalArn_
  pSAMLAssertion_ =
    AssumeRoleWithSAML'
      { durationSeconds =
          Prelude.Nothing,
        policy = Prelude.Nothing,
        policyArns = Prelude.Nothing,
        roleArn = pRoleArn_,
        principalArn = pPrincipalArn_,
        sAMLAssertion = pSAMLAssertion_
      }

-- | The duration, in seconds, of the role session. Your role session lasts
-- for the duration that you specify for the @DurationSeconds@ parameter,
-- or until the time specified in the SAML authentication response\'s
-- @SessionNotOnOrAfter@ value, whichever is shorter. You can provide a
-- @DurationSeconds@ value from 900 seconds (15 minutes) up to the maximum
-- session duration setting for the role. This setting can have a value
-- from 1 hour to 12 hours. If you specify a value higher than this
-- setting, the operation fails. For example, if you specify a session
-- duration of 12 hours, but your administrator set the maximum session
-- duration to 6 hours, your operation fails. To learn how to view the
-- maximum value for your role, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role>
-- in the /IAM User Guide/.
--
-- By default, the value is set to @3600@ seconds.
--
-- The @DurationSeconds@ parameter is separate from the duration of a
-- console session that you might request using the returned credentials.
-- The request to the federation endpoint for a console sign-in token takes
-- a @SessionDuration@ parameter that specifies the maximum length of the
-- console session. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-custom-url.html Creating a URL that Enables Federated Users to Access the Amazon Web Services Management Console>
-- in the /IAM User Guide/.
assumeRoleWithSAML_durationSeconds :: Lens.Lens' AssumeRoleWithSAML (Prelude.Maybe Prelude.Natural)
assumeRoleWithSAML_durationSeconds = Lens.lens (\AssumeRoleWithSAML' {durationSeconds} -> durationSeconds) (\s@AssumeRoleWithSAML' {} a -> s {durationSeconds = a} :: AssumeRoleWithSAML)

-- | An IAM policy in JSON format that you want to use as an inline session
-- policy.
--
-- This parameter is optional. Passing policies to this operation returns
-- new temporary credentials. The resulting session\'s permissions are the
-- intersection of the role\'s identity-based policy and the session
-- policies. You can use the role\'s temporary credentials in subsequent
-- Amazon Web Services API calls to access resources in the account that
-- owns the role. You cannot use session policies to grant more permissions
-- than those allowed by the identity-based policy of the role that is
-- being assumed. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- The plaintext that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters. The JSON policy characters can be any
-- ASCII character from the space character to the end of the valid
-- character list (\\u0020 through \\u00FF). It can also include the tab
-- (\\u0009), linefeed (\\u000A), and carriage return (\\u000D) characters.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
assumeRoleWithSAML_policy :: Lens.Lens' AssumeRoleWithSAML (Prelude.Maybe Prelude.Text)
assumeRoleWithSAML_policy = Lens.lens (\AssumeRoleWithSAML' {policy} -> policy) (\s@AssumeRoleWithSAML' {} a -> s {policy = a} :: AssumeRoleWithSAML)

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you
-- want to use as managed session policies. The policies must exist in the
-- same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy
-- ARNs. However, the plaintext that you use for both inline and managed
-- session policies can\'t exceed 2,048 characters. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the Amazon Web Services General Reference.
--
-- An Amazon Web Services conversion compresses the passed inline session
-- policy, managed policy ARNs, and session tags into a packed binary
-- format that has a separate limit. Your request can fail for this limit
-- even if your plaintext meets the other requirements. The
-- @PackedPolicySize@ response element indicates by percentage how close
-- the policies and tags for your request are to the upper size limit.
--
-- Passing policies to this operation returns new temporary credentials.
-- The resulting session\'s permissions are the intersection of the role\'s
-- identity-based policy and the session policies. You can use the role\'s
-- temporary credentials in subsequent Amazon Web Services API calls to
-- access resources in the account that owns the role. You cannot use
-- session policies to grant more permissions than those allowed by the
-- identity-based policy of the role that is being assumed. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
assumeRoleWithSAML_policyArns :: Lens.Lens' AssumeRoleWithSAML (Prelude.Maybe [PolicyDescriptorType])
assumeRoleWithSAML_policyArns = Lens.lens (\AssumeRoleWithSAML' {policyArns} -> policyArns) (\s@AssumeRoleWithSAML' {} a -> s {policyArns = a} :: AssumeRoleWithSAML) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
assumeRoleWithSAML_roleArn :: Lens.Lens' AssumeRoleWithSAML Prelude.Text
assumeRoleWithSAML_roleArn = Lens.lens (\AssumeRoleWithSAML' {roleArn} -> roleArn) (\s@AssumeRoleWithSAML' {} a -> s {roleArn = a} :: AssumeRoleWithSAML)

-- | The Amazon Resource Name (ARN) of the SAML provider in IAM that
-- describes the IdP.
assumeRoleWithSAML_principalArn :: Lens.Lens' AssumeRoleWithSAML Prelude.Text
assumeRoleWithSAML_principalArn = Lens.lens (\AssumeRoleWithSAML' {principalArn} -> principalArn) (\s@AssumeRoleWithSAML' {} a -> s {principalArn = a} :: AssumeRoleWithSAML)

-- | The base64 encoded SAML authentication response provided by the IdP.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/create-role-saml-IdP-tasks.html Configuring a Relying Party and Adding Claims>
-- in the /IAM User Guide/.
assumeRoleWithSAML_sAMLAssertion :: Lens.Lens' AssumeRoleWithSAML Prelude.Text
assumeRoleWithSAML_sAMLAssertion = Lens.lens (\AssumeRoleWithSAML' {sAMLAssertion} -> sAMLAssertion) (\s@AssumeRoleWithSAML' {} a -> s {sAMLAssertion = a} :: AssumeRoleWithSAML)

instance Core.AWSRequest AssumeRoleWithSAML where
  type
    AWSResponse AssumeRoleWithSAML =
      AssumeRoleWithSAMLResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AssumeRoleWithSAMLResult"
      ( \s h x ->
          AssumeRoleWithSAMLResponse'
            Prelude.<$> (x Data..@? "AssumedRoleUser")
            Prelude.<*> (x Data..@? "Audience")
            Prelude.<*> (x Data..@? "Credentials")
            Prelude.<*> (x Data..@? "Issuer")
            Prelude.<*> (x Data..@? "NameQualifier")
            Prelude.<*> (x Data..@? "PackedPolicySize")
            Prelude.<*> (x Data..@? "SourceIdentity")
            Prelude.<*> (x Data..@? "Subject")
            Prelude.<*> (x Data..@? "SubjectType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssumeRoleWithSAML where
  hashWithSalt _salt AssumeRoleWithSAML' {..} =
    _salt
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` policyArns
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` sAMLAssertion

instance Prelude.NFData AssumeRoleWithSAML where
  rnf AssumeRoleWithSAML' {..} =
    Prelude.rnf durationSeconds `Prelude.seq`
      Prelude.rnf policy `Prelude.seq`
        Prelude.rnf policyArns `Prelude.seq`
          Prelude.rnf roleArn `Prelude.seq`
            Prelude.rnf principalArn `Prelude.seq`
              Prelude.rnf sAMLAssertion

instance Data.ToHeaders AssumeRoleWithSAML where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssumeRoleWithSAML where
  toPath = Prelude.const "/"

instance Data.ToQuery AssumeRoleWithSAML where
  toQuery AssumeRoleWithSAML' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssumeRoleWithSAML" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-06-15" :: Prelude.ByteString),
        "DurationSeconds" Data.=: durationSeconds,
        "Policy" Data.=: policy,
        "PolicyArns"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> policyArns),
        "RoleArn" Data.=: roleArn,
        "PrincipalArn" Data.=: principalArn,
        "SAMLAssertion" Data.=: sAMLAssertion
      ]

-- | Contains the response to a successful AssumeRoleWithSAML request,
-- including temporary Amazon Web Services credentials that can be used to
-- make Amazon Web Services requests.
--
-- /See:/ 'newAssumeRoleWithSAMLResponse' smart constructor.
data AssumeRoleWithSAMLResponse = AssumeRoleWithSAMLResponse'
  { -- | The identifiers for the temporary security credentials that the
    -- operation returns.
    assumedRoleUser :: Prelude.Maybe AssumedRoleUser,
    -- | The value of the @Recipient@ attribute of the @SubjectConfirmationData@
    -- element of the SAML assertion.
    audience :: Prelude.Maybe Prelude.Text,
    -- | The temporary security credentials, which include an access key ID, a
    -- secret access key, and a security (or session) token.
    --
    -- The size of the security token that STS API operations return is not
    -- fixed. We strongly recommend that you make no assumptions about the
    -- maximum size.
    credentials :: Prelude.Maybe Core.AuthEnv,
    -- | The value of the @Issuer@ element of the SAML assertion.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | A hash value based on the concatenation of the following:
    --
    -- -   The @Issuer@ response value.
    --
    -- -   The Amazon Web Services account ID.
    --
    -- -   The friendly name (the last part of the ARN) of the SAML provider in
    --     IAM.
    --
    -- The combination of @NameQualifier@ and @Subject@ can be used to uniquely
    -- identify a federated user.
    --
    -- The following pseudocode shows how the hash value is calculated:
    --
    -- @BASE64 ( SHA1 ( \"https:\/\/example.com\/saml\" + \"123456789012\" + \"\/MySAMLIdP\" ) )@
    nameQualifier :: Prelude.Maybe Prelude.Text,
    -- | A percentage value that indicates the packed size of the session
    -- policies and session tags combined passed in the request. The request
    -- fails if the packed size is greater than 100 percent, which means the
    -- policies and tags exceeded the allowed space.
    packedPolicySize :: Prelude.Maybe Prelude.Natural,
    -- | The value in the @SourceIdentity@ attribute in the SAML assertion.
    --
    -- You can require users to set a source identity value when they assume a
    -- role. You do this by using the @sts:SourceIdentity@ condition key in a
    -- role trust policy. That way, actions that are taken with the role are
    -- associated with that user. After the source identity is set, the value
    -- cannot be changed. It is present in the request for all actions that are
    -- taken by the role and persists across
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts#iam-term-role-chaining chained role>
    -- sessions. You can configure your SAML identity provider to use an
    -- attribute associated with your users, like user name or email, as the
    -- source identity when calling @AssumeRoleWithSAML@. You do this by adding
    -- an attribute to the SAML assertion. For more information about using
    -- source identity, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
    -- in the /IAM User Guide/.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@-
    sourceIdentity :: Prelude.Maybe Prelude.Text,
    -- | The value of the @NameID@ element in the @Subject@ element of the SAML
    -- assertion.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The format of the name ID, as defined by the @Format@ attribute in the
    -- @NameID@ element of the SAML assertion. Typical examples of the format
    -- are @transient@ or @persistent@.
    --
    -- If the format includes the prefix
    -- @urn:oasis:names:tc:SAML:2.0:nameid-format@, that prefix is removed. For
    -- example, @urn:oasis:names:tc:SAML:2.0:nameid-format:transient@ is
    -- returned as @transient@. If the format includes any other prefix, the
    -- format is returned with no modifications.
    subjectType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeRoleWithSAMLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assumedRoleUser', 'assumeRoleWithSAMLResponse_assumedRoleUser' - The identifiers for the temporary security credentials that the
-- operation returns.
--
-- 'audience', 'assumeRoleWithSAMLResponse_audience' - The value of the @Recipient@ attribute of the @SubjectConfirmationData@
-- element of the SAML assertion.
--
-- 'credentials', 'assumeRoleWithSAMLResponse_credentials' - The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
--
-- 'issuer', 'assumeRoleWithSAMLResponse_issuer' - The value of the @Issuer@ element of the SAML assertion.
--
-- 'nameQualifier', 'assumeRoleWithSAMLResponse_nameQualifier' - A hash value based on the concatenation of the following:
--
-- -   The @Issuer@ response value.
--
-- -   The Amazon Web Services account ID.
--
-- -   The friendly name (the last part of the ARN) of the SAML provider in
--     IAM.
--
-- The combination of @NameQualifier@ and @Subject@ can be used to uniquely
-- identify a federated user.
--
-- The following pseudocode shows how the hash value is calculated:
--
-- @BASE64 ( SHA1 ( \"https:\/\/example.com\/saml\" + \"123456789012\" + \"\/MySAMLIdP\" ) )@
--
-- 'packedPolicySize', 'assumeRoleWithSAMLResponse_packedPolicySize' - A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
--
-- 'sourceIdentity', 'assumeRoleWithSAMLResponse_sourceIdentity' - The value in the @SourceIdentity@ attribute in the SAML assertion.
--
-- You can require users to set a source identity value when they assume a
-- role. You do this by using the @sts:SourceIdentity@ condition key in a
-- role trust policy. That way, actions that are taken with the role are
-- associated with that user. After the source identity is set, the value
-- cannot be changed. It is present in the request for all actions that are
-- taken by the role and persists across
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts#iam-term-role-chaining chained role>
-- sessions. You can configure your SAML identity provider to use an
-- attribute associated with your users, like user name or email, as the
-- source identity when calling @AssumeRoleWithSAML@. You do this by adding
-- an attribute to the SAML assertion. For more information about using
-- source identity, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
--
-- 'subject', 'assumeRoleWithSAMLResponse_subject' - The value of the @NameID@ element in the @Subject@ element of the SAML
-- assertion.
--
-- 'subjectType', 'assumeRoleWithSAMLResponse_subjectType' - The format of the name ID, as defined by the @Format@ attribute in the
-- @NameID@ element of the SAML assertion. Typical examples of the format
-- are @transient@ or @persistent@.
--
-- If the format includes the prefix
-- @urn:oasis:names:tc:SAML:2.0:nameid-format@, that prefix is removed. For
-- example, @urn:oasis:names:tc:SAML:2.0:nameid-format:transient@ is
-- returned as @transient@. If the format includes any other prefix, the
-- format is returned with no modifications.
--
-- 'httpStatus', 'assumeRoleWithSAMLResponse_httpStatus' - The response's http status code.
newAssumeRoleWithSAMLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssumeRoleWithSAMLResponse
newAssumeRoleWithSAMLResponse pHttpStatus_ =
  AssumeRoleWithSAMLResponse'
    { assumedRoleUser =
        Prelude.Nothing,
      audience = Prelude.Nothing,
      credentials = Prelude.Nothing,
      issuer = Prelude.Nothing,
      nameQualifier = Prelude.Nothing,
      packedPolicySize = Prelude.Nothing,
      sourceIdentity = Prelude.Nothing,
      subject = Prelude.Nothing,
      subjectType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifiers for the temporary security credentials that the
-- operation returns.
assumeRoleWithSAMLResponse_assumedRoleUser :: Lens.Lens' AssumeRoleWithSAMLResponse (Prelude.Maybe AssumedRoleUser)
assumeRoleWithSAMLResponse_assumedRoleUser = Lens.lens (\AssumeRoleWithSAMLResponse' {assumedRoleUser} -> assumedRoleUser) (\s@AssumeRoleWithSAMLResponse' {} a -> s {assumedRoleUser = a} :: AssumeRoleWithSAMLResponse)

-- | The value of the @Recipient@ attribute of the @SubjectConfirmationData@
-- element of the SAML assertion.
assumeRoleWithSAMLResponse_audience :: Lens.Lens' AssumeRoleWithSAMLResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithSAMLResponse_audience = Lens.lens (\AssumeRoleWithSAMLResponse' {audience} -> audience) (\s@AssumeRoleWithSAMLResponse' {} a -> s {audience = a} :: AssumeRoleWithSAMLResponse)

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
assumeRoleWithSAMLResponse_credentials :: Lens.Lens' AssumeRoleWithSAMLResponse (Prelude.Maybe Core.AuthEnv)
assumeRoleWithSAMLResponse_credentials = Lens.lens (\AssumeRoleWithSAMLResponse' {credentials} -> credentials) (\s@AssumeRoleWithSAMLResponse' {} a -> s {credentials = a} :: AssumeRoleWithSAMLResponse)

-- | The value of the @Issuer@ element of the SAML assertion.
assumeRoleWithSAMLResponse_issuer :: Lens.Lens' AssumeRoleWithSAMLResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithSAMLResponse_issuer = Lens.lens (\AssumeRoleWithSAMLResponse' {issuer} -> issuer) (\s@AssumeRoleWithSAMLResponse' {} a -> s {issuer = a} :: AssumeRoleWithSAMLResponse)

-- | A hash value based on the concatenation of the following:
--
-- -   The @Issuer@ response value.
--
-- -   The Amazon Web Services account ID.
--
-- -   The friendly name (the last part of the ARN) of the SAML provider in
--     IAM.
--
-- The combination of @NameQualifier@ and @Subject@ can be used to uniquely
-- identify a federated user.
--
-- The following pseudocode shows how the hash value is calculated:
--
-- @BASE64 ( SHA1 ( \"https:\/\/example.com\/saml\" + \"123456789012\" + \"\/MySAMLIdP\" ) )@
assumeRoleWithSAMLResponse_nameQualifier :: Lens.Lens' AssumeRoleWithSAMLResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithSAMLResponse_nameQualifier = Lens.lens (\AssumeRoleWithSAMLResponse' {nameQualifier} -> nameQualifier) (\s@AssumeRoleWithSAMLResponse' {} a -> s {nameQualifier = a} :: AssumeRoleWithSAMLResponse)

-- | A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
assumeRoleWithSAMLResponse_packedPolicySize :: Lens.Lens' AssumeRoleWithSAMLResponse (Prelude.Maybe Prelude.Natural)
assumeRoleWithSAMLResponse_packedPolicySize = Lens.lens (\AssumeRoleWithSAMLResponse' {packedPolicySize} -> packedPolicySize) (\s@AssumeRoleWithSAMLResponse' {} a -> s {packedPolicySize = a} :: AssumeRoleWithSAMLResponse)

-- | The value in the @SourceIdentity@ attribute in the SAML assertion.
--
-- You can require users to set a source identity value when they assume a
-- role. You do this by using the @sts:SourceIdentity@ condition key in a
-- role trust policy. That way, actions that are taken with the role are
-- associated with that user. After the source identity is set, the value
-- cannot be changed. It is present in the request for all actions that are
-- taken by the role and persists across
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts#iam-term-role-chaining chained role>
-- sessions. You can configure your SAML identity provider to use an
-- attribute associated with your users, like user name or email, as the
-- source identity when calling @AssumeRoleWithSAML@. You do this by adding
-- an attribute to the SAML assertion. For more information about using
-- source identity, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
assumeRoleWithSAMLResponse_sourceIdentity :: Lens.Lens' AssumeRoleWithSAMLResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithSAMLResponse_sourceIdentity = Lens.lens (\AssumeRoleWithSAMLResponse' {sourceIdentity} -> sourceIdentity) (\s@AssumeRoleWithSAMLResponse' {} a -> s {sourceIdentity = a} :: AssumeRoleWithSAMLResponse)

-- | The value of the @NameID@ element in the @Subject@ element of the SAML
-- assertion.
assumeRoleWithSAMLResponse_subject :: Lens.Lens' AssumeRoleWithSAMLResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithSAMLResponse_subject = Lens.lens (\AssumeRoleWithSAMLResponse' {subject} -> subject) (\s@AssumeRoleWithSAMLResponse' {} a -> s {subject = a} :: AssumeRoleWithSAMLResponse)

-- | The format of the name ID, as defined by the @Format@ attribute in the
-- @NameID@ element of the SAML assertion. Typical examples of the format
-- are @transient@ or @persistent@.
--
-- If the format includes the prefix
-- @urn:oasis:names:tc:SAML:2.0:nameid-format@, that prefix is removed. For
-- example, @urn:oasis:names:tc:SAML:2.0:nameid-format:transient@ is
-- returned as @transient@. If the format includes any other prefix, the
-- format is returned with no modifications.
assumeRoleWithSAMLResponse_subjectType :: Lens.Lens' AssumeRoleWithSAMLResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithSAMLResponse_subjectType = Lens.lens (\AssumeRoleWithSAMLResponse' {subjectType} -> subjectType) (\s@AssumeRoleWithSAMLResponse' {} a -> s {subjectType = a} :: AssumeRoleWithSAMLResponse)

-- | The response's http status code.
assumeRoleWithSAMLResponse_httpStatus :: Lens.Lens' AssumeRoleWithSAMLResponse Prelude.Int
assumeRoleWithSAMLResponse_httpStatus = Lens.lens (\AssumeRoleWithSAMLResponse' {httpStatus} -> httpStatus) (\s@AssumeRoleWithSAMLResponse' {} a -> s {httpStatus = a} :: AssumeRoleWithSAMLResponse)

instance Prelude.NFData AssumeRoleWithSAMLResponse where
  rnf AssumeRoleWithSAMLResponse' {..} =
    Prelude.rnf assumedRoleUser `Prelude.seq`
      Prelude.rnf audience `Prelude.seq`
        Prelude.rnf credentials `Prelude.seq`
          Prelude.rnf issuer `Prelude.seq`
            Prelude.rnf nameQualifier `Prelude.seq`
              Prelude.rnf packedPolicySize `Prelude.seq`
                Prelude.rnf sourceIdentity `Prelude.seq`
                  Prelude.rnf subject `Prelude.seq`
                    Prelude.rnf subjectType `Prelude.seq`
                      Prelude.rnf httpStatus
