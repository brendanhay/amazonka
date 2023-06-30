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
-- Module      : Amazonka.STS.AssumeRoleWithWebIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials for users who have been
-- authenticated in a mobile or web application with a web identity
-- provider. Example providers include the OAuth 2.0 providers Login with
-- Amazon and Facebook, or any OpenID Connect-compatible identity provider
-- such as Google or
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-identity.html Amazon Cognito federated identities>.
--
-- For mobile applications, we recommend that you use Amazon Cognito. You
-- can use Amazon Cognito with the
-- <http://aws.amazon.com/sdkforios/ Amazon Web Services SDK for iOS Developer Guide>
-- and the
-- <http://aws.amazon.com/sdkforandroid/ Amazon Web Services SDK for Android Developer Guide>
-- to uniquely identify a user. You can also supply the user with a
-- consistent identity throughout the lifetime of an application.
--
-- To learn more about Amazon Cognito, see
-- <https://docs.aws.amazon.com/mobile/sdkforandroid/developerguide/cognito-auth.html#d0e840 Amazon Cognito Overview>
-- in /Amazon Web Services SDK for Android Developer Guide/ and
-- <https://docs.aws.amazon.com/mobile/sdkforios/developerguide/cognito-auth.html#d0e664 Amazon Cognito Overview>
-- in the /Amazon Web Services SDK for iOS Developer Guide/.
--
-- Calling @AssumeRoleWithWebIdentity@ does not require the use of Amazon
-- Web Services security credentials. Therefore, you can distribute an
-- application (for example, on mobile devices) that requests temporary
-- security credentials without including long-term Amazon Web Services
-- credentials in the application. You also don\'t need to deploy
-- server-based proxy services that use long-term Amazon Web Services
-- credentials. Instead, the identity of the caller is validated by using a
-- token from the web identity provider. For a comparison of
-- @AssumeRoleWithWebIdentity@ with the other API operations that produce
-- temporary credentials, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the Amazon Web Services STS API operations>
-- in the /IAM User Guide/.
--
-- The temporary security credentials returned by this API consist of an
-- access key ID, a secret access key, and a security token. Applications
-- can use these temporary security credentials to sign calls to Amazon Web
-- Services service API operations.
--
-- __Session Duration__
--
-- By default, the temporary security credentials created by
-- @AssumeRoleWithWebIdentity@ last for one hour. However, you can use the
-- optional @DurationSeconds@ parameter to specify the duration of your
-- session. You can provide a value from 900 seconds (15 minutes) up to the
-- maximum session duration setting for the role. This setting can have a
-- value from 1 hour to 12 hours. To learn how to view the maximum value
-- for your role, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role>
-- in the /IAM User Guide/. The maximum session duration limit applies when
-- you use the @AssumeRole*@ API operations or the @assume-role*@ CLI
-- commands. However the limit does not apply when you use those operations
-- to create a console URL. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles>
-- in the /IAM User Guide/.
--
-- __Permissions__
--
-- The temporary security credentials created by
-- @AssumeRoleWithWebIdentity@ can be used to make API calls to any Amazon
-- Web Services service with the following exception: you cannot call the
-- STS @GetFederationToken@ or @GetSessionToken@ API operations.
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
-- __Tags__
--
-- (Optional) You can configure your IdP to pass attributes into your web
-- identity token as session tags. Each session tag consists of a key name
-- and an associated value. For more information about session tags, see
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
-- to the role. When you do, the session tag overrides the role tag with
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
-- __Identities__
--
-- Before your application can call @AssumeRoleWithWebIdentity@, you must
-- have an identity token from a supported identity provider and create a
-- role that the application can assume. The role that your application
-- assumes must trust the identity provider that is associated with the
-- identity token. In other words, the identity provider must be specified
-- in the role\'s trust policy.
--
-- Calling @AssumeRoleWithWebIdentity@ can result in an entry in your
-- CloudTrail logs. The entry includes the
-- <http://openid.net/specs/openid-connect-core-1_0.html#Claims Subject> of
-- the provided web identity token. We recommend that you avoid using any
-- personally identifiable information (PII) in this field. For example,
-- you could instead use a GUID or a pairwise identifier, as
-- <http://openid.net/specs/openid-connect-core-1_0.html#SubjectIDTypes suggested in the OIDC specification>.
--
-- For more information about how to use web identity federation and the
-- @AssumeRoleWithWebIdentity@ API, see the following resources:
--
-- -   <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_oidc_manual.html Using Web Identity Federation API Operations for Mobile Apps>
--     and
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#api_assumerolewithwebidentity Federation Through a Web-based Identity Provider>.
--
-- -   <https://aws.amazon.com/blogs/aws/the-aws-web-identity-federation-playground/ Web Identity Federation Playground>.
--     Walk through the process of authenticating through Login with
--     Amazon, Facebook, or Google, getting temporary security credentials,
--     and then using those credentials to make a request to Amazon Web
--     Services.
--
-- -   <http://aws.amazon.com/sdkforios/ Amazon Web Services SDK for iOS Developer Guide>
--     and
--     <http://aws.amazon.com/sdkforandroid/ Amazon Web Services SDK for Android Developer Guide>.
--     These toolkits contain sample apps that show how to invoke the
--     identity providers. The toolkits then show how to use the
--     information from these providers to get and use temporary security
--     credentials.
--
-- -   <http://aws.amazon.com/articles/web-identity-federation-with-mobile-applications Web Identity Federation with Mobile Applications>.
--     This article discusses web identity federation and shows an example
--     of how to use web identity federation to get access to content in
--     Amazon S3.
module Amazonka.STS.AssumeRoleWithWebIdentity
  ( -- * Creating a Request
    AssumeRoleWithWebIdentity (..),
    newAssumeRoleWithWebIdentity,

    -- * Request Lenses
    assumeRoleWithWebIdentity_durationSeconds,
    assumeRoleWithWebIdentity_policy,
    assumeRoleWithWebIdentity_policyArns,
    assumeRoleWithWebIdentity_providerId,
    assumeRoleWithWebIdentity_roleArn,
    assumeRoleWithWebIdentity_roleSessionName,
    assumeRoleWithWebIdentity_webIdentityToken,

    -- * Destructuring the Response
    AssumeRoleWithWebIdentityResponse (..),
    newAssumeRoleWithWebIdentityResponse,

    -- * Response Lenses
    assumeRoleWithWebIdentityResponse_assumedRoleUser,
    assumeRoleWithWebIdentityResponse_audience,
    assumeRoleWithWebIdentityResponse_packedPolicySize,
    assumeRoleWithWebIdentityResponse_provider,
    assumeRoleWithWebIdentityResponse_sourceIdentity,
    assumeRoleWithWebIdentityResponse_subjectFromWebIdentityToken,
    assumeRoleWithWebIdentityResponse_httpStatus,
    assumeRoleWithWebIdentityResponse_credentials,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.STS.Types

-- | /See:/ 'newAssumeRoleWithWebIdentity' smart constructor.
data AssumeRoleWithWebIdentity = AssumeRoleWithWebIdentity'
  { -- | The duration, in seconds, of the role session. The value can range from
    -- 900 seconds (15 minutes) up to the maximum session duration setting for
    -- the role. This setting can have a value from 1 hour to 12 hours. If you
    -- specify a value higher than this setting, the operation fails. For
    -- example, if you specify a session duration of 12 hours, but your
    -- administrator set the maximum session duration to 6 hours, your
    -- operation fails. To learn how to view the maximum value for your role,
    -- see
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
    -- | The fully qualified host component of the domain name of the OAuth 2.0
    -- identity provider. Do not specify this value for an OpenID Connect
    -- identity provider.
    --
    -- Currently @www.amazon.com@ and @graph.facebook.com@ are the only
    -- supported identity providers for OAuth 2.0 access tokens. Do not include
    -- URL schemes and port numbers.
    --
    -- Do not specify this value for OpenID Connect ID tokens.
    providerId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
    roleArn :: Prelude.Text,
    -- | An identifier for the assumed role session. Typically, you pass the name
    -- or identifier that is associated with the user who is using your
    -- application. That way, the temporary security credentials that your
    -- application will use are associated with that user. This session name is
    -- included as part of the ARN and assumed role ID in the @AssumedRoleUser@
    -- response element.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@-
    roleSessionName :: Prelude.Text,
    -- | The OAuth 2.0 access token or OpenID Connect ID token that is provided
    -- by the identity provider. Your application must get this token by
    -- authenticating the user who is using your application with a web
    -- identity provider before the application makes an
    -- @AssumeRoleWithWebIdentity@ call.
    webIdentityToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeRoleWithWebIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationSeconds', 'assumeRoleWithWebIdentity_durationSeconds' - The duration, in seconds, of the role session. The value can range from
-- 900 seconds (15 minutes) up to the maximum session duration setting for
-- the role. This setting can have a value from 1 hour to 12 hours. If you
-- specify a value higher than this setting, the operation fails. For
-- example, if you specify a session duration of 12 hours, but your
-- administrator set the maximum session duration to 6 hours, your
-- operation fails. To learn how to view the maximum value for your role,
-- see
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
-- 'policy', 'assumeRoleWithWebIdentity_policy' - An IAM policy in JSON format that you want to use as an inline session
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
-- 'policyArns', 'assumeRoleWithWebIdentity_policyArns' - The Amazon Resource Names (ARNs) of the IAM managed policies that you
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
-- 'providerId', 'assumeRoleWithWebIdentity_providerId' - The fully qualified host component of the domain name of the OAuth 2.0
-- identity provider. Do not specify this value for an OpenID Connect
-- identity provider.
--
-- Currently @www.amazon.com@ and @graph.facebook.com@ are the only
-- supported identity providers for OAuth 2.0 access tokens. Do not include
-- URL schemes and port numbers.
--
-- Do not specify this value for OpenID Connect ID tokens.
--
-- 'roleArn', 'assumeRoleWithWebIdentity_roleArn' - The Amazon Resource Name (ARN) of the role that the caller is assuming.
--
-- 'roleSessionName', 'assumeRoleWithWebIdentity_roleSessionName' - An identifier for the assumed role session. Typically, you pass the name
-- or identifier that is associated with the user who is using your
-- application. That way, the temporary security credentials that your
-- application will use are associated with that user. This session name is
-- included as part of the ARN and assumed role ID in the @AssumedRoleUser@
-- response element.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
--
-- 'webIdentityToken', 'assumeRoleWithWebIdentity_webIdentityToken' - The OAuth 2.0 access token or OpenID Connect ID token that is provided
-- by the identity provider. Your application must get this token by
-- authenticating the user who is using your application with a web
-- identity provider before the application makes an
-- @AssumeRoleWithWebIdentity@ call.
newAssumeRoleWithWebIdentity ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'roleSessionName'
  Prelude.Text ->
  -- | 'webIdentityToken'
  Prelude.Text ->
  AssumeRoleWithWebIdentity
newAssumeRoleWithWebIdentity
  pRoleArn_
  pRoleSessionName_
  pWebIdentityToken_ =
    AssumeRoleWithWebIdentity'
      { durationSeconds =
          Prelude.Nothing,
        policy = Prelude.Nothing,
        policyArns = Prelude.Nothing,
        providerId = Prelude.Nothing,
        roleArn = pRoleArn_,
        roleSessionName = pRoleSessionName_,
        webIdentityToken = pWebIdentityToken_
      }

-- | The duration, in seconds, of the role session. The value can range from
-- 900 seconds (15 minutes) up to the maximum session duration setting for
-- the role. This setting can have a value from 1 hour to 12 hours. If you
-- specify a value higher than this setting, the operation fails. For
-- example, if you specify a session duration of 12 hours, but your
-- administrator set the maximum session duration to 6 hours, your
-- operation fails. To learn how to view the maximum value for your role,
-- see
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
assumeRoleWithWebIdentity_durationSeconds :: Lens.Lens' AssumeRoleWithWebIdentity (Prelude.Maybe Prelude.Natural)
assumeRoleWithWebIdentity_durationSeconds = Lens.lens (\AssumeRoleWithWebIdentity' {durationSeconds} -> durationSeconds) (\s@AssumeRoleWithWebIdentity' {} a -> s {durationSeconds = a} :: AssumeRoleWithWebIdentity)

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
assumeRoleWithWebIdentity_policy :: Lens.Lens' AssumeRoleWithWebIdentity (Prelude.Maybe Prelude.Text)
assumeRoleWithWebIdentity_policy = Lens.lens (\AssumeRoleWithWebIdentity' {policy} -> policy) (\s@AssumeRoleWithWebIdentity' {} a -> s {policy = a} :: AssumeRoleWithWebIdentity)

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
assumeRoleWithWebIdentity_policyArns :: Lens.Lens' AssumeRoleWithWebIdentity (Prelude.Maybe [PolicyDescriptorType])
assumeRoleWithWebIdentity_policyArns = Lens.lens (\AssumeRoleWithWebIdentity' {policyArns} -> policyArns) (\s@AssumeRoleWithWebIdentity' {} a -> s {policyArns = a} :: AssumeRoleWithWebIdentity) Prelude.. Lens.mapping Lens.coerced

-- | The fully qualified host component of the domain name of the OAuth 2.0
-- identity provider. Do not specify this value for an OpenID Connect
-- identity provider.
--
-- Currently @www.amazon.com@ and @graph.facebook.com@ are the only
-- supported identity providers for OAuth 2.0 access tokens. Do not include
-- URL schemes and port numbers.
--
-- Do not specify this value for OpenID Connect ID tokens.
assumeRoleWithWebIdentity_providerId :: Lens.Lens' AssumeRoleWithWebIdentity (Prelude.Maybe Prelude.Text)
assumeRoleWithWebIdentity_providerId = Lens.lens (\AssumeRoleWithWebIdentity' {providerId} -> providerId) (\s@AssumeRoleWithWebIdentity' {} a -> s {providerId = a} :: AssumeRoleWithWebIdentity)

-- | The Amazon Resource Name (ARN) of the role that the caller is assuming.
assumeRoleWithWebIdentity_roleArn :: Lens.Lens' AssumeRoleWithWebIdentity Prelude.Text
assumeRoleWithWebIdentity_roleArn = Lens.lens (\AssumeRoleWithWebIdentity' {roleArn} -> roleArn) (\s@AssumeRoleWithWebIdentity' {} a -> s {roleArn = a} :: AssumeRoleWithWebIdentity)

-- | An identifier for the assumed role session. Typically, you pass the name
-- or identifier that is associated with the user who is using your
-- application. That way, the temporary security credentials that your
-- application will use are associated with that user. This session name is
-- included as part of the ARN and assumed role ID in the @AssumedRoleUser@
-- response element.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
assumeRoleWithWebIdentity_roleSessionName :: Lens.Lens' AssumeRoleWithWebIdentity Prelude.Text
assumeRoleWithWebIdentity_roleSessionName = Lens.lens (\AssumeRoleWithWebIdentity' {roleSessionName} -> roleSessionName) (\s@AssumeRoleWithWebIdentity' {} a -> s {roleSessionName = a} :: AssumeRoleWithWebIdentity)

-- | The OAuth 2.0 access token or OpenID Connect ID token that is provided
-- by the identity provider. Your application must get this token by
-- authenticating the user who is using your application with a web
-- identity provider before the application makes an
-- @AssumeRoleWithWebIdentity@ call.
assumeRoleWithWebIdentity_webIdentityToken :: Lens.Lens' AssumeRoleWithWebIdentity Prelude.Text
assumeRoleWithWebIdentity_webIdentityToken = Lens.lens (\AssumeRoleWithWebIdentity' {webIdentityToken} -> webIdentityToken) (\s@AssumeRoleWithWebIdentity' {} a -> s {webIdentityToken = a} :: AssumeRoleWithWebIdentity)

instance Core.AWSRequest AssumeRoleWithWebIdentity where
  type
    AWSResponse AssumeRoleWithWebIdentity =
      AssumeRoleWithWebIdentityResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AssumeRoleWithWebIdentityResult"
      ( \s h x ->
          AssumeRoleWithWebIdentityResponse'
            Prelude.<$> (x Data..@? "AssumedRoleUser")
            Prelude.<*> (x Data..@? "Audience")
            Prelude.<*> (x Data..@? "PackedPolicySize")
            Prelude.<*> (x Data..@? "Provider")
            Prelude.<*> (x Data..@? "SourceIdentity")
            Prelude.<*> (x Data..@? "SubjectFromWebIdentityToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Credentials")
      )

instance Prelude.Hashable AssumeRoleWithWebIdentity where
  hashWithSalt _salt AssumeRoleWithWebIdentity' {..} =
    _salt
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` policyArns
      `Prelude.hashWithSalt` providerId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` roleSessionName
      `Prelude.hashWithSalt` webIdentityToken

instance Prelude.NFData AssumeRoleWithWebIdentity where
  rnf AssumeRoleWithWebIdentity' {..} =
    Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf policyArns
      `Prelude.seq` Prelude.rnf providerId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf roleSessionName
      `Prelude.seq` Prelude.rnf webIdentityToken

instance Data.ToHeaders AssumeRoleWithWebIdentity where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssumeRoleWithWebIdentity where
  toPath = Prelude.const "/"

instance Data.ToQuery AssumeRoleWithWebIdentity where
  toQuery AssumeRoleWithWebIdentity' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssumeRoleWithWebIdentity" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-06-15" :: Prelude.ByteString),
        "DurationSeconds" Data.=: durationSeconds,
        "Policy" Data.=: policy,
        "PolicyArns"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> policyArns),
        "ProviderId" Data.=: providerId,
        "RoleArn" Data.=: roleArn,
        "RoleSessionName" Data.=: roleSessionName,
        "WebIdentityToken" Data.=: webIdentityToken
      ]

-- | Contains the response to a successful AssumeRoleWithWebIdentity request,
-- including temporary Amazon Web Services credentials that can be used to
-- make Amazon Web Services requests.
--
-- /See:/ 'newAssumeRoleWithWebIdentityResponse' smart constructor.
data AssumeRoleWithWebIdentityResponse = AssumeRoleWithWebIdentityResponse'
  { -- | The Amazon Resource Name (ARN) and the assumed role ID, which are
    -- identifiers that you can use to refer to the resulting temporary
    -- security credentials. For example, you can reference these credentials
    -- as a principal in a resource-based policy by using the ARN or assumed
    -- role ID. The ARN and ID include the @RoleSessionName@ that you specified
    -- when you called @AssumeRole@.
    assumedRoleUser :: Prelude.Maybe AssumedRoleUser,
    -- | The intended audience (also known as client ID) of the web identity
    -- token. This is traditionally the client identifier issued to the
    -- application that requested the web identity token.
    audience :: Prelude.Maybe Prelude.Text,
    -- | A percentage value that indicates the packed size of the session
    -- policies and session tags combined passed in the request. The request
    -- fails if the packed size is greater than 100 percent, which means the
    -- policies and tags exceeded the allowed space.
    packedPolicySize :: Prelude.Maybe Prelude.Natural,
    -- | The issuing authority of the web identity token presented. For OpenID
    -- Connect ID tokens, this contains the value of the @iss@ field. For OAuth
    -- 2.0 access tokens, this contains the value of the @ProviderId@ parameter
    -- that was passed in the @AssumeRoleWithWebIdentity@ request.
    provider :: Prelude.Maybe Prelude.Text,
    -- | The value of the source identity that is returned in the JSON web token
    -- (JWT) from the identity provider.
    --
    -- You can require users to set a source identity value when they assume a
    -- role. You do this by using the @sts:SourceIdentity@ condition key in a
    -- role trust policy. That way, actions that are taken with the role are
    -- associated with that user. After the source identity is set, the value
    -- cannot be changed. It is present in the request for all actions that are
    -- taken by the role and persists across
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts#iam-term-role-chaining chained role>
    -- sessions. You can configure your identity provider to use an attribute
    -- associated with your users, like user name or email, as the source
    -- identity when calling @AssumeRoleWithWebIdentity@. You do this by adding
    -- a claim to the JSON web token. To learn more about OIDC tokens and
    -- claims, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/amazon-cognito-user-pools-using-tokens-with-identity-providers.html Using Tokens with User Pools>
    -- in the /Amazon Cognito Developer Guide/. For more information about
    -- using source identity, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
    -- in the /IAM User Guide/.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@-
    sourceIdentity :: Prelude.Maybe Prelude.Text,
    -- | The unique user identifier that is returned by the identity provider.
    -- This identifier is associated with the @WebIdentityToken@ that was
    -- submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is
    -- typically unique to the user and the application that acquired the
    -- @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens,
    -- this field contains the value returned by the identity provider as the
    -- token\'s @sub@ (Subject) claim.
    subjectFromWebIdentityToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The temporary security credentials, which include an access key ID, a
    -- secret access key, and a security token.
    --
    -- The size of the security token that STS API operations return is not
    -- fixed. We strongly recommend that you make no assumptions about the
    -- maximum size.
    credentials :: Core.AuthEnv
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeRoleWithWebIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assumedRoleUser', 'assumeRoleWithWebIdentityResponse_assumedRoleUser' - The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary
-- security credentials. For example, you can reference these credentials
-- as a principal in a resource-based policy by using the ARN or assumed
-- role ID. The ARN and ID include the @RoleSessionName@ that you specified
-- when you called @AssumeRole@.
--
-- 'audience', 'assumeRoleWithWebIdentityResponse_audience' - The intended audience (also known as client ID) of the web identity
-- token. This is traditionally the client identifier issued to the
-- application that requested the web identity token.
--
-- 'packedPolicySize', 'assumeRoleWithWebIdentityResponse_packedPolicySize' - A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
--
-- 'provider', 'assumeRoleWithWebIdentityResponse_provider' - The issuing authority of the web identity token presented. For OpenID
-- Connect ID tokens, this contains the value of the @iss@ field. For OAuth
-- 2.0 access tokens, this contains the value of the @ProviderId@ parameter
-- that was passed in the @AssumeRoleWithWebIdentity@ request.
--
-- 'sourceIdentity', 'assumeRoleWithWebIdentityResponse_sourceIdentity' - The value of the source identity that is returned in the JSON web token
-- (JWT) from the identity provider.
--
-- You can require users to set a source identity value when they assume a
-- role. You do this by using the @sts:SourceIdentity@ condition key in a
-- role trust policy. That way, actions that are taken with the role are
-- associated with that user. After the source identity is set, the value
-- cannot be changed. It is present in the request for all actions that are
-- taken by the role and persists across
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts#iam-term-role-chaining chained role>
-- sessions. You can configure your identity provider to use an attribute
-- associated with your users, like user name or email, as the source
-- identity when calling @AssumeRoleWithWebIdentity@. You do this by adding
-- a claim to the JSON web token. To learn more about OIDC tokens and
-- claims, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/amazon-cognito-user-pools-using-tokens-with-identity-providers.html Using Tokens with User Pools>
-- in the /Amazon Cognito Developer Guide/. For more information about
-- using source identity, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
--
-- 'subjectFromWebIdentityToken', 'assumeRoleWithWebIdentityResponse_subjectFromWebIdentityToken' - The unique user identifier that is returned by the identity provider.
-- This identifier is associated with the @WebIdentityToken@ that was
-- submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is
-- typically unique to the user and the application that acquired the
-- @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens,
-- this field contains the value returned by the identity provider as the
-- token\'s @sub@ (Subject) claim.
--
-- 'httpStatus', 'assumeRoleWithWebIdentityResponse_httpStatus' - The response's http status code.
--
-- 'credentials', 'assumeRoleWithWebIdentityResponse_credentials' - The temporary security credentials, which include an access key ID, a
-- secret access key, and a security token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
newAssumeRoleWithWebIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'credentials'
  Core.AuthEnv ->
  AssumeRoleWithWebIdentityResponse
newAssumeRoleWithWebIdentityResponse
  pHttpStatus_
  pCredentials_ =
    AssumeRoleWithWebIdentityResponse'
      { assumedRoleUser =
          Prelude.Nothing,
        audience = Prelude.Nothing,
        packedPolicySize = Prelude.Nothing,
        provider = Prelude.Nothing,
        sourceIdentity = Prelude.Nothing,
        subjectFromWebIdentityToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        credentials = pCredentials_
      }

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary
-- security credentials. For example, you can reference these credentials
-- as a principal in a resource-based policy by using the ARN or assumed
-- role ID. The ARN and ID include the @RoleSessionName@ that you specified
-- when you called @AssumeRole@.
assumeRoleWithWebIdentityResponse_assumedRoleUser :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Prelude.Maybe AssumedRoleUser)
assumeRoleWithWebIdentityResponse_assumedRoleUser = Lens.lens (\AssumeRoleWithWebIdentityResponse' {assumedRoleUser} -> assumedRoleUser) (\s@AssumeRoleWithWebIdentityResponse' {} a -> s {assumedRoleUser = a} :: AssumeRoleWithWebIdentityResponse)

-- | The intended audience (also known as client ID) of the web identity
-- token. This is traditionally the client identifier issued to the
-- application that requested the web identity token.
assumeRoleWithWebIdentityResponse_audience :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithWebIdentityResponse_audience = Lens.lens (\AssumeRoleWithWebIdentityResponse' {audience} -> audience) (\s@AssumeRoleWithWebIdentityResponse' {} a -> s {audience = a} :: AssumeRoleWithWebIdentityResponse)

-- | A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
assumeRoleWithWebIdentityResponse_packedPolicySize :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Prelude.Maybe Prelude.Natural)
assumeRoleWithWebIdentityResponse_packedPolicySize = Lens.lens (\AssumeRoleWithWebIdentityResponse' {packedPolicySize} -> packedPolicySize) (\s@AssumeRoleWithWebIdentityResponse' {} a -> s {packedPolicySize = a} :: AssumeRoleWithWebIdentityResponse)

-- | The issuing authority of the web identity token presented. For OpenID
-- Connect ID tokens, this contains the value of the @iss@ field. For OAuth
-- 2.0 access tokens, this contains the value of the @ProviderId@ parameter
-- that was passed in the @AssumeRoleWithWebIdentity@ request.
assumeRoleWithWebIdentityResponse_provider :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithWebIdentityResponse_provider = Lens.lens (\AssumeRoleWithWebIdentityResponse' {provider} -> provider) (\s@AssumeRoleWithWebIdentityResponse' {} a -> s {provider = a} :: AssumeRoleWithWebIdentityResponse)

-- | The value of the source identity that is returned in the JSON web token
-- (JWT) from the identity provider.
--
-- You can require users to set a source identity value when they assume a
-- role. You do this by using the @sts:SourceIdentity@ condition key in a
-- role trust policy. That way, actions that are taken with the role are
-- associated with that user. After the source identity is set, the value
-- cannot be changed. It is present in the request for all actions that are
-- taken by the role and persists across
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts#iam-term-role-chaining chained role>
-- sessions. You can configure your identity provider to use an attribute
-- associated with your users, like user name or email, as the source
-- identity when calling @AssumeRoleWithWebIdentity@. You do this by adding
-- a claim to the JSON web token. To learn more about OIDC tokens and
-- claims, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/amazon-cognito-user-pools-using-tokens-with-identity-providers.html Using Tokens with User Pools>
-- in the /Amazon Cognito Developer Guide/. For more information about
-- using source identity, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
assumeRoleWithWebIdentityResponse_sourceIdentity :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithWebIdentityResponse_sourceIdentity = Lens.lens (\AssumeRoleWithWebIdentityResponse' {sourceIdentity} -> sourceIdentity) (\s@AssumeRoleWithWebIdentityResponse' {} a -> s {sourceIdentity = a} :: AssumeRoleWithWebIdentityResponse)

-- | The unique user identifier that is returned by the identity provider.
-- This identifier is associated with the @WebIdentityToken@ that was
-- submitted with the @AssumeRoleWithWebIdentity@ call. The identifier is
-- typically unique to the user and the application that acquired the
-- @WebIdentityToken@ (pairwise identifier). For OpenID Connect ID tokens,
-- this field contains the value returned by the identity provider as the
-- token\'s @sub@ (Subject) claim.
assumeRoleWithWebIdentityResponse_subjectFromWebIdentityToken :: Lens.Lens' AssumeRoleWithWebIdentityResponse (Prelude.Maybe Prelude.Text)
assumeRoleWithWebIdentityResponse_subjectFromWebIdentityToken = Lens.lens (\AssumeRoleWithWebIdentityResponse' {subjectFromWebIdentityToken} -> subjectFromWebIdentityToken) (\s@AssumeRoleWithWebIdentityResponse' {} a -> s {subjectFromWebIdentityToken = a} :: AssumeRoleWithWebIdentityResponse)

-- | The response's http status code.
assumeRoleWithWebIdentityResponse_httpStatus :: Lens.Lens' AssumeRoleWithWebIdentityResponse Prelude.Int
assumeRoleWithWebIdentityResponse_httpStatus = Lens.lens (\AssumeRoleWithWebIdentityResponse' {httpStatus} -> httpStatus) (\s@AssumeRoleWithWebIdentityResponse' {} a -> s {httpStatus = a} :: AssumeRoleWithWebIdentityResponse)

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
assumeRoleWithWebIdentityResponse_credentials :: Lens.Lens' AssumeRoleWithWebIdentityResponse Core.AuthEnv
assumeRoleWithWebIdentityResponse_credentials = Lens.lens (\AssumeRoleWithWebIdentityResponse' {credentials} -> credentials) (\s@AssumeRoleWithWebIdentityResponse' {} a -> s {credentials = a} :: AssumeRoleWithWebIdentityResponse)

instance
  Prelude.NFData
    AssumeRoleWithWebIdentityResponse
  where
  rnf AssumeRoleWithWebIdentityResponse' {..} =
    Prelude.rnf assumedRoleUser
      `Prelude.seq` Prelude.rnf audience
      `Prelude.seq` Prelude.rnf packedPolicySize
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf sourceIdentity
      `Prelude.seq` Prelude.rnf subjectFromWebIdentityToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf credentials
