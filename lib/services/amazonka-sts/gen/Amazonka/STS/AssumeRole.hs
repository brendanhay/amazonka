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
-- Module      : Amazonka.STS.AssumeRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials that you can use to
-- access Amazon Web Services resources that you might not normally have
-- access to. These temporary credentials consist of an access key ID, a
-- secret access key, and a security token. Typically, you use @AssumeRole@
-- within your account or for cross-account access. For a comparison of
-- @AssumeRole@ with other API operations that produce temporary
-- credentials, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the STS API operations>
-- in the /IAM User Guide/.
--
-- __Permissions__
--
-- The temporary security credentials created by @AssumeRole@ can be used
-- to make API calls to any Amazon Web Services service with the following
-- exception: You cannot call the STS @GetFederationToken@ or
-- @GetSessionToken@ API operations.
--
-- (Optional) You can pass inline or managed
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policies>
-- to this operation. You can pass a single JSON policy document to use as
-- an inline session policy. You can also specify up to 10 managed policies
-- to use as managed session policies. The plaintext that you use for both
-- inline and managed session policies can\'t exceed 2,048 characters.
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
-- To assume a role from a different account, your account must be trusted
-- by the role. The trust relationship is defined in the role\'s trust
-- policy when the role is created. That trust policy states which accounts
-- are allowed to delegate that access to users in the account.
--
-- A user who wants to access a role in a different account must also have
-- permissions that are delegated from the user account administrator. The
-- administrator must attach a policy that allows the user to call
-- @AssumeRole@ for the ARN of the role in the other account. If the user
-- is in the same account as the role, then you can do either of the
-- following:
--
-- -   Attach a policy to the user (identical to the previous user in a
--     different account).
--
-- -   Add the user as a principal directly in the role\'s trust policy.
--
-- In this case, the trust policy acts as an IAM resource-based policy.
-- Users in the same account as the role do not need explicit permission to
-- assume the role. For more information about trust policies and
-- resource-based policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies>
-- in the /IAM User Guide/.
--
-- __Tags__
--
-- (Optional) You can pass tag key-value pairs to your session. These tags
-- are called session tags. For more information about session tags, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS>
-- in the /IAM User Guide/.
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
-- __Using MFA with AssumeRole__
--
-- (Optional) You can include multi-factor authentication (MFA) information
-- when you call @AssumeRole@. This is useful for cross-account scenarios
-- to ensure that the user that assumes the role has been authenticated
-- with an Amazon Web Services MFA device. In that scenario, the trust
-- policy of the role being assumed includes a condition that tests for MFA
-- authentication. If the caller does not include valid MFA information,
-- the request to assume the role is denied. The condition in a trust
-- policy that tests for MFA authentication might look like the following
-- example.
--
-- @\"Condition\": {\"Bool\": {\"aws:MultiFactorAuthPresent\": true}}@
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/MFAProtectedAPI.html Configuring MFA-Protected API Access>
-- in the /IAM User Guide/ guide.
--
-- To use MFA with @AssumeRole@, you pass values for the @SerialNumber@ and
-- @TokenCode@ parameters. The @SerialNumber@ value identifies the user\'s
-- hardware or virtual MFA device. The @TokenCode@ is the time-based
-- one-time password (TOTP) that the MFA device produces.
module Amazonka.STS.AssumeRole
  ( -- * Creating a Request
    AssumeRole (..),
    newAssumeRole,

    -- * Request Lenses
    assumeRole_transitiveTagKeys,
    assumeRole_tokenCode,
    assumeRole_policyArns,
    assumeRole_durationSeconds,
    assumeRole_policy,
    assumeRole_externalId,
    assumeRole_sourceIdentity,
    assumeRole_serialNumber,
    assumeRole_tags,
    assumeRole_roleArn,
    assumeRole_roleSessionName,

    -- * Destructuring the Response
    AssumeRoleResponse (..),
    newAssumeRoleResponse,

    -- * Response Lenses
    assumeRoleResponse_packedPolicySize,
    assumeRoleResponse_credentials,
    assumeRoleResponse_assumedRoleUser,
    assumeRoleResponse_sourceIdentity,
    assumeRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.STS.Types

-- | /See:/ 'newAssumeRole' smart constructor.
data AssumeRole = AssumeRole'
  { -- | A list of keys for session tags that you want to set as transitive. If
    -- you set a tag key as transitive, the corresponding key and value passes
    -- to subsequent sessions in a role chain. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. When you set session tags as transitive, the
    -- session policy and session tags packed binary limit is not affected.
    --
    -- If you choose not to specify a transitive tag key, then no tags are
    -- passed from this session to any subsequent sessions.
    transitiveTagKeys :: Prelude.Maybe [Prelude.Text],
    -- | The value provided by the MFA device, if the trust policy of the role
    -- being assumed requires MFA. (In other words, if the policy includes a
    -- condition that tests for MFA). If the role being assumed requires MFA
    -- and if the @TokenCode@ value is missing or expired, the @AssumeRole@
    -- call returns an \"access denied\" error.
    --
    -- The format for this parameter, as described by its regex pattern, is a
    -- sequence of six numeric digits.
    tokenCode :: Prelude.Maybe Prelude.Text,
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
    -- An Amazon Web Services conversion compresses the passed session policies
    -- and session tags into a packed binary format that has a separate limit.
    -- Your request can fail for this limit even if your plaintext meets the
    -- other requirements. The @PackedPolicySize@ response element indicates by
    -- percentage how close the policies and tags for your request are to the
    -- upper size limit.
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
    -- | The duration, in seconds, of the role session. The value specified can
    -- can range from 900 seconds (15 minutes) up to the maximum session
    -- duration that is set for the role. The maximum session duration setting
    -- can have a value from 1 hour to 12 hours. If you specify a value higher
    -- than this setting or the administrator setting (whichever is lower), the
    -- operation fails. For example, if you specify a session duration of 12
    -- hours, but your administrator set the maximum session duration to 6
    -- hours, your operation fails. To learn how to view the maximum value for
    -- your role, see
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
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-custom-url.html Creating a URL that Enables Federated Users to Access the Management Console>
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
    -- An Amazon Web Services conversion compresses the passed session policies
    -- and session tags into a packed binary format that has a separate limit.
    -- Your request can fail for this limit even if your plaintext meets the
    -- other requirements. The @PackedPolicySize@ response element indicates by
    -- percentage how close the policies and tags for your request are to the
    -- upper size limit.
    policy :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that might be required when you assume a role in
    -- another account. If the administrator of the account to which the role
    -- belongs provided you with an external ID, then provide that value in the
    -- @ExternalId@ parameter. This value can be any string, such as a
    -- passphrase or account number. A cross-account role is usually set up to
    -- trust everyone in an account. Therefore, the administrator of the
    -- trusting account might send an external ID to the administrator of the
    -- trusted account. That way, only someone with the ID can assume the role,
    -- rather than everyone in the account. For more information about the
    -- external ID, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your Amazon Web Services Resources to a Third Party>
    -- in the /IAM User Guide/.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@:\/-
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The source identity specified by the principal that is calling the
    -- @AssumeRole@ operation.
    --
    -- You can require users to specify a source identity when they assume a
    -- role. You do this by using the @sts:SourceIdentity@ condition key in a
    -- role trust policy. You can use source identity information in CloudTrail
    -- logs to determine who took actions with a role. You can use the
    -- @aws:SourceIdentity@ condition key to further control access to Amazon
    -- Web Services resources based on the value of source identity. For more
    -- information about using source identity, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
    -- in the /IAM User Guide/.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@-. You cannot use a value that begins with the text
    -- @aws:@. This prefix is reserved for Amazon Web Services internal use.
    sourceIdentity :: Prelude.Maybe Prelude.Text,
    -- | The identification number of the MFA device that is associated with the
    -- user who is making the @AssumeRole@ call. Specify this value if the
    -- trust policy of the role being assumed includes a condition that
    -- requires MFA authentication. The value is either the serial number for a
    -- hardware device (such as @GAHT12345678@) or an Amazon Resource Name
    -- (ARN) for a virtual device (such as
    -- @arn:aws:iam::123456789012:mfa\/user@).
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@-
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | A list of session tags that you want to pass. Each session tag consists
    -- of a key name and an associated value. For more information about
    -- session tags, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging STS Sessions>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. You can pass up to 50 session tags. The
    -- plaintext session tag keys can’t exceed 128 characters, and the values
    -- can’t exceed 256 characters. For these and additional limits, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
    -- in the /IAM User Guide/.
    --
    -- An Amazon Web Services conversion compresses the passed session policies
    -- and session tags into a packed binary format that has a separate limit.
    -- Your request can fail for this limit even if your plaintext meets the
    -- other requirements. The @PackedPolicySize@ response element indicates by
    -- percentage how close the policies and tags for your request are to the
    -- upper size limit.
    --
    -- You can pass a session tag with the same key as a tag that is already
    -- attached to the role. When you do, session tags override a role tag with
    -- the same key.
    --
    -- Tag key–value pairs are not case sensitive, but case is preserved. This
    -- means that you cannot have separate @Department@ and @department@ tag
    -- keys. Assume that the role has the @Department@=@Marketing@ tag and you
    -- pass the @department@=@engineering@ session tag. @Department@ and
    -- @department@ are not saved as separate tags, and the session tag passed
    -- in the request takes precedence over the role tag.
    --
    -- Additionally, if you used temporary credentials to perform this
    -- operation, the new session inherits any transitive session tags from the
    -- calling session. If you pass a session tag with the same key as an
    -- inherited tag, the operation fails. To view the inherited tags for a
    -- session, see the CloudTrail logs. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the role to assume.
    roleArn :: Prelude.Text,
    -- | An identifier for the assumed role session.
    --
    -- Use the role session name to uniquely identify a session when the same
    -- role is assumed by different principals or for different reasons. In
    -- cross-account scenarios, the role session name is visible to, and can be
    -- logged by the account that owns the role. The role session name is also
    -- used in the ARN of the assumed role principal. This means that
    -- subsequent cross-account API requests that use the temporary security
    -- credentials will expose the role session name to the external account in
    -- their CloudTrail logs.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@-
    roleSessionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitiveTagKeys', 'assumeRole_transitiveTagKeys' - A list of keys for session tags that you want to set as transitive. If
-- you set a tag key as transitive, the corresponding key and value passes
-- to subsequent sessions in a role chain. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags>
-- in the /IAM User Guide/.
--
-- This parameter is optional. When you set session tags as transitive, the
-- session policy and session tags packed binary limit is not affected.
--
-- If you choose not to specify a transitive tag key, then no tags are
-- passed from this session to any subsequent sessions.
--
-- 'tokenCode', 'assumeRole_tokenCode' - The value provided by the MFA device, if the trust policy of the role
-- being assumed requires MFA. (In other words, if the policy includes a
-- condition that tests for MFA). If the role being assumed requires MFA
-- and if the @TokenCode@ value is missing or expired, the @AssumeRole@
-- call returns an \"access denied\" error.
--
-- The format for this parameter, as described by its regex pattern, is a
-- sequence of six numeric digits.
--
-- 'policyArns', 'assumeRole_policyArns' - The Amazon Resource Names (ARNs) of the IAM managed policies that you
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
-- An Amazon Web Services conversion compresses the passed session policies
-- and session tags into a packed binary format that has a separate limit.
-- Your request can fail for this limit even if your plaintext meets the
-- other requirements. The @PackedPolicySize@ response element indicates by
-- percentage how close the policies and tags for your request are to the
-- upper size limit.
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
-- 'durationSeconds', 'assumeRole_durationSeconds' - The duration, in seconds, of the role session. The value specified can
-- can range from 900 seconds (15 minutes) up to the maximum session
-- duration that is set for the role. The maximum session duration setting
-- can have a value from 1 hour to 12 hours. If you specify a value higher
-- than this setting or the administrator setting (whichever is lower), the
-- operation fails. For example, if you specify a session duration of 12
-- hours, but your administrator set the maximum session duration to 6
-- hours, your operation fails. To learn how to view the maximum value for
-- your role, see
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
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-custom-url.html Creating a URL that Enables Federated Users to Access the Management Console>
-- in the /IAM User Guide/.
--
-- 'policy', 'assumeRole_policy' - An IAM policy in JSON format that you want to use as an inline session
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
-- An Amazon Web Services conversion compresses the passed session policies
-- and session tags into a packed binary format that has a separate limit.
-- Your request can fail for this limit even if your plaintext meets the
-- other requirements. The @PackedPolicySize@ response element indicates by
-- percentage how close the policies and tags for your request are to the
-- upper size limit.
--
-- 'externalId', 'assumeRole_externalId' - A unique identifier that might be required when you assume a role in
-- another account. If the administrator of the account to which the role
-- belongs provided you with an external ID, then provide that value in the
-- @ExternalId@ parameter. This value can be any string, such as a
-- passphrase or account number. A cross-account role is usually set up to
-- trust everyone in an account. Therefore, the administrator of the
-- trusting account might send an external ID to the administrator of the
-- trusted account. That way, only someone with the ID can assume the role,
-- rather than everyone in the account. For more information about the
-- external ID, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your Amazon Web Services Resources to a Third Party>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
--
-- 'sourceIdentity', 'assumeRole_sourceIdentity' - The source identity specified by the principal that is calling the
-- @AssumeRole@ operation.
--
-- You can require users to specify a source identity when they assume a
-- role. You do this by using the @sts:SourceIdentity@ condition key in a
-- role trust policy. You can use source identity information in CloudTrail
-- logs to determine who took actions with a role. You can use the
-- @aws:SourceIdentity@ condition key to further control access to Amazon
-- Web Services resources based on the value of source identity. For more
-- information about using source identity, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-. You cannot use a value that begins with the text
-- @aws:@. This prefix is reserved for Amazon Web Services internal use.
--
-- 'serialNumber', 'assumeRole_serialNumber' - The identification number of the MFA device that is associated with the
-- user who is making the @AssumeRole@ call. Specify this value if the
-- trust policy of the role being assumed includes a condition that
-- requires MFA authentication. The value is either the serial number for a
-- hardware device (such as @GAHT12345678@) or an Amazon Resource Name
-- (ARN) for a virtual device (such as
-- @arn:aws:iam::123456789012:mfa\/user@).
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
--
-- 'tags', 'assumeRole_tags' - A list of session tags that you want to pass. Each session tag consists
-- of a key name and an associated value. For more information about
-- session tags, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging STS Sessions>
-- in the /IAM User Guide/.
--
-- This parameter is optional. You can pass up to 50 session tags. The
-- plaintext session tag keys can’t exceed 128 characters, and the values
-- can’t exceed 256 characters. For these and additional limits, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
-- in the /IAM User Guide/.
--
-- An Amazon Web Services conversion compresses the passed session policies
-- and session tags into a packed binary format that has a separate limit.
-- Your request can fail for this limit even if your plaintext meets the
-- other requirements. The @PackedPolicySize@ response element indicates by
-- percentage how close the policies and tags for your request are to the
-- upper size limit.
--
-- You can pass a session tag with the same key as a tag that is already
-- attached to the role. When you do, session tags override a role tag with
-- the same key.
--
-- Tag key–value pairs are not case sensitive, but case is preserved. This
-- means that you cannot have separate @Department@ and @department@ tag
-- keys. Assume that the role has the @Department@=@Marketing@ tag and you
-- pass the @department@=@engineering@ session tag. @Department@ and
-- @department@ are not saved as separate tags, and the session tag passed
-- in the request takes precedence over the role tag.
--
-- Additionally, if you used temporary credentials to perform this
-- operation, the new session inherits any transitive session tags from the
-- calling session. If you pass a session tag with the same key as an
-- inherited tag, the operation fails. To view the inherited tags for a
-- session, see the CloudTrail logs. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail>
-- in the /IAM User Guide/.
--
-- 'roleArn', 'assumeRole_roleArn' - The Amazon Resource Name (ARN) of the role to assume.
--
-- 'roleSessionName', 'assumeRole_roleSessionName' - An identifier for the assumed role session.
--
-- Use the role session name to uniquely identify a session when the same
-- role is assumed by different principals or for different reasons. In
-- cross-account scenarios, the role session name is visible to, and can be
-- logged by the account that owns the role. The role session name is also
-- used in the ARN of the assumed role principal. This means that
-- subsequent cross-account API requests that use the temporary security
-- credentials will expose the role session name to the external account in
-- their CloudTrail logs.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
newAssumeRole ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'roleSessionName'
  Prelude.Text ->
  AssumeRole
newAssumeRole pRoleArn_ pRoleSessionName_ =
  AssumeRole'
    { transitiveTagKeys = Prelude.Nothing,
      tokenCode = Prelude.Nothing,
      policyArns = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      policy = Prelude.Nothing,
      externalId = Prelude.Nothing,
      sourceIdentity = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      tags = Prelude.Nothing,
      roleArn = pRoleArn_,
      roleSessionName = pRoleSessionName_
    }

-- | A list of keys for session tags that you want to set as transitive. If
-- you set a tag key as transitive, the corresponding key and value passes
-- to subsequent sessions in a role chain. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags>
-- in the /IAM User Guide/.
--
-- This parameter is optional. When you set session tags as transitive, the
-- session policy and session tags packed binary limit is not affected.
--
-- If you choose not to specify a transitive tag key, then no tags are
-- passed from this session to any subsequent sessions.
assumeRole_transitiveTagKeys :: Lens.Lens' AssumeRole (Prelude.Maybe [Prelude.Text])
assumeRole_transitiveTagKeys = Lens.lens (\AssumeRole' {transitiveTagKeys} -> transitiveTagKeys) (\s@AssumeRole' {} a -> s {transitiveTagKeys = a} :: AssumeRole) Prelude.. Lens.mapping Lens.coerced

-- | The value provided by the MFA device, if the trust policy of the role
-- being assumed requires MFA. (In other words, if the policy includes a
-- condition that tests for MFA). If the role being assumed requires MFA
-- and if the @TokenCode@ value is missing or expired, the @AssumeRole@
-- call returns an \"access denied\" error.
--
-- The format for this parameter, as described by its regex pattern, is a
-- sequence of six numeric digits.
assumeRole_tokenCode :: Lens.Lens' AssumeRole (Prelude.Maybe Prelude.Text)
assumeRole_tokenCode = Lens.lens (\AssumeRole' {tokenCode} -> tokenCode) (\s@AssumeRole' {} a -> s {tokenCode = a} :: AssumeRole)

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
-- An Amazon Web Services conversion compresses the passed session policies
-- and session tags into a packed binary format that has a separate limit.
-- Your request can fail for this limit even if your plaintext meets the
-- other requirements. The @PackedPolicySize@ response element indicates by
-- percentage how close the policies and tags for your request are to the
-- upper size limit.
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
assumeRole_policyArns :: Lens.Lens' AssumeRole (Prelude.Maybe [PolicyDescriptorType])
assumeRole_policyArns = Lens.lens (\AssumeRole' {policyArns} -> policyArns) (\s@AssumeRole' {} a -> s {policyArns = a} :: AssumeRole) Prelude.. Lens.mapping Lens.coerced

-- | The duration, in seconds, of the role session. The value specified can
-- can range from 900 seconds (15 minutes) up to the maximum session
-- duration that is set for the role. The maximum session duration setting
-- can have a value from 1 hour to 12 hours. If you specify a value higher
-- than this setting or the administrator setting (whichever is lower), the
-- operation fails. For example, if you specify a session duration of 12
-- hours, but your administrator set the maximum session duration to 6
-- hours, your operation fails. To learn how to view the maximum value for
-- your role, see
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
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-custom-url.html Creating a URL that Enables Federated Users to Access the Management Console>
-- in the /IAM User Guide/.
assumeRole_durationSeconds :: Lens.Lens' AssumeRole (Prelude.Maybe Prelude.Natural)
assumeRole_durationSeconds = Lens.lens (\AssumeRole' {durationSeconds} -> durationSeconds) (\s@AssumeRole' {} a -> s {durationSeconds = a} :: AssumeRole)

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
-- An Amazon Web Services conversion compresses the passed session policies
-- and session tags into a packed binary format that has a separate limit.
-- Your request can fail for this limit even if your plaintext meets the
-- other requirements. The @PackedPolicySize@ response element indicates by
-- percentage how close the policies and tags for your request are to the
-- upper size limit.
assumeRole_policy :: Lens.Lens' AssumeRole (Prelude.Maybe Prelude.Text)
assumeRole_policy = Lens.lens (\AssumeRole' {policy} -> policy) (\s@AssumeRole' {} a -> s {policy = a} :: AssumeRole)

-- | A unique identifier that might be required when you assume a role in
-- another account. If the administrator of the account to which the role
-- belongs provided you with an external ID, then provide that value in the
-- @ExternalId@ parameter. This value can be any string, such as a
-- passphrase or account number. A cross-account role is usually set up to
-- trust everyone in an account. Therefore, the administrator of the
-- trusting account might send an external ID to the administrator of the
-- trusted account. That way, only someone with the ID can assume the role,
-- rather than everyone in the account. For more information about the
-- external ID, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your Amazon Web Services Resources to a Third Party>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
assumeRole_externalId :: Lens.Lens' AssumeRole (Prelude.Maybe Prelude.Text)
assumeRole_externalId = Lens.lens (\AssumeRole' {externalId} -> externalId) (\s@AssumeRole' {} a -> s {externalId = a} :: AssumeRole)

-- | The source identity specified by the principal that is calling the
-- @AssumeRole@ operation.
--
-- You can require users to specify a source identity when they assume a
-- role. You do this by using the @sts:SourceIdentity@ condition key in a
-- role trust policy. You can use source identity information in CloudTrail
-- logs to determine who took actions with a role. You can use the
-- @aws:SourceIdentity@ condition key to further control access to Amazon
-- Web Services resources based on the value of source identity. For more
-- information about using source identity, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-. You cannot use a value that begins with the text
-- @aws:@. This prefix is reserved for Amazon Web Services internal use.
assumeRole_sourceIdentity :: Lens.Lens' AssumeRole (Prelude.Maybe Prelude.Text)
assumeRole_sourceIdentity = Lens.lens (\AssumeRole' {sourceIdentity} -> sourceIdentity) (\s@AssumeRole' {} a -> s {sourceIdentity = a} :: AssumeRole)

-- | The identification number of the MFA device that is associated with the
-- user who is making the @AssumeRole@ call. Specify this value if the
-- trust policy of the role being assumed includes a condition that
-- requires MFA authentication. The value is either the serial number for a
-- hardware device (such as @GAHT12345678@) or an Amazon Resource Name
-- (ARN) for a virtual device (such as
-- @arn:aws:iam::123456789012:mfa\/user@).
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
assumeRole_serialNumber :: Lens.Lens' AssumeRole (Prelude.Maybe Prelude.Text)
assumeRole_serialNumber = Lens.lens (\AssumeRole' {serialNumber} -> serialNumber) (\s@AssumeRole' {} a -> s {serialNumber = a} :: AssumeRole)

-- | A list of session tags that you want to pass. Each session tag consists
-- of a key name and an associated value. For more information about
-- session tags, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging STS Sessions>
-- in the /IAM User Guide/.
--
-- This parameter is optional. You can pass up to 50 session tags. The
-- plaintext session tag keys can’t exceed 128 characters, and the values
-- can’t exceed 256 characters. For these and additional limits, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
-- in the /IAM User Guide/.
--
-- An Amazon Web Services conversion compresses the passed session policies
-- and session tags into a packed binary format that has a separate limit.
-- Your request can fail for this limit even if your plaintext meets the
-- other requirements. The @PackedPolicySize@ response element indicates by
-- percentage how close the policies and tags for your request are to the
-- upper size limit.
--
-- You can pass a session tag with the same key as a tag that is already
-- attached to the role. When you do, session tags override a role tag with
-- the same key.
--
-- Tag key–value pairs are not case sensitive, but case is preserved. This
-- means that you cannot have separate @Department@ and @department@ tag
-- keys. Assume that the role has the @Department@=@Marketing@ tag and you
-- pass the @department@=@engineering@ session tag. @Department@ and
-- @department@ are not saved as separate tags, and the session tag passed
-- in the request takes precedence over the role tag.
--
-- Additionally, if you used temporary credentials to perform this
-- operation, the new session inherits any transitive session tags from the
-- calling session. If you pass a session tag with the same key as an
-- inherited tag, the operation fails. To view the inherited tags for a
-- session, see the CloudTrail logs. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail>
-- in the /IAM User Guide/.
assumeRole_tags :: Lens.Lens' AssumeRole (Prelude.Maybe [Tag])
assumeRole_tags = Lens.lens (\AssumeRole' {tags} -> tags) (\s@AssumeRole' {} a -> s {tags = a} :: AssumeRole) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the role to assume.
assumeRole_roleArn :: Lens.Lens' AssumeRole Prelude.Text
assumeRole_roleArn = Lens.lens (\AssumeRole' {roleArn} -> roleArn) (\s@AssumeRole' {} a -> s {roleArn = a} :: AssumeRole)

-- | An identifier for the assumed role session.
--
-- Use the role session name to uniquely identify a session when the same
-- role is assumed by different principals or for different reasons. In
-- cross-account scenarios, the role session name is visible to, and can be
-- logged by the account that owns the role. The role session name is also
-- used in the ARN of the assumed role principal. This means that
-- subsequent cross-account API requests that use the temporary security
-- credentials will expose the role session name to the external account in
-- their CloudTrail logs.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
assumeRole_roleSessionName :: Lens.Lens' AssumeRole Prelude.Text
assumeRole_roleSessionName = Lens.lens (\AssumeRole' {roleSessionName} -> roleSessionName) (\s@AssumeRole' {} a -> s {roleSessionName = a} :: AssumeRole)

instance Core.AWSRequest AssumeRole where
  type AWSResponse AssumeRole = AssumeRoleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AssumeRoleResult"
      ( \s h x ->
          AssumeRoleResponse'
            Prelude.<$> (x Core..@? "PackedPolicySize")
            Prelude.<*> (x Core..@? "Credentials")
            Prelude.<*> (x Core..@? "AssumedRoleUser")
            Prelude.<*> (x Core..@? "SourceIdentity")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssumeRole where
  hashWithSalt _salt AssumeRole' {..} =
    _salt `Prelude.hashWithSalt` transitiveTagKeys
      `Prelude.hashWithSalt` tokenCode
      `Prelude.hashWithSalt` policyArns
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` sourceIdentity
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` roleSessionName

instance Prelude.NFData AssumeRole where
  rnf AssumeRole' {..} =
    Prelude.rnf transitiveTagKeys
      `Prelude.seq` Prelude.rnf tokenCode
      `Prelude.seq` Prelude.rnf policyArns
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf sourceIdentity
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf roleSessionName

instance Core.ToHeaders AssumeRole where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AssumeRole where
  toPath = Prelude.const "/"

instance Core.ToQuery AssumeRole where
  toQuery AssumeRole' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AssumeRole" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-06-15" :: Prelude.ByteString),
        "TransitiveTagKeys"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> transitiveTagKeys
            ),
        "TokenCode" Core.=: tokenCode,
        "PolicyArns"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> policyArns),
        "DurationSeconds" Core.=: durationSeconds,
        "Policy" Core.=: policy,
        "ExternalId" Core.=: externalId,
        "SourceIdentity" Core.=: sourceIdentity,
        "SerialNumber" Core.=: serialNumber,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "RoleArn" Core.=: roleArn,
        "RoleSessionName" Core.=: roleSessionName
      ]

-- | Contains the response to a successful AssumeRole request, including
-- temporary Amazon Web Services credentials that can be used to make
-- Amazon Web Services requests.
--
-- /See:/ 'newAssumeRoleResponse' smart constructor.
data AssumeRoleResponse = AssumeRoleResponse'
  { -- | A percentage value that indicates the packed size of the session
    -- policies and session tags combined passed in the request. The request
    -- fails if the packed size is greater than 100 percent, which means the
    -- policies and tags exceeded the allowed space.
    packedPolicySize :: Prelude.Maybe Prelude.Natural,
    -- | The temporary security credentials, which include an access key ID, a
    -- secret access key, and a security (or session) token.
    --
    -- The size of the security token that STS API operations return is not
    -- fixed. We strongly recommend that you make no assumptions about the
    -- maximum size.
    credentials :: Prelude.Maybe Core.AuthEnv,
    -- | The Amazon Resource Name (ARN) and the assumed role ID, which are
    -- identifiers that you can use to refer to the resulting temporary
    -- security credentials. For example, you can reference these credentials
    -- as a principal in a resource-based policy by using the ARN or assumed
    -- role ID. The ARN and ID include the @RoleSessionName@ that you specified
    -- when you called @AssumeRole@.
    assumedRoleUser :: Prelude.Maybe AssumedRoleUser,
    -- | The source identity specified by the principal that is calling the
    -- @AssumeRole@ operation.
    --
    -- You can require users to specify a source identity when they assume a
    -- role. You do this by using the @sts:SourceIdentity@ condition key in a
    -- role trust policy. You can use source identity information in CloudTrail
    -- logs to determine who took actions with a role. You can use the
    -- @aws:SourceIdentity@ condition key to further control access to Amazon
    -- Web Services resources based on the value of source identity. For more
    -- information about using source identity, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
    -- in the /IAM User Guide/.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@-
    sourceIdentity :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssumeRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packedPolicySize', 'assumeRoleResponse_packedPolicySize' - A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
--
-- 'credentials', 'assumeRoleResponse_credentials' - The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
--
-- 'assumedRoleUser', 'assumeRoleResponse_assumedRoleUser' - The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary
-- security credentials. For example, you can reference these credentials
-- as a principal in a resource-based policy by using the ARN or assumed
-- role ID. The ARN and ID include the @RoleSessionName@ that you specified
-- when you called @AssumeRole@.
--
-- 'sourceIdentity', 'assumeRoleResponse_sourceIdentity' - The source identity specified by the principal that is calling the
-- @AssumeRole@ operation.
--
-- You can require users to specify a source identity when they assume a
-- role. You do this by using the @sts:SourceIdentity@ condition key in a
-- role trust policy. You can use source identity information in CloudTrail
-- logs to determine who took actions with a role. You can use the
-- @aws:SourceIdentity@ condition key to further control access to Amazon
-- Web Services resources based on the value of source identity. For more
-- information about using source identity, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
--
-- 'httpStatus', 'assumeRoleResponse_httpStatus' - The response's http status code.
newAssumeRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssumeRoleResponse
newAssumeRoleResponse pHttpStatus_ =
  AssumeRoleResponse'
    { packedPolicySize =
        Prelude.Nothing,
      credentials = Prelude.Nothing,
      assumedRoleUser = Prelude.Nothing,
      sourceIdentity = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
assumeRoleResponse_packedPolicySize :: Lens.Lens' AssumeRoleResponse (Prelude.Maybe Prelude.Natural)
assumeRoleResponse_packedPolicySize = Lens.lens (\AssumeRoleResponse' {packedPolicySize} -> packedPolicySize) (\s@AssumeRoleResponse' {} a -> s {packedPolicySize = a} :: AssumeRoleResponse)

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
assumeRoleResponse_credentials :: Lens.Lens' AssumeRoleResponse (Prelude.Maybe Core.AuthEnv)
assumeRoleResponse_credentials = Lens.lens (\AssumeRoleResponse' {credentials} -> credentials) (\s@AssumeRoleResponse' {} a -> s {credentials = a} :: AssumeRoleResponse)

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary
-- security credentials. For example, you can reference these credentials
-- as a principal in a resource-based policy by using the ARN or assumed
-- role ID. The ARN and ID include the @RoleSessionName@ that you specified
-- when you called @AssumeRole@.
assumeRoleResponse_assumedRoleUser :: Lens.Lens' AssumeRoleResponse (Prelude.Maybe AssumedRoleUser)
assumeRoleResponse_assumedRoleUser = Lens.lens (\AssumeRoleResponse' {assumedRoleUser} -> assumedRoleUser) (\s@AssumeRoleResponse' {} a -> s {assumedRoleUser = a} :: AssumeRoleResponse)

-- | The source identity specified by the principal that is calling the
-- @AssumeRole@ operation.
--
-- You can require users to specify a source identity when they assume a
-- role. You do this by using the @sts:SourceIdentity@ condition key in a
-- role trust policy. You can use source identity information in CloudTrail
-- logs to determine who took actions with a role. You can use the
-- @aws:SourceIdentity@ condition key to further control access to Amazon
-- Web Services resources based on the value of source identity. For more
-- information about using source identity, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_monitor.html Monitor and control actions taken with assumed roles>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
assumeRoleResponse_sourceIdentity :: Lens.Lens' AssumeRoleResponse (Prelude.Maybe Prelude.Text)
assumeRoleResponse_sourceIdentity = Lens.lens (\AssumeRoleResponse' {sourceIdentity} -> sourceIdentity) (\s@AssumeRoleResponse' {} a -> s {sourceIdentity = a} :: AssumeRoleResponse)

-- | The response's http status code.
assumeRoleResponse_httpStatus :: Lens.Lens' AssumeRoleResponse Prelude.Int
assumeRoleResponse_httpStatus = Lens.lens (\AssumeRoleResponse' {httpStatus} -> httpStatus) (\s@AssumeRoleResponse' {} a -> s {httpStatus = a} :: AssumeRoleResponse)

instance Prelude.NFData AssumeRoleResponse where
  rnf AssumeRoleResponse' {..} =
    Prelude.rnf packedPolicySize
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf assumedRoleUser
      `Prelude.seq` Prelude.rnf sourceIdentity
      `Prelude.seq` Prelude.rnf httpStatus
