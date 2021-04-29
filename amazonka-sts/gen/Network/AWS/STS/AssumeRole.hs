{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.STS.AssumeRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials that you can use to
-- access AWS resources that you might not normally have access to. These
-- temporary credentials consist of an access key ID, a secret access key,
-- and a security token. Typically, you use @AssumeRole@ within your
-- account or for cross-account access. For a comparison of @AssumeRole@
-- with other API operations that produce temporary credentials, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS API operations>
-- in the /IAM User Guide/.
--
-- You cannot use AWS account root user credentials to call @AssumeRole@.
-- You must use credentials for an IAM user or an IAM role to call
-- @AssumeRole@.
--
-- For cross-account access, imagine that you own multiple accounts and
-- need to access resources in each account. You could create long-term
-- credentials in each account to access those resources. However, managing
-- all those credentials and remembering which one can access which account
-- can be time consuming. Instead, you can create one set of long-term
-- credentials in one account. Then use temporary security credentials to
-- access all the other accounts by assuming roles in those accounts. For
-- more information about roles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM Roles>
-- in the /IAM User Guide/.
--
-- __Session Duration__
--
-- By default, the temporary security credentials created by @AssumeRole@
-- last for one hour. However, you can use the optional @DurationSeconds@
-- parameter to specify the duration of your session. You can provide a
-- value from 900 seconds (15 minutes) up to the maximum session duration
-- setting for the role. This setting can have a value from 1 hour to 12
-- hours. To learn how to view the maximum value for your role, see
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
-- The temporary security credentials created by @AssumeRole@ can be used
-- to make API calls to any AWS service with the following exception: You
-- cannot call the AWS STS @GetFederationToken@ or @GetSessionToken@ API
-- operations.
--
-- (Optional) You can pass inline or managed
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policies>
-- to this operation. You can pass a single JSON policy document to use as
-- an inline session policy. You can also specify up to 10 managed policies
-- to use as managed session policies. The plain text that you use for both
-- inline and managed session policies can\'t exceed 2,048 characters.
-- Passing policies to this operation returns new temporary credentials.
-- The resulting session\'s permissions are the intersection of the role\'s
-- identity-based policy and the session policies. You can use the role\'s
-- temporary credentials in subsequent AWS API calls to access resources in
-- the account that owns the role. You cannot use session policies to grant
-- more permissions than those allowed by the identity-based policy of the
-- role that is being assumed. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- To assume a role from a different account, your AWS account must be
-- trusted by the role. The trust relationship is defined in the role\'s
-- trust policy when the role is created. That trust policy states which
-- accounts are allowed to delegate that access to users in the account.
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
-- with an AWS MFA device. In that scenario, the trust policy of the role
-- being assumed includes a condition that tests for MFA authentication. If
-- the caller does not include valid MFA information, the request to assume
-- the role is denied. The condition in a trust policy that tests for MFA
-- authentication might look like the following example.
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
module Network.AWS.STS.AssumeRole
  ( -- * Creating a Request
    AssumeRole (..),
    newAssumeRole,

    -- * Request Lenses
    assumeRole_tokenCode,
    assumeRole_tags,
    assumeRole_policyArns,
    assumeRole_transitiveTagKeys,
    assumeRole_serialNumber,
    assumeRole_policy,
    assumeRole_externalId,
    assumeRole_durationSeconds,
    assumeRole_roleArn,
    assumeRole_roleSessionName,

    -- * Destructuring the Response
    AssumeRoleResponse (..),
    newAssumeRoleResponse,

    -- * Response Lenses
    assumeRoleResponse_credentials,
    assumeRoleResponse_assumedRoleUser,
    assumeRoleResponse_packedPolicySize,
    assumeRoleResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.STS.Types

-- | /See:/ 'newAssumeRole' smart constructor.
data AssumeRole = AssumeRole'
  { -- | The value provided by the MFA device, if the trust policy of the role
    -- being assumed requires MFA (that is, if the policy includes a condition
    -- that tests for MFA). If the role being assumed requires MFA and if the
    -- @TokenCode@ value is missing or expired, the @AssumeRole@ call returns
    -- an \"access denied\" error.
    --
    -- The format for this parameter, as described by its regex pattern, is a
    -- sequence of six numeric digits.
    tokenCode :: Prelude.Maybe Prelude.Text,
    -- | A list of session tags that you want to pass. Each session tag consists
    -- of a key name and an associated value. For more information about
    -- session tags, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging AWS STS Sessions>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. You can pass up to 50 session tags. The
    -- plain text session tag keys can’t exceed 128 characters, and the values
    -- can’t exceed 256 characters. For these and additional limits, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
    -- in the /IAM User Guide/.
    --
    -- An AWS conversion compresses the passed session policies and session
    -- tags into a packed binary format that has a separate limit. Your request
    -- can fail for this limit even if your plain text meets the other
    -- requirements. The @PackedPolicySize@ response element indicates by
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
    -- session, see the AWS CloudTrail logs. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Names (ARNs) of the IAM managed policies that you
    -- want to use as managed session policies. The policies must exist in the
    -- same account as the role.
    --
    -- This parameter is optional. You can provide up to 10 managed policy
    -- ARNs. However, the plain text that you use for both inline and managed
    -- session policies can\'t exceed 2,048 characters. For more information
    -- about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the AWS General Reference.
    --
    -- An AWS conversion compresses the passed session policies and session
    -- tags into a packed binary format that has a separate limit. Your request
    -- can fail for this limit even if your plain text meets the other
    -- requirements. The @PackedPolicySize@ response element indicates by
    -- percentage how close the policies and tags for your request are to the
    -- upper size limit.
    --
    -- Passing policies to this operation returns new temporary credentials.
    -- The resulting session\'s permissions are the intersection of the role\'s
    -- identity-based policy and the session policies. You can use the role\'s
    -- temporary credentials in subsequent AWS API calls to access resources in
    -- the account that owns the role. You cannot use session policies to grant
    -- more permissions than those allowed by the identity-based policy of the
    -- role that is being assumed. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
    -- in the /IAM User Guide/.
    policyArns :: Prelude.Maybe [PolicyDescriptorType],
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
    transitiveTagKeys :: Prelude.Maybe [Prelude.Text],
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
    -- | An IAM policy in JSON format that you want to use as an inline session
    -- policy.
    --
    -- This parameter is optional. Passing policies to this operation returns
    -- new temporary credentials. The resulting session\'s permissions are the
    -- intersection of the role\'s identity-based policy and the session
    -- policies. You can use the role\'s temporary credentials in subsequent
    -- AWS API calls to access resources in the account that owns the role. You
    -- cannot use session policies to grant more permissions than those allowed
    -- by the identity-based policy of the role that is being assumed. For more
    -- information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
    -- in the /IAM User Guide/.
    --
    -- The plain text that you use for both inline and managed session policies
    -- can\'t exceed 2,048 characters. The JSON policy characters can be any
    -- ASCII character from the space character to the end of the valid
    -- character list (\\u0020 through \\u00FF). It can also include the tab
    -- (\\u0009), linefeed (\\u000A), and carriage return (\\u000D) characters.
    --
    -- An AWS conversion compresses the passed session policies and session
    -- tags into a packed binary format that has a separate limit. Your request
    -- can fail for this limit even if your plain text meets the other
    -- requirements. The @PackedPolicySize@ response element indicates by
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
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party>
    -- in the /IAM User Guide/.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@:\/-
    externalId :: Prelude.Maybe Prelude.Text,
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
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-custom-url.html Creating a URL that Enables Federated Users to Access the AWS Management Console>
    -- in the /IAM User Guide/.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
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
    -- their AWS CloudTrail logs.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of upper- and lower-case alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@-
    roleSessionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssumeRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tokenCode', 'assumeRole_tokenCode' - The value provided by the MFA device, if the trust policy of the role
-- being assumed requires MFA (that is, if the policy includes a condition
-- that tests for MFA). If the role being assumed requires MFA and if the
-- @TokenCode@ value is missing or expired, the @AssumeRole@ call returns
-- an \"access denied\" error.
--
-- The format for this parameter, as described by its regex pattern, is a
-- sequence of six numeric digits.
--
-- 'tags', 'assumeRole_tags' - A list of session tags that you want to pass. Each session tag consists
-- of a key name and an associated value. For more information about
-- session tags, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging AWS STS Sessions>
-- in the /IAM User Guide/.
--
-- This parameter is optional. You can pass up to 50 session tags. The
-- plain text session tag keys can’t exceed 128 characters, and the values
-- can’t exceed 256 characters. For these and additional limits, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
-- in the /IAM User Guide/.
--
-- An AWS conversion compresses the passed session policies and session
-- tags into a packed binary format that has a separate limit. Your request
-- can fail for this limit even if your plain text meets the other
-- requirements. The @PackedPolicySize@ response element indicates by
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
-- session, see the AWS CloudTrail logs. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail>
-- in the /IAM User Guide/.
--
-- 'policyArns', 'assumeRole_policyArns' - The Amazon Resource Names (ARNs) of the IAM managed policies that you
-- want to use as managed session policies. The policies must exist in the
-- same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy
-- ARNs. However, the plain text that you use for both inline and managed
-- session policies can\'t exceed 2,048 characters. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the AWS General Reference.
--
-- An AWS conversion compresses the passed session policies and session
-- tags into a packed binary format that has a separate limit. Your request
-- can fail for this limit even if your plain text meets the other
-- requirements. The @PackedPolicySize@ response element indicates by
-- percentage how close the policies and tags for your request are to the
-- upper size limit.
--
-- Passing policies to this operation returns new temporary credentials.
-- The resulting session\'s permissions are the intersection of the role\'s
-- identity-based policy and the session policies. You can use the role\'s
-- temporary credentials in subsequent AWS API calls to access resources in
-- the account that owns the role. You cannot use session policies to grant
-- more permissions than those allowed by the identity-based policy of the
-- role that is being assumed. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
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
-- 'policy', 'assumeRole_policy' - An IAM policy in JSON format that you want to use as an inline session
-- policy.
--
-- This parameter is optional. Passing policies to this operation returns
-- new temporary credentials. The resulting session\'s permissions are the
-- intersection of the role\'s identity-based policy and the session
-- policies. You can use the role\'s temporary credentials in subsequent
-- AWS API calls to access resources in the account that owns the role. You
-- cannot use session policies to grant more permissions than those allowed
-- by the identity-based policy of the role that is being assumed. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- The plain text that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters. The JSON policy characters can be any
-- ASCII character from the space character to the end of the valid
-- character list (\\u0020 through \\u00FF). It can also include the tab
-- (\\u0009), linefeed (\\u000A), and carriage return (\\u000D) characters.
--
-- An AWS conversion compresses the passed session policies and session
-- tags into a packed binary format that has a separate limit. Your request
-- can fail for this limit even if your plain text meets the other
-- requirements. The @PackedPolicySize@ response element indicates by
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
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
--
-- 'durationSeconds', 'assumeRole_durationSeconds' - The duration, in seconds, of the role session. The value can range from
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
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-custom-url.html Creating a URL that Enables Federated Users to Access the AWS Management Console>
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
-- their AWS CloudTrail logs.
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
    { tokenCode = Prelude.Nothing,
      tags = Prelude.Nothing,
      policyArns = Prelude.Nothing,
      transitiveTagKeys = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      policy = Prelude.Nothing,
      externalId = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      roleArn = pRoleArn_,
      roleSessionName = pRoleSessionName_
    }

-- | The value provided by the MFA device, if the trust policy of the role
-- being assumed requires MFA (that is, if the policy includes a condition
-- that tests for MFA). If the role being assumed requires MFA and if the
-- @TokenCode@ value is missing or expired, the @AssumeRole@ call returns
-- an \"access denied\" error.
--
-- The format for this parameter, as described by its regex pattern, is a
-- sequence of six numeric digits.
assumeRole_tokenCode :: Lens.Lens' AssumeRole (Prelude.Maybe Prelude.Text)
assumeRole_tokenCode = Lens.lens (\AssumeRole' {tokenCode} -> tokenCode) (\s@AssumeRole' {} a -> s {tokenCode = a} :: AssumeRole)

-- | A list of session tags that you want to pass. Each session tag consists
-- of a key name and an associated value. For more information about
-- session tags, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging AWS STS Sessions>
-- in the /IAM User Guide/.
--
-- This parameter is optional. You can pass up to 50 session tags. The
-- plain text session tag keys can’t exceed 128 characters, and the values
-- can’t exceed 256 characters. For these and additional limits, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits>
-- in the /IAM User Guide/.
--
-- An AWS conversion compresses the passed session policies and session
-- tags into a packed binary format that has a separate limit. Your request
-- can fail for this limit even if your plain text meets the other
-- requirements. The @PackedPolicySize@ response element indicates by
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
-- session, see the AWS CloudTrail logs. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail>
-- in the /IAM User Guide/.
assumeRole_tags :: Lens.Lens' AssumeRole (Prelude.Maybe [Tag])
assumeRole_tags = Lens.lens (\AssumeRole' {tags} -> tags) (\s@AssumeRole' {} a -> s {tags = a} :: AssumeRole) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you
-- want to use as managed session policies. The policies must exist in the
-- same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy
-- ARNs. However, the plain text that you use for both inline and managed
-- session policies can\'t exceed 2,048 characters. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the AWS General Reference.
--
-- An AWS conversion compresses the passed session policies and session
-- tags into a packed binary format that has a separate limit. Your request
-- can fail for this limit even if your plain text meets the other
-- requirements. The @PackedPolicySize@ response element indicates by
-- percentage how close the policies and tags for your request are to the
-- upper size limit.
--
-- Passing policies to this operation returns new temporary credentials.
-- The resulting session\'s permissions are the intersection of the role\'s
-- identity-based policy and the session policies. You can use the role\'s
-- temporary credentials in subsequent AWS API calls to access resources in
-- the account that owns the role. You cannot use session policies to grant
-- more permissions than those allowed by the identity-based policy of the
-- role that is being assumed. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
assumeRole_policyArns :: Lens.Lens' AssumeRole (Prelude.Maybe [PolicyDescriptorType])
assumeRole_policyArns = Lens.lens (\AssumeRole' {policyArns} -> policyArns) (\s@AssumeRole' {} a -> s {policyArns = a} :: AssumeRole) Prelude.. Lens.mapping Prelude._Coerce

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
assumeRole_transitiveTagKeys = Lens.lens (\AssumeRole' {transitiveTagKeys} -> transitiveTagKeys) (\s@AssumeRole' {} a -> s {transitiveTagKeys = a} :: AssumeRole) Prelude.. Lens.mapping Prelude._Coerce

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

-- | An IAM policy in JSON format that you want to use as an inline session
-- policy.
--
-- This parameter is optional. Passing policies to this operation returns
-- new temporary credentials. The resulting session\'s permissions are the
-- intersection of the role\'s identity-based policy and the session
-- policies. You can use the role\'s temporary credentials in subsequent
-- AWS API calls to access resources in the account that owns the role. You
-- cannot use session policies to grant more permissions than those allowed
-- by the identity-based policy of the role that is being assumed. For more
-- information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies>
-- in the /IAM User Guide/.
--
-- The plain text that you use for both inline and managed session policies
-- can\'t exceed 2,048 characters. The JSON policy characters can be any
-- ASCII character from the space character to the end of the valid
-- character list (\\u0020 through \\u00FF). It can also include the tab
-- (\\u0009), linefeed (\\u000A), and carriage return (\\u000D) characters.
--
-- An AWS conversion compresses the passed session policies and session
-- tags into a packed binary format that has a separate limit. Your request
-- can fail for this limit even if your plain text meets the other
-- requirements. The @PackedPolicySize@ response element indicates by
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
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party>
-- in the /IAM User Guide/.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
assumeRole_externalId :: Lens.Lens' AssumeRole (Prelude.Maybe Prelude.Text)
assumeRole_externalId = Lens.lens (\AssumeRole' {externalId} -> externalId) (\s@AssumeRole' {} a -> s {externalId = a} :: AssumeRole)

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
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-custom-url.html Creating a URL that Enables Federated Users to Access the AWS Management Console>
-- in the /IAM User Guide/.
assumeRole_durationSeconds :: Lens.Lens' AssumeRole (Prelude.Maybe Prelude.Natural)
assumeRole_durationSeconds = Lens.lens (\AssumeRole' {durationSeconds} -> durationSeconds) (\s@AssumeRole' {} a -> s {durationSeconds = a} :: AssumeRole)

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
-- their AWS CloudTrail logs.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of upper- and lower-case alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@-
assumeRole_roleSessionName :: Lens.Lens' AssumeRole Prelude.Text
assumeRole_roleSessionName = Lens.lens (\AssumeRole' {roleSessionName} -> roleSessionName) (\s@AssumeRole' {} a -> s {roleSessionName = a} :: AssumeRole)

instance Prelude.AWSRequest AssumeRole where
  type Rs AssumeRole = AssumeRoleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AssumeRoleResult"
      ( \s h x ->
          AssumeRoleResponse'
            Prelude.<$> (x Prelude..@? "Credentials")
            Prelude.<*> (x Prelude..@? "AssumedRoleUser")
            Prelude.<*> (x Prelude..@? "PackedPolicySize")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssumeRole

instance Prelude.NFData AssumeRole

instance Prelude.ToHeaders AssumeRole where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AssumeRole where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssumeRole where
  toQuery AssumeRole' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AssumeRole" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-06-15" :: Prelude.ByteString),
        "TokenCode" Prelude.=: tokenCode,
        "Tags"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> tags),
        "PolicyArns"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> policyArns
            ),
        "TransitiveTagKeys"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> transitiveTagKeys
            ),
        "SerialNumber" Prelude.=: serialNumber,
        "Policy" Prelude.=: policy,
        "ExternalId" Prelude.=: externalId,
        "DurationSeconds" Prelude.=: durationSeconds,
        "RoleArn" Prelude.=: roleArn,
        "RoleSessionName" Prelude.=: roleSessionName
      ]

-- | Contains the response to a successful AssumeRole request, including
-- temporary AWS credentials that can be used to make AWS requests.
--
-- /See:/ 'newAssumeRoleResponse' smart constructor.
data AssumeRoleResponse = AssumeRoleResponse'
  { -- | The temporary security credentials, which include an access key ID, a
    -- secret access key, and a security (or session) token.
    --
    -- The size of the security token that STS API operations return is not
    -- fixed. We strongly recommend that you make no assumptions about the
    -- maximum size.
    credentials :: Prelude.Maybe Prelude.AuthEnv,
    -- | The Amazon Resource Name (ARN) and the assumed role ID, which are
    -- identifiers that you can use to refer to the resulting temporary
    -- security credentials. For example, you can reference these credentials
    -- as a principal in a resource-based policy by using the ARN or assumed
    -- role ID. The ARN and ID include the @RoleSessionName@ that you specified
    -- when you called @AssumeRole@.
    assumedRoleUser :: Prelude.Maybe AssumedRoleUser,
    -- | A percentage value that indicates the packed size of the session
    -- policies and session tags combined passed in the request. The request
    -- fails if the packed size is greater than 100 percent, which means the
    -- policies and tags exceeded the allowed space.
    packedPolicySize :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssumeRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'packedPolicySize', 'assumeRoleResponse_packedPolicySize' - A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
--
-- 'httpStatus', 'assumeRoleResponse_httpStatus' - The response's http status code.
newAssumeRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssumeRoleResponse
newAssumeRoleResponse pHttpStatus_ =
  AssumeRoleResponse'
    { credentials = Prelude.Nothing,
      assumedRoleUser = Prelude.Nothing,
      packedPolicySize = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The temporary security credentials, which include an access key ID, a
-- secret access key, and a security (or session) token.
--
-- The size of the security token that STS API operations return is not
-- fixed. We strongly recommend that you make no assumptions about the
-- maximum size.
assumeRoleResponse_credentials :: Lens.Lens' AssumeRoleResponse (Prelude.Maybe Prelude.AuthEnv)
assumeRoleResponse_credentials = Lens.lens (\AssumeRoleResponse' {credentials} -> credentials) (\s@AssumeRoleResponse' {} a -> s {credentials = a} :: AssumeRoleResponse)

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are
-- identifiers that you can use to refer to the resulting temporary
-- security credentials. For example, you can reference these credentials
-- as a principal in a resource-based policy by using the ARN or assumed
-- role ID. The ARN and ID include the @RoleSessionName@ that you specified
-- when you called @AssumeRole@.
assumeRoleResponse_assumedRoleUser :: Lens.Lens' AssumeRoleResponse (Prelude.Maybe AssumedRoleUser)
assumeRoleResponse_assumedRoleUser = Lens.lens (\AssumeRoleResponse' {assumedRoleUser} -> assumedRoleUser) (\s@AssumeRoleResponse' {} a -> s {assumedRoleUser = a} :: AssumeRoleResponse)

-- | A percentage value that indicates the packed size of the session
-- policies and session tags combined passed in the request. The request
-- fails if the packed size is greater than 100 percent, which means the
-- policies and tags exceeded the allowed space.
assumeRoleResponse_packedPolicySize :: Lens.Lens' AssumeRoleResponse (Prelude.Maybe Prelude.Natural)
assumeRoleResponse_packedPolicySize = Lens.lens (\AssumeRoleResponse' {packedPolicySize} -> packedPolicySize) (\s@AssumeRoleResponse' {} a -> s {packedPolicySize = a} :: AssumeRoleResponse)

-- | The response's http status code.
assumeRoleResponse_httpStatus :: Lens.Lens' AssumeRoleResponse Prelude.Int
assumeRoleResponse_httpStatus = Lens.lens (\AssumeRoleResponse' {httpStatus} -> httpStatus) (\s@AssumeRoleResponse' {} a -> s {httpStatus = a} :: AssumeRoleResponse)

instance Prelude.NFData AssumeRoleResponse
