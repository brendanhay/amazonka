{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.AssumeRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials that you can use to access AWS resources that you might not normally have access to. These temporary credentials consist of an access key ID, a secret access key, and a security token. Typically, you use @AssumeRole@ within your account or for cross-account access. For a comparison of @AssumeRole@ with other API operations that produce temporary credentials, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials> and <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS API operations> in the /IAM User Guide/ .
--
-- /Important:/ You cannot use AWS account root user credentials to call @AssumeRole@ . You must use credentials for an IAM user or an IAM role to call @AssumeRole@ .
-- For cross-account access, imagine that you own multiple accounts and need to access resources in each account. You could create long-term credentials in each account to access those resources. However, managing all those credentials and remembering which one can access which account can be time consuming. Instead, you can create one set of long-term credentials in one account. Then use temporary security credentials to access all the other accounts by assuming roles in those accounts. For more information about roles, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM Roles> in the /IAM User Guide/ .
-- __Session Duration__
-- By default, the temporary security credentials created by @AssumeRole@ last for one hour. However, you can use the optional @DurationSeconds@ parameter to specify the duration of your session. You can provide a value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . The maximum session duration limit applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI commands. However the limit does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
-- __Permissions__
-- The temporary security credentials created by @AssumeRole@ can be used to make API calls to any AWS service with the following exception: You cannot call the AWS STS @GetFederationToken@ or @GetSessionToken@ API operations.
-- (Optional) You can pass inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policies> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- To assume a role from a different account, your AWS account must be trusted by the role. The trust relationship is defined in the role's trust policy when the role is created. That trust policy states which accounts are allowed to delegate that access to users in the account.
-- A user who wants to access a role in a different account must also have permissions that are delegated from the user account administrator. The administrator must attach a policy that allows the user to call @AssumeRole@ for the ARN of the role in the other account. If the user is in the same account as the role, then you can do either of the following:
--
--     * Attach a policy to the user (identical to the previous user in a different account).
--
--
--     * Add the user as a principal directly in the role's trust policy.
--
--
-- In this case, the trust policy acts as an IAM resource-based policy. Users in the same account as the role do not need explicit permission to assume the role. For more information about trust policies and resource-based policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies> in the /IAM User Guide/ .
-- __Tags__
-- (Optional) You can pass tag key-value pairs to your session. These tags are called session tags. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
-- An administrator must grant you the permissions necessary to pass session tags. The administrator can also create granular permissions to allow you to pass only specific session tags. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_attribute-based-access-control.html Tutorial: Using Tags for Attribute-Based Access Control> in the /IAM User Guide/ .
-- You can set the session tags as transitive. Transitive tags persist during role chaining. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ .
-- __Using MFA with AssumeRole__
-- (Optional) You can include multi-factor authentication (MFA) information when you call @AssumeRole@ . This is useful for cross-account scenarios to ensure that the user that assumes the role has been authenticated with an AWS MFA device. In that scenario, the trust policy of the role being assumed includes a condition that tests for MFA authentication. If the caller does not include valid MFA information, the request to assume the role is denied. The condition in a trust policy that tests for MFA authentication might look like the following example.
-- @"Condition": {"Bool": {"aws:MultiFactorAuthPresent": true}}@
-- For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/MFAProtectedAPI.html Configuring MFA-Protected API Access> in the /IAM User Guide/ guide.
-- To use MFA with @AssumeRole@ , you pass values for the @SerialNumber@ and @TokenCode@ parameters. The @SerialNumber@ value identifies the user's hardware or virtual MFA device. The @TokenCode@ is the time-based one-time password (TOTP) that the MFA device produces.
module Network.AWS.STS.AssumeRole
  ( -- * Creating a request
    AssumeRole (..),
    mkAssumeRole,

    -- ** Request lenses
    arRoleSessionName,
    arTransitiveTagKeys,
    arTokenCode,
    arPolicyARNs,
    arDurationSeconds,
    arPolicy,
    arExternalId,
    arSerialNumber,
    arTags,
    arRoleARN,

    -- * Destructuring the response
    AssumeRoleResponse (..),
    mkAssumeRoleResponse,

    -- ** Response lenses
    arrsPackedPolicySize,
    arrsCredentials,
    arrsAssumedRoleUser,
    arrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.STS.Types

-- | /See:/ 'mkAssumeRole' smart constructor.
data AssumeRole = AssumeRole'
  { -- | An identifier for the assumed role session.
    --
    -- Use the role session name to uniquely identify a session when the same role is assumed by different principals or for different reasons. In cross-account scenarios, the role session name is visible to, and can be logged by the account that owns the role. The role session name is also used in the ARN of the assumed role principal. This means that subsequent cross-account API requests that use the temporary security credentials will expose the role session name to the external account in their AWS CloudTrail logs.
    -- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
    roleSessionName :: Lude.Text,
    -- | A list of keys for session tags that you want to set as transitive. If you set a tag key as transitive, the corresponding key and value passes to subsequent sessions in a role chain. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ .
    --
    -- This parameter is optional. When you set session tags as transitive, the session policy and session tags packed binary limit is not affected.
    -- If you choose not to specify a transitive tag key, then no tags are passed from this session to any subsequent sessions.
    transitiveTagKeys :: Lude.Maybe [Lude.Text],
    -- | The value provided by the MFA device, if the trust policy of the role being assumed requires MFA (that is, if the policy includes a condition that tests for MFA). If the role being assumed requires MFA and if the @TokenCode@ value is missing or expired, the @AssumeRole@ call returns an "access denied" error.
    --
    -- The format for this parameter, as described by its regex pattern, is a sequence of six numeric digits.
    tokenCode :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
    --
    -- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
    -- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
    policyARNs :: Lude.Maybe [PolicyDescriptorType],
    -- | The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
    --
    -- By default, the value is set to @3600@ seconds.
    durationSeconds :: Lude.Maybe Lude.Natural,
    -- | An IAM policy in JSON format that you want to use as an inline session policy.
    --
    -- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
    -- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
    policy :: Lude.Maybe Lude.Text,
    -- | A unique identifier that might be required when you assume a role in another account. If the administrator of the account to which the role belongs provided you with an external ID, then provide that value in the @ExternalId@ parameter. This value can be any string, such as a passphrase or account number. A cross-account role is usually set up to trust everyone in an account. Therefore, the administrator of the trusting account might send an external ID to the administrator of the trusted account. That way, only someone with the ID can assume the role, rather than everyone in the account. For more information about the external ID, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party> in the /IAM User Guide/ .
    --
    -- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@:/-
    externalId :: Lude.Maybe Lude.Text,
    -- | The identification number of the MFA device that is associated with the user who is making the @AssumeRole@ call. Specify this value if the trust policy of the role being assumed includes a condition that requires MFA authentication. The value is either the serial number for a hardware device (such as @GAHT12345678@ ) or an Amazon Resource Name (ARN) for a virtual device (such as @arn:aws:iam::123456789012:mfa/user@ ).
    --
    -- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
    serialNumber :: Lude.Maybe Lude.Text,
    -- | A list of session tags that you want to pass. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging AWS STS Sessions> in the /IAM User Guide/ .
    --
    -- This parameter is optional. You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters, and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ .
    -- You can pass a session tag with the same key as a tag that is already attached to the role. When you do, session tags override a role tag with the same key.
    -- Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the role has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the role tag.
    -- Additionally, if you used temporary credentials to perform this operation, the new session inherits any transitive session tags from the calling session. If you pass a session tag with the same key as an inherited tag, the operation fails. To view the inherited tags for a session, see the AWS CloudTrail logs. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail> in the /IAM User Guide/ .
    tags :: Lude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the role to assume.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssumeRole' with the minimum fields required to make a request.
--
-- * 'roleSessionName' - An identifier for the assumed role session.
--
-- Use the role session name to uniquely identify a session when the same role is assumed by different principals or for different reasons. In cross-account scenarios, the role session name is visible to, and can be logged by the account that owns the role. The role session name is also used in the ARN of the assumed role principal. This means that subsequent cross-account API requests that use the temporary security credentials will expose the role session name to the external account in their AWS CloudTrail logs.
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
-- * 'transitiveTagKeys' - A list of keys for session tags that you want to set as transitive. If you set a tag key as transitive, the corresponding key and value passes to subsequent sessions in a role chain. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ .
--
-- This parameter is optional. When you set session tags as transitive, the session policy and session tags packed binary limit is not affected.
-- If you choose not to specify a transitive tag key, then no tags are passed from this session to any subsequent sessions.
-- * 'tokenCode' - The value provided by the MFA device, if the trust policy of the role being assumed requires MFA (that is, if the policy includes a condition that tests for MFA). If the role being assumed requires MFA and if the @TokenCode@ value is missing or expired, the @AssumeRole@ call returns an "access denied" error.
--
-- The format for this parameter, as described by its regex pattern, is a sequence of six numeric digits.
-- * 'policyARNs' - The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
-- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- * 'durationSeconds' - The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
--
-- By default, the value is set to @3600@ seconds.
-- * 'policy' - An IAM policy in JSON format that you want to use as an inline session policy.
--
-- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
-- * 'externalId' - A unique identifier that might be required when you assume a role in another account. If the administrator of the account to which the role belongs provided you with an external ID, then provide that value in the @ExternalId@ parameter. This value can be any string, such as a passphrase or account number. A cross-account role is usually set up to trust everyone in an account. Therefore, the administrator of the trusting account might send an external ID to the administrator of the trusted account. That way, only someone with the ID can assume the role, rather than everyone in the account. For more information about the external ID, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party> in the /IAM User Guide/ .
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@:/-
-- * 'serialNumber' - The identification number of the MFA device that is associated with the user who is making the @AssumeRole@ call. Specify this value if the trust policy of the role being assumed includes a condition that requires MFA authentication. The value is either the serial number for a hardware device (such as @GAHT12345678@ ) or an Amazon Resource Name (ARN) for a virtual device (such as @arn:aws:iam::123456789012:mfa/user@ ).
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
-- * 'tags' - A list of session tags that you want to pass. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging AWS STS Sessions> in the /IAM User Guide/ .
--
-- This parameter is optional. You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters, and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ .
-- You can pass a session tag with the same key as a tag that is already attached to the role. When you do, session tags override a role tag with the same key.
-- Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the role has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the role tag.
-- Additionally, if you used temporary credentials to perform this operation, the new session inherits any transitive session tags from the calling session. If you pass a session tag with the same key as an inherited tag, the operation fails. To view the inherited tags for a session, see the AWS CloudTrail logs. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail> in the /IAM User Guide/ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the role to assume.
mkAssumeRole ::
  -- | 'roleSessionName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  AssumeRole
mkAssumeRole pRoleSessionName_ pRoleARN_ =
  AssumeRole'
    { roleSessionName = pRoleSessionName_,
      transitiveTagKeys = Lude.Nothing,
      tokenCode = Lude.Nothing,
      policyARNs = Lude.Nothing,
      durationSeconds = Lude.Nothing,
      policy = Lude.Nothing,
      externalId = Lude.Nothing,
      serialNumber = Lude.Nothing,
      tags = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | An identifier for the assumed role session.
--
-- Use the role session name to uniquely identify a session when the same role is assumed by different principals or for different reasons. In cross-account scenarios, the role session name is visible to, and can be logged by the account that owns the role. The role session name is also used in the ARN of the assumed role principal. This means that subsequent cross-account API requests that use the temporary security credentials will expose the role session name to the external account in their AWS CloudTrail logs.
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
--
-- /Note:/ Consider using 'roleSessionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRoleSessionName :: Lens.Lens' AssumeRole Lude.Text
arRoleSessionName = Lens.lens (roleSessionName :: AssumeRole -> Lude.Text) (\s a -> s {roleSessionName = a} :: AssumeRole)
{-# DEPRECATED arRoleSessionName "Use generic-lens or generic-optics with 'roleSessionName' instead." #-}

-- | A list of keys for session tags that you want to set as transitive. If you set a tag key as transitive, the corresponding key and value passes to subsequent sessions in a role chain. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ .
--
-- This parameter is optional. When you set session tags as transitive, the session policy and session tags packed binary limit is not affected.
-- If you choose not to specify a transitive tag key, then no tags are passed from this session to any subsequent sessions.
--
-- /Note:/ Consider using 'transitiveTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arTransitiveTagKeys :: Lens.Lens' AssumeRole (Lude.Maybe [Lude.Text])
arTransitiveTagKeys = Lens.lens (transitiveTagKeys :: AssumeRole -> Lude.Maybe [Lude.Text]) (\s a -> s {transitiveTagKeys = a} :: AssumeRole)
{-# DEPRECATED arTransitiveTagKeys "Use generic-lens or generic-optics with 'transitiveTagKeys' instead." #-}

-- | The value provided by the MFA device, if the trust policy of the role being assumed requires MFA (that is, if the policy includes a condition that tests for MFA). If the role being assumed requires MFA and if the @TokenCode@ value is missing or expired, the @AssumeRole@ call returns an "access denied" error.
--
-- The format for this parameter, as described by its regex pattern, is a sequence of six numeric digits.
--
-- /Note:/ Consider using 'tokenCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arTokenCode :: Lens.Lens' AssumeRole (Lude.Maybe Lude.Text)
arTokenCode = Lens.lens (tokenCode :: AssumeRole -> Lude.Maybe Lude.Text) (\s a -> s {tokenCode = a} :: AssumeRole)
{-# DEPRECATED arTokenCode "Use generic-lens or generic-optics with 'tokenCode' instead." #-}

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role.
--
-- This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference.
-- Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'policyARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arPolicyARNs :: Lens.Lens' AssumeRole (Lude.Maybe [PolicyDescriptorType])
arPolicyARNs = Lens.lens (policyARNs :: AssumeRole -> Lude.Maybe [PolicyDescriptorType]) (\s a -> s {policyARNs = a} :: AssumeRole)
{-# DEPRECATED arPolicyARNs "Use generic-lens or generic-optics with 'policyARNs' instead." #-}

-- | The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ .
--
-- By default, the value is set to @3600@ seconds.
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arDurationSeconds :: Lens.Lens' AssumeRole (Lude.Maybe Lude.Natural)
arDurationSeconds = Lens.lens (durationSeconds :: AssumeRole -> Lude.Maybe Lude.Natural) (\s a -> s {durationSeconds = a} :: AssumeRole)
{-# DEPRECATED arDurationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead." #-}

-- | An IAM policy in JSON format that you want to use as an inline session policy.
--
-- This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
-- The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arPolicy :: Lens.Lens' AssumeRole (Lude.Maybe Lude.Text)
arPolicy = Lens.lens (policy :: AssumeRole -> Lude.Maybe Lude.Text) (\s a -> s {policy = a} :: AssumeRole)
{-# DEPRECATED arPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | A unique identifier that might be required when you assume a role in another account. If the administrator of the account to which the role belongs provided you with an external ID, then provide that value in the @ExternalId@ parameter. This value can be any string, such as a passphrase or account number. A cross-account role is usually set up to trust everyone in an account. Therefore, the administrator of the trusting account might send an external ID to the administrator of the trusted account. That way, only someone with the ID can assume the role, rather than everyone in the account. For more information about the external ID, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party> in the /IAM User Guide/ .
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@:/-
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arExternalId :: Lens.Lens' AssumeRole (Lude.Maybe Lude.Text)
arExternalId = Lens.lens (externalId :: AssumeRole -> Lude.Maybe Lude.Text) (\s a -> s {externalId = a} :: AssumeRole)
{-# DEPRECATED arExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

-- | The identification number of the MFA device that is associated with the user who is making the @AssumeRole@ call. Specify this value if the trust policy of the role being assumed includes a condition that requires MFA authentication. The value is either the serial number for a hardware device (such as @GAHT12345678@ ) or an Amazon Resource Name (ARN) for a virtual device (such as @arn:aws:iam::123456789012:mfa/user@ ).
--
-- The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arSerialNumber :: Lens.Lens' AssumeRole (Lude.Maybe Lude.Text)
arSerialNumber = Lens.lens (serialNumber :: AssumeRole -> Lude.Maybe Lude.Text) (\s a -> s {serialNumber = a} :: AssumeRole)
{-# DEPRECATED arSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | A list of session tags that you want to pass. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging AWS STS Sessions> in the /IAM User Guide/ .
--
-- This parameter is optional. You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters, and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ .
-- You can pass a session tag with the same key as a tag that is already attached to the role. When you do, session tags override a role tag with the same key.
-- Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the role has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the role tag.
-- Additionally, if you used temporary credentials to perform this operation, the new session inherits any transitive session tags from the calling session. If you pass a session tag with the same key as an inherited tag, the operation fails. To view the inherited tags for a session, see the AWS CloudTrail logs. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arTags :: Lens.Lens' AssumeRole (Lude.Maybe [Tag])
arTags = Lens.lens (tags :: AssumeRole -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: AssumeRole)
{-# DEPRECATED arTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the role to assume.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRoleARN :: Lens.Lens' AssumeRole Lude.Text
arRoleARN = Lens.lens (roleARN :: AssumeRole -> Lude.Text) (\s a -> s {roleARN = a} :: AssumeRole)
{-# DEPRECATED arRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest AssumeRole where
  type Rs AssumeRole = AssumeRoleResponse
  request = Req.postQuery stsService
  response =
    Res.receiveXMLWrapper
      "AssumeRoleResult"
      ( \s h x ->
          AssumeRoleResponse'
            Lude.<$> (x Lude..@? "PackedPolicySize")
            Lude.<*> (x Lude..@? "Credentials")
            Lude.<*> (x Lude..@? "AssumedRoleUser")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssumeRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssumeRole where
  toPath = Lude.const "/"

instance Lude.ToQuery AssumeRole where
  toQuery AssumeRole' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssumeRole" :: Lude.ByteString),
        "Version" Lude.=: ("2011-06-15" :: Lude.ByteString),
        "RoleSessionName" Lude.=: roleSessionName,
        "TransitiveTagKeys"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> transitiveTagKeys),
        "TokenCode" Lude.=: tokenCode,
        "PolicyArns"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> policyARNs),
        "DurationSeconds" Lude.=: durationSeconds,
        "Policy" Lude.=: policy,
        "ExternalId" Lude.=: externalId,
        "SerialNumber" Lude.=: serialNumber,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "RoleArn" Lude.=: roleARN
      ]

-- | Contains the response to a successful 'AssumeRole' request, including temporary AWS credentials that can be used to make AWS requests.
--
-- /See:/ 'mkAssumeRoleResponse' smart constructor.
data AssumeRoleResponse = AssumeRoleResponse'
  { -- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
    packedPolicySize :: Lude.Maybe Lude.Natural,
    -- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
    credentials :: Lude.Maybe AuthEnv,
    -- | The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
    assumedRoleUser :: Lude.Maybe AssumedRoleUser,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssumeRoleResponse' with the minimum fields required to make a request.
--
-- * 'packedPolicySize' - A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
-- * 'credentials' - The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
-- * 'assumedRoleUser' - The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
-- * 'responseStatus' - The response status code.
mkAssumeRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssumeRoleResponse
mkAssumeRoleResponse pResponseStatus_ =
  AssumeRoleResponse'
    { packedPolicySize = Lude.Nothing,
      credentials = Lude.Nothing,
      assumedRoleUser = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
--
-- /Note:/ Consider using 'packedPolicySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arrsPackedPolicySize :: Lens.Lens' AssumeRoleResponse (Lude.Maybe Lude.Natural)
arrsPackedPolicySize = Lens.lens (packedPolicySize :: AssumeRoleResponse -> Lude.Maybe Lude.Natural) (\s a -> s {packedPolicySize = a} :: AssumeRoleResponse)
{-# DEPRECATED arrsPackedPolicySize "Use generic-lens or generic-optics with 'packedPolicySize' instead." #-}

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arrsCredentials :: Lens.Lens' AssumeRoleResponse (Lude.Maybe AuthEnv)
arrsCredentials = Lens.lens (credentials :: AssumeRoleResponse -> Lude.Maybe AuthEnv) (\s a -> s {credentials = a} :: AssumeRoleResponse)
{-# DEPRECATED arrsCredentials "Use generic-lens or generic-optics with 'credentials' instead." #-}

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
--
-- /Note:/ Consider using 'assumedRoleUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arrsAssumedRoleUser :: Lens.Lens' AssumeRoleResponse (Lude.Maybe AssumedRoleUser)
arrsAssumedRoleUser = Lens.lens (assumedRoleUser :: AssumeRoleResponse -> Lude.Maybe AssumedRoleUser) (\s a -> s {assumedRoleUser = a} :: AssumeRoleResponse)
{-# DEPRECATED arrsAssumedRoleUser "Use generic-lens or generic-optics with 'assumedRoleUser' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arrsResponseStatus :: Lens.Lens' AssumeRoleResponse Lude.Int
arrsResponseStatus = Lens.lens (responseStatus :: AssumeRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssumeRoleResponse)
{-# DEPRECATED arrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
