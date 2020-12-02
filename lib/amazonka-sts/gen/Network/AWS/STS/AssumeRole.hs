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
-- Module      : Network.AWS.STS.AssumeRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials that you can use to access AWS resources that you might not normally have access to. These temporary credentials consist of an access key ID, a secret access key, and a security token. Typically, you use @AssumeRole@ within your account or for cross-account access. For a comparison of @AssumeRole@ with other API operations that produce temporary credentials, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials> and <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS API operations> in the /IAM User Guide/ .
--
--
-- /Important:/ You cannot use AWS account root user credentials to call @AssumeRole@ . You must use credentials for an IAM user or an IAM role to call @AssumeRole@ .
--
-- For cross-account access, imagine that you own multiple accounts and need to access resources in each account. You could create long-term credentials in each account to access those resources. However, managing all those credentials and remembering which one can access which account can be time consuming. Instead, you can create one set of long-term credentials in one account. Then use temporary security credentials to access all the other accounts by assuming roles in those accounts. For more information about roles, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM Roles> in the /IAM User Guide/ .
--
-- __Session Duration__
--
-- By default, the temporary security credentials created by @AssumeRole@ last for one hour. However, you can use the optional @DurationSeconds@ parameter to specify the duration of your session. You can provide a value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . The maximum session duration limit applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI commands. However the limit does not apply when you use those operations to create a console URL. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
--
-- __Permissions__
--
-- The temporary security credentials created by @AssumeRole@ can be used to make API calls to any AWS service with the following exception: You cannot call the AWS STS @GetFederationToken@ or @GetSessionToken@ API operations.
--
-- (Optional) You can pass inline or managed <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session session policies> to this operation. You can pass a single JSON policy document to use as an inline session policy. You can also specify up to 10 managed policies to use as managed session policies. The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
--
-- To assume a role from a different account, your AWS account must be trusted by the role. The trust relationship is defined in the role's trust policy when the role is created. That trust policy states which accounts are allowed to delegate that access to users in the account.
--
-- A user who wants to access a role in a different account must also have permissions that are delegated from the user account administrator. The administrator must attach a policy that allows the user to call @AssumeRole@ for the ARN of the role in the other account. If the user is in the same account as the role, then you can do either of the following:
--
--     * Attach a policy to the user (identical to the previous user in a different account).
--
--     * Add the user as a principal directly in the role's trust policy.
--
--
--
-- In this case, the trust policy acts as an IAM resource-based policy. Users in the same account as the role do not need explicit permission to assume the role. For more information about trust policies and resource-based policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies> in the /IAM User Guide/ .
--
-- __Tags__
--
-- (Optional) You can pass tag key-value pairs to your session. These tags are called session tags. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
--
-- An administrator must grant you the permissions necessary to pass session tags. The administrator can also create granular permissions to allow you to pass only specific session tags. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/tutorial_attribute-based-access-control.html Tutorial: Using Tags for Attribute-Based Access Control> in the /IAM User Guide/ .
--
-- You can set the session tags as transitive. Transitive tags persist during role chaining. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ .
--
-- __Using MFA with AssumeRole__
--
-- (Optional) You can include multi-factor authentication (MFA) information when you call @AssumeRole@ . This is useful for cross-account scenarios to ensure that the user that assumes the role has been authenticated with an AWS MFA device. In that scenario, the trust policy of the role being assumed includes a condition that tests for MFA authentication. If the caller does not include valid MFA information, the request to assume the role is denied. The condition in a trust policy that tests for MFA authentication might look like the following example.
--
-- @"Condition": {"Bool": {"aws:MultiFactorAuthPresent": true}}@
--
-- For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/MFAProtectedAPI.html Configuring MFA-Protected API Access> in the /IAM User Guide/ guide.
--
-- To use MFA with @AssumeRole@ , you pass values for the @SerialNumber@ and @TokenCode@ parameters. The @SerialNumber@ value identifies the user's hardware or virtual MFA device. The @TokenCode@ is the time-based one-time password (TOTP) that the MFA device produces.
module Network.AWS.STS.AssumeRole
  ( -- * Creating a Request
    assumeRole,
    AssumeRole,

    -- * Request Lenses
    arTransitiveTagKeys,
    arTokenCode,
    arPolicyARNs,
    arDurationSeconds,
    arPolicy,
    arExternalId,
    arSerialNumber,
    arTags,
    arRoleARN,
    arRoleSessionName,

    -- * Destructuring the Response
    assumeRoleResponse,
    AssumeRoleResponse,

    -- * Response Lenses
    arrsPackedPolicySize,
    arrsCredentials,
    arrsAssumedRoleUser,
    arrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.STS.Types

-- | /See:/ 'assumeRole' smart constructor.
data AssumeRole = AssumeRole'
  { _arTransitiveTagKeys ::
      !(Maybe [Text]),
    _arTokenCode :: !(Maybe Text),
    _arPolicyARNs :: !(Maybe [PolicyDescriptorType]),
    _arDurationSeconds :: !(Maybe Nat),
    _arPolicy :: !(Maybe Text),
    _arExternalId :: !(Maybe Text),
    _arSerialNumber :: !(Maybe Text),
    _arTags :: !(Maybe [Tag]),
    _arRoleARN :: !Text,
    _arRoleSessionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssumeRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arTransitiveTagKeys' - A list of keys for session tags that you want to set as transitive. If you set a tag key as transitive, the corresponding key and value passes to subsequent sessions in a role chain. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ . This parameter is optional. When you set session tags as transitive, the session policy and session tags packed binary limit is not affected. If you choose not to specify a transitive tag key, then no tags are passed from this session to any subsequent sessions.
--
-- * 'arTokenCode' - The value provided by the MFA device, if the trust policy of the role being assumed requires MFA (that is, if the policy includes a condition that tests for MFA). If the role being assumed requires MFA and if the @TokenCode@ value is missing or expired, the @AssumeRole@ call returns an "access denied" error. The format for this parameter, as described by its regex pattern, is a sequence of six numeric digits.
--
-- * 'arPolicyARNs' - The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role. This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
--
-- * 'arDurationSeconds' - The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . By default, the value is set to @3600@ seconds.
--
-- * 'arPolicy' - An IAM policy in JSON format that you want to use as an inline session policy. This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ . The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- * 'arExternalId' - A unique identifier that might be required when you assume a role in another account. If the administrator of the account to which the role belongs provided you with an external ID, then provide that value in the @ExternalId@ parameter. This value can be any string, such as a passphrase or account number. A cross-account role is usually set up to trust everyone in an account. Therefore, the administrator of the trusting account might send an external ID to the administrator of the trusted account. That way, only someone with the ID can assume the role, rather than everyone in the account. For more information about the external ID, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party> in the /IAM User Guide/ . The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@:/-
--
-- * 'arSerialNumber' - The identification number of the MFA device that is associated with the user who is making the @AssumeRole@ call. Specify this value if the trust policy of the role being assumed includes a condition that requires MFA authentication. The value is either the serial number for a hardware device (such as @GAHT12345678@ ) or an Amazon Resource Name (ARN) for a virtual device (such as @arn:aws:iam::123456789012:mfa/user@ ). The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
--
-- * 'arTags' - A list of session tags that you want to pass. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging AWS STS Sessions> in the /IAM User Guide/ . This parameter is optional. You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters, and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ . You can pass a session tag with the same key as a tag that is already attached to the role. When you do, session tags override a role tag with the same key.  Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the role has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the role tag. Additionally, if you used temporary credentials to perform this operation, the new session inherits any transitive session tags from the calling session. If you pass a session tag with the same key as an inherited tag, the operation fails. To view the inherited tags for a session, see the AWS CloudTrail logs. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail> in the /IAM User Guide/ .
--
-- * 'arRoleARN' - The Amazon Resource Name (ARN) of the role to assume.
--
-- * 'arRoleSessionName' - An identifier for the assumed role session. Use the role session name to uniquely identify a session when the same role is assumed by different principals or for different reasons. In cross-account scenarios, the role session name is visible to, and can be logged by the account that owns the role. The role session name is also used in the ARN of the assumed role principal. This means that subsequent cross-account API requests that use the temporary security credentials will expose the role session name to the external account in their AWS CloudTrail logs. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
assumeRole ::
  -- | 'arRoleARN'
  Text ->
  -- | 'arRoleSessionName'
  Text ->
  AssumeRole
assumeRole pRoleARN_ pRoleSessionName_ =
  AssumeRole'
    { _arTransitiveTagKeys = Nothing,
      _arTokenCode = Nothing,
      _arPolicyARNs = Nothing,
      _arDurationSeconds = Nothing,
      _arPolicy = Nothing,
      _arExternalId = Nothing,
      _arSerialNumber = Nothing,
      _arTags = Nothing,
      _arRoleARN = pRoleARN_,
      _arRoleSessionName = pRoleSessionName_
    }

-- | A list of keys for session tags that you want to set as transitive. If you set a tag key as transitive, the corresponding key and value passes to subsequent sessions in a role chain. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html#id_session-tags_role-chaining Chaining Roles with Session Tags> in the /IAM User Guide/ . This parameter is optional. When you set session tags as transitive, the session policy and session tags packed binary limit is not affected. If you choose not to specify a transitive tag key, then no tags are passed from this session to any subsequent sessions.
arTransitiveTagKeys :: Lens' AssumeRole [Text]
arTransitiveTagKeys = lens _arTransitiveTagKeys (\s a -> s {_arTransitiveTagKeys = a}) . _Default . _Coerce

-- | The value provided by the MFA device, if the trust policy of the role being assumed requires MFA (that is, if the policy includes a condition that tests for MFA). If the role being assumed requires MFA and if the @TokenCode@ value is missing or expired, the @AssumeRole@ call returns an "access denied" error. The format for this parameter, as described by its regex pattern, is a sequence of six numeric digits.
arTokenCode :: Lens' AssumeRole (Maybe Text)
arTokenCode = lens _arTokenCode (\s a -> s {_arTokenCode = a})

-- | The Amazon Resource Names (ARNs) of the IAM managed policies that you want to use as managed session policies. The policies must exist in the same account as the role. This parameter is optional. You can provide up to 10 managed policy ARNs. However, the plain text that you use for both inline and managed session policies can't exceed 2,048 characters. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the AWS General Reference. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ .
arPolicyARNs :: Lens' AssumeRole [PolicyDescriptorType]
arPolicyARNs = lens _arPolicyARNs (\s a -> s {_arPolicyARNs = a}) . _Default . _Coerce

-- | The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . By default, the value is set to @3600@ seconds.
arDurationSeconds :: Lens' AssumeRole (Maybe Natural)
arDurationSeconds = lens _arDurationSeconds (\s a -> s {_arDurationSeconds = a}) . mapping _Nat

-- | An IAM policy in JSON format that you want to use as an inline session policy. This parameter is optional. Passing policies to this operation returns new temporary credentials. The resulting session's permissions are the intersection of the role's identity-based policy and the session policies. You can use the role's temporary credentials in subsequent AWS API calls to access resources in the account that owns the role. You cannot use session policies to grant more permissions than those allowed by the identity-based policy of the role that is being assumed. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#policies_session Session Policies> in the /IAM User Guide/ . The plain text that you use for both inline and managed session policies can't exceed 2,048 characters. The JSON policy characters can be any ASCII character from the space character to the end of the valid character list (\u0020 through \u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
arPolicy :: Lens' AssumeRole (Maybe Text)
arPolicy = lens _arPolicy (\s a -> s {_arPolicy = a})

-- | A unique identifier that might be required when you assume a role in another account. If the administrator of the account to which the role belongs provided you with an external ID, then provide that value in the @ExternalId@ parameter. This value can be any string, such as a passphrase or account number. A cross-account role is usually set up to trust everyone in an account. Therefore, the administrator of the trusting account might send an external ID to the administrator of the trusted account. That way, only someone with the ID can assume the role, rather than everyone in the account. For more information about the external ID, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party> in the /IAM User Guide/ . The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@:/-
arExternalId :: Lens' AssumeRole (Maybe Text)
arExternalId = lens _arExternalId (\s a -> s {_arExternalId = a})

-- | The identification number of the MFA device that is associated with the user who is making the @AssumeRole@ call. Specify this value if the trust policy of the role being assumed includes a condition that requires MFA authentication. The value is either the serial number for a hardware device (such as @GAHT12345678@ ) or an Amazon Resource Name (ARN) for a virtual device (such as @arn:aws:iam::123456789012:mfa/user@ ). The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
arSerialNumber :: Lens' AssumeRole (Maybe Text)
arSerialNumber = lens _arSerialNumber (\s a -> s {_arSerialNumber = a})

-- | A list of session tags that you want to pass. Each session tag consists of a key name and an associated value. For more information about session tags, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Tagging AWS STS Sessions> in the /IAM User Guide/ . This parameter is optional. You can pass up to 50 session tags. The plain text session tag keys can’t exceed 128 characters, and the values can’t exceed 256 characters. For these and additional limits, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html#reference_iam-limits-entity-length IAM and STS Character Limits> in the /IAM User Guide/ . You can pass a session tag with the same key as a tag that is already attached to the role. When you do, session tags override a role tag with the same key.  Tag key–value pairs are not case sensitive, but case is preserved. This means that you cannot have separate @Department@ and @department@ tag keys. Assume that the role has the @Department@ =@Marketing@ tag and you pass the @department@ =@engineering@ session tag. @Department@ and @department@ are not saved as separate tags, and the session tag passed in the request takes precedence over the role tag. Additionally, if you used temporary credentials to perform this operation, the new session inherits any transitive session tags from the calling session. If you pass a session tag with the same key as an inherited tag, the operation fails. To view the inherited tags for a session, see the AWS CloudTrail logs. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/session-tags.html#id_session-tags_ctlogs Viewing Session Tags in CloudTrail> in the /IAM User Guide/ .
arTags :: Lens' AssumeRole [Tag]
arTags = lens _arTags (\s a -> s {_arTags = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the role to assume.
arRoleARN :: Lens' AssumeRole Text
arRoleARN = lens _arRoleARN (\s a -> s {_arRoleARN = a})

-- | An identifier for the assumed role session. Use the role session name to uniquely identify a session when the same role is assumed by different principals or for different reasons. In cross-account scenarios, the role session name is visible to, and can be logged by the account that owns the role. The role session name is also used in the ARN of the assumed role principal. This means that subsequent cross-account API requests that use the temporary security credentials will expose the role session name to the external account in their AWS CloudTrail logs. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
arRoleSessionName :: Lens' AssumeRole Text
arRoleSessionName = lens _arRoleSessionName (\s a -> s {_arRoleSessionName = a})

instance AWSRequest AssumeRole where
  type Rs AssumeRole = AssumeRoleResponse
  request = postQuery sts
  response =
    receiveXMLWrapper
      "AssumeRoleResult"
      ( \s h x ->
          AssumeRoleResponse'
            <$> (x .@? "PackedPolicySize")
            <*> (x .@? "Credentials")
            <*> (x .@? "AssumedRoleUser")
            <*> (pure (fromEnum s))
      )

instance Hashable AssumeRole

instance NFData AssumeRole

instance ToHeaders AssumeRole where
  toHeaders = const mempty

instance ToPath AssumeRole where
  toPath = const "/"

instance ToQuery AssumeRole where
  toQuery AssumeRole' {..} =
    mconcat
      [ "Action" =: ("AssumeRole" :: ByteString),
        "Version" =: ("2011-06-15" :: ByteString),
        "TransitiveTagKeys"
          =: toQuery (toQueryList "member" <$> _arTransitiveTagKeys),
        "TokenCode" =: _arTokenCode,
        "PolicyArns" =: toQuery (toQueryList "member" <$> _arPolicyARNs),
        "DurationSeconds" =: _arDurationSeconds,
        "Policy" =: _arPolicy,
        "ExternalId" =: _arExternalId,
        "SerialNumber" =: _arSerialNumber,
        "Tags" =: toQuery (toQueryList "member" <$> _arTags),
        "RoleArn" =: _arRoleARN,
        "RoleSessionName" =: _arRoleSessionName
      ]

-- | Contains the response to a successful 'AssumeRole' request, including temporary AWS credentials that can be used to make AWS requests.
--
--
--
-- /See:/ 'assumeRoleResponse' smart constructor.
data AssumeRoleResponse = AssumeRoleResponse'
  { _arrsPackedPolicySize ::
      !(Maybe Nat),
    _arrsCredentials :: !(Maybe AuthEnv),
    _arrsAssumedRoleUser :: !(Maybe AssumedRoleUser),
    _arrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssumeRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arrsPackedPolicySize' - A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
--
-- * 'arrsCredentials' - The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
--
-- * 'arrsAssumedRoleUser' - The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
--
-- * 'arrsResponseStatus' - -- | The response status code.
assumeRoleResponse ::
  -- | 'arrsResponseStatus'
  Int ->
  AssumeRoleResponse
assumeRoleResponse pResponseStatus_ =
  AssumeRoleResponse'
    { _arrsPackedPolicySize = Nothing,
      _arrsCredentials = Nothing,
      _arrsAssumedRoleUser = Nothing,
      _arrsResponseStatus = pResponseStatus_
    }

-- | A percentage value that indicates the packed size of the session policies and session tags combined passed in the request. The request fails if the packed size is greater than 100 percent, which means the policies and tags exceeded the allowed space.
arrsPackedPolicySize :: Lens' AssumeRoleResponse (Maybe Natural)
arrsPackedPolicySize = lens _arrsPackedPolicySize (\s a -> s {_arrsPackedPolicySize = a}) . mapping _Nat

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token.
arrsCredentials :: Lens' AssumeRoleResponse (Maybe AuthEnv)
arrsCredentials = lens _arrsCredentials (\s a -> s {_arrsCredentials = a})

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
arrsAssumedRoleUser :: Lens' AssumeRoleResponse (Maybe AssumedRoleUser)
arrsAssumedRoleUser = lens _arrsAssumedRoleUser (\s a -> s {_arrsAssumedRoleUser = a})

-- | -- | The response status code.
arrsResponseStatus :: Lens' AssumeRoleResponse Int
arrsResponseStatus = lens _arrsResponseStatus (\s a -> s {_arrsResponseStatus = a})

instance NFData AssumeRoleResponse
