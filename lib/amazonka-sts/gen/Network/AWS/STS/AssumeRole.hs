{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.AssumeRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of temporary security credentials (consisting of an access key ID, a secret access key, and a security token) that you can use to access AWS resources that you might not normally have access to. Typically, you use @AssumeRole@ for cross-account access or federation. For a comparison of @AssumeRole@ with the other APIs that produce temporary credentials, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html Requesting Temporary Security Credentials> and <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_request.html#stsapi_comparison Comparing the AWS STS APIs> in the /IAM User Guide/ .
--
--
-- __Important:__ You cannot call @AssumeRole@ by using AWS root account credentials; access is denied. You must use credentials for an IAM user or an IAM role to call @AssumeRole@ .
--
-- For cross-account access, imagine that you own multiple accounts and need to access resources in each account. You could create long-term credentials in each account to access those resources. However, managing all those credentials and remembering which one can access which account can be time consuming. Instead, you can create one set of long-term credentials in one account and then use temporary security credentials to access all the other accounts by assuming roles in those accounts. For more information about roles, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/roles-toplevel.html IAM Roles (Delegation and Federation)> in the /IAM User Guide/ .
--
-- For federation, you can, for example, grant single sign-on access to the AWS Management Console. If you already have an identity and authentication system in your corporate network, you don't have to recreate user identities in AWS in order to grant those user identities access to AWS. Instead, after a user has been authenticated, you call @AssumeRole@ (and specify the role with the appropriate permissions) to get temporary security credentials for that user. With those temporary security credentials, you construct a sign-in URL that users can use to access the console. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html#sts-introduction Common Scenarios for Temporary Credentials> in the /IAM User Guide/ .
--
-- By default, the temporary security credentials created by @AssumeRole@ last for one hour. However, you can use the optional @DurationSeconds@ parameter to specify the duration of your session. You can provide a value from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. To learn how to view the maximum value for your role, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . The maximum session duration limit applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
--
-- The temporary security credentials created by @AssumeRole@ can be used to make API calls to any AWS service with the following exception: you cannot call the STS service's @GetFederationToken@ or @GetSessionToken@ APIs.
--
-- Optionally, you can pass an IAM access policy to this operation. If you choose not to pass a policy, the temporary security credentials that are returned by the operation have the permissions that are defined in the access policy of the role that is being assumed. If you pass a policy to this operation, the temporary security credentials that are returned by the operation have the permissions that are allowed by both the access policy of the role that is being assumed, /__and__ / the policy that you pass. This gives you a way to further restrict the permissions for the resulting temporary security credentials. You cannot use the passed policy to grant permissions that are in excess of those allowed by the access policy of the role that is being assumed. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_assumerole.html Permissions for AssumeRole, AssumeRoleWithSAML, and AssumeRoleWithWebIdentity> in the /IAM User Guide/ .
--
-- To assume a role, your AWS account must be trusted by the role. The trust relationship is defined in the role's trust policy when the role is created. That trust policy states which accounts are allowed to delegate access to this account's role.
--
-- The user who wants to access the role must also have permissions delegated from the role's administrator. If the user is in a different account than the role, then the user's administrator must attach a policy that allows the user to call AssumeRole on the ARN of the role in the other account. If the user is in the same account as the role, then you can either attach a policy to the user (identical to the previous different account user), or you can add the user as a principal directly in the role's trust policy. In this case, the trust policy acts as the only resource-based policy in IAM, and users in the same account as the role do not need explicit permission to assume the role. For more information about trust policies and resource-based policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html IAM Policies> in the /IAM User Guide/ .
--
-- __Using MFA with AssumeRole__
--
-- You can optionally include multi-factor authentication (MFA) information when you call @AssumeRole@ . This is useful for cross-account scenarios in which you want to make sure that the user who is assuming the role has been authenticated using an AWS MFA device. In that scenario, the trust policy of the role being assumed includes a condition that tests for MFA authentication; if the caller does not include valid MFA information, the request to assume the role is denied. The condition in a trust policy that tests for MFA authentication might look like the following example.
--
-- @"Condition": {"Bool": {"aws:MultiFactorAuthPresent": true}}@
--
-- For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/MFAProtectedAPI.html Configuring MFA-Protected API Access> in the /IAM User Guide/ guide.
--
-- To use MFA with @AssumeRole@ , you pass values for the @SerialNumber@ and @TokenCode@ parameters. The @SerialNumber@ value identifies the user's hardware or virtual MFA device. The @TokenCode@ is the time-based one-time password (TOTP) that the MFA devices produces.
--
module Network.AWS.STS.AssumeRole
    (
    -- * Creating a Request
      assumeRole
    , AssumeRole
    -- * Request Lenses
    , arTokenCode
    , arDurationSeconds
    , arPolicy
    , arExternalId
    , arSerialNumber
    , arRoleARN
    , arRoleSessionName

    -- * Destructuring the Response
    , assumeRoleResponse
    , AssumeRoleResponse
    -- * Response Lenses
    , arrsPackedPolicySize
    , arrsCredentials
    , arrsAssumedRoleUser
    , arrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.STS.Types
import Network.AWS.STS.Types.Product

-- | /See:/ 'assumeRole' smart constructor.
data AssumeRole = AssumeRole'
  { _arTokenCode       :: !(Maybe Text)
  , _arDurationSeconds :: !(Maybe Nat)
  , _arPolicy          :: !(Maybe Text)
  , _arExternalId      :: !(Maybe Text)
  , _arSerialNumber    :: !(Maybe Text)
  , _arRoleARN         :: !Text
  , _arRoleSessionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssumeRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arTokenCode' - The value provided by the MFA device, if the trust policy of the role being assumed requires MFA (that is, if the policy includes a condition that tests for MFA). If the role being assumed requires MFA and if the @TokenCode@ value is missing or expired, the @AssumeRole@ call returns an "access denied" error. The format for this parameter, as described by its regex pattern, is a sequence of six numeric digits.
--
-- * 'arDurationSeconds' - The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . By default, the value is set to 3600 seconds.
--
-- * 'arPolicy' - An IAM policy in JSON format. This parameter is optional. If you pass a policy, the temporary security credentials that are returned by the operation have the permissions that are allowed by both (the intersection of) the access policy of the role that is being assumed, /and/ the policy that you pass. This gives you a way to further restrict the permissions for the resulting temporary security credentials. You cannot use the passed policy to grant permissions that are in excess of those allowed by the access policy of the role that is being assumed. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_assumerole.html Permissions for AssumeRole, AssumeRoleWithSAML, and AssumeRoleWithWebIdentity> in the /IAM User Guide/ . The format for this parameter, as described by its regex pattern, is a string of characters up to 2048 characters in length. The characters can be any ASCII character from the space character to the end of the valid character list (\u0020-\u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
--
-- * 'arExternalId' - A unique identifier that is used by third parties when assuming roles in their customers' accounts. For each role that the third party can assume, they should instruct their customers to ensure the role's trust policy checks for the external ID that the third party generated. Each time the third party assumes the role, they should pass the customer's external ID. The external ID is useful in order to help third parties bind a role to the customer who created it. For more information about the external ID, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party> in the /IAM User Guide/ . The regex used to validated this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@:/-
--
-- * 'arSerialNumber' - The identification number of the MFA device that is associated with the user who is making the @AssumeRole@ call. Specify this value if the trust policy of the role being assumed includes a condition that requires MFA authentication. The value is either the serial number for a hardware device (such as @GAHT12345678@ ) or an Amazon Resource Name (ARN) for a virtual device (such as @arn:aws:iam::123456789012:mfa/user@ ). The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
--
-- * 'arRoleARN' - The Amazon Resource Name (ARN) of the role to assume.
--
-- * 'arRoleSessionName' - An identifier for the assumed role session. Use the role session name to uniquely identify a session when the same role is assumed by different principals or for different reasons. In cross-account scenarios, the role session name is visible to, and can be logged by the account that owns the role. The role session name is also used in the ARN of the assumed role principal. This means that subsequent cross-account API requests using the temporary security credentials will expose the role session name to the external account in their CloudTrail logs. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
assumeRole
    :: Text -- ^ 'arRoleARN'
    -> Text -- ^ 'arRoleSessionName'
    -> AssumeRole
assumeRole pRoleARN_ pRoleSessionName_ =
  AssumeRole'
    { _arTokenCode = Nothing
    , _arDurationSeconds = Nothing
    , _arPolicy = Nothing
    , _arExternalId = Nothing
    , _arSerialNumber = Nothing
    , _arRoleARN = pRoleARN_
    , _arRoleSessionName = pRoleSessionName_
    }


-- | The value provided by the MFA device, if the trust policy of the role being assumed requires MFA (that is, if the policy includes a condition that tests for MFA). If the role being assumed requires MFA and if the @TokenCode@ value is missing or expired, the @AssumeRole@ call returns an "access denied" error. The format for this parameter, as described by its regex pattern, is a sequence of six numeric digits.
arTokenCode :: Lens' AssumeRole (Maybe Text)
arTokenCode = lens _arTokenCode (\ s a -> s{_arTokenCode = a})

-- | The duration, in seconds, of the role session. The value can range from 900 seconds (15 minutes) up to the maximum session duration setting for the role. This setting can have a value from 1 hour to 12 hours. If you specify a value higher than this setting, the operation fails. For example, if you specify a session duration of 12 hours, but your administrator set the maximum session duration to 6 hours, your operation fails. To learn how to view the maximum value for your role, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html#id_roles_use_view-role-max-session View the Maximum Session Duration Setting for a Role> in the /IAM User Guide/ . By default, the value is set to 3600 seconds.
arDurationSeconds :: Lens' AssumeRole (Maybe Natural)
arDurationSeconds = lens _arDurationSeconds (\ s a -> s{_arDurationSeconds = a}) . mapping _Nat

-- | An IAM policy in JSON format. This parameter is optional. If you pass a policy, the temporary security credentials that are returned by the operation have the permissions that are allowed by both (the intersection of) the access policy of the role that is being assumed, /and/ the policy that you pass. This gives you a way to further restrict the permissions for the resulting temporary security credentials. You cannot use the passed policy to grant permissions that are in excess of those allowed by the access policy of the role that is being assumed. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_control-access_assumerole.html Permissions for AssumeRole, AssumeRoleWithSAML, and AssumeRoleWithWebIdentity> in the /IAM User Guide/ . The format for this parameter, as described by its regex pattern, is a string of characters up to 2048 characters in length. The characters can be any ASCII character from the space character to the end of the valid character list (\u0020-\u00FF). It can also include the tab (\u0009), linefeed (\u000A), and carriage return (\u000D) characters.
arPolicy :: Lens' AssumeRole (Maybe Text)
arPolicy = lens _arPolicy (\ s a -> s{_arPolicy = a})

-- | A unique identifier that is used by third parties when assuming roles in their customers' accounts. For each role that the third party can assume, they should instruct their customers to ensure the role's trust policy checks for the external ID that the third party generated. Each time the third party assumes the role, they should pass the customer's external ID. The external ID is useful in order to help third parties bind a role to the customer who created it. For more information about the external ID, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_create_for-user_externalid.html How to Use an External ID When Granting Access to Your AWS Resources to a Third Party> in the /IAM User Guide/ . The regex used to validated this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@:/-
arExternalId :: Lens' AssumeRole (Maybe Text)
arExternalId = lens _arExternalId (\ s a -> s{_arExternalId = a})

-- | The identification number of the MFA device that is associated with the user who is making the @AssumeRole@ call. Specify this value if the trust policy of the role being assumed includes a condition that requires MFA authentication. The value is either the serial number for a hardware device (such as @GAHT12345678@ ) or an Amazon Resource Name (ARN) for a virtual device (such as @arn:aws:iam::123456789012:mfa/user@ ). The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
arSerialNumber :: Lens' AssumeRole (Maybe Text)
arSerialNumber = lens _arSerialNumber (\ s a -> s{_arSerialNumber = a})

-- | The Amazon Resource Name (ARN) of the role to assume.
arRoleARN :: Lens' AssumeRole Text
arRoleARN = lens _arRoleARN (\ s a -> s{_arRoleARN = a})

-- | An identifier for the assumed role session. Use the role session name to uniquely identify a session when the same role is assumed by different principals or for different reasons. In cross-account scenarios, the role session name is visible to, and can be logged by the account that owns the role. The role session name is also used in the ARN of the assumed role principal. This means that subsequent cross-account API requests using the temporary security credentials will expose the role session name to the external account in their CloudTrail logs. The regex used to validate this parameter is a string of characters consisting of upper- and lower-case alphanumeric characters with no spaces. You can also include underscores or any of the following characters: =,.@-
arRoleSessionName :: Lens' AssumeRole Text
arRoleSessionName = lens _arRoleSessionName (\ s a -> s{_arRoleSessionName = a})

instance AWSRequest AssumeRole where
        type Rs AssumeRole = AssumeRoleResponse
        request = postQuery sts
        response
          = receiveXMLWrapper "AssumeRoleResult"
              (\ s h x ->
                 AssumeRoleResponse' <$>
                   (x .@? "PackedPolicySize") <*> (x .@? "Credentials")
                     <*> (x .@? "AssumedRoleUser")
                     <*> (pure (fromEnum s)))

instance Hashable AssumeRole where

instance NFData AssumeRole where

instance ToHeaders AssumeRole where
        toHeaders = const mempty

instance ToPath AssumeRole where
        toPath = const "/"

instance ToQuery AssumeRole where
        toQuery AssumeRole'{..}
          = mconcat
              ["Action" =: ("AssumeRole" :: ByteString),
               "Version" =: ("2011-06-15" :: ByteString),
               "TokenCode" =: _arTokenCode,
               "DurationSeconds" =: _arDurationSeconds,
               "Policy" =: _arPolicy, "ExternalId" =: _arExternalId,
               "SerialNumber" =: _arSerialNumber,
               "RoleArn" =: _arRoleARN,
               "RoleSessionName" =: _arRoleSessionName]

-- | Contains the response to a successful 'AssumeRole' request, including temporary AWS credentials that can be used to make AWS requests.
--
--
--
-- /See:/ 'assumeRoleResponse' smart constructor.
data AssumeRoleResponse = AssumeRoleResponse'
  { _arrsPackedPolicySize :: !(Maybe Nat)
  , _arrsCredentials      :: !(Maybe AuthEnv)
  , _arrsAssumedRoleUser  :: !(Maybe AssumedRoleUser)
  , _arrsResponseStatus   :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssumeRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arrsPackedPolicySize' - A percentage value that indicates the size of the policy in packed form. The service rejects any policy with a packed size greater than 100 percent, which means the policy exceeded the allowed space.
--
-- * 'arrsCredentials' - The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token. __Note:__ The size of the security token that STS APIs return is not fixed. We strongly recommend that you make no assumptions about the maximum size. As of this writing, the typical size is less than 4096 bytes, but that can vary. Also, future updates to AWS might require larger sizes.
--
-- * 'arrsAssumedRoleUser' - The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
--
-- * 'arrsResponseStatus' - -- | The response status code.
assumeRoleResponse
    :: Int -- ^ 'arrsResponseStatus'
    -> AssumeRoleResponse
assumeRoleResponse pResponseStatus_ =
  AssumeRoleResponse'
    { _arrsPackedPolicySize = Nothing
    , _arrsCredentials = Nothing
    , _arrsAssumedRoleUser = Nothing
    , _arrsResponseStatus = pResponseStatus_
    }


-- | A percentage value that indicates the size of the policy in packed form. The service rejects any policy with a packed size greater than 100 percent, which means the policy exceeded the allowed space.
arrsPackedPolicySize :: Lens' AssumeRoleResponse (Maybe Natural)
arrsPackedPolicySize = lens _arrsPackedPolicySize (\ s a -> s{_arrsPackedPolicySize = a}) . mapping _Nat

-- | The temporary security credentials, which include an access key ID, a secret access key, and a security (or session) token. __Note:__ The size of the security token that STS APIs return is not fixed. We strongly recommend that you make no assumptions about the maximum size. As of this writing, the typical size is less than 4096 bytes, but that can vary. Also, future updates to AWS might require larger sizes.
arrsCredentials :: Lens' AssumeRoleResponse (Maybe AuthEnv)
arrsCredentials = lens _arrsCredentials (\ s a -> s{_arrsCredentials = a})

-- | The Amazon Resource Name (ARN) and the assumed role ID, which are identifiers that you can use to refer to the resulting temporary security credentials. For example, you can reference these credentials as a principal in a resource-based policy by using the ARN or assumed role ID. The ARN and ID include the @RoleSessionName@ that you specified when you called @AssumeRole@ .
arrsAssumedRoleUser :: Lens' AssumeRoleResponse (Maybe AssumedRoleUser)
arrsAssumedRoleUser = lens _arrsAssumedRoleUser (\ s a -> s{_arrsAssumedRoleUser = a})

-- | -- | The response status code.
arrsResponseStatus :: Lens' AssumeRoleResponse Int
arrsResponseStatus = lens _arrsResponseStatus (\ s a -> s{_arrsResponseStatus = a})

instance NFData AssumeRoleResponse where
