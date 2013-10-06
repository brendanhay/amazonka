{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.IAM
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | IAM is a web service that enables AWS customers to manage users and user
-- permissions under their AWS account.
module Network.AWS.IAM
   (
   -- * Actions
   -- ** AddRoleToInstanceProfile
     AddRoleToInstanceProfile              (..)
   , AddRoleToInstanceProfileResponse      (..)

   -- ** AddUserToGroup
   , AddUserToGroup                        (..)
   , AddUserToGroupResponse                (..)

   -- ** ChangePassword
   , ChangePassword                        (..)
   , ChangePasswordResponse                (..)

   -- ** CreateAccessKey
   , CreateAccessKey                       (..)
   , CreateAccessKeyResponse               (..)

   -- ** CreateAccountAlias
   , CreateAccountAlias                    (..)
   , CreateAccountAliasResponse            (..)

   -- ** CreateGroup
   , CreateGroup                           (..)
   , CreateGroupResponse                   (..)

   -- ** CreateInstanceProfile
   , CreateInstanceProfile                 (..)
   , CreateInstanceProfileResponse         (..)

   -- ** CreateLoginProfile
   , CreateLoginProfile                    (..)
   , CreateLoginProfileResponse            (..)

   -- ** CreateRole
   , CreateRole                            (..)
   , CreateRoleResponse                    (..)

   -- ** CreateUser
   , CreateUser                            (..)
   , CreateUserResponse                    (..)

   -- ** CreateVirtualMFADevice
   , CreateVirtualMFADevice                (..)
   , CreateVirtualMFADeviceResponse        (..)

   -- ** DeactivateMFADevice
   , DeactivateMFADevice                   (..)
   , DeactivateMFADeviceResponse           (..)

   -- ** DeleteAccessKey
   , DeleteAccessKey                       (..)
   , DeleteAccessKeyResponse               (..)

   -- ** DeleteAccountAlias
   , DeleteAccountAlias                    (..)
   , DeleteAccountAliasResponse            (..)

   -- ** DeleteAccountPasswordPolicy
   , DeleteAccountPasswordPolicy           (..)
   , DeleteAccountPasswordPolicyResponse   (..)

   -- ** DeleteGroup
   , DeleteGroup                           (..)
   , DeleteGroupResponse                   (..)

   -- ** DeleteGroupPolicy
   , DeleteGroupPolicy                     (..)
   , DeleteGroupPolicyResponse             (..)

   -- ** DeleteInstanceProfile
   , DeleteInstanceProfile                 (..)
   , DeleteInstanceProfileResponse         (..)

   -- ** DeleteLoginProfile
   , DeleteLoginProfile                    (..)
   , DeleteLoginProfileResponse            (..)

   -- ** DeleteRole
   , DeleteRole                            (..)
   , DeleteRoleResponse                    (..)

   -- ** DeleteRolePolicy
   , DeleteRolePolicy                      (..)
   , DeleteRolePolicyResponse              (..)

   -- ** DeleteServerCertificate
   , DeleteServerCertificate               (..)
   , DeleteServerCertificateResponse       (..)

   -- ** DeleteSigningCertificate
   , DeleteSigningCertificate              (..)
   , DeleteSigningCertificateResponse      (..)

   -- ** DeleteUser
   , DeleteUser                            (..)
   , DeleteUserResponse                    (..)

   -- ** DeleteUserPolicy
   , DeleteUserPolicy                      (..)
   , DeleteUserPolicyResponse              (..)

   -- ** DeleteVirtualMFADevice
   , DeleteVirtualMFADevice                (..)
   , DeleteVirtualMFADeviceResponse        (..)

   -- ** EnableMFADevice
   , EnableMFADevice                       (..)
   , EnableMFADeviceResponse               (..)

   -- ** GetAccountPasswordPolicy
   , GetAccountPasswordPolicy              (..)
   , GetAccountPasswordPolicyResponse      (..)

   -- ** GetAccountSummary
   , GetAccountSummary                     (..)
   , GetAccountSummaryResponse             (..)

   -- ** GetGroup
   , GetGroup                              (..)
   , GetGroupResponse                      (..)

   -- ** GetGroupPolicy
   , GetGroupPolicy                        (..)
   , GetGroupPolicyResponse                (..)

   -- ** GetInstanceProfile
   , GetInstanceProfile                    (..)
   , GetInstanceProfileResponse            (..)

   -- ** GetLoginProfile
   , GetLoginProfile                       (..)
   , GetLoginProfileResponse               (..)

   -- ** GetRole
   , GetRole                               (..)
   , GetRoleResponse                       (..)

   -- ** GetRolePolicy
   , GetRolePolicy                         (..)
   , GetRolePolicyResponse                 (..)

   -- ** GetServerCertificate
   , GetServerCertificate                  (..)
   , GetServerCertificateResponse          (..)

   -- ** GetUser
   , GetUser                               (..)
   , GetUserResponse                       (..)

   -- ** GetUserPolicy
   , GetUserPolicy                         (..)
   , GetUserPolicyResponse                 (..)

   -- ** ListAccessKeys
   , ListAccessKeys                        (..)
   , ListAccessKeysResponse                (..)

   -- ** ListAccountAliases
   , ListAccountAliases                    (..)
   , ListAccountAliasesResponse            (..)

   -- ** ListGroupPolicies
   , ListGroupPolicies                     (..)
   , ListGroupPoliciesResponse             (..)

   -- ** ListGroups
   , ListGroups                            (..)
   , ListGroupsResponse                    (..)

   -- ** ListGroupsForUser
   , ListGroupsForUser                     (..)
   , ListGroupsForUserResponse             (..)

   -- ** ListInstanceProfiles
   , ListInstanceProfiles                  (..)
   , ListInstanceProfilesResponse          (..)

   -- ** ListInstanceProfilesForRole
   , ListInstanceProfilesForRole           (..)
   , ListInstanceProfilesForRoleResponse   (..)

   -- ** ListMFADevices
   , ListMFADevices                        (..)
   , ListMFADevicesResponse                (..)

   -- ** ListRolePolicies
   , ListRolePolicies                      (..)
   , ListRolePoliciesResponse              (..)

   -- ** ListRoles
   , ListRoles                             (..)
   , ListRolesResponse                     (..)

   -- ** ListServerCertificates
   , ListServerCertificates                (..)
   , ListServerCertificatesResponse        (..)

   -- ** ListSigningCertificates
   , ListSigningCertificates               (..)
   , ListSigningCertificatesResponse       (..)

   -- ** ListUserPolicies
   , ListUserPolicies                      (..)
   , ListUserPoliciesResponse              (..)

   -- ** ListUsers
   , ListUsers                             (..)
   , ListUsersResponse                     (..)

   -- ** ListVirtualMFADevices
   , ListVirtualMFADevices                 (..)
   , ListVirtualMFADevicesResponse         (..)

   -- ** PutGroupPolicy
   , PutGroupPolicy                        (..)
   , PutGroupPolicyResponse                (..)

   -- ** PutRolePolicy
   , PutRolePolicy                         (..)
   , PutRolePolicyResponse                 (..)

   -- ** PutUserPolicy
   , PutUserPolicy                         (..)
   , PutUserPolicyResponse                 (..)

   -- ** RemoveRoleFromInstanceProfile
   , RemoveRoleFromInstanceProfile         (..)
   , RemoveRoleFromInstanceProfileResponse (..)

   -- ** RemoveUserFromGroup
   , RemoveUserFromGroup                   (..)
   , RemoveUserFromGroupResponse           (..)

   -- ** ResyncMFADevice
   , ResyncMFADevice                       (..)
   , ResyncMFADeviceResponse               (..)

   -- ** UpdateAccessKey
   , UpdateAccessKey                       (..)
   , UpdateAccessKeyResponse               (..)

   -- ** UpdateAccountPasswordPolicy
   , UpdateAccountPasswordPolicy           (..)
   , UpdateAccountPasswordPolicyResponse   (..)

   -- ** UpdateAssumeRolePolicy
   , UpdateAssumeRolePolicy                (..)
   , UpdateAssumeRolePolicyResponse        (..)

   -- ** UpdateGroup
   , UpdateGroup                           (..)
   , UpdateGroupResponse                   (..)

   -- ** UpdateLoginProfile
   , UpdateLoginProfile                    (..)
   , UpdateLoginProfileResponse            (..)

   -- ** UpdateServerCertificate
   , UpdateServerCertificate               (..)
   , UpdateServerCertificateResponse       (..)

   -- ** UpdateSigningCertificate
   , UpdateSigningCertificate              (..)
   , UpdateSigningCertificateResponse      (..)

   -- ** UpdateUser
   , UpdateUser                            (..)
   , UpdateUserResponse                    (..)

   -- ** UploadServerCertificate
   , UploadServerCertificate               (..)
   , UploadServerCertificateResponse       (..)

   -- ** UploadSigningCertificate
   , UploadSigningCertificate              (..)
   , UploadSigningCertificateResponse      (..)

   -- * Data Types
   , module Network.AWS.IAM.Types
   ) where

import Data.ByteString       (ByteString)
import Data.Text             (Text)
import Network.AWS.IAM.Types
import Network.AWS.Internal
import Network.Http.Client   (Method(..))

qry :: IsQuery a => Method -> ByteString -> a -> RawRequest
qry meth act q = queryAppend (queryRequest iamService meth "/" q)
    [ ("Action",  act)
    , ("Version", sPack iamVersion)
    ]

--
-- Actions
--

-- | Adds the specified role to the specified instance profile. For more
-- information about roles, go to Working with Roles. For more information
-- about instance profiles, go to About Instance Profiles.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddRoleToInstanceProfile.html>
data AddRoleToInstanceProfile = AddRoleToInstanceProfile
    { artipInstanceProfileName :: !Text
      -- ^ Name of the instance profile to update.
    , artipRoleName            :: !Text
      -- ^ Name of the role to add.
    } deriving (Eq, Show, Generic)

instance IsQuery AddRoleToInstanceProfile

instance Rq AddRoleToInstanceProfile where
    type Er AddRoleToInstanceProfile = IAMError
    type Rs AddRoleToInstanceProfile = AddRoleToInstanceProfileResponse
    request = qry GET "AddRoleToInstanceProfile"

data AddRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse
    { artiprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML AddRoleToInstanceProfileResponse where
    xmlPickler = withNS iamNS

-- | Adds the specified user to the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddUserToGroup.html>
data AddUserToGroup = AddUserToGroup
    { autgGroupName :: !Text
      -- ^ Name of the group to update.
    , autgUserName  :: !Text
      -- ^ Name of the user to add.
    } deriving (Eq, Show, Generic)

instance IsQuery AddUserToGroup

instance Rq AddUserToGroup where
    type Er AddUserToGroup = IAMError
    type Rs AddUserToGroup = AddUserToGroupResponse
    request = qry GET "AddUserToGroup"

data AddUserToGroupResponse = AddUserToGroupResponse
    { autgrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML AddUserToGroupResponse where
    xmlPickler = withNS iamNS

-- | Changes the password of the IAM user calling ChangePassword. The root
-- account password is not affected by this action. For information about
-- modifying passwords, see Managing Passwords.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ChangePassword.html>
data ChangePassword = ChangePassword
    { cpNewPassword :: !Text
      -- ^ Type: String
    , cpOldPassword :: !Text
      -- ^ Type: String
    } deriving (Eq, Show, Generic)

instance IsQuery ChangePassword

instance Rq ChangePassword where
    type Er ChangePassword = IAMError
    type Rs ChangePassword = ChangePasswordResponse
    request = qry GET "ChangePassword"

data ChangePasswordResponse = ChangePasswordResponse
    { cprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML ChangePasswordResponse where
    xmlPickler = withNS iamNS

-- | Creates a new AWS Secret Access Key and corresponding AWS Access Key ID for
-- the specified user. The default status for new keys is Active. If you do
-- not specify a user name, IAM determines the user name implicitly based on
-- the AWS Access Key ID signing the request. Because this action works for
-- access keys under the AWS account, you can use this API to manage root
-- credentials even if the AWS account has no associated users. For
-- information about limits on the number of keys you can create, see
-- Limitations on IAM Entities in Using AWS Identity and Access Management.
-- Important To ensure the security of your AWS account, the Secret Access Key
-- is accessible only during key and user creation. You must save the key (for
-- example, in a text file) if you want to be able to access it again. If a
-- secret key is lost, you can delete the access keys for the associated user
-- and then create new keys.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateAccessKey.html>
data CreateAccessKey = CreateAccessKey
    { cakUserName :: Maybe Text
      -- ^ The user name that the new key will belong to.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateAccessKey

instance Rq CreateAccessKey where
    type Er CreateAccessKey = IAMError
    type Rs CreateAccessKey = CreateAccessKeyResponse
    request = qry GET "CreateAccessKey"

data CreateAccessKeyResponse = CreateAccessKeyResponse
    { cakrResponseMetadata :: !Text
    , cakrCreateAccessKeyResult :: !CreateAccessKeyResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateAccessKeyResponse where
    xmlPickler = withNS iamNS

-- | This action creates an alias for your AWS account. For information about
-- using an AWS account alias, see Using an Alias for Your AWS Account ID in
-- Using AWS Identity and Access Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateAccountAlias.html>
data CreateAccountAlias = CreateAccountAlias
    { caaAccountAlias :: !Text
      -- ^ Name of the account alias to create.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateAccountAlias

instance Rq CreateAccountAlias where
    type Er CreateAccountAlias = IAMError
    type Rs CreateAccountAlias = CreateAccountAliasResponse
    request = qry GET "CreateAccountAlias"

data CreateAccountAliasResponse = CreateAccountAliasResponse
    { caarResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML CreateAccountAliasResponse where
    xmlPickler = withNS iamNS

-- | Creates a new group. For information about the number of groups you can
-- create, see Limitations on IAM Entities in Using AWS Identity and Access
-- Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateGroup.html>
data CreateGroup = CreateGroup
    { cgGroupName :: !Text
      -- ^ Name of the group to create. Do not include the path in this
      -- value.
    , cgPath      :: Maybe Text
      -- ^ The path to the group. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateGroup

instance Rq CreateGroup where
    type Er CreateGroup = IAMError
    type Rs CreateGroup = CreateGroupResponse
    request = qry GET "CreateGroup"

data CreateGroupResponse = CreateGroupResponse
    { cgrResponseMetadata :: !Text
    , cgrCreateGroupResult :: !CreateGroupResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateGroupResponse where
    xmlPickler = withNS iamNS

-- | Creates a new instance profile. For information about instance profiles, go
-- to About Instance Profiles. For information about the number of instance
-- profiles you can create, see Limitations on IAM Entities in Using AWS
-- Identity and Access Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateInstanceProfile.html>
data CreateInstanceProfile = CreateInstanceProfile
    { cipInstanceProfileName :: !Text
      -- ^ Name of the instance profile to create.
    , cipPath                :: Maybe Text
      -- ^ The path to the instance profile. For more information about
      -- paths, see Identifiers for IAM Entities in Using AWS Identity and
      -- Access Management.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateInstanceProfile

instance Rq CreateInstanceProfile where
    type Er CreateInstanceProfile = IAMError
    type Rs CreateInstanceProfile = CreateInstanceProfileResponse
    request = qry GET "CreateInstanceProfile"

data CreateInstanceProfileResponse = CreateInstanceProfileResponse
    { ciprResponseMetadata :: !Text
    , ciprCreateInstanceProfileResult :: !CreateInstanceProfileResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateInstanceProfileResponse where
    xmlPickler = withNS iamNS

-- | Creates a password for the specified user, giving the user the ability to
-- access AWS services through the AWS Management Console. For more
-- information about managing passwords, see Managing Passwords in Using IAM.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateLoginProfile.html>
data CreateLoginProfile = CreateLoginProfile
    { clpPassword :: !Text
      -- ^ The new password for the user name.
    , clpUserName :: !Text
      -- ^ Name of the user to create a password for.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateLoginProfile

instance Rq CreateLoginProfile where
    type Er CreateLoginProfile = IAMError
    type Rs CreateLoginProfile = CreateLoginProfileResponse
    request = qry GET "CreateLoginProfile"

data CreateLoginProfileResponse = CreateLoginProfileResponse
    { clprResponseMetadata :: !Text
    , clprCreateLoginProfileResult :: !CreateLoginProfileResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateLoginProfileResponse where
    xmlPickler = withNS iamNS

-- | Creates a new role for your AWS account. For more information about roles,
-- go to Working with Roles. For information about limitations on role names
-- and the number of roles you can create, go to Limitations on IAM Entities
-- in Using AWS Identity and Access Management. The policy grants permission
-- to an EC2 instance to assume the role. The policy is URL-encoded according
-- to RFC 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html. Currently, only EC2 instances can
-- assume roles.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html>
data CreateRole = CreateRole
    { crAssumeRolePolicyDocument :: !Text
      -- ^ The policy that grants an entity permission to assume the role.
    , crPath                     :: Maybe Text
      -- ^ The path to the role.
    , crRoleName                 :: !Text
      -- ^ Name of the role to create.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateRole

instance Rq CreateRole where
    type Er CreateRole = IAMError
    type Rs CreateRole = CreateRoleResponse
    request = qry GET "CreateRole"

data CreateRoleResponse = CreateRoleResponse
    { crrResponseMetadata :: !Text
    , crrCreateRoleResult :: !CreateRoleResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateRoleResponse where
    xmlPickler = withNS iamNS

-- | Creates a new user for your AWS account. For information about limitations
-- on the number of users you can create, see Limitations on IAM Entities in
-- Using AWS Identity and Access Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateUser.html>
data CreateUser = CreateUser
    { cuPath     :: Maybe Text
      -- ^ The path for the user name. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , cuUserName :: !Text
      -- ^ Name of the user to create.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateUser

instance Rq CreateUser where
    type Er CreateUser = IAMError
    type Rs CreateUser = CreateUserResponse
    request = qry GET "CreateUser"

data CreateUserResponse = CreateUserResponse
    { curResponseMetadata :: !Text
    , curCreateUserResult :: !CreateUserResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateUserResponse where
    xmlPickler = withNS iamNS

-- | Creates a new virtual MFA device for the AWS account. After creating the
-- virtual MFA, use EnableMFADevice to attach the MFA device to an IAM user.
-- For more information about creating and working with virtual MFA devices,
-- go to Using a Virtual MFA Device in Using AWS Identity and Access
-- Management. For information about limits on the number of MFA devices you
-- can create, see Limitations on Entities in Using AWS Identity and Access
-- Management. Important The seed information contained in the QR code and the
-- Base32 string should be treated like any other secret access information,
-- such as your AWS access keys or your passwords. After you provision your
-- virtual device, you should ensure that the information is destroyed
-- following secure procedures.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateVirtualMFADevice.html>
data CreateVirtualMFADevice = CreateVirtualMFADevice
    { cvmfadPath                 :: Maybe Text
      -- ^ The path for the virtual MFA device. For more information about
      -- paths, see Identifiers for IAM Entities in Using AWS Identity and
      -- Access Management.
    , cvmfadVirtualMFADeviceName :: !Text
      -- ^ The name of the virtual MFA device. Use with path to uniquely
      -- identify a virtual MFA device.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateVirtualMFADevice

instance Rq CreateVirtualMFADevice where
    type Er CreateVirtualMFADevice = IAMError
    type Rs CreateVirtualMFADevice = CreateVirtualMFADeviceResponse
    request = qry GET "CreateVirtualMFADevice"

data CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse
    { cvmfadrResponseMetadata :: !Text
    , cvmfadrCreateVirtualMFADeviceResult :: !CreateVirtualMFADeviceResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateVirtualMFADeviceResponse where
    xmlPickler = withNS iamNS

-- | Deactivates the specified MFA device and removes it from association with
-- the user name for which it was originally enabled.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeactivateMFADevice.html>
data DeactivateMFADevice = DeactivateMFADevice
    { dmfadSerialNumber :: !Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    , dmfadUserName     :: !Text
      -- ^ Name of the user whose MFA device you want to deactivate.
    } deriving (Eq, Show, Generic)

instance IsQuery DeactivateMFADevice

instance Rq DeactivateMFADevice where
    type Er DeactivateMFADevice = IAMError
    type Rs DeactivateMFADevice = DeactivateMFADeviceResponse
    request = qry GET "DeactivateMFADevice"

data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse
    { dmfadrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeactivateMFADeviceResponse where
    xmlPickler = withNS iamNS

-- | Deletes the access key associated with the specified user. If you do not
-- specify a user name, IAM determines the user name implicitly based on the
-- AWS Access Key ID signing the request. Because this action works for access
-- keys under the AWS account, you can use this API to manage root credentials
-- even if the AWS account has no associated users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccessKey.html>
data DeleteAccessKey = DeleteAccessKey
    { dakAccessKeyId :: !Text
      -- ^ The Access Key ID for the Access Key ID and Secret Access Key you
      -- want to delete.
    , dakUserName    :: Maybe Text
      -- ^ Name of the user whose key you want to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteAccessKey

instance Rq DeleteAccessKey where
    type Er DeleteAccessKey = IAMError
    type Rs DeleteAccessKey = DeleteAccessKeyResponse
    request = qry GET "DeleteAccessKey"

data DeleteAccessKeyResponse = DeleteAccessKeyResponse
    { dakrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteAccessKeyResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified AWS account alias. For information about using an AWS
-- account alias, see Using an Alias for Your AWS Account ID in Using AWS
-- Identity and Access Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccountAlias.html>
data DeleteAccountAlias = DeleteAccountAlias
    { daaAccountAlias :: !Text
      -- ^ Name of the account alias to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteAccountAlias

instance Rq DeleteAccountAlias where
    type Er DeleteAccountAlias = IAMError
    type Rs DeleteAccountAlias = DeleteAccountAliasResponse
    request = qry GET "DeleteAccountAlias"

data DeleteAccountAliasResponse = DeleteAccountAliasResponse
    { daarResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteAccountAliasResponse where
    xmlPickler = withNS iamNS

-- | Deletes the password policy for the AWS account.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccountPasswordPolicy.html>
data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy
    { dappNoSuchEntity :: !Text
      -- ^ The request was rejected because it referenced an entity that
      -- does not exist. The error message describes the entity.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteAccountPasswordPolicy

instance Rq DeleteAccountPasswordPolicy where
    type Er DeleteAccountPasswordPolicy = IAMError
    type Rs DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicyResponse
    request = qry GET "DeleteAccountPasswordPolicy"

data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse
    { dapprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteAccountPasswordPolicyResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified group. The group must not contain any users or have
-- any attached policies.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroup.html>
data DeleteGroup = DeleteGroup
    { dgGroupName :: !Text
      -- ^ Name of the group to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteGroup

instance Rq DeleteGroup where
    type Er DeleteGroup = IAMError
    type Rs DeleteGroup = DeleteGroupResponse
    request = qry GET "DeleteGroup"

data DeleteGroupResponse = DeleteGroupResponse
    { dgrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteGroupResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified policy that is associated with the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroupPolicy.html>
data DeleteGroupPolicy = DeleteGroupPolicy
    { dgpGroupName  :: !Text
      -- ^ Name of the group the policy is associated with.
    , dgpPolicyName :: !Text
      -- ^ Name of the policy document to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteGroupPolicy

instance Rq DeleteGroupPolicy where
    type Er DeleteGroupPolicy = IAMError
    type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse
    request = qry GET "DeleteGroupPolicy"

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    { dgprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteGroupPolicyResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified instance profile. The instance profile must not have
-- an associated role. Important Make sure you do not have any Amazon EC2
-- instances running with the instance profile you are about to delete.
-- Deleting a role or instance profile that is associated with a running
-- instance will break any applications running on the instance. For more
-- information about instance profiles, go to About Instance Profiles.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteInstanceProfile.html>
data DeleteInstanceProfile = DeleteInstanceProfile
    { dipInstanceProfileName :: !Text
      -- ^ Name of the instance profile to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteInstanceProfile

instance Rq DeleteInstanceProfile where
    type Er DeleteInstanceProfile = IAMError
    type Rs DeleteInstanceProfile = DeleteInstanceProfileResponse
    request = qry GET "DeleteInstanceProfile"

data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse
    { diprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteInstanceProfileResponse where
    xmlPickler = withNS iamNS

-- | Deletes the password for the specified user, which terminates the user's
-- ability to access AWS services through the AWS Management Console.
-- Important Deleting a user's password does not prevent a user from accessing
-- IAM through the command line interface or the API. To prevent all user
-- access you must also either make the access key inactive or delete it. For
-- more information about making keys inactive or deleting them, see
-- UpdateAccessKey and DeleteAccessKey.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteLoginProfile.html>
data DeleteLoginProfile = DeleteLoginProfile
    { dlpUserName :: !Text
      -- ^ Name of the user whose password you want to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteLoginProfile

instance Rq DeleteLoginProfile where
    type Er DeleteLoginProfile = IAMError
    type Rs DeleteLoginProfile = DeleteLoginProfileResponse
    request = qry GET "DeleteLoginProfile"

data DeleteLoginProfileResponse = DeleteLoginProfileResponse
    { dlprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteLoginProfileResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified role. The role must not have any policies attached.
-- For more information about roles, go to Working with Roles. Important Make
-- sure you do not have any Amazon EC2 instances running with the role you are
-- about to delete. Deleting a role or instance profile that is associated
-- with a running instance will break any applications running on the
-- instance.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRole.html>
data DeleteRole = DeleteRole
    { drRoleName :: !Text
      -- ^ Name of the role to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteRole

instance Rq DeleteRole where
    type Er DeleteRole = IAMError
    type Rs DeleteRole = DeleteRoleResponse
    request = qry GET "DeleteRole"

data DeleteRoleResponse = DeleteRoleResponse
    { drrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteRoleResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified policy associated with the specified role.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRolePolicy.html>
data DeleteRolePolicy = DeleteRolePolicy
    { drpPolicyName :: !Text
      -- ^ Name of the policy document to delete.
    , drpRoleName   :: !Text
      -- ^ Name of the role the associated with the policy.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteRolePolicy

instance Rq DeleteRolePolicy where
    type Er DeleteRolePolicy = IAMError
    type Rs DeleteRolePolicy = DeleteRolePolicyResponse
    request = qry GET "DeleteRolePolicy"

data DeleteRolePolicyResponse = DeleteRolePolicyResponse
    { drprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteRolePolicyResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified server certificate. Important If you are using a
-- server certificate with Elastic Load Balancing, deleting the certificate
-- could have implications for your application. If Elastic Load Balancing
-- doesn't detect the deletion of bound certificates, it may continue to use
-- the certificates. This could cause Elastic Load Balancing to stop accepting
-- traffic. We recommend that you remove the reference to the certificate from
-- Elastic Load Balancing before using this command to delete the certificate.
-- For more information, go to DeleteLoadBalancerListeners in the Elastic Load
-- Balancing API Reference.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteServerCertificate.html>
data DeleteServerCertificate = DeleteServerCertificate
    { dscServerCertificateName :: !Text
      -- ^ The name of the server certificate you want to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteServerCertificate

instance Rq DeleteServerCertificate where
    type Er DeleteServerCertificate = IAMError
    type Rs DeleteServerCertificate = DeleteServerCertificateResponse
    request = qry GET "DeleteServerCertificate"

data DeleteServerCertificateResponse = DeleteServerCertificateResponse
    { dscrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteServerCertificateResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified signing certificate associated with the specified
-- user. If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS Access Key ID signing the request. Because this
-- action works for access keys under the AWS account, you can use this API to
-- manage root credentials even if the AWS account has no associated users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteSigningCertificate.html>
data DeleteSigningCertificate = DeleteSigningCertificate
    { dscCertificateId :: !Text
      -- ^ ID of the signing certificate to delete.
    , dscUserName      :: Maybe Text
      -- ^ Name of the user the signing certificate belongs to.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteSigningCertificate

instance Rq DeleteSigningCertificate where
    type Er DeleteSigningCertificate = IAMError
    type Rs DeleteSigningCertificate = DeleteSigningCertificateResponse
    request = qry GET "DeleteSigningCertificate"

data DeleteSigningCertificateResponse = DeleteSigningCertificateResponse
    { dscsResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteSigningCertificateResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified user. The user must not belong to any groups, have
-- any keys or signing certificates, or have any attached policies.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteUser.html>
data DeleteUser = DeleteUser
    { duUserName :: !Text
      -- ^ Name of the user to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteUser

instance Rq DeleteUser where
    type Er DeleteUser = IAMError
    type Rs DeleteUser = DeleteUserResponse
    request = qry GET "DeleteUser"

data DeleteUserResponse = DeleteUserResponse
    { durResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteUserResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified policy associated with the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteUserPolicy.html>
data DeleteUserPolicy = DeleteUserPolicy
    { dupPolicyName :: !Text
      -- ^ Name of the policy document to delete.
    , dupUserName   :: !Text
      -- ^ Name of the user the policy is associated with.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteUserPolicy

instance Rq DeleteUserPolicy where
    type Er DeleteUserPolicy = IAMError
    type Rs DeleteUserPolicy = DeleteUserPolicyResponse
    request = qry GET "DeleteUserPolicy"

data DeleteUserPolicyResponse = DeleteUserPolicyResponse
    { duprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteUserPolicyResponse where
    xmlPickler = withNS iamNS

-- | Deletes a virtual MFA device. Note You must deactivate a user's virtual MFA
-- device before you can delete it. For information about deactivating MFA
-- devices, see DeactivateMFADevice.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteVirtualMFADevice.html>
data DeleteVirtualMFADevice = DeleteVirtualMFADevice
    { dvmfadSerialNumber :: !Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the same as the ARN.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteVirtualMFADevice

instance Rq DeleteVirtualMFADevice where
    type Er DeleteVirtualMFADevice = IAMError
    type Rs DeleteVirtualMFADevice = DeleteVirtualMFADeviceResponse
    request = qry GET "DeleteVirtualMFADevice"

data DeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse
    { dvmfadrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML DeleteVirtualMFADeviceResponse where
    xmlPickler = withNS iamNS

-- | Enables the specified MFA device and associates it with the specified user
-- name. When enabled, the MFA device is required for every subsequent login
-- by the user name associated with the device.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_EnableMFADevice.html>
data EnableMFADevice = EnableMFADevice
    { emfadAuthenticationCode1 :: !Text
      -- ^ An authentication code emitted by the device.
    , emfadAuthenticationCode2 :: !Text
      -- ^ A subsequent authentication code emitted by the device.
    , emfadSerialNumber        :: !Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    , emfadUserName            :: !Text
      -- ^ Name of the user for whom you want to enable the MFA device.
    } deriving (Eq, Show, Generic)

instance IsQuery EnableMFADevice

instance Rq EnableMFADevice where
    type Er EnableMFADevice = IAMError
    type Rs EnableMFADevice = EnableMFADeviceResponse
    request = qry GET "EnableMFADevice"

data EnableMFADeviceResponse = EnableMFADeviceResponse
    { emfadrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML EnableMFADeviceResponse where
    xmlPickler = withNS iamNS

-- | Retrieves the password policy for the AWS account. For more information
-- about using a password policy, go to Managing an IAM Password Policy.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountPasswordPolicy.html>
data GetAccountPasswordPolicy = GetAccountPasswordPolicy
    { gappPasswordPolicy :: !PasswordPolicy
      -- ^ The PasswordPolicy data type contains information about the
      -- account password policy.
    } deriving (Eq, Show, Generic)

instance IsQuery GetAccountPasswordPolicy

instance Rq GetAccountPasswordPolicy where
    type Er GetAccountPasswordPolicy = IAMError
    type Rs GetAccountPasswordPolicy = GetAccountPasswordPolicyResponse
    request = qry GET "GetAccountPasswordPolicy"

data GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse
    { gapprResponseMetadata :: !Text
    , gapprGetAccountPasswordPolicyResult :: !GetAccountPasswordPolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML GetAccountPasswordPolicyResponse where
    xmlPickler = withNS iamNS

-- | Retrieves account level information about account entity usage and IAM
-- quotas. For information about limitations on IAM entities, see Limitations
-- on IAM Entities in Using AWS Identity and Access Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetAccountSummary.html>
data GetAccountSummary = GetAccountSummary
    { gasSummaryMap :: !Text
      -- ^ A set of key value pairs containing account-level information.
    } deriving (Eq, Show, Generic)

instance IsQuery GetAccountSummary

instance Rq GetAccountSummary where
    type Er GetAccountSummary = IAMError
    type Rs GetAccountSummary = GetAccountSummaryResponse
    request = qry GET "GetAccountSummary"

data GetAccountSummaryResponse = GetAccountSummaryResponse
    { gasrResponseMetadata :: !Text
    , gasrGetAccountSummaryResult :: !GetAccountSummaryResult
    } deriving (Eq, Show, Generic)

instance IsXML GetAccountSummaryResponse where
    xmlPickler = withNS iamNS

-- | Returns a list of users that are in the specified group. You can paginate
-- the results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroup.html>
data GetGroup = GetGroup
    { ggGroupName :: !Text
      -- ^ Name of the group.
    , ggMarker    :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , ggMaxItems  :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of user names you want in the response. If there are
      -- additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Eq, Show, Generic)

instance IsQuery GetGroup

instance Rq GetGroup where
    type Er GetGroup = IAMError
    type Rs GetGroup = GetGroupResponse
    request = qry GET "GetGroup"

data GetGroupResponse = GetGroupResponse
    { ggrResponseMetadata :: !Text
    , ggrGetGroupResult :: !GetGroupResult
    } deriving (Eq, Show, Generic)

instance IsXML GetGroupResponse where
    xmlPickler = withNS iamNS

-- | Retrieves the specified policy document for the specified group. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroupPolicy.html>
data GetGroupPolicy = GetGroupPolicy
    { ggpGroupName  :: !Text
      -- ^ Name of the group the policy is associated with.
    , ggpPolicyName :: !Text
      -- ^ Name of the policy document to get.
    } deriving (Eq, Show, Generic)

instance IsQuery GetGroupPolicy

instance Rq GetGroupPolicy where
    type Er GetGroupPolicy = IAMError
    type Rs GetGroupPolicy = GetGroupPolicyResponse
    request = qry GET "GetGroupPolicy"

data GetGroupPolicyResponse = GetGroupPolicyResponse
    { ggprResponseMetadata :: !Text
    , ggprGetGroupPolicyResult :: !GetGroupPolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML GetGroupPolicyResponse where
    xmlPickler = withNS iamNS

-- | Retrieves information about the specified instance profile, including the
-- instance profile's path, GUID, ARN, and role. For more information about
-- instance profiles, go to About Instance Profiles. For more information
-- about ARNs, go to ARNs.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetInstanceProfile.html>
data GetInstanceProfile = GetInstanceProfile
    { gipInstanceProfileName :: !Text
      -- ^ Name of the instance profile to get information about.
    } deriving (Eq, Show, Generic)

instance IsQuery GetInstanceProfile

instance Rq GetInstanceProfile where
    type Er GetInstanceProfile = IAMError
    type Rs GetInstanceProfile = GetInstanceProfileResponse
    request = qry GET "GetInstanceProfile"

data GetInstanceProfileResponse = GetInstanceProfileResponse
    { giprResponseMetadata :: !Text
    , giprGetInstanceProfileResult :: !GetInstanceProfileResult
    } deriving (Eq, Show, Generic)

instance IsXML GetInstanceProfileResponse where
    xmlPickler = withNS iamNS

-- | Retrieves the user name and password create date for the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetLoginProfile.html>
data GetLoginProfile = GetLoginProfile
    { glpUserName :: !Text
      -- ^ Name of the user whose login profile you want to retrieve.
    } deriving (Eq, Show, Generic)

instance IsQuery GetLoginProfile

instance Rq GetLoginProfile where
    type Er GetLoginProfile = IAMError
    type Rs GetLoginProfile = GetLoginProfileResponse
    request = qry GET "GetLoginProfile"

data GetLoginProfileResponse = GetLoginProfileResponse
    { glprResponseMetadata :: !Text
    , glprGetLoginProfileResult :: !GetLoginProfileResult
    } deriving (Eq, Show, Generic)

instance IsXML GetLoginProfileResponse where
    xmlPickler = withNS iamNS

-- | Retrieves information about the specified role, including the role's path,
-- GUID, ARN, and the policy granting permission to EC2 to assume the role.
-- For more information about ARNs, go to ARNs. For more information about
-- roles, go to Working with Roles. The returned policy is URL-encoded
-- according to RFC 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetRole.html>
data GetRole = GetRole
    { grRoleName :: !Text
      -- ^ Name of the role to get information about.
    } deriving (Eq, Show, Generic)

instance IsQuery GetRole

instance Rq GetRole where
    type Er GetRole = IAMError
    type Rs GetRole = GetRoleResponse
    request = qry GET "GetRole"

data GetRoleResponse = GetRoleResponse
    { grrResponseMetadata :: !Text
    , grrGetRoleResult :: !GetRoleResult
    } deriving (Eq, Show, Generic)

instance IsXML GetRoleResponse where
    xmlPickler = withNS iamNS

-- | Retrieves the specified policy document for the specified role. For more
-- information about roles, go to Working with Roles. The returned policy is
-- URL-encoded according to RFC 3986. For more information about RFC 3986, go
-- to http://www.faqs.org/rfcs/rfc3986.html.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetRolePolicy.html>
data GetRolePolicy = GetRolePolicy
    { grpPolicyName :: !Text
      -- ^ Name of the policy document to get.
    , grpRoleName   :: !Text
      -- ^ Name of the role associated with the policy.
    } deriving (Eq, Show, Generic)

instance IsQuery GetRolePolicy

instance Rq GetRolePolicy where
    type Er GetRolePolicy = IAMError
    type Rs GetRolePolicy = GetRolePolicyResponse
    request = qry GET "GetRolePolicy"

data GetRolePolicyResponse = GetRolePolicyResponse
    { grprResponseMetadata :: !Text
    , grprGetRolePolicyResult :: !GetRolePolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML GetRolePolicyResponse where
    xmlPickler = withNS iamNS

-- | Retrieves information about the specified server certificate.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetServerCertificate.html>
data GetServerCertificate = GetServerCertificate
    { gscServerCertificateName :: !Text
      -- ^ The name of the server certificate you want to retrieve
      -- information about.
    } deriving (Eq, Show, Generic)

instance IsQuery GetServerCertificate

instance Rq GetServerCertificate where
    type Er GetServerCertificate = IAMError
    type Rs GetServerCertificate = GetServerCertificateResponse
    request = qry GET "GetServerCertificate"

data GetServerCertificateResponse = GetServerCertificateResponse
    { gscrResponseMetadata :: !Text
    , gscrGetServerCertificateResult :: !GetServerCertificateResult
    } deriving (Eq, Show, Generic)

instance IsXML GetServerCertificateResponse where
    xmlPickler = withNS iamNS

-- | Retrieves information about the specified user, including the user's path,
-- GUID, and ARN. If you do not specify a user name, IAM determines the user
-- name implicitly based on the AWS Access Key ID signing the request.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUser.html>
data GetUser = GetUser
    { guUserName :: Maybe Text
      -- ^ Name of the user to get information about.
    } deriving (Eq, Show, Generic)

instance IsQuery GetUser

instance Rq GetUser where
    type Er GetUser = IAMError
    type Rs GetUser = GetUserResponse
    request = qry GET "GetUser"

data GetUserResponse = GetUserResponse
    { gurResponseMetadata :: !Text
    , gurGetUserResult :: !GetUserResult
    } deriving (Eq, Show, Generic)

instance IsXML GetUserResponse where
    xmlPickler = withNS iamNS

-- | Retrieves the specified policy document for the specified user. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetUserPolicy.html>
data GetUserPolicy = GetUserPolicy
    { gupPolicyName :: !Text
      -- ^ Name of the policy document to get.
    , gupUserName   :: !Text
      -- ^ Name of the user who the policy is associated with.
    } deriving (Eq, Show, Generic)

instance IsQuery GetUserPolicy

instance Rq GetUserPolicy where
    type Er GetUserPolicy = IAMError
    type Rs GetUserPolicy = GetUserPolicyResponse
    request = qry GET "GetUserPolicy"

data GetUserPolicyResponse = GetUserPolicyResponse
    { guprResponseMetadata :: !Text
    , guprGetUserPolicyResult :: !GetUserPolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML GetUserPolicyResponse where
    xmlPickler = withNS iamNS

-- | Returns information about the Access Key IDs associated with the specified
-- user. If there are none, the action returns an empty list. Although each
-- user is limited to a small number of keys, you can still paginate the
-- results using the MaxItems and Marker parameters. If the UserName field is
-- not specified, the UserName is determined implicitly based on the AWS
-- Access Key ID used to sign the request. Because this action works for
-- access keys under the AWS account, this API can be used to manage root
-- credentials even if the AWS account has no associated users. Note To ensure
-- the security of your AWS account, the secret access key is accessible only
-- during key and user creation.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccessKeys.html>
data ListAccessKeys = ListAccessKeys
    { lakMarker   :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , lakMaxItems :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of keys you want in the response. If there are
      -- additional keys beyond the maximum you specify, the IsTruncated
      -- response element is true. This parameter is optional. If you do
      -- not include it, it defaults to 100.
    , lakUserName :: Maybe Text
      -- ^ Name of the user.
    } deriving (Eq, Show, Generic)

instance IsQuery ListAccessKeys

instance Rq ListAccessKeys where
    type Er ListAccessKeys = IAMError
    type Rs ListAccessKeys = ListAccessKeysResponse
    request = qry GET "ListAccessKeys"

data ListAccessKeysResponse = ListAccessKeysResponse
    { lakrResponseMetadata :: !Text
    , lakrListAccessKeysResult :: !ListAccessKeysResult
    } deriving (Eq, Show, Generic)

instance IsXML ListAccessKeysResponse where
    xmlPickler = withNS iamNS

-- | Lists the account aliases associated with the account. For information
-- about using an AWS account alias, see Using an Alias for Your AWS Account
-- ID in Using AWS Identity and Access Management. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccountAliases.html>
data ListAccountAliases = ListAccountAliases
    { laaMarker   :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , laaMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of account aliases you want in the response. If there are
      -- additional account aliases beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Eq, Show, Generic)

instance IsQuery ListAccountAliases

instance Rq ListAccountAliases where
    type Er ListAccountAliases = IAMError
    type Rs ListAccountAliases = ListAccountAliasesResponse
    request = qry GET "ListAccountAliases"

data ListAccountAliasesResponse = ListAccountAliasesResponse
    { laarResponseMetadata :: !Text
    , laarListAccountAliasesResult :: !ListAccountAliasesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListAccountAliasesResponse where
    xmlPickler = withNS iamNS

-- | Lists the names of the policies associated with the specified group. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupPolicies.html>
data ListGroupPolicies = ListGroupPolicies
    { lgpGroupName :: !Text
      -- ^ The name of the group to list policies for.
    , lgpMarker    :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , lgpMaxItems  :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of policy names you want in the response. If there are
      -- additional policy names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Eq, Show, Generic)

instance IsQuery ListGroupPolicies

instance Rq ListGroupPolicies where
    type Er ListGroupPolicies = IAMError
    type Rs ListGroupPolicies = ListGroupPoliciesResponse
    request = qry GET "ListGroupPolicies"

data ListGroupPoliciesResponse = ListGroupPoliciesResponse
    { lgprResponseMetadata :: !Text
    , lgprListGroupPoliciesResult :: !ListGroupPoliciesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListGroupPoliciesResponse where
    xmlPickler = withNS iamNS

-- | Lists the groups that have the specified path prefix. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroups.html>
data ListGroups = ListGroups
    { lgMarker     :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , lgMaxItems   :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of groups you want in the response. If there are
      -- additional groups beyond the maximum you specify, the IsTruncated
      -- response element is true. This parameter is optional. If you do
      -- not include it, it defaults to 100.
    , lgPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /division_abc/subdivision_xyz/, which would get all groups whose
      -- path starts with /division_abc/subdivision_xyz/.
    } deriving (Eq, Show, Generic)

instance IsQuery ListGroups

instance Rq ListGroups where
    type Er ListGroups = IAMError
    type Rs ListGroups = ListGroupsResponse
    request = qry GET "ListGroups"

data ListGroupsResponse = ListGroupsResponse
    { lgrResponseMetadata :: !Text
    , lgrListGroupsResult :: !ListGroupsResult
    } deriving (Eq, Show, Generic)

instance IsXML ListGroupsResponse where
    xmlPickler = withNS iamNS

-- | Lists the groups the specified user belongs to. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupsForUser.html>
data ListGroupsForUser = ListGroupsForUser
    { lgfuMarker   :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , lgfuMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of groups you want in the response. If there are
      -- additional groups beyond the maximum you specify, the IsTruncated
      -- response element is true. This parameter is optional. If you do
      -- not include it, it defaults to 100.
    , lgfuUserName :: !Text
      -- ^ The name of the user to list groups for.
    } deriving (Eq, Show, Generic)

instance IsQuery ListGroupsForUser

instance Rq ListGroupsForUser where
    type Er ListGroupsForUser = IAMError
    type Rs ListGroupsForUser = ListGroupsForUserResponse
    request = qry GET "ListGroupsForUser"

data ListGroupsForUserResponse = ListGroupsForUserResponse
    { lgfurResponseMetadata :: !Text
    , lgfurListGroupsForUserResult :: !ListGroupsForUserResult
    } deriving (Eq, Show, Generic)

instance IsXML ListGroupsForUserResponse where
    xmlPickler = withNS iamNS

-- | Lists the instance profiles that have the specified path prefix. If there
-- are none, the action returns an empty list. For more information about
-- instance profiles, go to About Instance Profiles. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListInstanceProfiles.html>
data ListInstanceProfiles = ListInstanceProfiles
    { lipMarker     :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , lipMaxItems   :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    , lipPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /application_abc/component_xyz/, which would get all instance
      -- profiles whose path starts with /application_abc/component_xyz/.
    } deriving (Eq, Show, Generic)

instance IsQuery ListInstanceProfiles

instance Rq ListInstanceProfiles where
    type Er ListInstanceProfiles = IAMError
    type Rs ListInstanceProfiles = ListInstanceProfilesResponse
    request = qry GET "ListInstanceProfiles"

data ListInstanceProfilesResponse = ListInstanceProfilesResponse
    { liprResponseMetadata :: !Text
    , liprListInstanceProfilesResult :: !ListInstanceProfilesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListInstanceProfilesResponse where
    xmlPickler = withNS iamNS

-- | Lists the instance profiles that have the specified associated role. If
-- there are none, the action returns an empty list. For more information
-- about instance profiles, go to About Instance Profiles. You can paginate
-- the results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListInstanceProfilesForRole.html>
data ListInstanceProfilesForRole = ListInstanceProfilesForRole
    { lipfrMarker   :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , lipfrMaxItems :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    , lipfrRoleName :: !Text
      -- ^ The name of the role to list instance profiles for.
    } deriving (Eq, Show, Generic)

instance IsQuery ListInstanceProfilesForRole

instance Rq ListInstanceProfilesForRole where
    type Er ListInstanceProfilesForRole = IAMError
    type Rs ListInstanceProfilesForRole = ListInstanceProfilesForRoleResponse
    request = qry GET "ListInstanceProfilesForRole"

data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse
    { lipfrrResponseMetadata :: !Text
    , lipfrrListInstanceProfilesForRoleResult :: !ListInstanceProfilesForRoleResult
    } deriving (Eq, Show, Generic)

instance IsXML ListInstanceProfilesForRoleResponse where
    xmlPickler = withNS iamNS

-- | Lists the MFA devices. If the request includes the user name, then this
-- action lists all the MFA devices associated with the specified user name.
-- If you do not specify a user name, IAM determines the user name implicitly
-- based on the AWS Access Key ID signing the request. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListMFADevices.html>
data ListMFADevices = ListMFADevices
    { lmfadMarker   :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , lmfadMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of MFA devices you want in the response. If there are
      -- additional MFA devices beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    , lmfadUserName :: Maybe Text
      -- ^ Name of the user whose MFA devices you want to list.
    } deriving (Eq, Show, Generic)

instance IsQuery ListMFADevices

instance Rq ListMFADevices where
    type Er ListMFADevices = IAMError
    type Rs ListMFADevices = ListMFADevicesResponse
    request = qry GET "ListMFADevices"

data ListMFADevicesResponse = ListMFADevicesResponse
    { lmfadrResponseMetadata :: !Text
    , lmfadrListMFADevicesResult :: !ListMFADevicesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListMFADevicesResponse where
    xmlPickler = withNS iamNS

-- | Lists the names of the policies associated with the specified role. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRolePolicies.html>
data ListRolePolicies = ListRolePolicies
    { lrpMarker   :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , lrpMaxItems :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    , lrpRoleName :: !Text
      -- ^ The name of the role to list policies for.
    } deriving (Eq, Show, Generic)

instance IsQuery ListRolePolicies

instance Rq ListRolePolicies where
    type Er ListRolePolicies = IAMError
    type Rs ListRolePolicies = ListRolePoliciesResponse
    request = qry GET "ListRolePolicies"

data ListRolePoliciesResponse = ListRolePoliciesResponse
    { lrprResponseMetadata :: !Text
    , lrprListRolePoliciesResult :: !ListRolePoliciesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListRolePoliciesResponse where
    xmlPickler = withNS iamNS

-- | Lists the roles that have the specified path prefix. If there are none, the
-- action returns an empty list. For more information about roles, go to
-- Working with Roles. You can paginate the results using the MaxItems and
-- Marker parameters. The returned policy is URL-encoded according to RFC
-- 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListRoles.html>
data ListRoles = ListRoles
    { lrMarker     :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , lrMaxItems   :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    , lrPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /application_abc/component_xyz/, which would get all roles whose
      -- path starts with /application_abc/component_xyz/.
    } deriving (Eq, Show, Generic)

instance IsQuery ListRoles

instance Rq ListRoles where
    type Er ListRoles = IAMError
    type Rs ListRoles = ListRolesResponse
    request = qry GET "ListRoles"

instance Pg ListRoles where
    next ListRoles{..} (lrrListRolesResult -> ListRolesResult{..})
        | not lrrIsTruncated = Nothing
        | otherwise = Just $ ListRoles lrrMarker lrMaxItems lrPathPrefix

data ListRolesResponse = ListRolesResponse
    { lrrResponseMetadata :: !Text
    , lrrListRolesResult  :: !ListRolesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListRolesResponse where
    xmlPickler = withNS iamNS

-- | Lists the server certificates that have the specified path prefix. If none
-- exist, the action returns an empty list. You can paginate the results using
-- the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListServerCertificates.html>
data ListServerCertificates = ListServerCertificates
    { lscMarker     :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , lscMaxItems   :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of server certificates you want in the response. If there
      -- are additional server certificates beyond the maximum you
      -- specify, the IsTruncated response element will be set to true.
      -- This parameter is optional. If you do not include it, it defaults
      -- to 100.
    , lscPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /company/servercerts would get all server certificates for which
      -- the path starts with /company/servercerts.
    } deriving (Eq, Show, Generic)

instance IsQuery ListServerCertificates

instance Rq ListServerCertificates where
    type Er ListServerCertificates = IAMError
    type Rs ListServerCertificates = ListServerCertificatesResponse
    request = qry GET "ListServerCertificates"

data ListServerCertificatesResponse = ListServerCertificatesResponse
    { lscrResponseMetadata :: !Text
    , lscrListServerCertificatesResult :: !ListServerCertificatesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListServerCertificatesResponse where
    xmlPickler = withNS iamNS

-- | Returns information about the signing certificates associated with the
-- specified user. If there are none, the action returns an empty list.
-- Although each user is limited to a small number of signing certificates,
-- you can still paginate the results using the MaxItems and Marker
-- parameters. If the UserName field is not specified, the user name is
-- determined implicitly based on the AWS Access Key ID used to sign the
-- request. Because this action works for access keys under the AWS account,
-- this API can be used to manage root credentials even if the AWS account has
-- no associated users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListSigningCertificates.html>
data ListSigningCertificates = ListSigningCertificates
    { lsdMarker   :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , lsdMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of certificate IDs you want in the response. If there are
      -- additional certificate IDs beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    , lsdUserName :: Maybe Text
      -- ^ The name of the user.
    } deriving (Eq, Show, Generic)

instance IsQuery ListSigningCertificates

instance Rq ListSigningCertificates where
    type Er ListSigningCertificates = IAMError
    type Rs ListSigningCertificates = ListSigningCertificatesResponse
    request = qry GET "ListSigningCertificates"

data ListSigningCertificatesResponse = ListSigningCertificatesResponse
    { lscsResponseMetadata :: !Text
    , lscsListSigningCertificatesResult :: !ListSigningCertificatesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListSigningCertificatesResponse where
    xmlPickler = withNS iamNS

-- | Lists the names of the policies associated with the specified user. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUserPolicies.html>
data ListUserPolicies = ListUserPolicies
    { lupMarker   :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are
      -- truncated. Set it to the value of the Marker element in the
      -- response you just received.
    , lupMaxItems :: Maybe Integer
      -- ^ Use this only when paginating results to indicate the maximum
      -- number of policy names you want in the response. If there are
      -- additional policy names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    , lupUserName :: !Text
      -- ^ The name of the user to list policies for.
    } deriving (Eq, Show, Generic)

instance IsQuery ListUserPolicies

instance Rq ListUserPolicies where
    type Er ListUserPolicies = IAMError
    type Rs ListUserPolicies = ListUserPoliciesResponse
    request = qry GET "ListUserPolicies"

data ListUserPoliciesResponse = ListUserPoliciesResponse
    { luprResponseMetadata :: !Text
    , luprListUserPoliciesResult :: !ListUserPoliciesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListUserPoliciesResponse where
    xmlPickler = withNS iamNS

-- | Lists the users that have the specified path prefix. If there are none, the
-- action returns an empty list. You can paginate the results using the
-- MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListUsers.html>
data ListUsers = ListUsers
    { luMarker     :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , luMaxItems   :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    , luPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /division_abc/subdivision_xyz/, which would get all user names
      -- whose path starts with /division_abc/subdivision_xyz/.
    } deriving (Eq, Show, Generic)

instance IsQuery ListUsers

instance Rq ListUsers where
    type Er ListUsers = IAMError
    type Rs ListUsers = ListUsersResponse
    request = qry GET "ListUsers"

data ListUsersResponse = ListUsersResponse
    { lurResponseMetadata :: !Text
    , lurListUsersResult :: !ListUsersResult
    } deriving (Eq, Show, Generic)

instance IsXML ListUsersResponse where
    xmlPickler = withNS iamNS

-- | Lists the virtual MFA devices under the AWS account by assignment status.
-- If you do not specify an assignment status, the action returns a list of
-- all virtual MFA devices. Assignment status can be Assigned, Unassigned, or
-- Any. You can paginate the results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListVirtualMFADevices.html>
data ListVirtualMFADevices = ListVirtualMFADevices
    { lvmfadAssignmentStatus :: Maybe Text
      -- ^ The status (unassigned or assigned) of the devices to list. If
      -- you do not specify an AssignmentStatus, the action defaults to
      -- Any which lists both assigned and unassigned virtual MFA devices.
    , lvmfadMarker           :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a
      -- subsequent request after you've received a response where the
      -- results are truncated. Set it to the value of the Marker element
      -- in the response you just received.
    , lvmfadMaxItems         :: Maybe Integer
      -- ^ Use this parameter only when paginating results to indicate the
      -- maximum number of user names you want in the response. If there
      -- are additional user names beyond the maximum you specify, the
      -- IsTruncated response element is true. This parameter is optional.
      -- If you do not include it, it defaults to 100.
    } deriving (Eq, Show, Generic)

instance IsQuery ListVirtualMFADevices

instance Rq ListVirtualMFADevices where
    type Er ListVirtualMFADevices = IAMError
    type Rs ListVirtualMFADevices = ListVirtualMFADevicesResponse
    request = qry GET "ListVirtualMFADevices"

data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse
    { lvmfadrResponseMetadata :: !Text
    , lvmfadrListVirtualMFADevicesResult :: !ListVirtualMFADevicesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListVirtualMFADevicesResponse where
    xmlPickler = withNS iamNS

-- | Adds (or updates) a policy document associated with the specified group.
-- For information about policies, refer to Overview of Policies in Using AWS
-- Identity and Access Management. For information about limits on the number
-- of policies you can associate with a group, see Limitations on IAM Entities
-- in Using AWS Identity and Access Management. Note Because policy documents
-- can be large, you should use POST rather than GET when calling
-- PutGroupPolicy. For information about setting up signatures and
-- authorization through the API, go to Signing AWS API Requests in the AWS
-- General Reference. For general information about using the Query API with
-- IAM, go to Making Query Requests in Using IAM.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutGroupPolicy.html>
data PutGroupPolicy = PutGroupPolicy
    { pgpGroupName      :: !Text
      -- ^ Name of the group to associate the policy with.
    , pgpPolicyDocument :: !Text
      -- ^ The policy document.
    , pgpPolicyName     :: !Text
      -- ^ Name of the policy document.
    } deriving (Eq, Show, Generic)

instance IsQuery PutGroupPolicy

instance Rq PutGroupPolicy where
    type Er PutGroupPolicy = IAMError
    type Rs PutGroupPolicy = PutGroupPolicyResponse
    request = qry GET "PutGroupPolicy"

data PutGroupPolicyResponse = PutGroupPolicyResponse
    { pgprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML PutGroupPolicyResponse where
    xmlPickler = withNS iamNS

-- | Adds (or updates) a policy document associated with the specified role. For
-- information about policies, go to Overview of Policies in Using AWS
-- Identity and Access Management. For information about limits on the
-- policies you can associate with a role, see Limitations on IAM Entities in
-- Using AWS Identity and Access Management. Note Because policy documents can
-- be large, you should use POST rather than GET when calling PutRolePolicy.
-- For information about setting up signatures and authorization through the
-- API, go to Signing AWS API Requests in the AWS General Reference. For
-- general information about using the Query API with IAM, go to Making Query
-- Requests in Using IAM.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutRolePolicy.html>
data PutRolePolicy = PutRolePolicy
    { prpPolicyDocument :: !Text
      -- ^ The policy document.
    , prpPolicyName     :: !Text
      -- ^ Name of the policy document.
    , prpRoleName       :: !Text
      -- ^ Name of the role to associate the policy with.
    } deriving (Eq, Show, Generic)

instance IsQuery PutRolePolicy

instance Rq PutRolePolicy where
    type Er PutRolePolicy = IAMError
    type Rs PutRolePolicy = PutRolePolicyResponse
    request = qry GET "PutRolePolicy"

data PutRolePolicyResponse = PutRolePolicyResponse
    { prprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML PutRolePolicyResponse where
    xmlPickler = withNS iamNS

-- | Adds (or updates) a policy document associated with the specified user. For
-- information about policies, refer to Overview of Policies in Using AWS
-- Identity and Access Management. For information about limits on the number
-- of policies you can associate with a user, see Limitations on IAM Entities
-- in Using AWS Identity and Access Management. Note Because policy documents
-- can be large, you should use POST rather than GET when calling
-- PutUserPolicy. For information about setting up signatures and
-- authorization through the API, go to Signing AWS API Requests in the AWS
-- General Reference. For general information about using the Query API with
-- IAM, go to Making Query Requests in Using IAM.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutUserPolicy.html>
data PutUserPolicy = PutUserPolicy
    { pupPolicyDocument :: !Text
      -- ^ The policy document.
    , pupPolicyName     :: !Text
      -- ^ Name of the policy document.
    , pupUserName       :: !Text
      -- ^ Name of the user to associate the policy with.
    } deriving (Eq, Show, Generic)

instance IsQuery PutUserPolicy

instance Rq PutUserPolicy where
    type Er PutUserPolicy = IAMError
    type Rs PutUserPolicy = PutUserPolicyResponse
    request = qry GET "PutUserPolicy"

data PutUserPolicyResponse = PutUserPolicyResponse
    { puprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML PutUserPolicyResponse where
    xmlPickler = withNS iamNS

-- | Removes the specified role from the specified instance profile. Important
-- Make sure you do not have any Amazon EC2 instances running with the role
-- you are about to remove from the instance profile. Removing a role from an
-- instance profile that is associated with a running instance will break any
-- applications running on the instance. For more information about roles, go
-- to Working with Roles. For more information about instance profiles, go to
-- About Instance Profiles.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveRoleFromInstanceProfile.html>
data RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfile
    { rrfipInstanceProfileName :: !Text
      -- ^ Name of the instance profile to update.
    , rrfipRoleName            :: !Text
      -- ^ Name of the role to remove.
    } deriving (Eq, Show, Generic)

instance IsQuery RemoveRoleFromInstanceProfile

instance Rq RemoveRoleFromInstanceProfile where
    type Er RemoveRoleFromInstanceProfile = IAMError
    type Rs RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfileResponse
    request = qry GET "RemoveRoleFromInstanceProfile"

data RemoveRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse
    { rrfiprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML RemoveRoleFromInstanceProfileResponse where
    xmlPickler = withNS iamNS

-- | Removes the specified user from the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveUserFromGroup.html>
data RemoveUserFromGroup = RemoveUserFromGroup
    { rufgGroupName :: !Text
      -- ^ Name of the group to update.
    , rufgUserName  :: !Text
      -- ^ Name of the user to remove.
    } deriving (Eq, Show, Generic)

instance IsQuery RemoveUserFromGroup

instance Rq RemoveUserFromGroup where
    type Er RemoveUserFromGroup = IAMError
    type Rs RemoveUserFromGroup = RemoveUserFromGroupResponse
    request = qry GET "RemoveUserFromGroup"

data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse
    { rufgrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML RemoveUserFromGroupResponse where
    xmlPickler = withNS iamNS

-- | Synchronizes the specified MFA device with AWS servers.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ResyncMFADevice.html>
data ResyncMFADevice = ResyncMFADevice
    { rmfadAuthenticationCode1 :: !Text
      -- ^ An authentication code emitted by the device.
    , rmfadAuthenticationCode2 :: !Text
      -- ^ A subsequent authentication code emitted by the device.
    , rmfadSerialNumber        :: !Text
      -- ^ Serial number that uniquely identifies the MFA device.
    , rmfadUserName            :: !Text
      -- ^ Name of the user whose MFA device you want to resynchronize.
    } deriving (Eq, Show, Generic)

instance IsQuery ResyncMFADevice

instance Rq ResyncMFADevice where
    type Er ResyncMFADevice = IAMError
    type Rs ResyncMFADevice = ResyncMFADeviceResponse
    request = qry GET "ResyncMFADevice"

data ResyncMFADeviceResponse = ResyncMFADeviceResponse
    { rmfadrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML ResyncMFADeviceResponse where
    xmlPickler = withNS iamNS

-- | Changes the status of the specified access key from Active to Inactive, or
-- vice versa. This action can be used to disable a user's key as part of a
-- key rotation work flow. If the UserName field is not specified, the
-- UserName is determined implicitly based on the AWS Access Key ID used to
-- sign the request. Because this action works for access keys under the AWS
-- account, this API can be used to manage root credentials even if the AWS
-- account has no associated users. For information about rotating keys, see
-- Managing Keys and Certificates in Using AWS Identity and Access Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAccessKey.html>
data UpdateAccessKey = UpdateAccessKey
    { uakAccessKeyId :: !Text
      -- ^ The Access Key ID of the Secret Access Key you want to update.
    , uakStatus      :: !Text
      -- ^ The status you want to assign to the Secret Access Key. Active
      -- means the key can be used for API calls to AWS, while Inactive
      -- means the key cannot be used.
    , uakUserName    :: Maybe Text
      -- ^ Name of the user whose key you want to update.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateAccessKey

instance Rq UpdateAccessKey where
    type Er UpdateAccessKey = IAMError
    type Rs UpdateAccessKey = UpdateAccessKeyResponse
    request = qry GET "UpdateAccessKey"

data UpdateAccessKeyResponse = UpdateAccessKeyResponse
    { uakrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML UpdateAccessKeyResponse where
    xmlPickler = withNS iamNS

-- | Updates the password policy settings for the account. For more information
-- about using a password policy, go to Managing an IAM Password Policy.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAccountPasswordPolicy.html>
data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { uappAllowUsersToChangePassword :: Maybe Bool
      -- ^ Type: Boolean
    , uappMinimumPasswordLength      :: Maybe Integer
      -- ^ Type: Integer
    , uappRequireLowercaseCharacters :: Maybe Bool
      -- ^ Type: Boolean
    , uappRequireNumbers             :: Maybe Bool
      -- ^ Type: Boolean
    , uappRequireSymbols             :: Maybe Bool
      -- ^ Type: Boolean
    , uappRequireUppercaseCharacters :: Maybe Bool
      -- ^ Type: Boolean
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateAccountPasswordPolicy

instance Rq UpdateAccountPasswordPolicy where
    type Er UpdateAccountPasswordPolicy = IAMError
    type Rs UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicyResponse
    request = qry GET "UpdateAccountPasswordPolicy"

data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse
    { uapprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML UpdateAccountPasswordPolicyResponse where
    xmlPickler = withNS iamNS

-- | Updates the policy that grants an entity permission to assume a role.
-- Currently, only an Amazon EC2 instance can assume a role. For more
-- information about roles, go to Working with Roles.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAssumeRolePolicy.html>
data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy
    { uarpPolicyDocument :: !Text
      -- ^ The policy that grants an entity permission to assume the role.
    , uarpRoleName       :: !Text
      -- ^ Name of the role to update.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateAssumeRolePolicy

instance Rq UpdateAssumeRolePolicy where
    type Er UpdateAssumeRolePolicy = IAMError
    type Rs UpdateAssumeRolePolicy = UpdateAssumeRolePolicyResponse
    request = qry GET "UpdateAssumeRolePolicy"

data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse
    { uarprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML UpdateAssumeRolePolicyResponse where
    xmlPickler = withNS iamNS

-- | Updates the name and/or the path of the specified group. Important You
-- should understand the implications of changing a group's path or name. For
-- more information, see Renaming Users and Groups in Using AWS Identity and
-- Access Management. Note To change a group name the requester must have
-- appropriate permissions on both the source object and the target object.
-- For example, to change Managers to MGRs, the entity making the request must
-- have permission on Managers and MGRs, or must have permission on all (*).
-- For more information about permissions, see Permissions and Policies.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateGroup.html>
data UpdateGroup = UpdateGroup
    { ugGroupName    :: !Text
      -- ^ Name of the group to update. If you're changing the name of the
      -- group, this is the original name.
    , ugNewGroupName :: Maybe Text
      -- ^ New name for the group. Only include this if changing the group's
      -- name.
    , ugNewPath      :: Maybe Text
      -- ^ New path for the group. Only include this if changing the group's
      -- path.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateGroup

instance Rq UpdateGroup where
    type Er UpdateGroup = IAMError
    type Rs UpdateGroup = UpdateGroupResponse
    request = qry GET "UpdateGroup"

data UpdateGroupResponse = UpdateGroupResponse
    { ugrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML UpdateGroupResponse where
    xmlPickler = withNS iamNS

-- | Changes the password for the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateLoginProfile.html>
data UpdateLoginProfile = UpdateLoginProfile
    { ulpPassword :: !Text
      -- ^ The new password for the user name.
    , ulpUserName :: !Text
      -- ^ Name of the user whose password you want to update.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateLoginProfile

instance Rq UpdateLoginProfile where
    type Er UpdateLoginProfile = IAMError
    type Rs UpdateLoginProfile = UpdateLoginProfileResponse
    request = qry GET "UpdateLoginProfile"

data UpdateLoginProfileResponse = UpdateLoginProfileResponse
    { ulprResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML UpdateLoginProfileResponse where
    xmlPickler = withNS iamNS

-- | Updates the name and/or the path of the specified server certificate.
-- Important You should understand the implications of changing a server
-- certificate's path or name. For more information, see Managing Server
-- Certificates in Using AWS Identity and Access Management. Note To change a
-- server certificate name the requester must have appropriate permissions on
-- both the source object and the target object. For example, to change the
-- name from ProductionCert to ProdCert, the entity making the request must
-- have permission on ProductionCert and ProdCert, or must have permission on
-- all (*). For more information about permissions, see Permissions and
-- Policies.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateServerCertificate.html>
data UpdateServerCertificate = UpdateServerCertificate
    { uscNewPath                  :: Maybe Text
      -- ^ The new path for the server certificate. Include this only if you
      -- are updating the server certificate's path.
    , uscNewServerCertificateName :: Maybe Text
      -- ^ The new name for the server certificate. Include this only if you
      -- are updating the server certificate's name.
    , uscServerCertificateName    :: !Text
      -- ^ The name of the server certificate that you want to update.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateServerCertificate

instance Rq UpdateServerCertificate where
    type Er UpdateServerCertificate = IAMError
    type Rs UpdateServerCertificate = UpdateServerCertificateResponse
    request = qry GET "UpdateServerCertificate"

data UpdateServerCertificateResponse = UpdateServerCertificateResponse
    { uscrResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML UpdateServerCertificateResponse where
    xmlPickler = withNS iamNS

-- | Changes the status of the specified signing certificate from active to
-- disabled, or vice versa. This action can be used to disable a user's
-- signing certificate as part of a certificate rotation work flow. If the
-- UserName field is not specified, the UserName is determined implicitly
-- based on the AWS Access Key ID used to sign the request. Because this
-- action works for access keys under the AWS account, this API can be used to
-- manage root credentials even if the AWS account has no associated users.
-- For information about rotating certificates, see Managing Keys and
-- Certificates in Using AWS Identity and Access Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateSigningCertificate.html>
data UpdateSigningCertificate = UpdateSigningCertificate
    { uscCertificateId :: !Text
      -- ^ The ID of the signing certificate you want to update.
    , uscStatus        :: !Text
      -- ^ The status you want to assign to the certificate. Active means
      -- the certificate can be used for API calls to AWS, while Inactive
      -- means the certificate cannot be used.
    , uscUserName      :: Maybe Text
      -- ^ Name of the user the signing certificate belongs to.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateSigningCertificate

instance Rq UpdateSigningCertificate where
    type Er UpdateSigningCertificate = IAMError
    type Rs UpdateSigningCertificate = UpdateSigningCertificateResponse
    request = qry GET "UpdateSigningCertificate"

data UpdateSigningCertificateResponse = UpdateSigningCertificateResponse
    { uscsResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML UpdateSigningCertificateResponse where
    xmlPickler = withNS iamNS

-- | Updates the name and/or the path of the specified user. Important You
-- should understand the implications of changing a user's path or name. For
-- more information, see Renaming Users and Groups in Using AWS Identity and
-- Access Management. Note To change a user name the requester must have
-- appropriate permissions on both the source object and the target object.
-- For example, to change Bob to Robert, the entity making the request must
-- have permission on Bob and Robert, or must have permission on all (*). For
-- more information about permissions, see Permissions and Policies.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateUser.html>
data UpdateUser = UpdateUser
    { uuNewPath     :: Maybe Text
      -- ^ New path for the user. Include this parameter only if you're
      -- changing the user's path.
    , uuNewUserName :: Maybe Text
      -- ^ New name for the user. Include this parameter only if you're
      -- changing the user's name.
    , uuUserName    :: !Text
      -- ^ Name of the user to update. If you're changing the name of the
      -- user, this is the original user name.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateUser

instance Rq UpdateUser where
    type Er UpdateUser = IAMError
    type Rs UpdateUser = UpdateUserResponse
    request = qry GET "UpdateUser"

data UpdateUserResponse = UpdateUserResponse
    { uurResponseMetadata :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML UpdateUserResponse where
    xmlPickler = withNS iamNS

-- | Uploads a server certificate entity for the AWS account. The server
-- certificate entity includes a public key certificate, a private key, and an
-- optional certificate chain, which should all be PEM-encoded. For
-- information about the number of server certificates you can upload, see
-- Limitations on IAM Entities in Using AWS Identity and Access Management.
-- Note Because the body of the public key certificate, private key, and the
-- certificate chain can be large, you should use POST rather than GET when
-- calling UploadServerCertificate. For information about setting up
-- signatures and authorization through the API, go to Signing AWS API
-- Requests in the AWS General Reference. For general information about using
-- the Query API with IAM, go to Making Query Requests in Using IAM.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UploadServerCertificate.html>
data UploadServerCertificate = UploadServerCertificate
    { uscCertificateBody       :: !Text
      -- ^ The contents of the public key certificate in PEM-encoded format.
    , uscCertificateChain      :: Maybe Text
      -- ^ The contents of the certificate chain. This is typically a
      -- concatenation of the PEM-encoded public key certificates of the
      -- chain.
    , uscPath                  :: Maybe Text
      -- ^ The path for the server certificate. For more information about
      -- paths, see Identifiers for IAM Entities in Using AWS Identity and
      -- Access Management.
    , uscPrivateKey            :: !Text
      -- ^ The contents of the private key in PEM-encoded format.
    , usdServerCertificateName :: !Text
      -- ^ The name for the server certificate. Do not include the path in
      -- this value.
    } deriving (Eq, Show, Generic)

instance IsQuery UploadServerCertificate

instance Rq UploadServerCertificate where
    type Er UploadServerCertificate = IAMError
    type Rs UploadServerCertificate = UploadServerCertificateResponse
    request = qry GET "UploadServerCertificate"

data UploadServerCertificateResponse = UploadServerCertificateResponse
    { usctResponseMetadata :: !Text
    , usctUploadServerCertificateResult :: !UploadServerCertificateResult
    } deriving (Eq, Show, Generic)

instance IsXML UploadServerCertificateResponse where
    xmlPickler = withNS iamNS

-- | Uploads an X.509 signing certificate and associates it with the specified
-- user. Some AWS services use X.509 signing certificates to validate requests
-- that are signed with a corresponding private key. When you upload the
-- certificate, its default status is Active. If the UserName field is not
-- specified, the user name is determined implicitly based on the AWS Access
-- Key ID used to sign the request. Because this action works for access keys
-- under the AWS account, this API can be used to manage root credentials even
-- if the AWS account has no associated users. Note Because the body of a
-- X.509 certificate can be large, you should use POST rather than GET when
-- calling UploadSigningCertificate. For information about setting up
-- signatures and authorization through the API, go to Signing AWS API
-- Requests in the AWS General Reference. For general information about using
-- the Query API with IAM, go to Making Query Requests in Using IAM.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UploadSigningCertificate.html>
data UploadSigningCertificate = UploadSigningCertificate
    { usdCertificateBody :: !Text
      -- ^ The contents of the signing certificate.
    , usdUserName        :: Maybe Text
      -- ^ Name of the user the signing certificate is for.
    } deriving (Eq, Show, Generic)

instance IsQuery UploadSigningCertificate

instance Rq UploadSigningCertificate where
    type Er UploadSigningCertificate = IAMError
    type Rs UploadSigningCertificate = UploadSigningCertificateResponse
    request = qry GET "UploadSigningCertificate"

data UploadSigningCertificateResponse = UploadSigningCertificateResponse
    { uscuResponseMetadata :: !Text
    , uscuUploadSigningCertificateResult :: !UploadSigningCertificateResult
    } deriving (Eq, Show, Generic)

instance IsXML UploadSigningCertificateResponse where
    xmlPickler = withNS iamNS
