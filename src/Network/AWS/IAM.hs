{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

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
     AddRoleToInstanceProfile      (..)

   -- ** AddUserToGroup
   , AddUserToGroup                (..)

   -- ** ChangePassword
   , ChangePassword                (..)

   -- ** CreateAccessKey
   , CreateAccessKey               (..)

   -- ** CreateAccountAlias
   , CreateAccountAlias            (..)

   -- ** CreateGroup
   , CreateGroup                   (..)

   -- ** CreateInstanceProfile
   , CreateInstanceProfile         (..)

   -- ** CreateLoginProfile
   , CreateLoginProfile            (..)

   -- ** CreateRole
   , CreateRole                    (..)

   -- ** CreateUser
   , CreateUser                    (..)

   -- ** CreateVirtualMFADevice
   , CreateVirtualMFADevice        (..)

   -- ** DeactivateMFADevice
   , DeactivateMFADevice           (..)

   -- ** DeleteAccessKey
   , DeleteAccessKey               (..)

   -- ** DeleteAccountAlias
   , DeleteAccountAlias            (..)

   -- ** DeleteAccountPasswordPolicy
   , DeleteAccountPasswordPolicy   (..)

   -- ** DeleteGroup
   , DeleteGroup                   (..)

   -- ** DeleteGroupPolicy
   , DeleteGroupPolicy             (..)

   -- ** DeleteInstanceProfile
   , DeleteInstanceProfile         (..)

   -- ** DeleteLoginProfile
   , DeleteLoginProfile            (..)

   -- ** DeleteRole
   , DeleteRole                    (..)

   -- ** DeleteRolePolicy
   , DeleteRolePolicy              (..)

   -- ** DeleteServerCertificate
   , DeleteServerCertificate       (..)

   -- ** DeleteSigningCertificate
   , DeleteSigningCertificate      (..)

   -- ** DeleteUser
   , DeleteUser                    (..)

   -- ** DeleteUserPolicy
   , DeleteUserPolicy              (..)

   -- ** DeleteVirtualMFADevice
   , DeleteVirtualMFADevice        (..)

   -- ** EnableMFADevice
   , EnableMFADevice               (..)

   -- ** GetAccountPasswordPolicy
   , GetAccountPasswordPolicy      (..)

   -- ** GetAccountSummary
   , GetAccountSummary             (..)

   -- ** GetGroup
   , GetGroup                      (..)

   -- ** GetGroupPolicy
   , GetGroupPolicy                (..)

   -- ** GetInstanceProfile
   , GetInstanceProfile            (..)

   -- ** GetLoginProfile
   , GetLoginProfile               (..)

   -- ** GetRole
   , GetRole                       (..)

   -- ** GetRolePolicy
   , GetRolePolicy                 (..)

   -- ** GetServerCertificate
   , GetServerCertificate          (..)

   -- ** GetUser
   , GetUser                       (..)

   -- ** GetUserPolicy
   , GetUserPolicy                 (..)

   -- ** ListAccessKeys
   , ListAccessKeys                (..)

   -- ** ListAccountAliases
   , ListAccountAliases            (..)

   -- ** ListGroupPolicies
   , ListGroupPolicies             (..)

   -- ** ListGroups
   , ListGroups                    (..)

   -- ** ListGroupsForUser
   , ListGroupsForUser             (..)

   -- ** ListInstanceProfiles
   , ListInstanceProfiles          (..)

   -- ** ListInstanceProfilesForRole
   , ListInstanceProfilesForRole   (..)

   -- ** ListMFADevices
   , ListMFADevices                (..)

   -- ** ListRolePolicies
   , ListRolePolicies              (..)

   -- ** ListRoles
   , ListRoles                     (..)

   -- ** ListServerCertificates
   , ListServerCertificates        (..)

   -- ** ListSigningCertificates
   , ListSigningCertificates       (..)

   -- ** ListUserPolicies
   , ListUserPolicies              (..)

   -- ** ListUsers
   , ListUsers                     (..)

   -- ** ListVirtualMFADevices
   , ListVirtualMFADevices         (..)

   -- ** PutGroupPolicy
   , PutGroupPolicy                (..)

   -- ** PutRolePolicy
   , PutRolePolicy                 (..)

   -- ** PutUserPolicy
   , PutUserPolicy                 (..)

   -- ** RemoveRoleFromInstanceProfile
   , RemoveRoleFromInstanceProfile (..)

   -- ** RemoveUserFromGroup
   , RemoveUserFromGroup           (..)

   -- ** ResyncMFADevice
   , ResyncMFADevice               (..)

   -- ** UpdateAccessKey
   , UpdateAccessKey               (..)

   -- ** UpdateAccountPasswordPolicy
   , UpdateAccountPasswordPolicy   (..)

   -- ** UpdateAssumeRolePolicy
   , UpdateAssumeRolePolicy        (..)

   -- ** UpdateGroup
   , UpdateGroup                   (..)

   -- ** UpdateLoginProfile
   , UpdateLoginProfile            (..)

   -- ** UpdateServerCertificate
   , UpdateServerCertificate       (..)

   -- ** UpdateSigningCertificate
   , UpdateSigningCertificate      (..)

   -- ** UpdateUser
   , UpdateUser                    (..)

   -- ** UploadServerCertificate
   , UploadServerCertificate       (..)

   -- ** UploadSigningCertificate
   , UploadSigningCertificate      (..)

   -- * Data Types
   , module Network.AWS.IAM.Types
   , Rs                            (..)
   ) where

import Data.ByteString       (ByteString)
import Data.Monoid
import Data.Time
import Network.AWS.IAM.Types
import Network.AWS.Internal
import Network.Http.Client   (Method(..))

data IAM

instance AWSService IAM where
    service _ = awsService "iam" iamVersion SigningVersion4

req :: IsQuery a => Method -> ByteString -> a -> RawRequest IAM b
req meth act qry = (emptyRequest meth FormEncoded "/" Nothing)
    { rqAction = Just act
    , rqQuery  = toQuery qry
    }

--
-- Actions
--

-- | Adds the specified role to the specified instance profile. For more
-- information about roles, go to Working with Roles. For more information
-- about instance profiles, go to About Instance Profiles.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddRoleToInstanceProfile.html>
data AddRoleToInstanceProfile = AddRoleToInstanceProfile
    { artipInstanceProfileName :: !ByteString
      -- ^ Name of the instance profile to update.
    , artipRoleName            :: !ByteString
      -- ^ Name of the role to add.
    } deriving (Eq, Show, Generic)

instance IsQuery AddRoleToInstanceProfile

instance AWSRequest IAM AddRoleToInstanceProfile AddRoleToInstanceProfileResponse where
    request = req GET "AddRoleToInstanceProfile"

data AddRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse
    { artiprResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML AddRoleToInstanceProfileResponse where
    xmlPickler = withNS iamNS

-- | Adds the specified user to the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddUserToGroup.html>
data AddUserToGroup = AddUserToGroup
    { autgGroupName :: !ByteString
      -- ^ Name of the group to update.
    , autgUserName  :: !ByteString
      -- ^ Name of the user to add.
    } deriving (Eq, Show, Generic)

instance IsQuery AddUserToGroup

instance AWSRequest IAM AddUserToGroup AddUserToGroupResponse where
    request = req GET "AddUserToGroup"

data AddUserToGroupResponse = AddUserToGroupResponse
    { autgrResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML AddUserToGroupResponse where
    xmlPickler = withNS iamNS

-- | Changes the password of the IAM user calling ChangePassword. The root
-- account password is not affected by this action. For information about
-- modifying passwords, see Managing Passwords.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ChangePassword.html>
data ChangePassword = ChangePassword
    { cpNewPassword :: !ByteString
      -- ^ Type: String
    , cpOldPassword :: !ByteString
      -- ^ Type: String
    } deriving (Eq, Show, Generic)

instance IsQuery ChangePassword

instance AWSRequest IAM ChangePassword ChangePasswordResponse where
    request = req GET "ChangePassword"

data ChangePasswordResponse = ChangePasswordResponse
    { cprResponseMetadata :: !ByteString
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
    { cakUserName :: Maybe ByteString
      -- ^ The user name that the new key will belong to.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateAccessKey

instance AWSRequest IAM CreateAccessKey CreateAccessKeyResponse where
    request = req GET "CreateAccessKey"

data CreateAccessKeyResponse = CreateAccessKeyResponse
    { cakrResponseMetadata :: !ByteString
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
    { caaAccountAlias :: !ByteString
      -- ^ Name of the account alias to create.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateAccountAlias

instance AWSRequest IAM CreateAccountAlias CreateAccountAliasResponse where
    request = req GET "CreateAccountAlias"

data CreateAccountAliasResponse = CreateAccountAliasResponse
    { caarResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML CreateAccountAliasResponse where
    xmlPickler = withNS iamNS

-- | Creates a new group. For information about the number of groups you can
-- create, see Limitations on IAM Entities in Using AWS Identity and Access
-- Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateGroup.html>
data CreateGroup = CreateGroup
    { cgGroupName :: !ByteString
      -- ^ Name of the group to create. Do not include the path in this
      -- value.
    , cgPath      :: Maybe ByteString
      -- ^ The path to the group. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateGroup

instance AWSRequest IAM CreateGroup CreateGroupResponse where
    request = req GET "CreateGroup"

data CreateGroupResponse = CreateGroupResponse
    { cgrResponseMetadata :: !ByteString
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
    { cipInstanceProfileName :: !ByteString
      -- ^ Name of the instance profile to create.
    , cipPath                :: Maybe ByteString
      -- ^ The path to the instance profile. For more information about
      -- paths, see Identifiers for IAM Entities in Using AWS Identity and
      -- Access Management.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateInstanceProfile

instance AWSRequest IAM CreateInstanceProfile CreateInstanceProfileResponse where
    request = req GET "CreateInstanceProfile"

data CreateInstanceProfileResponse = CreateInstanceProfileResponse
    { ciprResponseMetadata :: !ByteString
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
    { clpPassword :: !ByteString
      -- ^ The new password for the user name.
    , clpUserName :: !ByteString
      -- ^ Name of the user to create a password for.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateLoginProfile

instance AWSRequest IAM CreateLoginProfile CreateLoginProfileResponse where
    request = req GET "CreateLoginProfile"

data CreateLoginProfileResponse = CreateLoginProfileResponse
    { clprResponseMetadata :: !ByteString
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
    { crAssumeRolePolicyDocument :: !ByteString
      -- ^ The policy that grants an entity permission to assume the role.
    , crPath                     :: Maybe ByteString
      -- ^ The path to the role. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , crRoleName                 :: !ByteString
      -- ^ Name of the role to create.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateRole

instance AWSRequest IAM CreateRole CreateRoleResponse where
    request = req GET "CreateRole"

data CreateRoleResponse = CreateRoleResponse
    { crrResponseMetadata :: !ByteString
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
    { cuPath     :: Maybe ByteString
      -- ^ The path for the user name. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access
      -- Management.
    , cuUserName :: !ByteString
      -- ^ Name of the user to create.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateUser

instance AWSRequest IAM CreateUser CreateUserResponse where
    request = req GET "CreateUser"

data CreateUserResponse = CreateUserResponse
    { curResponseMetadata :: !ByteString
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
    { cvmfadPath                 :: Maybe ByteString
      -- ^ The path for the virtual MFA device. For more information about
      -- paths, see Identifiers for IAM Entities in Using AWS Identity and
      -- Access Management.
    , cvmfadVirtualMFADeviceName :: !ByteString
      -- ^ The name of the virtual MFA device. Use with path to uniquely
      -- identify a virtual MFA device.
    } deriving (Eq, Show, Generic)

instance IsQuery CreateVirtualMFADevice

instance AWSRequest IAM CreateVirtualMFADevice CreateVirtualMFADeviceResponse where
    request = req GET "CreateVirtualMFADevice"

data CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse
    { cvmfadrResponseMetadata :: !ByteString
    , cvmfadrCreateVirtualMFADeviceResult :: !CreateVirtualMFADeviceResult
    } deriving (Eq, Show, Generic)

instance IsXML CreateVirtualMFADeviceResponse where
    xmlPickler = withNS iamNS

-- | Deactivates the specified MFA device and removes it from association with
-- the user name for which it was originally enabled.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeactivateMFADevice.html>
data DeactivateMFADevice = DeactivateMFADevice
    { dmfadSerialNumber :: !ByteString
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    , dmfadUserName     :: !ByteString
      -- ^ Name of the user whose MFA device you want to deactivate.
    } deriving (Eq, Show, Generic)

instance IsQuery DeactivateMFADevice

instance AWSRequest IAM DeactivateMFADevice DeactivateMFADeviceResponse where
    request = req GET "DeactivateMFADevice"

data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse
    { dmfadrResponseMetadata :: !ByteString
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
    { dakAccessKeyId :: !ByteString
      -- ^ The Access Key ID for the Access Key ID and Secret Access Key you
      -- want to delete.
    , dakUserName    :: Maybe ByteString
      -- ^ Name of the user whose key you want to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteAccessKey

instance AWSRequest IAM DeleteAccessKey DeleteAccessKeyResponse where
    request = req GET "DeleteAccessKey"

data DeleteAccessKeyResponse = DeleteAccessKeyResponse
    { dakrResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML DeleteAccessKeyResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified AWS account alias. For information about using an AWS
-- account alias, see Using an Alias for Your AWS Account ID in Using AWS
-- Identity and Access Management.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccountAlias.html>
data DeleteAccountAlias = DeleteAccountAlias
    { daaAccountAlias :: !ByteString
      -- ^ Name of the account alias to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteAccountAlias

instance AWSRequest IAM DeleteAccountAlias DeleteAccountAliasResponse where
    request = req GET "DeleteAccountAlias"

data DeleteAccountAliasResponse = DeleteAccountAliasResponse
    { daarResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML DeleteAccountAliasResponse where
    xmlPickler = withNS iamNS

-- | Deletes the password policy for the AWS account.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccountPasswordPolicy.html>
data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy
    { dappNoSuchEntity :: !ByteString
      -- ^ The request was rejected because it referenced an entity that
      -- does not exist. The error message describes the entity.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteAccountPasswordPolicy

instance AWSRequest IAM DeleteAccountPasswordPolicy DeleteAccountPasswordPolicyResponse where
    request = req GET "DeleteAccountPasswordPolicy"

data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse
    { dapprResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML DeleteAccountPasswordPolicyResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified group. The group must not contain any users or have
-- any attached policies.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroup.html>
data DeleteGroup = DeleteGroup
    { dgGroupName :: !ByteString
      -- ^ Name of the group to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteGroup

instance AWSRequest IAM DeleteGroup DeleteGroupResponse where
    request = req GET "DeleteGroup"

data DeleteGroupResponse = DeleteGroupResponse
    { dgrResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML DeleteGroupResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified policy that is associated with the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroupPolicy.html>
data DeleteGroupPolicy = DeleteGroupPolicy
    { dgpGroupName  :: !ByteString
      -- ^ Name of the group the policy is associated with.
    , dgpPolicyName :: !ByteString
      -- ^ Name of the policy document to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteGroupPolicy

instance AWSRequest IAM DeleteGroupPolicy DeleteGroupPolicyResponse where
    request = req GET "DeleteGroupPolicy"

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    { dgprResponseMetadata :: !ByteString
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
    { dipInstanceProfileName :: !ByteString
      -- ^ Name of the instance profile to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteInstanceProfile

instance AWSRequest IAM DeleteInstanceProfile DeleteInstanceProfileResponse where
    request = req GET "DeleteInstanceProfile"

data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse
    { diprResponseMetadata :: !ByteString
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
    { dlpUserName :: !ByteString
      -- ^ Name of the user whose password you want to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteLoginProfile

instance AWSRequest IAM DeleteLoginProfile DeleteLoginProfileResponse where
    request = req GET "DeleteLoginProfile"

data DeleteLoginProfileResponse = DeleteLoginProfileResponse
    { dlprResponseMetadata :: !ByteString
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
    { drRoleName :: !ByteString
      -- ^ Name of the role to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteRole

instance AWSRequest IAM DeleteRole DeleteRoleResponse where
    request = req GET "DeleteRole"

data DeleteRoleResponse = DeleteRoleResponse
    { drrResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML DeleteRoleResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified policy associated with the specified role.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteRolePolicy.html>
data DeleteRolePolicy = DeleteRolePolicy
    { drpPolicyName :: !ByteString
      -- ^ Name of the policy document to delete.
    , drpRoleName   :: !ByteString
      -- ^ Name of the role the associated with the policy.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteRolePolicy

instance AWSRequest IAM DeleteRolePolicy DeleteRolePolicyResponse where
    request = req GET "DeleteRolePolicy"

data DeleteRolePolicyResponse = DeleteRolePolicyResponse
    { drprResponseMetadata :: !ByteString
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
    { dscServerCertificateName :: !ByteString
      -- ^ The name of the server certificate you want to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteServerCertificate

instance AWSRequest IAM DeleteServerCertificate DeleteServerCertificateResponse where
    request = req GET "DeleteServerCertificate"

data DeleteServerCertificateResponse = DeleteServerCertificateResponse
    { dscrResponseMetadata :: !ByteString
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
    { dscCertificateId :: !ByteString
      -- ^ ID of the signing certificate to delete.
    , dscUserName      :: Maybe ByteString
      -- ^ Name of the user the signing certificate belongs to.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteSigningCertificate

instance AWSRequest IAM DeleteSigningCertificate DeleteSigningCertificateResponse where
    request = req GET "DeleteSigningCertificate"

data DeleteSigningCertificateResponse = DeleteSigningCertificateResponse
    { dscsResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML DeleteSigningCertificateResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified user. The user must not belong to any groups, have
-- any keys or signing certificates, or have any attached policies.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteUser.html>
data DeleteUser = DeleteUser
    { duUserName :: !ByteString
      -- ^ Name of the user to delete.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteUser

instance AWSRequest IAM DeleteUser DeleteUserResponse where
    request = req GET "DeleteUser"

data DeleteUserResponse = DeleteUserResponse
    { durResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML DeleteUserResponse where
    xmlPickler = withNS iamNS

-- | Deletes the specified policy associated with the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteUserPolicy.html>
data DeleteUserPolicy = DeleteUserPolicy
    { dupPolicyName :: !ByteString
      -- ^ Name of the policy document to delete.
    , dupUserName   :: !ByteString
      -- ^ Name of the user the policy is associated with.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteUserPolicy

instance AWSRequest IAM DeleteUserPolicy DeleteUserPolicyResponse where
    request = req GET "DeleteUserPolicy"

data DeleteUserPolicyResponse = DeleteUserPolicyResponse
    { duprResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML DeleteUserPolicyResponse where
    xmlPickler = withNS iamNS

-- | Deletes a virtual MFA device. Note You must deactivate a user's virtual MFA
-- device before you can delete it. For information about deactivating MFA
-- devices, see DeactivateMFADevice.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteVirtualMFADevice.html>
data DeleteVirtualMFADevice = DeleteVirtualMFADevice
    { dvmfadSerialNumber :: !ByteString
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the same as the ARN.
    } deriving (Eq, Show, Generic)

instance IsQuery DeleteVirtualMFADevice

instance AWSRequest IAM DeleteVirtualMFADevice DeleteVirtualMFADeviceResponse where
    request = req GET "DeleteVirtualMFADevice"

data DeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse
    { dvmfadrResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML DeleteVirtualMFADeviceResponse where
    xmlPickler = withNS iamNS

-- | Enables the specified MFA device and associates it with the specified user
-- name. When enabled, the MFA device is required for every subsequent login
-- by the user name associated with the device.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_EnableMFADevice.html>
data EnableMFADevice = EnableMFADevice
    { emfadAuthenticationCode1 :: !ByteString
      -- ^ An authentication code emitted by the device.
    , emfadAuthenticationCode2 :: !ByteString
      -- ^ A subsequent authentication code emitted by the device.
    , emfadSerialNumber        :: !ByteString
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    , emfadUserName            :: !ByteString
      -- ^ Name of the user for whom you want to enable the MFA device.
    } deriving (Eq, Show, Generic)

instance IsQuery EnableMFADevice

instance AWSRequest IAM EnableMFADevice EnableMFADeviceResponse where
    request = req GET "EnableMFADevice"

data EnableMFADeviceResponse = EnableMFADeviceResponse
    { emfadrResponseMetadata :: !ByteString
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

instance AWSRequest IAM GetAccountPasswordPolicy GetAccountPasswordPolicyResponse where
    request = req GET "GetAccountPasswordPolicy"

data GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse
    { gapprResponseMetadata :: !ByteString
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
    { gasSummaryMap :: !ByteString
      -- ^ A set of key value pairs containing account-level information.
    } deriving (Eq, Show, Generic)

instance IsQuery GetAccountSummary

instance AWSRequest IAM GetAccountSummary GetAccountSummaryResponse where
    request = req GET "GetAccountSummary"

data GetAccountSummaryResponse = GetAccountSummaryResponse
    { gasrResponseMetadata :: !ByteString
    , gasrGetAccountSummaryResult :: !GetAccountSummaryResult
    } deriving (Eq, Show, Generic)

instance IsXML GetAccountSummaryResponse where
    xmlPickler = withNS iamNS

-- | Returns a list of users that are in the specified group. You can paginate
-- the results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetGroup.html>
data GetGroup = GetGroup
    { ggGroupName :: !ByteString
      -- ^ Name of the group.
    , ggMarker    :: Maybe ByteString
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

instance AWSRequest IAM GetGroup GetGroupResponse where
    request = req GET "GetGroup"

data GetGroupResponse = GetGroupResponse
    { ggrResponseMetadata :: !ByteString
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
    { ggpGroupName  :: !ByteString
      -- ^ Name of the group the policy is associated with.
    , ggpPolicyName :: !ByteString
      -- ^ Name of the policy document to get.
    } deriving (Eq, Show, Generic)

instance IsQuery GetGroupPolicy

instance AWSRequest IAM GetGroupPolicy GetGroupPolicyResponse where
    request = req GET "GetGroupPolicy"

data GetGroupPolicyResponse = GetGroupPolicyResponse
    { ggprResponseMetadata :: !ByteString
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
    { gipInstanceProfileName :: !ByteString
      -- ^ Name of the instance profile to get information about.
    } deriving (Eq, Show, Generic)

instance IsQuery GetInstanceProfile

instance AWSRequest IAM GetInstanceProfile GetInstanceProfileResponse where
    request = req GET "GetInstanceProfile"

data GetInstanceProfileResponse = GetInstanceProfileResponse
    { giprResponseMetadata :: !ByteString
    , giprGetInstanceProfileResult :: !GetInstanceProfileResult
    } deriving (Eq, Show, Generic)

instance IsXML GetInstanceProfileResponse where
    xmlPickler = withNS iamNS

-- | Retrieves the user name and password create date for the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetLoginProfile.html>
data GetLoginProfile = GetLoginProfile
    { glpUserName :: !ByteString
      -- ^ Name of the user whose login profile you want to retrieve.
    } deriving (Eq, Show, Generic)

instance IsQuery GetLoginProfile

instance AWSRequest IAM GetLoginProfile GetLoginProfileResponse where
    request = req GET "GetLoginProfile"

data GetLoginProfileResponse = GetLoginProfileResponse
    { glprResponseMetadata :: !ByteString
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
    { grRoleName :: !ByteString
      -- ^ Name of the role to get information about.
    } deriving (Eq, Show, Generic)

instance IsQuery GetRole

instance AWSRequest IAM GetRole GetRoleResponse where
    request = req GET "GetRole"

data GetRoleResponse = GetRoleResponse
    { grrResponseMetadata :: !ByteString
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
    { grpPolicyName :: !ByteString
      -- ^ Name of the policy document to get.
    , grpRoleName   :: !ByteString
      -- ^ Name of the role associated with the policy.
    } deriving (Eq, Show, Generic)

instance IsQuery GetRolePolicy

instance AWSRequest IAM GetRolePolicy GetRolePolicyResponse where
    request = req GET "GetRolePolicy"

data GetRolePolicyResponse = GetRolePolicyResponse
    { grprResponseMetadata :: !ByteString
    , grprGetRolePolicyResult :: !GetRolePolicyResult
    } deriving (Eq, Show, Generic)

instance IsXML GetRolePolicyResponse where
    xmlPickler = withNS iamNS

-- | Retrieves information about the specified server certificate.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetServerCertificate.html>
data GetServerCertificate = GetServerCertificate
    { gscServerCertificateName :: !ByteString
      -- ^ The name of the server certificate you want to retrieve
      -- information about.
    } deriving (Eq, Show, Generic)

instance IsQuery GetServerCertificate

instance AWSRequest IAM GetServerCertificate GetServerCertificateResponse where
    request = req GET "GetServerCertificate"

data GetServerCertificateResponse = GetServerCertificateResponse
    { gscrResponseMetadata :: !ByteString
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
    { guUserName :: Maybe ByteString
      -- ^ Name of the user to get information about.
    } deriving (Eq, Show, Generic)

instance IsQuery GetUser

instance AWSRequest IAM GetUser GetUserResponse where
    request = req GET "GetUser"

data GetUserResponse = GetUserResponse
    { gurResponseMetadata :: !ByteString
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
    { gupPolicyName :: !ByteString
      -- ^ Name of the policy document to get.
    , gupUserName   :: !ByteString
      -- ^ Name of the user who the policy is associated with.
    } deriving (Eq, Show, Generic)

instance IsQuery GetUserPolicy

instance AWSRequest IAM GetUserPolicy GetUserPolicyResponse where
    request = req GET "GetUserPolicy"

data GetUserPolicyResponse = GetUserPolicyResponse
    { guprResponseMetadata :: !ByteString
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
    { lakMarker   :: Maybe ByteString
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
    , lakUserName :: Maybe ByteString
      -- ^ Name of the user.
    } deriving (Eq, Show, Generic)

instance IsQuery ListAccessKeys

instance AWSRequest IAM ListAccessKeys ListAccessKeysResponse where
    request = req GET "ListAccessKeys"

data ListAccessKeysResponse = ListAccessKeysResponse
    { lakrResponseMetadata :: !ByteString
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
    { laaMarker   :: Maybe ByteString
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

instance AWSRequest IAM ListAccountAliases ListAccountAliasesResponse where
    request = req GET "ListAccountAliases"

data ListAccountAliasesResponse = ListAccountAliasesResponse
    { laarResponseMetadata :: !ByteString
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
    { lgpGroupName :: !ByteString
      -- ^ The name of the group to list policies for.
    , lgpMarker    :: Maybe ByteString
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

instance AWSRequest IAM ListGroupPolicies ListGroupPoliciesResponse where
    request = req GET "ListGroupPolicies"

data ListGroupPoliciesResponse = ListGroupPoliciesResponse
    { lgprResponseMetadata :: !ByteString
    , lgprListGroupPoliciesResult :: !ListGroupPoliciesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListGroupPoliciesResponse where
    xmlPickler = withNS iamNS

-- | Lists the groups that have the specified path prefix. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroups.html>
data ListGroups = ListGroups
    { lgMarker     :: Maybe ByteString
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
    , lgPathPrefix :: Maybe ByteString
      -- ^ The path prefix for filtering the results. For example:
      -- /division_abc/subdivision_xyz/, which would get all groups whose
      -- path starts with /division_abc/subdivision_xyz/.
    } deriving (Eq, Show, Generic)

instance IsQuery ListGroups

instance AWSRequest IAM ListGroups ListGroupsResponse where
    request = req GET "ListGroups"

data ListGroupsResponse = ListGroupsResponse
    { lgrResponseMetadata :: !ByteString
    , lgrListGroupsResult :: !ListGroupsResult
    } deriving (Eq, Show, Generic)

instance IsXML ListGroupsResponse where
    xmlPickler = withNS iamNS

-- | Lists the groups the specified user belongs to. You can paginate the
-- results using the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListGroupsForUser.html>
data ListGroupsForUser = ListGroupsForUser
    { lgfuMarker   :: Maybe ByteString
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
    , lgfuUserName :: !ByteString
      -- ^ The name of the user to list groups for.
    } deriving (Eq, Show, Generic)

instance IsQuery ListGroupsForUser

instance AWSRequest IAM ListGroupsForUser ListGroupsForUserResponse where
    request = req GET "ListGroupsForUser"

data ListGroupsForUserResponse = ListGroupsForUserResponse
    { lgfurResponseMetadata :: !ByteString
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
    { lipMarker     :: Maybe ByteString
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
    , lipPathPrefix :: Maybe ByteString
      -- ^ The path prefix for filtering the results. For example:
      -- /application_abc/component_xyz/, which would get all instance
      -- profiles whose path starts with /application_abc/component_xyz/.
    } deriving (Eq, Show, Generic)

instance IsQuery ListInstanceProfiles

instance AWSRequest IAM ListInstanceProfiles ListInstanceProfilesResponse where
    request = req GET "ListInstanceProfiles"

data ListInstanceProfilesResponse = ListInstanceProfilesResponse
    { liprResponseMetadata :: !ByteString
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
    { lipfrMarker   :: Maybe ByteString
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
    , lipfrRoleName :: !ByteString
      -- ^ The name of the role to list instance profiles for.
    } deriving (Eq, Show, Generic)

instance IsQuery ListInstanceProfilesForRole

instance AWSRequest IAM ListInstanceProfilesForRole ListInstanceProfilesForRoleResponse where
    request = req GET "ListInstanceProfilesForRole"

data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse
    { lipfrrResponseMetadata :: !ByteString
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
    { lmfadMarker   :: Maybe ByteString
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
    , lmfadUserName :: Maybe ByteString
      -- ^ Name of the user whose MFA devices you want to list.
    } deriving (Eq, Show, Generic)

instance IsQuery ListMFADevices

instance AWSRequest IAM ListMFADevices ListMFADevicesResponse where
    request = req GET "ListMFADevices"

data ListMFADevicesResponse = ListMFADevicesResponse
    { lmfadrResponseMetadata :: !ByteString
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
    { lrpMarker   :: Maybe ByteString
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
    , lrpRoleName :: !ByteString
      -- ^ The name of the role to list policies for.
    } deriving (Eq, Show, Generic)

instance IsQuery ListRolePolicies

instance AWSRequest IAM ListRolePolicies ListRolePoliciesResponse where
    request = req GET "ListRolePolicies"

data ListRolePoliciesResponse = ListRolePoliciesResponse
    { lrprResponseMetadata :: !ByteString
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
    { lrMarker     :: Maybe ByteString
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
    , lrPathPrefix :: Maybe ByteString
      -- ^ The path prefix for filtering the results. For example:
      -- /application_abc/component_xyz/, which would get all roles whose
      -- path starts with /application_abc/component_xyz/.
    } deriving (Eq, Show, Generic)

instance IsQuery ListRoles

instance AWSRequest IAM ListRoles ListRolesResponse where
    request = req GET "ListRoles"

data ListRolesResponse = ListRolesResponse
    { lrrResponseMetadata :: !ByteString
    , lrrListRolesResult :: !ListRolesResult
    } deriving (Eq, Show, Generic)

instance IsXML ListRolesResponse where
    xmlPickler = withNS iamNS

-- | Lists the server certificates that have the specified path prefix. If none
-- exist, the action returns an empty list. You can paginate the results using
-- the MaxItems and Marker parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListServerCertificates.html>
data ListServerCertificates = ListServerCertificates
    { lscMarker     :: Maybe ByteString
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
    , lscPathPrefix :: Maybe ByteString
      -- ^ The path prefix for filtering the results. For example:
      -- /company/servercerts would get all server certificates for which
      -- the path starts with /company/servercerts.
    } deriving (Eq, Show, Generic)

instance IsQuery ListServerCertificates

instance AWSRequest IAM ListServerCertificates ListServerCertificatesResponse where
    request = req GET "ListServerCertificates"

data ListServerCertificatesResponse = ListServerCertificatesResponse
    { lscrResponseMetadata :: !ByteString
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
    { lsdMarker   :: Maybe ByteString
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
    , lsdUserName :: Maybe ByteString
      -- ^ The name of the user.
    } deriving (Eq, Show, Generic)

instance IsQuery ListSigningCertificates

instance AWSRequest IAM ListSigningCertificates ListSigningCertificatesResponse where
    request = req GET "ListSigningCertificates"

data ListSigningCertificatesResponse = ListSigningCertificatesResponse
    { lscsResponseMetadata :: !ByteString
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
    { lupMarker   :: Maybe ByteString
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
    , lupUserName :: !ByteString
      -- ^ The name of the user to list policies for.
    } deriving (Eq, Show, Generic)

instance IsQuery ListUserPolicies

instance AWSRequest IAM ListUserPolicies ListUserPoliciesResponse where
    request = req GET "ListUserPolicies"

data ListUserPoliciesResponse = ListUserPoliciesResponse
    { luprResponseMetadata :: !ByteString
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
    { luMarker     :: Maybe ByteString
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
    , luPathPrefix :: Maybe ByteString
      -- ^ The path prefix for filtering the results. For example:
      -- /division_abc/subdivision_xyz/, which would get all user names
      -- whose path starts with /division_abc/subdivision_xyz/.
    } deriving (Eq, Show, Generic)

instance IsQuery ListUsers

instance AWSRequest IAM ListUsers ListUsersResponse where
    request = req GET "ListUsers"

data ListUsersResponse = ListUsersResponse
    { lurResponseMetadata :: !ByteString
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
    { lvmfadAssignmentStatus :: Maybe ByteString
      -- ^ The status (unassigned or assigned) of the devices to list. If
      -- you do not specify an AssignmentStatus, the action defaults to
      -- Any which lists both assigned and unassigned virtual MFA devices.
    , lvmfadMarker           :: Maybe ByteString
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

instance AWSRequest IAM ListVirtualMFADevices ListVirtualMFADevicesResponse where
    request = req GET "ListVirtualMFADevices"

data ListVirtualMFADevicesResponse = ListVirtualMFADevicesResponse
    { lvmfadrResponseMetadata :: !ByteString
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
    { pgpGroupName      :: !ByteString
      -- ^ Name of the group to associate the policy with.
    , pgpPolicyDocument :: !ByteString
      -- ^ The policy document.
    , pgpPolicyName     :: !ByteString
      -- ^ Name of the policy document.
    } deriving (Eq, Show, Generic)

instance IsQuery PutGroupPolicy

instance AWSRequest IAM PutGroupPolicy PutGroupPolicyResponse where
    request = req GET "PutGroupPolicy"

data PutGroupPolicyResponse = PutGroupPolicyResponse
    { pgprResponseMetadata :: !ByteString
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
    { prpPolicyDocument :: !ByteString
      -- ^ The policy document.
    , prpPolicyName     :: !ByteString
      -- ^ Name of the policy document.
    , prpRoleName       :: !ByteString
      -- ^ Name of the role to associate the policy with.
    } deriving (Eq, Show, Generic)

instance IsQuery PutRolePolicy

instance AWSRequest IAM PutRolePolicy PutRolePolicyResponse where
    request = req GET "PutRolePolicy"

data PutRolePolicyResponse = PutRolePolicyResponse
    { prprResponseMetadata :: !ByteString
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
    { pupPolicyDocument :: !ByteString
      -- ^ The policy document.
    , pupPolicyName     :: !ByteString
      -- ^ Name of the policy document.
    , pupUserName       :: !ByteString
      -- ^ Name of the user to associate the policy with.
    } deriving (Eq, Show, Generic)

instance IsQuery PutUserPolicy

instance AWSRequest IAM PutUserPolicy PutUserPolicyResponse where
    request = req GET "PutUserPolicy"

data PutUserPolicyResponse = PutUserPolicyResponse
    { puprResponseMetadata :: !ByteString
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
    { rrfipInstanceProfileName :: !ByteString
      -- ^ Name of the instance profile to update.
    , rrfipRoleName            :: !ByteString
      -- ^ Name of the role to remove.
    } deriving (Eq, Show, Generic)

instance IsQuery RemoveRoleFromInstanceProfile

instance AWSRequest IAM RemoveRoleFromInstanceProfile RemoveRoleFromInstanceProfileResponse where
    request = req GET "RemoveRoleFromInstanceProfile"

data RemoveRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse
    { rrfiprResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML RemoveRoleFromInstanceProfileResponse where
    xmlPickler = withNS iamNS

-- | Removes the specified user from the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveUserFromGroup.html>
data RemoveUserFromGroup = RemoveUserFromGroup
    { rufgGroupName :: !ByteString
      -- ^ Name of the group to update.
    , rufgUserName  :: !ByteString
      -- ^ Name of the user to remove.
    } deriving (Eq, Show, Generic)

instance IsQuery RemoveUserFromGroup

instance AWSRequest IAM RemoveUserFromGroup RemoveUserFromGroupResponse where
    request = req GET "RemoveUserFromGroup"

data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse
    { rufgrResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML RemoveUserFromGroupResponse where
    xmlPickler = withNS iamNS

-- | Synchronizes the specified MFA device with AWS servers.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ResyncMFADevice.html>
data ResyncMFADevice = ResyncMFADevice
    { rmfadAuthenticationCode1 :: !ByteString
      -- ^ An authentication code emitted by the device.
    , rmfadAuthenticationCode2 :: !ByteString
      -- ^ A subsequent authentication code emitted by the device.
    , rmfadSerialNumber        :: !ByteString
      -- ^ Serial number that uniquely identifies the MFA device.
    , rmfadUserName            :: !ByteString
      -- ^ Name of the user whose MFA device you want to resynchronize.
    } deriving (Eq, Show, Generic)

instance IsQuery ResyncMFADevice

instance AWSRequest IAM ResyncMFADevice ResyncMFADeviceResponse where
    request = req GET "ResyncMFADevice"

data ResyncMFADeviceResponse = ResyncMFADeviceResponse
    { rmfadrResponseMetadata :: !ByteString
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
    { uakAccessKeyId :: !ByteString
      -- ^ The Access Key ID of the Secret Access Key you want to update.
    , uakStatus      :: !ByteString
      -- ^ The status you want to assign to the Secret Access Key. Active
      -- means the key can be used for API calls to AWS, while Inactive
      -- means the key cannot be used.
    , uakUserName    :: Maybe ByteString
      -- ^ Name of the user whose key you want to update.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateAccessKey

instance AWSRequest IAM UpdateAccessKey UpdateAccessKeyResponse where
    request = req GET "UpdateAccessKey"

data UpdateAccessKeyResponse = UpdateAccessKeyResponse
    { uakrResponseMetadata :: !ByteString
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

instance AWSRequest IAM UpdateAccountPasswordPolicy UpdateAccountPasswordPolicyResponse where
    request = req GET "UpdateAccountPasswordPolicy"

data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse
    { uapprResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML UpdateAccountPasswordPolicyResponse where
    xmlPickler = withNS iamNS

-- | Updates the policy that grants an entity permission to assume a role.
-- Currently, only an Amazon EC2 instance can assume a role. For more
-- information about roles, go to Working with Roles.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAssumeRolePolicy.html>
data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy
    { uarpPolicyDocument :: !ByteString
      -- ^ The policy that grants an entity permission to assume the role.
    , uarpRoleName       :: !ByteString
      -- ^ Name of the role to update.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateAssumeRolePolicy

instance AWSRequest IAM UpdateAssumeRolePolicy UpdateAssumeRolePolicyResponse where
    request = req GET "UpdateAssumeRolePolicy"

data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse
    { uarprResponseMetadata :: !ByteString
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
    { ugGroupName    :: !ByteString
      -- ^ Name of the group to update. If you're changing the name of the
      -- group, this is the original name.
    , ugNewGroupName :: Maybe ByteString
      -- ^ New name for the group. Only include this if changing the group's
      -- name.
    , ugNewPath      :: Maybe ByteString
      -- ^ New path for the group. Only include this if changing the group's
      -- path.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateGroup

instance AWSRequest IAM UpdateGroup UpdateGroupResponse where
    request = req GET "UpdateGroup"

data UpdateGroupResponse = UpdateGroupResponse
    { ugrResponseMetadata :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML UpdateGroupResponse where
    xmlPickler = withNS iamNS

-- | Changes the password for the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateLoginProfile.html>
data UpdateLoginProfile = UpdateLoginProfile
    { ulpPassword :: !ByteString
      -- ^ The new password for the user name.
    , ulpUserName :: !ByteString
      -- ^ Name of the user whose password you want to update.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateLoginProfile

instance AWSRequest IAM UpdateLoginProfile UpdateLoginProfileResponse where
    request = req GET "UpdateLoginProfile"

data UpdateLoginProfileResponse = UpdateLoginProfileResponse
    { ulprResponseMetadata :: !ByteString
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
    { uscNewPath                  :: Maybe ByteString
      -- ^ The new path for the server certificate. Include this only if you
      -- are updating the server certificate's path.
    , uscNewServerCertificateName :: Maybe ByteString
      -- ^ The new name for the server certificate. Include this only if you
      -- are updating the server certificate's name.
    , uscServerCertificateName    :: !ByteString
      -- ^ The name of the server certificate that you want to update.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateServerCertificate

instance AWSRequest IAM UpdateServerCertificate UpdateServerCertificateResponse where
    request = req GET "UpdateServerCertificate"

data UpdateServerCertificateResponse = UpdateServerCertificateResponse
    { uscrResponseMetadata :: !ByteString
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
    { uscCertificateId :: !ByteString
      -- ^ The ID of the signing certificate you want to update.
    , uscStatus        :: !ByteString
      -- ^ The status you want to assign to the certificate. Active means
      -- the certificate can be used for API calls to AWS, while Inactive
      -- means the certificate cannot be used.
    , uscUserName      :: Maybe ByteString
      -- ^ Name of the user the signing certificate belongs to.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateSigningCertificate

instance AWSRequest IAM UpdateSigningCertificate UpdateSigningCertificateResponse where
    request = req GET "UpdateSigningCertificate"

data UpdateSigningCertificateResponse = UpdateSigningCertificateResponse
    { uscsResponseMetadata :: !ByteString
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
    { uuNewPath     :: Maybe ByteString
      -- ^ New path for the user. Include this parameter only if you're
      -- changing the user's path.
    , uuNewUserName :: Maybe ByteString
      -- ^ New name for the user. Include this parameter only if you're
      -- changing the user's name.
    , uuUserName    :: !ByteString
      -- ^ Name of the user to update. If you're changing the name of the
      -- user, this is the original user name.
    } deriving (Eq, Show, Generic)

instance IsQuery UpdateUser

instance AWSRequest IAM UpdateUser UpdateUserResponse where
    request = req GET "UpdateUser"

data UpdateUserResponse = UpdateUserResponse
    { uurResponseMetadata :: !ByteString
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
    { uscCertificateBody       :: !ByteString
      -- ^ The contents of the public key certificate in PEM-encoded format.
    , uscCertificateChain      :: Maybe ByteString
      -- ^ The contents of the certificate chain. This is typically a
      -- concatenation of the PEM-encoded public key certificates of the
      -- chain.
    , uscPath                  :: Maybe ByteString
      -- ^ The path for the server certificate. For more information about
      -- paths, see Identifiers for IAM Entities in Using AWS Identity and
      -- Access Management.
    , uscPrivateKey            :: !ByteString
      -- ^ The contents of the private key in PEM-encoded format.
    , usdServerCertificateName :: !ByteString
      -- ^ The name for the server certificate. Do not include the path in
      -- this value.
    } deriving (Eq, Show, Generic)

instance IsQuery UploadServerCertificate

instance AWSRequest IAM UploadServerCertificate UploadServerCertificateResponse where
    request = req GET "UploadServerCertificate"

data UploadServerCertificateResponse = UploadServerCertificateResponse
    { usctResponseMetadata :: !ByteString
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
    { usdCertificateBody :: !ByteString
      -- ^ The contents of the signing certificate.
    , usdUserName        :: Maybe ByteString
      -- ^ Name of the user the signing certificate is for.
    } deriving (Eq, Show, Generic)

instance IsQuery UploadSigningCertificate

instance AWSRequest IAM UploadSigningCertificate UploadSigningCertificateResponse where
    request = req GET "UploadSigningCertificate"

data UploadSigningCertificateResponse = UploadSigningCertificateResponse
    { uscuResponseMetadata :: !ByteString
    , uscuUploadSigningCertificateResult :: !UploadSigningCertificateResult
    } deriving (Eq, Show, Generic)

instance IsXML UploadSigningCertificateResponse where
    xmlPickler = withNS iamNS
