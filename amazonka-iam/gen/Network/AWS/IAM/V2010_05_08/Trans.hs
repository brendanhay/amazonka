{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.IAM.V2010_05_08.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Identity and Access Management (IAM) enables you to securely control
-- access to AWS services and resources for your users. Using IAM, you can
-- create and manage AWS users and groups and use permissions to allow and
-- deny their access to AWS resources.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.IAM.V2010_05_08.Trans
    (
    -- * AddRoleToInstanceProfile
      addRoleToInstanceProfile
    -- * AddUserToGroup
    , addUserToGroup
    -- * ChangePassword
    , changePassword
    -- * CreateAccessKey
    , createAccessKey
    -- * CreateAccountAlias
    , createAccountAlias
    -- * CreateGroup
    , createGroup
    -- * CreateInstanceProfile
    , createInstanceProfile
    -- * CreateLoginProfile
    , createLoginProfile
    -- * CreateRole
    , createRole
    -- * CreateSAMLProvider
    , createSAMLProvider
    -- * CreateUser
    , createUser
    -- * CreateVirtualMFADevice
    , createVirtualMFADevice
    -- * DeactivateMFADevice
    , deactivateMFADevice
    -- * DeleteAccessKey
    , deleteAccessKey
    -- * DeleteAccountAlias
    , deleteAccountAlias
    -- * DeleteAccountPasswordPolicy
    , deleteAccountPasswordPolicy
    -- * DeleteGroup
    , deleteGroup
    -- * DeleteGroupPolicy
    , deleteGroupPolicy
    -- * DeleteInstanceProfile
    , deleteInstanceProfile
    -- * DeleteLoginProfile
    , deleteLoginProfile
    -- * DeleteRole
    , deleteRole
    -- * DeleteRolePolicy
    , deleteRolePolicy
    -- * DeleteSAMLProvider
    , deleteSAMLProvider
    -- * DeleteServerCertificate
    , deleteServerCertificate
    -- * DeleteSigningCertificate
    , deleteSigningCertificate
    -- * DeleteUser
    , deleteUser
    -- * DeleteUserPolicy
    , deleteUserPolicy
    -- * DeleteVirtualMFADevice
    , deleteVirtualMFADevice
    -- * EnableMFADevice
    , enableMFADevice
    -- * GenerateCredentialReport
    , generateCredentialReport
    -- * GetAccountPasswordPolicy
    , getAccountPasswordPolicy
    -- * GetAccountSummary
    , getAccountSummary
    -- * GetCredentialReport
    , getCredentialReport
    -- * GetGroup
    , getGroup
    -- * GetGroupPolicy
    , getGroupPolicy
    -- * GetInstanceProfile
    , getInstanceProfile
    -- * GetLoginProfile
    , getLoginProfile
    -- * GetRole
    , getRole
    -- * GetRolePolicy
    , getRolePolicy
    -- * GetSAMLProvider
    , getSAMLProvider
    -- * GetServerCertificate
    , getServerCertificate
    -- * GetUser
    , getUser
    -- * GetUserPolicy
    , getUserPolicy
    -- * ListAccessKeys
    , listAccessKeys
    -- * ListAccountAliases
    , listAccountAliases
    -- * ListGroupPolicies
    , listGroupPolicies
    -- * ListGroups
    , listGroups
    -- * ListGroupsForUser
    , listGroupsForUser
    -- * ListInstanceProfiles
    , listInstanceProfiles
    -- * ListInstanceProfilesForRole
    , listInstanceProfilesForRole
    -- * ListMFADevices
    , listMFADevices
    -- * ListRolePolicies
    , listRolePolicies
    -- * ListRoles
    , listRoles
    -- * ListSAMLProviders
    , listSAMLProviders
    -- * ListServerCertificates
    , listServerCertificates
    -- * ListSigningCertificates
    , listSigningCertificates
    -- * ListUserPolicies
    , listUserPolicies
    -- * ListUsers
    , listUsers
    -- * ListVirtualMFADevices
    , listVirtualMFADevices
    -- * PutGroupPolicy
    , putGroupPolicy
    -- * PutRolePolicy
    , putRolePolicy
    -- * PutUserPolicy
    , putUserPolicy
    -- * RemoveRoleFromInstanceProfile
    , removeRoleFromInstanceProfile
    -- * RemoveUserFromGroup
    , removeUserFromGroup
    -- * ResyncMFADevice
    , resyncMFADevice
    -- * UpdateAccessKey
    , updateAccessKey
    -- * UpdateAccountPasswordPolicy
    , updateAccountPasswordPolicy
    -- * UpdateAssumeRolePolicy
    , updateAssumeRolePolicy
    -- * UpdateGroup
    , updateGroup
    -- * UpdateLoginProfile
    , updateLoginProfile
    -- * UpdateSAMLProvider
    , updateSAMLProvider
    -- * UpdateServerCertificate
    , updateServerCertificate
    -- * UpdateSigningCertificate
    , updateSigningCertificate
    -- * UpdateUser
    , updateUser
    -- * UploadServerCertificate
    , uploadServerCertificate
    -- * UploadSigningCertificate
    , uploadSigningCertificate

    -- * Re-exported
    , module Control.Monad.Trans.AWS
    , module Network.AWS.IAM.V2010_05_08
    ) where

import Control.Monad.Trans.AWS
import Network.AWS.Prelude
import Network.AWS.IAM.V2010_05_08

-- | Adds the specified role to the specified instance profile. For more
-- information about roles, go to Working with Roles. For more information
-- about instance profiles, go to About Instance Profiles.
-- https://iam.amazonaws.com/ ?Action=AddRoleToInstanceProfile
-- &InstanceProfileName=Webserver &RoleName=S3Access &Version=2010-05-08
-- &AUTHPARAMS 12657608-99f2-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.AddRoleToInstanceProfile'
addRoleToInstanceProfile :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'artipInstanceProfileName'
                         -> Text -- ^ 'artipRoleName'
                         -> State AddRoleToInstanceProfile a
                         -> m AddRoleToInstanceProfileResponse
addRoleToInstanceProfile p1 p2 s =
    send $ (mkAddRoleToInstanceProfile p1 p2) &~ s

-- | Adds the specified user to the specified group. https://iam.amazonaws.com/
-- ?Action=AddUserToGroup &GroupName=Managers &UserName=Bob &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.AddUserToGroup'
addUserToGroup :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'autgGroupName'
               -> Text -- ^ 'autgUserName'
               -> State AddUserToGroup a
               -> m AddUserToGroupResponse
addUserToGroup p1 p2 s =
    send $ (mkAddUserToGroup p1 p2) &~ s

-- | Changes the password of the IAM user calling ChangePassword. The root
-- account password is not affected by this action. For information about
-- modifying passwords, see Managing Passwords in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=ChangePassword &OldPassword=U79}kgds4?
-- &NewPassword=Lb0*1(9xpN &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ChangePassword'
changePassword :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'cpOldPassword'
               -> Text -- ^ 'cpNewPassword'
               -> State ChangePassword a
               -> m ChangePasswordResponse
changePassword p1 p2 s =
    send $ (mkChangePassword p1 p2) &~ s

-- | Creates a new AWS secret access key and corresponding AWS access key ID for
-- the specified user. The default status for new keys is Active. If you do
-- not specify a user name, IAM determines the user name implicitly based on
-- the AWS access key ID signing the request. Because this action works for
-- access keys under the AWS account, you can use this API to manage root
-- credentials even if the AWS account has no associated users. For
-- information about limits on the number of keys you can create, see
-- Limitations on IAM Entities in the Using IAM guide. To ensure the security
-- of your AWS account, the secret access key is accessible only during key
-- and user creation. You must save the key (for example, in a text file) if
-- you want to be able to access it again. If a secret key is lost, you can
-- delete the access keys for the associated user and then create new keys.
-- https://iam.amazonaws.com/ ?Action=CreateAccessKey &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS Bob AKIAIOSFODNN7EXAMPLE Active
-- wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.CreateAccessKey'
createAccessKey :: ( MonadCatch m
                   , MonadResource m
                   , MonadError Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => State CreateAccessKey a
                -> m CreateAccessKeyResponse
createAccessKey s =
    send (mkCreateAccessKey &~ s)

-- | This action creates an alias for your AWS account. For information about
-- using an AWS account alias, see Using an Alias for Your AWS Account ID in
-- the Using IAM guide. https://iam.amazonaws.com/ ?Action=CreateAccountAlias
-- &AccountAlias=foocorporation &Version=2010-05-08 &AUTHPARAMS
-- 36b5db08-f1b0-11df-8fbe-45274EXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.CreateAccountAlias'
createAccountAlias :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'caaAccountAlias'
                   -> State CreateAccountAlias a
                   -> m CreateAccountAliasResponse
createAccountAlias p1 s =
    send $ (mkCreateAccountAlias p1) &~ s

-- | Creates a new group. For information about the number of groups you can
-- create, see Limitations on IAM Entities in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=CreateGroup &Path=/ &GroupName=Admins
-- &Version=2010-05-08 &AUTHPARAMS / Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.CreateGroup'
createGroup :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'cgGroupName'
            -> State CreateGroup a
            -> m CreateGroupResponse
createGroup p2 s =
    send $ (mkCreateGroup p2) &~ s

-- | Creates a new instance profile. For information about instance profiles, go
-- to About Instance Profiles. For information about the number of instance
-- profiles you can create, see Limitations on IAM Entities in the Using IAM
-- guide. https://iam.amazonaws.com/ ?Action=CreateInstanceProfile
-- &InstanceProfileName=Webserver &Path=/application_abc/component_xyz/
-- &Version=2010-05-08 &AUTHPARAMS AIPAD5ARO2C5EXAMPLE3G Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:11:10.222Z 974142ee-99f1-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.CreateInstanceProfile'
createInstanceProfile :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => Text -- ^ 'cipInstanceProfileName'
                      -> State CreateInstanceProfile a
                      -> m CreateInstanceProfileResponse
createInstanceProfile p1 s =
    send $ (mkCreateInstanceProfile p1) &~ s

-- | Creates a password for the specified user, giving the user the ability to
-- access AWS services through the AWS Management Console. For more
-- information about managing passwords, see Managing Passwords in the Using
-- IAM guide. https://iam.amazonaws.com/ ?Action=CreateLoginProfile
-- &UserName=Bob &Password=Password1 &AUTHPARAMS Bob 2011-09-19T23:00:56Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.CreateLoginProfile'
createLoginProfile :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'clpUserName'
                   -> Text -- ^ 'clpPassword'
                   -> State CreateLoginProfile a
                   -> m CreateLoginProfileResponse
createLoginProfile p1 p2 s =
    send $ (mkCreateLoginProfile p1 p2) &~ s

-- | Creates a new role for your AWS account. For more information about roles,
-- go to Working with Roles. For information about limitations on role names
-- and the number of roles you can create, go to Limitations on IAM Entities
-- in the Using IAM guide. The example policy grants permission to an EC2
-- instance to assume the role. The policy is URL-encoded according to RFC
-- 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=CreateRole &RoleName=S3Access &Path=/application_abc/component_xyz/
-- &AssumeRolePolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- &Version=2010-05-08 &AUTHPARAMS /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-08T23:34:01.495Z AROADBQP57FF2AEXAMPLE
-- 4a93ceee-9966-11e1-b624-b1aEXAMPLE7c.
--
-- See: 'Network.AWS.IAM.V2010_05_08.CreateRole'
createRole :: ( MonadCatch m
              , MonadResource m
              , MonadError Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => Text -- ^ 'crRoleName'
           -> Text -- ^ 'crAssumeRolePolicyDocument'
           -> State CreateRole a
           -> m CreateRoleResponse
createRole p2 p3 s =
    send $ (mkCreateRole p2 p3) &~ s

-- | Creates an IAM entity to describe an identity provider (IdP) that supports
-- SAML 2.0. The SAML provider that you create with this operation can be used
-- as a principal in a role's trust policy to establish a trust relationship
-- between AWS and a SAML identity provider. You can create an IAM role that
-- supports Web-based single sign-on (SSO) to the AWS Management Console or
-- one that supports API access to AWS. When you create the SAML provider, you
-- upload an a SAML metadata document that you get from your IdP and that
-- includes the issuer's name, expiration information, and keys that can be
-- used to validate the SAML authentication response (assertions) that are
-- received from the IdP. You must generate the metadata document using the
-- identity management software that is used as your organization's IdP. This
-- operation requires Signature Version 4. For more information, see Giving
-- Console Access Using SAML and Creating Temporary Security Credentials for
-- SAML Federation in the Using Temporary Credentials guide.
-- https://iam.amazonaws.com/ ?Action=CreateSAMLProvider &Name=MyUniversity
-- &SAMLProviderDocument=VGhpcyBpcyB3aGVyZSB5b3UgcHV0IHRoZSBTQU1MIHByb3ZpZGVyIG1ldGFkYXRhIGRvY3VtZW50
-- LCBCYXNlNjQtZW5jb2RlZCBpbnRvIGEgYmlnIHN0cmluZy4= &Version=2010-05-08
-- &AUTHPARAMS arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- 29f47818-99f5-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.CreateSAMLProvider'
createSAMLProvider :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'csamlpSAMLMetadataDocument'
                   -> Text -- ^ 'csamlpName'
                   -> State CreateSAMLProvider a
                   -> m CreateSAMLProviderResponse
createSAMLProvider p1 p2 s =
    send $ (mkCreateSAMLProvider p1 p2) &~ s

-- | Creates a new user for your AWS account. For information about limitations
-- on the number of users you can create, see Limitations on IAM Entities in
-- the Using IAM guide. https://iam.amazonaws.com/ ?Action=CreateUser
-- &Path=/division_abc/subdivision_xyz/ &UserName=Bob &Version=2010-05-08
-- &AUTHPARAMS /division_abc/subdivision_xyz/ Bob AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.CreateUser'
createUser :: ( MonadCatch m
              , MonadResource m
              , MonadError Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => Text -- ^ 'cuUserName'
           -> State CreateUser a
           -> m CreateUserResponse
createUser p2 s =
    send $ (mkCreateUser p2) &~ s

-- | Creates a new virtual MFA device for the AWS account. After creating the
-- virtual MFA, use EnableMFADevice to attach the MFA device to an IAM user.
-- For more information about creating and working with virtual MFA devices,
-- go to Using a Virtual MFA Device in the Using IAM guide. For information
-- about limits on the number of MFA devices you can create, see Limitations
-- on Entities in the Using IAM guide. The seed information contained in the
-- QR code and the Base32 string should be treated like any other secret
-- access information, such as your AWS access keys or your passwords. After
-- you provision your virtual device, you should ensure that the information
-- is destroyed following secure procedures. https://iam.amazonaws.com/
-- ?Action=CreateVirtualMFADevice &VirtualMFADeviceName=ExampleName &Path=/
-- &Version=2010-05-08 &AUTHPARAMS arn:aws:iam::123456789012:mfa/ExampleName
-- 2K5K5XTLA7GGE75TQLYEXAMPLEEXAMPLEEXAMPLECHDFW4KJYZ6 UFQ75LL7COCYKM
-- 89504E470D0A1A0AASDFAHSDFKJKLJFKALSDFJASDF
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.CreateVirtualMFADevice'
createVirtualMFADevice :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'cvmfadVirtualMFADeviceName'
                       -> State CreateVirtualMFADevice a
                       -> m CreateVirtualMFADeviceResponse
createVirtualMFADevice p2 s =
    send $ (mkCreateVirtualMFADevice p2) &~ s

-- | Deactivates the specified MFA device and removes it from association with
-- the user name for which it was originally enabled.
-- https://iam.amazonaws.com/ ?Action=DeactivateMFADevice &UserName=Bob
-- &SerialNumber=R1234 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeactivateMFADevice'
deactivateMFADevice :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'dmfadUserName'
                    -> Text -- ^ 'dmfadSerialNumber'
                    -> State DeactivateMFADevice a
                    -> m DeactivateMFADeviceResponse
deactivateMFADevice p1 p2 s =
    send $ (mkDeactivateMFADevice p1 p2) &~ s

-- | Deletes the access key associated with the specified user. If you do not
-- specify a user name, IAM determines the user name implicitly based on the
-- AWS access key ID signing the request. Because this action works for access
-- keys under the AWS account, you can use this API to manage root credentials
-- even if the AWS account has no associated users. https://iam.amazonaws.com/
-- ?Action=DeleteAccessKey &UserName=Bob &AccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteAccessKey'
deleteAccessKey :: ( MonadCatch m
                   , MonadResource m
                   , MonadError Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'dakAccessKeyId'
                -> State DeleteAccessKey a
                -> m DeleteAccessKeyResponse
deleteAccessKey p2 s =
    send $ (mkDeleteAccessKey p2) &~ s

-- | Deletes the specified AWS account alias. For information about using an AWS
-- account alias, see Using an Alias for Your AWS Account ID in the Using IAM
-- guide. https://iam.amazonaws.com/ ?Action=DeleteAccountAlias
-- &AccountAlias=foocorporation &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteAccountAlias'
deleteAccountAlias :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'daaAccountAlias'
                   -> State DeleteAccountAlias a
                   -> m DeleteAccountAliasResponse
deleteAccountAlias p1 s =
    send $ (mkDeleteAccountAlias p1) &~ s

-- | Deletes the password policy for the AWS account. https://iam.amazonaws.com/
-- ?Action=DeleteAccountPasswordPolicy &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteAccountPasswordPolicy'
deleteAccountPasswordPolicy :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => State DeleteAccountPasswordPolicy a
                            -> m DeleteAccountPasswordPolicyResponse
deleteAccountPasswordPolicy s =
    send (mkDeleteAccountPasswordPolicy &~ s)

-- | Deletes the specified group. The group must not contain any users or have
-- any attached policies. https://iam.amazonaws.com/ ?Action=DeleteGroup
-- &Group=Test &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteGroup'
deleteGroup :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'dgGroupName'
            -> State DeleteGroup a
            -> m DeleteGroupResponse
deleteGroup p1 s =
    send $ (mkDeleteGroup p1) &~ s

-- | Deletes the specified policy that is associated with the specified group.
-- https://iam.amazonaws.com/ ?Action=DeleteGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteGroupPolicy'
deleteGroupPolicy :: ( MonadCatch m
                     , MonadResource m
                     , MonadError Error m
                     , MonadReader Env m
                     , AWSRequest a
                     )
                  => Text -- ^ 'dgpGroupName'
                  -> Text -- ^ 'dgpPolicyName'
                  -> State DeleteGroupPolicy a
                  -> m DeleteGroupPolicyResponse
deleteGroupPolicy p1 p2 s =
    send $ (mkDeleteGroupPolicy p1 p2) &~ s

-- | Deletes the specified instance profile. The instance profile must not have
-- an associated role. Make sure you do not have any Amazon EC2 instances
-- running with the instance profile you are about to delete. Deleting a role
-- or instance profile that is associated with a running instance will break
-- any applications running on the instance. For more information about
-- instance profiles, go to About Instance Profiles.
-- https://iam.amazonaws.com/ ?Action=DeleteInstanceProfile
-- &InstanceProfileName=Webserver &Version=2010-05-08 &AUTHPARAMS
-- 90c18667-99f3-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteInstanceProfile'
deleteInstanceProfile :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => Text -- ^ 'dipInstanceProfileName'
                      -> State DeleteInstanceProfile a
                      -> m DeleteInstanceProfileResponse
deleteInstanceProfile p1 s =
    send $ (mkDeleteInstanceProfile p1) &~ s

-- | Deletes the password for the specified user, which terminates the user's
-- ability to access AWS services through the AWS Management Console. Deleting
-- a user's password does not prevent a user from accessing IAM through the
-- command line interface or the API. To prevent all user access you must also
-- either make the access key inactive or delete it. For more information
-- about making keys inactive or deleting them, see UpdateAccessKey and
-- DeleteAccessKey. https://iam.amazonaws.com/ ?Action=DeleteLoginProfile
-- &UserName=Bob &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteLoginProfile'
deleteLoginProfile :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'dlpUserName'
                   -> State DeleteLoginProfile a
                   -> m DeleteLoginProfileResponse
deleteLoginProfile p1 s =
    send $ (mkDeleteLoginProfile p1) &~ s

-- | Deletes the specified role. The role must not have any policies attached.
-- For more information about roles, go to Working with Roles. Make sure you
-- do not have any Amazon EC2 instances running with the role you are about to
-- delete. Deleting a role or instance profile that is associated with a
-- running instance will break any applications running on the instance.
-- https://iam.amazonaws.com/ ?Action=DeleteRole &RoleName=S3Access
-- &Version=2010-05-08 &AUTHPARAMS 913e3f37-99ed-11e1-a4c3-270EXAMPLE04.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteRole'
deleteRole :: ( MonadCatch m
              , MonadResource m
              , MonadError Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => Text -- ^ 'drRoleName'
           -> State DeleteRole a
           -> m DeleteRoleResponse
deleteRole p1 s =
    send $ (mkDeleteRole p1) &~ s

-- | Deletes the specified policy associated with the specified role.
-- https://iam.amazonaws.com/ ?Action=DeleteRolePolicy
-- &PolicyName=S3AccessPolicy &RoleName=S3Access &Version=2010-05-08
-- &AUTHPARAMS c749ee7f-99ef-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteRolePolicy'
deleteRolePolicy :: ( MonadCatch m
                    , MonadResource m
                    , MonadError Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'drpRoleName'
                 -> Text -- ^ 'drpPolicyName'
                 -> State DeleteRolePolicy a
                 -> m DeleteRolePolicyResponse
deleteRolePolicy p1 p2 s =
    send $ (mkDeleteRolePolicy p1 p2) &~ s

-- | Deletes a SAML provider. Deleting the provider does not update any roles
-- that reference the SAML provider as a principal in their trust policies.
-- Any attempt to assume a role that references a SAML provider that has been
-- deleted will fail. This operation requires Signature Version 4.
-- https://iam.amazonaws.com/ ?Action=DeleteSAMLProvider
-- &Name=arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- &Version=2010-05-08 &AUTHPARAMS.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteSAMLProvider'
deleteSAMLProvider :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'dsamlpSAMLProviderArn'
                   -> State DeleteSAMLProvider a
                   -> m DeleteSAMLProviderResponse
deleteSAMLProvider p1 s =
    send $ (mkDeleteSAMLProvider p1) &~ s

-- | Deletes the specified server certificate. If you are using a server
-- certificate with Elastic Load Balancing, deleting the certificate could
-- have implications for your application. If Elastic Load Balancing doesn't
-- detect the deletion of bound certificates, it may continue to use the
-- certificates. This could cause Elastic Load Balancing to stop accepting
-- traffic. We recommend that you remove the reference to the certificate from
-- Elastic Load Balancing before using this command to delete the certificate.
-- For more information, go to DeleteLoadBalancerListeners in the Elastic Load
-- Balancing API Reference. https://iam.amazonaws.com/
-- ?Action=DeleteServerCertificate &ServerCertificateName=ProdServerCert
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteServerCertificate'
deleteServerCertificate :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'dscServerCertificateName'
                        -> State DeleteServerCertificate a
                        -> m DeleteServerCertificateResponse
deleteServerCertificate p1 s =
    send $ (mkDeleteServerCertificate p1) &~ s

-- | Deletes the specified signing certificate associated with the specified
-- user. If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID signing the request. Because this
-- action works for access keys under the AWS account, you can use this API to
-- manage root credentials even if the AWS account has no associated users.
-- https://iam.amazonaws.com/ ?Action=DeleteSigningCertificate &UserName=Bob
-- &CertificateId=TA7SMP42TDN5Z26OBPJE7EXAMPLE &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteSigningCertificate'
deleteSigningCertificate :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'dsc1CertificateId'
                         -> State DeleteSigningCertificate a
                         -> m DeleteSigningCertificateResponse
deleteSigningCertificate p2 s =
    send $ (mkDeleteSigningCertificate p2) &~ s

-- | Deletes the specified user. The user must not belong to any groups, have
-- any keys or signing certificates, or have any attached policies.
-- https://iam.amazonaws.com/ ?Action=DeleteUser &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteUser'
deleteUser :: ( MonadCatch m
              , MonadResource m
              , MonadError Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => Text -- ^ 'duUserName'
           -> State DeleteUser a
           -> m DeleteUserResponse
deleteUser p1 s =
    send $ (mkDeleteUser p1) &~ s

-- | Deletes the specified policy associated with the specified user.
-- https://iam.amazonaws.com/ ?Action=DeleteUserPolicy &UserName=Bob
-- &PolicyName=AllAccessPolicy &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteUserPolicy'
deleteUserPolicy :: ( MonadCatch m
                    , MonadResource m
                    , MonadError Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'dupUserName'
                 -> Text -- ^ 'dupPolicyName'
                 -> State DeleteUserPolicy a
                 -> m DeleteUserPolicyResponse
deleteUserPolicy p1 p2 s =
    send $ (mkDeleteUserPolicy p1 p2) &~ s

-- | Deletes a virtual MFA device. You must deactivate a user's virtual MFA
-- device before you can delete it. For information about deactivating MFA
-- devices, see DeactivateMFADevice. https://iam.amazonaws.com/
-- ?Action=DeleteVirtualMFADevice
-- &SerialNumber=arn:aws:iam::123456789012:mfa/ExampleName &Version=2010-05-08
-- &AUTHPARAMS arn:aws:iam::123456789012:mfa/ExampleName
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.DeleteVirtualMFADevice'
deleteVirtualMFADevice :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'dvmfadSerialNumber'
                       -> State DeleteVirtualMFADevice a
                       -> m DeleteVirtualMFADeviceResponse
deleteVirtualMFADevice p1 s =
    send $ (mkDeleteVirtualMFADevice p1) &~ s

-- | Enables the specified MFA device and associates it with the specified user
-- name. When enabled, the MFA device is required for every subsequent login
-- by the user name associated with the device. https://iam.amazonaws.com/
-- ?Action=EnableMFADevice &UserName=Bob &SerialNumber=R1234
-- &AuthenticationCode1=234567 &AuthenticationCode2=987654 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.EnableMFADevice'
enableMFADevice :: ( MonadCatch m
                   , MonadResource m
                   , MonadError Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'emfadUserName'
                -> Text -- ^ 'emfadSerialNumber'
                -> Text -- ^ 'emfadAuthenticationCode1'
                -> Text -- ^ 'emfadAuthenticationCode2'
                -> State EnableMFADevice a
                -> m EnableMFADeviceResponse
enableMFADevice p1 p2 p3 p4 s =
    send $ (mkEnableMFADevice p1 p2 p3 p4) &~ s

-- | Generates a credential report for the AWS account. For more information
-- about the credential report, see Getting Credential Reports in the Using
-- IAM guide.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GenerateCredentialReport'
generateCredentialReport :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => State GenerateCredentialReport a
                         -> m GenerateCredentialReportResponse
generateCredentialReport s =
    send (mkGenerateCredentialReport &~ s)

-- | Retrieves the password policy for the AWS account. For more information
-- about using a password policy, go to Managing an IAM Password Policy.
-- https://iam.amazonaws.com/ ?Action=GetAccountPasswordPolicy
-- &Version=2010-05-08 &AUTHPARAMS 6, false false false false true
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetAccountPasswordPolicy'
getAccountPasswordPolicy :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => State GetAccountPasswordPolicy a
                         -> m GetAccountPasswordPolicyResponse
getAccountPasswordPolicy s =
    send (mkGetAccountPasswordPolicy &~ s)

-- | Retrieves account level information about account entity usage and IAM
-- quotas. For information about limitations on IAM entities, see Limitations
-- on IAM Entities in the Using IAM guide. https://iam.amazonaws.com/
-- ?Action=GetAccountSummary &Version=2010-05-08 &AUTHPARAMS Groups 31
-- GroupsQuota 50 UsersQuota 150 Users 35 GroupPolicySizeQuota 10240
-- AccessKeysPerUserQuota 2 GroupsPerUserQuota 10 UserPolicySizeQuota 10240
-- SigningCertificatesPerUserQuota 2 ServerCertificates 0
-- ServerCertificatesQuota 10 AccountMFAEnabled 0 MFADevicesInUse 10
-- MFADevices 20 f1e38443-f1ad-11df-b1ef-a9265EXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetAccountSummary'
getAccountSummary :: ( MonadCatch m
                     , MonadResource m
                     , MonadError Error m
                     , MonadReader Env m
                     , AWSRequest a
                     )
                  => State GetAccountSummary a
                  -> m GetAccountSummaryResponse
getAccountSummary s =
    send (mkGetAccountSummary &~ s)

-- | Retrieves a credential report for the AWS account. For more information
-- about the credential report, see Getting Credential Reports in the Using
-- IAM guide.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetCredentialReport'
getCredentialReport :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => State GetCredentialReport a
                    -> m GetCredentialReportResponse
getCredentialReport s =
    send (mkGetCredentialReport &~ s)

-- | Returns a list of users that are in the specified group. You can paginate
-- the results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=GetGroup &GroupName=Admins
-- &Version=2010-05-08 &AUTHPARAMS / Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins /division_abc/subdivision_xyz/ Bob
-- AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- /division_abc/subdivision_xyz/ Susan AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Susan false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetGroup'
getGroup :: ( MonadCatch m
            , MonadResource m
            , MonadError Error m
            , MonadReader Env (ResumableSource m)
            , AWSPager a
            )
         => Text -- ^ 'ggGroupName'
         -> State GetGroup a
         -> ResumableSource m GetGroupResponse
getGroup p1 s =
    paginate $ (mkGetGroup p1) &~ s

-- | Retrieves the specified policy document for the specified group. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
-- https://iam.amazonaws.com/ ?Action=GetGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot &AUTHPARAMS Admins AdminRoot
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetGroupPolicy'
getGroupPolicy :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'ggpGroupName'
               -> Text -- ^ 'ggpPolicyName'
               -> State GetGroupPolicy a
               -> m GetGroupPolicyResponse
getGroupPolicy p1 p2 s =
    send $ (mkGetGroupPolicy p1 p2) &~ s

-- | Retrieves information about the specified instance profile, including the
-- instance profile's path, GUID, ARN, and role. For more information about
-- instance profiles, go to About Instance Profiles. For more information
-- about ARNs, go to ARNs. https://iam.amazonaws.com/
-- ?Action=GetInstanceProfile &InstanceProfileName=Webserver
-- &Version=2010-05-08 &AUTHPARAMS AIPAD5ARO2C5EXAMPLE3G
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:35Z AROACVYKSVTSZFEXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:11:10Z 37289fda-99f2-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetInstanceProfile'
getInstanceProfile :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'gipInstanceProfileName'
                   -> State GetInstanceProfile a
                   -> m GetInstanceProfileResponse
getInstanceProfile p1 s =
    send $ (mkGetInstanceProfile p1) &~ s

-- | Retrieves the user name and password-creation date for the specified user.
-- If the user has not been assigned a password, the action returns a 404
-- (NoSuchEntity) error. https://iam.amazonaws.com/ ?Action=GetLoginProfile
-- &UserName=Bob &AUTHPARAMS Bob 2011-09-19T23:00:56Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetLoginProfile'
getLoginProfile :: ( MonadCatch m
                   , MonadResource m
                   , MonadError Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'glpUserName'
                -> State GetLoginProfile a
                -> m GetLoginProfileResponse
getLoginProfile p1 s =
    send $ (mkGetLoginProfile p1) &~ s

-- | Retrieves information about the specified role, including the role's path,
-- GUID, ARN, and the policy granting permission to assume the role. For more
-- information about ARNs, go to ARNs. For more information about roles, go to
-- Working with Roles. The returned policy is URL-encoded according to RFC
-- 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=GetRole &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-08T23:34:01Z AROADBQP57FF2AEXAMPLE
-- df37e965-9967-11e1-a4c3-270EXAMPLE04.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetRole'
getRole :: ( MonadCatch m
           , MonadResource m
           , MonadError Error m
           , MonadReader Env m
           , AWSRequest a
           )
        => Text -- ^ 'grRoleName'
        -> State GetRole a
        -> m GetRoleResponse
getRole p1 s =
    send $ (mkGetRole p1) &~ s

-- | Retrieves the specified policy document for the specified role. For more
-- information about roles, go to Working with Roles. The returned policy is
-- URL-encoded according to RFC 3986. For more information about RFC 3986, go
-- to http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=GetRolePolicy &PolicyName=S3AccessPolicy &RoleName=S3Access
-- &Version=2010-05-08 &AUTHPARAMS S3AccessPolicy S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":["s3:*"],"Resource":["*"]}]}
-- 7e7cd8bc-99ef-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetRolePolicy'
getRolePolicy :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'grpRoleName'
              -> Text -- ^ 'grpPolicyName'
              -> State GetRolePolicy a
              -> m GetRolePolicyResponse
getRolePolicy p1 p2 s =
    send $ (mkGetRolePolicy p1 p2) &~ s

-- | Returns the SAML provider metadocument that was uploaded when the provider
-- was created or updated. This operation requires Signature Version 4.
-- https://iam.amazonaws.com/ ?Action=GetSAMLProvider
-- &Name=arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- &Version=2010-05-08 &AUTHPARAMS 2012-05-09T16:27:11Z 2015-12-31T211:59:59Z
-- Pd9fexDssTkRgGNqs...DxptfEs== 29f47818-99f5-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetSAMLProvider'
getSAMLProvider :: ( MonadCatch m
                   , MonadResource m
                   , MonadError Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'gsamlpSAMLProviderArn'
                -> State GetSAMLProvider a
                -> m GetSAMLProviderResponse
getSAMLProvider p1 s =
    send $ (mkGetSAMLProvider p1) &~ s

-- | Retrieves information about the specified server certificate.
-- https://iam.amazonaws.com/ ?Action=GetServerCertificate
-- &ServerCertificateName=ProdServerCert &Version=2010-05-08 &AUTHPARAMS
-- ProdServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/ProdServerCert
-- 2010-05-08T01:02:03.004Z ASCACKCEVSQ6C2EXAMPLE 2012-05-08T01:02:03.004Z
-- -----BEGIN CERTIFICATE-----
-- MIICdzCCAeCgAwIBAgIGANc+Ha2wMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNVBAYT
-- AlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMQwwCgYDVQQLEwNBV1MxITAfBgNVBAMT
-- GEFXUyBMaW1pdGVkLUFzc3VyYW5jZSBDQTAeFw0wOTAyMDQxNzE5MjdaFw0xMDAy
-- MDQxNzE5MjdaMFIxCzAJBgNVBAYTAlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMRcw
-- FQYDVQQLEw5BV1MtRGV2ZWxvcGVyczEVMBMGA1UEAxMMNTdxNDl0c3ZwYjRtMIGf
-- MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCpB/vsOwmT/O0td1RqzKjttSBaPjbr
-- dqwNe9BrOyB08fw2+Ch5oonZYXfGUrT6mkYXH5fQot9HvASrzAKHO596FdJA6DmL
-- ywdWe1Oggk7zFSXO1Xv+3vPrJtaYxYo3eRIp7w80PMkiOv6M0XK8ubcTouODeJbf
-- suDqcLnLDxwsvwIDAQABo1cwVTAOBgNVHQ8BAf8EBAMCBaAwFgYDVR0lAQH/BAww
-- CgYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQULGNaBphBumaKbDRK
-- CAi0mH8B3mowDQYJKoZIhvcNAQEFBQADgYEAuKxhkXaCLGcqDuweKtO/AEw9ZePH
-- wr0XqsaIK2HZboqruebXEGsojK4Ks0WzwgrEynuHJwTn760xe39rSqXWIOGrOBaX
-- wFpWHVjTFMKk+tSDG1lssLHyYWWdFFU4AnejRGORJYNaRHgVTKjHphc5jEhHm0BX
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE-----
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetServerCertificate'
getServerCertificate :: ( MonadCatch m
                        , MonadResource m
                        , MonadError Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => Text -- ^ 'gscServerCertificateName'
                     -> State GetServerCertificate a
                     -> m GetServerCertificateResponse
getServerCertificate p1 s =
    send $ (mkGetServerCertificate p1) &~ s

-- | Retrieves information about the specified user, including the user's path,
-- unique ID, and ARN. If you do not specify a user name, IAM determines the
-- user name implicitly based on the AWS access key ID signing the request.
-- https://iam.amazonaws.com/ ?Action=GetUser &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS /division_abc/subdivision_xyz/ Bob
-- AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetUser'
getUser :: ( MonadCatch m
           , MonadResource m
           , MonadError Error m
           , MonadReader Env m
           , AWSRequest a
           )
        => State GetUser a
        -> m GetUserResponse
getUser s =
    send (mkGetUser &~ s)

-- | Retrieves the specified policy document for the specified user. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
-- https://iam.amazonaws.com/ ?Action=GetUserPolicy &UserName=Bob
-- &PolicyName=AllAccessPolicy &AUTHPARAMS Bob AllAccessPolicy
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.GetUserPolicy'
getUserPolicy :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'gupUserName'
              -> Text -- ^ 'gupPolicyName'
              -> State GetUserPolicy a
              -> m GetUserPolicyResponse
getUserPolicy p1 p2 s =
    send $ (mkGetUserPolicy p1 p2) &~ s

-- | Returns information about the access key IDs associated with the specified
-- user. If there are none, the action returns an empty list. Although each
-- user is limited to a small number of keys, you can still paginate the
-- results using the MaxItems and Marker parameters. If the UserName field is
-- not specified, the UserName is determined implicitly based on the AWS
-- access key ID used to sign the request. Because this action works for
-- access keys under the AWS account, this API can be used to manage root
-- credentials even if the AWS account has no associated users. To ensure the
-- security of your AWS account, the secret access key is accessible only
-- during key and user creation. https://iam.amazonaws.com/
-- ?Action=ListAccessKeys &UserName=Bob &Version=2010-05-08 &AUTHPARAMS Bob
-- Bob AKIAIOSFODNN7EXAMPLE Active Bob AKIAI44QH8DHBEXAMPLE Inactive false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListAccessKeys'
listAccessKeys :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env (ResumableSource m)
                  , AWSPager a
                  )
               => State ListAccessKeys a
               -> ResumableSource m ListAccessKeysResponse
listAccessKeys s =
    paginate (mkListAccessKeys &~ s)

-- | Lists the account aliases associated with the account. For information
-- about using an AWS account alias, see Using an Alias for Your AWS Account
-- ID in the Using IAM guide. You can paginate the results using the MaxItems
-- and Marker parameters. https://iam.amazonaws.com/
-- ?Action=ListAccountAliases &Version=2010-05-08 &AUTHPARAMS false
-- foocorporation c5a076e9-f1b0-11df-8fbe-45274EXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListAccountAliases'
listAccountAliases :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env (ResumableSource m)
                      , AWSPager a
                      )
                   => State ListAccountAliases a
                   -> ResumableSource m ListAccountAliasesResponse
listAccountAliases s =
    paginate (mkListAccountAliases &~ s)

-- | Lists the names of the policies associated with the specified group. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListGroupPolicies &GroupName=Admins
-- &AUTHPARAMS AdminRoot KeyPolicy false 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListGroupPolicies'
listGroupPolicies :: ( MonadCatch m
                     , MonadResource m
                     , MonadError Error m
                     , MonadReader Env (ResumableSource m)
                     , AWSPager a
                     )
                  => Text -- ^ 'lgpGroupName'
                  -> State ListGroupPolicies a
                  -> ResumableSource m ListGroupPoliciesResponse
listGroupPolicies p1 s =
    paginate $ (mkListGroupPolicies p1) &~ s

-- | Lists the groups that have the specified path prefix. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListGroups
-- &PathPrefix=/division_abc/subdivision_xyz/ &Version=2010-05-08 &AUTHPARAMS
-- /division_abc/subdivision_xyz/ Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins
-- /division_abc/subdivision_xyz/product_1234/engineering/ Test
-- AGP2MAB8DPLSRHEXAMPLE arn:aws:iam::123456789012:group
-- /division_abc/subdivision_xyz/product_1234/engineering/Test
-- /division_abc/subdivision_xyz/product_1234/ Managers AGPIODR4TAW7CSEXAMPLE
-- arn:aws:iam::123456789012
-- :group/division_abc/subdivision_xyz/product_1234/Managers false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListGroups'
listGroups :: ( MonadCatch m
              , MonadResource m
              , MonadError Error m
              , MonadReader Env (ResumableSource m)
              , AWSPager a
              )
           => State ListGroups a
           -> ResumableSource m ListGroupsResponse
listGroups s =
    paginate (mkListGroups &~ s)

-- | Lists the groups the specified user belongs to. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListGroupsForUser &UserName=Bob
-- &AUTHPARAMS / Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListGroupsForUser'
listGroupsForUser :: ( MonadCatch m
                     , MonadResource m
                     , MonadError Error m
                     , MonadReader Env (ResumableSource m)
                     , AWSPager a
                     )
                  => Text -- ^ 'lgfuUserName'
                  -> State ListGroupsForUser a
                  -> ResumableSource m ListGroupsForUserResponse
listGroupsForUser p1 s =
    paginate $ (mkListGroupsForUser p1) &~ s

-- | Lists the instance profiles that have the specified path prefix. If there
-- are none, the action returns an empty list. For more information about
-- instance profiles, go to About Instance Profiles. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListInstanceProfiles &MaxItems=100
-- &PathPrefix=/application_abc/ &Version=2010-05-08 &AUTHPARAMS false
-- AIPACIFN4OZXG7EXAMPLE Database /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Database
-- 2012-05-09T16:27:03Z AIPACZLSXM2EYYEXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:27:11Z fd74fa8d-99f3-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListInstanceProfiles'
listInstanceProfiles :: ( MonadCatch m
                        , MonadResource m
                        , MonadError Error m
                        , MonadReader Env (ResumableSource m)
                        , AWSPager a
                        )
                     => State ListInstanceProfiles a
                     -> ResumableSource m ListInstanceProfilesResponse
listInstanceProfiles s =
    paginate (mkListInstanceProfiles &~ s)

-- | Lists the instance profiles that have the specified associated role. If
-- there are none, the action returns an empty list. For more information
-- about instance profiles, go to About Instance Profiles. You can paginate
-- the results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListInstanceProfilesForRole
-- &MaxItems=100 &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS false
-- AIPACZLS2EYYXMEXAMPLE /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:35Z AROACVSVTSZYK3EXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:27:11Z 6a8c3992-99f4-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListInstanceProfilesForRole'
listInstanceProfilesForRole :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env (ResumableSource m)
                               , AWSPager a
                               )
                            => Text -- ^ 'lipfrRoleName'
                            -> State ListInstanceProfilesForRole a
                            -> ResumableSource m ListInstanceProfilesForRoleResponse
listInstanceProfilesForRole p1 s =
    paginate $ (mkListInstanceProfilesForRole p1) &~ s

-- | Lists the MFA devices. If the request includes the user name, then this
-- action lists all the MFA devices associated with the specified user name.
-- If you do not specify a user name, IAM determines the user name implicitly
-- based on the AWS access key ID signing the request. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListMFADevices &UserName=Bob &AUTHPARAMS
-- Bob R1234 false 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListMFADevices'
listMFADevices :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env (ResumableSource m)
                  , AWSPager a
                  )
               => State ListMFADevices a
               -> ResumableSource m ListMFADevicesResponse
listMFADevices s =
    paginate (mkListMFADevices &~ s)

-- | Lists the names of the policies associated with the specified role. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListRolePolicies &MaxItems=100
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- CloudwatchPutMetricPolicy S3AccessPolicy false
-- 8c7e1816-99f0-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListRolePolicies'
listRolePolicies :: ( MonadCatch m
                    , MonadResource m
                    , MonadError Error m
                    , MonadReader Env (ResumableSource m)
                    , AWSPager a
                    )
                 => Text -- ^ 'lrpRoleName'
                 -> State ListRolePolicies a
                 -> ResumableSource m ListRolePoliciesResponse
listRolePolicies p1 s =
    paginate $ (mkListRolePolicies p1) &~ s

-- | Lists the roles that have the specified path prefix. If there are none, the
-- action returns an empty list. For more information about roles, go to
-- Working with Roles. You can paginate the results using the MaxItems and
-- Marker parameters. The returned policy is URL-encoded according to RFC
-- 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=ListRoles &MaxItems=100 &PathPrefix=/application_abc/
-- &Version=2010-05-08 &AUTHPARAMS false /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:35Z AROACVSVTSZYEXAMPLEYK /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/SDBAccess
-- SDBAccess
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:45Z AROAC2ICXG32EXAMPLEWK
-- 20f7279f-99ee-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListRoles'
listRoles :: ( MonadCatch m
             , MonadResource m
             , MonadError Error m
             , MonadReader Env (ResumableSource m)
             , AWSPager a
             )
          => State ListRoles a
          -> ResumableSource m ListRolesResponse
listRoles s =
    paginate (mkListRoles &~ s)

-- | Lists the SAML providers in the account. This operation requires Signature
-- Version 4. https://iam.amazonaws.com/ ?Action=ListSAMLProviders
-- &MaxItems=100 &PathPrefix=/application_abc/ &Version=2010-05-08 &AUTHPARAMS
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Database
-- 2032-05-09T16:27:11Z 2012-05-09T16:27:03Z
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2015-03-11T13:11:02Z 2012-05-09T16:27:11Z
-- fd74fa8d-99f3-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListSAMLProviders'
listSAMLProviders :: ( MonadCatch m
                     , MonadResource m
                     , MonadError Error m
                     , MonadReader Env m
                     , AWSRequest a
                     )
                  => State ListSAMLProviders a
                  -> m ListSAMLProvidersResponse
listSAMLProviders s =
    send (mkListSAMLProviders &~ s)

-- | Lists the server certificates that have the specified path prefix. If none
-- exist, the action returns an empty list. You can paginate the results using
-- the MaxItems and Marker parameters. https://iam.amazonaws.com/
-- ?Action=ListServerCertificates &PathPrefix=/company/servercerts
-- &Version=2010-05-08 &AUTHPARAMS false ProdServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/ProdServerCert
-- 2010-05-08T01:02:03.004Z ASCACKCEVSQ6CEXAMPLE1 2012-05-08T01:02:03.004Z
-- BetaServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/BetaServerCert
-- 2010-05-08T02:03:01.004Z ASCACKCEVSQ6CEXAMPLE2 2012-05-08T02:03:01.004Z
-- TestServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/TestServerCert
-- 2010-05-08T03:01:02.004Z ASCACKCEVSQ6CEXAMPLE3 2012-05-08T03:01:02.004Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListServerCertificates'
listServerCertificates :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env (ResumableSource m)
                          , AWSPager a
                          )
                       => State ListServerCertificates a
                       -> ResumableSource m ListServerCertificatesResponse
listServerCertificates s =
    paginate (mkListServerCertificates &~ s)

-- | Returns information about the signing certificates associated with the
-- specified user. If there are none, the action returns an empty list.
-- Although each user is limited to a small number of signing certificates,
-- you can still paginate the results using the MaxItems and Marker
-- parameters. If the UserName field is not specified, the user name is
-- determined implicitly based on the AWS access key ID used to sign the
-- request. Because this action works for access keys under the AWS account,
-- this API can be used to manage root credentials even if the AWS account has
-- no associated users. https://iam.amazonaws.com/
-- ?Action=ListSigningCertificates &UserName=Bob &Version=2010-05-08
-- &AUTHPARAMS Bob Bob TA7SMP42TDN5Z26OBPJE7EXAMPLE -----BEGIN
-- CERTIFICATE-----
-- MIICdzCCAeCgAwIBAgIGANc+Ha2wMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNVBAYT
-- AlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMQwwCgYDVQQLEwNBV1MxITAfBgNVBAMT
-- GEFXUyBMaW1pdGVkLUFzc3VyYW5jZSBDQTAeFw0wOTAyMDQxNzE5MjdaFw0xMDAy
-- MDQxNzE5MjdaMFIxCzAJBgNVBAYTAlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMRcw
-- FQYDVQQLEw5BV1MtRGV2ZWxvcGVyczEVMBMGA1UEAxMMNTdxNDl0c3ZwYjRtMIGf
-- MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCpB/vsOwmT/O0td1RqzKjttSBaPjbr
-- dqwNe9BrOyB08fw2+Ch5oonZYXfGUrT6mkYXH5fQot9HvASrzAKHO596FdJA6DmL
-- ywdWe1Oggk7zFSXO1Xv+3vPrJtaYxYo3eRIp7w80PMkiOv6M0XK8ubcTouODeJbf
-- suDqcLnLDxwsvwIDAQABo1cwVTAOBgNVHQ8BAf8EBAMCBaAwFgYDVR0lAQH/BAww
-- CgYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQULGNaBphBumaKbDRK
-- CAi0mH8B3mowDQYJKoZIhvcNAQEFBQADgYEAuKxhkXaCLGcqDuweKtO/AEw9ZePH
-- wr0XqsaIK2HZboqruebXEGsojK4Ks0WzwgrEynuHJwTn760xe39rSqXWIOGrOBaX
-- wFpWHVjTFMKk+tSDG1lssLHyYWWdFFU4AnejRGORJYNaRHgVTKjHphc5jEhHm0BX
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE----- Active false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListSigningCertificates'
listSigningCertificates :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env (ResumableSource m)
                           , AWSPager a
                           )
                        => State ListSigningCertificates a
                        -> ResumableSource m ListSigningCertificatesResponse
listSigningCertificates s =
    paginate (mkListSigningCertificates &~ s)

-- | Lists the names of the policies associated with the specified user. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListUserPolicies &UserName=Bob
-- &AUTHPARAMS AllAccessPolicy KeyPolicy false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListUserPolicies'
listUserPolicies :: ( MonadCatch m
                    , MonadResource m
                    , MonadError Error m
                    , MonadReader Env (ResumableSource m)
                    , AWSPager a
                    )
                 => Text -- ^ 'lupUserName'
                 -> State ListUserPolicies a
                 -> ResumableSource m ListUserPoliciesResponse
listUserPolicies p1 s =
    paginate $ (mkListUserPolicies p1) &~ s

-- | Lists the users that have the specified path prefix. If there are none, the
-- action returns an empty list. You can paginate the results using the
-- MaxItems and Marker parameters. https://iam.amazonaws.com/
-- ?Action=ListUsers
-- &PathPrefix=/division_abc/subdivision_xyz/product_1234/engineering/
-- &Version=2010-05-08 &AUTHPARAMS /division_abc/subdivision_xyz/engineering/
-- Andrew AID2MAB8DPLSRHEXAMPLE arn:aws:iam::123456789012:user
-- /division_abc/subdivision_xyz/engineering/Andrew
-- /division_abc/subdivision_xyz/engineering/ Jackie AIDIODR4TAW7CSEXAMPLE
-- arn:aws:iam::123456789012:user
-- /division_abc/subdivision_xyz/engineering/Jackie false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListUsers'
listUsers :: ( MonadCatch m
             , MonadResource m
             , MonadError Error m
             , MonadReader Env (ResumableSource m)
             , AWSPager a
             )
          => State ListUsers a
          -> ResumableSource m ListUsersResponse
listUsers s =
    paginate (mkListUsers &~ s)

-- | Lists the virtual MFA devices under the AWS account by assignment status.
-- If you do not specify an assignment status, the action returns a list of
-- all virtual MFA devices. Assignment status can be Assigned, Unassigned, or
-- Any. You can paginate the results using the MaxItems and Marker parameters.
-- the AssignmentStatus is Any --> https://iam.amazonaws.com/
-- ?Action=ListVirtualMFADevices &AssignmentStatus=Any &AUTHPARAMS associated
-- with the account: the first device is unassigned, the second is assigned to
-- the root account, and the third is assigned to a user named ExampleUser
-- under the account. --> false arn:aws:iam::123456789012:mfa/MFAdeviceName
-- arn:aws:iam::123456789012:mfa/RootMFAdeviceName 2011-10-20T20:49:03Z
-- 123456789012 arn:aws:iam::123456789012:root 2009-10-13T22:00:36Z
-- arn:aws:iam:::mfa/ExampleUserMFAdeviceName 2011-10-31T20:45:02Z
-- AIDEXAMPLE4EXAMPLEXYZ / ExampleUser
-- arn:aws:iam::111122223333:user/ExampleUser 2011-07-01T17:23:07Z
-- b61ce1b1-0401-11e1-b2f8-2dEXAMPLEbfc.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ListVirtualMFADevices'
listVirtualMFADevices :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env (ResumableSource m)
                         , AWSPager a
                         )
                      => State ListVirtualMFADevices a
                      -> ResumableSource m ListVirtualMFADevicesResponse
listVirtualMFADevices s =
    paginate (mkListVirtualMFADevices &~ s)

-- | Adds (or updates) a policy document associated with the specified group.
-- For information about policies, refer to Overview of Policies in the Using
-- IAM guide. For information about limits on the number of policies you can
-- associate with a group, see Limitations on IAM Entities in the Using IAM
-- guide. Because policy documents can be large, you should use POST rather
-- than GET when calling PutGroupPolicy. For information about setting up
-- signatures and authorization through the API, go to Signing AWS API
-- Requests in the AWS General Reference. For general information about using
-- the Query API with IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=PutGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.PutGroupPolicy'
putGroupPolicy :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'pgpGroupName'
               -> Text -- ^ 'pgpPolicyName'
               -> Text -- ^ 'pgpPolicyDocument'
               -> State PutGroupPolicy a
               -> m PutGroupPolicyResponse
putGroupPolicy p1 p2 p3 s =
    send $ (mkPutGroupPolicy p1 p2 p3) &~ s

-- | Adds (or updates) a policy document associated with the specified role. For
-- information about policies, go to Overview of Policies in the Using IAM
-- guide. For information about limits on the policies you can associate with
-- a role, see Limitations on IAM Entities in the Using IAM guide. Because
-- policy documents can be large, you should use POST rather than GET when
-- calling PutRolePolicy. For information about setting up signatures and
-- authorization through the API, go to Signing AWS API Requests in the AWS
-- General Reference. For general information about using the Query API with
-- IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=PutRolePolicy &RoleName=S3Access
-- &PolicyName=S3AccessPolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"s3:*","Resource":"*"}]}
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.PutRolePolicy'
putRolePolicy :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'prpRoleName'
              -> Text -- ^ 'prpPolicyName'
              -> Text -- ^ 'prpPolicyDocument'
              -> State PutRolePolicy a
              -> m PutRolePolicyResponse
putRolePolicy p1 p2 p3 s =
    send $ (mkPutRolePolicy p1 p2 p3) &~ s

-- | Adds (or updates) a policy document associated with the specified user. For
-- information about policies, refer to Overview of Policies in the Using IAM
-- guide. For information about limits on the number of policies you can
-- associate with a user, see Limitations on IAM Entities in the Using IAM
-- guide. Because policy documents can be large, you should use POST rather
-- than GET when calling PutUserPolicy. For information about setting up
-- signatures and authorization through the API, go to Signing AWS API
-- Requests in the AWS General Reference. For general information about using
-- the Query API with IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=PutUserPolicy &UserName=Bob
-- &PolicyName=AllAccessPolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.PutUserPolicy'
putUserPolicy :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'pupUserName'
              -> Text -- ^ 'pupPolicyName'
              -> Text -- ^ 'pupPolicyDocument'
              -> State PutUserPolicy a
              -> m PutUserPolicyResponse
putUserPolicy p1 p2 p3 s =
    send $ (mkPutUserPolicy p1 p2 p3) &~ s

-- | Removes the specified role from the specified instance profile. Make sure
-- you do not have any Amazon EC2 instances running with the role you are
-- about to remove from the instance profile. Removing a role from an instance
-- profile that is associated with a running instance will break any
-- applications running on the instance. For more information about roles, go
-- to Working with Roles. For more information about instance profiles, go to
-- About Instance Profiles. https://iam.amazonaws.com/
-- ?Action=RemoveRoleFromInstanceProfile &InstanceProfileName=Webserver
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- 29f47818-99f5-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.RemoveRoleFromInstanceProfile'
removeRoleFromInstanceProfile :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError Error m
                                 , MonadReader Env m
                                 , AWSRequest a
                                 )
                              => Text -- ^ 'rrfipInstanceProfileName'
                              -> Text -- ^ 'rrfipRoleName'
                              -> State RemoveRoleFromInstanceProfile a
                              -> m RemoveRoleFromInstanceProfileResponse
removeRoleFromInstanceProfile p1 p2 s =
    send $ (mkRemoveRoleFromInstanceProfile p1 p2) &~ s

-- | Removes the specified user from the specified group.
-- https://iam.amazonaws.com/ ?Action=RemoveUserFromGroup &GroupName=Managers
-- &UserName=Bob &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.RemoveUserFromGroup'
removeUserFromGroup :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'rufgGroupName'
                    -> Text -- ^ 'rufgUserName'
                    -> State RemoveUserFromGroup a
                    -> m RemoveUserFromGroupResponse
removeUserFromGroup p1 p2 s =
    send $ (mkRemoveUserFromGroup p1 p2) &~ s

-- | Synchronizes the specified MFA device with AWS servers.
-- https://iam.amazonaws.com/ ?Action=ResyncMFADevice &UserName=Bob
-- &SerialNumber=R1234 &AuthenticationCode1=234567 &AuthenticationCode2=987654
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.ResyncMFADevice'
resyncMFADevice :: ( MonadCatch m
                   , MonadResource m
                   , MonadError Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'rmfadUserName'
                -> Text -- ^ 'rmfadSerialNumber'
                -> Text -- ^ 'rmfadAuthenticationCode1'
                -> Text -- ^ 'rmfadAuthenticationCode2'
                -> State ResyncMFADevice a
                -> m ResyncMFADeviceResponse
resyncMFADevice p1 p2 p3 p4 s =
    send $ (mkResyncMFADevice p1 p2 p3 p4) &~ s

-- | Changes the status of the specified access key from Active to Inactive, or
-- vice versa. This action can be used to disable a user's key as part of a
-- key rotation work flow. If the UserName field is not specified, the
-- UserName is determined implicitly based on the AWS access key ID used to
-- sign the request. Because this action works for access keys under the AWS
-- account, this API can be used to manage root credentials even if the AWS
-- account has no associated users. For information about rotating keys, see
-- Managing Keys and Certificates in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=UpdateAccessKey &UserName=Bob
-- &AccessKeyId=AKIAIOSFODNN7EXAMPLE &Status=Inactive &Version=2010-05-08
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UpdateAccessKey'
updateAccessKey :: ( MonadCatch m
                   , MonadResource m
                   , MonadError Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'uakAccessKeyId'
                -> StatusType -- ^ 'uakStatus'
                -> State UpdateAccessKey a
                -> m UpdateAccessKeyResponse
updateAccessKey p2 p3 s =
    send $ (mkUpdateAccessKey p2 p3) &~ s

-- | Updates the password policy settings for the account. For more information
-- about using a password policy, see Managing an IAM Password Policy in the
-- Using IAM guide. https://iam.amazonaws.com/
-- ?Action=UpdateAccountPasswordPolicy &MinimumPasswordLength=12
-- &RequireSymbols=false &RequireNumbers=true &RequireUppercaseCharacters=true
-- &RequireLowercaseCharacters=true &AllowUsersToChangePassword=true
-- &MaxPasswordAge=90 &PasswordReusePrevention=6 &HardExpiry=false
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UpdateAccountPasswordPolicy'
updateAccountPasswordPolicy :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => State UpdateAccountPasswordPolicy a
                            -> m UpdateAccountPasswordPolicyResponse
updateAccountPasswordPolicy s =
    send (mkUpdateAccountPasswordPolicy &~ s)

-- | Updates the policy that grants an entity permission to assume a role. For
-- more information about roles, go to Working with Roles.
-- https://iam.amazonaws.com/ ?Action=UpdateAssumeRolePolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- 309c1671-99ed-11e1-a4c3-270EXAMPLE04.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UpdateAssumeRolePolicy'
updateAssumeRolePolicy :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'uarpRoleName'
                       -> Text -- ^ 'uarpPolicyDocument'
                       -> State UpdateAssumeRolePolicy a
                       -> m UpdateAssumeRolePolicyResponse
updateAssumeRolePolicy p1 p2 s =
    send $ (mkUpdateAssumeRolePolicy p1 p2) &~ s

-- | Updates the name and/or the path of the specified group. You should
-- understand the implications of changing a group's path or name. For more
-- information, see Renaming Users and Groups in the Using IAM guide. To
-- change a group name the requester must have appropriate permissions on both
-- the source object and the target object. For example, to change Managers to
-- MGRs, the entity making the request must have permission on Managers and
-- MGRs, or must have permission on all (*). For more information about
-- permissions, see Permissions and Policies. https://iam.amazonaws.com/
-- ?Action=UpdateGroup &GroupName=Test &NewGroupName=Test_1
-- &Version=2010-05-08 &AUTHPARAMS
-- /division_abc/subdivision_xyz/product_1234/engineering/ Test_1
-- AGP2MAB8DPLSRHEXAMPLE
-- arn:aws:iam::123456789012:group/division_abc/subdivision_xyz/
-- product_1234/engineering/Test_1 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UpdateGroup'
updateGroup :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'ugGroupName'
            -> State UpdateGroup a
            -> m UpdateGroupResponse
updateGroup p1 s =
    send $ (mkUpdateGroup p1) &~ s

-- | Changes the password for the specified user. https://iam.amazonaws.com/
-- ?Action=UpdateLoginProfile &UserName=Bob &Password=NewPassword &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UpdateLoginProfile'
updateLoginProfile :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'ulpUserName'
                   -> State UpdateLoginProfile a
                   -> m UpdateLoginProfileResponse
updateLoginProfile p1 s =
    send $ (mkUpdateLoginProfile p1) &~ s

-- | Updates the metadata document for an existing SAML provider. This operation
-- requires Signature Version 4. https://iam.amazonaws.com/
-- ?Action=UpdateSAMLProvider
-- &Name=arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- &SAMLProviderDocument=VGhpcyBpcyB3aGVyZSB5b3UgcHV0IHRoZSBTQU1MIHByb3ZpZGVyIG1ldGFkYXRhIGRvY3VtZW50
-- LCBCYXNlNjQtZW5jb2RlZCBpbnRvIGEgYmlnIHN0cmluZy4= &Version=2010-05-08
-- &AUTHPARAMS arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- 29f47818-99f5-11e1-a4c3-27EXAMPLE804.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UpdateSAMLProvider'
updateSAMLProvider :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => Text -- ^ 'usamlpSAMLMetadataDocument'
                   -> Text -- ^ 'usamlpSAMLProviderArn'
                   -> State UpdateSAMLProvider a
                   -> m UpdateSAMLProviderResponse
updateSAMLProvider p1 p2 s =
    send $ (mkUpdateSAMLProvider p1 p2) &~ s

-- | Updates the name and/or the path of the specified server certificate. You
-- should understand the implications of changing a server certificate's path
-- or name. For more information, see Managing Server Certificates in the
-- Using IAM guide. To change a server certificate name the requester must
-- have appropriate permissions on both the source object and the target
-- object. For example, to change the name from ProductionCert to ProdCert,
-- the entity making the request must have permission on ProductionCert and
-- ProdCert, or must have permission on all (*). For more information about
-- permissions, see Permissions and Policies. https://iam.amazonaws.com/
-- ?Action=UpdateServerCertificate &ServerCertificateName=ProdServerCert
-- &NewServerCertificateName=ProdServerCertName &Version=2010-05-08
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UpdateServerCertificate'
updateServerCertificate :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'uscServerCertificateName'
                        -> State UpdateServerCertificate a
                        -> m UpdateServerCertificateResponse
updateServerCertificate p1 s =
    send $ (mkUpdateServerCertificate p1) &~ s

-- | Changes the status of the specified signing certificate from active to
-- disabled, or vice versa. This action can be used to disable a user's
-- signing certificate as part of a certificate rotation work flow. If the
-- UserName field is not specified, the UserName is determined implicitly
-- based on the AWS access key ID used to sign the request. Because this
-- action works for access keys under the AWS account, this API can be used to
-- manage root credentials even if the AWS account has no associated users.
-- For information about rotating certificates, see Managing Keys and
-- Certificates in the Using IAM guide. https://iam.amazonaws.com/
-- ?Action=UpdateSigningCertificate &UserName=Bob
-- &CertificateId=TA7SMP42TDN5Z26OBPJE7EXAMPLE &Status=Inactive
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UpdateSigningCertificate'
updateSigningCertificate :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'usc1CertificateId'
                         -> StatusType -- ^ 'usc1Status'
                         -> State UpdateSigningCertificate a
                         -> m UpdateSigningCertificateResponse
updateSigningCertificate p2 p3 s =
    send $ (mkUpdateSigningCertificate p2 p3) &~ s

-- | Updates the name and/or the path of the specified user. You should
-- understand the implications of changing a user's path or name. For more
-- information, see Renaming Users and Groups in the Using IAM guide. To
-- change a user name the requester must have appropriate permissions on both
-- the source object and the target object. For example, to change Bob to
-- Robert, the entity making the request must have permission on Bob and
-- Robert, or must have permission on all (*). For more information about
-- permissions, see Permissions and Policies. https://iam.amazonaws.com/
-- ?Action=UpdateUser &UserName=Bob &NewUserName=Robert &Version=2010-05-08
-- &AUTHPARAMS /division_abc/subdivision_xyz/ Robert AIDACKCEVSQ6C2EXAMPLE
-- arn:aws::123456789012:user/division_abc/subdivision_xyz/Robert
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UpdateUser'
updateUser :: ( MonadCatch m
              , MonadResource m
              , MonadError Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => Text -- ^ 'uuUserName'
           -> State UpdateUser a
           -> m UpdateUserResponse
updateUser p1 s =
    send $ (mkUpdateUser p1) &~ s

-- | Uploads a server certificate entity for the AWS account. The server
-- certificate entity includes a public key certificate, a private key, and an
-- optional certificate chain, which should all be PEM-encoded. For
-- information about the number of server certificates you can upload, see
-- Limitations on IAM Entities in the Using IAM guide. Because the body of the
-- public key certificate, private key, and the certificate chain can be
-- large, you should use POST rather than GET when calling
-- UploadServerCertificate. For information about setting up signatures and
-- authorization through the API, go to Signing AWS API Requests in the AWS
-- General Reference. For general information about using the Query API with
-- IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=UploadServerCertificate
-- &ServerCertificateName=ProdServerCert &Path=/company/servercerts/
-- &CertificateBody=-----BEGIN CERTIFICATE-----
-- MIICdzCCAeCgAwIBAgIGANc+Ha2wMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNVBAYT
-- AlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMQwwCgYDVQQLEwNBV1MxITAfBgNVBAMT
-- GEFXUyBMaW1pdGVkLUFzc3VyYW5jZSBDQTAeFw0wOTAyMDQxNzE5MjdaFw0xMDAy
-- MDQxNzE5MjdaMFIxCzAJBgNVBAYTAlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMRcw
-- FQYDVQQLEw5BV1MtRGV2ZWxvcGVyczEVMBMGA1UEAxMMNTdxNDl0c3ZwYjRtMIGf
-- MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCpB/vsOwmT/O0td1RqzKjttSBaPjbr
-- dqwNe9BrOyB08fw2+Ch5oonZYXfGUrT6mkYXH5fQot9HvASrzAKHO596FdJA6DmL
-- ywdWe1Oggk7zFSXO1Xv+3vPrJtaYxYo3eRIp7w80PMkiOv6M0XK8ubcTouODeJbf
-- suDqcLnLDxwsvwIDAQABo1cwVTAOBgNVHQ8BAf8EBAMCBaAwFgYDVR0lAQH/BAww
-- CgYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQULGNaBphBumaKbDRK
-- CAi0mH8B3mowDQYJKoZIhvcNAQEFBQADgYEAuKxhkXaCLGcqDuweKtO/AEw9ZePH
-- wr0XqsaIK2HZboqruebXEGsojK4Ks0WzwgrEynuHJwTn760xe39rSqXWIOGrOBaX
-- wFpWHVjTFMKk+tSDG1lssLHyYWWdFFU4AnejRGORJYNaRHgVTKjHphc5jEhHm0BX
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE----- &PrivateKey=-----BEGIN DSA
-- PRIVATE KEY-----
-- MIIBugIBTTKBgQD33xToSXPJ6hr37L3+KNi3/7DgywlBcvlFPPSHIw3ORuO/22mT
-- 8Cy5fT89WwNvZ3BPKWU6OZ38TQv3eWjNc/3U3+oqVNG2poX5nCPOtO1b96HYX2mR
-- 3FTdH6FRKbQEhpDzZ6tRrjTHjMX6sT3JRWkBd2c4bGu+HUHO1H7QvrCTeQIVTKMs
-- TCKCyrLiGhUWuUGNJUMU6y6zToGTHl84Tz7TPwDGDXuy/Dk5s4jTVr+xibROC/gS
-- Qrs4Dzz3T1ze6lvU8S1KT9UsOB5FUJNTTPCPey+Lo4mmK6b23XdTyCIT8e2fsm2j
-- jHHC1pIPiTkdLS3j6ZYjF8LY6TENFng+LDY/xwPOl7TJVoD3J/WXC2J9CEYq9o34
-- kq6WWn3CgYTuo54nXUgnoCb3xdG8COFrg+oTbIkHTSzs3w5o/GGgKK7TDF3UlJjq
-- vHNyJQ6kWBrQRR1Xp5KYQ4c/Dm5kef+62mH53HpcCELguWVcffuVQpmq3EWL9Zp9
-- jobTJQ2VHjb5IVxiO6HRSd27di3njyrzUuJCyHSDTqwLJmTThpd6OTIUTL3Tc4m2
-- 62TITdw53KWJEXAMPLE= -----END DSA PRIVATE KEY----- &Version=2010-05-08
-- &AUTHPARAMS ProdServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/ProdServerCert
-- 2010-05-08T01:02:03.004Z ASCACKCEVSQ6C2EXAMPLE 2012-05-08T01:02:03.004Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UploadServerCertificate'
uploadServerCertificate :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'usc2ServerCertificateName'
                        -> Text -- ^ 'usc2CertificateBody'
                        -> Text -- ^ 'usc2PrivateKey'
                        -> State UploadServerCertificate a
                        -> m UploadServerCertificateResponse
uploadServerCertificate p2 p3 p4 s =
    send $ (mkUploadServerCertificate p2 p3 p4) &~ s

-- | Uploads an X.509 signing certificate and associates it with the specified
-- user. Some AWS services use X.509 signing certificates to validate requests
-- that are signed with a corresponding private key. When you upload the
-- certificate, its default status is Active. If the UserName field is not
-- specified, the user name is determined implicitly based on the AWS access
-- key ID used to sign the request. Because this action works for access keys
-- under the AWS account, this API can be used to manage root credentials even
-- if the AWS account has no associated users. Because the body of a X.509
-- certificate can be large, you should use POST rather than GET when calling
-- UploadSigningCertificate. For information about setting up signatures and
-- authorization through the API, go to Signing AWS API Requests in the AWS
-- General Reference. For general information about using the Query API with
-- IAM, go to Making Query Requests in the Using IAMguide. POST / HTTP/1.1
-- Host: iam.amazonaws.com Content-Type: application/x-www-form-urlencoded
-- Action=UploadSigningCertificate &UserName=Bob &CertificateBody=-----BEGIN
-- CERTIFICATE-----
-- MIICdzCCAeCgAwIBAgIGANc+Ha2wMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNVBAYT
-- AlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMQwwCgYDVQQLEwNBV1MxITAfBgNVBAMT
-- GEFXUyBMaW1pdGVkLUFzc3VyYW5jZSBDQTAeFw0wOTAyMDQxNzE5MjdaFw0xMDAy
-- MDQxNzE5MjdaMFIxCzAJBgNVBAYTAlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMRcw
-- FQYDVQQLEw5BV1MtRGV2ZWxvcGVyczEVMBMGA1UEAxMMNTdxNDl0c3ZwYjRtMIGf
-- MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCpB/vsOwmT/O0td1RqzKjttSBaPjbr
-- dqwNe9BrOyB08fw2+Ch5oonZYXfGUrT6mkYXH5fQot9HvASrzAKHO596FdJA6DmL
-- ywdWe1Oggk7zFSXO1Xv+3vPrJtaYxYo3eRIp7w80PMkiOv6M0XK8ubcTouODeJbf
-- suDqcLnLDxwsvwIDAQABo1cwVTAOBgNVHQ8BAf8EBAMCBaAwFgYDVR0lAQH/BAww
-- CgYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQULGNaBphBumaKbDRK
-- CAi0mH8B3mowDQYJKoZIhvcNAQEFBQADgYEAuKxhkXaCLGcqDuweKtO/AEw9ZePH
-- wr0XqsaIK2HZboqruebXEGsojK4Ks0WzwgrEynuHJwTn760xe39rSqXWIOGrOBaX
-- wFpWHVjTFMKk+tSDG1lssLHyYWWdFFU4AnejRGORJYNaRHgVTKjHphc5jEhHm0BX
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE----- &Version=2010-05-08 &AUTHPARAMS
-- Bob TA7SMP42TDN5Z26OBPJE7EXAMPLE -----BEGIN CERTIFICATE-----
-- MIICdzCCAeCgAwIBAgIGANc+Ha2wMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNVBAYT
-- AlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMQwwCgYDVQQLEwNBV1MxITAfBgNVBAMT
-- GEFXUyBMaW1pdGVkLUFzc3VyYW5jZSBDQTAeFw0wOTAyMDQxNzE5MjdaFw0xMDAy
-- MDQxNzE5MjdaMFIxCzAJBgNVBAYTAlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMRcw
-- FQYDVQQLEw5BV1MtRGV2ZWxvcGVyczEVMBMGA1UEAxMMNTdxNDl0c3ZwYjRtMIGf
-- MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCpB/vsOwmT/O0td1RqzKjttSBaPjbr
-- dqwNe9BrOyB08fw2+Ch5oonZYXfGUrT6mkYXH5fQot9HvASrzAKHO596FdJA6DmL
-- ywdWe1Oggk7zFSXO1Xv+3vPrJtaYxYo3eRIp7w80PMkiOv6M0XK8ubcTouODeJbf
-- suDqcLnLDxwsvwIDAQABo1cwVTAOBgNVHQ8BAf8EBAMCBaAwFgYDVR0lAQH/BAww
-- CgYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQULGNaBphBumaKbDRK
-- CAi0mH8B3mowDQYJKoZIhvcNAQEFBQADgYEAuKxhkXaCLGcqDuweKtO/AEw9ZePH
-- wr0XqsaIK2HZboqruebXEGsojK4Ks0WzwgrEynuHJwTn760xe39rSqXWIOGrOBaX
-- wFpWHVjTFMKk+tSDG1lssLHyYWWdFFU4AnejRGORJYNaRHgVTKjHphc5jEhHm0BX
-- AEaHzTpmEXAMPLE= -----END CERTIFICATE----- Active
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
--
-- See: 'Network.AWS.IAM.V2010_05_08.UploadSigningCertificate'
uploadSigningCertificate :: ( MonadCatch m
                            , MonadResource m
                            , MonadError Error m
                            , MonadReader Env m
                            , AWSRequest a
                            )
                         => Text -- ^ 'usc3CertificateBody'
                         -> State UploadSigningCertificate a
                         -> m UploadSigningCertificateResponse
uploadSigningCertificate p2 s =
    send $ (mkUploadSigningCertificate p2) &~ s
