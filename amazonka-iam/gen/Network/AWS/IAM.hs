-- Module      : Network.AWS.IAM
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | AWS Identity and Access Management (IAM) enables you to securely control
-- access to AWS services and resources for your users. Using IAM, you can
-- create and manage AWS users and groups and use permissions to allow and deny
-- their access to AWS resources.
module Network.AWS.IAM
    ( module Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
    , module Network.AWS.IAM.AddRoleToInstanceProfile
    , module Network.AWS.IAM.AddUserToGroup
    , module Network.AWS.IAM.ChangePassword
    , module Network.AWS.IAM.CreateAccessKey
    , module Network.AWS.IAM.CreateAccountAlias
    , module Network.AWS.IAM.CreateGroup
    , module Network.AWS.IAM.CreateInstanceProfile
    , module Network.AWS.IAM.CreateLoginProfile
    , module Network.AWS.IAM.CreateOpenIDConnectProvider
    , module Network.AWS.IAM.CreateRole
    , module Network.AWS.IAM.CreateSAMLProvider
    , module Network.AWS.IAM.CreateUser
    , module Network.AWS.IAM.CreateVirtualMFADevice
    , module Network.AWS.IAM.DeactivateMFADevice
    , module Network.AWS.IAM.DeleteAccessKey
    , module Network.AWS.IAM.DeleteAccountAlias
    , module Network.AWS.IAM.DeleteAccountPasswordPolicy
    , module Network.AWS.IAM.DeleteGroup
    , module Network.AWS.IAM.DeleteGroupPolicy
    , module Network.AWS.IAM.DeleteInstanceProfile
    , module Network.AWS.IAM.DeleteLoginProfile
    , module Network.AWS.IAM.DeleteOpenIDConnectProvider
    , module Network.AWS.IAM.DeleteRole
    , module Network.AWS.IAM.DeleteRolePolicy
    , module Network.AWS.IAM.DeleteSAMLProvider
    , module Network.AWS.IAM.DeleteServerCertificate
    , module Network.AWS.IAM.DeleteSigningCertificate
    , module Network.AWS.IAM.DeleteUser
    , module Network.AWS.IAM.DeleteUserPolicy
    , module Network.AWS.IAM.DeleteVirtualMFADevice
    , module Network.AWS.IAM.EnableMFADevice
    , module Network.AWS.IAM.GenerateCredentialReport
    , module Network.AWS.IAM.GetAccountAuthorizationDetails
    , module Network.AWS.IAM.GetAccountPasswordPolicy
    , module Network.AWS.IAM.GetAccountSummary
    , module Network.AWS.IAM.GetCredentialReport
    , module Network.AWS.IAM.GetGroup
    , module Network.AWS.IAM.GetGroupPolicy
    , module Network.AWS.IAM.GetInstanceProfile
    , module Network.AWS.IAM.GetLoginProfile
    , module Network.AWS.IAM.GetOpenIDConnectProvider
    , module Network.AWS.IAM.GetRole
    , module Network.AWS.IAM.GetRolePolicy
    , module Network.AWS.IAM.GetSAMLProvider
    , module Network.AWS.IAM.GetServerCertificate
    , module Network.AWS.IAM.GetUser
    , module Network.AWS.IAM.GetUserPolicy
    , module Network.AWS.IAM.ListAccessKeys
    , module Network.AWS.IAM.ListAccountAliases
    , module Network.AWS.IAM.ListGroupPolicies
    , module Network.AWS.IAM.ListGroups
    , module Network.AWS.IAM.ListGroupsForUser
    , module Network.AWS.IAM.ListInstanceProfiles
    , module Network.AWS.IAM.ListInstanceProfilesForRole
    , module Network.AWS.IAM.ListMFADevices
    , module Network.AWS.IAM.ListOpenIDConnectProviders
    , module Network.AWS.IAM.ListRolePolicies
    , module Network.AWS.IAM.ListRoles
    , module Network.AWS.IAM.ListSAMLProviders
    , module Network.AWS.IAM.ListServerCertificates
    , module Network.AWS.IAM.ListSigningCertificates
    , module Network.AWS.IAM.ListUserPolicies
    , module Network.AWS.IAM.ListUsers
    , module Network.AWS.IAM.ListVirtualMFADevices
    , module Network.AWS.IAM.PutGroupPolicy
    , module Network.AWS.IAM.PutRolePolicy
    , module Network.AWS.IAM.PutUserPolicy
    , module Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
    , module Network.AWS.IAM.RemoveRoleFromInstanceProfile
    , module Network.AWS.IAM.RemoveUserFromGroup
    , module Network.AWS.IAM.ResyncMFADevice
    , module Network.AWS.IAM.Types
    , module Network.AWS.IAM.UpdateAccessKey
    , module Network.AWS.IAM.UpdateAccountPasswordPolicy
    , module Network.AWS.IAM.UpdateAssumeRolePolicy
    , module Network.AWS.IAM.UpdateGroup
    , module Network.AWS.IAM.UpdateLoginProfile
    , module Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
    , module Network.AWS.IAM.UpdateSAMLProvider
    , module Network.AWS.IAM.UpdateServerCertificate
    , module Network.AWS.IAM.UpdateSigningCertificate
    , module Network.AWS.IAM.UpdateUser
    , module Network.AWS.IAM.UploadServerCertificate
    , module Network.AWS.IAM.UploadSigningCertificate
    ) where

import Network.AWS.IAM.AddClientIDToOpenIDConnectProvider
import Network.AWS.IAM.AddRoleToInstanceProfile
import Network.AWS.IAM.AddUserToGroup
import Network.AWS.IAM.ChangePassword
import Network.AWS.IAM.CreateAccessKey
import Network.AWS.IAM.CreateAccountAlias
import Network.AWS.IAM.CreateGroup
import Network.AWS.IAM.CreateInstanceProfile
import Network.AWS.IAM.CreateLoginProfile
import Network.AWS.IAM.CreateOpenIDConnectProvider
import Network.AWS.IAM.CreateRole
import Network.AWS.IAM.CreateSAMLProvider
import Network.AWS.IAM.CreateUser
import Network.AWS.IAM.CreateVirtualMFADevice
import Network.AWS.IAM.DeactivateMFADevice
import Network.AWS.IAM.DeleteAccessKey
import Network.AWS.IAM.DeleteAccountAlias
import Network.AWS.IAM.DeleteAccountPasswordPolicy
import Network.AWS.IAM.DeleteGroup
import Network.AWS.IAM.DeleteGroupPolicy
import Network.AWS.IAM.DeleteInstanceProfile
import Network.AWS.IAM.DeleteLoginProfile
import Network.AWS.IAM.DeleteOpenIDConnectProvider
import Network.AWS.IAM.DeleteRole
import Network.AWS.IAM.DeleteRolePolicy
import Network.AWS.IAM.DeleteSAMLProvider
import Network.AWS.IAM.DeleteServerCertificate
import Network.AWS.IAM.DeleteSigningCertificate
import Network.AWS.IAM.DeleteUser
import Network.AWS.IAM.DeleteUserPolicy
import Network.AWS.IAM.DeleteVirtualMFADevice
import Network.AWS.IAM.EnableMFADevice
import Network.AWS.IAM.GenerateCredentialReport
import Network.AWS.IAM.GetAccountAuthorizationDetails
import Network.AWS.IAM.GetAccountPasswordPolicy
import Network.AWS.IAM.GetAccountSummary
import Network.AWS.IAM.GetCredentialReport
import Network.AWS.IAM.GetGroup
import Network.AWS.IAM.GetGroupPolicy
import Network.AWS.IAM.GetInstanceProfile
import Network.AWS.IAM.GetLoginProfile
import Network.AWS.IAM.GetOpenIDConnectProvider
import Network.AWS.IAM.GetRole
import Network.AWS.IAM.GetRolePolicy
import Network.AWS.IAM.GetSAMLProvider
import Network.AWS.IAM.GetServerCertificate
import Network.AWS.IAM.GetUser
import Network.AWS.IAM.GetUserPolicy
import Network.AWS.IAM.ListAccessKeys
import Network.AWS.IAM.ListAccountAliases
import Network.AWS.IAM.ListGroupPolicies
import Network.AWS.IAM.ListGroups
import Network.AWS.IAM.ListGroupsForUser
import Network.AWS.IAM.ListInstanceProfiles
import Network.AWS.IAM.ListInstanceProfilesForRole
import Network.AWS.IAM.ListMFADevices
import Network.AWS.IAM.ListOpenIDConnectProviders
import Network.AWS.IAM.ListRolePolicies
import Network.AWS.IAM.ListRoles
import Network.AWS.IAM.ListSAMLProviders
import Network.AWS.IAM.ListServerCertificates
import Network.AWS.IAM.ListSigningCertificates
import Network.AWS.IAM.ListUserPolicies
import Network.AWS.IAM.ListUsers
import Network.AWS.IAM.ListVirtualMFADevices
import Network.AWS.IAM.PutGroupPolicy
import Network.AWS.IAM.PutRolePolicy
import Network.AWS.IAM.PutUserPolicy
import Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider
import Network.AWS.IAM.RemoveRoleFromInstanceProfile
import Network.AWS.IAM.RemoveUserFromGroup
import Network.AWS.IAM.ResyncMFADevice
import Network.AWS.IAM.Types
import Network.AWS.IAM.UpdateAccessKey
import Network.AWS.IAM.UpdateAccountPasswordPolicy
import Network.AWS.IAM.UpdateAssumeRolePolicy
import Network.AWS.IAM.UpdateGroup
import Network.AWS.IAM.UpdateLoginProfile
import Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint
import Network.AWS.IAM.UpdateSAMLProvider
import Network.AWS.IAM.UpdateServerCertificate
import Network.AWS.IAM.UpdateSigningCertificate
import Network.AWS.IAM.UpdateUser
import Network.AWS.IAM.UploadServerCertificate
import Network.AWS.IAM.UploadSigningCertificate
