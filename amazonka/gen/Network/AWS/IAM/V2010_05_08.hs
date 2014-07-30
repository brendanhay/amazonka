{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08
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
module Network.AWS.IAM.V2010_05_08
    ( module Network.AWS.IAM.V2010_05_08.AddRoleToInstanceProfile
    , module Network.AWS.IAM.V2010_05_08.AddUserToGroup
    , module Network.AWS.IAM.V2010_05_08.ChangePassword
    , module Network.AWS.IAM.V2010_05_08.CreateAccessKey
    , module Network.AWS.IAM.V2010_05_08.CreateAccountAlias
    , module Network.AWS.IAM.V2010_05_08.CreateGroup
    , module Network.AWS.IAM.V2010_05_08.CreateInstanceProfile
    , module Network.AWS.IAM.V2010_05_08.CreateLoginProfile
    , module Network.AWS.IAM.V2010_05_08.CreateRole
    , module Network.AWS.IAM.V2010_05_08.CreateSAMLProvider
    , module Network.AWS.IAM.V2010_05_08.CreateUser
    , module Network.AWS.IAM.V2010_05_08.CreateVirtualMFADevice
    , module Network.AWS.IAM.V2010_05_08.DeactivateMFADevice
    , module Network.AWS.IAM.V2010_05_08.DeleteAccessKey
    , module Network.AWS.IAM.V2010_05_08.DeleteAccountAlias
    , module Network.AWS.IAM.V2010_05_08.DeleteAccountPasswordPolicy
    , module Network.AWS.IAM.V2010_05_08.DeleteGroup
    , module Network.AWS.IAM.V2010_05_08.DeleteGroupPolicy
    , module Network.AWS.IAM.V2010_05_08.DeleteInstanceProfile
    , module Network.AWS.IAM.V2010_05_08.DeleteLoginProfile
    , module Network.AWS.IAM.V2010_05_08.DeleteRole
    , module Network.AWS.IAM.V2010_05_08.DeleteRolePolicy
    , module Network.AWS.IAM.V2010_05_08.DeleteSAMLProvider
    , module Network.AWS.IAM.V2010_05_08.DeleteServerCertificate
    , module Network.AWS.IAM.V2010_05_08.DeleteSigningCertificate
    , module Network.AWS.IAM.V2010_05_08.DeleteUser
    , module Network.AWS.IAM.V2010_05_08.DeleteUserPolicy
    , module Network.AWS.IAM.V2010_05_08.DeleteVirtualMFADevice
    , module Network.AWS.IAM.V2010_05_08.EnableMFADevice
    , module Network.AWS.IAM.V2010_05_08.GenerateCredentialReport
    , module Network.AWS.IAM.V2010_05_08.GetAccountPasswordPolicy
    , module Network.AWS.IAM.V2010_05_08.GetAccountSummary
    , module Network.AWS.IAM.V2010_05_08.GetCredentialReport
    , module Network.AWS.IAM.V2010_05_08.GetGroup
    , module Network.AWS.IAM.V2010_05_08.GetGroupPolicy
    , module Network.AWS.IAM.V2010_05_08.GetInstanceProfile
    , module Network.AWS.IAM.V2010_05_08.GetLoginProfile
    , module Network.AWS.IAM.V2010_05_08.GetRole
    , module Network.AWS.IAM.V2010_05_08.GetRolePolicy
    , module Network.AWS.IAM.V2010_05_08.GetSAMLProvider
    , module Network.AWS.IAM.V2010_05_08.GetServerCertificate
    , module Network.AWS.IAM.V2010_05_08.GetUser
    , module Network.AWS.IAM.V2010_05_08.GetUserPolicy
    , module Network.AWS.IAM.V2010_05_08.Lenses
    , module Network.AWS.IAM.V2010_05_08.ListAccessKeys
    , module Network.AWS.IAM.V2010_05_08.ListAccountAliases
    , module Network.AWS.IAM.V2010_05_08.ListGroupPolicies
    , module Network.AWS.IAM.V2010_05_08.ListGroups
    , module Network.AWS.IAM.V2010_05_08.ListGroupsForUser
    , module Network.AWS.IAM.V2010_05_08.ListInstanceProfiles
    , module Network.AWS.IAM.V2010_05_08.ListInstanceProfilesForRole
    , module Network.AWS.IAM.V2010_05_08.ListMFADevices
    , module Network.AWS.IAM.V2010_05_08.ListRolePolicies
    , module Network.AWS.IAM.V2010_05_08.ListRoles
    , module Network.AWS.IAM.V2010_05_08.ListSAMLProviders
    , module Network.AWS.IAM.V2010_05_08.ListServerCertificates
    , module Network.AWS.IAM.V2010_05_08.ListSigningCertificates
    , module Network.AWS.IAM.V2010_05_08.ListUserPolicies
    , module Network.AWS.IAM.V2010_05_08.ListUsers
    , module Network.AWS.IAM.V2010_05_08.ListVirtualMFADevices
    , module Network.AWS.IAM.V2010_05_08.PutGroupPolicy
    , module Network.AWS.IAM.V2010_05_08.PutRolePolicy
    , module Network.AWS.IAM.V2010_05_08.PutUserPolicy
    , module Network.AWS.IAM.V2010_05_08.RemoveRoleFromInstanceProfile
    , module Network.AWS.IAM.V2010_05_08.RemoveUserFromGroup
    , module Network.AWS.IAM.V2010_05_08.ResyncMFADevice
    , module Network.AWS.IAM.V2010_05_08.Types
    , module Network.AWS.IAM.V2010_05_08.UpdateAccessKey
    , module Network.AWS.IAM.V2010_05_08.UpdateAccountPasswordPolicy
    , module Network.AWS.IAM.V2010_05_08.UpdateAssumeRolePolicy
    , module Network.AWS.IAM.V2010_05_08.UpdateGroup
    , module Network.AWS.IAM.V2010_05_08.UpdateLoginProfile
    , module Network.AWS.IAM.V2010_05_08.UpdateSAMLProvider
    , module Network.AWS.IAM.V2010_05_08.UpdateServerCertificate
    , module Network.AWS.IAM.V2010_05_08.UpdateSigningCertificate
    , module Network.AWS.IAM.V2010_05_08.UpdateUser
    , module Network.AWS.IAM.V2010_05_08.UploadServerCertificate
    , module Network.AWS.IAM.V2010_05_08.UploadSigningCertificate
    ) where

import Network.AWS.IAM.V2010_05_08.AddRoleToInstanceProfile
import Network.AWS.IAM.V2010_05_08.AddUserToGroup
import Network.AWS.IAM.V2010_05_08.ChangePassword
import Network.AWS.IAM.V2010_05_08.CreateAccessKey
import Network.AWS.IAM.V2010_05_08.CreateAccountAlias
import Network.AWS.IAM.V2010_05_08.CreateGroup
import Network.AWS.IAM.V2010_05_08.CreateInstanceProfile
import Network.AWS.IAM.V2010_05_08.CreateLoginProfile
import Network.AWS.IAM.V2010_05_08.CreateRole
import Network.AWS.IAM.V2010_05_08.CreateSAMLProvider
import Network.AWS.IAM.V2010_05_08.CreateUser
import Network.AWS.IAM.V2010_05_08.CreateVirtualMFADevice
import Network.AWS.IAM.V2010_05_08.DeactivateMFADevice
import Network.AWS.IAM.V2010_05_08.DeleteAccessKey
import Network.AWS.IAM.V2010_05_08.DeleteAccountAlias
import Network.AWS.IAM.V2010_05_08.DeleteAccountPasswordPolicy
import Network.AWS.IAM.V2010_05_08.DeleteGroup
import Network.AWS.IAM.V2010_05_08.DeleteGroupPolicy
import Network.AWS.IAM.V2010_05_08.DeleteInstanceProfile
import Network.AWS.IAM.V2010_05_08.DeleteLoginProfile
import Network.AWS.IAM.V2010_05_08.DeleteRole
import Network.AWS.IAM.V2010_05_08.DeleteRolePolicy
import Network.AWS.IAM.V2010_05_08.DeleteSAMLProvider
import Network.AWS.IAM.V2010_05_08.DeleteServerCertificate
import Network.AWS.IAM.V2010_05_08.DeleteSigningCertificate
import Network.AWS.IAM.V2010_05_08.DeleteUser
import Network.AWS.IAM.V2010_05_08.DeleteUserPolicy
import Network.AWS.IAM.V2010_05_08.DeleteVirtualMFADevice
import Network.AWS.IAM.V2010_05_08.EnableMFADevice
import Network.AWS.IAM.V2010_05_08.GenerateCredentialReport
import Network.AWS.IAM.V2010_05_08.GetAccountPasswordPolicy
import Network.AWS.IAM.V2010_05_08.GetAccountSummary
import Network.AWS.IAM.V2010_05_08.GetCredentialReport
import Network.AWS.IAM.V2010_05_08.GetGroup
import Network.AWS.IAM.V2010_05_08.GetGroupPolicy
import Network.AWS.IAM.V2010_05_08.GetInstanceProfile
import Network.AWS.IAM.V2010_05_08.GetLoginProfile
import Network.AWS.IAM.V2010_05_08.GetRole
import Network.AWS.IAM.V2010_05_08.GetRolePolicy
import Network.AWS.IAM.V2010_05_08.GetSAMLProvider
import Network.AWS.IAM.V2010_05_08.GetServerCertificate
import Network.AWS.IAM.V2010_05_08.GetUser
import Network.AWS.IAM.V2010_05_08.GetUserPolicy
import Network.AWS.IAM.V2010_05_08.Lenses
import Network.AWS.IAM.V2010_05_08.ListAccessKeys
import Network.AWS.IAM.V2010_05_08.ListAccountAliases
import Network.AWS.IAM.V2010_05_08.ListGroupPolicies
import Network.AWS.IAM.V2010_05_08.ListGroups
import Network.AWS.IAM.V2010_05_08.ListGroupsForUser
import Network.AWS.IAM.V2010_05_08.ListInstanceProfiles
import Network.AWS.IAM.V2010_05_08.ListInstanceProfilesForRole
import Network.AWS.IAM.V2010_05_08.ListMFADevices
import Network.AWS.IAM.V2010_05_08.ListRolePolicies
import Network.AWS.IAM.V2010_05_08.ListRoles
import Network.AWS.IAM.V2010_05_08.ListSAMLProviders
import Network.AWS.IAM.V2010_05_08.ListServerCertificates
import Network.AWS.IAM.V2010_05_08.ListSigningCertificates
import Network.AWS.IAM.V2010_05_08.ListUserPolicies
import Network.AWS.IAM.V2010_05_08.ListUsers
import Network.AWS.IAM.V2010_05_08.ListVirtualMFADevices
import Network.AWS.IAM.V2010_05_08.PutGroupPolicy
import Network.AWS.IAM.V2010_05_08.PutRolePolicy
import Network.AWS.IAM.V2010_05_08.PutUserPolicy
import Network.AWS.IAM.V2010_05_08.RemoveRoleFromInstanceProfile
import Network.AWS.IAM.V2010_05_08.RemoveUserFromGroup
import Network.AWS.IAM.V2010_05_08.ResyncMFADevice
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.IAM.V2010_05_08.UpdateAccessKey
import Network.AWS.IAM.V2010_05_08.UpdateAccountPasswordPolicy
import Network.AWS.IAM.V2010_05_08.UpdateAssumeRolePolicy
import Network.AWS.IAM.V2010_05_08.UpdateGroup
import Network.AWS.IAM.V2010_05_08.UpdateLoginProfile
import Network.AWS.IAM.V2010_05_08.UpdateSAMLProvider
import Network.AWS.IAM.V2010_05_08.UpdateServerCertificate
import Network.AWS.IAM.V2010_05_08.UpdateSigningCertificate
import Network.AWS.IAM.V2010_05_08.UpdateUser
import Network.AWS.IAM.V2010_05_08.UploadServerCertificate
import Network.AWS.IAM.V2010_05_08.UploadSigningCertificate
