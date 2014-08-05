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
module Network.AWS.IAM.V2010_05_08 (module Export) where

import Network.AWS.IAM.V2010_05_08.AddRoleToInstanceProfile as Export
import Network.AWS.IAM.V2010_05_08.AddUserToGroup as Export
import Network.AWS.IAM.V2010_05_08.ChangePassword as Export
import Network.AWS.IAM.V2010_05_08.CreateAccessKey as Export
import Network.AWS.IAM.V2010_05_08.CreateAccountAlias as Export
import Network.AWS.IAM.V2010_05_08.CreateGroup as Export
import Network.AWS.IAM.V2010_05_08.CreateInstanceProfile as Export
import Network.AWS.IAM.V2010_05_08.CreateLoginProfile as Export
import Network.AWS.IAM.V2010_05_08.CreateRole as Export
import Network.AWS.IAM.V2010_05_08.CreateSAMLProvider as Export
import Network.AWS.IAM.V2010_05_08.CreateUser as Export
import Network.AWS.IAM.V2010_05_08.CreateVirtualMFADevice as Export
import Network.AWS.IAM.V2010_05_08.DeactivateMFADevice as Export
import Network.AWS.IAM.V2010_05_08.DeleteAccessKey as Export
import Network.AWS.IAM.V2010_05_08.DeleteAccountAlias as Export
import Network.AWS.IAM.V2010_05_08.DeleteAccountPasswordPolicy as Export
import Network.AWS.IAM.V2010_05_08.DeleteGroup as Export
import Network.AWS.IAM.V2010_05_08.DeleteGroupPolicy as Export
import Network.AWS.IAM.V2010_05_08.DeleteInstanceProfile as Export
import Network.AWS.IAM.V2010_05_08.DeleteLoginProfile as Export
import Network.AWS.IAM.V2010_05_08.DeleteRole as Export
import Network.AWS.IAM.V2010_05_08.DeleteRolePolicy as Export
import Network.AWS.IAM.V2010_05_08.DeleteSAMLProvider as Export
import Network.AWS.IAM.V2010_05_08.DeleteServerCertificate as Export
import Network.AWS.IAM.V2010_05_08.DeleteSigningCertificate as Export
import Network.AWS.IAM.V2010_05_08.DeleteUser as Export
import Network.AWS.IAM.V2010_05_08.DeleteUserPolicy as Export
import Network.AWS.IAM.V2010_05_08.DeleteVirtualMFADevice as Export
import Network.AWS.IAM.V2010_05_08.EnableMFADevice as Export
import Network.AWS.IAM.V2010_05_08.GenerateCredentialReport as Export
import Network.AWS.IAM.V2010_05_08.GetAccountPasswordPolicy as Export
import Network.AWS.IAM.V2010_05_08.GetAccountSummary as Export
import Network.AWS.IAM.V2010_05_08.GetCredentialReport as Export
import Network.AWS.IAM.V2010_05_08.GetGroup as Export
import Network.AWS.IAM.V2010_05_08.GetGroupPolicy as Export
import Network.AWS.IAM.V2010_05_08.GetInstanceProfile as Export
import Network.AWS.IAM.V2010_05_08.GetLoginProfile as Export
import Network.AWS.IAM.V2010_05_08.GetRole as Export
import Network.AWS.IAM.V2010_05_08.GetRolePolicy as Export
import Network.AWS.IAM.V2010_05_08.GetSAMLProvider as Export
import Network.AWS.IAM.V2010_05_08.GetServerCertificate as Export
import Network.AWS.IAM.V2010_05_08.GetUser as Export
import Network.AWS.IAM.V2010_05_08.GetUserPolicy as Export
import Network.AWS.IAM.V2010_05_08.ListAccessKeys as Export
import Network.AWS.IAM.V2010_05_08.ListAccountAliases as Export
import Network.AWS.IAM.V2010_05_08.ListGroupPolicies as Export
import Network.AWS.IAM.V2010_05_08.ListGroups as Export
import Network.AWS.IAM.V2010_05_08.ListGroupsForUser as Export
import Network.AWS.IAM.V2010_05_08.ListInstanceProfiles as Export
import Network.AWS.IAM.V2010_05_08.ListInstanceProfilesForRole as Export
import Network.AWS.IAM.V2010_05_08.ListMFADevices as Export
import Network.AWS.IAM.V2010_05_08.ListRolePolicies as Export
import Network.AWS.IAM.V2010_05_08.ListRoles as Export
import Network.AWS.IAM.V2010_05_08.ListSAMLProviders as Export
import Network.AWS.IAM.V2010_05_08.ListServerCertificates as Export
import Network.AWS.IAM.V2010_05_08.ListSigningCertificates as Export
import Network.AWS.IAM.V2010_05_08.ListUserPolicies as Export
import Network.AWS.IAM.V2010_05_08.ListUsers as Export
import Network.AWS.IAM.V2010_05_08.ListVirtualMFADevices as Export
import Network.AWS.IAM.V2010_05_08.PutGroupPolicy as Export
import Network.AWS.IAM.V2010_05_08.PutRolePolicy as Export
import Network.AWS.IAM.V2010_05_08.PutUserPolicy as Export
import Network.AWS.IAM.V2010_05_08.RemoveRoleFromInstanceProfile as Export
import Network.AWS.IAM.V2010_05_08.RemoveUserFromGroup as Export
import Network.AWS.IAM.V2010_05_08.ResyncMFADevice as Export
import Network.AWS.IAM.V2010_05_08.Types as Export
import Network.AWS.IAM.V2010_05_08.UpdateAccessKey as Export
import Network.AWS.IAM.V2010_05_08.UpdateAccountPasswordPolicy as Export
import Network.AWS.IAM.V2010_05_08.UpdateAssumeRolePolicy as Export
import Network.AWS.IAM.V2010_05_08.UpdateGroup as Export
import Network.AWS.IAM.V2010_05_08.UpdateLoginProfile as Export
import Network.AWS.IAM.V2010_05_08.UpdateSAMLProvider as Export
import Network.AWS.IAM.V2010_05_08.UpdateServerCertificate as Export
import Network.AWS.IAM.V2010_05_08.UpdateSigningCertificate as Export
import Network.AWS.IAM.V2010_05_08.UpdateUser as Export
import Network.AWS.IAM.V2010_05_08.UploadServerCertificate as Export
import Network.AWS.IAM.V2010_05_08.UploadSigningCertificate as Export
