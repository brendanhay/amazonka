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

-- | AWS Identity and Access Management
--
-- AWS Identity and Access Management (IAM) is a web service that you can
-- use to manage users and user permissions under your AWS account. This
-- guide provides descriptions of IAM actions that you can call
-- programmatically. For general information about IAM, see
-- <http://aws.amazon.com/iam/ AWS Identity and Access Management (IAM)>.
-- For the user guide for IAM, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/ Using IAM>.
--
-- AWS provides SDKs that consist of libraries and sample code for various
-- programming languages and platforms (Java, Ruby, .NET, iOS, Android,
-- etc.). The SDKs provide a convenient way to create programmatic access
-- to IAM and AWS. For example, the SDKs take care of tasks such as
-- cryptographically signing requests (see below), managing errors, and
-- retrying requests automatically. For information about the AWS SDKs,
-- including how to download and install them, see the
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services> page.
--
-- We recommend that you use the AWS SDKs to make programmatic API calls to
-- IAM. However, you can also use the IAM Query API to make direct calls to
-- the IAM web service. To learn more about the IAM Query API, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests>
-- in the /Using IAM/ guide. IAM supports GET and POST requests for all
-- actions. That is, the API does not require you to use GET for some
-- actions and POST for others. However, GET requests are subject to the
-- limitation size of a URL. Therefore, for operations that require larger
-- sizes, use a POST request.
--
-- __Signing Requests__
--
-- Requests must be signed using an access key ID and a secret access key.
-- We strongly recommend that you do not use your AWS account access key ID
-- and secret access key for everyday work with IAM. You can use the access
-- key ID and secret access key for an IAM user or you can use the AWS
-- Security Token Service to generate temporary security credentials and
-- use those to sign requests.
--
-- To sign requests, we recommend that you use
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
-- If you have an existing application that uses Signature Version 2, you
-- do not have to update it to use Signature Version 4. However, some
-- operations now require Signature Version 4. The documentation for
-- operations that require version 4 indicate this requirement.
--
-- __Additional Resources__
--
-- For more information, see the following:
--
-- -   <http://docs.aws.amazon.com/general/latest/gr/aws-security-credentials.html AWS Security Credentials>.
--     This topic provides general information about the types of
--     credentials used for accessing AWS.
-- -   <http://docs.aws.amazon.com/IAM/latest/UserGuide/IAMBestPractices.html IAM Best Practices>.
--     This topic presents a list of suggestions for using the IAM service
--     to help secure your AWS resources.
-- -   <http://docs.aws.amazon.com/STS/latest/UsingSTS/ AWS Security Token Service>.
--     This guide describes how to create and use temporary security
--     credentials.
-- -   <http://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests>.
--     This set of topics walk you through the process of signing a request
--     using an access key ID and secret access key.
module Network.AWS.IAM
    ( module Export
    ) where

import           Network.AWS.IAM.AddClientIDToOpenIDConnectProvider      as Export
import           Network.AWS.IAM.AddRoleToInstanceProfile                as Export
import           Network.AWS.IAM.AddUserToGroup                          as Export
import           Network.AWS.IAM.AttachGroupPolicy                       as Export
import           Network.AWS.IAM.AttachRolePolicy                        as Export
import           Network.AWS.IAM.AttachUserPolicy                        as Export
import           Network.AWS.IAM.ChangePassword                          as Export
import           Network.AWS.IAM.CreateAccessKey                         as Export
import           Network.AWS.IAM.CreateAccountAlias                      as Export
import           Network.AWS.IAM.CreateGroup                             as Export
import           Network.AWS.IAM.CreateInstanceProfile                   as Export
import           Network.AWS.IAM.CreateLoginProfile                      as Export
import           Network.AWS.IAM.CreateOpenIDConnectProvider             as Export
import           Network.AWS.IAM.CreatePolicy                            as Export
import           Network.AWS.IAM.CreatePolicyVersion                     as Export
import           Network.AWS.IAM.CreateRole                              as Export
import           Network.AWS.IAM.CreateSAMLProvider                      as Export
import           Network.AWS.IAM.CreateUser                              as Export
import           Network.AWS.IAM.CreateVirtualMFADevice                  as Export
import           Network.AWS.IAM.DeactivateMFADevice                     as Export
import           Network.AWS.IAM.DeleteAccessKey                         as Export
import           Network.AWS.IAM.DeleteAccountAlias                      as Export
import           Network.AWS.IAM.DeleteAccountPasswordPolicy             as Export
import           Network.AWS.IAM.DeleteGroup                             as Export
import           Network.AWS.IAM.DeleteGroupPolicy                       as Export
import           Network.AWS.IAM.DeleteInstanceProfile                   as Export
import           Network.AWS.IAM.DeleteLoginProfile                      as Export
import           Network.AWS.IAM.DeleteOpenIDConnectProvider             as Export
import           Network.AWS.IAM.DeletePolicy                            as Export
import           Network.AWS.IAM.DeletePolicyVersion                     as Export
import           Network.AWS.IAM.DeleteRole                              as Export
import           Network.AWS.IAM.DeleteRolePolicy                        as Export
import           Network.AWS.IAM.DeleteSAMLProvider                      as Export
import           Network.AWS.IAM.DeleteServerCertificate                 as Export
import           Network.AWS.IAM.DeleteSigningCertificate                as Export
import           Network.AWS.IAM.DeleteUser                              as Export
import           Network.AWS.IAM.DeleteUserPolicy                        as Export
import           Network.AWS.IAM.DeleteVirtualMFADevice                  as Export
import           Network.AWS.IAM.DetachGroupPolicy                       as Export
import           Network.AWS.IAM.DetachRolePolicy                        as Export
import           Network.AWS.IAM.DetachUserPolicy                        as Export
import           Network.AWS.IAM.EnableMFADevice                         as Export
import           Network.AWS.IAM.GenerateCredentialReport                as Export
import           Network.AWS.IAM.GetAccessKeyLastUsed                    as Export
import           Network.AWS.IAM.GetAccountAuthorizationDetails          as Export
import           Network.AWS.IAM.GetAccountPasswordPolicy                as Export
import           Network.AWS.IAM.GetAccountSummary                       as Export
import           Network.AWS.IAM.GetCredentialReport                     as Export
import           Network.AWS.IAM.GetGroup                                as Export
import           Network.AWS.IAM.GetGroupPolicy                          as Export
import           Network.AWS.IAM.GetInstanceProfile                      as Export
import           Network.AWS.IAM.GetLoginProfile                         as Export
import           Network.AWS.IAM.GetOpenIDConnectProvider                as Export
import           Network.AWS.IAM.GetPolicy                               as Export
import           Network.AWS.IAM.GetPolicyVersion                        as Export
import           Network.AWS.IAM.GetRole                                 as Export
import           Network.AWS.IAM.GetRolePolicy                           as Export
import           Network.AWS.IAM.GetSAMLProvider                         as Export
import           Network.AWS.IAM.GetServerCertificate                    as Export
import           Network.AWS.IAM.GetUser                                 as Export
import           Network.AWS.IAM.GetUserPolicy                           as Export
import           Network.AWS.IAM.ListAccessKeys                          as Export
import           Network.AWS.IAM.ListAccountAliases                      as Export
import           Network.AWS.IAM.ListAttachedGroupPolicies               as Export
import           Network.AWS.IAM.ListAttachedRolePolicies                as Export
import           Network.AWS.IAM.ListAttachedUserPolicies                as Export
import           Network.AWS.IAM.ListEntitiesForPolicy                   as Export
import           Network.AWS.IAM.ListGroupPolicies                       as Export
import           Network.AWS.IAM.ListGroups                              as Export
import           Network.AWS.IAM.ListGroupsForUser                       as Export
import           Network.AWS.IAM.ListInstanceProfiles                    as Export
import           Network.AWS.IAM.ListInstanceProfilesForRole             as Export
import           Network.AWS.IAM.ListMFADevices                          as Export
import           Network.AWS.IAM.ListOpenIDConnectProviders              as Export
import           Network.AWS.IAM.ListPolicies                            as Export
import           Network.AWS.IAM.ListPolicyVersions                      as Export
import           Network.AWS.IAM.ListRolePolicies                        as Export
import           Network.AWS.IAM.ListRoles                               as Export
import           Network.AWS.IAM.ListSAMLProviders                       as Export
import           Network.AWS.IAM.ListServerCertificates                  as Export
import           Network.AWS.IAM.ListSigningCertificates                 as Export
import           Network.AWS.IAM.ListUserPolicies                        as Export
import           Network.AWS.IAM.ListUsers                               as Export
import           Network.AWS.IAM.ListVirtualMFADevices                   as Export
import           Network.AWS.IAM.PutGroupPolicy                          as Export
import           Network.AWS.IAM.PutRolePolicy                           as Export
import           Network.AWS.IAM.PutUserPolicy                           as Export
import           Network.AWS.IAM.RemoveClientIDFromOpenIDConnectProvider as Export
import           Network.AWS.IAM.RemoveRoleFromInstanceProfile           as Export
import           Network.AWS.IAM.RemoveUserFromGroup                     as Export
import           Network.AWS.IAM.ResyncMFADevice                         as Export
import           Network.AWS.IAM.SetDefaultPolicyVersion                 as Export
import           Network.AWS.IAM.Types                                   as Export
import           Network.AWS.IAM.UpdateAccessKey                         as Export
import           Network.AWS.IAM.UpdateAccountPasswordPolicy             as Export
import           Network.AWS.IAM.UpdateAssumeRolePolicy                  as Export
import           Network.AWS.IAM.UpdateGroup                             as Export
import           Network.AWS.IAM.UpdateLoginProfile                      as Export
import           Network.AWS.IAM.UpdateOpenIDConnectProviderThumbprint   as Export
import           Network.AWS.IAM.UpdateSAMLProvider                      as Export
import           Network.AWS.IAM.UpdateServerCertificate                 as Export
import           Network.AWS.IAM.UpdateSigningCertificate                as Export
import           Network.AWS.IAM.UpdateUser                              as Export
import           Network.AWS.IAM.UploadServerCertificate                 as Export
import           Network.AWS.IAM.UploadSigningCertificate                as Export
import           Network.AWS.IAM.Waiters                                 as Export
