{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoIdentityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CognitoIdentityProvider where

import Data.Proxy
import Network.AWS.CognitoIdentityProvider
import Test.AWS.CognitoIdentityProvider.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetUserAttributeVerificationCode $
--             newGetUserAttributeVerificationCode
--
--         , requestAdminDeleteUser $
--             newAdminDeleteUser
--
--         , requestCreateUserImportJob $
--             newCreateUserImportJob
--
--         , requestGetUser $
--             newGetUser
--
--         , requestSetUserPoolMfaConfig $
--             newSetUserPoolMfaConfig
--
--         , requestUpdateUserAttributes $
--             newUpdateUserAttributes
--
--         , requestDeleteUserAttributes $
--             newDeleteUserAttributes
--
--         , requestUpdateAuthEventFeedback $
--             newUpdateAuthEventFeedback
--
--         , requestListUserPools $
--             newListUserPools
--
--         , requestConfirmSignUp $
--             newConfirmSignUp
--
--         , requestAdminLinkProviderForUser $
--             newAdminLinkProviderForUser
--
--         , requestUpdateUserPool $
--             newUpdateUserPool
--
--         , requestDeleteUserPool $
--             newDeleteUserPool
--
--         , requestUpdateUserPoolDomain $
--             newUpdateUserPoolDomain
--
--         , requestDeleteUserPoolDomain $
--             newDeleteUserPoolDomain
--
--         , requestCreateUserPoolDomain $
--             newCreateUserPoolDomain
--
--         , requestChangePassword $
--             newChangePassword
--
--         , requestRespondToAuthChallenge $
--             newRespondToAuthChallenge
--
--         , requestListDevices $
--             newListDevices
--
--         , requestAdminGetDevice $
--             newAdminGetDevice
--
--         , requestCreateUserPool $
--             newCreateUserPool
--
--         , requestAdminRemoveUserFromGroup $
--             newAdminRemoveUserFromGroup
--
--         , requestSetRiskConfiguration $
--             newSetRiskConfiguration
--
--         , requestListGroups $
--             newListGroups
--
--         , requestUpdateIdentityProvider $
--             newUpdateIdentityProvider
--
--         , requestListIdentityProviders $
--             newListIdentityProviders
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestDeleteIdentityProvider $
--             newDeleteIdentityProvider
--
--         , requestAdminListDevices $
--             newAdminListDevices
--
--         , requestAdminConfirmSignUp $
--             newAdminConfirmSignUp
--
--         , requestSetUICustomization $
--             newSetUICustomization
--
--         , requestAdminListUserAuthEvents $
--             newAdminListUserAuthEvents
--
--         , requestAdminAddUserToGroup $
--             newAdminAddUserToGroup
--
--         , requestVerifySoftwareToken $
--             newVerifySoftwareToken
--
--         , requestStopUserImportJob $
--             newStopUserImportJob
--
--         , requestCreateIdentityProvider $
--             newCreateIdentityProvider
--
--         , requestInitiateAuth $
--             newInitiateAuth
--
--         , requestAdminSetUserPassword $
--             newAdminSetUserPassword
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestAdminListGroupsForUser $
--             newAdminListGroupsForUser
--
--         , requestAdminUpdateAuthEventFeedback $
--             newAdminUpdateAuthEventFeedback
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestStartUserImportJob $
--             newStartUserImportJob
--
--         , requestDescribeUserPoolDomain $
--             newDescribeUserPoolDomain
--
--         , requestListUsersInGroup $
--             newListUsersInGroup
--
--         , requestAdminUserGlobalSignOut $
--             newAdminUserGlobalSignOut
--
--         , requestDescribeUserPool $
--             newDescribeUserPool
--
--         , requestAdminGetUser $
--             newAdminGetUser
--
--         , requestGetSigningCertificate $
--             newGetSigningCertificate
--
--         , requestTagResource $
--             newTagResource
--
--         , requestAssociateSoftwareToken $
--             newAssociateSoftwareToken
--
--         , requestForgotPassword $
--             newForgotPassword
--
--         , requestUpdateDeviceStatus $
--             newUpdateDeviceStatus
--
--         , requestDeleteUserPoolClient $
--             newDeleteUserPoolClient
--
--         , requestUpdateUserPoolClient $
--             newUpdateUserPoolClient
--
--         , requestForgetDevice $
--             newForgetDevice
--
--         , requestAdminDisableUser $
--             newAdminDisableUser
--
--         , requestAdminCreateUser $
--             newAdminCreateUser
--
--         , requestAdminUpdateDeviceStatus $
--             newAdminUpdateDeviceStatus
--
--         , requestAdminForgetDevice $
--             newAdminForgetDevice
--
--         , requestDescribeIdentityProvider $
--             newDescribeIdentityProvider
--
--         , requestListUserImportJobs $
--             newListUserImportJobs
--
--         , requestGetUICustomization $
--             newGetUICustomization
--
--         , requestGetCSVHeader $
--             newGetCSVHeader
--
--         , requestAdminEnableUser $
--             newAdminEnableUser
--
--         , requestAdminInitiateAuth $
--             newAdminInitiateAuth
--
--         , requestAdminResetUserPassword $
--             newAdminResetUserPassword
--
--         , requestGetIdentityProviderByIdentifier $
--             newGetIdentityProviderByIdentifier
--
--         , requestAdminSetUserSettings $
--             newAdminSetUserSettings
--
--         , requestDescribeUserPoolClient $
--             newDescribeUserPoolClient
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestResendConfirmationCode $
--             newResendConfirmationCode
--
--         , requestAdminRespondToAuthChallenge $
--             newAdminRespondToAuthChallenge
--
--         , requestSignUp $
--             newSignUp
--
--         , requestDescribeUserImportJob $
--             newDescribeUserImportJob
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestUpdateResourceServer $
--             newUpdateResourceServer
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestSetUserSettings $
--             newSetUserSettings
--
--         , requestListResourceServers $
--             newListResourceServers
--
--         , requestDeleteResourceServer $
--             newDeleteResourceServer
--
--         , requestDescribeRiskConfiguration $
--             newDescribeRiskConfiguration
--
--         , requestGlobalSignOut $
--             newGlobalSignOut
--
--         , requestGetUserPoolMfaConfig $
--             newGetUserPoolMfaConfig
--
--         , requestCreateResourceServer $
--             newCreateResourceServer
--
--         , requestAdminUpdateUserAttributes $
--             newAdminUpdateUserAttributes
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestAdminDeleteUserAttributes $
--             newAdminDeleteUserAttributes
--
--         , requestListUsers $
--             newListUsers
--
--         , requestAdminDisableProviderForUser $
--             newAdminDisableProviderForUser
--
--         , requestCreateUserPoolClient $
--             newCreateUserPoolClient
--
--         , requestListUserPoolClients $
--             newListUserPoolClients
--
--         , requestAddCustomAttributes $
--             newAddCustomAttributes
--
--         , requestAdminSetUserMFAPreference $
--             newAdminSetUserMFAPreference
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestConfirmForgotPassword $
--             newConfirmForgotPassword
--
--         , requestSetUserMFAPreference $
--             newSetUserMFAPreference
--
--         , requestVerifyUserAttribute $
--             newVerifyUserAttribute
--
--         , requestConfirmDevice $
--             newConfirmDevice
--
--         , requestDescribeResourceServer $
--             newDescribeResourceServer
--
--           ]

--     , testGroup "response"
--         [ responseGetUserAttributeVerificationCode $
--             newGetUserAttributeVerificationCodeResponse
--
--         , responseAdminDeleteUser $
--             newAdminDeleteUserResponse
--
--         , responseCreateUserImportJob $
--             newCreateUserImportJobResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseSetUserPoolMfaConfig $
--             newSetUserPoolMfaConfigResponse
--
--         , responseUpdateUserAttributes $
--             newUpdateUserAttributesResponse
--
--         , responseDeleteUserAttributes $
--             newDeleteUserAttributesResponse
--
--         , responseUpdateAuthEventFeedback $
--             newUpdateAuthEventFeedbackResponse
--
--         , responseListUserPools $
--             newListUserPoolsResponse
--
--         , responseConfirmSignUp $
--             newConfirmSignUpResponse
--
--         , responseAdminLinkProviderForUser $
--             newAdminLinkProviderForUserResponse
--
--         , responseUpdateUserPool $
--             newUpdateUserPoolResponse
--
--         , responseDeleteUserPool $
--             newDeleteUserPoolResponse
--
--         , responseUpdateUserPoolDomain $
--             newUpdateUserPoolDomainResponse
--
--         , responseDeleteUserPoolDomain $
--             newDeleteUserPoolDomainResponse
--
--         , responseCreateUserPoolDomain $
--             newCreateUserPoolDomainResponse
--
--         , responseChangePassword $
--             newChangePasswordResponse
--
--         , responseRespondToAuthChallenge $
--             newRespondToAuthChallengeResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseAdminGetDevice $
--             newAdminGetDeviceResponse
--
--         , responseCreateUserPool $
--             newCreateUserPoolResponse
--
--         , responseAdminRemoveUserFromGroup $
--             newAdminRemoveUserFromGroupResponse
--
--         , responseSetRiskConfiguration $
--             newSetRiskConfigurationResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseUpdateIdentityProvider $
--             newUpdateIdentityProviderResponse
--
--         , responseListIdentityProviders $
--             newListIdentityProvidersResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseDeleteIdentityProvider $
--             newDeleteIdentityProviderResponse
--
--         , responseAdminListDevices $
--             newAdminListDevicesResponse
--
--         , responseAdminConfirmSignUp $
--             newAdminConfirmSignUpResponse
--
--         , responseSetUICustomization $
--             newSetUICustomizationResponse
--
--         , responseAdminListUserAuthEvents $
--             newAdminListUserAuthEventsResponse
--
--         , responseAdminAddUserToGroup $
--             newAdminAddUserToGroupResponse
--
--         , responseVerifySoftwareToken $
--             newVerifySoftwareTokenResponse
--
--         , responseStopUserImportJob $
--             newStopUserImportJobResponse
--
--         , responseCreateIdentityProvider $
--             newCreateIdentityProviderResponse
--
--         , responseInitiateAuth $
--             newInitiateAuthResponse
--
--         , responseAdminSetUserPassword $
--             newAdminSetUserPasswordResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseAdminListGroupsForUser $
--             newAdminListGroupsForUserResponse
--
--         , responseAdminUpdateAuthEventFeedback $
--             newAdminUpdateAuthEventFeedbackResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseStartUserImportJob $
--             newStartUserImportJobResponse
--
--         , responseDescribeUserPoolDomain $
--             newDescribeUserPoolDomainResponse
--
--         , responseListUsersInGroup $
--             newListUsersInGroupResponse
--
--         , responseAdminUserGlobalSignOut $
--             newAdminUserGlobalSignOutResponse
--
--         , responseDescribeUserPool $
--             newDescribeUserPoolResponse
--
--         , responseAdminGetUser $
--             newAdminGetUserResponse
--
--         , responseGetSigningCertificate $
--             newGetSigningCertificateResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseAssociateSoftwareToken $
--             newAssociateSoftwareTokenResponse
--
--         , responseForgotPassword $
--             newForgotPasswordResponse
--
--         , responseUpdateDeviceStatus $
--             newUpdateDeviceStatusResponse
--
--         , responseDeleteUserPoolClient $
--             newDeleteUserPoolClientResponse
--
--         , responseUpdateUserPoolClient $
--             newUpdateUserPoolClientResponse
--
--         , responseForgetDevice $
--             newForgetDeviceResponse
--
--         , responseAdminDisableUser $
--             newAdminDisableUserResponse
--
--         , responseAdminCreateUser $
--             newAdminCreateUserResponse
--
--         , responseAdminUpdateDeviceStatus $
--             newAdminUpdateDeviceStatusResponse
--
--         , responseAdminForgetDevice $
--             newAdminForgetDeviceResponse
--
--         , responseDescribeIdentityProvider $
--             newDescribeIdentityProviderResponse
--
--         , responseListUserImportJobs $
--             newListUserImportJobsResponse
--
--         , responseGetUICustomization $
--             newGetUICustomizationResponse
--
--         , responseGetCSVHeader $
--             newGetCSVHeaderResponse
--
--         , responseAdminEnableUser $
--             newAdminEnableUserResponse
--
--         , responseAdminInitiateAuth $
--             newAdminInitiateAuthResponse
--
--         , responseAdminResetUserPassword $
--             newAdminResetUserPasswordResponse
--
--         , responseGetIdentityProviderByIdentifier $
--             newGetIdentityProviderByIdentifierResponse
--
--         , responseAdminSetUserSettings $
--             newAdminSetUserSettingsResponse
--
--         , responseDescribeUserPoolClient $
--             newDescribeUserPoolClientResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseResendConfirmationCode $
--             newResendConfirmationCodeResponse
--
--         , responseAdminRespondToAuthChallenge $
--             newAdminRespondToAuthChallengeResponse
--
--         , responseSignUp $
--             newSignUpResponse
--
--         , responseDescribeUserImportJob $
--             newDescribeUserImportJobResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseUpdateResourceServer $
--             newUpdateResourceServerResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseSetUserSettings $
--             newSetUserSettingsResponse
--
--         , responseListResourceServers $
--             newListResourceServersResponse
--
--         , responseDeleteResourceServer $
--             newDeleteResourceServerResponse
--
--         , responseDescribeRiskConfiguration $
--             newDescribeRiskConfigurationResponse
--
--         , responseGlobalSignOut $
--             newGlobalSignOutResponse
--
--         , responseGetUserPoolMfaConfig $
--             newGetUserPoolMfaConfigResponse
--
--         , responseCreateResourceServer $
--             newCreateResourceServerResponse
--
--         , responseAdminUpdateUserAttributes $
--             newAdminUpdateUserAttributesResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseAdminDeleteUserAttributes $
--             newAdminDeleteUserAttributesResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseAdminDisableProviderForUser $
--             newAdminDisableProviderForUserResponse
--
--         , responseCreateUserPoolClient $
--             newCreateUserPoolClientResponse
--
--         , responseListUserPoolClients $
--             newListUserPoolClientsResponse
--
--         , responseAddCustomAttributes $
--             newAddCustomAttributesResponse
--
--         , responseAdminSetUserMFAPreference $
--             newAdminSetUserMFAPreferenceResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseConfirmForgotPassword $
--             newConfirmForgotPasswordResponse
--
--         , responseSetUserMFAPreference $
--             newSetUserMFAPreferenceResponse
--
--         , responseVerifyUserAttribute $
--             newVerifyUserAttributeResponse
--
--         , responseConfirmDevice $
--             newConfirmDeviceResponse
--
--         , responseDescribeResourceServer $
--             newDescribeResourceServerResponse
--
--           ]
--     ]

-- Requests

requestGetUserAttributeVerificationCode :: GetUserAttributeVerificationCode -> TestTree
requestGetUserAttributeVerificationCode =
  req
    "GetUserAttributeVerificationCode"
    "fixture/GetUserAttributeVerificationCode.yaml"

requestAdminDeleteUser :: AdminDeleteUser -> TestTree
requestAdminDeleteUser =
  req
    "AdminDeleteUser"
    "fixture/AdminDeleteUser.yaml"

requestCreateUserImportJob :: CreateUserImportJob -> TestTree
requestCreateUserImportJob =
  req
    "CreateUserImportJob"
    "fixture/CreateUserImportJob.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser =
  req
    "GetUser"
    "fixture/GetUser.yaml"

requestSetUserPoolMfaConfig :: SetUserPoolMfaConfig -> TestTree
requestSetUserPoolMfaConfig =
  req
    "SetUserPoolMfaConfig"
    "fixture/SetUserPoolMfaConfig.yaml"

requestUpdateUserAttributes :: UpdateUserAttributes -> TestTree
requestUpdateUserAttributes =
  req
    "UpdateUserAttributes"
    "fixture/UpdateUserAttributes.yaml"

requestDeleteUserAttributes :: DeleteUserAttributes -> TestTree
requestDeleteUserAttributes =
  req
    "DeleteUserAttributes"
    "fixture/DeleteUserAttributes.yaml"

requestUpdateAuthEventFeedback :: UpdateAuthEventFeedback -> TestTree
requestUpdateAuthEventFeedback =
  req
    "UpdateAuthEventFeedback"
    "fixture/UpdateAuthEventFeedback.yaml"

requestListUserPools :: ListUserPools -> TestTree
requestListUserPools =
  req
    "ListUserPools"
    "fixture/ListUserPools.yaml"

requestConfirmSignUp :: ConfirmSignUp -> TestTree
requestConfirmSignUp =
  req
    "ConfirmSignUp"
    "fixture/ConfirmSignUp.yaml"

requestAdminLinkProviderForUser :: AdminLinkProviderForUser -> TestTree
requestAdminLinkProviderForUser =
  req
    "AdminLinkProviderForUser"
    "fixture/AdminLinkProviderForUser.yaml"

requestUpdateUserPool :: UpdateUserPool -> TestTree
requestUpdateUserPool =
  req
    "UpdateUserPool"
    "fixture/UpdateUserPool.yaml"

requestDeleteUserPool :: DeleteUserPool -> TestTree
requestDeleteUserPool =
  req
    "DeleteUserPool"
    "fixture/DeleteUserPool.yaml"

requestUpdateUserPoolDomain :: UpdateUserPoolDomain -> TestTree
requestUpdateUserPoolDomain =
  req
    "UpdateUserPoolDomain"
    "fixture/UpdateUserPoolDomain.yaml"

requestDeleteUserPoolDomain :: DeleteUserPoolDomain -> TestTree
requestDeleteUserPoolDomain =
  req
    "DeleteUserPoolDomain"
    "fixture/DeleteUserPoolDomain.yaml"

requestCreateUserPoolDomain :: CreateUserPoolDomain -> TestTree
requestCreateUserPoolDomain =
  req
    "CreateUserPoolDomain"
    "fixture/CreateUserPoolDomain.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword =
  req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

requestRespondToAuthChallenge :: RespondToAuthChallenge -> TestTree
requestRespondToAuthChallenge =
  req
    "RespondToAuthChallenge"
    "fixture/RespondToAuthChallenge.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestAdminGetDevice :: AdminGetDevice -> TestTree
requestAdminGetDevice =
  req
    "AdminGetDevice"
    "fixture/AdminGetDevice.yaml"

requestCreateUserPool :: CreateUserPool -> TestTree
requestCreateUserPool =
  req
    "CreateUserPool"
    "fixture/CreateUserPool.yaml"

requestAdminRemoveUserFromGroup :: AdminRemoveUserFromGroup -> TestTree
requestAdminRemoveUserFromGroup =
  req
    "AdminRemoveUserFromGroup"
    "fixture/AdminRemoveUserFromGroup.yaml"

requestSetRiskConfiguration :: SetRiskConfiguration -> TestTree
requestSetRiskConfiguration =
  req
    "SetRiskConfiguration"
    "fixture/SetRiskConfiguration.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestUpdateIdentityProvider :: UpdateIdentityProvider -> TestTree
requestUpdateIdentityProvider =
  req
    "UpdateIdentityProvider"
    "fixture/UpdateIdentityProvider.yaml"

requestListIdentityProviders :: ListIdentityProviders -> TestTree
requestListIdentityProviders =
  req
    "ListIdentityProviders"
    "fixture/ListIdentityProviders.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestDeleteIdentityProvider :: DeleteIdentityProvider -> TestTree
requestDeleteIdentityProvider =
  req
    "DeleteIdentityProvider"
    "fixture/DeleteIdentityProvider.yaml"

requestAdminListDevices :: AdminListDevices -> TestTree
requestAdminListDevices =
  req
    "AdminListDevices"
    "fixture/AdminListDevices.yaml"

requestAdminConfirmSignUp :: AdminConfirmSignUp -> TestTree
requestAdminConfirmSignUp =
  req
    "AdminConfirmSignUp"
    "fixture/AdminConfirmSignUp.yaml"

requestSetUICustomization :: SetUICustomization -> TestTree
requestSetUICustomization =
  req
    "SetUICustomization"
    "fixture/SetUICustomization.yaml"

requestAdminListUserAuthEvents :: AdminListUserAuthEvents -> TestTree
requestAdminListUserAuthEvents =
  req
    "AdminListUserAuthEvents"
    "fixture/AdminListUserAuthEvents.yaml"

requestAdminAddUserToGroup :: AdminAddUserToGroup -> TestTree
requestAdminAddUserToGroup =
  req
    "AdminAddUserToGroup"
    "fixture/AdminAddUserToGroup.yaml"

requestVerifySoftwareToken :: VerifySoftwareToken -> TestTree
requestVerifySoftwareToken =
  req
    "VerifySoftwareToken"
    "fixture/VerifySoftwareToken.yaml"

requestStopUserImportJob :: StopUserImportJob -> TestTree
requestStopUserImportJob =
  req
    "StopUserImportJob"
    "fixture/StopUserImportJob.yaml"

requestCreateIdentityProvider :: CreateIdentityProvider -> TestTree
requestCreateIdentityProvider =
  req
    "CreateIdentityProvider"
    "fixture/CreateIdentityProvider.yaml"

requestInitiateAuth :: InitiateAuth -> TestTree
requestInitiateAuth =
  req
    "InitiateAuth"
    "fixture/InitiateAuth.yaml"

requestAdminSetUserPassword :: AdminSetUserPassword -> TestTree
requestAdminSetUserPassword =
  req
    "AdminSetUserPassword"
    "fixture/AdminSetUserPassword.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestAdminListGroupsForUser :: AdminListGroupsForUser -> TestTree
requestAdminListGroupsForUser =
  req
    "AdminListGroupsForUser"
    "fixture/AdminListGroupsForUser.yaml"

requestAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedback -> TestTree
requestAdminUpdateAuthEventFeedback =
  req
    "AdminUpdateAuthEventFeedback"
    "fixture/AdminUpdateAuthEventFeedback.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestStartUserImportJob :: StartUserImportJob -> TestTree
requestStartUserImportJob =
  req
    "StartUserImportJob"
    "fixture/StartUserImportJob.yaml"

requestDescribeUserPoolDomain :: DescribeUserPoolDomain -> TestTree
requestDescribeUserPoolDomain =
  req
    "DescribeUserPoolDomain"
    "fixture/DescribeUserPoolDomain.yaml"

requestListUsersInGroup :: ListUsersInGroup -> TestTree
requestListUsersInGroup =
  req
    "ListUsersInGroup"
    "fixture/ListUsersInGroup.yaml"

requestAdminUserGlobalSignOut :: AdminUserGlobalSignOut -> TestTree
requestAdminUserGlobalSignOut =
  req
    "AdminUserGlobalSignOut"
    "fixture/AdminUserGlobalSignOut.yaml"

requestDescribeUserPool :: DescribeUserPool -> TestTree
requestDescribeUserPool =
  req
    "DescribeUserPool"
    "fixture/DescribeUserPool.yaml"

requestAdminGetUser :: AdminGetUser -> TestTree
requestAdminGetUser =
  req
    "AdminGetUser"
    "fixture/AdminGetUser.yaml"

requestGetSigningCertificate :: GetSigningCertificate -> TestTree
requestGetSigningCertificate =
  req
    "GetSigningCertificate"
    "fixture/GetSigningCertificate.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestAssociateSoftwareToken :: AssociateSoftwareToken -> TestTree
requestAssociateSoftwareToken =
  req
    "AssociateSoftwareToken"
    "fixture/AssociateSoftwareToken.yaml"

requestForgotPassword :: ForgotPassword -> TestTree
requestForgotPassword =
  req
    "ForgotPassword"
    "fixture/ForgotPassword.yaml"

requestUpdateDeviceStatus :: UpdateDeviceStatus -> TestTree
requestUpdateDeviceStatus =
  req
    "UpdateDeviceStatus"
    "fixture/UpdateDeviceStatus.yaml"

requestDeleteUserPoolClient :: DeleteUserPoolClient -> TestTree
requestDeleteUserPoolClient =
  req
    "DeleteUserPoolClient"
    "fixture/DeleteUserPoolClient.yaml"

requestUpdateUserPoolClient :: UpdateUserPoolClient -> TestTree
requestUpdateUserPoolClient =
  req
    "UpdateUserPoolClient"
    "fixture/UpdateUserPoolClient.yaml"

requestForgetDevice :: ForgetDevice -> TestTree
requestForgetDevice =
  req
    "ForgetDevice"
    "fixture/ForgetDevice.yaml"

requestAdminDisableUser :: AdminDisableUser -> TestTree
requestAdminDisableUser =
  req
    "AdminDisableUser"
    "fixture/AdminDisableUser.yaml"

requestAdminCreateUser :: AdminCreateUser -> TestTree
requestAdminCreateUser =
  req
    "AdminCreateUser"
    "fixture/AdminCreateUser.yaml"

requestAdminUpdateDeviceStatus :: AdminUpdateDeviceStatus -> TestTree
requestAdminUpdateDeviceStatus =
  req
    "AdminUpdateDeviceStatus"
    "fixture/AdminUpdateDeviceStatus.yaml"

requestAdminForgetDevice :: AdminForgetDevice -> TestTree
requestAdminForgetDevice =
  req
    "AdminForgetDevice"
    "fixture/AdminForgetDevice.yaml"

requestDescribeIdentityProvider :: DescribeIdentityProvider -> TestTree
requestDescribeIdentityProvider =
  req
    "DescribeIdentityProvider"
    "fixture/DescribeIdentityProvider.yaml"

requestListUserImportJobs :: ListUserImportJobs -> TestTree
requestListUserImportJobs =
  req
    "ListUserImportJobs"
    "fixture/ListUserImportJobs.yaml"

requestGetUICustomization :: GetUICustomization -> TestTree
requestGetUICustomization =
  req
    "GetUICustomization"
    "fixture/GetUICustomization.yaml"

requestGetCSVHeader :: GetCSVHeader -> TestTree
requestGetCSVHeader =
  req
    "GetCSVHeader"
    "fixture/GetCSVHeader.yaml"

requestAdminEnableUser :: AdminEnableUser -> TestTree
requestAdminEnableUser =
  req
    "AdminEnableUser"
    "fixture/AdminEnableUser.yaml"

requestAdminInitiateAuth :: AdminInitiateAuth -> TestTree
requestAdminInitiateAuth =
  req
    "AdminInitiateAuth"
    "fixture/AdminInitiateAuth.yaml"

requestAdminResetUserPassword :: AdminResetUserPassword -> TestTree
requestAdminResetUserPassword =
  req
    "AdminResetUserPassword"
    "fixture/AdminResetUserPassword.yaml"

requestGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifier -> TestTree
requestGetIdentityProviderByIdentifier =
  req
    "GetIdentityProviderByIdentifier"
    "fixture/GetIdentityProviderByIdentifier.yaml"

requestAdminSetUserSettings :: AdminSetUserSettings -> TestTree
requestAdminSetUserSettings =
  req
    "AdminSetUserSettings"
    "fixture/AdminSetUserSettings.yaml"

requestDescribeUserPoolClient :: DescribeUserPoolClient -> TestTree
requestDescribeUserPoolClient =
  req
    "DescribeUserPoolClient"
    "fixture/DescribeUserPoolClient.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestResendConfirmationCode :: ResendConfirmationCode -> TestTree
requestResendConfirmationCode =
  req
    "ResendConfirmationCode"
    "fixture/ResendConfirmationCode.yaml"

requestAdminRespondToAuthChallenge :: AdminRespondToAuthChallenge -> TestTree
requestAdminRespondToAuthChallenge =
  req
    "AdminRespondToAuthChallenge"
    "fixture/AdminRespondToAuthChallenge.yaml"

requestSignUp :: SignUp -> TestTree
requestSignUp =
  req
    "SignUp"
    "fixture/SignUp.yaml"

requestDescribeUserImportJob :: DescribeUserImportJob -> TestTree
requestDescribeUserImportJob =
  req
    "DescribeUserImportJob"
    "fixture/DescribeUserImportJob.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestUpdateResourceServer :: UpdateResourceServer -> TestTree
requestUpdateResourceServer =
  req
    "UpdateResourceServer"
    "fixture/UpdateResourceServer.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestSetUserSettings :: SetUserSettings -> TestTree
requestSetUserSettings =
  req
    "SetUserSettings"
    "fixture/SetUserSettings.yaml"

requestListResourceServers :: ListResourceServers -> TestTree
requestListResourceServers =
  req
    "ListResourceServers"
    "fixture/ListResourceServers.yaml"

requestDeleteResourceServer :: DeleteResourceServer -> TestTree
requestDeleteResourceServer =
  req
    "DeleteResourceServer"
    "fixture/DeleteResourceServer.yaml"

requestDescribeRiskConfiguration :: DescribeRiskConfiguration -> TestTree
requestDescribeRiskConfiguration =
  req
    "DescribeRiskConfiguration"
    "fixture/DescribeRiskConfiguration.yaml"

requestGlobalSignOut :: GlobalSignOut -> TestTree
requestGlobalSignOut =
  req
    "GlobalSignOut"
    "fixture/GlobalSignOut.yaml"

requestGetUserPoolMfaConfig :: GetUserPoolMfaConfig -> TestTree
requestGetUserPoolMfaConfig =
  req
    "GetUserPoolMfaConfig"
    "fixture/GetUserPoolMfaConfig.yaml"

requestCreateResourceServer :: CreateResourceServer -> TestTree
requestCreateResourceServer =
  req
    "CreateResourceServer"
    "fixture/CreateResourceServer.yaml"

requestAdminUpdateUserAttributes :: AdminUpdateUserAttributes -> TestTree
requestAdminUpdateUserAttributes =
  req
    "AdminUpdateUserAttributes"
    "fixture/AdminUpdateUserAttributes.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestAdminDeleteUserAttributes :: AdminDeleteUserAttributes -> TestTree
requestAdminDeleteUserAttributes =
  req
    "AdminDeleteUserAttributes"
    "fixture/AdminDeleteUserAttributes.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestAdminDisableProviderForUser :: AdminDisableProviderForUser -> TestTree
requestAdminDisableProviderForUser =
  req
    "AdminDisableProviderForUser"
    "fixture/AdminDisableProviderForUser.yaml"

requestCreateUserPoolClient :: CreateUserPoolClient -> TestTree
requestCreateUserPoolClient =
  req
    "CreateUserPoolClient"
    "fixture/CreateUserPoolClient.yaml"

requestListUserPoolClients :: ListUserPoolClients -> TestTree
requestListUserPoolClients =
  req
    "ListUserPoolClients"
    "fixture/ListUserPoolClients.yaml"

requestAddCustomAttributes :: AddCustomAttributes -> TestTree
requestAddCustomAttributes =
  req
    "AddCustomAttributes"
    "fixture/AddCustomAttributes.yaml"

requestAdminSetUserMFAPreference :: AdminSetUserMFAPreference -> TestTree
requestAdminSetUserMFAPreference =
  req
    "AdminSetUserMFAPreference"
    "fixture/AdminSetUserMFAPreference.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestConfirmForgotPassword :: ConfirmForgotPassword -> TestTree
requestConfirmForgotPassword =
  req
    "ConfirmForgotPassword"
    "fixture/ConfirmForgotPassword.yaml"

requestSetUserMFAPreference :: SetUserMFAPreference -> TestTree
requestSetUserMFAPreference =
  req
    "SetUserMFAPreference"
    "fixture/SetUserMFAPreference.yaml"

requestVerifyUserAttribute :: VerifyUserAttribute -> TestTree
requestVerifyUserAttribute =
  req
    "VerifyUserAttribute"
    "fixture/VerifyUserAttribute.yaml"

requestConfirmDevice :: ConfirmDevice -> TestTree
requestConfirmDevice =
  req
    "ConfirmDevice"
    "fixture/ConfirmDevice.yaml"

requestDescribeResourceServer :: DescribeResourceServer -> TestTree
requestDescribeResourceServer =
  req
    "DescribeResourceServer"
    "fixture/DescribeResourceServer.yaml"

-- Responses

responseGetUserAttributeVerificationCode :: GetUserAttributeVerificationCodeResponse -> TestTree
responseGetUserAttributeVerificationCode =
  res
    "GetUserAttributeVerificationCodeResponse"
    "fixture/GetUserAttributeVerificationCodeResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserAttributeVerificationCode)

responseAdminDeleteUser :: AdminDeleteUserResponse -> TestTree
responseAdminDeleteUser =
  res
    "AdminDeleteUserResponse"
    "fixture/AdminDeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminDeleteUser)

responseCreateUserImportJob :: CreateUserImportJobResponse -> TestTree
responseCreateUserImportJob =
  res
    "CreateUserImportJobResponse"
    "fixture/CreateUserImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserImportJob)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy :: Proxy GetUser)

responseSetUserPoolMfaConfig :: SetUserPoolMfaConfigResponse -> TestTree
responseSetUserPoolMfaConfig =
  res
    "SetUserPoolMfaConfigResponse"
    "fixture/SetUserPoolMfaConfigResponse.proto"
    defaultService
    (Proxy :: Proxy SetUserPoolMfaConfig)

responseUpdateUserAttributes :: UpdateUserAttributesResponse -> TestTree
responseUpdateUserAttributes =
  res
    "UpdateUserAttributesResponse"
    "fixture/UpdateUserAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserAttributes)

responseDeleteUserAttributes :: DeleteUserAttributesResponse -> TestTree
responseDeleteUserAttributes =
  res
    "DeleteUserAttributesResponse"
    "fixture/DeleteUserAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserAttributes)

responseUpdateAuthEventFeedback :: UpdateAuthEventFeedbackResponse -> TestTree
responseUpdateAuthEventFeedback =
  res
    "UpdateAuthEventFeedbackResponse"
    "fixture/UpdateAuthEventFeedbackResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAuthEventFeedback)

responseListUserPools :: ListUserPoolsResponse -> TestTree
responseListUserPools =
  res
    "ListUserPoolsResponse"
    "fixture/ListUserPoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserPools)

responseConfirmSignUp :: ConfirmSignUpResponse -> TestTree
responseConfirmSignUp =
  res
    "ConfirmSignUpResponse"
    "fixture/ConfirmSignUpResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmSignUp)

responseAdminLinkProviderForUser :: AdminLinkProviderForUserResponse -> TestTree
responseAdminLinkProviderForUser =
  res
    "AdminLinkProviderForUserResponse"
    "fixture/AdminLinkProviderForUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminLinkProviderForUser)

responseUpdateUserPool :: UpdateUserPoolResponse -> TestTree
responseUpdateUserPool =
  res
    "UpdateUserPoolResponse"
    "fixture/UpdateUserPoolResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserPool)

responseDeleteUserPool :: DeleteUserPoolResponse -> TestTree
responseDeleteUserPool =
  res
    "DeleteUserPoolResponse"
    "fixture/DeleteUserPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPool)

responseUpdateUserPoolDomain :: UpdateUserPoolDomainResponse -> TestTree
responseUpdateUserPoolDomain =
  res
    "UpdateUserPoolDomainResponse"
    "fixture/UpdateUserPoolDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserPoolDomain)

responseDeleteUserPoolDomain :: DeleteUserPoolDomainResponse -> TestTree
responseDeleteUserPoolDomain =
  res
    "DeleteUserPoolDomainResponse"
    "fixture/DeleteUserPoolDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPoolDomain)

responseCreateUserPoolDomain :: CreateUserPoolDomainResponse -> TestTree
responseCreateUserPoolDomain =
  res
    "CreateUserPoolDomainResponse"
    "fixture/CreateUserPoolDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserPoolDomain)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword =
  res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ChangePassword)

responseRespondToAuthChallenge :: RespondToAuthChallengeResponse -> TestTree
responseRespondToAuthChallenge =
  res
    "RespondToAuthChallengeResponse"
    "fixture/RespondToAuthChallengeResponse.proto"
    defaultService
    (Proxy :: Proxy RespondToAuthChallenge)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevices)

responseAdminGetDevice :: AdminGetDeviceResponse -> TestTree
responseAdminGetDevice =
  res
    "AdminGetDeviceResponse"
    "fixture/AdminGetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy AdminGetDevice)

responseCreateUserPool :: CreateUserPoolResponse -> TestTree
responseCreateUserPool =
  res
    "CreateUserPoolResponse"
    "fixture/CreateUserPoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserPool)

responseAdminRemoveUserFromGroup :: AdminRemoveUserFromGroupResponse -> TestTree
responseAdminRemoveUserFromGroup =
  res
    "AdminRemoveUserFromGroupResponse"
    "fixture/AdminRemoveUserFromGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AdminRemoveUserFromGroup)

responseSetRiskConfiguration :: SetRiskConfigurationResponse -> TestTree
responseSetRiskConfiguration =
  res
    "SetRiskConfigurationResponse"
    "fixture/SetRiskConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy SetRiskConfiguration)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroups)

responseUpdateIdentityProvider :: UpdateIdentityProviderResponse -> TestTree
responseUpdateIdentityProvider =
  res
    "UpdateIdentityProviderResponse"
    "fixture/UpdateIdentityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIdentityProvider)

responseListIdentityProviders :: ListIdentityProvidersResponse -> TestTree
responseListIdentityProviders =
  res
    "ListIdentityProvidersResponse"
    "fixture/ListIdentityProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentityProviders)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevice)

responseDeleteIdentityProvider :: DeleteIdentityProviderResponse -> TestTree
responseDeleteIdentityProvider =
  res
    "DeleteIdentityProviderResponse"
    "fixture/DeleteIdentityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIdentityProvider)

responseAdminListDevices :: AdminListDevicesResponse -> TestTree
responseAdminListDevices =
  res
    "AdminListDevicesResponse"
    "fixture/AdminListDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy AdminListDevices)

responseAdminConfirmSignUp :: AdminConfirmSignUpResponse -> TestTree
responseAdminConfirmSignUp =
  res
    "AdminConfirmSignUpResponse"
    "fixture/AdminConfirmSignUpResponse.proto"
    defaultService
    (Proxy :: Proxy AdminConfirmSignUp)

responseSetUICustomization :: SetUICustomizationResponse -> TestTree
responseSetUICustomization =
  res
    "SetUICustomizationResponse"
    "fixture/SetUICustomizationResponse.proto"
    defaultService
    (Proxy :: Proxy SetUICustomization)

responseAdminListUserAuthEvents :: AdminListUserAuthEventsResponse -> TestTree
responseAdminListUserAuthEvents =
  res
    "AdminListUserAuthEventsResponse"
    "fixture/AdminListUserAuthEventsResponse.proto"
    defaultService
    (Proxy :: Proxy AdminListUserAuthEvents)

responseAdminAddUserToGroup :: AdminAddUserToGroupResponse -> TestTree
responseAdminAddUserToGroup =
  res
    "AdminAddUserToGroupResponse"
    "fixture/AdminAddUserToGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AdminAddUserToGroup)

responseVerifySoftwareToken :: VerifySoftwareTokenResponse -> TestTree
responseVerifySoftwareToken =
  res
    "VerifySoftwareTokenResponse"
    "fixture/VerifySoftwareTokenResponse.proto"
    defaultService
    (Proxy :: Proxy VerifySoftwareToken)

responseStopUserImportJob :: StopUserImportJobResponse -> TestTree
responseStopUserImportJob =
  res
    "StopUserImportJobResponse"
    "fixture/StopUserImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopUserImportJob)

responseCreateIdentityProvider :: CreateIdentityProviderResponse -> TestTree
responseCreateIdentityProvider =
  res
    "CreateIdentityProviderResponse"
    "fixture/CreateIdentityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIdentityProvider)

responseInitiateAuth :: InitiateAuthResponse -> TestTree
responseInitiateAuth =
  res
    "InitiateAuthResponse"
    "fixture/InitiateAuthResponse.proto"
    defaultService
    (Proxy :: Proxy InitiateAuth)

responseAdminSetUserPassword :: AdminSetUserPasswordResponse -> TestTree
responseAdminSetUserPassword =
  res
    "AdminSetUserPasswordResponse"
    "fixture/AdminSetUserPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy AdminSetUserPassword)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseAdminListGroupsForUser :: AdminListGroupsForUserResponse -> TestTree
responseAdminListGroupsForUser =
  res
    "AdminListGroupsForUserResponse"
    "fixture/AdminListGroupsForUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminListGroupsForUser)

responseAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedbackResponse -> TestTree
responseAdminUpdateAuthEventFeedback =
  res
    "AdminUpdateAuthEventFeedbackResponse"
    "fixture/AdminUpdateAuthEventFeedbackResponse.proto"
    defaultService
    (Proxy :: Proxy AdminUpdateAuthEventFeedback)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroup)

responseStartUserImportJob :: StartUserImportJobResponse -> TestTree
responseStartUserImportJob =
  res
    "StartUserImportJobResponse"
    "fixture/StartUserImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartUserImportJob)

responseDescribeUserPoolDomain :: DescribeUserPoolDomainResponse -> TestTree
responseDescribeUserPoolDomain =
  res
    "DescribeUserPoolDomainResponse"
    "fixture/DescribeUserPoolDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserPoolDomain)

responseListUsersInGroup :: ListUsersInGroupResponse -> TestTree
responseListUsersInGroup =
  res
    "ListUsersInGroupResponse"
    "fixture/ListUsersInGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsersInGroup)

responseAdminUserGlobalSignOut :: AdminUserGlobalSignOutResponse -> TestTree
responseAdminUserGlobalSignOut =
  res
    "AdminUserGlobalSignOutResponse"
    "fixture/AdminUserGlobalSignOutResponse.proto"
    defaultService
    (Proxy :: Proxy AdminUserGlobalSignOut)

responseDescribeUserPool :: DescribeUserPoolResponse -> TestTree
responseDescribeUserPool =
  res
    "DescribeUserPoolResponse"
    "fixture/DescribeUserPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserPool)

responseAdminGetUser :: AdminGetUserResponse -> TestTree
responseAdminGetUser =
  res
    "AdminGetUserResponse"
    "fixture/AdminGetUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminGetUser)

responseGetSigningCertificate :: GetSigningCertificateResponse -> TestTree
responseGetSigningCertificate =
  res
    "GetSigningCertificateResponse"
    "fixture/GetSigningCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GetSigningCertificate)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseAssociateSoftwareToken :: AssociateSoftwareTokenResponse -> TestTree
responseAssociateSoftwareToken =
  res
    "AssociateSoftwareTokenResponse"
    "fixture/AssociateSoftwareTokenResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateSoftwareToken)

responseForgotPassword :: ForgotPasswordResponse -> TestTree
responseForgotPassword =
  res
    "ForgotPasswordResponse"
    "fixture/ForgotPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ForgotPassword)

responseUpdateDeviceStatus :: UpdateDeviceStatusResponse -> TestTree
responseUpdateDeviceStatus =
  res
    "UpdateDeviceStatusResponse"
    "fixture/UpdateDeviceStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeviceStatus)

responseDeleteUserPoolClient :: DeleteUserPoolClientResponse -> TestTree
responseDeleteUserPoolClient =
  res
    "DeleteUserPoolClientResponse"
    "fixture/DeleteUserPoolClientResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPoolClient)

responseUpdateUserPoolClient :: UpdateUserPoolClientResponse -> TestTree
responseUpdateUserPoolClient =
  res
    "UpdateUserPoolClientResponse"
    "fixture/UpdateUserPoolClientResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserPoolClient)

responseForgetDevice :: ForgetDeviceResponse -> TestTree
responseForgetDevice =
  res
    "ForgetDeviceResponse"
    "fixture/ForgetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy ForgetDevice)

responseAdminDisableUser :: AdminDisableUserResponse -> TestTree
responseAdminDisableUser =
  res
    "AdminDisableUserResponse"
    "fixture/AdminDisableUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminDisableUser)

responseAdminCreateUser :: AdminCreateUserResponse -> TestTree
responseAdminCreateUser =
  res
    "AdminCreateUserResponse"
    "fixture/AdminCreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminCreateUser)

responseAdminUpdateDeviceStatus :: AdminUpdateDeviceStatusResponse -> TestTree
responseAdminUpdateDeviceStatus =
  res
    "AdminUpdateDeviceStatusResponse"
    "fixture/AdminUpdateDeviceStatusResponse.proto"
    defaultService
    (Proxy :: Proxy AdminUpdateDeviceStatus)

responseAdminForgetDevice :: AdminForgetDeviceResponse -> TestTree
responseAdminForgetDevice =
  res
    "AdminForgetDeviceResponse"
    "fixture/AdminForgetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy AdminForgetDevice)

responseDescribeIdentityProvider :: DescribeIdentityProviderResponse -> TestTree
responseDescribeIdentityProvider =
  res
    "DescribeIdentityProviderResponse"
    "fixture/DescribeIdentityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentityProvider)

responseListUserImportJobs :: ListUserImportJobsResponse -> TestTree
responseListUserImportJobs =
  res
    "ListUserImportJobsResponse"
    "fixture/ListUserImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserImportJobs)

responseGetUICustomization :: GetUICustomizationResponse -> TestTree
responseGetUICustomization =
  res
    "GetUICustomizationResponse"
    "fixture/GetUICustomizationResponse.proto"
    defaultService
    (Proxy :: Proxy GetUICustomization)

responseGetCSVHeader :: GetCSVHeaderResponse -> TestTree
responseGetCSVHeader =
  res
    "GetCSVHeaderResponse"
    "fixture/GetCSVHeaderResponse.proto"
    defaultService
    (Proxy :: Proxy GetCSVHeader)

responseAdminEnableUser :: AdminEnableUserResponse -> TestTree
responseAdminEnableUser =
  res
    "AdminEnableUserResponse"
    "fixture/AdminEnableUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminEnableUser)

responseAdminInitiateAuth :: AdminInitiateAuthResponse -> TestTree
responseAdminInitiateAuth =
  res
    "AdminInitiateAuthResponse"
    "fixture/AdminInitiateAuthResponse.proto"
    defaultService
    (Proxy :: Proxy AdminInitiateAuth)

responseAdminResetUserPassword :: AdminResetUserPasswordResponse -> TestTree
responseAdminResetUserPassword =
  res
    "AdminResetUserPasswordResponse"
    "fixture/AdminResetUserPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy AdminResetUserPassword)

responseGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifierResponse -> TestTree
responseGetIdentityProviderByIdentifier =
  res
    "GetIdentityProviderByIdentifierResponse"
    "fixture/GetIdentityProviderByIdentifierResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityProviderByIdentifier)

responseAdminSetUserSettings :: AdminSetUserSettingsResponse -> TestTree
responseAdminSetUserSettings =
  res
    "AdminSetUserSettingsResponse"
    "fixture/AdminSetUserSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy AdminSetUserSettings)

responseDescribeUserPoolClient :: DescribeUserPoolClientResponse -> TestTree
responseDescribeUserPoolClient =
  res
    "DescribeUserPoolClientResponse"
    "fixture/DescribeUserPoolClientResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserPoolClient)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroup)

responseResendConfirmationCode :: ResendConfirmationCodeResponse -> TestTree
responseResendConfirmationCode =
  res
    "ResendConfirmationCodeResponse"
    "fixture/ResendConfirmationCodeResponse.proto"
    defaultService
    (Proxy :: Proxy ResendConfirmationCode)

responseAdminRespondToAuthChallenge :: AdminRespondToAuthChallengeResponse -> TestTree
responseAdminRespondToAuthChallenge =
  res
    "AdminRespondToAuthChallengeResponse"
    "fixture/AdminRespondToAuthChallengeResponse.proto"
    defaultService
    (Proxy :: Proxy AdminRespondToAuthChallenge)

responseSignUp :: SignUpResponse -> TestTree
responseSignUp =
  res
    "SignUpResponse"
    "fixture/SignUpResponse.proto"
    defaultService
    (Proxy :: Proxy SignUp)

responseDescribeUserImportJob :: DescribeUserImportJobResponse -> TestTree
responseDescribeUserImportJob =
  res
    "DescribeUserImportJobResponse"
    "fixture/DescribeUserImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserImportJob)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroup)

responseUpdateResourceServer :: UpdateResourceServerResponse -> TestTree
responseUpdateResourceServer =
  res
    "UpdateResourceServerResponse"
    "fixture/UpdateResourceServerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResourceServer)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseSetUserSettings :: SetUserSettingsResponse -> TestTree
responseSetUserSettings =
  res
    "SetUserSettingsResponse"
    "fixture/SetUserSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy SetUserSettings)

responseListResourceServers :: ListResourceServersResponse -> TestTree
responseListResourceServers =
  res
    "ListResourceServersResponse"
    "fixture/ListResourceServersResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceServers)

responseDeleteResourceServer :: DeleteResourceServerResponse -> TestTree
responseDeleteResourceServer =
  res
    "DeleteResourceServerResponse"
    "fixture/DeleteResourceServerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourceServer)

responseDescribeRiskConfiguration :: DescribeRiskConfigurationResponse -> TestTree
responseDescribeRiskConfiguration =
  res
    "DescribeRiskConfigurationResponse"
    "fixture/DescribeRiskConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRiskConfiguration)

responseGlobalSignOut :: GlobalSignOutResponse -> TestTree
responseGlobalSignOut =
  res
    "GlobalSignOutResponse"
    "fixture/GlobalSignOutResponse.proto"
    defaultService
    (Proxy :: Proxy GlobalSignOut)

responseGetUserPoolMfaConfig :: GetUserPoolMfaConfigResponse -> TestTree
responseGetUserPoolMfaConfig =
  res
    "GetUserPoolMfaConfigResponse"
    "fixture/GetUserPoolMfaConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserPoolMfaConfig)

responseCreateResourceServer :: CreateResourceServerResponse -> TestTree
responseCreateResourceServer =
  res
    "CreateResourceServerResponse"
    "fixture/CreateResourceServerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourceServer)

responseAdminUpdateUserAttributes :: AdminUpdateUserAttributesResponse -> TestTree
responseAdminUpdateUserAttributes =
  res
    "AdminUpdateUserAttributesResponse"
    "fixture/AdminUpdateUserAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy AdminUpdateUserAttributes)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseAdminDeleteUserAttributes :: AdminDeleteUserAttributesResponse -> TestTree
responseAdminDeleteUserAttributes =
  res
    "AdminDeleteUserAttributesResponse"
    "fixture/AdminDeleteUserAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy AdminDeleteUserAttributes)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseAdminDisableProviderForUser :: AdminDisableProviderForUserResponse -> TestTree
responseAdminDisableProviderForUser =
  res
    "AdminDisableProviderForUserResponse"
    "fixture/AdminDisableProviderForUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminDisableProviderForUser)

responseCreateUserPoolClient :: CreateUserPoolClientResponse -> TestTree
responseCreateUserPoolClient =
  res
    "CreateUserPoolClientResponse"
    "fixture/CreateUserPoolClientResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserPoolClient)

responseListUserPoolClients :: ListUserPoolClientsResponse -> TestTree
responseListUserPoolClients =
  res
    "ListUserPoolClientsResponse"
    "fixture/ListUserPoolClientsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserPoolClients)

responseAddCustomAttributes :: AddCustomAttributesResponse -> TestTree
responseAddCustomAttributes =
  res
    "AddCustomAttributesResponse"
    "fixture/AddCustomAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy AddCustomAttributes)

responseAdminSetUserMFAPreference :: AdminSetUserMFAPreferenceResponse -> TestTree
responseAdminSetUserMFAPreference =
  res
    "AdminSetUserMFAPreferenceResponse"
    "fixture/AdminSetUserMFAPreferenceResponse.proto"
    defaultService
    (Proxy :: Proxy AdminSetUserMFAPreference)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseConfirmForgotPassword :: ConfirmForgotPasswordResponse -> TestTree
responseConfirmForgotPassword =
  res
    "ConfirmForgotPasswordResponse"
    "fixture/ConfirmForgotPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmForgotPassword)

responseSetUserMFAPreference :: SetUserMFAPreferenceResponse -> TestTree
responseSetUserMFAPreference =
  res
    "SetUserMFAPreferenceResponse"
    "fixture/SetUserMFAPreferenceResponse.proto"
    defaultService
    (Proxy :: Proxy SetUserMFAPreference)

responseVerifyUserAttribute :: VerifyUserAttributeResponse -> TestTree
responseVerifyUserAttribute =
  res
    "VerifyUserAttributeResponse"
    "fixture/VerifyUserAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyUserAttribute)

responseConfirmDevice :: ConfirmDeviceResponse -> TestTree
responseConfirmDevice =
  res
    "ConfirmDeviceResponse"
    "fixture/ConfirmDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmDevice)

responseDescribeResourceServer :: DescribeResourceServerResponse -> TestTree
responseDescribeResourceServer =
  res
    "DescribeResourceServerResponse"
    "fixture/DescribeResourceServerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResourceServer)
