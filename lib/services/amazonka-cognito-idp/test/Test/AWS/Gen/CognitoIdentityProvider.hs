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
--         , requestCreateUserImportJob $
--             newCreateUserImportJob
--
--         , requestSetUserPoolMfaConfig $
--             newSetUserPoolMfaConfig
--
--         , requestAdminDeleteUser $
--             newAdminDeleteUser
--
--         , requestGetUser $
--             newGetUser
--
--         , requestDeleteUserAttributes $
--             newDeleteUserAttributes
--
--         , requestUpdateUserAttributes $
--             newUpdateUserAttributes
--
--         , requestDeleteUserPoolDomain $
--             newDeleteUserPoolDomain
--
--         , requestListUserPools $
--             newListUserPools
--
--         , requestDeleteUserPool $
--             newDeleteUserPool
--
--         , requestUpdateAuthEventFeedback $
--             newUpdateAuthEventFeedback
--
--         , requestUpdateUserPool $
--             newUpdateUserPool
--
--         , requestConfirmSignUp $
--             newConfirmSignUp
--
--         , requestUpdateUserPoolDomain $
--             newUpdateUserPoolDomain
--
--         , requestAdminLinkProviderForUser $
--             newAdminLinkProviderForUser
--
--         , requestSetRiskConfiguration $
--             newSetRiskConfiguration
--
--         , requestListDevices $
--             newListDevices
--
--         , requestCreateUserPool $
--             newCreateUserPool
--
--         , requestChangePassword $
--             newChangePassword
--
--         , requestAdminGetDevice $
--             newAdminGetDevice
--
--         , requestRespondToAuthChallenge $
--             newRespondToAuthChallenge
--
--         , requestAdminRemoveUserFromGroup $
--             newAdminRemoveUserFromGroup
--
--         , requestCreateUserPoolDomain $
--             newCreateUserPoolDomain
--
--         , requestListIdentityProviders $
--             newListIdentityProviders
--
--         , requestDeleteIdentityProvider $
--             newDeleteIdentityProvider
--
--         , requestUpdateIdentityProvider $
--             newUpdateIdentityProvider
--
--         , requestListGroups $
--             newListGroups
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestAdminListDevices $
--             newAdminListDevices
--
--         , requestRevokeToken $
--             newRevokeToken
--
--         , requestAdminSetUserPassword $
--             newAdminSetUserPassword
--
--         , requestStartUserImportJob $
--             newStartUserImportJob
--
--         , requestAdminConfirmSignUp $
--             newAdminConfirmSignUp
--
--         , requestAdminListUserAuthEvents $
--             newAdminListUserAuthEvents
--
--         , requestStopUserImportJob $
--             newStopUserImportJob
--
--         , requestInitiateAuth $
--             newInitiateAuth
--
--         , requestVerifySoftwareToken $
--             newVerifySoftwareToken
--
--         , requestCreateIdentityProvider $
--             newCreateIdentityProvider
--
--         , requestAdminListGroupsForUser $
--             newAdminListGroupsForUser
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestAdminAddUserToGroup $
--             newAdminAddUserToGroup
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestSetUICustomization $
--             newSetUICustomization
--
--         , requestAdminUpdateAuthEventFeedback $
--             newAdminUpdateAuthEventFeedback
--
--         , requestDescribeUserPool $
--             newDescribeUserPool
--
--         , requestAssociateSoftwareToken $
--             newAssociateSoftwareToken
--
--         , requestForgotPassword $
--             newForgotPassword
--
--         , requestAdminUserGlobalSignOut $
--             newAdminUserGlobalSignOut
--
--         , requestTagResource $
--             newTagResource
--
--         , requestAdminGetUser $
--             newAdminGetUser
--
--         , requestListUsersInGroup $
--             newListUsersInGroup
--
--         , requestGetSigningCertificate $
--             newGetSigningCertificate
--
--         , requestDescribeUserPoolDomain $
--             newDescribeUserPoolDomain
--
--         , requestUpdateUserPoolClient $
--             newUpdateUserPoolClient
--
--         , requestUpdateDeviceStatus $
--             newUpdateDeviceStatus
--
--         , requestForgetDevice $
--             newForgetDevice
--
--         , requestDeleteUserPoolClient $
--             newDeleteUserPoolClient
--
--         , requestAdminUpdateDeviceStatus $
--             newAdminUpdateDeviceStatus
--
--         , requestDescribeIdentityProvider $
--             newDescribeIdentityProvider
--
--         , requestAdminForgetDevice $
--             newAdminForgetDevice
--
--         , requestListUserImportJobs $
--             newListUserImportJobs
--
--         , requestAdminCreateUser $
--             newAdminCreateUser
--
--         , requestAdminDisableUser $
--             newAdminDisableUser
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
--         , requestAdminResetUserPassword $
--             newAdminResetUserPassword
--
--         , requestAdminInitiateAuth $
--             newAdminInitiateAuth
--
--         , requestAdminSetUserSettings $
--             newAdminSetUserSettings
--
--         , requestDescribeUserPoolClient $
--             newDescribeUserPoolClient
--
--         , requestResendConfirmationCode $
--             newResendConfirmationCode
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestGetIdentityProviderByIdentifier $
--             newGetIdentityProviderByIdentifier
--
--         , requestDescribeRiskConfiguration $
--             newDescribeRiskConfiguration
--
--         , requestDescribeUserImportJob $
--             newDescribeUserImportJob
--
--         , requestSetUserSettings $
--             newSetUserSettings
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestAdminRespondToAuthChallenge $
--             newAdminRespondToAuthChallenge
--
--         , requestUpdateResourceServer $
--             newUpdateResourceServer
--
--         , requestListResourceServers $
--             newListResourceServers
--
--         , requestDeleteResourceServer $
--             newDeleteResourceServer
--
--         , requestSignUp $
--             newSignUp
--
--         , requestUpdateGroup $
--             newUpdateGroup
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
--         , requestAdminDeleteUserAttributes $
--             newAdminDeleteUserAttributes
--
--         , requestAdminDisableProviderForUser $
--             newAdminDisableProviderForUser
--
--         , requestAdminUpdateUserAttributes $
--             newAdminUpdateUserAttributes
--
--         , requestListUsers $
--             newListUsers
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestCreateUserPoolClient $
--             newCreateUserPoolClient
--
--         , requestAddCustomAttributes $
--             newAddCustomAttributes
--
--         , requestListUserPoolClients $
--             newListUserPoolClients
--
--         , requestAdminSetUserMFAPreference $
--             newAdminSetUserMFAPreference
--
--         , requestDescribeResourceServer $
--             newDescribeResourceServer
--
--         , requestSetUserMFAPreference $
--             newSetUserMFAPreference
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestConfirmDevice $
--             newConfirmDevice
--
--         , requestConfirmForgotPassword $
--             newConfirmForgotPassword
--
--         , requestVerifyUserAttribute $
--             newVerifyUserAttribute
--
--           ]

--     , testGroup "response"
--         [ responseGetUserAttributeVerificationCode $
--             newGetUserAttributeVerificationCodeResponse
--
--         , responseCreateUserImportJob $
--             newCreateUserImportJobResponse
--
--         , responseSetUserPoolMfaConfig $
--             newSetUserPoolMfaConfigResponse
--
--         , responseAdminDeleteUser $
--             newAdminDeleteUserResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseDeleteUserAttributes $
--             newDeleteUserAttributesResponse
--
--         , responseUpdateUserAttributes $
--             newUpdateUserAttributesResponse
--
--         , responseDeleteUserPoolDomain $
--             newDeleteUserPoolDomainResponse
--
--         , responseListUserPools $
--             newListUserPoolsResponse
--
--         , responseDeleteUserPool $
--             newDeleteUserPoolResponse
--
--         , responseUpdateAuthEventFeedback $
--             newUpdateAuthEventFeedbackResponse
--
--         , responseUpdateUserPool $
--             newUpdateUserPoolResponse
--
--         , responseConfirmSignUp $
--             newConfirmSignUpResponse
--
--         , responseUpdateUserPoolDomain $
--             newUpdateUserPoolDomainResponse
--
--         , responseAdminLinkProviderForUser $
--             newAdminLinkProviderForUserResponse
--
--         , responseSetRiskConfiguration $
--             newSetRiskConfigurationResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseCreateUserPool $
--             newCreateUserPoolResponse
--
--         , responseChangePassword $
--             newChangePasswordResponse
--
--         , responseAdminGetDevice $
--             newAdminGetDeviceResponse
--
--         , responseRespondToAuthChallenge $
--             newRespondToAuthChallengeResponse
--
--         , responseAdminRemoveUserFromGroup $
--             newAdminRemoveUserFromGroupResponse
--
--         , responseCreateUserPoolDomain $
--             newCreateUserPoolDomainResponse
--
--         , responseListIdentityProviders $
--             newListIdentityProvidersResponse
--
--         , responseDeleteIdentityProvider $
--             newDeleteIdentityProviderResponse
--
--         , responseUpdateIdentityProvider $
--             newUpdateIdentityProviderResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseAdminListDevices $
--             newAdminListDevicesResponse
--
--         , responseRevokeToken $
--             newRevokeTokenResponse
--
--         , responseAdminSetUserPassword $
--             newAdminSetUserPasswordResponse
--
--         , responseStartUserImportJob $
--             newStartUserImportJobResponse
--
--         , responseAdminConfirmSignUp $
--             newAdminConfirmSignUpResponse
--
--         , responseAdminListUserAuthEvents $
--             newAdminListUserAuthEventsResponse
--
--         , responseStopUserImportJob $
--             newStopUserImportJobResponse
--
--         , responseInitiateAuth $
--             newInitiateAuthResponse
--
--         , responseVerifySoftwareToken $
--             newVerifySoftwareTokenResponse
--
--         , responseCreateIdentityProvider $
--             newCreateIdentityProviderResponse
--
--         , responseAdminListGroupsForUser $
--             newAdminListGroupsForUserResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseAdminAddUserToGroup $
--             newAdminAddUserToGroupResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseSetUICustomization $
--             newSetUICustomizationResponse
--
--         , responseAdminUpdateAuthEventFeedback $
--             newAdminUpdateAuthEventFeedbackResponse
--
--         , responseDescribeUserPool $
--             newDescribeUserPoolResponse
--
--         , responseAssociateSoftwareToken $
--             newAssociateSoftwareTokenResponse
--
--         , responseForgotPassword $
--             newForgotPasswordResponse
--
--         , responseAdminUserGlobalSignOut $
--             newAdminUserGlobalSignOutResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseAdminGetUser $
--             newAdminGetUserResponse
--
--         , responseListUsersInGroup $
--             newListUsersInGroupResponse
--
--         , responseGetSigningCertificate $
--             newGetSigningCertificateResponse
--
--         , responseDescribeUserPoolDomain $
--             newDescribeUserPoolDomainResponse
--
--         , responseUpdateUserPoolClient $
--             newUpdateUserPoolClientResponse
--
--         , responseUpdateDeviceStatus $
--             newUpdateDeviceStatusResponse
--
--         , responseForgetDevice $
--             newForgetDeviceResponse
--
--         , responseDeleteUserPoolClient $
--             newDeleteUserPoolClientResponse
--
--         , responseAdminUpdateDeviceStatus $
--             newAdminUpdateDeviceStatusResponse
--
--         , responseDescribeIdentityProvider $
--             newDescribeIdentityProviderResponse
--
--         , responseAdminForgetDevice $
--             newAdminForgetDeviceResponse
--
--         , responseListUserImportJobs $
--             newListUserImportJobsResponse
--
--         , responseAdminCreateUser $
--             newAdminCreateUserResponse
--
--         , responseAdminDisableUser $
--             newAdminDisableUserResponse
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
--         , responseAdminResetUserPassword $
--             newAdminResetUserPasswordResponse
--
--         , responseAdminInitiateAuth $
--             newAdminInitiateAuthResponse
--
--         , responseAdminSetUserSettings $
--             newAdminSetUserSettingsResponse
--
--         , responseDescribeUserPoolClient $
--             newDescribeUserPoolClientResponse
--
--         , responseResendConfirmationCode $
--             newResendConfirmationCodeResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseGetIdentityProviderByIdentifier $
--             newGetIdentityProviderByIdentifierResponse
--
--         , responseDescribeRiskConfiguration $
--             newDescribeRiskConfigurationResponse
--
--         , responseDescribeUserImportJob $
--             newDescribeUserImportJobResponse
--
--         , responseSetUserSettings $
--             newSetUserSettingsResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseAdminRespondToAuthChallenge $
--             newAdminRespondToAuthChallengeResponse
--
--         , responseUpdateResourceServer $
--             newUpdateResourceServerResponse
--
--         , responseListResourceServers $
--             newListResourceServersResponse
--
--         , responseDeleteResourceServer $
--             newDeleteResourceServerResponse
--
--         , responseSignUp $
--             newSignUpResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
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
--         , responseAdminDeleteUserAttributes $
--             newAdminDeleteUserAttributesResponse
--
--         , responseAdminDisableProviderForUser $
--             newAdminDisableProviderForUserResponse
--
--         , responseAdminUpdateUserAttributes $
--             newAdminUpdateUserAttributesResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseCreateUserPoolClient $
--             newCreateUserPoolClientResponse
--
--         , responseAddCustomAttributes $
--             newAddCustomAttributesResponse
--
--         , responseListUserPoolClients $
--             newListUserPoolClientsResponse
--
--         , responseAdminSetUserMFAPreference $
--             newAdminSetUserMFAPreferenceResponse
--
--         , responseDescribeResourceServer $
--             newDescribeResourceServerResponse
--
--         , responseSetUserMFAPreference $
--             newSetUserMFAPreferenceResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseConfirmDevice $
--             newConfirmDeviceResponse
--
--         , responseConfirmForgotPassword $
--             newConfirmForgotPasswordResponse
--
--         , responseVerifyUserAttribute $
--             newVerifyUserAttributeResponse
--
--           ]
--     ]

-- Requests

requestGetUserAttributeVerificationCode :: GetUserAttributeVerificationCode -> TestTree
requestGetUserAttributeVerificationCode =
  req
    "GetUserAttributeVerificationCode"
    "fixture/GetUserAttributeVerificationCode.yaml"

requestCreateUserImportJob :: CreateUserImportJob -> TestTree
requestCreateUserImportJob =
  req
    "CreateUserImportJob"
    "fixture/CreateUserImportJob.yaml"

requestSetUserPoolMfaConfig :: SetUserPoolMfaConfig -> TestTree
requestSetUserPoolMfaConfig =
  req
    "SetUserPoolMfaConfig"
    "fixture/SetUserPoolMfaConfig.yaml"

requestAdminDeleteUser :: AdminDeleteUser -> TestTree
requestAdminDeleteUser =
  req
    "AdminDeleteUser"
    "fixture/AdminDeleteUser.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser =
  req
    "GetUser"
    "fixture/GetUser.yaml"

requestDeleteUserAttributes :: DeleteUserAttributes -> TestTree
requestDeleteUserAttributes =
  req
    "DeleteUserAttributes"
    "fixture/DeleteUserAttributes.yaml"

requestUpdateUserAttributes :: UpdateUserAttributes -> TestTree
requestUpdateUserAttributes =
  req
    "UpdateUserAttributes"
    "fixture/UpdateUserAttributes.yaml"

requestDeleteUserPoolDomain :: DeleteUserPoolDomain -> TestTree
requestDeleteUserPoolDomain =
  req
    "DeleteUserPoolDomain"
    "fixture/DeleteUserPoolDomain.yaml"

requestListUserPools :: ListUserPools -> TestTree
requestListUserPools =
  req
    "ListUserPools"
    "fixture/ListUserPools.yaml"

requestDeleteUserPool :: DeleteUserPool -> TestTree
requestDeleteUserPool =
  req
    "DeleteUserPool"
    "fixture/DeleteUserPool.yaml"

requestUpdateAuthEventFeedback :: UpdateAuthEventFeedback -> TestTree
requestUpdateAuthEventFeedback =
  req
    "UpdateAuthEventFeedback"
    "fixture/UpdateAuthEventFeedback.yaml"

requestUpdateUserPool :: UpdateUserPool -> TestTree
requestUpdateUserPool =
  req
    "UpdateUserPool"
    "fixture/UpdateUserPool.yaml"

requestConfirmSignUp :: ConfirmSignUp -> TestTree
requestConfirmSignUp =
  req
    "ConfirmSignUp"
    "fixture/ConfirmSignUp.yaml"

requestUpdateUserPoolDomain :: UpdateUserPoolDomain -> TestTree
requestUpdateUserPoolDomain =
  req
    "UpdateUserPoolDomain"
    "fixture/UpdateUserPoolDomain.yaml"

requestAdminLinkProviderForUser :: AdminLinkProviderForUser -> TestTree
requestAdminLinkProviderForUser =
  req
    "AdminLinkProviderForUser"
    "fixture/AdminLinkProviderForUser.yaml"

requestSetRiskConfiguration :: SetRiskConfiguration -> TestTree
requestSetRiskConfiguration =
  req
    "SetRiskConfiguration"
    "fixture/SetRiskConfiguration.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestCreateUserPool :: CreateUserPool -> TestTree
requestCreateUserPool =
  req
    "CreateUserPool"
    "fixture/CreateUserPool.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword =
  req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

requestAdminGetDevice :: AdminGetDevice -> TestTree
requestAdminGetDevice =
  req
    "AdminGetDevice"
    "fixture/AdminGetDevice.yaml"

requestRespondToAuthChallenge :: RespondToAuthChallenge -> TestTree
requestRespondToAuthChallenge =
  req
    "RespondToAuthChallenge"
    "fixture/RespondToAuthChallenge.yaml"

requestAdminRemoveUserFromGroup :: AdminRemoveUserFromGroup -> TestTree
requestAdminRemoveUserFromGroup =
  req
    "AdminRemoveUserFromGroup"
    "fixture/AdminRemoveUserFromGroup.yaml"

requestCreateUserPoolDomain :: CreateUserPoolDomain -> TestTree
requestCreateUserPoolDomain =
  req
    "CreateUserPoolDomain"
    "fixture/CreateUserPoolDomain.yaml"

requestListIdentityProviders :: ListIdentityProviders -> TestTree
requestListIdentityProviders =
  req
    "ListIdentityProviders"
    "fixture/ListIdentityProviders.yaml"

requestDeleteIdentityProvider :: DeleteIdentityProvider -> TestTree
requestDeleteIdentityProvider =
  req
    "DeleteIdentityProvider"
    "fixture/DeleteIdentityProvider.yaml"

requestUpdateIdentityProvider :: UpdateIdentityProvider -> TestTree
requestUpdateIdentityProvider =
  req
    "UpdateIdentityProvider"
    "fixture/UpdateIdentityProvider.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestAdminListDevices :: AdminListDevices -> TestTree
requestAdminListDevices =
  req
    "AdminListDevices"
    "fixture/AdminListDevices.yaml"

requestRevokeToken :: RevokeToken -> TestTree
requestRevokeToken =
  req
    "RevokeToken"
    "fixture/RevokeToken.yaml"

requestAdminSetUserPassword :: AdminSetUserPassword -> TestTree
requestAdminSetUserPassword =
  req
    "AdminSetUserPassword"
    "fixture/AdminSetUserPassword.yaml"

requestStartUserImportJob :: StartUserImportJob -> TestTree
requestStartUserImportJob =
  req
    "StartUserImportJob"
    "fixture/StartUserImportJob.yaml"

requestAdminConfirmSignUp :: AdminConfirmSignUp -> TestTree
requestAdminConfirmSignUp =
  req
    "AdminConfirmSignUp"
    "fixture/AdminConfirmSignUp.yaml"

requestAdminListUserAuthEvents :: AdminListUserAuthEvents -> TestTree
requestAdminListUserAuthEvents =
  req
    "AdminListUserAuthEvents"
    "fixture/AdminListUserAuthEvents.yaml"

requestStopUserImportJob :: StopUserImportJob -> TestTree
requestStopUserImportJob =
  req
    "StopUserImportJob"
    "fixture/StopUserImportJob.yaml"

requestInitiateAuth :: InitiateAuth -> TestTree
requestInitiateAuth =
  req
    "InitiateAuth"
    "fixture/InitiateAuth.yaml"

requestVerifySoftwareToken :: VerifySoftwareToken -> TestTree
requestVerifySoftwareToken =
  req
    "VerifySoftwareToken"
    "fixture/VerifySoftwareToken.yaml"

requestCreateIdentityProvider :: CreateIdentityProvider -> TestTree
requestCreateIdentityProvider =
  req
    "CreateIdentityProvider"
    "fixture/CreateIdentityProvider.yaml"

requestAdminListGroupsForUser :: AdminListGroupsForUser -> TestTree
requestAdminListGroupsForUser =
  req
    "AdminListGroupsForUser"
    "fixture/AdminListGroupsForUser.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestAdminAddUserToGroup :: AdminAddUserToGroup -> TestTree
requestAdminAddUserToGroup =
  req
    "AdminAddUserToGroup"
    "fixture/AdminAddUserToGroup.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestSetUICustomization :: SetUICustomization -> TestTree
requestSetUICustomization =
  req
    "SetUICustomization"
    "fixture/SetUICustomization.yaml"

requestAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedback -> TestTree
requestAdminUpdateAuthEventFeedback =
  req
    "AdminUpdateAuthEventFeedback"
    "fixture/AdminUpdateAuthEventFeedback.yaml"

requestDescribeUserPool :: DescribeUserPool -> TestTree
requestDescribeUserPool =
  req
    "DescribeUserPool"
    "fixture/DescribeUserPool.yaml"

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

requestAdminUserGlobalSignOut :: AdminUserGlobalSignOut -> TestTree
requestAdminUserGlobalSignOut =
  req
    "AdminUserGlobalSignOut"
    "fixture/AdminUserGlobalSignOut.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestAdminGetUser :: AdminGetUser -> TestTree
requestAdminGetUser =
  req
    "AdminGetUser"
    "fixture/AdminGetUser.yaml"

requestListUsersInGroup :: ListUsersInGroup -> TestTree
requestListUsersInGroup =
  req
    "ListUsersInGroup"
    "fixture/ListUsersInGroup.yaml"

requestGetSigningCertificate :: GetSigningCertificate -> TestTree
requestGetSigningCertificate =
  req
    "GetSigningCertificate"
    "fixture/GetSigningCertificate.yaml"

requestDescribeUserPoolDomain :: DescribeUserPoolDomain -> TestTree
requestDescribeUserPoolDomain =
  req
    "DescribeUserPoolDomain"
    "fixture/DescribeUserPoolDomain.yaml"

requestUpdateUserPoolClient :: UpdateUserPoolClient -> TestTree
requestUpdateUserPoolClient =
  req
    "UpdateUserPoolClient"
    "fixture/UpdateUserPoolClient.yaml"

requestUpdateDeviceStatus :: UpdateDeviceStatus -> TestTree
requestUpdateDeviceStatus =
  req
    "UpdateDeviceStatus"
    "fixture/UpdateDeviceStatus.yaml"

requestForgetDevice :: ForgetDevice -> TestTree
requestForgetDevice =
  req
    "ForgetDevice"
    "fixture/ForgetDevice.yaml"

requestDeleteUserPoolClient :: DeleteUserPoolClient -> TestTree
requestDeleteUserPoolClient =
  req
    "DeleteUserPoolClient"
    "fixture/DeleteUserPoolClient.yaml"

requestAdminUpdateDeviceStatus :: AdminUpdateDeviceStatus -> TestTree
requestAdminUpdateDeviceStatus =
  req
    "AdminUpdateDeviceStatus"
    "fixture/AdminUpdateDeviceStatus.yaml"

requestDescribeIdentityProvider :: DescribeIdentityProvider -> TestTree
requestDescribeIdentityProvider =
  req
    "DescribeIdentityProvider"
    "fixture/DescribeIdentityProvider.yaml"

requestAdminForgetDevice :: AdminForgetDevice -> TestTree
requestAdminForgetDevice =
  req
    "AdminForgetDevice"
    "fixture/AdminForgetDevice.yaml"

requestListUserImportJobs :: ListUserImportJobs -> TestTree
requestListUserImportJobs =
  req
    "ListUserImportJobs"
    "fixture/ListUserImportJobs.yaml"

requestAdminCreateUser :: AdminCreateUser -> TestTree
requestAdminCreateUser =
  req
    "AdminCreateUser"
    "fixture/AdminCreateUser.yaml"

requestAdminDisableUser :: AdminDisableUser -> TestTree
requestAdminDisableUser =
  req
    "AdminDisableUser"
    "fixture/AdminDisableUser.yaml"

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

requestAdminResetUserPassword :: AdminResetUserPassword -> TestTree
requestAdminResetUserPassword =
  req
    "AdminResetUserPassword"
    "fixture/AdminResetUserPassword.yaml"

requestAdminInitiateAuth :: AdminInitiateAuth -> TestTree
requestAdminInitiateAuth =
  req
    "AdminInitiateAuth"
    "fixture/AdminInitiateAuth.yaml"

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

requestResendConfirmationCode :: ResendConfirmationCode -> TestTree
requestResendConfirmationCode =
  req
    "ResendConfirmationCode"
    "fixture/ResendConfirmationCode.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifier -> TestTree
requestGetIdentityProviderByIdentifier =
  req
    "GetIdentityProviderByIdentifier"
    "fixture/GetIdentityProviderByIdentifier.yaml"

requestDescribeRiskConfiguration :: DescribeRiskConfiguration -> TestTree
requestDescribeRiskConfiguration =
  req
    "DescribeRiskConfiguration"
    "fixture/DescribeRiskConfiguration.yaml"

requestDescribeUserImportJob :: DescribeUserImportJob -> TestTree
requestDescribeUserImportJob =
  req
    "DescribeUserImportJob"
    "fixture/DescribeUserImportJob.yaml"

requestSetUserSettings :: SetUserSettings -> TestTree
requestSetUserSettings =
  req
    "SetUserSettings"
    "fixture/SetUserSettings.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestAdminRespondToAuthChallenge :: AdminRespondToAuthChallenge -> TestTree
requestAdminRespondToAuthChallenge =
  req
    "AdminRespondToAuthChallenge"
    "fixture/AdminRespondToAuthChallenge.yaml"

requestUpdateResourceServer :: UpdateResourceServer -> TestTree
requestUpdateResourceServer =
  req
    "UpdateResourceServer"
    "fixture/UpdateResourceServer.yaml"

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

requestSignUp :: SignUp -> TestTree
requestSignUp =
  req
    "SignUp"
    "fixture/SignUp.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

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

requestAdminDeleteUserAttributes :: AdminDeleteUserAttributes -> TestTree
requestAdminDeleteUserAttributes =
  req
    "AdminDeleteUserAttributes"
    "fixture/AdminDeleteUserAttributes.yaml"

requestAdminDisableProviderForUser :: AdminDisableProviderForUser -> TestTree
requestAdminDisableProviderForUser =
  req
    "AdminDisableProviderForUser"
    "fixture/AdminDisableProviderForUser.yaml"

requestAdminUpdateUserAttributes :: AdminUpdateUserAttributes -> TestTree
requestAdminUpdateUserAttributes =
  req
    "AdminUpdateUserAttributes"
    "fixture/AdminUpdateUserAttributes.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestCreateUserPoolClient :: CreateUserPoolClient -> TestTree
requestCreateUserPoolClient =
  req
    "CreateUserPoolClient"
    "fixture/CreateUserPoolClient.yaml"

requestAddCustomAttributes :: AddCustomAttributes -> TestTree
requestAddCustomAttributes =
  req
    "AddCustomAttributes"
    "fixture/AddCustomAttributes.yaml"

requestListUserPoolClients :: ListUserPoolClients -> TestTree
requestListUserPoolClients =
  req
    "ListUserPoolClients"
    "fixture/ListUserPoolClients.yaml"

requestAdminSetUserMFAPreference :: AdminSetUserMFAPreference -> TestTree
requestAdminSetUserMFAPreference =
  req
    "AdminSetUserMFAPreference"
    "fixture/AdminSetUserMFAPreference.yaml"

requestDescribeResourceServer :: DescribeResourceServer -> TestTree
requestDescribeResourceServer =
  req
    "DescribeResourceServer"
    "fixture/DescribeResourceServer.yaml"

requestSetUserMFAPreference :: SetUserMFAPreference -> TestTree
requestSetUserMFAPreference =
  req
    "SetUserMFAPreference"
    "fixture/SetUserMFAPreference.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestConfirmDevice :: ConfirmDevice -> TestTree
requestConfirmDevice =
  req
    "ConfirmDevice"
    "fixture/ConfirmDevice.yaml"

requestConfirmForgotPassword :: ConfirmForgotPassword -> TestTree
requestConfirmForgotPassword =
  req
    "ConfirmForgotPassword"
    "fixture/ConfirmForgotPassword.yaml"

requestVerifyUserAttribute :: VerifyUserAttribute -> TestTree
requestVerifyUserAttribute =
  req
    "VerifyUserAttribute"
    "fixture/VerifyUserAttribute.yaml"

-- Responses

responseGetUserAttributeVerificationCode :: GetUserAttributeVerificationCodeResponse -> TestTree
responseGetUserAttributeVerificationCode =
  res
    "GetUserAttributeVerificationCodeResponse"
    "fixture/GetUserAttributeVerificationCodeResponse.proto"
    defaultService
    (Proxy :: Proxy GetUserAttributeVerificationCode)

responseCreateUserImportJob :: CreateUserImportJobResponse -> TestTree
responseCreateUserImportJob =
  res
    "CreateUserImportJobResponse"
    "fixture/CreateUserImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserImportJob)

responseSetUserPoolMfaConfig :: SetUserPoolMfaConfigResponse -> TestTree
responseSetUserPoolMfaConfig =
  res
    "SetUserPoolMfaConfigResponse"
    "fixture/SetUserPoolMfaConfigResponse.proto"
    defaultService
    (Proxy :: Proxy SetUserPoolMfaConfig)

responseAdminDeleteUser :: AdminDeleteUserResponse -> TestTree
responseAdminDeleteUser =
  res
    "AdminDeleteUserResponse"
    "fixture/AdminDeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminDeleteUser)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy :: Proxy GetUser)

responseDeleteUserAttributes :: DeleteUserAttributesResponse -> TestTree
responseDeleteUserAttributes =
  res
    "DeleteUserAttributesResponse"
    "fixture/DeleteUserAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserAttributes)

responseUpdateUserAttributes :: UpdateUserAttributesResponse -> TestTree
responseUpdateUserAttributes =
  res
    "UpdateUserAttributesResponse"
    "fixture/UpdateUserAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserAttributes)

responseDeleteUserPoolDomain :: DeleteUserPoolDomainResponse -> TestTree
responseDeleteUserPoolDomain =
  res
    "DeleteUserPoolDomainResponse"
    "fixture/DeleteUserPoolDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPoolDomain)

responseListUserPools :: ListUserPoolsResponse -> TestTree
responseListUserPools =
  res
    "ListUserPoolsResponse"
    "fixture/ListUserPoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserPools)

responseDeleteUserPool :: DeleteUserPoolResponse -> TestTree
responseDeleteUserPool =
  res
    "DeleteUserPoolResponse"
    "fixture/DeleteUserPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPool)

responseUpdateAuthEventFeedback :: UpdateAuthEventFeedbackResponse -> TestTree
responseUpdateAuthEventFeedback =
  res
    "UpdateAuthEventFeedbackResponse"
    "fixture/UpdateAuthEventFeedbackResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAuthEventFeedback)

responseUpdateUserPool :: UpdateUserPoolResponse -> TestTree
responseUpdateUserPool =
  res
    "UpdateUserPoolResponse"
    "fixture/UpdateUserPoolResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserPool)

responseConfirmSignUp :: ConfirmSignUpResponse -> TestTree
responseConfirmSignUp =
  res
    "ConfirmSignUpResponse"
    "fixture/ConfirmSignUpResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmSignUp)

responseUpdateUserPoolDomain :: UpdateUserPoolDomainResponse -> TestTree
responseUpdateUserPoolDomain =
  res
    "UpdateUserPoolDomainResponse"
    "fixture/UpdateUserPoolDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserPoolDomain)

responseAdminLinkProviderForUser :: AdminLinkProviderForUserResponse -> TestTree
responseAdminLinkProviderForUser =
  res
    "AdminLinkProviderForUserResponse"
    "fixture/AdminLinkProviderForUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminLinkProviderForUser)

responseSetRiskConfiguration :: SetRiskConfigurationResponse -> TestTree
responseSetRiskConfiguration =
  res
    "SetRiskConfigurationResponse"
    "fixture/SetRiskConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy SetRiskConfiguration)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevices)

responseCreateUserPool :: CreateUserPoolResponse -> TestTree
responseCreateUserPool =
  res
    "CreateUserPoolResponse"
    "fixture/CreateUserPoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserPool)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword =
  res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ChangePassword)

responseAdminGetDevice :: AdminGetDeviceResponse -> TestTree
responseAdminGetDevice =
  res
    "AdminGetDeviceResponse"
    "fixture/AdminGetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy AdminGetDevice)

responseRespondToAuthChallenge :: RespondToAuthChallengeResponse -> TestTree
responseRespondToAuthChallenge =
  res
    "RespondToAuthChallengeResponse"
    "fixture/RespondToAuthChallengeResponse.proto"
    defaultService
    (Proxy :: Proxy RespondToAuthChallenge)

responseAdminRemoveUserFromGroup :: AdminRemoveUserFromGroupResponse -> TestTree
responseAdminRemoveUserFromGroup =
  res
    "AdminRemoveUserFromGroupResponse"
    "fixture/AdminRemoveUserFromGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AdminRemoveUserFromGroup)

responseCreateUserPoolDomain :: CreateUserPoolDomainResponse -> TestTree
responseCreateUserPoolDomain =
  res
    "CreateUserPoolDomainResponse"
    "fixture/CreateUserPoolDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserPoolDomain)

responseListIdentityProviders :: ListIdentityProvidersResponse -> TestTree
responseListIdentityProviders =
  res
    "ListIdentityProvidersResponse"
    "fixture/ListIdentityProvidersResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentityProviders)

responseDeleteIdentityProvider :: DeleteIdentityProviderResponse -> TestTree
responseDeleteIdentityProvider =
  res
    "DeleteIdentityProviderResponse"
    "fixture/DeleteIdentityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIdentityProvider)

responseUpdateIdentityProvider :: UpdateIdentityProviderResponse -> TestTree
responseUpdateIdentityProvider =
  res
    "UpdateIdentityProviderResponse"
    "fixture/UpdateIdentityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIdentityProvider)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroups)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevice)

responseAdminListDevices :: AdminListDevicesResponse -> TestTree
responseAdminListDevices =
  res
    "AdminListDevicesResponse"
    "fixture/AdminListDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy AdminListDevices)

responseRevokeToken :: RevokeTokenResponse -> TestTree
responseRevokeToken =
  res
    "RevokeTokenResponse"
    "fixture/RevokeTokenResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeToken)

responseAdminSetUserPassword :: AdminSetUserPasswordResponse -> TestTree
responseAdminSetUserPassword =
  res
    "AdminSetUserPasswordResponse"
    "fixture/AdminSetUserPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy AdminSetUserPassword)

responseStartUserImportJob :: StartUserImportJobResponse -> TestTree
responseStartUserImportJob =
  res
    "StartUserImportJobResponse"
    "fixture/StartUserImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartUserImportJob)

responseAdminConfirmSignUp :: AdminConfirmSignUpResponse -> TestTree
responseAdminConfirmSignUp =
  res
    "AdminConfirmSignUpResponse"
    "fixture/AdminConfirmSignUpResponse.proto"
    defaultService
    (Proxy :: Proxy AdminConfirmSignUp)

responseAdminListUserAuthEvents :: AdminListUserAuthEventsResponse -> TestTree
responseAdminListUserAuthEvents =
  res
    "AdminListUserAuthEventsResponse"
    "fixture/AdminListUserAuthEventsResponse.proto"
    defaultService
    (Proxy :: Proxy AdminListUserAuthEvents)

responseStopUserImportJob :: StopUserImportJobResponse -> TestTree
responseStopUserImportJob =
  res
    "StopUserImportJobResponse"
    "fixture/StopUserImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopUserImportJob)

responseInitiateAuth :: InitiateAuthResponse -> TestTree
responseInitiateAuth =
  res
    "InitiateAuthResponse"
    "fixture/InitiateAuthResponse.proto"
    defaultService
    (Proxy :: Proxy InitiateAuth)

responseVerifySoftwareToken :: VerifySoftwareTokenResponse -> TestTree
responseVerifySoftwareToken =
  res
    "VerifySoftwareTokenResponse"
    "fixture/VerifySoftwareTokenResponse.proto"
    defaultService
    (Proxy :: Proxy VerifySoftwareToken)

responseCreateIdentityProvider :: CreateIdentityProviderResponse -> TestTree
responseCreateIdentityProvider =
  res
    "CreateIdentityProviderResponse"
    "fixture/CreateIdentityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIdentityProvider)

responseAdminListGroupsForUser :: AdminListGroupsForUserResponse -> TestTree
responseAdminListGroupsForUser =
  res
    "AdminListGroupsForUserResponse"
    "fixture/AdminListGroupsForUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminListGroupsForUser)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroup)

responseAdminAddUserToGroup :: AdminAddUserToGroupResponse -> TestTree
responseAdminAddUserToGroup =
  res
    "AdminAddUserToGroupResponse"
    "fixture/AdminAddUserToGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AdminAddUserToGroup)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseSetUICustomization :: SetUICustomizationResponse -> TestTree
responseSetUICustomization =
  res
    "SetUICustomizationResponse"
    "fixture/SetUICustomizationResponse.proto"
    defaultService
    (Proxy :: Proxy SetUICustomization)

responseAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedbackResponse -> TestTree
responseAdminUpdateAuthEventFeedback =
  res
    "AdminUpdateAuthEventFeedbackResponse"
    "fixture/AdminUpdateAuthEventFeedbackResponse.proto"
    defaultService
    (Proxy :: Proxy AdminUpdateAuthEventFeedback)

responseDescribeUserPool :: DescribeUserPoolResponse -> TestTree
responseDescribeUserPool =
  res
    "DescribeUserPoolResponse"
    "fixture/DescribeUserPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserPool)

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

responseAdminUserGlobalSignOut :: AdminUserGlobalSignOutResponse -> TestTree
responseAdminUserGlobalSignOut =
  res
    "AdminUserGlobalSignOutResponse"
    "fixture/AdminUserGlobalSignOutResponse.proto"
    defaultService
    (Proxy :: Proxy AdminUserGlobalSignOut)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseAdminGetUser :: AdminGetUserResponse -> TestTree
responseAdminGetUser =
  res
    "AdminGetUserResponse"
    "fixture/AdminGetUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminGetUser)

responseListUsersInGroup :: ListUsersInGroupResponse -> TestTree
responseListUsersInGroup =
  res
    "ListUsersInGroupResponse"
    "fixture/ListUsersInGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsersInGroup)

responseGetSigningCertificate :: GetSigningCertificateResponse -> TestTree
responseGetSigningCertificate =
  res
    "GetSigningCertificateResponse"
    "fixture/GetSigningCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy GetSigningCertificate)

responseDescribeUserPoolDomain :: DescribeUserPoolDomainResponse -> TestTree
responseDescribeUserPoolDomain =
  res
    "DescribeUserPoolDomainResponse"
    "fixture/DescribeUserPoolDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserPoolDomain)

responseUpdateUserPoolClient :: UpdateUserPoolClientResponse -> TestTree
responseUpdateUserPoolClient =
  res
    "UpdateUserPoolClientResponse"
    "fixture/UpdateUserPoolClientResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserPoolClient)

responseUpdateDeviceStatus :: UpdateDeviceStatusResponse -> TestTree
responseUpdateDeviceStatus =
  res
    "UpdateDeviceStatusResponse"
    "fixture/UpdateDeviceStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeviceStatus)

responseForgetDevice :: ForgetDeviceResponse -> TestTree
responseForgetDevice =
  res
    "ForgetDeviceResponse"
    "fixture/ForgetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy ForgetDevice)

responseDeleteUserPoolClient :: DeleteUserPoolClientResponse -> TestTree
responseDeleteUserPoolClient =
  res
    "DeleteUserPoolClientResponse"
    "fixture/DeleteUserPoolClientResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserPoolClient)

responseAdminUpdateDeviceStatus :: AdminUpdateDeviceStatusResponse -> TestTree
responseAdminUpdateDeviceStatus =
  res
    "AdminUpdateDeviceStatusResponse"
    "fixture/AdminUpdateDeviceStatusResponse.proto"
    defaultService
    (Proxy :: Proxy AdminUpdateDeviceStatus)

responseDescribeIdentityProvider :: DescribeIdentityProviderResponse -> TestTree
responseDescribeIdentityProvider =
  res
    "DescribeIdentityProviderResponse"
    "fixture/DescribeIdentityProviderResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentityProvider)

responseAdminForgetDevice :: AdminForgetDeviceResponse -> TestTree
responseAdminForgetDevice =
  res
    "AdminForgetDeviceResponse"
    "fixture/AdminForgetDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy AdminForgetDevice)

responseListUserImportJobs :: ListUserImportJobsResponse -> TestTree
responseListUserImportJobs =
  res
    "ListUserImportJobsResponse"
    "fixture/ListUserImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserImportJobs)

responseAdminCreateUser :: AdminCreateUserResponse -> TestTree
responseAdminCreateUser =
  res
    "AdminCreateUserResponse"
    "fixture/AdminCreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminCreateUser)

responseAdminDisableUser :: AdminDisableUserResponse -> TestTree
responseAdminDisableUser =
  res
    "AdminDisableUserResponse"
    "fixture/AdminDisableUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminDisableUser)

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

responseAdminResetUserPassword :: AdminResetUserPasswordResponse -> TestTree
responseAdminResetUserPassword =
  res
    "AdminResetUserPasswordResponse"
    "fixture/AdminResetUserPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy AdminResetUserPassword)

responseAdminInitiateAuth :: AdminInitiateAuthResponse -> TestTree
responseAdminInitiateAuth =
  res
    "AdminInitiateAuthResponse"
    "fixture/AdminInitiateAuthResponse.proto"
    defaultService
    (Proxy :: Proxy AdminInitiateAuth)

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

responseResendConfirmationCode :: ResendConfirmationCodeResponse -> TestTree
responseResendConfirmationCode =
  res
    "ResendConfirmationCodeResponse"
    "fixture/ResendConfirmationCodeResponse.proto"
    defaultService
    (Proxy :: Proxy ResendConfirmationCode)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroup)

responseGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifierResponse -> TestTree
responseGetIdentityProviderByIdentifier =
  res
    "GetIdentityProviderByIdentifierResponse"
    "fixture/GetIdentityProviderByIdentifierResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityProviderByIdentifier)

responseDescribeRiskConfiguration :: DescribeRiskConfigurationResponse -> TestTree
responseDescribeRiskConfiguration =
  res
    "DescribeRiskConfigurationResponse"
    "fixture/DescribeRiskConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRiskConfiguration)

responseDescribeUserImportJob :: DescribeUserImportJobResponse -> TestTree
responseDescribeUserImportJob =
  res
    "DescribeUserImportJobResponse"
    "fixture/DescribeUserImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserImportJob)

responseSetUserSettings :: SetUserSettingsResponse -> TestTree
responseSetUserSettings =
  res
    "SetUserSettingsResponse"
    "fixture/SetUserSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy SetUserSettings)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseAdminRespondToAuthChallenge :: AdminRespondToAuthChallengeResponse -> TestTree
responseAdminRespondToAuthChallenge =
  res
    "AdminRespondToAuthChallengeResponse"
    "fixture/AdminRespondToAuthChallengeResponse.proto"
    defaultService
    (Proxy :: Proxy AdminRespondToAuthChallenge)

responseUpdateResourceServer :: UpdateResourceServerResponse -> TestTree
responseUpdateResourceServer =
  res
    "UpdateResourceServerResponse"
    "fixture/UpdateResourceServerResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResourceServer)

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

responseSignUp :: SignUpResponse -> TestTree
responseSignUp =
  res
    "SignUpResponse"
    "fixture/SignUpResponse.proto"
    defaultService
    (Proxy :: Proxy SignUp)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroup)

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

responseAdminDeleteUserAttributes :: AdminDeleteUserAttributesResponse -> TestTree
responseAdminDeleteUserAttributes =
  res
    "AdminDeleteUserAttributesResponse"
    "fixture/AdminDeleteUserAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy AdminDeleteUserAttributes)

responseAdminDisableProviderForUser :: AdminDisableProviderForUserResponse -> TestTree
responseAdminDisableProviderForUser =
  res
    "AdminDisableProviderForUserResponse"
    "fixture/AdminDisableProviderForUserResponse.proto"
    defaultService
    (Proxy :: Proxy AdminDisableProviderForUser)

responseAdminUpdateUserAttributes :: AdminUpdateUserAttributesResponse -> TestTree
responseAdminUpdateUserAttributes =
  res
    "AdminUpdateUserAttributesResponse"
    "fixture/AdminUpdateUserAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy AdminUpdateUserAttributes)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseCreateUserPoolClient :: CreateUserPoolClientResponse -> TestTree
responseCreateUserPoolClient =
  res
    "CreateUserPoolClientResponse"
    "fixture/CreateUserPoolClientResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserPoolClient)

responseAddCustomAttributes :: AddCustomAttributesResponse -> TestTree
responseAddCustomAttributes =
  res
    "AddCustomAttributesResponse"
    "fixture/AddCustomAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy AddCustomAttributes)

responseListUserPoolClients :: ListUserPoolClientsResponse -> TestTree
responseListUserPoolClients =
  res
    "ListUserPoolClientsResponse"
    "fixture/ListUserPoolClientsResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserPoolClients)

responseAdminSetUserMFAPreference :: AdminSetUserMFAPreferenceResponse -> TestTree
responseAdminSetUserMFAPreference =
  res
    "AdminSetUserMFAPreferenceResponse"
    "fixture/AdminSetUserMFAPreferenceResponse.proto"
    defaultService
    (Proxy :: Proxy AdminSetUserMFAPreference)

responseDescribeResourceServer :: DescribeResourceServerResponse -> TestTree
responseDescribeResourceServer =
  res
    "DescribeResourceServerResponse"
    "fixture/DescribeResourceServerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResourceServer)

responseSetUserMFAPreference :: SetUserMFAPreferenceResponse -> TestTree
responseSetUserMFAPreference =
  res
    "SetUserMFAPreferenceResponse"
    "fixture/SetUserMFAPreferenceResponse.proto"
    defaultService
    (Proxy :: Proxy SetUserMFAPreference)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseConfirmDevice :: ConfirmDeviceResponse -> TestTree
responseConfirmDevice =
  res
    "ConfirmDeviceResponse"
    "fixture/ConfirmDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmDevice)

responseConfirmForgotPassword :: ConfirmForgotPasswordResponse -> TestTree
responseConfirmForgotPassword =
  res
    "ConfirmForgotPasswordResponse"
    "fixture/ConfirmForgotPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmForgotPassword)

responseVerifyUserAttribute :: VerifyUserAttributeResponse -> TestTree
responseVerifyUserAttribute =
  res
    "VerifyUserAttributeResponse"
    "fixture/VerifyUserAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy VerifyUserAttribute)
