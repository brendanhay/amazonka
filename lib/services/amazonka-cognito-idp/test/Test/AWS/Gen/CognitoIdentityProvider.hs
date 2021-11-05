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

import qualified Data.Proxy as Proxy
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
--         [ requestDeleteUserPool $
--             newDeleteUserPool
--
--         , requestUpdateUserPool $
--             newUpdateUserPool
--
--         , requestUpdateUserPoolDomain $
--             newUpdateUserPoolDomain
--
--         , requestDeleteUserPoolDomain $
--             newDeleteUserPoolDomain
--
--         , requestAdminInitiateAuth $
--             newAdminInitiateAuth
--
--         , requestAdminLinkProviderForUser $
--             newAdminLinkProviderForUser
--
--         , requestAdminEnableUser $
--             newAdminEnableUser
--
--         , requestGetUserAttributeVerificationCode $
--             newGetUserAttributeVerificationCode
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
--         , requestVerifyUserAttribute $
--             newVerifyUserAttribute
--
--         , requestAdminDisableUser $
--             newAdminDisableUser
--
--         , requestConfirmDevice $
--             newConfirmDevice
--
--         , requestConfirmForgotPassword $
--             newConfirmForgotPassword
--
--         , requestListUserImportJobs $
--             newListUserImportJobs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeIdentityProvider $
--             newDescribeIdentityProvider
--
--         , requestListUsers $
--             newListUsers
--
--         , requestAdminDeleteUserAttributes $
--             newAdminDeleteUserAttributes
--
--         , requestDescribeUserPoolDomain $
--             newDescribeUserPoolDomain
--
--         , requestAdminUpdateUserAttributes $
--             newAdminUpdateUserAttributes
--
--         , requestAdminGetUser $
--             newAdminGetUser
--
--         , requestAdminUserGlobalSignOut $
--             newAdminUserGlobalSignOut
--
--         , requestListUsersInGroup $
--             newListUsersInGroup
--
--         , requestAssociateSoftwareToken $
--             newAssociateSoftwareToken
--
--         , requestAdminDisableProviderForUser $
--             newAdminDisableProviderForUser
--
--         , requestForgotPassword $
--             newForgotPassword
--
--         , requestDescribeUserPool $
--             newDescribeUserPool
--
--         , requestInitiateAuth $
--             newInitiateAuth
--
--         , requestAdminListGroupsForUser $
--             newAdminListGroupsForUser
--
--         , requestAdminConfirmSignUp $
--             newAdminConfirmSignUp
--
--         , requestAdminUpdateAuthEventFeedback $
--             newAdminUpdateAuthEventFeedback
--
--         , requestAdminSetUserPassword $
--             newAdminSetUserPassword
--
--         , requestStartUserImportJob $
--             newStartUserImportJob
--
--         , requestCreateIdentityProvider $
--             newCreateIdentityProvider
--
--         , requestSetUICustomization $
--             newSetUICustomization
--
--         , requestListIdentityProviders $
--             newListIdentityProviders
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestSignUp $
--             newSignUp
--
--         , requestDeleteResourceServer $
--             newDeleteResourceServer
--
--         , requestUpdateResourceServer $
--             newUpdateResourceServer
--
--         , requestChangePassword $
--             newChangePassword
--
--         , requestCreateUserPoolDomain $
--             newCreateUserPoolDomain
--
--         , requestRespondToAuthChallenge $
--             newRespondToAuthChallenge
--
--         , requestCreateUserPool $
--             newCreateUserPool
--
--         , requestAdminGetDevice $
--             newAdminGetDevice
--
--         , requestGetIdentityProviderByIdentifier $
--             newGetIdentityProviderByIdentifier
--
--         , requestAdminRemoveUserFromGroup $
--             newAdminRemoveUserFromGroup
--
--         , requestSetRiskConfiguration $
--             newSetRiskConfiguration
--
--         , requestConfirmSignUp $
--             newConfirmSignUp
--
--         , requestListUserPools $
--             newListUserPools
--
--         , requestAdminResetUserPassword $
--             newAdminResetUserPassword
--
--         , requestUpdateAuthEventFeedback $
--             newUpdateAuthEventFeedback
--
--         , requestCreateUserImportJob $
--             newCreateUserImportJob
--
--         , requestGetUser $
--             newGetUser
--
--         , requestGetUICustomization $
--             newGetUICustomization
--
--         , requestGetCSVHeader $
--             newGetCSVHeader
--
--         , requestAdminDeleteUser $
--             newAdminDeleteUser
--
--         , requestAdminForgetDevice $
--             newAdminForgetDevice
--
--         , requestDescribeResourceServer $
--             newDescribeResourceServer
--
--         , requestSetUserMFAPreference $
--             newSetUserMFAPreference
--
--         , requestAdminUpdateDeviceStatus $
--             newAdminUpdateDeviceStatus
--
--         , requestAdminCreateUser $
--             newAdminCreateUser
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
--         , requestUpdateUserPoolClient $
--             newUpdateUserPoolClient
--
--         , requestDeleteUserPoolClient $
--             newDeleteUserPoolClient
--
--         , requestUpdateDeviceStatus $
--             newUpdateDeviceStatus
--
--         , requestForgetDevice $
--             newForgetDevice
--
--         , requestGetSigningCertificate $
--             newGetSigningCertificate
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateUserPoolClient $
--             newCreateUserPoolClient
--
--         , requestGetUserPoolMfaConfig $
--             newGetUserPoolMfaConfig
--
--         , requestCreateResourceServer $
--             newCreateResourceServer
--
--         , requestAdminListUserAuthEvents $
--             newAdminListUserAuthEvents
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestAdminAddUserToGroup $
--             newAdminAddUserToGroup
--
--         , requestVerifySoftwareToken $
--             newVerifySoftwareToken
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestRevokeToken $
--             newRevokeToken
--
--         , requestStopUserImportJob $
--             newStopUserImportJob
--
--         , requestDescribeUserImportJob $
--             newDescribeUserImportJob
--
--         , requestDescribeRiskConfiguration $
--             newDescribeRiskConfiguration
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestGlobalSignOut $
--             newGlobalSignOut
--
--         , requestListGroups $
--             newListGroups
--
--         , requestUpdateIdentityProvider $
--             newUpdateIdentityProvider
--
--         , requestDeleteIdentityProvider $
--             newDeleteIdentityProvider
--
--         , requestListResourceServers $
--             newListResourceServers
--
--         , requestAdminRespondToAuthChallenge $
--             newAdminRespondToAuthChallenge
--
--         , requestSetUserSettings $
--             newSetUserSettings
--
--         , requestAdminListDevices $
--             newAdminListDevices
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
--         , requestAdminSetUserSettings $
--             newAdminSetUserSettings
--
--         , requestListDevices $
--             newListDevices
--
--           ]

--     , testGroup "response"
--         [ responseDeleteUserPool $
--             newDeleteUserPoolResponse
--
--         , responseUpdateUserPool $
--             newUpdateUserPoolResponse
--
--         , responseUpdateUserPoolDomain $
--             newUpdateUserPoolDomainResponse
--
--         , responseDeleteUserPoolDomain $
--             newDeleteUserPoolDomainResponse
--
--         , responseAdminInitiateAuth $
--             newAdminInitiateAuthResponse
--
--         , responseAdminLinkProviderForUser $
--             newAdminLinkProviderForUserResponse
--
--         , responseAdminEnableUser $
--             newAdminEnableUserResponse
--
--         , responseGetUserAttributeVerificationCode $
--             newGetUserAttributeVerificationCodeResponse
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
--         , responseVerifyUserAttribute $
--             newVerifyUserAttributeResponse
--
--         , responseAdminDisableUser $
--             newAdminDisableUserResponse
--
--         , responseConfirmDevice $
--             newConfirmDeviceResponse
--
--         , responseConfirmForgotPassword $
--             newConfirmForgotPasswordResponse
--
--         , responseListUserImportJobs $
--             newListUserImportJobsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeIdentityProvider $
--             newDescribeIdentityProviderResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseAdminDeleteUserAttributes $
--             newAdminDeleteUserAttributesResponse
--
--         , responseDescribeUserPoolDomain $
--             newDescribeUserPoolDomainResponse
--
--         , responseAdminUpdateUserAttributes $
--             newAdminUpdateUserAttributesResponse
--
--         , responseAdminGetUser $
--             newAdminGetUserResponse
--
--         , responseAdminUserGlobalSignOut $
--             newAdminUserGlobalSignOutResponse
--
--         , responseListUsersInGroup $
--             newListUsersInGroupResponse
--
--         , responseAssociateSoftwareToken $
--             newAssociateSoftwareTokenResponse
--
--         , responseAdminDisableProviderForUser $
--             newAdminDisableProviderForUserResponse
--
--         , responseForgotPassword $
--             newForgotPasswordResponse
--
--         , responseDescribeUserPool $
--             newDescribeUserPoolResponse
--
--         , responseInitiateAuth $
--             newInitiateAuthResponse
--
--         , responseAdminListGroupsForUser $
--             newAdminListGroupsForUserResponse
--
--         , responseAdminConfirmSignUp $
--             newAdminConfirmSignUpResponse
--
--         , responseAdminUpdateAuthEventFeedback $
--             newAdminUpdateAuthEventFeedbackResponse
--
--         , responseAdminSetUserPassword $
--             newAdminSetUserPasswordResponse
--
--         , responseStartUserImportJob $
--             newStartUserImportJobResponse
--
--         , responseCreateIdentityProvider $
--             newCreateIdentityProviderResponse
--
--         , responseSetUICustomization $
--             newSetUICustomizationResponse
--
--         , responseListIdentityProviders $
--             newListIdentityProvidersResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseSignUp $
--             newSignUpResponse
--
--         , responseDeleteResourceServer $
--             newDeleteResourceServerResponse
--
--         , responseUpdateResourceServer $
--             newUpdateResourceServerResponse
--
--         , responseChangePassword $
--             newChangePasswordResponse
--
--         , responseCreateUserPoolDomain $
--             newCreateUserPoolDomainResponse
--
--         , responseRespondToAuthChallenge $
--             newRespondToAuthChallengeResponse
--
--         , responseCreateUserPool $
--             newCreateUserPoolResponse
--
--         , responseAdminGetDevice $
--             newAdminGetDeviceResponse
--
--         , responseGetIdentityProviderByIdentifier $
--             newGetIdentityProviderByIdentifierResponse
--
--         , responseAdminRemoveUserFromGroup $
--             newAdminRemoveUserFromGroupResponse
--
--         , responseSetRiskConfiguration $
--             newSetRiskConfigurationResponse
--
--         , responseConfirmSignUp $
--             newConfirmSignUpResponse
--
--         , responseListUserPools $
--             newListUserPoolsResponse
--
--         , responseAdminResetUserPassword $
--             newAdminResetUserPasswordResponse
--
--         , responseUpdateAuthEventFeedback $
--             newUpdateAuthEventFeedbackResponse
--
--         , responseCreateUserImportJob $
--             newCreateUserImportJobResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseGetUICustomization $
--             newGetUICustomizationResponse
--
--         , responseGetCSVHeader $
--             newGetCSVHeaderResponse
--
--         , responseAdminDeleteUser $
--             newAdminDeleteUserResponse
--
--         , responseAdminForgetDevice $
--             newAdminForgetDeviceResponse
--
--         , responseDescribeResourceServer $
--             newDescribeResourceServerResponse
--
--         , responseSetUserMFAPreference $
--             newSetUserMFAPreferenceResponse
--
--         , responseAdminUpdateDeviceStatus $
--             newAdminUpdateDeviceStatusResponse
--
--         , responseAdminCreateUser $
--             newAdminCreateUserResponse
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
--         , responseUpdateUserPoolClient $
--             newUpdateUserPoolClientResponse
--
--         , responseDeleteUserPoolClient $
--             newDeleteUserPoolClientResponse
--
--         , responseUpdateDeviceStatus $
--             newUpdateDeviceStatusResponse
--
--         , responseForgetDevice $
--             newForgetDeviceResponse
--
--         , responseGetSigningCertificate $
--             newGetSigningCertificateResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateUserPoolClient $
--             newCreateUserPoolClientResponse
--
--         , responseGetUserPoolMfaConfig $
--             newGetUserPoolMfaConfigResponse
--
--         , responseCreateResourceServer $
--             newCreateResourceServerResponse
--
--         , responseAdminListUserAuthEvents $
--             newAdminListUserAuthEventsResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseAdminAddUserToGroup $
--             newAdminAddUserToGroupResponse
--
--         , responseVerifySoftwareToken $
--             newVerifySoftwareTokenResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseRevokeToken $
--             newRevokeTokenResponse
--
--         , responseStopUserImportJob $
--             newStopUserImportJobResponse
--
--         , responseDescribeUserImportJob $
--             newDescribeUserImportJobResponse
--
--         , responseDescribeRiskConfiguration $
--             newDescribeRiskConfigurationResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseGlobalSignOut $
--             newGlobalSignOutResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseUpdateIdentityProvider $
--             newUpdateIdentityProviderResponse
--
--         , responseDeleteIdentityProvider $
--             newDeleteIdentityProviderResponse
--
--         , responseListResourceServers $
--             newListResourceServersResponse
--
--         , responseAdminRespondToAuthChallenge $
--             newAdminRespondToAuthChallengeResponse
--
--         , responseSetUserSettings $
--             newSetUserSettingsResponse
--
--         , responseAdminListDevices $
--             newAdminListDevicesResponse
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
--         , responseAdminSetUserSettings $
--             newAdminSetUserSettingsResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--           ]
--     ]

-- Requests

requestDeleteUserPool :: DeleteUserPool -> TestTree
requestDeleteUserPool =
  req
    "DeleteUserPool"
    "fixture/DeleteUserPool.yaml"

requestUpdateUserPool :: UpdateUserPool -> TestTree
requestUpdateUserPool =
  req
    "UpdateUserPool"
    "fixture/UpdateUserPool.yaml"

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

requestAdminInitiateAuth :: AdminInitiateAuth -> TestTree
requestAdminInitiateAuth =
  req
    "AdminInitiateAuth"
    "fixture/AdminInitiateAuth.yaml"

requestAdminLinkProviderForUser :: AdminLinkProviderForUser -> TestTree
requestAdminLinkProviderForUser =
  req
    "AdminLinkProviderForUser"
    "fixture/AdminLinkProviderForUser.yaml"

requestAdminEnableUser :: AdminEnableUser -> TestTree
requestAdminEnableUser =
  req
    "AdminEnableUser"
    "fixture/AdminEnableUser.yaml"

requestGetUserAttributeVerificationCode :: GetUserAttributeVerificationCode -> TestTree
requestGetUserAttributeVerificationCode =
  req
    "GetUserAttributeVerificationCode"
    "fixture/GetUserAttributeVerificationCode.yaml"

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

requestVerifyUserAttribute :: VerifyUserAttribute -> TestTree
requestVerifyUserAttribute =
  req
    "VerifyUserAttribute"
    "fixture/VerifyUserAttribute.yaml"

requestAdminDisableUser :: AdminDisableUser -> TestTree
requestAdminDisableUser =
  req
    "AdminDisableUser"
    "fixture/AdminDisableUser.yaml"

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

requestListUserImportJobs :: ListUserImportJobs -> TestTree
requestListUserImportJobs =
  req
    "ListUserImportJobs"
    "fixture/ListUserImportJobs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeIdentityProvider :: DescribeIdentityProvider -> TestTree
requestDescribeIdentityProvider =
  req
    "DescribeIdentityProvider"
    "fixture/DescribeIdentityProvider.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestAdminDeleteUserAttributes :: AdminDeleteUserAttributes -> TestTree
requestAdminDeleteUserAttributes =
  req
    "AdminDeleteUserAttributes"
    "fixture/AdminDeleteUserAttributes.yaml"

requestDescribeUserPoolDomain :: DescribeUserPoolDomain -> TestTree
requestDescribeUserPoolDomain =
  req
    "DescribeUserPoolDomain"
    "fixture/DescribeUserPoolDomain.yaml"

requestAdminUpdateUserAttributes :: AdminUpdateUserAttributes -> TestTree
requestAdminUpdateUserAttributes =
  req
    "AdminUpdateUserAttributes"
    "fixture/AdminUpdateUserAttributes.yaml"

requestAdminGetUser :: AdminGetUser -> TestTree
requestAdminGetUser =
  req
    "AdminGetUser"
    "fixture/AdminGetUser.yaml"

requestAdminUserGlobalSignOut :: AdminUserGlobalSignOut -> TestTree
requestAdminUserGlobalSignOut =
  req
    "AdminUserGlobalSignOut"
    "fixture/AdminUserGlobalSignOut.yaml"

requestListUsersInGroup :: ListUsersInGroup -> TestTree
requestListUsersInGroup =
  req
    "ListUsersInGroup"
    "fixture/ListUsersInGroup.yaml"

requestAssociateSoftwareToken :: AssociateSoftwareToken -> TestTree
requestAssociateSoftwareToken =
  req
    "AssociateSoftwareToken"
    "fixture/AssociateSoftwareToken.yaml"

requestAdminDisableProviderForUser :: AdminDisableProviderForUser -> TestTree
requestAdminDisableProviderForUser =
  req
    "AdminDisableProviderForUser"
    "fixture/AdminDisableProviderForUser.yaml"

requestForgotPassword :: ForgotPassword -> TestTree
requestForgotPassword =
  req
    "ForgotPassword"
    "fixture/ForgotPassword.yaml"

requestDescribeUserPool :: DescribeUserPool -> TestTree
requestDescribeUserPool =
  req
    "DescribeUserPool"
    "fixture/DescribeUserPool.yaml"

requestInitiateAuth :: InitiateAuth -> TestTree
requestInitiateAuth =
  req
    "InitiateAuth"
    "fixture/InitiateAuth.yaml"

requestAdminListGroupsForUser :: AdminListGroupsForUser -> TestTree
requestAdminListGroupsForUser =
  req
    "AdminListGroupsForUser"
    "fixture/AdminListGroupsForUser.yaml"

requestAdminConfirmSignUp :: AdminConfirmSignUp -> TestTree
requestAdminConfirmSignUp =
  req
    "AdminConfirmSignUp"
    "fixture/AdminConfirmSignUp.yaml"

requestAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedback -> TestTree
requestAdminUpdateAuthEventFeedback =
  req
    "AdminUpdateAuthEventFeedback"
    "fixture/AdminUpdateAuthEventFeedback.yaml"

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

requestCreateIdentityProvider :: CreateIdentityProvider -> TestTree
requestCreateIdentityProvider =
  req
    "CreateIdentityProvider"
    "fixture/CreateIdentityProvider.yaml"

requestSetUICustomization :: SetUICustomization -> TestTree
requestSetUICustomization =
  req
    "SetUICustomization"
    "fixture/SetUICustomization.yaml"

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

requestSignUp :: SignUp -> TestTree
requestSignUp =
  req
    "SignUp"
    "fixture/SignUp.yaml"

requestDeleteResourceServer :: DeleteResourceServer -> TestTree
requestDeleteResourceServer =
  req
    "DeleteResourceServer"
    "fixture/DeleteResourceServer.yaml"

requestUpdateResourceServer :: UpdateResourceServer -> TestTree
requestUpdateResourceServer =
  req
    "UpdateResourceServer"
    "fixture/UpdateResourceServer.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword =
  req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

requestCreateUserPoolDomain :: CreateUserPoolDomain -> TestTree
requestCreateUserPoolDomain =
  req
    "CreateUserPoolDomain"
    "fixture/CreateUserPoolDomain.yaml"

requestRespondToAuthChallenge :: RespondToAuthChallenge -> TestTree
requestRespondToAuthChallenge =
  req
    "RespondToAuthChallenge"
    "fixture/RespondToAuthChallenge.yaml"

requestCreateUserPool :: CreateUserPool -> TestTree
requestCreateUserPool =
  req
    "CreateUserPool"
    "fixture/CreateUserPool.yaml"

requestAdminGetDevice :: AdminGetDevice -> TestTree
requestAdminGetDevice =
  req
    "AdminGetDevice"
    "fixture/AdminGetDevice.yaml"

requestGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifier -> TestTree
requestGetIdentityProviderByIdentifier =
  req
    "GetIdentityProviderByIdentifier"
    "fixture/GetIdentityProviderByIdentifier.yaml"

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

requestConfirmSignUp :: ConfirmSignUp -> TestTree
requestConfirmSignUp =
  req
    "ConfirmSignUp"
    "fixture/ConfirmSignUp.yaml"

requestListUserPools :: ListUserPools -> TestTree
requestListUserPools =
  req
    "ListUserPools"
    "fixture/ListUserPools.yaml"

requestAdminResetUserPassword :: AdminResetUserPassword -> TestTree
requestAdminResetUserPassword =
  req
    "AdminResetUserPassword"
    "fixture/AdminResetUserPassword.yaml"

requestUpdateAuthEventFeedback :: UpdateAuthEventFeedback -> TestTree
requestUpdateAuthEventFeedback =
  req
    "UpdateAuthEventFeedback"
    "fixture/UpdateAuthEventFeedback.yaml"

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

requestAdminDeleteUser :: AdminDeleteUser -> TestTree
requestAdminDeleteUser =
  req
    "AdminDeleteUser"
    "fixture/AdminDeleteUser.yaml"

requestAdminForgetDevice :: AdminForgetDevice -> TestTree
requestAdminForgetDevice =
  req
    "AdminForgetDevice"
    "fixture/AdminForgetDevice.yaml"

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

requestAdminUpdateDeviceStatus :: AdminUpdateDeviceStatus -> TestTree
requestAdminUpdateDeviceStatus =
  req
    "AdminUpdateDeviceStatus"
    "fixture/AdminUpdateDeviceStatus.yaml"

requestAdminCreateUser :: AdminCreateUser -> TestTree
requestAdminCreateUser =
  req
    "AdminCreateUser"
    "fixture/AdminCreateUser.yaml"

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

requestUpdateUserPoolClient :: UpdateUserPoolClient -> TestTree
requestUpdateUserPoolClient =
  req
    "UpdateUserPoolClient"
    "fixture/UpdateUserPoolClient.yaml"

requestDeleteUserPoolClient :: DeleteUserPoolClient -> TestTree
requestDeleteUserPoolClient =
  req
    "DeleteUserPoolClient"
    "fixture/DeleteUserPoolClient.yaml"

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

requestGetSigningCertificate :: GetSigningCertificate -> TestTree
requestGetSigningCertificate =
  req
    "GetSigningCertificate"
    "fixture/GetSigningCertificate.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateUserPoolClient :: CreateUserPoolClient -> TestTree
requestCreateUserPoolClient =
  req
    "CreateUserPoolClient"
    "fixture/CreateUserPoolClient.yaml"

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

requestAdminListUserAuthEvents :: AdminListUserAuthEvents -> TestTree
requestAdminListUserAuthEvents =
  req
    "AdminListUserAuthEvents"
    "fixture/AdminListUserAuthEvents.yaml"

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

requestVerifySoftwareToken :: VerifySoftwareToken -> TestTree
requestVerifySoftwareToken =
  req
    "VerifySoftwareToken"
    "fixture/VerifySoftwareToken.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestRevokeToken :: RevokeToken -> TestTree
requestRevokeToken =
  req
    "RevokeToken"
    "fixture/RevokeToken.yaml"

requestStopUserImportJob :: StopUserImportJob -> TestTree
requestStopUserImportJob =
  req
    "StopUserImportJob"
    "fixture/StopUserImportJob.yaml"

requestDescribeUserImportJob :: DescribeUserImportJob -> TestTree
requestDescribeUserImportJob =
  req
    "DescribeUserImportJob"
    "fixture/DescribeUserImportJob.yaml"

requestDescribeRiskConfiguration :: DescribeRiskConfiguration -> TestTree
requestDescribeRiskConfiguration =
  req
    "DescribeRiskConfiguration"
    "fixture/DescribeRiskConfiguration.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

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

requestDeleteIdentityProvider :: DeleteIdentityProvider -> TestTree
requestDeleteIdentityProvider =
  req
    "DeleteIdentityProvider"
    "fixture/DeleteIdentityProvider.yaml"

requestListResourceServers :: ListResourceServers -> TestTree
requestListResourceServers =
  req
    "ListResourceServers"
    "fixture/ListResourceServers.yaml"

requestAdminRespondToAuthChallenge :: AdminRespondToAuthChallenge -> TestTree
requestAdminRespondToAuthChallenge =
  req
    "AdminRespondToAuthChallenge"
    "fixture/AdminRespondToAuthChallenge.yaml"

requestSetUserSettings :: SetUserSettings -> TestTree
requestSetUserSettings =
  req
    "SetUserSettings"
    "fixture/SetUserSettings.yaml"

requestAdminListDevices :: AdminListDevices -> TestTree
requestAdminListDevices =
  req
    "AdminListDevices"
    "fixture/AdminListDevices.yaml"

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

requestAdminSetUserSettings :: AdminSetUserSettings -> TestTree
requestAdminSetUserSettings =
  req
    "AdminSetUserSettings"
    "fixture/AdminSetUserSettings.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

-- Responses

responseDeleteUserPool :: DeleteUserPoolResponse -> TestTree
responseDeleteUserPool =
  res
    "DeleteUserPoolResponse"
    "fixture/DeleteUserPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserPool)

responseUpdateUserPool :: UpdateUserPoolResponse -> TestTree
responseUpdateUserPool =
  res
    "UpdateUserPoolResponse"
    "fixture/UpdateUserPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserPool)

responseUpdateUserPoolDomain :: UpdateUserPoolDomainResponse -> TestTree
responseUpdateUserPoolDomain =
  res
    "UpdateUserPoolDomainResponse"
    "fixture/UpdateUserPoolDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserPoolDomain)

responseDeleteUserPoolDomain :: DeleteUserPoolDomainResponse -> TestTree
responseDeleteUserPoolDomain =
  res
    "DeleteUserPoolDomainResponse"
    "fixture/DeleteUserPoolDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserPoolDomain)

responseAdminInitiateAuth :: AdminInitiateAuthResponse -> TestTree
responseAdminInitiateAuth =
  res
    "AdminInitiateAuthResponse"
    "fixture/AdminInitiateAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminInitiateAuth)

responseAdminLinkProviderForUser :: AdminLinkProviderForUserResponse -> TestTree
responseAdminLinkProviderForUser =
  res
    "AdminLinkProviderForUserResponse"
    "fixture/AdminLinkProviderForUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminLinkProviderForUser)

responseAdminEnableUser :: AdminEnableUserResponse -> TestTree
responseAdminEnableUser =
  res
    "AdminEnableUserResponse"
    "fixture/AdminEnableUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminEnableUser)

responseGetUserAttributeVerificationCode :: GetUserAttributeVerificationCodeResponse -> TestTree
responseGetUserAttributeVerificationCode =
  res
    "GetUserAttributeVerificationCodeResponse"
    "fixture/GetUserAttributeVerificationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserAttributeVerificationCode)

responseSetUserPoolMfaConfig :: SetUserPoolMfaConfigResponse -> TestTree
responseSetUserPoolMfaConfig =
  res
    "SetUserPoolMfaConfigResponse"
    "fixture/SetUserPoolMfaConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetUserPoolMfaConfig)

responseUpdateUserAttributes :: UpdateUserAttributesResponse -> TestTree
responseUpdateUserAttributes =
  res
    "UpdateUserAttributesResponse"
    "fixture/UpdateUserAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserAttributes)

responseDeleteUserAttributes :: DeleteUserAttributesResponse -> TestTree
responseDeleteUserAttributes =
  res
    "DeleteUserAttributesResponse"
    "fixture/DeleteUserAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserAttributes)

responseVerifyUserAttribute :: VerifyUserAttributeResponse -> TestTree
responseVerifyUserAttribute =
  res
    "VerifyUserAttributeResponse"
    "fixture/VerifyUserAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyUserAttribute)

responseAdminDisableUser :: AdminDisableUserResponse -> TestTree
responseAdminDisableUser =
  res
    "AdminDisableUserResponse"
    "fixture/AdminDisableUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminDisableUser)

responseConfirmDevice :: ConfirmDeviceResponse -> TestTree
responseConfirmDevice =
  res
    "ConfirmDeviceResponse"
    "fixture/ConfirmDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmDevice)

responseConfirmForgotPassword :: ConfirmForgotPasswordResponse -> TestTree
responseConfirmForgotPassword =
  res
    "ConfirmForgotPasswordResponse"
    "fixture/ConfirmForgotPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmForgotPassword)

responseListUserImportJobs :: ListUserImportJobsResponse -> TestTree
responseListUserImportJobs =
  res
    "ListUserImportJobsResponse"
    "fixture/ListUserImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserImportJobs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribeIdentityProvider :: DescribeIdentityProviderResponse -> TestTree
responseDescribeIdentityProvider =
  res
    "DescribeIdentityProviderResponse"
    "fixture/DescribeIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityProvider)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseAdminDeleteUserAttributes :: AdminDeleteUserAttributesResponse -> TestTree
responseAdminDeleteUserAttributes =
  res
    "AdminDeleteUserAttributesResponse"
    "fixture/AdminDeleteUserAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminDeleteUserAttributes)

responseDescribeUserPoolDomain :: DescribeUserPoolDomainResponse -> TestTree
responseDescribeUserPoolDomain =
  res
    "DescribeUserPoolDomainResponse"
    "fixture/DescribeUserPoolDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserPoolDomain)

responseAdminUpdateUserAttributes :: AdminUpdateUserAttributesResponse -> TestTree
responseAdminUpdateUserAttributes =
  res
    "AdminUpdateUserAttributesResponse"
    "fixture/AdminUpdateUserAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminUpdateUserAttributes)

responseAdminGetUser :: AdminGetUserResponse -> TestTree
responseAdminGetUser =
  res
    "AdminGetUserResponse"
    "fixture/AdminGetUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminGetUser)

responseAdminUserGlobalSignOut :: AdminUserGlobalSignOutResponse -> TestTree
responseAdminUserGlobalSignOut =
  res
    "AdminUserGlobalSignOutResponse"
    "fixture/AdminUserGlobalSignOutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminUserGlobalSignOut)

responseListUsersInGroup :: ListUsersInGroupResponse -> TestTree
responseListUsersInGroup =
  res
    "ListUsersInGroupResponse"
    "fixture/ListUsersInGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsersInGroup)

responseAssociateSoftwareToken :: AssociateSoftwareTokenResponse -> TestTree
responseAssociateSoftwareToken =
  res
    "AssociateSoftwareTokenResponse"
    "fixture/AssociateSoftwareTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSoftwareToken)

responseAdminDisableProviderForUser :: AdminDisableProviderForUserResponse -> TestTree
responseAdminDisableProviderForUser =
  res
    "AdminDisableProviderForUserResponse"
    "fixture/AdminDisableProviderForUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminDisableProviderForUser)

responseForgotPassword :: ForgotPasswordResponse -> TestTree
responseForgotPassword =
  res
    "ForgotPasswordResponse"
    "fixture/ForgotPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ForgotPassword)

responseDescribeUserPool :: DescribeUserPoolResponse -> TestTree
responseDescribeUserPool =
  res
    "DescribeUserPoolResponse"
    "fixture/DescribeUserPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserPool)

responseInitiateAuth :: InitiateAuthResponse -> TestTree
responseInitiateAuth =
  res
    "InitiateAuthResponse"
    "fixture/InitiateAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateAuth)

responseAdminListGroupsForUser :: AdminListGroupsForUserResponse -> TestTree
responseAdminListGroupsForUser =
  res
    "AdminListGroupsForUserResponse"
    "fixture/AdminListGroupsForUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminListGroupsForUser)

responseAdminConfirmSignUp :: AdminConfirmSignUpResponse -> TestTree
responseAdminConfirmSignUp =
  res
    "AdminConfirmSignUpResponse"
    "fixture/AdminConfirmSignUpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminConfirmSignUp)

responseAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedbackResponse -> TestTree
responseAdminUpdateAuthEventFeedback =
  res
    "AdminUpdateAuthEventFeedbackResponse"
    "fixture/AdminUpdateAuthEventFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminUpdateAuthEventFeedback)

responseAdminSetUserPassword :: AdminSetUserPasswordResponse -> TestTree
responseAdminSetUserPassword =
  res
    "AdminSetUserPasswordResponse"
    "fixture/AdminSetUserPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminSetUserPassword)

responseStartUserImportJob :: StartUserImportJobResponse -> TestTree
responseStartUserImportJob =
  res
    "StartUserImportJobResponse"
    "fixture/StartUserImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartUserImportJob)

responseCreateIdentityProvider :: CreateIdentityProviderResponse -> TestTree
responseCreateIdentityProvider =
  res
    "CreateIdentityProviderResponse"
    "fixture/CreateIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIdentityProvider)

responseSetUICustomization :: SetUICustomizationResponse -> TestTree
responseSetUICustomization =
  res
    "SetUICustomizationResponse"
    "fixture/SetUICustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetUICustomization)

responseListIdentityProviders :: ListIdentityProvidersResponse -> TestTree
responseListIdentityProviders =
  res
    "ListIdentityProvidersResponse"
    "fixture/ListIdentityProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityProviders)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevice)

responseSignUp :: SignUpResponse -> TestTree
responseSignUp =
  res
    "SignUpResponse"
    "fixture/SignUpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SignUp)

responseDeleteResourceServer :: DeleteResourceServerResponse -> TestTree
responseDeleteResourceServer =
  res
    "DeleteResourceServerResponse"
    "fixture/DeleteResourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceServer)

responseUpdateResourceServer :: UpdateResourceServerResponse -> TestTree
responseUpdateResourceServer =
  res
    "UpdateResourceServerResponse"
    "fixture/UpdateResourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceServer)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword =
  res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangePassword)

responseCreateUserPoolDomain :: CreateUserPoolDomainResponse -> TestTree
responseCreateUserPoolDomain =
  res
    "CreateUserPoolDomainResponse"
    "fixture/CreateUserPoolDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserPoolDomain)

responseRespondToAuthChallenge :: RespondToAuthChallengeResponse -> TestTree
responseRespondToAuthChallenge =
  res
    "RespondToAuthChallengeResponse"
    "fixture/RespondToAuthChallengeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondToAuthChallenge)

responseCreateUserPool :: CreateUserPoolResponse -> TestTree
responseCreateUserPool =
  res
    "CreateUserPoolResponse"
    "fixture/CreateUserPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserPool)

responseAdminGetDevice :: AdminGetDeviceResponse -> TestTree
responseAdminGetDevice =
  res
    "AdminGetDeviceResponse"
    "fixture/AdminGetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminGetDevice)

responseGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifierResponse -> TestTree
responseGetIdentityProviderByIdentifier =
  res
    "GetIdentityProviderByIdentifierResponse"
    "fixture/GetIdentityProviderByIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityProviderByIdentifier)

responseAdminRemoveUserFromGroup :: AdminRemoveUserFromGroupResponse -> TestTree
responseAdminRemoveUserFromGroup =
  res
    "AdminRemoveUserFromGroupResponse"
    "fixture/AdminRemoveUserFromGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminRemoveUserFromGroup)

responseSetRiskConfiguration :: SetRiskConfigurationResponse -> TestTree
responseSetRiskConfiguration =
  res
    "SetRiskConfigurationResponse"
    "fixture/SetRiskConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetRiskConfiguration)

responseConfirmSignUp :: ConfirmSignUpResponse -> TestTree
responseConfirmSignUp =
  res
    "ConfirmSignUpResponse"
    "fixture/ConfirmSignUpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmSignUp)

responseListUserPools :: ListUserPoolsResponse -> TestTree
responseListUserPools =
  res
    "ListUserPoolsResponse"
    "fixture/ListUserPoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserPools)

responseAdminResetUserPassword :: AdminResetUserPasswordResponse -> TestTree
responseAdminResetUserPassword =
  res
    "AdminResetUserPasswordResponse"
    "fixture/AdminResetUserPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminResetUserPassword)

responseUpdateAuthEventFeedback :: UpdateAuthEventFeedbackResponse -> TestTree
responseUpdateAuthEventFeedback =
  res
    "UpdateAuthEventFeedbackResponse"
    "fixture/UpdateAuthEventFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuthEventFeedback)

responseCreateUserImportJob :: CreateUserImportJobResponse -> TestTree
responseCreateUserImportJob =
  res
    "CreateUserImportJobResponse"
    "fixture/CreateUserImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserImportJob)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUser)

responseGetUICustomization :: GetUICustomizationResponse -> TestTree
responseGetUICustomization =
  res
    "GetUICustomizationResponse"
    "fixture/GetUICustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUICustomization)

responseGetCSVHeader :: GetCSVHeaderResponse -> TestTree
responseGetCSVHeader =
  res
    "GetCSVHeaderResponse"
    "fixture/GetCSVHeaderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCSVHeader)

responseAdminDeleteUser :: AdminDeleteUserResponse -> TestTree
responseAdminDeleteUser =
  res
    "AdminDeleteUserResponse"
    "fixture/AdminDeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminDeleteUser)

responseAdminForgetDevice :: AdminForgetDeviceResponse -> TestTree
responseAdminForgetDevice =
  res
    "AdminForgetDeviceResponse"
    "fixture/AdminForgetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminForgetDevice)

responseDescribeResourceServer :: DescribeResourceServerResponse -> TestTree
responseDescribeResourceServer =
  res
    "DescribeResourceServerResponse"
    "fixture/DescribeResourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourceServer)

responseSetUserMFAPreference :: SetUserMFAPreferenceResponse -> TestTree
responseSetUserMFAPreference =
  res
    "SetUserMFAPreferenceResponse"
    "fixture/SetUserMFAPreferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetUserMFAPreference)

responseAdminUpdateDeviceStatus :: AdminUpdateDeviceStatusResponse -> TestTree
responseAdminUpdateDeviceStatus =
  res
    "AdminUpdateDeviceStatusResponse"
    "fixture/AdminUpdateDeviceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminUpdateDeviceStatus)

responseAdminCreateUser :: AdminCreateUserResponse -> TestTree
responseAdminCreateUser =
  res
    "AdminCreateUserResponse"
    "fixture/AdminCreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminCreateUser)

responseAddCustomAttributes :: AddCustomAttributesResponse -> TestTree
responseAddCustomAttributes =
  res
    "AddCustomAttributesResponse"
    "fixture/AddCustomAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddCustomAttributes)

responseListUserPoolClients :: ListUserPoolClientsResponse -> TestTree
responseListUserPoolClients =
  res
    "ListUserPoolClientsResponse"
    "fixture/ListUserPoolClientsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserPoolClients)

responseAdminSetUserMFAPreference :: AdminSetUserMFAPreferenceResponse -> TestTree
responseAdminSetUserMFAPreference =
  res
    "AdminSetUserMFAPreferenceResponse"
    "fixture/AdminSetUserMFAPreferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminSetUserMFAPreference)

responseUpdateUserPoolClient :: UpdateUserPoolClientResponse -> TestTree
responseUpdateUserPoolClient =
  res
    "UpdateUserPoolClientResponse"
    "fixture/UpdateUserPoolClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserPoolClient)

responseDeleteUserPoolClient :: DeleteUserPoolClientResponse -> TestTree
responseDeleteUserPoolClient =
  res
    "DeleteUserPoolClientResponse"
    "fixture/DeleteUserPoolClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserPoolClient)

responseUpdateDeviceStatus :: UpdateDeviceStatusResponse -> TestTree
responseUpdateDeviceStatus =
  res
    "UpdateDeviceStatusResponse"
    "fixture/UpdateDeviceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceStatus)

responseForgetDevice :: ForgetDeviceResponse -> TestTree
responseForgetDevice =
  res
    "ForgetDeviceResponse"
    "fixture/ForgetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ForgetDevice)

responseGetSigningCertificate :: GetSigningCertificateResponse -> TestTree
responseGetSigningCertificate =
  res
    "GetSigningCertificateResponse"
    "fixture/GetSigningCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSigningCertificate)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateUserPoolClient :: CreateUserPoolClientResponse -> TestTree
responseCreateUserPoolClient =
  res
    "CreateUserPoolClientResponse"
    "fixture/CreateUserPoolClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserPoolClient)

responseGetUserPoolMfaConfig :: GetUserPoolMfaConfigResponse -> TestTree
responseGetUserPoolMfaConfig =
  res
    "GetUserPoolMfaConfigResponse"
    "fixture/GetUserPoolMfaConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserPoolMfaConfig)

responseCreateResourceServer :: CreateResourceServerResponse -> TestTree
responseCreateResourceServer =
  res
    "CreateResourceServerResponse"
    "fixture/CreateResourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceServer)

responseAdminListUserAuthEvents :: AdminListUserAuthEventsResponse -> TestTree
responseAdminListUserAuthEvents =
  res
    "AdminListUserAuthEventsResponse"
    "fixture/AdminListUserAuthEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminListUserAuthEvents)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseAdminAddUserToGroup :: AdminAddUserToGroupResponse -> TestTree
responseAdminAddUserToGroup =
  res
    "AdminAddUserToGroupResponse"
    "fixture/AdminAddUserToGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminAddUserToGroup)

responseVerifySoftwareToken :: VerifySoftwareTokenResponse -> TestTree
responseVerifySoftwareToken =
  res
    "VerifySoftwareTokenResponse"
    "fixture/VerifySoftwareTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifySoftwareToken)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseRevokeToken :: RevokeTokenResponse -> TestTree
responseRevokeToken =
  res
    "RevokeTokenResponse"
    "fixture/RevokeTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeToken)

responseStopUserImportJob :: StopUserImportJobResponse -> TestTree
responseStopUserImportJob =
  res
    "StopUserImportJobResponse"
    "fixture/StopUserImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopUserImportJob)

responseDescribeUserImportJob :: DescribeUserImportJobResponse -> TestTree
responseDescribeUserImportJob =
  res
    "DescribeUserImportJobResponse"
    "fixture/DescribeUserImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserImportJob)

responseDescribeRiskConfiguration :: DescribeRiskConfigurationResponse -> TestTree
responseDescribeRiskConfiguration =
  res
    "DescribeRiskConfigurationResponse"
    "fixture/DescribeRiskConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRiskConfiguration)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseGlobalSignOut :: GlobalSignOutResponse -> TestTree
responseGlobalSignOut =
  res
    "GlobalSignOutResponse"
    "fixture/GlobalSignOutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GlobalSignOut)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseUpdateIdentityProvider :: UpdateIdentityProviderResponse -> TestTree
responseUpdateIdentityProvider =
  res
    "UpdateIdentityProviderResponse"
    "fixture/UpdateIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIdentityProvider)

responseDeleteIdentityProvider :: DeleteIdentityProviderResponse -> TestTree
responseDeleteIdentityProvider =
  res
    "DeleteIdentityProviderResponse"
    "fixture/DeleteIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIdentityProvider)

responseListResourceServers :: ListResourceServersResponse -> TestTree
responseListResourceServers =
  res
    "ListResourceServersResponse"
    "fixture/ListResourceServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceServers)

responseAdminRespondToAuthChallenge :: AdminRespondToAuthChallengeResponse -> TestTree
responseAdminRespondToAuthChallenge =
  res
    "AdminRespondToAuthChallengeResponse"
    "fixture/AdminRespondToAuthChallengeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminRespondToAuthChallenge)

responseSetUserSettings :: SetUserSettingsResponse -> TestTree
responseSetUserSettings =
  res
    "SetUserSettingsResponse"
    "fixture/SetUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetUserSettings)

responseAdminListDevices :: AdminListDevicesResponse -> TestTree
responseAdminListDevices =
  res
    "AdminListDevicesResponse"
    "fixture/AdminListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminListDevices)

responseDescribeUserPoolClient :: DescribeUserPoolClientResponse -> TestTree
responseDescribeUserPoolClient =
  res
    "DescribeUserPoolClientResponse"
    "fixture/DescribeUserPoolClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserPoolClient)

responseResendConfirmationCode :: ResendConfirmationCodeResponse -> TestTree
responseResendConfirmationCode =
  res
    "ResendConfirmationCodeResponse"
    "fixture/ResendConfirmationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResendConfirmationCode)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroup)

responseAdminSetUserSettings :: AdminSetUserSettingsResponse -> TestTree
responseAdminSetUserSettings =
  res
    "AdminSetUserSettingsResponse"
    "fixture/AdminSetUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminSetUserSettings)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)
