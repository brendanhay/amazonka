{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestDeleteUserPool $
--             mkDeleteUserPool
--
--         , requestUpdateUserPool $
--             mkUpdateUserPool
--
--         , requestUpdateUserPoolDomain $
--             mkUpdateUserPoolDomain
--
--         , requestDeleteUserPoolDomain $
--             mkDeleteUserPoolDomain
--
--         , requestAdminInitiateAuth $
--             mkAdminInitiateAuth
--
--         , requestAdminLinkProviderForUser $
--             mkAdminLinkProviderForUser
--
--         , requestAdminEnableUser $
--             mkAdminEnableUser
--
--         , requestGetUserAttributeVerificationCode $
--             mkGetUserAttributeVerificationCode
--
--         , requestSetUserPoolMFAConfig $
--             mkSetUserPoolMFAConfig
--
--         , requestUpdateUserAttributes $
--             mkUpdateUserAttributes
--
--         , requestDeleteUserAttributes $
--             mkDeleteUserAttributes
--
--         , requestVerifyUserAttribute $
--             mkVerifyUserAttribute
--
--         , requestAdminDisableUser $
--             mkAdminDisableUser
--
--         , requestConfirmDevice $
--             mkConfirmDevice
--
--         , requestConfirmForgotPassword $
--             mkConfirmForgotPassword
--
--         , requestListUserImportJobs $
--             mkListUserImportJobs
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDescribeIdentityProvider $
--             mkDescribeIdentityProvider
--
--         , requestListUsers $
--             mkListUsers
--
--         , requestAdminDeleteUserAttributes $
--             mkAdminDeleteUserAttributes
--
--         , requestDescribeUserPoolDomain $
--             mkDescribeUserPoolDomain
--
--         , requestAdminUpdateUserAttributes $
--             mkAdminUpdateUserAttributes
--
--         , requestAdminGetUser $
--             mkAdminGetUser
--
--         , requestAdminUserGlobalSignOut $
--             mkAdminUserGlobalSignOut
--
--         , requestListUsersInGroup $
--             mkListUsersInGroup
--
--         , requestAssociateSoftwareToken $
--             mkAssociateSoftwareToken
--
--         , requestAdminDisableProviderForUser $
--             mkAdminDisableProviderForUser
--
--         , requestForgotPassword $
--             mkForgotPassword
--
--         , requestDescribeUserPool $
--             mkDescribeUserPool
--
--         , requestInitiateAuth $
--             mkInitiateAuth
--
--         , requestAdminListGroupsForUser $
--             mkAdminListGroupsForUser
--
--         , requestAdminConfirmSignUp $
--             mkAdminConfirmSignUp
--
--         , requestAdminUpdateAuthEventFeedback $
--             mkAdminUpdateAuthEventFeedback
--
--         , requestAdminSetUserPassword $
--             mkAdminSetUserPassword
--
--         , requestStartUserImportJob $
--             mkStartUserImportJob
--
--         , requestCreateIdentityProvider $
--             mkCreateIdentityProvider
--
--         , requestSetUICustomization $
--             mkSetUICustomization
--
--         , requestListIdentityProviders $
--             mkListIdentityProviders
--
--         , requestGetDevice $
--             mkGetDevice
--
--         , requestSignUp $
--             mkSignUp
--
--         , requestDeleteResourceServer $
--             mkDeleteResourceServer
--
--         , requestUpdateResourceServer $
--             mkUpdateResourceServer
--
--         , requestChangePassword $
--             mkChangePassword
--
--         , requestCreateUserPoolDomain $
--             mkCreateUserPoolDomain
--
--         , requestRespondToAuthChallenge $
--             mkRespondToAuthChallenge
--
--         , requestCreateUserPool $
--             mkCreateUserPool
--
--         , requestAdminGetDevice $
--             mkAdminGetDevice
--
--         , requestGetIdentityProviderByIdentifier $
--             mkGetIdentityProviderByIdentifier
--
--         , requestAdminRemoveUserFromGroup $
--             mkAdminRemoveUserFromGroup
--
--         , requestSetRiskConfiguration $
--             mkSetRiskConfiguration
--
--         , requestConfirmSignUp $
--             mkConfirmSignUp
--
--         , requestListUserPools $
--             mkListUserPools
--
--         , requestAdminResetUserPassword $
--             mkAdminResetUserPassword
--
--         , requestUpdateAuthEventFeedback $
--             mkUpdateAuthEventFeedback
--
--         , requestCreateUserImportJob $
--             mkCreateUserImportJob
--
--         , requestGetUser $
--             mkGetUser
--
--         , requestGetUICustomization $
--             mkGetUICustomization
--
--         , requestGetCSVHeader $
--             mkGetCSVHeader
--
--         , requestAdminDeleteUser $
--             mkAdminDeleteUser
--
--         , requestAdminForgetDevice $
--             mkAdminForgetDevice
--
--         , requestDescribeResourceServer $
--             mkDescribeResourceServer
--
--         , requestSetUserMFAPreference $
--             mkSetUserMFAPreference
--
--         , requestAdminUpdateDeviceStatus $
--             mkAdminUpdateDeviceStatus
--
--         , requestAdminCreateUser $
--             mkAdminCreateUser
--
--         , requestAddCustomAttributes $
--             mkAddCustomAttributes
--
--         , requestListUserPoolClients $
--             mkListUserPoolClients
--
--         , requestAdminSetUserMFAPreference $
--             mkAdminSetUserMFAPreference
--
--         , requestUpdateUserPoolClient $
--             mkUpdateUserPoolClient
--
--         , requestDeleteUserPoolClient $
--             mkDeleteUserPoolClient
--
--         , requestUpdateDeviceStatus $
--             mkUpdateDeviceStatus
--
--         , requestForgetDevice $
--             mkForgetDevice
--
--         , requestGetSigningCertificate $
--             mkGetSigningCertificate
--
--         , requestDeleteUser $
--             mkDeleteUser
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestCreateUserPoolClient $
--             mkCreateUserPoolClient
--
--         , requestGetUserPoolMFAConfig $
--             mkGetUserPoolMFAConfig
--
--         , requestCreateResourceServer $
--             mkCreateResourceServer
--
--         , requestAdminListUserAuthEvents $
--             mkAdminListUserAuthEvents
--
--         , requestCreateGroup $
--             mkCreateGroup
--
--         , requestAdminAddUserToGroup $
--             mkAdminAddUserToGroup
--
--         , requestVerifySoftwareToken $
--             mkVerifySoftwareToken
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestStopUserImportJob $
--             mkStopUserImportJob
--
--         , requestDescribeUserImportJob $
--             mkDescribeUserImportJob
--
--         , requestDescribeRiskConfiguration $
--             mkDescribeRiskConfiguration
--
--         , requestDeleteGroup $
--             mkDeleteGroup
--
--         , requestUpdateGroup $
--             mkUpdateGroup
--
--         , requestGlobalSignOut $
--             mkGlobalSignOut
--
--         , requestListGroups $
--             mkListGroups
--
--         , requestUpdateIdentityProvider $
--             mkUpdateIdentityProvider
--
--         , requestDeleteIdentityProvider $
--             mkDeleteIdentityProvider
--
--         , requestListResourceServers $
--             mkListResourceServers
--
--         , requestAdminRespondToAuthChallenge $
--             mkAdminRespondToAuthChallenge
--
--         , requestSetUserSettings $
--             mkSetUserSettings
--
--         , requestAdminListDevices $
--             mkAdminListDevices
--
--         , requestDescribeUserPoolClient $
--             mkDescribeUserPoolClient
--
--         , requestResendConfirmationCode $
--             mkResendConfirmationCode
--
--         , requestGetGroup $
--             mkGetGroup
--
--         , requestAdminSetUserSettings $
--             mkAdminSetUserSettings
--
--         , requestListDevices $
--             mkListDevices
--
--           ]

--     , testGroup "response"
--         [ responseDeleteUserPool $
--             mkDeleteUserPoolResponse
--
--         , responseUpdateUserPool $
--             mkUpdateUserPoolResponse
--
--         , responseUpdateUserPoolDomain $
--             mkUpdateUserPoolDomainResponse
--
--         , responseDeleteUserPoolDomain $
--             mkDeleteUserPoolDomainResponse
--
--         , responseAdminInitiateAuth $
--             mkAdminInitiateAuthResponse
--
--         , responseAdminLinkProviderForUser $
--             mkAdminLinkProviderForUserResponse
--
--         , responseAdminEnableUser $
--             mkAdminEnableUserResponse
--
--         , responseGetUserAttributeVerificationCode $
--             mkGetUserAttributeVerificationCodeResponse
--
--         , responseSetUserPoolMFAConfig $
--             mkSetUserPoolMFAConfigResponse
--
--         , responseUpdateUserAttributes $
--             mkUpdateUserAttributesResponse
--
--         , responseDeleteUserAttributes $
--             mkDeleteUserAttributesResponse
--
--         , responseVerifyUserAttribute $
--             mkVerifyUserAttributeResponse
--
--         , responseAdminDisableUser $
--             mkAdminDisableUserResponse
--
--         , responseConfirmDevice $
--             mkConfirmDeviceResponse
--
--         , responseConfirmForgotPassword $
--             mkConfirmForgotPasswordResponse
--
--         , responseListUserImportJobs $
--             mkListUserImportJobsResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDescribeIdentityProvider $
--             mkDescribeIdentityProviderResponse
--
--         , responseListUsers $
--             mkListUsersResponse
--
--         , responseAdminDeleteUserAttributes $
--             mkAdminDeleteUserAttributesResponse
--
--         , responseDescribeUserPoolDomain $
--             mkDescribeUserPoolDomainResponse
--
--         , responseAdminUpdateUserAttributes $
--             mkAdminUpdateUserAttributesResponse
--
--         , responseAdminGetUser $
--             mkAdminGetUserResponse
--
--         , responseAdminUserGlobalSignOut $
--             mkAdminUserGlobalSignOutResponse
--
--         , responseListUsersInGroup $
--             mkListUsersInGroupResponse
--
--         , responseAssociateSoftwareToken $
--             mkAssociateSoftwareTokenResponse
--
--         , responseAdminDisableProviderForUser $
--             mkAdminDisableProviderForUserResponse
--
--         , responseForgotPassword $
--             mkForgotPasswordResponse
--
--         , responseDescribeUserPool $
--             mkDescribeUserPoolResponse
--
--         , responseInitiateAuth $
--             mkInitiateAuthResponse
--
--         , responseAdminListGroupsForUser $
--             mkAdminListGroupsForUserResponse
--
--         , responseAdminConfirmSignUp $
--             mkAdminConfirmSignUpResponse
--
--         , responseAdminUpdateAuthEventFeedback $
--             mkAdminUpdateAuthEventFeedbackResponse
--
--         , responseAdminSetUserPassword $
--             mkAdminSetUserPasswordResponse
--
--         , responseStartUserImportJob $
--             mkStartUserImportJobResponse
--
--         , responseCreateIdentityProvider $
--             mkCreateIdentityProviderResponse
--
--         , responseSetUICustomization $
--             mkSetUICustomizationResponse
--
--         , responseListIdentityProviders $
--             mkListIdentityProvidersResponse
--
--         , responseGetDevice $
--             mkGetDeviceResponse
--
--         , responseSignUp $
--             mkSignUpResponse
--
--         , responseDeleteResourceServer $
--             mkDeleteResourceServerResponse
--
--         , responseUpdateResourceServer $
--             mkUpdateResourceServerResponse
--
--         , responseChangePassword $
--             mkChangePasswordResponse
--
--         , responseCreateUserPoolDomain $
--             mkCreateUserPoolDomainResponse
--
--         , responseRespondToAuthChallenge $
--             mkRespondToAuthChallengeResponse
--
--         , responseCreateUserPool $
--             mkCreateUserPoolResponse
--
--         , responseAdminGetDevice $
--             mkAdminGetDeviceResponse
--
--         , responseGetIdentityProviderByIdentifier $
--             mkGetIdentityProviderByIdentifierResponse
--
--         , responseAdminRemoveUserFromGroup $
--             mkAdminRemoveUserFromGroupResponse
--
--         , responseSetRiskConfiguration $
--             mkSetRiskConfigurationResponse
--
--         , responseConfirmSignUp $
--             mkConfirmSignUpResponse
--
--         , responseListUserPools $
--             mkListUserPoolsResponse
--
--         , responseAdminResetUserPassword $
--             mkAdminResetUserPasswordResponse
--
--         , responseUpdateAuthEventFeedback $
--             mkUpdateAuthEventFeedbackResponse
--
--         , responseCreateUserImportJob $
--             mkCreateUserImportJobResponse
--
--         , responseGetUser $
--             mkGetUserResponse
--
--         , responseGetUICustomization $
--             mkGetUICustomizationResponse
--
--         , responseGetCSVHeader $
--             mkGetCSVHeaderResponse
--
--         , responseAdminDeleteUser $
--             mkAdminDeleteUserResponse
--
--         , responseAdminForgetDevice $
--             mkAdminForgetDeviceResponse
--
--         , responseDescribeResourceServer $
--             mkDescribeResourceServerResponse
--
--         , responseSetUserMFAPreference $
--             mkSetUserMFAPreferenceResponse
--
--         , responseAdminUpdateDeviceStatus $
--             mkAdminUpdateDeviceStatusResponse
--
--         , responseAdminCreateUser $
--             mkAdminCreateUserResponse
--
--         , responseAddCustomAttributes $
--             mkAddCustomAttributesResponse
--
--         , responseListUserPoolClients $
--             mkListUserPoolClientsResponse
--
--         , responseAdminSetUserMFAPreference $
--             mkAdminSetUserMFAPreferenceResponse
--
--         , responseUpdateUserPoolClient $
--             mkUpdateUserPoolClientResponse
--
--         , responseDeleteUserPoolClient $
--             mkDeleteUserPoolClientResponse
--
--         , responseUpdateDeviceStatus $
--             mkUpdateDeviceStatusResponse
--
--         , responseForgetDevice $
--             mkForgetDeviceResponse
--
--         , responseGetSigningCertificate $
--             mkGetSigningCertificateResponse
--
--         , responseDeleteUser $
--             mkDeleteUserResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseCreateUserPoolClient $
--             mkCreateUserPoolClientResponse
--
--         , responseGetUserPoolMFAConfig $
--             mkGetUserPoolMFAConfigResponse
--
--         , responseCreateResourceServer $
--             mkCreateResourceServerResponse
--
--         , responseAdminListUserAuthEvents $
--             mkAdminListUserAuthEventsResponse
--
--         , responseCreateGroup $
--             mkCreateGroupResponse
--
--         , responseAdminAddUserToGroup $
--             mkAdminAddUserToGroupResponse
--
--         , responseVerifySoftwareToken $
--             mkVerifySoftwareTokenResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseStopUserImportJob $
--             mkStopUserImportJobResponse
--
--         , responseDescribeUserImportJob $
--             mkDescribeUserImportJobResponse
--
--         , responseDescribeRiskConfiguration $
--             mkDescribeRiskConfigurationResponse
--
--         , responseDeleteGroup $
--             mkDeleteGroupResponse
--
--         , responseUpdateGroup $
--             mkUpdateGroupResponse
--
--         , responseGlobalSignOut $
--             mkGlobalSignOutResponse
--
--         , responseListGroups $
--             mkListGroupsResponse
--
--         , responseUpdateIdentityProvider $
--             mkUpdateIdentityProviderResponse
--
--         , responseDeleteIdentityProvider $
--             mkDeleteIdentityProviderResponse
--
--         , responseListResourceServers $
--             mkListResourceServersResponse
--
--         , responseAdminRespondToAuthChallenge $
--             mkAdminRespondToAuthChallengeResponse
--
--         , responseSetUserSettings $
--             mkSetUserSettingsResponse
--
--         , responseAdminListDevices $
--             mkAdminListDevicesResponse
--
--         , responseDescribeUserPoolClient $
--             mkDescribeUserPoolClientResponse
--
--         , responseResendConfirmationCode $
--             mkResendConfirmationCodeResponse
--
--         , responseGetGroup $
--             mkGetGroupResponse
--
--         , responseAdminSetUserSettings $
--             mkAdminSetUserSettingsResponse
--
--         , responseListDevices $
--             mkListDevicesResponse
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

requestSetUserPoolMFAConfig :: SetUserPoolMFAConfig -> TestTree
requestSetUserPoolMFAConfig =
  req
    "SetUserPoolMFAConfig"
    "fixture/SetUserPoolMFAConfig.yaml"

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

requestGetUserPoolMFAConfig :: GetUserPoolMFAConfig -> TestTree
requestGetUserPoolMFAConfig =
  req
    "GetUserPoolMFAConfig"
    "fixture/GetUserPoolMFAConfig.yaml"

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
    cognitoIdentityProviderService
    (Proxy :: Proxy DeleteUserPool)

responseUpdateUserPool :: UpdateUserPoolResponse -> TestTree
responseUpdateUserPool =
  res
    "UpdateUserPoolResponse"
    "fixture/UpdateUserPoolResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UpdateUserPool)

responseUpdateUserPoolDomain :: UpdateUserPoolDomainResponse -> TestTree
responseUpdateUserPoolDomain =
  res
    "UpdateUserPoolDomainResponse"
    "fixture/UpdateUserPoolDomainResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UpdateUserPoolDomain)

responseDeleteUserPoolDomain :: DeleteUserPoolDomainResponse -> TestTree
responseDeleteUserPoolDomain =
  res
    "DeleteUserPoolDomainResponse"
    "fixture/DeleteUserPoolDomainResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DeleteUserPoolDomain)

responseAdminInitiateAuth :: AdminInitiateAuthResponse -> TestTree
responseAdminInitiateAuth =
  res
    "AdminInitiateAuthResponse"
    "fixture/AdminInitiateAuthResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminInitiateAuth)

responseAdminLinkProviderForUser :: AdminLinkProviderForUserResponse -> TestTree
responseAdminLinkProviderForUser =
  res
    "AdminLinkProviderForUserResponse"
    "fixture/AdminLinkProviderForUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminLinkProviderForUser)

responseAdminEnableUser :: AdminEnableUserResponse -> TestTree
responseAdminEnableUser =
  res
    "AdminEnableUserResponse"
    "fixture/AdminEnableUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminEnableUser)

responseGetUserAttributeVerificationCode :: GetUserAttributeVerificationCodeResponse -> TestTree
responseGetUserAttributeVerificationCode =
  res
    "GetUserAttributeVerificationCodeResponse"
    "fixture/GetUserAttributeVerificationCodeResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GetUserAttributeVerificationCode)

responseSetUserPoolMFAConfig :: SetUserPoolMFAConfigResponse -> TestTree
responseSetUserPoolMFAConfig =
  res
    "SetUserPoolMFAConfigResponse"
    "fixture/SetUserPoolMFAConfigResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy SetUserPoolMFAConfig)

responseUpdateUserAttributes :: UpdateUserAttributesResponse -> TestTree
responseUpdateUserAttributes =
  res
    "UpdateUserAttributesResponse"
    "fixture/UpdateUserAttributesResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UpdateUserAttributes)

responseDeleteUserAttributes :: DeleteUserAttributesResponse -> TestTree
responseDeleteUserAttributes =
  res
    "DeleteUserAttributesResponse"
    "fixture/DeleteUserAttributesResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DeleteUserAttributes)

responseVerifyUserAttribute :: VerifyUserAttributeResponse -> TestTree
responseVerifyUserAttribute =
  res
    "VerifyUserAttributeResponse"
    "fixture/VerifyUserAttributeResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy VerifyUserAttribute)

responseAdminDisableUser :: AdminDisableUserResponse -> TestTree
responseAdminDisableUser =
  res
    "AdminDisableUserResponse"
    "fixture/AdminDisableUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminDisableUser)

responseConfirmDevice :: ConfirmDeviceResponse -> TestTree
responseConfirmDevice =
  res
    "ConfirmDeviceResponse"
    "fixture/ConfirmDeviceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ConfirmDevice)

responseConfirmForgotPassword :: ConfirmForgotPasswordResponse -> TestTree
responseConfirmForgotPassword =
  res
    "ConfirmForgotPasswordResponse"
    "fixture/ConfirmForgotPasswordResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ConfirmForgotPassword)

responseListUserImportJobs :: ListUserImportJobsResponse -> TestTree
responseListUserImportJobs =
  res
    "ListUserImportJobsResponse"
    "fixture/ListUserImportJobsResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListUserImportJobs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeIdentityProvider :: DescribeIdentityProviderResponse -> TestTree
responseDescribeIdentityProvider =
  res
    "DescribeIdentityProviderResponse"
    "fixture/DescribeIdentityProviderResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DescribeIdentityProvider)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListUsers)

responseAdminDeleteUserAttributes :: AdminDeleteUserAttributesResponse -> TestTree
responseAdminDeleteUserAttributes =
  res
    "AdminDeleteUserAttributesResponse"
    "fixture/AdminDeleteUserAttributesResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminDeleteUserAttributes)

responseDescribeUserPoolDomain :: DescribeUserPoolDomainResponse -> TestTree
responseDescribeUserPoolDomain =
  res
    "DescribeUserPoolDomainResponse"
    "fixture/DescribeUserPoolDomainResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DescribeUserPoolDomain)

responseAdminUpdateUserAttributes :: AdminUpdateUserAttributesResponse -> TestTree
responseAdminUpdateUserAttributes =
  res
    "AdminUpdateUserAttributesResponse"
    "fixture/AdminUpdateUserAttributesResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminUpdateUserAttributes)

responseAdminGetUser :: AdminGetUserResponse -> TestTree
responseAdminGetUser =
  res
    "AdminGetUserResponse"
    "fixture/AdminGetUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminGetUser)

responseAdminUserGlobalSignOut :: AdminUserGlobalSignOutResponse -> TestTree
responseAdminUserGlobalSignOut =
  res
    "AdminUserGlobalSignOutResponse"
    "fixture/AdminUserGlobalSignOutResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminUserGlobalSignOut)

responseListUsersInGroup :: ListUsersInGroupResponse -> TestTree
responseListUsersInGroup =
  res
    "ListUsersInGroupResponse"
    "fixture/ListUsersInGroupResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListUsersInGroup)

responseAssociateSoftwareToken :: AssociateSoftwareTokenResponse -> TestTree
responseAssociateSoftwareToken =
  res
    "AssociateSoftwareTokenResponse"
    "fixture/AssociateSoftwareTokenResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AssociateSoftwareToken)

responseAdminDisableProviderForUser :: AdminDisableProviderForUserResponse -> TestTree
responseAdminDisableProviderForUser =
  res
    "AdminDisableProviderForUserResponse"
    "fixture/AdminDisableProviderForUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminDisableProviderForUser)

responseForgotPassword :: ForgotPasswordResponse -> TestTree
responseForgotPassword =
  res
    "ForgotPasswordResponse"
    "fixture/ForgotPasswordResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ForgotPassword)

responseDescribeUserPool :: DescribeUserPoolResponse -> TestTree
responseDescribeUserPool =
  res
    "DescribeUserPoolResponse"
    "fixture/DescribeUserPoolResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DescribeUserPool)

responseInitiateAuth :: InitiateAuthResponse -> TestTree
responseInitiateAuth =
  res
    "InitiateAuthResponse"
    "fixture/InitiateAuthResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy InitiateAuth)

responseAdminListGroupsForUser :: AdminListGroupsForUserResponse -> TestTree
responseAdminListGroupsForUser =
  res
    "AdminListGroupsForUserResponse"
    "fixture/AdminListGroupsForUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminListGroupsForUser)

responseAdminConfirmSignUp :: AdminConfirmSignUpResponse -> TestTree
responseAdminConfirmSignUp =
  res
    "AdminConfirmSignUpResponse"
    "fixture/AdminConfirmSignUpResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminConfirmSignUp)

responseAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedbackResponse -> TestTree
responseAdminUpdateAuthEventFeedback =
  res
    "AdminUpdateAuthEventFeedbackResponse"
    "fixture/AdminUpdateAuthEventFeedbackResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminUpdateAuthEventFeedback)

responseAdminSetUserPassword :: AdminSetUserPasswordResponse -> TestTree
responseAdminSetUserPassword =
  res
    "AdminSetUserPasswordResponse"
    "fixture/AdminSetUserPasswordResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminSetUserPassword)

responseStartUserImportJob :: StartUserImportJobResponse -> TestTree
responseStartUserImportJob =
  res
    "StartUserImportJobResponse"
    "fixture/StartUserImportJobResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy StartUserImportJob)

responseCreateIdentityProvider :: CreateIdentityProviderResponse -> TestTree
responseCreateIdentityProvider =
  res
    "CreateIdentityProviderResponse"
    "fixture/CreateIdentityProviderResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy CreateIdentityProvider)

responseSetUICustomization :: SetUICustomizationResponse -> TestTree
responseSetUICustomization =
  res
    "SetUICustomizationResponse"
    "fixture/SetUICustomizationResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy SetUICustomization)

responseListIdentityProviders :: ListIdentityProvidersResponse -> TestTree
responseListIdentityProviders =
  res
    "ListIdentityProvidersResponse"
    "fixture/ListIdentityProvidersResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListIdentityProviders)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GetDevice)

responseSignUp :: SignUpResponse -> TestTree
responseSignUp =
  res
    "SignUpResponse"
    "fixture/SignUpResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy SignUp)

responseDeleteResourceServer :: DeleteResourceServerResponse -> TestTree
responseDeleteResourceServer =
  res
    "DeleteResourceServerResponse"
    "fixture/DeleteResourceServerResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DeleteResourceServer)

responseUpdateResourceServer :: UpdateResourceServerResponse -> TestTree
responseUpdateResourceServer =
  res
    "UpdateResourceServerResponse"
    "fixture/UpdateResourceServerResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UpdateResourceServer)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword =
  res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ChangePassword)

responseCreateUserPoolDomain :: CreateUserPoolDomainResponse -> TestTree
responseCreateUserPoolDomain =
  res
    "CreateUserPoolDomainResponse"
    "fixture/CreateUserPoolDomainResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy CreateUserPoolDomain)

responseRespondToAuthChallenge :: RespondToAuthChallengeResponse -> TestTree
responseRespondToAuthChallenge =
  res
    "RespondToAuthChallengeResponse"
    "fixture/RespondToAuthChallengeResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy RespondToAuthChallenge)

responseCreateUserPool :: CreateUserPoolResponse -> TestTree
responseCreateUserPool =
  res
    "CreateUserPoolResponse"
    "fixture/CreateUserPoolResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy CreateUserPool)

responseAdminGetDevice :: AdminGetDeviceResponse -> TestTree
responseAdminGetDevice =
  res
    "AdminGetDeviceResponse"
    "fixture/AdminGetDeviceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminGetDevice)

responseGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifierResponse -> TestTree
responseGetIdentityProviderByIdentifier =
  res
    "GetIdentityProviderByIdentifierResponse"
    "fixture/GetIdentityProviderByIdentifierResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GetIdentityProviderByIdentifier)

responseAdminRemoveUserFromGroup :: AdminRemoveUserFromGroupResponse -> TestTree
responseAdminRemoveUserFromGroup =
  res
    "AdminRemoveUserFromGroupResponse"
    "fixture/AdminRemoveUserFromGroupResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminRemoveUserFromGroup)

responseSetRiskConfiguration :: SetRiskConfigurationResponse -> TestTree
responseSetRiskConfiguration =
  res
    "SetRiskConfigurationResponse"
    "fixture/SetRiskConfigurationResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy SetRiskConfiguration)

responseConfirmSignUp :: ConfirmSignUpResponse -> TestTree
responseConfirmSignUp =
  res
    "ConfirmSignUpResponse"
    "fixture/ConfirmSignUpResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ConfirmSignUp)

responseListUserPools :: ListUserPoolsResponse -> TestTree
responseListUserPools =
  res
    "ListUserPoolsResponse"
    "fixture/ListUserPoolsResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListUserPools)

responseAdminResetUserPassword :: AdminResetUserPasswordResponse -> TestTree
responseAdminResetUserPassword =
  res
    "AdminResetUserPasswordResponse"
    "fixture/AdminResetUserPasswordResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminResetUserPassword)

responseUpdateAuthEventFeedback :: UpdateAuthEventFeedbackResponse -> TestTree
responseUpdateAuthEventFeedback =
  res
    "UpdateAuthEventFeedbackResponse"
    "fixture/UpdateAuthEventFeedbackResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UpdateAuthEventFeedback)

responseCreateUserImportJob :: CreateUserImportJobResponse -> TestTree
responseCreateUserImportJob =
  res
    "CreateUserImportJobResponse"
    "fixture/CreateUserImportJobResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy CreateUserImportJob)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GetUser)

responseGetUICustomization :: GetUICustomizationResponse -> TestTree
responseGetUICustomization =
  res
    "GetUICustomizationResponse"
    "fixture/GetUICustomizationResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GetUICustomization)

responseGetCSVHeader :: GetCSVHeaderResponse -> TestTree
responseGetCSVHeader =
  res
    "GetCSVHeaderResponse"
    "fixture/GetCSVHeaderResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GetCSVHeader)

responseAdminDeleteUser :: AdminDeleteUserResponse -> TestTree
responseAdminDeleteUser =
  res
    "AdminDeleteUserResponse"
    "fixture/AdminDeleteUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminDeleteUser)

responseAdminForgetDevice :: AdminForgetDeviceResponse -> TestTree
responseAdminForgetDevice =
  res
    "AdminForgetDeviceResponse"
    "fixture/AdminForgetDeviceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminForgetDevice)

responseDescribeResourceServer :: DescribeResourceServerResponse -> TestTree
responseDescribeResourceServer =
  res
    "DescribeResourceServerResponse"
    "fixture/DescribeResourceServerResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DescribeResourceServer)

responseSetUserMFAPreference :: SetUserMFAPreferenceResponse -> TestTree
responseSetUserMFAPreference =
  res
    "SetUserMFAPreferenceResponse"
    "fixture/SetUserMFAPreferenceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy SetUserMFAPreference)

responseAdminUpdateDeviceStatus :: AdminUpdateDeviceStatusResponse -> TestTree
responseAdminUpdateDeviceStatus =
  res
    "AdminUpdateDeviceStatusResponse"
    "fixture/AdminUpdateDeviceStatusResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminUpdateDeviceStatus)

responseAdminCreateUser :: AdminCreateUserResponse -> TestTree
responseAdminCreateUser =
  res
    "AdminCreateUserResponse"
    "fixture/AdminCreateUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminCreateUser)

responseAddCustomAttributes :: AddCustomAttributesResponse -> TestTree
responseAddCustomAttributes =
  res
    "AddCustomAttributesResponse"
    "fixture/AddCustomAttributesResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AddCustomAttributes)

responseListUserPoolClients :: ListUserPoolClientsResponse -> TestTree
responseListUserPoolClients =
  res
    "ListUserPoolClientsResponse"
    "fixture/ListUserPoolClientsResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListUserPoolClients)

responseAdminSetUserMFAPreference :: AdminSetUserMFAPreferenceResponse -> TestTree
responseAdminSetUserMFAPreference =
  res
    "AdminSetUserMFAPreferenceResponse"
    "fixture/AdminSetUserMFAPreferenceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminSetUserMFAPreference)

responseUpdateUserPoolClient :: UpdateUserPoolClientResponse -> TestTree
responseUpdateUserPoolClient =
  res
    "UpdateUserPoolClientResponse"
    "fixture/UpdateUserPoolClientResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UpdateUserPoolClient)

responseDeleteUserPoolClient :: DeleteUserPoolClientResponse -> TestTree
responseDeleteUserPoolClient =
  res
    "DeleteUserPoolClientResponse"
    "fixture/DeleteUserPoolClientResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DeleteUserPoolClient)

responseUpdateDeviceStatus :: UpdateDeviceStatusResponse -> TestTree
responseUpdateDeviceStatus =
  res
    "UpdateDeviceStatusResponse"
    "fixture/UpdateDeviceStatusResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UpdateDeviceStatus)

responseForgetDevice :: ForgetDeviceResponse -> TestTree
responseForgetDevice =
  res
    "ForgetDeviceResponse"
    "fixture/ForgetDeviceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ForgetDevice)

responseGetSigningCertificate :: GetSigningCertificateResponse -> TestTree
responseGetSigningCertificate =
  res
    "GetSigningCertificateResponse"
    "fixture/GetSigningCertificateResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GetSigningCertificate)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DeleteUser)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy TagResource)

responseCreateUserPoolClient :: CreateUserPoolClientResponse -> TestTree
responseCreateUserPoolClient =
  res
    "CreateUserPoolClientResponse"
    "fixture/CreateUserPoolClientResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy CreateUserPoolClient)

responseGetUserPoolMFAConfig :: GetUserPoolMFAConfigResponse -> TestTree
responseGetUserPoolMFAConfig =
  res
    "GetUserPoolMFAConfigResponse"
    "fixture/GetUserPoolMFAConfigResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GetUserPoolMFAConfig)

responseCreateResourceServer :: CreateResourceServerResponse -> TestTree
responseCreateResourceServer =
  res
    "CreateResourceServerResponse"
    "fixture/CreateResourceServerResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy CreateResourceServer)

responseAdminListUserAuthEvents :: AdminListUserAuthEventsResponse -> TestTree
responseAdminListUserAuthEvents =
  res
    "AdminListUserAuthEventsResponse"
    "fixture/AdminListUserAuthEventsResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminListUserAuthEvents)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy CreateGroup)

responseAdminAddUserToGroup :: AdminAddUserToGroupResponse -> TestTree
responseAdminAddUserToGroup =
  res
    "AdminAddUserToGroupResponse"
    "fixture/AdminAddUserToGroupResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminAddUserToGroup)

responseVerifySoftwareToken :: VerifySoftwareTokenResponse -> TestTree
responseVerifySoftwareToken =
  res
    "VerifySoftwareTokenResponse"
    "fixture/VerifySoftwareTokenResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy VerifySoftwareToken)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UntagResource)

responseStopUserImportJob :: StopUserImportJobResponse -> TestTree
responseStopUserImportJob =
  res
    "StopUserImportJobResponse"
    "fixture/StopUserImportJobResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy StopUserImportJob)

responseDescribeUserImportJob :: DescribeUserImportJobResponse -> TestTree
responseDescribeUserImportJob =
  res
    "DescribeUserImportJobResponse"
    "fixture/DescribeUserImportJobResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DescribeUserImportJob)

responseDescribeRiskConfiguration :: DescribeRiskConfigurationResponse -> TestTree
responseDescribeRiskConfiguration =
  res
    "DescribeRiskConfigurationResponse"
    "fixture/DescribeRiskConfigurationResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DescribeRiskConfiguration)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UpdateGroup)

responseGlobalSignOut :: GlobalSignOutResponse -> TestTree
responseGlobalSignOut =
  res
    "GlobalSignOutResponse"
    "fixture/GlobalSignOutResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GlobalSignOut)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListGroups)

responseUpdateIdentityProvider :: UpdateIdentityProviderResponse -> TestTree
responseUpdateIdentityProvider =
  res
    "UpdateIdentityProviderResponse"
    "fixture/UpdateIdentityProviderResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy UpdateIdentityProvider)

responseDeleteIdentityProvider :: DeleteIdentityProviderResponse -> TestTree
responseDeleteIdentityProvider =
  res
    "DeleteIdentityProviderResponse"
    "fixture/DeleteIdentityProviderResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DeleteIdentityProvider)

responseListResourceServers :: ListResourceServersResponse -> TestTree
responseListResourceServers =
  res
    "ListResourceServersResponse"
    "fixture/ListResourceServersResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListResourceServers)

responseAdminRespondToAuthChallenge :: AdminRespondToAuthChallengeResponse -> TestTree
responseAdminRespondToAuthChallenge =
  res
    "AdminRespondToAuthChallengeResponse"
    "fixture/AdminRespondToAuthChallengeResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminRespondToAuthChallenge)

responseSetUserSettings :: SetUserSettingsResponse -> TestTree
responseSetUserSettings =
  res
    "SetUserSettingsResponse"
    "fixture/SetUserSettingsResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy SetUserSettings)

responseAdminListDevices :: AdminListDevicesResponse -> TestTree
responseAdminListDevices =
  res
    "AdminListDevicesResponse"
    "fixture/AdminListDevicesResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminListDevices)

responseDescribeUserPoolClient :: DescribeUserPoolClientResponse -> TestTree
responseDescribeUserPoolClient =
  res
    "DescribeUserPoolClientResponse"
    "fixture/DescribeUserPoolClientResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy DescribeUserPoolClient)

responseResendConfirmationCode :: ResendConfirmationCodeResponse -> TestTree
responseResendConfirmationCode =
  res
    "ResendConfirmationCodeResponse"
    "fixture/ResendConfirmationCodeResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ResendConfirmationCode)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy GetGroup)

responseAdminSetUserSettings :: AdminSetUserSettingsResponse -> TestTree
responseAdminSetUserSettings =
  res
    "AdminSetUserSettingsResponse"
    "fixture/AdminSetUserSettingsResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy AdminSetUserSettings)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    cognitoIdentityProviderService
    (Proxy :: Proxy ListDevices)
