{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CognitoIdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CognitoIdentityProvider where

import Amazonka.CognitoIdentityProvider
import qualified Data.Proxy as Proxy
import Test.Amazonka.CognitoIdentityProvider.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddCustomAttributes $
--             newAddCustomAttributes
--
--         , requestAdminAddUserToGroup $
--             newAdminAddUserToGroup
--
--         , requestAdminConfirmSignUp $
--             newAdminConfirmSignUp
--
--         , requestAdminCreateUser $
--             newAdminCreateUser
--
--         , requestAdminDeleteUser $
--             newAdminDeleteUser
--
--         , requestAdminDeleteUserAttributes $
--             newAdminDeleteUserAttributes
--
--         , requestAdminDisableProviderForUser $
--             newAdminDisableProviderForUser
--
--         , requestAdminDisableUser $
--             newAdminDisableUser
--
--         , requestAdminEnableUser $
--             newAdminEnableUser
--
--         , requestAdminForgetDevice $
--             newAdminForgetDevice
--
--         , requestAdminGetDevice $
--             newAdminGetDevice
--
--         , requestAdminGetUser $
--             newAdminGetUser
--
--         , requestAdminInitiateAuth $
--             newAdminInitiateAuth
--
--         , requestAdminLinkProviderForUser $
--             newAdminLinkProviderForUser
--
--         , requestAdminListDevices $
--             newAdminListDevices
--
--         , requestAdminListGroupsForUser $
--             newAdminListGroupsForUser
--
--         , requestAdminListUserAuthEvents $
--             newAdminListUserAuthEvents
--
--         , requestAdminRemoveUserFromGroup $
--             newAdminRemoveUserFromGroup
--
--         , requestAdminResetUserPassword $
--             newAdminResetUserPassword
--
--         , requestAdminRespondToAuthChallenge $
--             newAdminRespondToAuthChallenge
--
--         , requestAdminSetUserMFAPreference $
--             newAdminSetUserMFAPreference
--
--         , requestAdminSetUserPassword $
--             newAdminSetUserPassword
--
--         , requestAdminSetUserSettings $
--             newAdminSetUserSettings
--
--         , requestAdminUpdateAuthEventFeedback $
--             newAdminUpdateAuthEventFeedback
--
--         , requestAdminUpdateDeviceStatus $
--             newAdminUpdateDeviceStatus
--
--         , requestAdminUpdateUserAttributes $
--             newAdminUpdateUserAttributes
--
--         , requestAdminUserGlobalSignOut $
--             newAdminUserGlobalSignOut
--
--         , requestAssociateSoftwareToken $
--             newAssociateSoftwareToken
--
--         , requestChangePassword $
--             newChangePassword
--
--         , requestConfirmDevice $
--             newConfirmDevice
--
--         , requestConfirmForgotPassword $
--             newConfirmForgotPassword
--
--         , requestConfirmSignUp $
--             newConfirmSignUp
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestCreateIdentityProvider $
--             newCreateIdentityProvider
--
--         , requestCreateResourceServer $
--             newCreateResourceServer
--
--         , requestCreateUserImportJob $
--             newCreateUserImportJob
--
--         , requestCreateUserPool $
--             newCreateUserPool
--
--         , requestCreateUserPoolClient $
--             newCreateUserPoolClient
--
--         , requestCreateUserPoolDomain $
--             newCreateUserPoolDomain
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestDeleteIdentityProvider $
--             newDeleteIdentityProvider
--
--         , requestDeleteResourceServer $
--             newDeleteResourceServer
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeleteUserAttributes $
--             newDeleteUserAttributes
--
--         , requestDeleteUserPool $
--             newDeleteUserPool
--
--         , requestDeleteUserPoolClient $
--             newDeleteUserPoolClient
--
--         , requestDeleteUserPoolDomain $
--             newDeleteUserPoolDomain
--
--         , requestDescribeIdentityProvider $
--             newDescribeIdentityProvider
--
--         , requestDescribeResourceServer $
--             newDescribeResourceServer
--
--         , requestDescribeRiskConfiguration $
--             newDescribeRiskConfiguration
--
--         , requestDescribeUserImportJob $
--             newDescribeUserImportJob
--
--         , requestDescribeUserPool $
--             newDescribeUserPool
--
--         , requestDescribeUserPoolClient $
--             newDescribeUserPoolClient
--
--         , requestDescribeUserPoolDomain $
--             newDescribeUserPoolDomain
--
--         , requestForgetDevice $
--             newForgetDevice
--
--         , requestForgotPassword $
--             newForgotPassword
--
--         , requestGetCSVHeader $
--             newGetCSVHeader
--
--         , requestGetDevice $
--             newGetDevice
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestGetIdentityProviderByIdentifier $
--             newGetIdentityProviderByIdentifier
--
--         , requestGetSigningCertificate $
--             newGetSigningCertificate
--
--         , requestGetUICustomization $
--             newGetUICustomization
--
--         , requestGetUser $
--             newGetUser
--
--         , requestGetUserAttributeVerificationCode $
--             newGetUserAttributeVerificationCode
--
--         , requestGetUserPoolMfaConfig $
--             newGetUserPoolMfaConfig
--
--         , requestGlobalSignOut $
--             newGlobalSignOut
--
--         , requestInitiateAuth $
--             newInitiateAuth
--
--         , requestListDevices $
--             newListDevices
--
--         , requestListGroups $
--             newListGroups
--
--         , requestListIdentityProviders $
--             newListIdentityProviders
--
--         , requestListResourceServers $
--             newListResourceServers
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListUserImportJobs $
--             newListUserImportJobs
--
--         , requestListUserPoolClients $
--             newListUserPoolClients
--
--         , requestListUserPools $
--             newListUserPools
--
--         , requestListUsers $
--             newListUsers
--
--         , requestListUsersInGroup $
--             newListUsersInGroup
--
--         , requestResendConfirmationCode $
--             newResendConfirmationCode
--
--         , requestRespondToAuthChallenge $
--             newRespondToAuthChallenge
--
--         , requestRevokeToken $
--             newRevokeToken
--
--         , requestSetRiskConfiguration $
--             newSetRiskConfiguration
--
--         , requestSetUICustomization $
--             newSetUICustomization
--
--         , requestSetUserMFAPreference $
--             newSetUserMFAPreference
--
--         , requestSetUserPoolMfaConfig $
--             newSetUserPoolMfaConfig
--
--         , requestSetUserSettings $
--             newSetUserSettings
--
--         , requestSignUp $
--             newSignUp
--
--         , requestStartUserImportJob $
--             newStartUserImportJob
--
--         , requestStopUserImportJob $
--             newStopUserImportJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAuthEventFeedback $
--             newUpdateAuthEventFeedback
--
--         , requestUpdateDeviceStatus $
--             newUpdateDeviceStatus
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestUpdateIdentityProvider $
--             newUpdateIdentityProvider
--
--         , requestUpdateResourceServer $
--             newUpdateResourceServer
--
--         , requestUpdateUserAttributes $
--             newUpdateUserAttributes
--
--         , requestUpdateUserPool $
--             newUpdateUserPool
--
--         , requestUpdateUserPoolClient $
--             newUpdateUserPoolClient
--
--         , requestUpdateUserPoolDomain $
--             newUpdateUserPoolDomain
--
--         , requestVerifySoftwareToken $
--             newVerifySoftwareToken
--
--         , requestVerifyUserAttribute $
--             newVerifyUserAttribute
--
--           ]

--     , testGroup "response"
--         [ responseAddCustomAttributes $
--             newAddCustomAttributesResponse
--
--         , responseAdminAddUserToGroup $
--             newAdminAddUserToGroupResponse
--
--         , responseAdminConfirmSignUp $
--             newAdminConfirmSignUpResponse
--
--         , responseAdminCreateUser $
--             newAdminCreateUserResponse
--
--         , responseAdminDeleteUser $
--             newAdminDeleteUserResponse
--
--         , responseAdminDeleteUserAttributes $
--             newAdminDeleteUserAttributesResponse
--
--         , responseAdminDisableProviderForUser $
--             newAdminDisableProviderForUserResponse
--
--         , responseAdminDisableUser $
--             newAdminDisableUserResponse
--
--         , responseAdminEnableUser $
--             newAdminEnableUserResponse
--
--         , responseAdminForgetDevice $
--             newAdminForgetDeviceResponse
--
--         , responseAdminGetDevice $
--             newAdminGetDeviceResponse
--
--         , responseAdminGetUser $
--             newAdminGetUserResponse
--
--         , responseAdminInitiateAuth $
--             newAdminInitiateAuthResponse
--
--         , responseAdminLinkProviderForUser $
--             newAdminLinkProviderForUserResponse
--
--         , responseAdminListDevices $
--             newAdminListDevicesResponse
--
--         , responseAdminListGroupsForUser $
--             newAdminListGroupsForUserResponse
--
--         , responseAdminListUserAuthEvents $
--             newAdminListUserAuthEventsResponse
--
--         , responseAdminRemoveUserFromGroup $
--             newAdminRemoveUserFromGroupResponse
--
--         , responseAdminResetUserPassword $
--             newAdminResetUserPasswordResponse
--
--         , responseAdminRespondToAuthChallenge $
--             newAdminRespondToAuthChallengeResponse
--
--         , responseAdminSetUserMFAPreference $
--             newAdminSetUserMFAPreferenceResponse
--
--         , responseAdminSetUserPassword $
--             newAdminSetUserPasswordResponse
--
--         , responseAdminSetUserSettings $
--             newAdminSetUserSettingsResponse
--
--         , responseAdminUpdateAuthEventFeedback $
--             newAdminUpdateAuthEventFeedbackResponse
--
--         , responseAdminUpdateDeviceStatus $
--             newAdminUpdateDeviceStatusResponse
--
--         , responseAdminUpdateUserAttributes $
--             newAdminUpdateUserAttributesResponse
--
--         , responseAdminUserGlobalSignOut $
--             newAdminUserGlobalSignOutResponse
--
--         , responseAssociateSoftwareToken $
--             newAssociateSoftwareTokenResponse
--
--         , responseChangePassword $
--             newChangePasswordResponse
--
--         , responseConfirmDevice $
--             newConfirmDeviceResponse
--
--         , responseConfirmForgotPassword $
--             newConfirmForgotPasswordResponse
--
--         , responseConfirmSignUp $
--             newConfirmSignUpResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseCreateIdentityProvider $
--             newCreateIdentityProviderResponse
--
--         , responseCreateResourceServer $
--             newCreateResourceServerResponse
--
--         , responseCreateUserImportJob $
--             newCreateUserImportJobResponse
--
--         , responseCreateUserPool $
--             newCreateUserPoolResponse
--
--         , responseCreateUserPoolClient $
--             newCreateUserPoolClientResponse
--
--         , responseCreateUserPoolDomain $
--             newCreateUserPoolDomainResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseDeleteIdentityProvider $
--             newDeleteIdentityProviderResponse
--
--         , responseDeleteResourceServer $
--             newDeleteResourceServerResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDeleteUserAttributes $
--             newDeleteUserAttributesResponse
--
--         , responseDeleteUserPool $
--             newDeleteUserPoolResponse
--
--         , responseDeleteUserPoolClient $
--             newDeleteUserPoolClientResponse
--
--         , responseDeleteUserPoolDomain $
--             newDeleteUserPoolDomainResponse
--
--         , responseDescribeIdentityProvider $
--             newDescribeIdentityProviderResponse
--
--         , responseDescribeResourceServer $
--             newDescribeResourceServerResponse
--
--         , responseDescribeRiskConfiguration $
--             newDescribeRiskConfigurationResponse
--
--         , responseDescribeUserImportJob $
--             newDescribeUserImportJobResponse
--
--         , responseDescribeUserPool $
--             newDescribeUserPoolResponse
--
--         , responseDescribeUserPoolClient $
--             newDescribeUserPoolClientResponse
--
--         , responseDescribeUserPoolDomain $
--             newDescribeUserPoolDomainResponse
--
--         , responseForgetDevice $
--             newForgetDeviceResponse
--
--         , responseForgotPassword $
--             newForgotPasswordResponse
--
--         , responseGetCSVHeader $
--             newGetCSVHeaderResponse
--
--         , responseGetDevice $
--             newGetDeviceResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseGetIdentityProviderByIdentifier $
--             newGetIdentityProviderByIdentifierResponse
--
--         , responseGetSigningCertificate $
--             newGetSigningCertificateResponse
--
--         , responseGetUICustomization $
--             newGetUICustomizationResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseGetUserAttributeVerificationCode $
--             newGetUserAttributeVerificationCodeResponse
--
--         , responseGetUserPoolMfaConfig $
--             newGetUserPoolMfaConfigResponse
--
--         , responseGlobalSignOut $
--             newGlobalSignOutResponse
--
--         , responseInitiateAuth $
--             newInitiateAuthResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseListIdentityProviders $
--             newListIdentityProvidersResponse
--
--         , responseListResourceServers $
--             newListResourceServersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListUserImportJobs $
--             newListUserImportJobsResponse
--
--         , responseListUserPoolClients $
--             newListUserPoolClientsResponse
--
--         , responseListUserPools $
--             newListUserPoolsResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseListUsersInGroup $
--             newListUsersInGroupResponse
--
--         , responseResendConfirmationCode $
--             newResendConfirmationCodeResponse
--
--         , responseRespondToAuthChallenge $
--             newRespondToAuthChallengeResponse
--
--         , responseRevokeToken $
--             newRevokeTokenResponse
--
--         , responseSetRiskConfiguration $
--             newSetRiskConfigurationResponse
--
--         , responseSetUICustomization $
--             newSetUICustomizationResponse
--
--         , responseSetUserMFAPreference $
--             newSetUserMFAPreferenceResponse
--
--         , responseSetUserPoolMfaConfig $
--             newSetUserPoolMfaConfigResponse
--
--         , responseSetUserSettings $
--             newSetUserSettingsResponse
--
--         , responseSignUp $
--             newSignUpResponse
--
--         , responseStartUserImportJob $
--             newStartUserImportJobResponse
--
--         , responseStopUserImportJob $
--             newStopUserImportJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAuthEventFeedback $
--             newUpdateAuthEventFeedbackResponse
--
--         , responseUpdateDeviceStatus $
--             newUpdateDeviceStatusResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseUpdateIdentityProvider $
--             newUpdateIdentityProviderResponse
--
--         , responseUpdateResourceServer $
--             newUpdateResourceServerResponse
--
--         , responseUpdateUserAttributes $
--             newUpdateUserAttributesResponse
--
--         , responseUpdateUserPool $
--             newUpdateUserPoolResponse
--
--         , responseUpdateUserPoolClient $
--             newUpdateUserPoolClientResponse
--
--         , responseUpdateUserPoolDomain $
--             newUpdateUserPoolDomainResponse
--
--         , responseVerifySoftwareToken $
--             newVerifySoftwareTokenResponse
--
--         , responseVerifyUserAttribute $
--             newVerifyUserAttributeResponse
--
--           ]
--     ]

-- Requests

requestAddCustomAttributes :: AddCustomAttributes -> TestTree
requestAddCustomAttributes =
  req
    "AddCustomAttributes"
    "fixture/AddCustomAttributes.yaml"

requestAdminAddUserToGroup :: AdminAddUserToGroup -> TestTree
requestAdminAddUserToGroup =
  req
    "AdminAddUserToGroup"
    "fixture/AdminAddUserToGroup.yaml"

requestAdminConfirmSignUp :: AdminConfirmSignUp -> TestTree
requestAdminConfirmSignUp =
  req
    "AdminConfirmSignUp"
    "fixture/AdminConfirmSignUp.yaml"

requestAdminCreateUser :: AdminCreateUser -> TestTree
requestAdminCreateUser =
  req
    "AdminCreateUser"
    "fixture/AdminCreateUser.yaml"

requestAdminDeleteUser :: AdminDeleteUser -> TestTree
requestAdminDeleteUser =
  req
    "AdminDeleteUser"
    "fixture/AdminDeleteUser.yaml"

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

requestAdminDisableUser :: AdminDisableUser -> TestTree
requestAdminDisableUser =
  req
    "AdminDisableUser"
    "fixture/AdminDisableUser.yaml"

requestAdminEnableUser :: AdminEnableUser -> TestTree
requestAdminEnableUser =
  req
    "AdminEnableUser"
    "fixture/AdminEnableUser.yaml"

requestAdminForgetDevice :: AdminForgetDevice -> TestTree
requestAdminForgetDevice =
  req
    "AdminForgetDevice"
    "fixture/AdminForgetDevice.yaml"

requestAdminGetDevice :: AdminGetDevice -> TestTree
requestAdminGetDevice =
  req
    "AdminGetDevice"
    "fixture/AdminGetDevice.yaml"

requestAdminGetUser :: AdminGetUser -> TestTree
requestAdminGetUser =
  req
    "AdminGetUser"
    "fixture/AdminGetUser.yaml"

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

requestAdminListDevices :: AdminListDevices -> TestTree
requestAdminListDevices =
  req
    "AdminListDevices"
    "fixture/AdminListDevices.yaml"

requestAdminListGroupsForUser :: AdminListGroupsForUser -> TestTree
requestAdminListGroupsForUser =
  req
    "AdminListGroupsForUser"
    "fixture/AdminListGroupsForUser.yaml"

requestAdminListUserAuthEvents :: AdminListUserAuthEvents -> TestTree
requestAdminListUserAuthEvents =
  req
    "AdminListUserAuthEvents"
    "fixture/AdminListUserAuthEvents.yaml"

requestAdminRemoveUserFromGroup :: AdminRemoveUserFromGroup -> TestTree
requestAdminRemoveUserFromGroup =
  req
    "AdminRemoveUserFromGroup"
    "fixture/AdminRemoveUserFromGroup.yaml"

requestAdminResetUserPassword :: AdminResetUserPassword -> TestTree
requestAdminResetUserPassword =
  req
    "AdminResetUserPassword"
    "fixture/AdminResetUserPassword.yaml"

requestAdminRespondToAuthChallenge :: AdminRespondToAuthChallenge -> TestTree
requestAdminRespondToAuthChallenge =
  req
    "AdminRespondToAuthChallenge"
    "fixture/AdminRespondToAuthChallenge.yaml"

requestAdminSetUserMFAPreference :: AdminSetUserMFAPreference -> TestTree
requestAdminSetUserMFAPreference =
  req
    "AdminSetUserMFAPreference"
    "fixture/AdminSetUserMFAPreference.yaml"

requestAdminSetUserPassword :: AdminSetUserPassword -> TestTree
requestAdminSetUserPassword =
  req
    "AdminSetUserPassword"
    "fixture/AdminSetUserPassword.yaml"

requestAdminSetUserSettings :: AdminSetUserSettings -> TestTree
requestAdminSetUserSettings =
  req
    "AdminSetUserSettings"
    "fixture/AdminSetUserSettings.yaml"

requestAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedback -> TestTree
requestAdminUpdateAuthEventFeedback =
  req
    "AdminUpdateAuthEventFeedback"
    "fixture/AdminUpdateAuthEventFeedback.yaml"

requestAdminUpdateDeviceStatus :: AdminUpdateDeviceStatus -> TestTree
requestAdminUpdateDeviceStatus =
  req
    "AdminUpdateDeviceStatus"
    "fixture/AdminUpdateDeviceStatus.yaml"

requestAdminUpdateUserAttributes :: AdminUpdateUserAttributes -> TestTree
requestAdminUpdateUserAttributes =
  req
    "AdminUpdateUserAttributes"
    "fixture/AdminUpdateUserAttributes.yaml"

requestAdminUserGlobalSignOut :: AdminUserGlobalSignOut -> TestTree
requestAdminUserGlobalSignOut =
  req
    "AdminUserGlobalSignOut"
    "fixture/AdminUserGlobalSignOut.yaml"

requestAssociateSoftwareToken :: AssociateSoftwareToken -> TestTree
requestAssociateSoftwareToken =
  req
    "AssociateSoftwareToken"
    "fixture/AssociateSoftwareToken.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword =
  req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

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

requestConfirmSignUp :: ConfirmSignUp -> TestTree
requestConfirmSignUp =
  req
    "ConfirmSignUp"
    "fixture/ConfirmSignUp.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestCreateIdentityProvider :: CreateIdentityProvider -> TestTree
requestCreateIdentityProvider =
  req
    "CreateIdentityProvider"
    "fixture/CreateIdentityProvider.yaml"

requestCreateResourceServer :: CreateResourceServer -> TestTree
requestCreateResourceServer =
  req
    "CreateResourceServer"
    "fixture/CreateResourceServer.yaml"

requestCreateUserImportJob :: CreateUserImportJob -> TestTree
requestCreateUserImportJob =
  req
    "CreateUserImportJob"
    "fixture/CreateUserImportJob.yaml"

requestCreateUserPool :: CreateUserPool -> TestTree
requestCreateUserPool =
  req
    "CreateUserPool"
    "fixture/CreateUserPool.yaml"

requestCreateUserPoolClient :: CreateUserPoolClient -> TestTree
requestCreateUserPoolClient =
  req
    "CreateUserPoolClient"
    "fixture/CreateUserPoolClient.yaml"

requestCreateUserPoolDomain :: CreateUserPoolDomain -> TestTree
requestCreateUserPoolDomain =
  req
    "CreateUserPoolDomain"
    "fixture/CreateUserPoolDomain.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestDeleteIdentityProvider :: DeleteIdentityProvider -> TestTree
requestDeleteIdentityProvider =
  req
    "DeleteIdentityProvider"
    "fixture/DeleteIdentityProvider.yaml"

requestDeleteResourceServer :: DeleteResourceServer -> TestTree
requestDeleteResourceServer =
  req
    "DeleteResourceServer"
    "fixture/DeleteResourceServer.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDeleteUserAttributes :: DeleteUserAttributes -> TestTree
requestDeleteUserAttributes =
  req
    "DeleteUserAttributes"
    "fixture/DeleteUserAttributes.yaml"

requestDeleteUserPool :: DeleteUserPool -> TestTree
requestDeleteUserPool =
  req
    "DeleteUserPool"
    "fixture/DeleteUserPool.yaml"

requestDeleteUserPoolClient :: DeleteUserPoolClient -> TestTree
requestDeleteUserPoolClient =
  req
    "DeleteUserPoolClient"
    "fixture/DeleteUserPoolClient.yaml"

requestDeleteUserPoolDomain :: DeleteUserPoolDomain -> TestTree
requestDeleteUserPoolDomain =
  req
    "DeleteUserPoolDomain"
    "fixture/DeleteUserPoolDomain.yaml"

requestDescribeIdentityProvider :: DescribeIdentityProvider -> TestTree
requestDescribeIdentityProvider =
  req
    "DescribeIdentityProvider"
    "fixture/DescribeIdentityProvider.yaml"

requestDescribeResourceServer :: DescribeResourceServer -> TestTree
requestDescribeResourceServer =
  req
    "DescribeResourceServer"
    "fixture/DescribeResourceServer.yaml"

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

requestDescribeUserPool :: DescribeUserPool -> TestTree
requestDescribeUserPool =
  req
    "DescribeUserPool"
    "fixture/DescribeUserPool.yaml"

requestDescribeUserPoolClient :: DescribeUserPoolClient -> TestTree
requestDescribeUserPoolClient =
  req
    "DescribeUserPoolClient"
    "fixture/DescribeUserPoolClient.yaml"

requestDescribeUserPoolDomain :: DescribeUserPoolDomain -> TestTree
requestDescribeUserPoolDomain =
  req
    "DescribeUserPoolDomain"
    "fixture/DescribeUserPoolDomain.yaml"

requestForgetDevice :: ForgetDevice -> TestTree
requestForgetDevice =
  req
    "ForgetDevice"
    "fixture/ForgetDevice.yaml"

requestForgotPassword :: ForgotPassword -> TestTree
requestForgotPassword =
  req
    "ForgotPassword"
    "fixture/ForgotPassword.yaml"

requestGetCSVHeader :: GetCSVHeader -> TestTree
requestGetCSVHeader =
  req
    "GetCSVHeader"
    "fixture/GetCSVHeader.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice =
  req
    "GetDevice"
    "fixture/GetDevice.yaml"

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

requestGetSigningCertificate :: GetSigningCertificate -> TestTree
requestGetSigningCertificate =
  req
    "GetSigningCertificate"
    "fixture/GetSigningCertificate.yaml"

requestGetUICustomization :: GetUICustomization -> TestTree
requestGetUICustomization =
  req
    "GetUICustomization"
    "fixture/GetUICustomization.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser =
  req
    "GetUser"
    "fixture/GetUser.yaml"

requestGetUserAttributeVerificationCode :: GetUserAttributeVerificationCode -> TestTree
requestGetUserAttributeVerificationCode =
  req
    "GetUserAttributeVerificationCode"
    "fixture/GetUserAttributeVerificationCode.yaml"

requestGetUserPoolMfaConfig :: GetUserPoolMfaConfig -> TestTree
requestGetUserPoolMfaConfig =
  req
    "GetUserPoolMfaConfig"
    "fixture/GetUserPoolMfaConfig.yaml"

requestGlobalSignOut :: GlobalSignOut -> TestTree
requestGlobalSignOut =
  req
    "GlobalSignOut"
    "fixture/GlobalSignOut.yaml"

requestInitiateAuth :: InitiateAuth -> TestTree
requestInitiateAuth =
  req
    "InitiateAuth"
    "fixture/InitiateAuth.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestListIdentityProviders :: ListIdentityProviders -> TestTree
requestListIdentityProviders =
  req
    "ListIdentityProviders"
    "fixture/ListIdentityProviders.yaml"

requestListResourceServers :: ListResourceServers -> TestTree
requestListResourceServers =
  req
    "ListResourceServers"
    "fixture/ListResourceServers.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListUserImportJobs :: ListUserImportJobs -> TestTree
requestListUserImportJobs =
  req
    "ListUserImportJobs"
    "fixture/ListUserImportJobs.yaml"

requestListUserPoolClients :: ListUserPoolClients -> TestTree
requestListUserPoolClients =
  req
    "ListUserPoolClients"
    "fixture/ListUserPoolClients.yaml"

requestListUserPools :: ListUserPools -> TestTree
requestListUserPools =
  req
    "ListUserPools"
    "fixture/ListUserPools.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListUsersInGroup :: ListUsersInGroup -> TestTree
requestListUsersInGroup =
  req
    "ListUsersInGroup"
    "fixture/ListUsersInGroup.yaml"

requestResendConfirmationCode :: ResendConfirmationCode -> TestTree
requestResendConfirmationCode =
  req
    "ResendConfirmationCode"
    "fixture/ResendConfirmationCode.yaml"

requestRespondToAuthChallenge :: RespondToAuthChallenge -> TestTree
requestRespondToAuthChallenge =
  req
    "RespondToAuthChallenge"
    "fixture/RespondToAuthChallenge.yaml"

requestRevokeToken :: RevokeToken -> TestTree
requestRevokeToken =
  req
    "RevokeToken"
    "fixture/RevokeToken.yaml"

requestSetRiskConfiguration :: SetRiskConfiguration -> TestTree
requestSetRiskConfiguration =
  req
    "SetRiskConfiguration"
    "fixture/SetRiskConfiguration.yaml"

requestSetUICustomization :: SetUICustomization -> TestTree
requestSetUICustomization =
  req
    "SetUICustomization"
    "fixture/SetUICustomization.yaml"

requestSetUserMFAPreference :: SetUserMFAPreference -> TestTree
requestSetUserMFAPreference =
  req
    "SetUserMFAPreference"
    "fixture/SetUserMFAPreference.yaml"

requestSetUserPoolMfaConfig :: SetUserPoolMfaConfig -> TestTree
requestSetUserPoolMfaConfig =
  req
    "SetUserPoolMfaConfig"
    "fixture/SetUserPoolMfaConfig.yaml"

requestSetUserSettings :: SetUserSettings -> TestTree
requestSetUserSettings =
  req
    "SetUserSettings"
    "fixture/SetUserSettings.yaml"

requestSignUp :: SignUp -> TestTree
requestSignUp =
  req
    "SignUp"
    "fixture/SignUp.yaml"

requestStartUserImportJob :: StartUserImportJob -> TestTree
requestStartUserImportJob =
  req
    "StartUserImportJob"
    "fixture/StartUserImportJob.yaml"

requestStopUserImportJob :: StopUserImportJob -> TestTree
requestStopUserImportJob =
  req
    "StopUserImportJob"
    "fixture/StopUserImportJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAuthEventFeedback :: UpdateAuthEventFeedback -> TestTree
requestUpdateAuthEventFeedback =
  req
    "UpdateAuthEventFeedback"
    "fixture/UpdateAuthEventFeedback.yaml"

requestUpdateDeviceStatus :: UpdateDeviceStatus -> TestTree
requestUpdateDeviceStatus =
  req
    "UpdateDeviceStatus"
    "fixture/UpdateDeviceStatus.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestUpdateIdentityProvider :: UpdateIdentityProvider -> TestTree
requestUpdateIdentityProvider =
  req
    "UpdateIdentityProvider"
    "fixture/UpdateIdentityProvider.yaml"

requestUpdateResourceServer :: UpdateResourceServer -> TestTree
requestUpdateResourceServer =
  req
    "UpdateResourceServer"
    "fixture/UpdateResourceServer.yaml"

requestUpdateUserAttributes :: UpdateUserAttributes -> TestTree
requestUpdateUserAttributes =
  req
    "UpdateUserAttributes"
    "fixture/UpdateUserAttributes.yaml"

requestUpdateUserPool :: UpdateUserPool -> TestTree
requestUpdateUserPool =
  req
    "UpdateUserPool"
    "fixture/UpdateUserPool.yaml"

requestUpdateUserPoolClient :: UpdateUserPoolClient -> TestTree
requestUpdateUserPoolClient =
  req
    "UpdateUserPoolClient"
    "fixture/UpdateUserPoolClient.yaml"

requestUpdateUserPoolDomain :: UpdateUserPoolDomain -> TestTree
requestUpdateUserPoolDomain =
  req
    "UpdateUserPoolDomain"
    "fixture/UpdateUserPoolDomain.yaml"

requestVerifySoftwareToken :: VerifySoftwareToken -> TestTree
requestVerifySoftwareToken =
  req
    "VerifySoftwareToken"
    "fixture/VerifySoftwareToken.yaml"

requestVerifyUserAttribute :: VerifyUserAttribute -> TestTree
requestVerifyUserAttribute =
  req
    "VerifyUserAttribute"
    "fixture/VerifyUserAttribute.yaml"

-- Responses

responseAddCustomAttributes :: AddCustomAttributesResponse -> TestTree
responseAddCustomAttributes =
  res
    "AddCustomAttributesResponse"
    "fixture/AddCustomAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddCustomAttributes)

responseAdminAddUserToGroup :: AdminAddUserToGroupResponse -> TestTree
responseAdminAddUserToGroup =
  res
    "AdminAddUserToGroupResponse"
    "fixture/AdminAddUserToGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminAddUserToGroup)

responseAdminConfirmSignUp :: AdminConfirmSignUpResponse -> TestTree
responseAdminConfirmSignUp =
  res
    "AdminConfirmSignUpResponse"
    "fixture/AdminConfirmSignUpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminConfirmSignUp)

responseAdminCreateUser :: AdminCreateUserResponse -> TestTree
responseAdminCreateUser =
  res
    "AdminCreateUserResponse"
    "fixture/AdminCreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminCreateUser)

responseAdminDeleteUser :: AdminDeleteUserResponse -> TestTree
responseAdminDeleteUser =
  res
    "AdminDeleteUserResponse"
    "fixture/AdminDeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminDeleteUser)

responseAdminDeleteUserAttributes :: AdminDeleteUserAttributesResponse -> TestTree
responseAdminDeleteUserAttributes =
  res
    "AdminDeleteUserAttributesResponse"
    "fixture/AdminDeleteUserAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminDeleteUserAttributes)

responseAdminDisableProviderForUser :: AdminDisableProviderForUserResponse -> TestTree
responseAdminDisableProviderForUser =
  res
    "AdminDisableProviderForUserResponse"
    "fixture/AdminDisableProviderForUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminDisableProviderForUser)

responseAdminDisableUser :: AdminDisableUserResponse -> TestTree
responseAdminDisableUser =
  res
    "AdminDisableUserResponse"
    "fixture/AdminDisableUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminDisableUser)

responseAdminEnableUser :: AdminEnableUserResponse -> TestTree
responseAdminEnableUser =
  res
    "AdminEnableUserResponse"
    "fixture/AdminEnableUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminEnableUser)

responseAdminForgetDevice :: AdminForgetDeviceResponse -> TestTree
responseAdminForgetDevice =
  res
    "AdminForgetDeviceResponse"
    "fixture/AdminForgetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminForgetDevice)

responseAdminGetDevice :: AdminGetDeviceResponse -> TestTree
responseAdminGetDevice =
  res
    "AdminGetDeviceResponse"
    "fixture/AdminGetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminGetDevice)

responseAdminGetUser :: AdminGetUserResponse -> TestTree
responseAdminGetUser =
  res
    "AdminGetUserResponse"
    "fixture/AdminGetUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminGetUser)

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

responseAdminListDevices :: AdminListDevicesResponse -> TestTree
responseAdminListDevices =
  res
    "AdminListDevicesResponse"
    "fixture/AdminListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminListDevices)

responseAdminListGroupsForUser :: AdminListGroupsForUserResponse -> TestTree
responseAdminListGroupsForUser =
  res
    "AdminListGroupsForUserResponse"
    "fixture/AdminListGroupsForUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminListGroupsForUser)

responseAdminListUserAuthEvents :: AdminListUserAuthEventsResponse -> TestTree
responseAdminListUserAuthEvents =
  res
    "AdminListUserAuthEventsResponse"
    "fixture/AdminListUserAuthEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminListUserAuthEvents)

responseAdminRemoveUserFromGroup :: AdminRemoveUserFromGroupResponse -> TestTree
responseAdminRemoveUserFromGroup =
  res
    "AdminRemoveUserFromGroupResponse"
    "fixture/AdminRemoveUserFromGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminRemoveUserFromGroup)

responseAdminResetUserPassword :: AdminResetUserPasswordResponse -> TestTree
responseAdminResetUserPassword =
  res
    "AdminResetUserPasswordResponse"
    "fixture/AdminResetUserPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminResetUserPassword)

responseAdminRespondToAuthChallenge :: AdminRespondToAuthChallengeResponse -> TestTree
responseAdminRespondToAuthChallenge =
  res
    "AdminRespondToAuthChallengeResponse"
    "fixture/AdminRespondToAuthChallengeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminRespondToAuthChallenge)

responseAdminSetUserMFAPreference :: AdminSetUserMFAPreferenceResponse -> TestTree
responseAdminSetUserMFAPreference =
  res
    "AdminSetUserMFAPreferenceResponse"
    "fixture/AdminSetUserMFAPreferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminSetUserMFAPreference)

responseAdminSetUserPassword :: AdminSetUserPasswordResponse -> TestTree
responseAdminSetUserPassword =
  res
    "AdminSetUserPasswordResponse"
    "fixture/AdminSetUserPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminSetUserPassword)

responseAdminSetUserSettings :: AdminSetUserSettingsResponse -> TestTree
responseAdminSetUserSettings =
  res
    "AdminSetUserSettingsResponse"
    "fixture/AdminSetUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminSetUserSettings)

responseAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedbackResponse -> TestTree
responseAdminUpdateAuthEventFeedback =
  res
    "AdminUpdateAuthEventFeedbackResponse"
    "fixture/AdminUpdateAuthEventFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminUpdateAuthEventFeedback)

responseAdminUpdateDeviceStatus :: AdminUpdateDeviceStatusResponse -> TestTree
responseAdminUpdateDeviceStatus =
  res
    "AdminUpdateDeviceStatusResponse"
    "fixture/AdminUpdateDeviceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminUpdateDeviceStatus)

responseAdminUpdateUserAttributes :: AdminUpdateUserAttributesResponse -> TestTree
responseAdminUpdateUserAttributes =
  res
    "AdminUpdateUserAttributesResponse"
    "fixture/AdminUpdateUserAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminUpdateUserAttributes)

responseAdminUserGlobalSignOut :: AdminUserGlobalSignOutResponse -> TestTree
responseAdminUserGlobalSignOut =
  res
    "AdminUserGlobalSignOutResponse"
    "fixture/AdminUserGlobalSignOutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AdminUserGlobalSignOut)

responseAssociateSoftwareToken :: AssociateSoftwareTokenResponse -> TestTree
responseAssociateSoftwareToken =
  res
    "AssociateSoftwareTokenResponse"
    "fixture/AssociateSoftwareTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateSoftwareToken)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword =
  res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangePassword)

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

responseConfirmSignUp :: ConfirmSignUpResponse -> TestTree
responseConfirmSignUp =
  res
    "ConfirmSignUpResponse"
    "fixture/ConfirmSignUpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmSignUp)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseCreateIdentityProvider :: CreateIdentityProviderResponse -> TestTree
responseCreateIdentityProvider =
  res
    "CreateIdentityProviderResponse"
    "fixture/CreateIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIdentityProvider)

responseCreateResourceServer :: CreateResourceServerResponse -> TestTree
responseCreateResourceServer =
  res
    "CreateResourceServerResponse"
    "fixture/CreateResourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceServer)

responseCreateUserImportJob :: CreateUserImportJobResponse -> TestTree
responseCreateUserImportJob =
  res
    "CreateUserImportJobResponse"
    "fixture/CreateUserImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserImportJob)

responseCreateUserPool :: CreateUserPoolResponse -> TestTree
responseCreateUserPool =
  res
    "CreateUserPoolResponse"
    "fixture/CreateUserPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserPool)

responseCreateUserPoolClient :: CreateUserPoolClientResponse -> TestTree
responseCreateUserPoolClient =
  res
    "CreateUserPoolClientResponse"
    "fixture/CreateUserPoolClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserPoolClient)

responseCreateUserPoolDomain :: CreateUserPoolDomainResponse -> TestTree
responseCreateUserPoolDomain =
  res
    "CreateUserPoolDomainResponse"
    "fixture/CreateUserPoolDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserPoolDomain)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseDeleteIdentityProvider :: DeleteIdentityProviderResponse -> TestTree
responseDeleteIdentityProvider =
  res
    "DeleteIdentityProviderResponse"
    "fixture/DeleteIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIdentityProvider)

responseDeleteResourceServer :: DeleteResourceServerResponse -> TestTree
responseDeleteResourceServer =
  res
    "DeleteResourceServerResponse"
    "fixture/DeleteResourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceServer)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDeleteUserAttributes :: DeleteUserAttributesResponse -> TestTree
responseDeleteUserAttributes =
  res
    "DeleteUserAttributesResponse"
    "fixture/DeleteUserAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserAttributes)

responseDeleteUserPool :: DeleteUserPoolResponse -> TestTree
responseDeleteUserPool =
  res
    "DeleteUserPoolResponse"
    "fixture/DeleteUserPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserPool)

responseDeleteUserPoolClient :: DeleteUserPoolClientResponse -> TestTree
responseDeleteUserPoolClient =
  res
    "DeleteUserPoolClientResponse"
    "fixture/DeleteUserPoolClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserPoolClient)

responseDeleteUserPoolDomain :: DeleteUserPoolDomainResponse -> TestTree
responseDeleteUserPoolDomain =
  res
    "DeleteUserPoolDomainResponse"
    "fixture/DeleteUserPoolDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserPoolDomain)

responseDescribeIdentityProvider :: DescribeIdentityProviderResponse -> TestTree
responseDescribeIdentityProvider =
  res
    "DescribeIdentityProviderResponse"
    "fixture/DescribeIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityProvider)

responseDescribeResourceServer :: DescribeResourceServerResponse -> TestTree
responseDescribeResourceServer =
  res
    "DescribeResourceServerResponse"
    "fixture/DescribeResourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourceServer)

responseDescribeRiskConfiguration :: DescribeRiskConfigurationResponse -> TestTree
responseDescribeRiskConfiguration =
  res
    "DescribeRiskConfigurationResponse"
    "fixture/DescribeRiskConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRiskConfiguration)

responseDescribeUserImportJob :: DescribeUserImportJobResponse -> TestTree
responseDescribeUserImportJob =
  res
    "DescribeUserImportJobResponse"
    "fixture/DescribeUserImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserImportJob)

responseDescribeUserPool :: DescribeUserPoolResponse -> TestTree
responseDescribeUserPool =
  res
    "DescribeUserPoolResponse"
    "fixture/DescribeUserPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserPool)

responseDescribeUserPoolClient :: DescribeUserPoolClientResponse -> TestTree
responseDescribeUserPoolClient =
  res
    "DescribeUserPoolClientResponse"
    "fixture/DescribeUserPoolClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserPoolClient)

responseDescribeUserPoolDomain :: DescribeUserPoolDomainResponse -> TestTree
responseDescribeUserPoolDomain =
  res
    "DescribeUserPoolDomainResponse"
    "fixture/DescribeUserPoolDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserPoolDomain)

responseForgetDevice :: ForgetDeviceResponse -> TestTree
responseForgetDevice =
  res
    "ForgetDeviceResponse"
    "fixture/ForgetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ForgetDevice)

responseForgotPassword :: ForgotPasswordResponse -> TestTree
responseForgotPassword =
  res
    "ForgotPasswordResponse"
    "fixture/ForgotPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ForgotPassword)

responseGetCSVHeader :: GetCSVHeaderResponse -> TestTree
responseGetCSVHeader =
  res
    "GetCSVHeaderResponse"
    "fixture/GetCSVHeaderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCSVHeader)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice =
  res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevice)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroup)

responseGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifierResponse -> TestTree
responseGetIdentityProviderByIdentifier =
  res
    "GetIdentityProviderByIdentifierResponse"
    "fixture/GetIdentityProviderByIdentifierResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityProviderByIdentifier)

responseGetSigningCertificate :: GetSigningCertificateResponse -> TestTree
responseGetSigningCertificate =
  res
    "GetSigningCertificateResponse"
    "fixture/GetSigningCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSigningCertificate)

responseGetUICustomization :: GetUICustomizationResponse -> TestTree
responseGetUICustomization =
  res
    "GetUICustomizationResponse"
    "fixture/GetUICustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUICustomization)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUser)

responseGetUserAttributeVerificationCode :: GetUserAttributeVerificationCodeResponse -> TestTree
responseGetUserAttributeVerificationCode =
  res
    "GetUserAttributeVerificationCodeResponse"
    "fixture/GetUserAttributeVerificationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserAttributeVerificationCode)

responseGetUserPoolMfaConfig :: GetUserPoolMfaConfigResponse -> TestTree
responseGetUserPoolMfaConfig =
  res
    "GetUserPoolMfaConfigResponse"
    "fixture/GetUserPoolMfaConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUserPoolMfaConfig)

responseGlobalSignOut :: GlobalSignOutResponse -> TestTree
responseGlobalSignOut =
  res
    "GlobalSignOutResponse"
    "fixture/GlobalSignOutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GlobalSignOut)

responseInitiateAuth :: InitiateAuthResponse -> TestTree
responseInitiateAuth =
  res
    "InitiateAuthResponse"
    "fixture/InitiateAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitiateAuth)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseListIdentityProviders :: ListIdentityProvidersResponse -> TestTree
responseListIdentityProviders =
  res
    "ListIdentityProvidersResponse"
    "fixture/ListIdentityProvidersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityProviders)

responseListResourceServers :: ListResourceServersResponse -> TestTree
responseListResourceServers =
  res
    "ListResourceServersResponse"
    "fixture/ListResourceServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceServers)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListUserImportJobs :: ListUserImportJobsResponse -> TestTree
responseListUserImportJobs =
  res
    "ListUserImportJobsResponse"
    "fixture/ListUserImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserImportJobs)

responseListUserPoolClients :: ListUserPoolClientsResponse -> TestTree
responseListUserPoolClients =
  res
    "ListUserPoolClientsResponse"
    "fixture/ListUserPoolClientsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserPoolClients)

responseListUserPools :: ListUserPoolsResponse -> TestTree
responseListUserPools =
  res
    "ListUserPoolsResponse"
    "fixture/ListUserPoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserPools)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseListUsersInGroup :: ListUsersInGroupResponse -> TestTree
responseListUsersInGroup =
  res
    "ListUsersInGroupResponse"
    "fixture/ListUsersInGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsersInGroup)

responseResendConfirmationCode :: ResendConfirmationCodeResponse -> TestTree
responseResendConfirmationCode =
  res
    "ResendConfirmationCodeResponse"
    "fixture/ResendConfirmationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResendConfirmationCode)

responseRespondToAuthChallenge :: RespondToAuthChallengeResponse -> TestTree
responseRespondToAuthChallenge =
  res
    "RespondToAuthChallengeResponse"
    "fixture/RespondToAuthChallengeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RespondToAuthChallenge)

responseRevokeToken :: RevokeTokenResponse -> TestTree
responseRevokeToken =
  res
    "RevokeTokenResponse"
    "fixture/RevokeTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeToken)

responseSetRiskConfiguration :: SetRiskConfigurationResponse -> TestTree
responseSetRiskConfiguration =
  res
    "SetRiskConfigurationResponse"
    "fixture/SetRiskConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetRiskConfiguration)

responseSetUICustomization :: SetUICustomizationResponse -> TestTree
responseSetUICustomization =
  res
    "SetUICustomizationResponse"
    "fixture/SetUICustomizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetUICustomization)

responseSetUserMFAPreference :: SetUserMFAPreferenceResponse -> TestTree
responseSetUserMFAPreference =
  res
    "SetUserMFAPreferenceResponse"
    "fixture/SetUserMFAPreferenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetUserMFAPreference)

responseSetUserPoolMfaConfig :: SetUserPoolMfaConfigResponse -> TestTree
responseSetUserPoolMfaConfig =
  res
    "SetUserPoolMfaConfigResponse"
    "fixture/SetUserPoolMfaConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetUserPoolMfaConfig)

responseSetUserSettings :: SetUserSettingsResponse -> TestTree
responseSetUserSettings =
  res
    "SetUserSettingsResponse"
    "fixture/SetUserSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetUserSettings)

responseSignUp :: SignUpResponse -> TestTree
responseSignUp =
  res
    "SignUpResponse"
    "fixture/SignUpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SignUp)

responseStartUserImportJob :: StartUserImportJobResponse -> TestTree
responseStartUserImportJob =
  res
    "StartUserImportJobResponse"
    "fixture/StartUserImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartUserImportJob)

responseStopUserImportJob :: StopUserImportJobResponse -> TestTree
responseStopUserImportJob =
  res
    "StopUserImportJobResponse"
    "fixture/StopUserImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopUserImportJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAuthEventFeedback :: UpdateAuthEventFeedbackResponse -> TestTree
responseUpdateAuthEventFeedback =
  res
    "UpdateAuthEventFeedbackResponse"
    "fixture/UpdateAuthEventFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAuthEventFeedback)

responseUpdateDeviceStatus :: UpdateDeviceStatusResponse -> TestTree
responseUpdateDeviceStatus =
  res
    "UpdateDeviceStatusResponse"
    "fixture/UpdateDeviceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceStatus)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseUpdateIdentityProvider :: UpdateIdentityProviderResponse -> TestTree
responseUpdateIdentityProvider =
  res
    "UpdateIdentityProviderResponse"
    "fixture/UpdateIdentityProviderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIdentityProvider)

responseUpdateResourceServer :: UpdateResourceServerResponse -> TestTree
responseUpdateResourceServer =
  res
    "UpdateResourceServerResponse"
    "fixture/UpdateResourceServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceServer)

responseUpdateUserAttributes :: UpdateUserAttributesResponse -> TestTree
responseUpdateUserAttributes =
  res
    "UpdateUserAttributesResponse"
    "fixture/UpdateUserAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserAttributes)

responseUpdateUserPool :: UpdateUserPoolResponse -> TestTree
responseUpdateUserPool =
  res
    "UpdateUserPoolResponse"
    "fixture/UpdateUserPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserPool)

responseUpdateUserPoolClient :: UpdateUserPoolClientResponse -> TestTree
responseUpdateUserPoolClient =
  res
    "UpdateUserPoolClientResponse"
    "fixture/UpdateUserPoolClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserPoolClient)

responseUpdateUserPoolDomain :: UpdateUserPoolDomainResponse -> TestTree
responseUpdateUserPoolDomain =
  res
    "UpdateUserPoolDomainResponse"
    "fixture/UpdateUserPoolDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserPoolDomain)

responseVerifySoftwareToken :: VerifySoftwareTokenResponse -> TestTree
responseVerifySoftwareToken =
  res
    "VerifySoftwareTokenResponse"
    "fixture/VerifySoftwareTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifySoftwareToken)

responseVerifyUserAttribute :: VerifyUserAttributeResponse -> TestTree
responseVerifyUserAttribute =
  res
    "VerifyUserAttributeResponse"
    "fixture/VerifyUserAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyUserAttribute)
