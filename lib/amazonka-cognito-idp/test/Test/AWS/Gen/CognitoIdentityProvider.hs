{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoIdentityProvider
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             deleteUserPool
--
--         , requestUpdateUserPool $
--             updateUserPool
--
--         , requestDeleteUserPoolDomain $
--             deleteUserPoolDomain
--
--         , requestAdminInitiateAuth $
--             adminInitiateAuth
--
--         , requestAdminLinkProviderForUser $
--             adminLinkProviderForUser
--
--         , requestAdminEnableUser $
--             adminEnableUser
--
--         , requestGetUserAttributeVerificationCode $
--             getUserAttributeVerificationCode
--
--         , requestSetUserPoolMFAConfig $
--             setUserPoolMFAConfig
--
--         , requestUpdateUserAttributes $
--             updateUserAttributes
--
--         , requestDeleteUserAttributes $
--             deleteUserAttributes
--
--         , requestVerifyUserAttribute $
--             verifyUserAttribute
--
--         , requestAdminDisableUser $
--             adminDisableUser
--
--         , requestConfirmDevice $
--             confirmDevice
--
--         , requestConfirmForgotPassword $
--             confirmForgotPassword
--
--         , requestListUserImportJobs $
--             listUserImportJobs
--
--         , requestDescribeIdentityProvider $
--             describeIdentityProvider
--
--         , requestListUsers $
--             listUsers
--
--         , requestAdminDeleteUserAttributes $
--             adminDeleteUserAttributes
--
--         , requestDescribeUserPoolDomain $
--             describeUserPoolDomain
--
--         , requestAdminUpdateUserAttributes $
--             adminUpdateUserAttributes
--
--         , requestAdminGetUser $
--             adminGetUser
--
--         , requestAdminUserGlobalSignOut $
--             adminUserGlobalSignOut
--
--         , requestListUsersInGroup $
--             listUsersInGroup
--
--         , requestAssociateSoftwareToken $
--             associateSoftwareToken
--
--         , requestAdminDisableProviderForUser $
--             adminDisableProviderForUser
--
--         , requestForgotPassword $
--             forgotPassword
--
--         , requestDescribeUserPool $
--             describeUserPool
--
--         , requestInitiateAuth $
--             initiateAuth
--
--         , requestAdminListGroupsForUser $
--             adminListGroupsForUser
--
--         , requestAdminConfirmSignUp $
--             adminConfirmSignUp
--
--         , requestAdminUpdateAuthEventFeedback $
--             adminUpdateAuthEventFeedback
--
--         , requestStartUserImportJob $
--             startUserImportJob
--
--         , requestCreateIdentityProvider $
--             createIdentityProvider
--
--         , requestSetUICustomization $
--             setUICustomization
--
--         , requestListIdentityProviders $
--             listIdentityProviders
--
--         , requestGetDevice $
--             getDevice
--
--         , requestSignUp $
--             signUp
--
--         , requestDeleteResourceServer $
--             deleteResourceServer
--
--         , requestUpdateResourceServer $
--             updateResourceServer
--
--         , requestChangePassword $
--             changePassword
--
--         , requestCreateUserPoolDomain $
--             createUserPoolDomain
--
--         , requestRespondToAuthChallenge $
--             respondToAuthChallenge
--
--         , requestCreateUserPool $
--             createUserPool
--
--         , requestAdminGetDevice $
--             adminGetDevice
--
--         , requestGetIdentityProviderByIdentifier $
--             getIdentityProviderByIdentifier
--
--         , requestAdminRemoveUserFromGroup $
--             adminRemoveUserFromGroup
--
--         , requestSetRiskConfiguration $
--             setRiskConfiguration
--
--         , requestConfirmSignUp $
--             confirmSignUp
--
--         , requestListUserPools $
--             listUserPools
--
--         , requestAdminResetUserPassword $
--             adminResetUserPassword
--
--         , requestUpdateAuthEventFeedback $
--             updateAuthEventFeedback
--
--         , requestCreateUserImportJob $
--             createUserImportJob
--
--         , requestGetUser $
--             getUser
--
--         , requestGetUICustomization $
--             getUICustomization
--
--         , requestGetCSVHeader $
--             getCSVHeader
--
--         , requestAdminDeleteUser $
--             adminDeleteUser
--
--         , requestAdminForgetDevice $
--             adminForgetDevice
--
--         , requestDescribeResourceServer $
--             describeResourceServer
--
--         , requestSetUserMFAPreference $
--             setUserMFAPreference
--
--         , requestAdminUpdateDeviceStatus $
--             adminUpdateDeviceStatus
--
--         , requestAdminCreateUser $
--             adminCreateUser
--
--         , requestAddCustomAttributes $
--             addCustomAttributes
--
--         , requestListUserPoolClients $
--             listUserPoolClients
--
--         , requestAdminSetUserMFAPreference $
--             adminSetUserMFAPreference
--
--         , requestUpdateUserPoolClient $
--             updateUserPoolClient
--
--         , requestDeleteUserPoolClient $
--             deleteUserPoolClient
--
--         , requestUpdateDeviceStatus $
--             updateDeviceStatus
--
--         , requestForgetDevice $
--             forgetDevice
--
--         , requestGetSigningCertificate $
--             getSigningCertificate
--
--         , requestDeleteUser $
--             deleteUser
--
--         , requestCreateUserPoolClient $
--             createUserPoolClient
--
--         , requestGetUserPoolMFAConfig $
--             getUserPoolMFAConfig
--
--         , requestCreateResourceServer $
--             createResourceServer
--
--         , requestAdminListUserAuthEvents $
--             adminListUserAuthEvents
--
--         , requestCreateGroup $
--             createGroup
--
--         , requestAdminAddUserToGroup $
--             adminAddUserToGroup
--
--         , requestVerifySoftwareToken $
--             verifySoftwareToken
--
--         , requestStopUserImportJob $
--             stopUserImportJob
--
--         , requestDescribeUserImportJob $
--             describeUserImportJob
--
--         , requestDescribeRiskConfiguration $
--             describeRiskConfiguration
--
--         , requestDeleteGroup $
--             deleteGroup
--
--         , requestUpdateGroup $
--             updateGroup
--
--         , requestGlobalSignOut $
--             globalSignOut
--
--         , requestListGroups $
--             listGroups
--
--         , requestUpdateIdentityProvider $
--             updateIdentityProvider
--
--         , requestDeleteIdentityProvider $
--             deleteIdentityProvider
--
--         , requestListResourceServers $
--             listResourceServers
--
--         , requestAdminRespondToAuthChallenge $
--             adminRespondToAuthChallenge
--
--         , requestSetUserSettings $
--             setUserSettings
--
--         , requestAdminListDevices $
--             adminListDevices
--
--         , requestDescribeUserPoolClient $
--             describeUserPoolClient
--
--         , requestResendConfirmationCode $
--             resendConfirmationCode
--
--         , requestGetGroup $
--             getGroup
--
--         , requestAdminSetUserSettings $
--             adminSetUserSettings
--
--         , requestListDevices $
--             listDevices
--
--           ]

--     , testGroup "response"
--         [ responseDeleteUserPool $
--             deleteUserPoolResponse
--
--         , responseUpdateUserPool $
--             updateUserPoolResponse
--
--         , responseDeleteUserPoolDomain $
--             deleteUserPoolDomainResponse
--
--         , responseAdminInitiateAuth $
--             adminInitiateAuthResponse
--
--         , responseAdminLinkProviderForUser $
--             adminLinkProviderForUserResponse
--
--         , responseAdminEnableUser $
--             adminEnableUserResponse
--
--         , responseGetUserAttributeVerificationCode $
--             getUserAttributeVerificationCodeResponse
--
--         , responseSetUserPoolMFAConfig $
--             setUserPoolMFAConfigResponse
--
--         , responseUpdateUserAttributes $
--             updateUserAttributesResponse
--
--         , responseDeleteUserAttributes $
--             deleteUserAttributesResponse
--
--         , responseVerifyUserAttribute $
--             verifyUserAttributeResponse
--
--         , responseAdminDisableUser $
--             adminDisableUserResponse
--
--         , responseConfirmDevice $
--             confirmDeviceResponse
--
--         , responseConfirmForgotPassword $
--             confirmForgotPasswordResponse
--
--         , responseListUserImportJobs $
--             listUserImportJobsResponse
--
--         , responseDescribeIdentityProvider $
--             describeIdentityProviderResponse
--
--         , responseListUsers $
--             listUsersResponse
--
--         , responseAdminDeleteUserAttributes $
--             adminDeleteUserAttributesResponse
--
--         , responseDescribeUserPoolDomain $
--             describeUserPoolDomainResponse
--
--         , responseAdminUpdateUserAttributes $
--             adminUpdateUserAttributesResponse
--
--         , responseAdminGetUser $
--             adminGetUserResponse
--
--         , responseAdminUserGlobalSignOut $
--             adminUserGlobalSignOutResponse
--
--         , responseListUsersInGroup $
--             listUsersInGroupResponse
--
--         , responseAssociateSoftwareToken $
--             associateSoftwareTokenResponse
--
--         , responseAdminDisableProviderForUser $
--             adminDisableProviderForUserResponse
--
--         , responseForgotPassword $
--             forgotPasswordResponse
--
--         , responseDescribeUserPool $
--             describeUserPoolResponse
--
--         , responseInitiateAuth $
--             initiateAuthResponse
--
--         , responseAdminListGroupsForUser $
--             adminListGroupsForUserResponse
--
--         , responseAdminConfirmSignUp $
--             adminConfirmSignUpResponse
--
--         , responseAdminUpdateAuthEventFeedback $
--             adminUpdateAuthEventFeedbackResponse
--
--         , responseStartUserImportJob $
--             startUserImportJobResponse
--
--         , responseCreateIdentityProvider $
--             createIdentityProviderResponse
--
--         , responseSetUICustomization $
--             setUICustomizationResponse
--
--         , responseListIdentityProviders $
--             listIdentityProvidersResponse
--
--         , responseGetDevice $
--             getDeviceResponse
--
--         , responseSignUp $
--             signUpResponse
--
--         , responseDeleteResourceServer $
--             deleteResourceServerResponse
--
--         , responseUpdateResourceServer $
--             updateResourceServerResponse
--
--         , responseChangePassword $
--             changePasswordResponse
--
--         , responseCreateUserPoolDomain $
--             createUserPoolDomainResponse
--
--         , responseRespondToAuthChallenge $
--             respondToAuthChallengeResponse
--
--         , responseCreateUserPool $
--             createUserPoolResponse
--
--         , responseAdminGetDevice $
--             adminGetDeviceResponse
--
--         , responseGetIdentityProviderByIdentifier $
--             getIdentityProviderByIdentifierResponse
--
--         , responseAdminRemoveUserFromGroup $
--             adminRemoveUserFromGroupResponse
--
--         , responseSetRiskConfiguration $
--             setRiskConfigurationResponse
--
--         , responseConfirmSignUp $
--             confirmSignUpResponse
--
--         , responseListUserPools $
--             listUserPoolsResponse
--
--         , responseAdminResetUserPassword $
--             adminResetUserPasswordResponse
--
--         , responseUpdateAuthEventFeedback $
--             updateAuthEventFeedbackResponse
--
--         , responseCreateUserImportJob $
--             createUserImportJobResponse
--
--         , responseGetUser $
--             getUserResponse
--
--         , responseGetUICustomization $
--             getUICustomizationResponse
--
--         , responseGetCSVHeader $
--             getCSVHeaderResponse
--
--         , responseAdminDeleteUser $
--             adminDeleteUserResponse
--
--         , responseAdminForgetDevice $
--             adminForgetDeviceResponse
--
--         , responseDescribeResourceServer $
--             describeResourceServerResponse
--
--         , responseSetUserMFAPreference $
--             setUserMFAPreferenceResponse
--
--         , responseAdminUpdateDeviceStatus $
--             adminUpdateDeviceStatusResponse
--
--         , responseAdminCreateUser $
--             adminCreateUserResponse
--
--         , responseAddCustomAttributes $
--             addCustomAttributesResponse
--
--         , responseListUserPoolClients $
--             listUserPoolClientsResponse
--
--         , responseAdminSetUserMFAPreference $
--             adminSetUserMFAPreferenceResponse
--
--         , responseUpdateUserPoolClient $
--             updateUserPoolClientResponse
--
--         , responseDeleteUserPoolClient $
--             deleteUserPoolClientResponse
--
--         , responseUpdateDeviceStatus $
--             updateDeviceStatusResponse
--
--         , responseForgetDevice $
--             forgetDeviceResponse
--
--         , responseGetSigningCertificate $
--             getSigningCertificateResponse
--
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseCreateUserPoolClient $
--             createUserPoolClientResponse
--
--         , responseGetUserPoolMFAConfig $
--             getUserPoolMFAConfigResponse
--
--         , responseCreateResourceServer $
--             createResourceServerResponse
--
--         , responseAdminListUserAuthEvents $
--             adminListUserAuthEventsResponse
--
--         , responseCreateGroup $
--             createGroupResponse
--
--         , responseAdminAddUserToGroup $
--             adminAddUserToGroupResponse
--
--         , responseVerifySoftwareToken $
--             verifySoftwareTokenResponse
--
--         , responseStopUserImportJob $
--             stopUserImportJobResponse
--
--         , responseDescribeUserImportJob $
--             describeUserImportJobResponse
--
--         , responseDescribeRiskConfiguration $
--             describeRiskConfigurationResponse
--
--         , responseDeleteGroup $
--             deleteGroupResponse
--
--         , responseUpdateGroup $
--             updateGroupResponse
--
--         , responseGlobalSignOut $
--             globalSignOutResponse
--
--         , responseListGroups $
--             listGroupsResponse
--
--         , responseUpdateIdentityProvider $
--             updateIdentityProviderResponse
--
--         , responseDeleteIdentityProvider $
--             deleteIdentityProviderResponse
--
--         , responseListResourceServers $
--             listResourceServersResponse
--
--         , responseAdminRespondToAuthChallenge $
--             adminRespondToAuthChallengeResponse
--
--         , responseSetUserSettings $
--             setUserSettingsResponse
--
--         , responseAdminListDevices $
--             adminListDevicesResponse
--
--         , responseDescribeUserPoolClient $
--             describeUserPoolClientResponse
--
--         , responseResendConfirmationCode $
--             resendConfirmationCodeResponse
--
--         , responseGetGroup $
--             getGroupResponse
--
--         , responseAdminSetUserSettings $
--             adminSetUserSettingsResponse
--
--         , responseListDevices $
--             listDevicesResponse
--
--           ]
--     ]

-- Requests

requestDeleteUserPool :: DeleteUserPool -> TestTree
requestDeleteUserPool = req
    "DeleteUserPool"
    "fixture/DeleteUserPool.yaml"

requestUpdateUserPool :: UpdateUserPool -> TestTree
requestUpdateUserPool = req
    "UpdateUserPool"
    "fixture/UpdateUserPool.yaml"

requestDeleteUserPoolDomain :: DeleteUserPoolDomain -> TestTree
requestDeleteUserPoolDomain = req
    "DeleteUserPoolDomain"
    "fixture/DeleteUserPoolDomain.yaml"

requestAdminInitiateAuth :: AdminInitiateAuth -> TestTree
requestAdminInitiateAuth = req
    "AdminInitiateAuth"
    "fixture/AdminInitiateAuth.yaml"

requestAdminLinkProviderForUser :: AdminLinkProviderForUser -> TestTree
requestAdminLinkProviderForUser = req
    "AdminLinkProviderForUser"
    "fixture/AdminLinkProviderForUser.yaml"

requestAdminEnableUser :: AdminEnableUser -> TestTree
requestAdminEnableUser = req
    "AdminEnableUser"
    "fixture/AdminEnableUser.yaml"

requestGetUserAttributeVerificationCode :: GetUserAttributeVerificationCode -> TestTree
requestGetUserAttributeVerificationCode = req
    "GetUserAttributeVerificationCode"
    "fixture/GetUserAttributeVerificationCode.yaml"

requestSetUserPoolMFAConfig :: SetUserPoolMFAConfig -> TestTree
requestSetUserPoolMFAConfig = req
    "SetUserPoolMFAConfig"
    "fixture/SetUserPoolMFAConfig.yaml"

requestUpdateUserAttributes :: UpdateUserAttributes -> TestTree
requestUpdateUserAttributes = req
    "UpdateUserAttributes"
    "fixture/UpdateUserAttributes.yaml"

requestDeleteUserAttributes :: DeleteUserAttributes -> TestTree
requestDeleteUserAttributes = req
    "DeleteUserAttributes"
    "fixture/DeleteUserAttributes.yaml"

requestVerifyUserAttribute :: VerifyUserAttribute -> TestTree
requestVerifyUserAttribute = req
    "VerifyUserAttribute"
    "fixture/VerifyUserAttribute.yaml"

requestAdminDisableUser :: AdminDisableUser -> TestTree
requestAdminDisableUser = req
    "AdminDisableUser"
    "fixture/AdminDisableUser.yaml"

requestConfirmDevice :: ConfirmDevice -> TestTree
requestConfirmDevice = req
    "ConfirmDevice"
    "fixture/ConfirmDevice.yaml"

requestConfirmForgotPassword :: ConfirmForgotPassword -> TestTree
requestConfirmForgotPassword = req
    "ConfirmForgotPassword"
    "fixture/ConfirmForgotPassword.yaml"

requestListUserImportJobs :: ListUserImportJobs -> TestTree
requestListUserImportJobs = req
    "ListUserImportJobs"
    "fixture/ListUserImportJobs.yaml"

requestDescribeIdentityProvider :: DescribeIdentityProvider -> TestTree
requestDescribeIdentityProvider = req
    "DescribeIdentityProvider"
    "fixture/DescribeIdentityProvider.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestAdminDeleteUserAttributes :: AdminDeleteUserAttributes -> TestTree
requestAdminDeleteUserAttributes = req
    "AdminDeleteUserAttributes"
    "fixture/AdminDeleteUserAttributes.yaml"

requestDescribeUserPoolDomain :: DescribeUserPoolDomain -> TestTree
requestDescribeUserPoolDomain = req
    "DescribeUserPoolDomain"
    "fixture/DescribeUserPoolDomain.yaml"

requestAdminUpdateUserAttributes :: AdminUpdateUserAttributes -> TestTree
requestAdminUpdateUserAttributes = req
    "AdminUpdateUserAttributes"
    "fixture/AdminUpdateUserAttributes.yaml"

requestAdminGetUser :: AdminGetUser -> TestTree
requestAdminGetUser = req
    "AdminGetUser"
    "fixture/AdminGetUser.yaml"

requestAdminUserGlobalSignOut :: AdminUserGlobalSignOut -> TestTree
requestAdminUserGlobalSignOut = req
    "AdminUserGlobalSignOut"
    "fixture/AdminUserGlobalSignOut.yaml"

requestListUsersInGroup :: ListUsersInGroup -> TestTree
requestListUsersInGroup = req
    "ListUsersInGroup"
    "fixture/ListUsersInGroup.yaml"

requestAssociateSoftwareToken :: AssociateSoftwareToken -> TestTree
requestAssociateSoftwareToken = req
    "AssociateSoftwareToken"
    "fixture/AssociateSoftwareToken.yaml"

requestAdminDisableProviderForUser :: AdminDisableProviderForUser -> TestTree
requestAdminDisableProviderForUser = req
    "AdminDisableProviderForUser"
    "fixture/AdminDisableProviderForUser.yaml"

requestForgotPassword :: ForgotPassword -> TestTree
requestForgotPassword = req
    "ForgotPassword"
    "fixture/ForgotPassword.yaml"

requestDescribeUserPool :: DescribeUserPool -> TestTree
requestDescribeUserPool = req
    "DescribeUserPool"
    "fixture/DescribeUserPool.yaml"

requestInitiateAuth :: InitiateAuth -> TestTree
requestInitiateAuth = req
    "InitiateAuth"
    "fixture/InitiateAuth.yaml"

requestAdminListGroupsForUser :: AdminListGroupsForUser -> TestTree
requestAdminListGroupsForUser = req
    "AdminListGroupsForUser"
    "fixture/AdminListGroupsForUser.yaml"

requestAdminConfirmSignUp :: AdminConfirmSignUp -> TestTree
requestAdminConfirmSignUp = req
    "AdminConfirmSignUp"
    "fixture/AdminConfirmSignUp.yaml"

requestAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedback -> TestTree
requestAdminUpdateAuthEventFeedback = req
    "AdminUpdateAuthEventFeedback"
    "fixture/AdminUpdateAuthEventFeedback.yaml"

requestStartUserImportJob :: StartUserImportJob -> TestTree
requestStartUserImportJob = req
    "StartUserImportJob"
    "fixture/StartUserImportJob.yaml"

requestCreateIdentityProvider :: CreateIdentityProvider -> TestTree
requestCreateIdentityProvider = req
    "CreateIdentityProvider"
    "fixture/CreateIdentityProvider.yaml"

requestSetUICustomization :: SetUICustomization -> TestTree
requestSetUICustomization = req
    "SetUICustomization"
    "fixture/SetUICustomization.yaml"

requestListIdentityProviders :: ListIdentityProviders -> TestTree
requestListIdentityProviders = req
    "ListIdentityProviders"
    "fixture/ListIdentityProviders.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice = req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestSignUp :: SignUp -> TestTree
requestSignUp = req
    "SignUp"
    "fixture/SignUp.yaml"

requestDeleteResourceServer :: DeleteResourceServer -> TestTree
requestDeleteResourceServer = req
    "DeleteResourceServer"
    "fixture/DeleteResourceServer.yaml"

requestUpdateResourceServer :: UpdateResourceServer -> TestTree
requestUpdateResourceServer = req
    "UpdateResourceServer"
    "fixture/UpdateResourceServer.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword = req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

requestCreateUserPoolDomain :: CreateUserPoolDomain -> TestTree
requestCreateUserPoolDomain = req
    "CreateUserPoolDomain"
    "fixture/CreateUserPoolDomain.yaml"

requestRespondToAuthChallenge :: RespondToAuthChallenge -> TestTree
requestRespondToAuthChallenge = req
    "RespondToAuthChallenge"
    "fixture/RespondToAuthChallenge.yaml"

requestCreateUserPool :: CreateUserPool -> TestTree
requestCreateUserPool = req
    "CreateUserPool"
    "fixture/CreateUserPool.yaml"

requestAdminGetDevice :: AdminGetDevice -> TestTree
requestAdminGetDevice = req
    "AdminGetDevice"
    "fixture/AdminGetDevice.yaml"

requestGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifier -> TestTree
requestGetIdentityProviderByIdentifier = req
    "GetIdentityProviderByIdentifier"
    "fixture/GetIdentityProviderByIdentifier.yaml"

requestAdminRemoveUserFromGroup :: AdminRemoveUserFromGroup -> TestTree
requestAdminRemoveUserFromGroup = req
    "AdminRemoveUserFromGroup"
    "fixture/AdminRemoveUserFromGroup.yaml"

requestSetRiskConfiguration :: SetRiskConfiguration -> TestTree
requestSetRiskConfiguration = req
    "SetRiskConfiguration"
    "fixture/SetRiskConfiguration.yaml"

requestConfirmSignUp :: ConfirmSignUp -> TestTree
requestConfirmSignUp = req
    "ConfirmSignUp"
    "fixture/ConfirmSignUp.yaml"

requestListUserPools :: ListUserPools -> TestTree
requestListUserPools = req
    "ListUserPools"
    "fixture/ListUserPools.yaml"

requestAdminResetUserPassword :: AdminResetUserPassword -> TestTree
requestAdminResetUserPassword = req
    "AdminResetUserPassword"
    "fixture/AdminResetUserPassword.yaml"

requestUpdateAuthEventFeedback :: UpdateAuthEventFeedback -> TestTree
requestUpdateAuthEventFeedback = req
    "UpdateAuthEventFeedback"
    "fixture/UpdateAuthEventFeedback.yaml"

requestCreateUserImportJob :: CreateUserImportJob -> TestTree
requestCreateUserImportJob = req
    "CreateUserImportJob"
    "fixture/CreateUserImportJob.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser = req
    "GetUser"
    "fixture/GetUser.yaml"

requestGetUICustomization :: GetUICustomization -> TestTree
requestGetUICustomization = req
    "GetUICustomization"
    "fixture/GetUICustomization.yaml"

requestGetCSVHeader :: GetCSVHeader -> TestTree
requestGetCSVHeader = req
    "GetCSVHeader"
    "fixture/GetCSVHeader.yaml"

requestAdminDeleteUser :: AdminDeleteUser -> TestTree
requestAdminDeleteUser = req
    "AdminDeleteUser"
    "fixture/AdminDeleteUser.yaml"

requestAdminForgetDevice :: AdminForgetDevice -> TestTree
requestAdminForgetDevice = req
    "AdminForgetDevice"
    "fixture/AdminForgetDevice.yaml"

requestDescribeResourceServer :: DescribeResourceServer -> TestTree
requestDescribeResourceServer = req
    "DescribeResourceServer"
    "fixture/DescribeResourceServer.yaml"

requestSetUserMFAPreference :: SetUserMFAPreference -> TestTree
requestSetUserMFAPreference = req
    "SetUserMFAPreference"
    "fixture/SetUserMFAPreference.yaml"

requestAdminUpdateDeviceStatus :: AdminUpdateDeviceStatus -> TestTree
requestAdminUpdateDeviceStatus = req
    "AdminUpdateDeviceStatus"
    "fixture/AdminUpdateDeviceStatus.yaml"

requestAdminCreateUser :: AdminCreateUser -> TestTree
requestAdminCreateUser = req
    "AdminCreateUser"
    "fixture/AdminCreateUser.yaml"

requestAddCustomAttributes :: AddCustomAttributes -> TestTree
requestAddCustomAttributes = req
    "AddCustomAttributes"
    "fixture/AddCustomAttributes.yaml"

requestListUserPoolClients :: ListUserPoolClients -> TestTree
requestListUserPoolClients = req
    "ListUserPoolClients"
    "fixture/ListUserPoolClients.yaml"

requestAdminSetUserMFAPreference :: AdminSetUserMFAPreference -> TestTree
requestAdminSetUserMFAPreference = req
    "AdminSetUserMFAPreference"
    "fixture/AdminSetUserMFAPreference.yaml"

requestUpdateUserPoolClient :: UpdateUserPoolClient -> TestTree
requestUpdateUserPoolClient = req
    "UpdateUserPoolClient"
    "fixture/UpdateUserPoolClient.yaml"

requestDeleteUserPoolClient :: DeleteUserPoolClient -> TestTree
requestDeleteUserPoolClient = req
    "DeleteUserPoolClient"
    "fixture/DeleteUserPoolClient.yaml"

requestUpdateDeviceStatus :: UpdateDeviceStatus -> TestTree
requestUpdateDeviceStatus = req
    "UpdateDeviceStatus"
    "fixture/UpdateDeviceStatus.yaml"

requestForgetDevice :: ForgetDevice -> TestTree
requestForgetDevice = req
    "ForgetDevice"
    "fixture/ForgetDevice.yaml"

requestGetSigningCertificate :: GetSigningCertificate -> TestTree
requestGetSigningCertificate = req
    "GetSigningCertificate"
    "fixture/GetSigningCertificate.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestCreateUserPoolClient :: CreateUserPoolClient -> TestTree
requestCreateUserPoolClient = req
    "CreateUserPoolClient"
    "fixture/CreateUserPoolClient.yaml"

requestGetUserPoolMFAConfig :: GetUserPoolMFAConfig -> TestTree
requestGetUserPoolMFAConfig = req
    "GetUserPoolMFAConfig"
    "fixture/GetUserPoolMFAConfig.yaml"

requestCreateResourceServer :: CreateResourceServer -> TestTree
requestCreateResourceServer = req
    "CreateResourceServer"
    "fixture/CreateResourceServer.yaml"

requestAdminListUserAuthEvents :: AdminListUserAuthEvents -> TestTree
requestAdminListUserAuthEvents = req
    "AdminListUserAuthEvents"
    "fixture/AdminListUserAuthEvents.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup = req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestAdminAddUserToGroup :: AdminAddUserToGroup -> TestTree
requestAdminAddUserToGroup = req
    "AdminAddUserToGroup"
    "fixture/AdminAddUserToGroup.yaml"

requestVerifySoftwareToken :: VerifySoftwareToken -> TestTree
requestVerifySoftwareToken = req
    "VerifySoftwareToken"
    "fixture/VerifySoftwareToken.yaml"

requestStopUserImportJob :: StopUserImportJob -> TestTree
requestStopUserImportJob = req
    "StopUserImportJob"
    "fixture/StopUserImportJob.yaml"

requestDescribeUserImportJob :: DescribeUserImportJob -> TestTree
requestDescribeUserImportJob = req
    "DescribeUserImportJob"
    "fixture/DescribeUserImportJob.yaml"

requestDescribeRiskConfiguration :: DescribeRiskConfiguration -> TestTree
requestDescribeRiskConfiguration = req
    "DescribeRiskConfiguration"
    "fixture/DescribeRiskConfiguration.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup = req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup = req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestGlobalSignOut :: GlobalSignOut -> TestTree
requestGlobalSignOut = req
    "GlobalSignOut"
    "fixture/GlobalSignOut.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups = req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestUpdateIdentityProvider :: UpdateIdentityProvider -> TestTree
requestUpdateIdentityProvider = req
    "UpdateIdentityProvider"
    "fixture/UpdateIdentityProvider.yaml"

requestDeleteIdentityProvider :: DeleteIdentityProvider -> TestTree
requestDeleteIdentityProvider = req
    "DeleteIdentityProvider"
    "fixture/DeleteIdentityProvider.yaml"

requestListResourceServers :: ListResourceServers -> TestTree
requestListResourceServers = req
    "ListResourceServers"
    "fixture/ListResourceServers.yaml"

requestAdminRespondToAuthChallenge :: AdminRespondToAuthChallenge -> TestTree
requestAdminRespondToAuthChallenge = req
    "AdminRespondToAuthChallenge"
    "fixture/AdminRespondToAuthChallenge.yaml"

requestSetUserSettings :: SetUserSettings -> TestTree
requestSetUserSettings = req
    "SetUserSettings"
    "fixture/SetUserSettings.yaml"

requestAdminListDevices :: AdminListDevices -> TestTree
requestAdminListDevices = req
    "AdminListDevices"
    "fixture/AdminListDevices.yaml"

requestDescribeUserPoolClient :: DescribeUserPoolClient -> TestTree
requestDescribeUserPoolClient = req
    "DescribeUserPoolClient"
    "fixture/DescribeUserPoolClient.yaml"

requestResendConfirmationCode :: ResendConfirmationCode -> TestTree
requestResendConfirmationCode = req
    "ResendConfirmationCode"
    "fixture/ResendConfirmationCode.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup = req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestAdminSetUserSettings :: AdminSetUserSettings -> TestTree
requestAdminSetUserSettings = req
    "AdminSetUserSettings"
    "fixture/AdminSetUserSettings.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices = req
    "ListDevices"
    "fixture/ListDevices.yaml"

-- Responses

responseDeleteUserPool :: DeleteUserPoolResponse -> TestTree
responseDeleteUserPool = res
    "DeleteUserPoolResponse"
    "fixture/DeleteUserPoolResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteUserPool)

responseUpdateUserPool :: UpdateUserPoolResponse -> TestTree
responseUpdateUserPool = res
    "UpdateUserPoolResponse"
    "fixture/UpdateUserPoolResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateUserPool)

responseDeleteUserPoolDomain :: DeleteUserPoolDomainResponse -> TestTree
responseDeleteUserPoolDomain = res
    "DeleteUserPoolDomainResponse"
    "fixture/DeleteUserPoolDomainResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteUserPoolDomain)

responseAdminInitiateAuth :: AdminInitiateAuthResponse -> TestTree
responseAdminInitiateAuth = res
    "AdminInitiateAuthResponse"
    "fixture/AdminInitiateAuthResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminInitiateAuth)

responseAdminLinkProviderForUser :: AdminLinkProviderForUserResponse -> TestTree
responseAdminLinkProviderForUser = res
    "AdminLinkProviderForUserResponse"
    "fixture/AdminLinkProviderForUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminLinkProviderForUser)

responseAdminEnableUser :: AdminEnableUserResponse -> TestTree
responseAdminEnableUser = res
    "AdminEnableUserResponse"
    "fixture/AdminEnableUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminEnableUser)

responseGetUserAttributeVerificationCode :: GetUserAttributeVerificationCodeResponse -> TestTree
responseGetUserAttributeVerificationCode = res
    "GetUserAttributeVerificationCodeResponse"
    "fixture/GetUserAttributeVerificationCodeResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetUserAttributeVerificationCode)

responseSetUserPoolMFAConfig :: SetUserPoolMFAConfigResponse -> TestTree
responseSetUserPoolMFAConfig = res
    "SetUserPoolMFAConfigResponse"
    "fixture/SetUserPoolMFAConfigResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy SetUserPoolMFAConfig)

responseUpdateUserAttributes :: UpdateUserAttributesResponse -> TestTree
responseUpdateUserAttributes = res
    "UpdateUserAttributesResponse"
    "fixture/UpdateUserAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateUserAttributes)

responseDeleteUserAttributes :: DeleteUserAttributesResponse -> TestTree
responseDeleteUserAttributes = res
    "DeleteUserAttributesResponse"
    "fixture/DeleteUserAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteUserAttributes)

responseVerifyUserAttribute :: VerifyUserAttributeResponse -> TestTree
responseVerifyUserAttribute = res
    "VerifyUserAttributeResponse"
    "fixture/VerifyUserAttributeResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy VerifyUserAttribute)

responseAdminDisableUser :: AdminDisableUserResponse -> TestTree
responseAdminDisableUser = res
    "AdminDisableUserResponse"
    "fixture/AdminDisableUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminDisableUser)

responseConfirmDevice :: ConfirmDeviceResponse -> TestTree
responseConfirmDevice = res
    "ConfirmDeviceResponse"
    "fixture/ConfirmDeviceResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ConfirmDevice)

responseConfirmForgotPassword :: ConfirmForgotPasswordResponse -> TestTree
responseConfirmForgotPassword = res
    "ConfirmForgotPasswordResponse"
    "fixture/ConfirmForgotPasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ConfirmForgotPassword)

responseListUserImportJobs :: ListUserImportJobsResponse -> TestTree
responseListUserImportJobs = res
    "ListUserImportJobsResponse"
    "fixture/ListUserImportJobsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListUserImportJobs)

responseDescribeIdentityProvider :: DescribeIdentityProviderResponse -> TestTree
responseDescribeIdentityProvider = res
    "DescribeIdentityProviderResponse"
    "fixture/DescribeIdentityProviderResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DescribeIdentityProvider)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListUsers)

responseAdminDeleteUserAttributes :: AdminDeleteUserAttributesResponse -> TestTree
responseAdminDeleteUserAttributes = res
    "AdminDeleteUserAttributesResponse"
    "fixture/AdminDeleteUserAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminDeleteUserAttributes)

responseDescribeUserPoolDomain :: DescribeUserPoolDomainResponse -> TestTree
responseDescribeUserPoolDomain = res
    "DescribeUserPoolDomainResponse"
    "fixture/DescribeUserPoolDomainResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DescribeUserPoolDomain)

responseAdminUpdateUserAttributes :: AdminUpdateUserAttributesResponse -> TestTree
responseAdminUpdateUserAttributes = res
    "AdminUpdateUserAttributesResponse"
    "fixture/AdminUpdateUserAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminUpdateUserAttributes)

responseAdminGetUser :: AdminGetUserResponse -> TestTree
responseAdminGetUser = res
    "AdminGetUserResponse"
    "fixture/AdminGetUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminGetUser)

responseAdminUserGlobalSignOut :: AdminUserGlobalSignOutResponse -> TestTree
responseAdminUserGlobalSignOut = res
    "AdminUserGlobalSignOutResponse"
    "fixture/AdminUserGlobalSignOutResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminUserGlobalSignOut)

responseListUsersInGroup :: ListUsersInGroupResponse -> TestTree
responseListUsersInGroup = res
    "ListUsersInGroupResponse"
    "fixture/ListUsersInGroupResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListUsersInGroup)

responseAssociateSoftwareToken :: AssociateSoftwareTokenResponse -> TestTree
responseAssociateSoftwareToken = res
    "AssociateSoftwareTokenResponse"
    "fixture/AssociateSoftwareTokenResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AssociateSoftwareToken)

responseAdminDisableProviderForUser :: AdminDisableProviderForUserResponse -> TestTree
responseAdminDisableProviderForUser = res
    "AdminDisableProviderForUserResponse"
    "fixture/AdminDisableProviderForUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminDisableProviderForUser)

responseForgotPassword :: ForgotPasswordResponse -> TestTree
responseForgotPassword = res
    "ForgotPasswordResponse"
    "fixture/ForgotPasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ForgotPassword)

responseDescribeUserPool :: DescribeUserPoolResponse -> TestTree
responseDescribeUserPool = res
    "DescribeUserPoolResponse"
    "fixture/DescribeUserPoolResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DescribeUserPool)

responseInitiateAuth :: InitiateAuthResponse -> TestTree
responseInitiateAuth = res
    "InitiateAuthResponse"
    "fixture/InitiateAuthResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy InitiateAuth)

responseAdminListGroupsForUser :: AdminListGroupsForUserResponse -> TestTree
responseAdminListGroupsForUser = res
    "AdminListGroupsForUserResponse"
    "fixture/AdminListGroupsForUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminListGroupsForUser)

responseAdminConfirmSignUp :: AdminConfirmSignUpResponse -> TestTree
responseAdminConfirmSignUp = res
    "AdminConfirmSignUpResponse"
    "fixture/AdminConfirmSignUpResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminConfirmSignUp)

responseAdminUpdateAuthEventFeedback :: AdminUpdateAuthEventFeedbackResponse -> TestTree
responseAdminUpdateAuthEventFeedback = res
    "AdminUpdateAuthEventFeedbackResponse"
    "fixture/AdminUpdateAuthEventFeedbackResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminUpdateAuthEventFeedback)

responseStartUserImportJob :: StartUserImportJobResponse -> TestTree
responseStartUserImportJob = res
    "StartUserImportJobResponse"
    "fixture/StartUserImportJobResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy StartUserImportJob)

responseCreateIdentityProvider :: CreateIdentityProviderResponse -> TestTree
responseCreateIdentityProvider = res
    "CreateIdentityProviderResponse"
    "fixture/CreateIdentityProviderResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateIdentityProvider)

responseSetUICustomization :: SetUICustomizationResponse -> TestTree
responseSetUICustomization = res
    "SetUICustomizationResponse"
    "fixture/SetUICustomizationResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy SetUICustomization)

responseListIdentityProviders :: ListIdentityProvidersResponse -> TestTree
responseListIdentityProviders = res
    "ListIdentityProvidersResponse"
    "fixture/ListIdentityProvidersResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListIdentityProviders)

responseGetDevice :: GetDeviceResponse -> TestTree
responseGetDevice = res
    "GetDeviceResponse"
    "fixture/GetDeviceResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetDevice)

responseSignUp :: SignUpResponse -> TestTree
responseSignUp = res
    "SignUpResponse"
    "fixture/SignUpResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy SignUp)

responseDeleteResourceServer :: DeleteResourceServerResponse -> TestTree
responseDeleteResourceServer = res
    "DeleteResourceServerResponse"
    "fixture/DeleteResourceServerResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteResourceServer)

responseUpdateResourceServer :: UpdateResourceServerResponse -> TestTree
responseUpdateResourceServer = res
    "UpdateResourceServerResponse"
    "fixture/UpdateResourceServerResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateResourceServer)

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword = res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ChangePassword)

responseCreateUserPoolDomain :: CreateUserPoolDomainResponse -> TestTree
responseCreateUserPoolDomain = res
    "CreateUserPoolDomainResponse"
    "fixture/CreateUserPoolDomainResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateUserPoolDomain)

responseRespondToAuthChallenge :: RespondToAuthChallengeResponse -> TestTree
responseRespondToAuthChallenge = res
    "RespondToAuthChallengeResponse"
    "fixture/RespondToAuthChallengeResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy RespondToAuthChallenge)

responseCreateUserPool :: CreateUserPoolResponse -> TestTree
responseCreateUserPool = res
    "CreateUserPoolResponse"
    "fixture/CreateUserPoolResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateUserPool)

responseAdminGetDevice :: AdminGetDeviceResponse -> TestTree
responseAdminGetDevice = res
    "AdminGetDeviceResponse"
    "fixture/AdminGetDeviceResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminGetDevice)

responseGetIdentityProviderByIdentifier :: GetIdentityProviderByIdentifierResponse -> TestTree
responseGetIdentityProviderByIdentifier = res
    "GetIdentityProviderByIdentifierResponse"
    "fixture/GetIdentityProviderByIdentifierResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetIdentityProviderByIdentifier)

responseAdminRemoveUserFromGroup :: AdminRemoveUserFromGroupResponse -> TestTree
responseAdminRemoveUserFromGroup = res
    "AdminRemoveUserFromGroupResponse"
    "fixture/AdminRemoveUserFromGroupResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminRemoveUserFromGroup)

responseSetRiskConfiguration :: SetRiskConfigurationResponse -> TestTree
responseSetRiskConfiguration = res
    "SetRiskConfigurationResponse"
    "fixture/SetRiskConfigurationResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy SetRiskConfiguration)

responseConfirmSignUp :: ConfirmSignUpResponse -> TestTree
responseConfirmSignUp = res
    "ConfirmSignUpResponse"
    "fixture/ConfirmSignUpResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ConfirmSignUp)

responseListUserPools :: ListUserPoolsResponse -> TestTree
responseListUserPools = res
    "ListUserPoolsResponse"
    "fixture/ListUserPoolsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListUserPools)

responseAdminResetUserPassword :: AdminResetUserPasswordResponse -> TestTree
responseAdminResetUserPassword = res
    "AdminResetUserPasswordResponse"
    "fixture/AdminResetUserPasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminResetUserPassword)

responseUpdateAuthEventFeedback :: UpdateAuthEventFeedbackResponse -> TestTree
responseUpdateAuthEventFeedback = res
    "UpdateAuthEventFeedbackResponse"
    "fixture/UpdateAuthEventFeedbackResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateAuthEventFeedback)

responseCreateUserImportJob :: CreateUserImportJobResponse -> TestTree
responseCreateUserImportJob = res
    "CreateUserImportJobResponse"
    "fixture/CreateUserImportJobResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateUserImportJob)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser = res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetUser)

responseGetUICustomization :: GetUICustomizationResponse -> TestTree
responseGetUICustomization = res
    "GetUICustomizationResponse"
    "fixture/GetUICustomizationResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetUICustomization)

responseGetCSVHeader :: GetCSVHeaderResponse -> TestTree
responseGetCSVHeader = res
    "GetCSVHeaderResponse"
    "fixture/GetCSVHeaderResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetCSVHeader)

responseAdminDeleteUser :: AdminDeleteUserResponse -> TestTree
responseAdminDeleteUser = res
    "AdminDeleteUserResponse"
    "fixture/AdminDeleteUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminDeleteUser)

responseAdminForgetDevice :: AdminForgetDeviceResponse -> TestTree
responseAdminForgetDevice = res
    "AdminForgetDeviceResponse"
    "fixture/AdminForgetDeviceResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminForgetDevice)

responseDescribeResourceServer :: DescribeResourceServerResponse -> TestTree
responseDescribeResourceServer = res
    "DescribeResourceServerResponse"
    "fixture/DescribeResourceServerResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DescribeResourceServer)

responseSetUserMFAPreference :: SetUserMFAPreferenceResponse -> TestTree
responseSetUserMFAPreference = res
    "SetUserMFAPreferenceResponse"
    "fixture/SetUserMFAPreferenceResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy SetUserMFAPreference)

responseAdminUpdateDeviceStatus :: AdminUpdateDeviceStatusResponse -> TestTree
responseAdminUpdateDeviceStatus = res
    "AdminUpdateDeviceStatusResponse"
    "fixture/AdminUpdateDeviceStatusResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminUpdateDeviceStatus)

responseAdminCreateUser :: AdminCreateUserResponse -> TestTree
responseAdminCreateUser = res
    "AdminCreateUserResponse"
    "fixture/AdminCreateUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminCreateUser)

responseAddCustomAttributes :: AddCustomAttributesResponse -> TestTree
responseAddCustomAttributes = res
    "AddCustomAttributesResponse"
    "fixture/AddCustomAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AddCustomAttributes)

responseListUserPoolClients :: ListUserPoolClientsResponse -> TestTree
responseListUserPoolClients = res
    "ListUserPoolClientsResponse"
    "fixture/ListUserPoolClientsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListUserPoolClients)

responseAdminSetUserMFAPreference :: AdminSetUserMFAPreferenceResponse -> TestTree
responseAdminSetUserMFAPreference = res
    "AdminSetUserMFAPreferenceResponse"
    "fixture/AdminSetUserMFAPreferenceResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminSetUserMFAPreference)

responseUpdateUserPoolClient :: UpdateUserPoolClientResponse -> TestTree
responseUpdateUserPoolClient = res
    "UpdateUserPoolClientResponse"
    "fixture/UpdateUserPoolClientResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateUserPoolClient)

responseDeleteUserPoolClient :: DeleteUserPoolClientResponse -> TestTree
responseDeleteUserPoolClient = res
    "DeleteUserPoolClientResponse"
    "fixture/DeleteUserPoolClientResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteUserPoolClient)

responseUpdateDeviceStatus :: UpdateDeviceStatusResponse -> TestTree
responseUpdateDeviceStatus = res
    "UpdateDeviceStatusResponse"
    "fixture/UpdateDeviceStatusResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateDeviceStatus)

responseForgetDevice :: ForgetDeviceResponse -> TestTree
responseForgetDevice = res
    "ForgetDeviceResponse"
    "fixture/ForgetDeviceResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ForgetDevice)

responseGetSigningCertificate :: GetSigningCertificateResponse -> TestTree
responseGetSigningCertificate = res
    "GetSigningCertificateResponse"
    "fixture/GetSigningCertificateResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetSigningCertificate)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteUser)

responseCreateUserPoolClient :: CreateUserPoolClientResponse -> TestTree
responseCreateUserPoolClient = res
    "CreateUserPoolClientResponse"
    "fixture/CreateUserPoolClientResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateUserPoolClient)

responseGetUserPoolMFAConfig :: GetUserPoolMFAConfigResponse -> TestTree
responseGetUserPoolMFAConfig = res
    "GetUserPoolMFAConfigResponse"
    "fixture/GetUserPoolMFAConfigResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetUserPoolMFAConfig)

responseCreateResourceServer :: CreateResourceServerResponse -> TestTree
responseCreateResourceServer = res
    "CreateResourceServerResponse"
    "fixture/CreateResourceServerResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateResourceServer)

responseAdminListUserAuthEvents :: AdminListUserAuthEventsResponse -> TestTree
responseAdminListUserAuthEvents = res
    "AdminListUserAuthEventsResponse"
    "fixture/AdminListUserAuthEventsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminListUserAuthEvents)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup = res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateGroup)

responseAdminAddUserToGroup :: AdminAddUserToGroupResponse -> TestTree
responseAdminAddUserToGroup = res
    "AdminAddUserToGroupResponse"
    "fixture/AdminAddUserToGroupResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminAddUserToGroup)

responseVerifySoftwareToken :: VerifySoftwareTokenResponse -> TestTree
responseVerifySoftwareToken = res
    "VerifySoftwareTokenResponse"
    "fixture/VerifySoftwareTokenResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy VerifySoftwareToken)

responseStopUserImportJob :: StopUserImportJobResponse -> TestTree
responseStopUserImportJob = res
    "StopUserImportJobResponse"
    "fixture/StopUserImportJobResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy StopUserImportJob)

responseDescribeUserImportJob :: DescribeUserImportJobResponse -> TestTree
responseDescribeUserImportJob = res
    "DescribeUserImportJobResponse"
    "fixture/DescribeUserImportJobResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DescribeUserImportJob)

responseDescribeRiskConfiguration :: DescribeRiskConfigurationResponse -> TestTree
responseDescribeRiskConfiguration = res
    "DescribeRiskConfigurationResponse"
    "fixture/DescribeRiskConfigurationResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DescribeRiskConfiguration)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup = res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup = res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateGroup)

responseGlobalSignOut :: GlobalSignOutResponse -> TestTree
responseGlobalSignOut = res
    "GlobalSignOutResponse"
    "fixture/GlobalSignOutResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GlobalSignOut)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups = res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListGroups)

responseUpdateIdentityProvider :: UpdateIdentityProviderResponse -> TestTree
responseUpdateIdentityProvider = res
    "UpdateIdentityProviderResponse"
    "fixture/UpdateIdentityProviderResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateIdentityProvider)

responseDeleteIdentityProvider :: DeleteIdentityProviderResponse -> TestTree
responseDeleteIdentityProvider = res
    "DeleteIdentityProviderResponse"
    "fixture/DeleteIdentityProviderResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteIdentityProvider)

responseListResourceServers :: ListResourceServersResponse -> TestTree
responseListResourceServers = res
    "ListResourceServersResponse"
    "fixture/ListResourceServersResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListResourceServers)

responseAdminRespondToAuthChallenge :: AdminRespondToAuthChallengeResponse -> TestTree
responseAdminRespondToAuthChallenge = res
    "AdminRespondToAuthChallengeResponse"
    "fixture/AdminRespondToAuthChallengeResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminRespondToAuthChallenge)

responseSetUserSettings :: SetUserSettingsResponse -> TestTree
responseSetUserSettings = res
    "SetUserSettingsResponse"
    "fixture/SetUserSettingsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy SetUserSettings)

responseAdminListDevices :: AdminListDevicesResponse -> TestTree
responseAdminListDevices = res
    "AdminListDevicesResponse"
    "fixture/AdminListDevicesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminListDevices)

responseDescribeUserPoolClient :: DescribeUserPoolClientResponse -> TestTree
responseDescribeUserPoolClient = res
    "DescribeUserPoolClientResponse"
    "fixture/DescribeUserPoolClientResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DescribeUserPoolClient)

responseResendConfirmationCode :: ResendConfirmationCodeResponse -> TestTree
responseResendConfirmationCode = res
    "ResendConfirmationCodeResponse"
    "fixture/ResendConfirmationCodeResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ResendConfirmationCode)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup = res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetGroup)

responseAdminSetUserSettings :: AdminSetUserSettingsResponse -> TestTree
responseAdminSetUserSettings = res
    "AdminSetUserSettingsResponse"
    "fixture/AdminSetUserSettingsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminSetUserSettings)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices = res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListDevices)
