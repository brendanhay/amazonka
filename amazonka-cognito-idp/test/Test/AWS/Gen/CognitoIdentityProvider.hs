{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoIdentityProvider
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CognitoIdentityProvider where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CognitoIdentityProvider
import Test.AWS.CognitoIdentityProvider.Internal

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
--         , requestAdminInitiateAuth $
--             adminInitiateAuth
--
--         , requestAdminEnableUser $
--             adminEnableUser
--
--         , requestGetUserAttributeVerificationCode $
--             getUserAttributeVerificationCode
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
--         , requestListUsers $
--             listUsers
--
--         , requestAdminDeleteUserAttributes $
--             adminDeleteUserAttributes
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
--         , requestForgotPassword $
--             forgotPassword
--
--         , requestDescribeUserPool $
--             describeUserPool
--
--         , requestInitiateAuth $
--             initiateAuth
--
--         , requestAdminConfirmSignUp $
--             adminConfirmSignUp
--
--         , requestStartUserImportJob $
--             startUserImportJob
--
--         , requestGetDevice $
--             getDevice
--
--         , requestSignUp $
--             signUp
--
--         , requestChangePassword $
--             changePassword
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
--         , requestConfirmSignUp $
--             confirmSignUp
--
--         , requestListUserPools $
--             listUserPools
--
--         , requestAdminResetUserPassword $
--             adminResetUserPassword
--
--         , requestCreateUserImportJob $
--             createUserImportJob
--
--         , requestGetUser $
--             getUser
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
--         , requestAdminUpdateDeviceStatus $
--             adminUpdateDeviceStatus
--
--         , requestAddCustomAttributes $
--             addCustomAttributes
--
--         , requestListUserPoolClients $
--             listUserPoolClients
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
--         , requestDeleteUser $
--             deleteUser
--
--         , requestCreateUserPoolClient $
--             createUserPoolClient
--
--         , requestStopUserImportJob $
--             stopUserImportJob
--
--         , requestDescribeUserImportJob $
--             describeUserImportJob
--
--         , requestGlobalSignOut $
--             globalSignOut
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
--         , responseAdminInitiateAuth $
--             adminInitiateAuthResponse
--
--         , responseAdminEnableUser $
--             adminEnableUserResponse
--
--         , responseGetUserAttributeVerificationCode $
--             getUserAttributeVerificationCodeResponse
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
--         , responseListUsers $
--             listUsersResponse
--
--         , responseAdminDeleteUserAttributes $
--             adminDeleteUserAttributesResponse
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
--         , responseForgotPassword $
--             forgotPasswordResponse
--
--         , responseDescribeUserPool $
--             describeUserPoolResponse
--
--         , responseInitiateAuth $
--             initiateAuthResponse
--
--         , responseAdminConfirmSignUp $
--             adminConfirmSignUpResponse
--
--         , responseStartUserImportJob $
--             startUserImportJobResponse
--
--         , responseGetDevice $
--             getDeviceResponse
--
--         , responseSignUp $
--             signUpResponse
--
--         , responseChangePassword $
--             changePasswordResponse
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
--         , responseConfirmSignUp $
--             confirmSignUpResponse
--
--         , responseListUserPools $
--             listUserPoolsResponse
--
--         , responseAdminResetUserPassword $
--             adminResetUserPasswordResponse
--
--         , responseCreateUserImportJob $
--             createUserImportJobResponse
--
--         , responseGetUser $
--             getUserResponse
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
--         , responseAdminUpdateDeviceStatus $
--             adminUpdateDeviceStatusResponse
--
--         , responseAddCustomAttributes $
--             addCustomAttributesResponse
--
--         , responseListUserPoolClients $
--             listUserPoolClientsResponse
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
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseCreateUserPoolClient $
--             createUserPoolClientResponse
--
--         , responseStopUserImportJob $
--             stopUserImportJobResponse
--
--         , responseDescribeUserImportJob $
--             describeUserImportJobResponse
--
--         , responseGlobalSignOut $
--             globalSignOutResponse
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

requestAdminInitiateAuth :: AdminInitiateAuth -> TestTree
requestAdminInitiateAuth = req
    "AdminInitiateAuth"
    "fixture/AdminInitiateAuth.yaml"

requestAdminEnableUser :: AdminEnableUser -> TestTree
requestAdminEnableUser = req
    "AdminEnableUser"
    "fixture/AdminEnableUser.yaml"

requestGetUserAttributeVerificationCode :: GetUserAttributeVerificationCode -> TestTree
requestGetUserAttributeVerificationCode = req
    "GetUserAttributeVerificationCode"
    "fixture/GetUserAttributeVerificationCode.yaml"

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

requestListUsers :: ListUsers -> TestTree
requestListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestAdminDeleteUserAttributes :: AdminDeleteUserAttributes -> TestTree
requestAdminDeleteUserAttributes = req
    "AdminDeleteUserAttributes"
    "fixture/AdminDeleteUserAttributes.yaml"

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

requestAdminConfirmSignUp :: AdminConfirmSignUp -> TestTree
requestAdminConfirmSignUp = req
    "AdminConfirmSignUp"
    "fixture/AdminConfirmSignUp.yaml"

requestStartUserImportJob :: StartUserImportJob -> TestTree
requestStartUserImportJob = req
    "StartUserImportJob"
    "fixture/StartUserImportJob.yaml"

requestGetDevice :: GetDevice -> TestTree
requestGetDevice = req
    "GetDevice"
    "fixture/GetDevice.yaml"

requestSignUp :: SignUp -> TestTree
requestSignUp = req
    "SignUp"
    "fixture/SignUp.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword = req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

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

requestCreateUserImportJob :: CreateUserImportJob -> TestTree
requestCreateUserImportJob = req
    "CreateUserImportJob"
    "fixture/CreateUserImportJob.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser = req
    "GetUser"
    "fixture/GetUser.yaml"

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

requestAdminUpdateDeviceStatus :: AdminUpdateDeviceStatus -> TestTree
requestAdminUpdateDeviceStatus = req
    "AdminUpdateDeviceStatus"
    "fixture/AdminUpdateDeviceStatus.yaml"

requestAddCustomAttributes :: AddCustomAttributes -> TestTree
requestAddCustomAttributes = req
    "AddCustomAttributes"
    "fixture/AddCustomAttributes.yaml"

requestListUserPoolClients :: ListUserPoolClients -> TestTree
requestListUserPoolClients = req
    "ListUserPoolClients"
    "fixture/ListUserPoolClients.yaml"

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

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestCreateUserPoolClient :: CreateUserPoolClient -> TestTree
requestCreateUserPoolClient = req
    "CreateUserPoolClient"
    "fixture/CreateUserPoolClient.yaml"

requestStopUserImportJob :: StopUserImportJob -> TestTree
requestStopUserImportJob = req
    "StopUserImportJob"
    "fixture/StopUserImportJob.yaml"

requestDescribeUserImportJob :: DescribeUserImportJob -> TestTree
requestDescribeUserImportJob = req
    "DescribeUserImportJob"
    "fixture/DescribeUserImportJob.yaml"

requestGlobalSignOut :: GlobalSignOut -> TestTree
requestGlobalSignOut = req
    "GlobalSignOut"
    "fixture/GlobalSignOut.yaml"

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

responseAdminInitiateAuth :: AdminInitiateAuthResponse -> TestTree
responseAdminInitiateAuth = res
    "AdminInitiateAuthResponse"
    "fixture/AdminInitiateAuthResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminInitiateAuth)

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

responseAdminConfirmSignUp :: AdminConfirmSignUpResponse -> TestTree
responseAdminConfirmSignUp = res
    "AdminConfirmSignUpResponse"
    "fixture/AdminConfirmSignUpResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminConfirmSignUp)

responseStartUserImportJob :: StartUserImportJobResponse -> TestTree
responseStartUserImportJob = res
    "StartUserImportJobResponse"
    "fixture/StartUserImportJobResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy StartUserImportJob)

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

responseChangePassword :: ChangePasswordResponse -> TestTree
responseChangePassword = res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ChangePassword)

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

responseAdminUpdateDeviceStatus :: AdminUpdateDeviceStatusResponse -> TestTree
responseAdminUpdateDeviceStatus = res
    "AdminUpdateDeviceStatusResponse"
    "fixture/AdminUpdateDeviceStatusResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminUpdateDeviceStatus)

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

responseGlobalSignOut :: GlobalSignOutResponse -> TestTree
responseGlobalSignOut = res
    "GlobalSignOutResponse"
    "fixture/GlobalSignOutResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GlobalSignOut)

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
