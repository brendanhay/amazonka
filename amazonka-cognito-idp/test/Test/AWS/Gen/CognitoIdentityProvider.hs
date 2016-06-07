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
--         , requestConfirmForgotPassword $
--             confirmForgotPassword
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
--         , requestForgotPassword $
--             forgotPassword
--
--         , requestDescribeUserPool $
--             describeUserPool
--
--         , requestAdminConfirmSignUp $
--             adminConfirmSignUp
--
--         , requestSignUp $
--             signUp
--
--         , requestChangePassword $
--             changePassword
--
--         , requestCreateUserPool $
--             createUserPool
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
--         , requestGetUser $
--             getUser
--
--         , requestAdminDeleteUser $
--             adminDeleteUser
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
--         , requestDeleteUser $
--             deleteUser
--
--         , requestCreateUserPoolClient $
--             createUserPoolClient
--
--         , requestSetUserSettings $
--             setUserSettings
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
--           ]

--     , testGroup "response"
--         [ responseDeleteUserPool $
--             deleteUserPoolResponse
--
--         , responseUpdateUserPool $
--             updateUserPoolResponse
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
--         , responseConfirmForgotPassword $
--             confirmForgotPasswordResponse
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
--         , responseForgotPassword $
--             forgotPasswordResponse
--
--         , responseDescribeUserPool $
--             describeUserPoolResponse
--
--         , responseAdminConfirmSignUp $
--             adminConfirmSignUpResponse
--
--         , responseSignUp $
--             signUpResponse
--
--         , responseChangePassword $
--             changePasswordResponse
--
--         , responseCreateUserPool $
--             createUserPoolResponse
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
--         , responseGetUser $
--             getUserResponse
--
--         , responseAdminDeleteUser $
--             adminDeleteUserResponse
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
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseCreateUserPoolClient $
--             createUserPoolClientResponse
--
--         , responseSetUserSettings $
--             setUserSettingsResponse
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

requestConfirmForgotPassword :: ConfirmForgotPassword -> TestTree
requestConfirmForgotPassword = req
    "ConfirmForgotPassword"
    "fixture/ConfirmForgotPassword.yaml"

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

requestForgotPassword :: ForgotPassword -> TestTree
requestForgotPassword = req
    "ForgotPassword"
    "fixture/ForgotPassword.yaml"

requestDescribeUserPool :: DescribeUserPool -> TestTree
requestDescribeUserPool = req
    "DescribeUserPool"
    "fixture/DescribeUserPool.yaml"

requestAdminConfirmSignUp :: AdminConfirmSignUp -> TestTree
requestAdminConfirmSignUp = req
    "AdminConfirmSignUp"
    "fixture/AdminConfirmSignUp.yaml"

requestSignUp :: SignUp -> TestTree
requestSignUp = req
    "SignUp"
    "fixture/SignUp.yaml"

requestChangePassword :: ChangePassword -> TestTree
requestChangePassword = req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

requestCreateUserPool :: CreateUserPool -> TestTree
requestCreateUserPool = req
    "CreateUserPool"
    "fixture/CreateUserPool.yaml"

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

requestGetUser :: GetUser -> TestTree
requestGetUser = req
    "GetUser"
    "fixture/GetUser.yaml"

requestAdminDeleteUser :: AdminDeleteUser -> TestTree
requestAdminDeleteUser = req
    "AdminDeleteUser"
    "fixture/AdminDeleteUser.yaml"

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

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestCreateUserPoolClient :: CreateUserPoolClient -> TestTree
requestCreateUserPoolClient = req
    "CreateUserPoolClient"
    "fixture/CreateUserPoolClient.yaml"

requestSetUserSettings :: SetUserSettings -> TestTree
requestSetUserSettings = req
    "SetUserSettings"
    "fixture/SetUserSettings.yaml"

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

responseConfirmForgotPassword :: ConfirmForgotPasswordResponse -> TestTree
responseConfirmForgotPassword = res
    "ConfirmForgotPasswordResponse"
    "fixture/ConfirmForgotPasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ConfirmForgotPassword)

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

responseAdminConfirmSignUp :: AdminConfirmSignUpResponse -> TestTree
responseAdminConfirmSignUp = res
    "AdminConfirmSignUpResponse"
    "fixture/AdminConfirmSignUpResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminConfirmSignUp)

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

responseCreateUserPool :: CreateUserPoolResponse -> TestTree
responseCreateUserPool = res
    "CreateUserPoolResponse"
    "fixture/CreateUserPoolResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateUserPool)

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

responseGetUser :: GetUserResponse -> TestTree
responseGetUser = res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetUser)

responseAdminDeleteUser :: AdminDeleteUserResponse -> TestTree
responseAdminDeleteUser = res
    "AdminDeleteUserResponse"
    "fixture/AdminDeleteUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminDeleteUser)

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

responseSetUserSettings :: SetUserSettingsResponse -> TestTree
responseSetUserSettings = res
    "SetUserSettingsResponse"
    "fixture/SetUserSettingsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy SetUserSettings)

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
