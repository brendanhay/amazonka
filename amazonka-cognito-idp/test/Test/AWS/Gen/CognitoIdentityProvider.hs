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
--         [ testDeleteUserPool $
--             deleteUserPool
--
--         , testUpdateUserPool $
--             updateUserPool
--
--         , testAdminEnableUser $
--             adminEnableUser
--
--         , testGetUserAttributeVerificationCode $
--             getUserAttributeVerificationCode
--
--         , testUpdateUserAttributes $
--             updateUserAttributes
--
--         , testDeleteUserAttributes $
--             deleteUserAttributes
--
--         , testVerifyUserAttribute $
--             verifyUserAttribute
--
--         , testAdminDisableUser $
--             adminDisableUser
--
--         , testConfirmForgotPassword $
--             confirmForgotPassword
--
--         , testListUsers $
--             listUsers
--
--         , testAdminDeleteUserAttributes $
--             adminDeleteUserAttributes
--
--         , testAdminUpdateUserAttributes $
--             adminUpdateUserAttributes
--
--         , testAdminGetUser $
--             adminGetUser
--
--         , testForgotPassword $
--             forgotPassword
--
--         , testDescribeUserPool $
--             describeUserPool
--
--         , testAdminConfirmSignUp $
--             adminConfirmSignUp
--
--         , testSignUp $
--             signUp
--
--         , testChangePassword $
--             changePassword
--
--         , testCreateUserPool $
--             createUserPool
--
--         , testConfirmSignUp $
--             confirmSignUp
--
--         , testListUserPools $
--             listUserPools
--
--         , testAdminResetUserPassword $
--             adminResetUserPassword
--
--         , testGetUser $
--             getUser
--
--         , testAdminDeleteUser $
--             adminDeleteUser
--
--         , testAddCustomAttributes $
--             addCustomAttributes
--
--         , testListUserPoolClients $
--             listUserPoolClients
--
--         , testUpdateUserPoolClient $
--             updateUserPoolClient
--
--         , testDeleteUserPoolClient $
--             deleteUserPoolClient
--
--         , testDeleteUser $
--             deleteUser
--
--         , testCreateUserPoolClient $
--             createUserPoolClient
--
--         , testSetUserSettings $
--             setUserSettings
--
--         , testDescribeUserPoolClient $
--             describeUserPoolClient
--
--         , testResendConfirmationCode $
--             resendConfirmationCode
--
--         , testAdminSetUserSettings $
--             adminSetUserSettings
--
--           ]

--     , testGroup "response"
--         [ testDeleteUserPoolResponse $
--             deleteUserPoolResponse
--
--         , testUpdateUserPoolResponse $
--             updateUserPoolResponse
--
--         , testAdminEnableUserResponse $
--             adminEnableUserResponse
--
--         , testGetUserAttributeVerificationCodeResponse $
--             getUserAttributeVerificationCodeResponse
--
--         , testUpdateUserAttributesResponse $
--             updateUserAttributesResponse
--
--         , testDeleteUserAttributesResponse $
--             deleteUserAttributesResponse
--
--         , testVerifyUserAttributeResponse $
--             verifyUserAttributeResponse
--
--         , testAdminDisableUserResponse $
--             adminDisableUserResponse
--
--         , testConfirmForgotPasswordResponse $
--             confirmForgotPasswordResponse
--
--         , testListUsersResponse $
--             listUsersResponse
--
--         , testAdminDeleteUserAttributesResponse $
--             adminDeleteUserAttributesResponse
--
--         , testAdminUpdateUserAttributesResponse $
--             adminUpdateUserAttributesResponse
--
--         , testAdminGetUserResponse $
--             adminGetUserResponse
--
--         , testForgotPasswordResponse $
--             forgotPasswordResponse
--
--         , testDescribeUserPoolResponse $
--             describeUserPoolResponse
--
--         , testAdminConfirmSignUpResponse $
--             adminConfirmSignUpResponse
--
--         , testSignUpResponse $
--             signUpResponse
--
--         , testChangePasswordResponse $
--             changePasswordResponse
--
--         , testCreateUserPoolResponse $
--             createUserPoolResponse
--
--         , testConfirmSignUpResponse $
--             confirmSignUpResponse
--
--         , testListUserPoolsResponse $
--             listUserPoolsResponse
--
--         , testAdminResetUserPasswordResponse $
--             adminResetUserPasswordResponse
--
--         , testGetUserResponse $
--             getUserResponse
--
--         , testAdminDeleteUserResponse $
--             adminDeleteUserResponse
--
--         , testAddCustomAttributesResponse $
--             addCustomAttributesResponse
--
--         , testListUserPoolClientsResponse $
--             listUserPoolClientsResponse
--
--         , testUpdateUserPoolClientResponse $
--             updateUserPoolClientResponse
--
--         , testDeleteUserPoolClientResponse $
--             deleteUserPoolClientResponse
--
--         , testDeleteUserResponse $
--             deleteUserResponse
--
--         , testCreateUserPoolClientResponse $
--             createUserPoolClientResponse
--
--         , testSetUserSettingsResponse $
--             setUserSettingsResponse
--
--         , testDescribeUserPoolClientResponse $
--             describeUserPoolClientResponse
--
--         , testResendConfirmationCodeResponse $
--             resendConfirmationCodeResponse
--
--         , testAdminSetUserSettingsResponse $
--             adminSetUserSettingsResponse
--
--           ]
--     ]

-- Requests

testDeleteUserPool :: DeleteUserPool -> TestTree
testDeleteUserPool = req
    "DeleteUserPool"
    "fixture/DeleteUserPool.yaml"

testUpdateUserPool :: UpdateUserPool -> TestTree
testUpdateUserPool = req
    "UpdateUserPool"
    "fixture/UpdateUserPool.yaml"

testAdminEnableUser :: AdminEnableUser -> TestTree
testAdminEnableUser = req
    "AdminEnableUser"
    "fixture/AdminEnableUser.yaml"

testGetUserAttributeVerificationCode :: GetUserAttributeVerificationCode -> TestTree
testGetUserAttributeVerificationCode = req
    "GetUserAttributeVerificationCode"
    "fixture/GetUserAttributeVerificationCode.yaml"

testUpdateUserAttributes :: UpdateUserAttributes -> TestTree
testUpdateUserAttributes = req
    "UpdateUserAttributes"
    "fixture/UpdateUserAttributes.yaml"

testDeleteUserAttributes :: DeleteUserAttributes -> TestTree
testDeleteUserAttributes = req
    "DeleteUserAttributes"
    "fixture/DeleteUserAttributes.yaml"

testVerifyUserAttribute :: VerifyUserAttribute -> TestTree
testVerifyUserAttribute = req
    "VerifyUserAttribute"
    "fixture/VerifyUserAttribute.yaml"

testAdminDisableUser :: AdminDisableUser -> TestTree
testAdminDisableUser = req
    "AdminDisableUser"
    "fixture/AdminDisableUser.yaml"

testConfirmForgotPassword :: ConfirmForgotPassword -> TestTree
testConfirmForgotPassword = req
    "ConfirmForgotPassword"
    "fixture/ConfirmForgotPassword.yaml"

testListUsers :: ListUsers -> TestTree
testListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

testAdminDeleteUserAttributes :: AdminDeleteUserAttributes -> TestTree
testAdminDeleteUserAttributes = req
    "AdminDeleteUserAttributes"
    "fixture/AdminDeleteUserAttributes.yaml"

testAdminUpdateUserAttributes :: AdminUpdateUserAttributes -> TestTree
testAdminUpdateUserAttributes = req
    "AdminUpdateUserAttributes"
    "fixture/AdminUpdateUserAttributes.yaml"

testAdminGetUser :: AdminGetUser -> TestTree
testAdminGetUser = req
    "AdminGetUser"
    "fixture/AdminGetUser.yaml"

testForgotPassword :: ForgotPassword -> TestTree
testForgotPassword = req
    "ForgotPassword"
    "fixture/ForgotPassword.yaml"

testDescribeUserPool :: DescribeUserPool -> TestTree
testDescribeUserPool = req
    "DescribeUserPool"
    "fixture/DescribeUserPool.yaml"

testAdminConfirmSignUp :: AdminConfirmSignUp -> TestTree
testAdminConfirmSignUp = req
    "AdminConfirmSignUp"
    "fixture/AdminConfirmSignUp.yaml"

testSignUp :: SignUp -> TestTree
testSignUp = req
    "SignUp"
    "fixture/SignUp.yaml"

testChangePassword :: ChangePassword -> TestTree
testChangePassword = req
    "ChangePassword"
    "fixture/ChangePassword.yaml"

testCreateUserPool :: CreateUserPool -> TestTree
testCreateUserPool = req
    "CreateUserPool"
    "fixture/CreateUserPool.yaml"

testConfirmSignUp :: ConfirmSignUp -> TestTree
testConfirmSignUp = req
    "ConfirmSignUp"
    "fixture/ConfirmSignUp.yaml"

testListUserPools :: ListUserPools -> TestTree
testListUserPools = req
    "ListUserPools"
    "fixture/ListUserPools.yaml"

testAdminResetUserPassword :: AdminResetUserPassword -> TestTree
testAdminResetUserPassword = req
    "AdminResetUserPassword"
    "fixture/AdminResetUserPassword.yaml"

testGetUser :: GetUser -> TestTree
testGetUser = req
    "GetUser"
    "fixture/GetUser.yaml"

testAdminDeleteUser :: AdminDeleteUser -> TestTree
testAdminDeleteUser = req
    "AdminDeleteUser"
    "fixture/AdminDeleteUser.yaml"

testAddCustomAttributes :: AddCustomAttributes -> TestTree
testAddCustomAttributes = req
    "AddCustomAttributes"
    "fixture/AddCustomAttributes.yaml"

testListUserPoolClients :: ListUserPoolClients -> TestTree
testListUserPoolClients = req
    "ListUserPoolClients"
    "fixture/ListUserPoolClients.yaml"

testUpdateUserPoolClient :: UpdateUserPoolClient -> TestTree
testUpdateUserPoolClient = req
    "UpdateUserPoolClient"
    "fixture/UpdateUserPoolClient.yaml"

testDeleteUserPoolClient :: DeleteUserPoolClient -> TestTree
testDeleteUserPoolClient = req
    "DeleteUserPoolClient"
    "fixture/DeleteUserPoolClient.yaml"

testDeleteUser :: DeleteUser -> TestTree
testDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

testCreateUserPoolClient :: CreateUserPoolClient -> TestTree
testCreateUserPoolClient = req
    "CreateUserPoolClient"
    "fixture/CreateUserPoolClient.yaml"

testSetUserSettings :: SetUserSettings -> TestTree
testSetUserSettings = req
    "SetUserSettings"
    "fixture/SetUserSettings.yaml"

testDescribeUserPoolClient :: DescribeUserPoolClient -> TestTree
testDescribeUserPoolClient = req
    "DescribeUserPoolClient"
    "fixture/DescribeUserPoolClient.yaml"

testResendConfirmationCode :: ResendConfirmationCode -> TestTree
testResendConfirmationCode = req
    "ResendConfirmationCode"
    "fixture/ResendConfirmationCode.yaml"

testAdminSetUserSettings :: AdminSetUserSettings -> TestTree
testAdminSetUserSettings = req
    "AdminSetUserSettings"
    "fixture/AdminSetUserSettings.yaml"

-- Responses

testDeleteUserPoolResponse :: DeleteUserPoolResponse -> TestTree
testDeleteUserPoolResponse = res
    "DeleteUserPoolResponse"
    "fixture/DeleteUserPoolResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteUserPool)

testUpdateUserPoolResponse :: UpdateUserPoolResponse -> TestTree
testUpdateUserPoolResponse = res
    "UpdateUserPoolResponse"
    "fixture/UpdateUserPoolResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateUserPool)

testAdminEnableUserResponse :: AdminEnableUserResponse -> TestTree
testAdminEnableUserResponse = res
    "AdminEnableUserResponse"
    "fixture/AdminEnableUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminEnableUser)

testGetUserAttributeVerificationCodeResponse :: GetUserAttributeVerificationCodeResponse -> TestTree
testGetUserAttributeVerificationCodeResponse = res
    "GetUserAttributeVerificationCodeResponse"
    "fixture/GetUserAttributeVerificationCodeResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetUserAttributeVerificationCode)

testUpdateUserAttributesResponse :: UpdateUserAttributesResponse -> TestTree
testUpdateUserAttributesResponse = res
    "UpdateUserAttributesResponse"
    "fixture/UpdateUserAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateUserAttributes)

testDeleteUserAttributesResponse :: DeleteUserAttributesResponse -> TestTree
testDeleteUserAttributesResponse = res
    "DeleteUserAttributesResponse"
    "fixture/DeleteUserAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteUserAttributes)

testVerifyUserAttributeResponse :: VerifyUserAttributeResponse -> TestTree
testVerifyUserAttributeResponse = res
    "VerifyUserAttributeResponse"
    "fixture/VerifyUserAttributeResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy VerifyUserAttribute)

testAdminDisableUserResponse :: AdminDisableUserResponse -> TestTree
testAdminDisableUserResponse = res
    "AdminDisableUserResponse"
    "fixture/AdminDisableUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminDisableUser)

testConfirmForgotPasswordResponse :: ConfirmForgotPasswordResponse -> TestTree
testConfirmForgotPasswordResponse = res
    "ConfirmForgotPasswordResponse"
    "fixture/ConfirmForgotPasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ConfirmForgotPassword)

testListUsersResponse :: ListUsersResponse -> TestTree
testListUsersResponse = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListUsers)

testAdminDeleteUserAttributesResponse :: AdminDeleteUserAttributesResponse -> TestTree
testAdminDeleteUserAttributesResponse = res
    "AdminDeleteUserAttributesResponse"
    "fixture/AdminDeleteUserAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminDeleteUserAttributes)

testAdminUpdateUserAttributesResponse :: AdminUpdateUserAttributesResponse -> TestTree
testAdminUpdateUserAttributesResponse = res
    "AdminUpdateUserAttributesResponse"
    "fixture/AdminUpdateUserAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminUpdateUserAttributes)

testAdminGetUserResponse :: AdminGetUserResponse -> TestTree
testAdminGetUserResponse = res
    "AdminGetUserResponse"
    "fixture/AdminGetUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminGetUser)

testForgotPasswordResponse :: ForgotPasswordResponse -> TestTree
testForgotPasswordResponse = res
    "ForgotPasswordResponse"
    "fixture/ForgotPasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ForgotPassword)

testDescribeUserPoolResponse :: DescribeUserPoolResponse -> TestTree
testDescribeUserPoolResponse = res
    "DescribeUserPoolResponse"
    "fixture/DescribeUserPoolResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DescribeUserPool)

testAdminConfirmSignUpResponse :: AdminConfirmSignUpResponse -> TestTree
testAdminConfirmSignUpResponse = res
    "AdminConfirmSignUpResponse"
    "fixture/AdminConfirmSignUpResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminConfirmSignUp)

testSignUpResponse :: SignUpResponse -> TestTree
testSignUpResponse = res
    "SignUpResponse"
    "fixture/SignUpResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy SignUp)

testChangePasswordResponse :: ChangePasswordResponse -> TestTree
testChangePasswordResponse = res
    "ChangePasswordResponse"
    "fixture/ChangePasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ChangePassword)

testCreateUserPoolResponse :: CreateUserPoolResponse -> TestTree
testCreateUserPoolResponse = res
    "CreateUserPoolResponse"
    "fixture/CreateUserPoolResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateUserPool)

testConfirmSignUpResponse :: ConfirmSignUpResponse -> TestTree
testConfirmSignUpResponse = res
    "ConfirmSignUpResponse"
    "fixture/ConfirmSignUpResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ConfirmSignUp)

testListUserPoolsResponse :: ListUserPoolsResponse -> TestTree
testListUserPoolsResponse = res
    "ListUserPoolsResponse"
    "fixture/ListUserPoolsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListUserPools)

testAdminResetUserPasswordResponse :: AdminResetUserPasswordResponse -> TestTree
testAdminResetUserPasswordResponse = res
    "AdminResetUserPasswordResponse"
    "fixture/AdminResetUserPasswordResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminResetUserPassword)

testGetUserResponse :: GetUserResponse -> TestTree
testGetUserResponse = res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy GetUser)

testAdminDeleteUserResponse :: AdminDeleteUserResponse -> TestTree
testAdminDeleteUserResponse = res
    "AdminDeleteUserResponse"
    "fixture/AdminDeleteUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminDeleteUser)

testAddCustomAttributesResponse :: AddCustomAttributesResponse -> TestTree
testAddCustomAttributesResponse = res
    "AddCustomAttributesResponse"
    "fixture/AddCustomAttributesResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AddCustomAttributes)

testListUserPoolClientsResponse :: ListUserPoolClientsResponse -> TestTree
testListUserPoolClientsResponse = res
    "ListUserPoolClientsResponse"
    "fixture/ListUserPoolClientsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ListUserPoolClients)

testUpdateUserPoolClientResponse :: UpdateUserPoolClientResponse -> TestTree
testUpdateUserPoolClientResponse = res
    "UpdateUserPoolClientResponse"
    "fixture/UpdateUserPoolClientResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy UpdateUserPoolClient)

testDeleteUserPoolClientResponse :: DeleteUserPoolClientResponse -> TestTree
testDeleteUserPoolClientResponse = res
    "DeleteUserPoolClientResponse"
    "fixture/DeleteUserPoolClientResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteUserPoolClient)

testDeleteUserResponse :: DeleteUserResponse -> TestTree
testDeleteUserResponse = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DeleteUser)

testCreateUserPoolClientResponse :: CreateUserPoolClientResponse -> TestTree
testCreateUserPoolClientResponse = res
    "CreateUserPoolClientResponse"
    "fixture/CreateUserPoolClientResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy CreateUserPoolClient)

testSetUserSettingsResponse :: SetUserSettingsResponse -> TestTree
testSetUserSettingsResponse = res
    "SetUserSettingsResponse"
    "fixture/SetUserSettingsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy SetUserSettings)

testDescribeUserPoolClientResponse :: DescribeUserPoolClientResponse -> TestTree
testDescribeUserPoolClientResponse = res
    "DescribeUserPoolClientResponse"
    "fixture/DescribeUserPoolClientResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy DescribeUserPoolClient)

testResendConfirmationCodeResponse :: ResendConfirmationCodeResponse -> TestTree
testResendConfirmationCodeResponse = res
    "ResendConfirmationCodeResponse"
    "fixture/ResendConfirmationCodeResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy ResendConfirmationCode)

testAdminSetUserSettingsResponse :: AdminSetUserSettingsResponse -> TestTree
testAdminSetUserSettingsResponse = res
    "AdminSetUserSettingsResponse"
    "fixture/AdminSetUserSettingsResponse.proto"
    cognitoIdentityProvider
    (Proxy :: Proxy AdminSetUserSettings)
