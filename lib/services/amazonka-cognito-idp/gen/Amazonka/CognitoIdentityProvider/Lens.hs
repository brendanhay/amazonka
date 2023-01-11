{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentityProvider.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Lens
  ( -- * Operations

    -- ** AddCustomAttributes
    addCustomAttributes_userPoolId,
    addCustomAttributes_customAttributes,
    addCustomAttributesResponse_httpStatus,

    -- ** AdminAddUserToGroup
    adminAddUserToGroup_userPoolId,
    adminAddUserToGroup_username,
    adminAddUserToGroup_groupName,

    -- ** AdminConfirmSignUp
    adminConfirmSignUp_clientMetadata,
    adminConfirmSignUp_userPoolId,
    adminConfirmSignUp_username,
    adminConfirmSignUpResponse_httpStatus,

    -- ** AdminCreateUser
    adminCreateUser_clientMetadata,
    adminCreateUser_desiredDeliveryMediums,
    adminCreateUser_forceAliasCreation,
    adminCreateUser_messageAction,
    adminCreateUser_temporaryPassword,
    adminCreateUser_userAttributes,
    adminCreateUser_validationData,
    adminCreateUser_userPoolId,
    adminCreateUser_username,
    adminCreateUserResponse_user,
    adminCreateUserResponse_httpStatus,

    -- ** AdminDeleteUser
    adminDeleteUser_userPoolId,
    adminDeleteUser_username,

    -- ** AdminDeleteUserAttributes
    adminDeleteUserAttributes_userPoolId,
    adminDeleteUserAttributes_username,
    adminDeleteUserAttributes_userAttributeNames,
    adminDeleteUserAttributesResponse_httpStatus,

    -- ** AdminDisableProviderForUser
    adminDisableProviderForUser_userPoolId,
    adminDisableProviderForUser_user,
    adminDisableProviderForUserResponse_httpStatus,

    -- ** AdminDisableUser
    adminDisableUser_userPoolId,
    adminDisableUser_username,
    adminDisableUserResponse_httpStatus,

    -- ** AdminEnableUser
    adminEnableUser_userPoolId,
    adminEnableUser_username,
    adminEnableUserResponse_httpStatus,

    -- ** AdminForgetDevice
    adminForgetDevice_userPoolId,
    adminForgetDevice_username,
    adminForgetDevice_deviceKey,

    -- ** AdminGetDevice
    adminGetDevice_deviceKey,
    adminGetDevice_userPoolId,
    adminGetDevice_username,
    adminGetDeviceResponse_httpStatus,
    adminGetDeviceResponse_device,

    -- ** AdminGetUser
    adminGetUser_userPoolId,
    adminGetUser_username,
    adminGetUserResponse_enabled,
    adminGetUserResponse_mfaOptions,
    adminGetUserResponse_preferredMfaSetting,
    adminGetUserResponse_userAttributes,
    adminGetUserResponse_userCreateDate,
    adminGetUserResponse_userLastModifiedDate,
    adminGetUserResponse_userMFASettingList,
    adminGetUserResponse_userStatus,
    adminGetUserResponse_httpStatus,
    adminGetUserResponse_username,

    -- ** AdminInitiateAuth
    adminInitiateAuth_analyticsMetadata,
    adminInitiateAuth_authParameters,
    adminInitiateAuth_clientMetadata,
    adminInitiateAuth_contextData,
    adminInitiateAuth_userPoolId,
    adminInitiateAuth_clientId,
    adminInitiateAuth_authFlow,
    adminInitiateAuthResponse_authenticationResult,
    adminInitiateAuthResponse_challengeName,
    adminInitiateAuthResponse_challengeParameters,
    adminInitiateAuthResponse_session,
    adminInitiateAuthResponse_httpStatus,

    -- ** AdminLinkProviderForUser
    adminLinkProviderForUser_userPoolId,
    adminLinkProviderForUser_destinationUser,
    adminLinkProviderForUser_sourceUser,
    adminLinkProviderForUserResponse_httpStatus,

    -- ** AdminListDevices
    adminListDevices_limit,
    adminListDevices_paginationToken,
    adminListDevices_userPoolId,
    adminListDevices_username,
    adminListDevicesResponse_devices,
    adminListDevicesResponse_paginationToken,
    adminListDevicesResponse_httpStatus,

    -- ** AdminListGroupsForUser
    adminListGroupsForUser_limit,
    adminListGroupsForUser_nextToken,
    adminListGroupsForUser_username,
    adminListGroupsForUser_userPoolId,
    adminListGroupsForUserResponse_groups,
    adminListGroupsForUserResponse_nextToken,
    adminListGroupsForUserResponse_httpStatus,

    -- ** AdminListUserAuthEvents
    adminListUserAuthEvents_maxResults,
    adminListUserAuthEvents_nextToken,
    adminListUserAuthEvents_userPoolId,
    adminListUserAuthEvents_username,
    adminListUserAuthEventsResponse_authEvents,
    adminListUserAuthEventsResponse_nextToken,
    adminListUserAuthEventsResponse_httpStatus,

    -- ** AdminRemoveUserFromGroup
    adminRemoveUserFromGroup_userPoolId,
    adminRemoveUserFromGroup_username,
    adminRemoveUserFromGroup_groupName,

    -- ** AdminResetUserPassword
    adminResetUserPassword_clientMetadata,
    adminResetUserPassword_userPoolId,
    adminResetUserPassword_username,
    adminResetUserPasswordResponse_httpStatus,

    -- ** AdminRespondToAuthChallenge
    adminRespondToAuthChallenge_analyticsMetadata,
    adminRespondToAuthChallenge_challengeResponses,
    adminRespondToAuthChallenge_clientMetadata,
    adminRespondToAuthChallenge_contextData,
    adminRespondToAuthChallenge_session,
    adminRespondToAuthChallenge_userPoolId,
    adminRespondToAuthChallenge_clientId,
    adminRespondToAuthChallenge_challengeName,
    adminRespondToAuthChallengeResponse_authenticationResult,
    adminRespondToAuthChallengeResponse_challengeName,
    adminRespondToAuthChallengeResponse_challengeParameters,
    adminRespondToAuthChallengeResponse_session,
    adminRespondToAuthChallengeResponse_httpStatus,

    -- ** AdminSetUserMFAPreference
    adminSetUserMFAPreference_sMSMfaSettings,
    adminSetUserMFAPreference_softwareTokenMfaSettings,
    adminSetUserMFAPreference_username,
    adminSetUserMFAPreference_userPoolId,
    adminSetUserMFAPreferenceResponse_httpStatus,

    -- ** AdminSetUserPassword
    adminSetUserPassword_permanent,
    adminSetUserPassword_userPoolId,
    adminSetUserPassword_username,
    adminSetUserPassword_password,
    adminSetUserPasswordResponse_httpStatus,

    -- ** AdminSetUserSettings
    adminSetUserSettings_userPoolId,
    adminSetUserSettings_username,
    adminSetUserSettings_mfaOptions,
    adminSetUserSettingsResponse_httpStatus,

    -- ** AdminUpdateAuthEventFeedback
    adminUpdateAuthEventFeedback_userPoolId,
    adminUpdateAuthEventFeedback_username,
    adminUpdateAuthEventFeedback_eventId,
    adminUpdateAuthEventFeedback_feedbackValue,
    adminUpdateAuthEventFeedbackResponse_httpStatus,

    -- ** AdminUpdateDeviceStatus
    adminUpdateDeviceStatus_deviceRememberedStatus,
    adminUpdateDeviceStatus_userPoolId,
    adminUpdateDeviceStatus_username,
    adminUpdateDeviceStatus_deviceKey,
    adminUpdateDeviceStatusResponse_httpStatus,

    -- ** AdminUpdateUserAttributes
    adminUpdateUserAttributes_clientMetadata,
    adminUpdateUserAttributes_userPoolId,
    adminUpdateUserAttributes_username,
    adminUpdateUserAttributes_userAttributes,
    adminUpdateUserAttributesResponse_httpStatus,

    -- ** AdminUserGlobalSignOut
    adminUserGlobalSignOut_userPoolId,
    adminUserGlobalSignOut_username,
    adminUserGlobalSignOutResponse_httpStatus,

    -- ** AssociateSoftwareToken
    associateSoftwareToken_accessToken,
    associateSoftwareToken_session,
    associateSoftwareTokenResponse_secretCode,
    associateSoftwareTokenResponse_session,
    associateSoftwareTokenResponse_httpStatus,

    -- ** ChangePassword
    changePassword_previousPassword,
    changePassword_proposedPassword,
    changePassword_accessToken,
    changePasswordResponse_httpStatus,

    -- ** ConfirmDevice
    confirmDevice_deviceName,
    confirmDevice_deviceSecretVerifierConfig,
    confirmDevice_accessToken,
    confirmDevice_deviceKey,
    confirmDeviceResponse_userConfirmationNecessary,
    confirmDeviceResponse_httpStatus,

    -- ** ConfirmForgotPassword
    confirmForgotPassword_analyticsMetadata,
    confirmForgotPassword_clientMetadata,
    confirmForgotPassword_secretHash,
    confirmForgotPassword_userContextData,
    confirmForgotPassword_clientId,
    confirmForgotPassword_username,
    confirmForgotPassword_confirmationCode,
    confirmForgotPassword_password,
    confirmForgotPasswordResponse_httpStatus,

    -- ** ConfirmSignUp
    confirmSignUp_analyticsMetadata,
    confirmSignUp_clientMetadata,
    confirmSignUp_forceAliasCreation,
    confirmSignUp_secretHash,
    confirmSignUp_userContextData,
    confirmSignUp_clientId,
    confirmSignUp_username,
    confirmSignUp_confirmationCode,
    confirmSignUpResponse_httpStatus,

    -- ** CreateGroup
    createGroup_description,
    createGroup_precedence,
    createGroup_roleArn,
    createGroup_groupName,
    createGroup_userPoolId,
    createGroupResponse_group,
    createGroupResponse_httpStatus,

    -- ** CreateIdentityProvider
    createIdentityProvider_attributeMapping,
    createIdentityProvider_idpIdentifiers,
    createIdentityProvider_userPoolId,
    createIdentityProvider_providerName,
    createIdentityProvider_providerType,
    createIdentityProvider_providerDetails,
    createIdentityProviderResponse_httpStatus,
    createIdentityProviderResponse_identityProvider,

    -- ** CreateResourceServer
    createResourceServer_scopes,
    createResourceServer_userPoolId,
    createResourceServer_identifier,
    createResourceServer_name,
    createResourceServerResponse_httpStatus,
    createResourceServerResponse_resourceServer,

    -- ** CreateUserImportJob
    createUserImportJob_jobName,
    createUserImportJob_userPoolId,
    createUserImportJob_cloudWatchLogsRoleArn,
    createUserImportJobResponse_userImportJob,
    createUserImportJobResponse_httpStatus,

    -- ** CreateUserPool
    createUserPool_accountRecoverySetting,
    createUserPool_adminCreateUserConfig,
    createUserPool_aliasAttributes,
    createUserPool_autoVerifiedAttributes,
    createUserPool_deletionProtection,
    createUserPool_deviceConfiguration,
    createUserPool_emailConfiguration,
    createUserPool_emailVerificationMessage,
    createUserPool_emailVerificationSubject,
    createUserPool_lambdaConfig,
    createUserPool_mfaConfiguration,
    createUserPool_policies,
    createUserPool_schema,
    createUserPool_smsAuthenticationMessage,
    createUserPool_smsConfiguration,
    createUserPool_smsVerificationMessage,
    createUserPool_userAttributeUpdateSettings,
    createUserPool_userPoolAddOns,
    createUserPool_userPoolTags,
    createUserPool_usernameAttributes,
    createUserPool_usernameConfiguration,
    createUserPool_verificationMessageTemplate,
    createUserPool_poolName,
    createUserPoolResponse_userPool,
    createUserPoolResponse_httpStatus,

    -- ** CreateUserPoolClient
    createUserPoolClient_accessTokenValidity,
    createUserPoolClient_allowedOAuthFlows,
    createUserPoolClient_allowedOAuthFlowsUserPoolClient,
    createUserPoolClient_allowedOAuthScopes,
    createUserPoolClient_analyticsConfiguration,
    createUserPoolClient_authSessionValidity,
    createUserPoolClient_callbackURLs,
    createUserPoolClient_defaultRedirectURI,
    createUserPoolClient_enablePropagateAdditionalUserContextData,
    createUserPoolClient_enableTokenRevocation,
    createUserPoolClient_explicitAuthFlows,
    createUserPoolClient_generateSecret,
    createUserPoolClient_idTokenValidity,
    createUserPoolClient_logoutURLs,
    createUserPoolClient_preventUserExistenceErrors,
    createUserPoolClient_readAttributes,
    createUserPoolClient_refreshTokenValidity,
    createUserPoolClient_supportedIdentityProviders,
    createUserPoolClient_tokenValidityUnits,
    createUserPoolClient_writeAttributes,
    createUserPoolClient_userPoolId,
    createUserPoolClient_clientName,
    createUserPoolClientResponse_userPoolClient,
    createUserPoolClientResponse_httpStatus,

    -- ** CreateUserPoolDomain
    createUserPoolDomain_customDomainConfig,
    createUserPoolDomain_domain,
    createUserPoolDomain_userPoolId,
    createUserPoolDomainResponse_cloudFrontDomain,
    createUserPoolDomainResponse_httpStatus,

    -- ** DeleteGroup
    deleteGroup_groupName,
    deleteGroup_userPoolId,

    -- ** DeleteIdentityProvider
    deleteIdentityProvider_userPoolId,
    deleteIdentityProvider_providerName,

    -- ** DeleteResourceServer
    deleteResourceServer_userPoolId,
    deleteResourceServer_identifier,

    -- ** DeleteUser
    deleteUser_accessToken,

    -- ** DeleteUserAttributes
    deleteUserAttributes_userAttributeNames,
    deleteUserAttributes_accessToken,
    deleteUserAttributesResponse_httpStatus,

    -- ** DeleteUserPool
    deleteUserPool_userPoolId,

    -- ** DeleteUserPoolClient
    deleteUserPoolClient_userPoolId,
    deleteUserPoolClient_clientId,

    -- ** DeleteUserPoolDomain
    deleteUserPoolDomain_domain,
    deleteUserPoolDomain_userPoolId,
    deleteUserPoolDomainResponse_httpStatus,

    -- ** DescribeIdentityProvider
    describeIdentityProvider_userPoolId,
    describeIdentityProvider_providerName,
    describeIdentityProviderResponse_httpStatus,
    describeIdentityProviderResponse_identityProvider,

    -- ** DescribeResourceServer
    describeResourceServer_userPoolId,
    describeResourceServer_identifier,
    describeResourceServerResponse_httpStatus,
    describeResourceServerResponse_resourceServer,

    -- ** DescribeRiskConfiguration
    describeRiskConfiguration_clientId,
    describeRiskConfiguration_userPoolId,
    describeRiskConfigurationResponse_httpStatus,
    describeRiskConfigurationResponse_riskConfiguration,

    -- ** DescribeUserImportJob
    describeUserImportJob_userPoolId,
    describeUserImportJob_jobId,
    describeUserImportJobResponse_userImportJob,
    describeUserImportJobResponse_httpStatus,

    -- ** DescribeUserPool
    describeUserPool_userPoolId,
    describeUserPoolResponse_userPool,
    describeUserPoolResponse_httpStatus,

    -- ** DescribeUserPoolClient
    describeUserPoolClient_userPoolId,
    describeUserPoolClient_clientId,
    describeUserPoolClientResponse_userPoolClient,
    describeUserPoolClientResponse_httpStatus,

    -- ** DescribeUserPoolDomain
    describeUserPoolDomain_domain,
    describeUserPoolDomainResponse_domainDescription,
    describeUserPoolDomainResponse_httpStatus,

    -- ** ForgetDevice
    forgetDevice_accessToken,
    forgetDevice_deviceKey,

    -- ** ForgotPassword
    forgotPassword_analyticsMetadata,
    forgotPassword_clientMetadata,
    forgotPassword_secretHash,
    forgotPassword_userContextData,
    forgotPassword_clientId,
    forgotPassword_username,
    forgotPasswordResponse_codeDeliveryDetails,
    forgotPasswordResponse_httpStatus,

    -- ** GetCSVHeader
    getCSVHeader_userPoolId,
    getCSVHeaderResponse_cSVHeader,
    getCSVHeaderResponse_userPoolId,
    getCSVHeaderResponse_httpStatus,

    -- ** GetDevice
    getDevice_accessToken,
    getDevice_deviceKey,
    getDeviceResponse_httpStatus,
    getDeviceResponse_device,

    -- ** GetGroup
    getGroup_groupName,
    getGroup_userPoolId,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** GetIdentityProviderByIdentifier
    getIdentityProviderByIdentifier_userPoolId,
    getIdentityProviderByIdentifier_idpIdentifier,
    getIdentityProviderByIdentifierResponse_httpStatus,
    getIdentityProviderByIdentifierResponse_identityProvider,

    -- ** GetSigningCertificate
    getSigningCertificate_userPoolId,
    getSigningCertificateResponse_certificate,
    getSigningCertificateResponse_httpStatus,

    -- ** GetUICustomization
    getUICustomization_clientId,
    getUICustomization_userPoolId,
    getUICustomizationResponse_httpStatus,
    getUICustomizationResponse_uICustomization,

    -- ** GetUser
    getUser_accessToken,
    getUserResponse_mfaOptions,
    getUserResponse_preferredMfaSetting,
    getUserResponse_userMFASettingList,
    getUserResponse_httpStatus,
    getUserResponse_username,
    getUserResponse_userAttributes,

    -- ** GetUserAttributeVerificationCode
    getUserAttributeVerificationCode_clientMetadata,
    getUserAttributeVerificationCode_accessToken,
    getUserAttributeVerificationCode_attributeName,
    getUserAttributeVerificationCodeResponse_codeDeliveryDetails,
    getUserAttributeVerificationCodeResponse_httpStatus,

    -- ** GetUserPoolMfaConfig
    getUserPoolMfaConfig_userPoolId,
    getUserPoolMfaConfigResponse_mfaConfiguration,
    getUserPoolMfaConfigResponse_smsMfaConfiguration,
    getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
    getUserPoolMfaConfigResponse_httpStatus,

    -- ** GlobalSignOut
    globalSignOut_accessToken,
    globalSignOutResponse_httpStatus,

    -- ** InitiateAuth
    initiateAuth_analyticsMetadata,
    initiateAuth_authParameters,
    initiateAuth_clientMetadata,
    initiateAuth_userContextData,
    initiateAuth_authFlow,
    initiateAuth_clientId,
    initiateAuthResponse_authenticationResult,
    initiateAuthResponse_challengeName,
    initiateAuthResponse_challengeParameters,
    initiateAuthResponse_session,
    initiateAuthResponse_httpStatus,

    -- ** ListDevices
    listDevices_limit,
    listDevices_paginationToken,
    listDevices_accessToken,
    listDevicesResponse_devices,
    listDevicesResponse_paginationToken,
    listDevicesResponse_httpStatus,

    -- ** ListGroups
    listGroups_limit,
    listGroups_nextToken,
    listGroups_userPoolId,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,

    -- ** ListIdentityProviders
    listIdentityProviders_maxResults,
    listIdentityProviders_nextToken,
    listIdentityProviders_userPoolId,
    listIdentityProvidersResponse_nextToken,
    listIdentityProvidersResponse_httpStatus,
    listIdentityProvidersResponse_providers,

    -- ** ListResourceServers
    listResourceServers_maxResults,
    listResourceServers_nextToken,
    listResourceServers_userPoolId,
    listResourceServersResponse_nextToken,
    listResourceServersResponse_httpStatus,
    listResourceServersResponse_resourceServers,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUserImportJobs
    listUserImportJobs_paginationToken,
    listUserImportJobs_userPoolId,
    listUserImportJobs_maxResults,
    listUserImportJobsResponse_paginationToken,
    listUserImportJobsResponse_userImportJobs,
    listUserImportJobsResponse_httpStatus,

    -- ** ListUserPoolClients
    listUserPoolClients_maxResults,
    listUserPoolClients_nextToken,
    listUserPoolClients_userPoolId,
    listUserPoolClientsResponse_nextToken,
    listUserPoolClientsResponse_userPoolClients,
    listUserPoolClientsResponse_httpStatus,

    -- ** ListUserPools
    listUserPools_nextToken,
    listUserPools_maxResults,
    listUserPoolsResponse_nextToken,
    listUserPoolsResponse_userPools,
    listUserPoolsResponse_httpStatus,

    -- ** ListUsers
    listUsers_attributesToGet,
    listUsers_filter,
    listUsers_limit,
    listUsers_paginationToken,
    listUsers_userPoolId,
    listUsersResponse_paginationToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** ListUsersInGroup
    listUsersInGroup_limit,
    listUsersInGroup_nextToken,
    listUsersInGroup_userPoolId,
    listUsersInGroup_groupName,
    listUsersInGroupResponse_nextToken,
    listUsersInGroupResponse_users,
    listUsersInGroupResponse_httpStatus,

    -- ** ResendConfirmationCode
    resendConfirmationCode_analyticsMetadata,
    resendConfirmationCode_clientMetadata,
    resendConfirmationCode_secretHash,
    resendConfirmationCode_userContextData,
    resendConfirmationCode_clientId,
    resendConfirmationCode_username,
    resendConfirmationCodeResponse_codeDeliveryDetails,
    resendConfirmationCodeResponse_httpStatus,

    -- ** RespondToAuthChallenge
    respondToAuthChallenge_analyticsMetadata,
    respondToAuthChallenge_challengeResponses,
    respondToAuthChallenge_clientMetadata,
    respondToAuthChallenge_session,
    respondToAuthChallenge_userContextData,
    respondToAuthChallenge_clientId,
    respondToAuthChallenge_challengeName,
    respondToAuthChallengeResponse_authenticationResult,
    respondToAuthChallengeResponse_challengeName,
    respondToAuthChallengeResponse_challengeParameters,
    respondToAuthChallengeResponse_session,
    respondToAuthChallengeResponse_httpStatus,

    -- ** RevokeToken
    revokeToken_clientSecret,
    revokeToken_token,
    revokeToken_clientId,
    revokeTokenResponse_httpStatus,

    -- ** SetRiskConfiguration
    setRiskConfiguration_accountTakeoverRiskConfiguration,
    setRiskConfiguration_clientId,
    setRiskConfiguration_compromisedCredentialsRiskConfiguration,
    setRiskConfiguration_riskExceptionConfiguration,
    setRiskConfiguration_userPoolId,
    setRiskConfigurationResponse_httpStatus,
    setRiskConfigurationResponse_riskConfiguration,

    -- ** SetUICustomization
    setUICustomization_css,
    setUICustomization_clientId,
    setUICustomization_imageFile,
    setUICustomization_userPoolId,
    setUICustomizationResponse_httpStatus,
    setUICustomizationResponse_uICustomization,

    -- ** SetUserMFAPreference
    setUserMFAPreference_sMSMfaSettings,
    setUserMFAPreference_softwareTokenMfaSettings,
    setUserMFAPreference_accessToken,
    setUserMFAPreferenceResponse_httpStatus,

    -- ** SetUserPoolMfaConfig
    setUserPoolMfaConfig_mfaConfiguration,
    setUserPoolMfaConfig_smsMfaConfiguration,
    setUserPoolMfaConfig_softwareTokenMfaConfiguration,
    setUserPoolMfaConfig_userPoolId,
    setUserPoolMfaConfigResponse_mfaConfiguration,
    setUserPoolMfaConfigResponse_smsMfaConfiguration,
    setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
    setUserPoolMfaConfigResponse_httpStatus,

    -- ** SetUserSettings
    setUserSettings_accessToken,
    setUserSettings_mfaOptions,
    setUserSettingsResponse_httpStatus,

    -- ** SignUp
    signUp_analyticsMetadata,
    signUp_clientMetadata,
    signUp_secretHash,
    signUp_userAttributes,
    signUp_userContextData,
    signUp_validationData,
    signUp_clientId,
    signUp_username,
    signUp_password,
    signUpResponse_codeDeliveryDetails,
    signUpResponse_httpStatus,
    signUpResponse_userConfirmed,
    signUpResponse_userSub,

    -- ** StartUserImportJob
    startUserImportJob_userPoolId,
    startUserImportJob_jobId,
    startUserImportJobResponse_userImportJob,
    startUserImportJobResponse_httpStatus,

    -- ** StopUserImportJob
    stopUserImportJob_userPoolId,
    stopUserImportJob_jobId,
    stopUserImportJobResponse_userImportJob,
    stopUserImportJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAuthEventFeedback
    updateAuthEventFeedback_userPoolId,
    updateAuthEventFeedback_username,
    updateAuthEventFeedback_eventId,
    updateAuthEventFeedback_feedbackToken,
    updateAuthEventFeedback_feedbackValue,
    updateAuthEventFeedbackResponse_httpStatus,

    -- ** UpdateDeviceStatus
    updateDeviceStatus_deviceRememberedStatus,
    updateDeviceStatus_accessToken,
    updateDeviceStatus_deviceKey,
    updateDeviceStatusResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_description,
    updateGroup_precedence,
    updateGroup_roleArn,
    updateGroup_groupName,
    updateGroup_userPoolId,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** UpdateIdentityProvider
    updateIdentityProvider_attributeMapping,
    updateIdentityProvider_idpIdentifiers,
    updateIdentityProvider_providerDetails,
    updateIdentityProvider_userPoolId,
    updateIdentityProvider_providerName,
    updateIdentityProviderResponse_httpStatus,
    updateIdentityProviderResponse_identityProvider,

    -- ** UpdateResourceServer
    updateResourceServer_scopes,
    updateResourceServer_userPoolId,
    updateResourceServer_identifier,
    updateResourceServer_name,
    updateResourceServerResponse_httpStatus,
    updateResourceServerResponse_resourceServer,

    -- ** UpdateUserAttributes
    updateUserAttributes_clientMetadata,
    updateUserAttributes_userAttributes,
    updateUserAttributes_accessToken,
    updateUserAttributesResponse_codeDeliveryDetailsList,
    updateUserAttributesResponse_httpStatus,

    -- ** UpdateUserPool
    updateUserPool_accountRecoverySetting,
    updateUserPool_adminCreateUserConfig,
    updateUserPool_autoVerifiedAttributes,
    updateUserPool_deletionProtection,
    updateUserPool_deviceConfiguration,
    updateUserPool_emailConfiguration,
    updateUserPool_emailVerificationMessage,
    updateUserPool_emailVerificationSubject,
    updateUserPool_lambdaConfig,
    updateUserPool_mfaConfiguration,
    updateUserPool_policies,
    updateUserPool_smsAuthenticationMessage,
    updateUserPool_smsConfiguration,
    updateUserPool_smsVerificationMessage,
    updateUserPool_userAttributeUpdateSettings,
    updateUserPool_userPoolAddOns,
    updateUserPool_userPoolTags,
    updateUserPool_verificationMessageTemplate,
    updateUserPool_userPoolId,
    updateUserPoolResponse_httpStatus,

    -- ** UpdateUserPoolClient
    updateUserPoolClient_accessTokenValidity,
    updateUserPoolClient_allowedOAuthFlows,
    updateUserPoolClient_allowedOAuthFlowsUserPoolClient,
    updateUserPoolClient_allowedOAuthScopes,
    updateUserPoolClient_analyticsConfiguration,
    updateUserPoolClient_authSessionValidity,
    updateUserPoolClient_callbackURLs,
    updateUserPoolClient_clientName,
    updateUserPoolClient_defaultRedirectURI,
    updateUserPoolClient_enablePropagateAdditionalUserContextData,
    updateUserPoolClient_enableTokenRevocation,
    updateUserPoolClient_explicitAuthFlows,
    updateUserPoolClient_idTokenValidity,
    updateUserPoolClient_logoutURLs,
    updateUserPoolClient_preventUserExistenceErrors,
    updateUserPoolClient_readAttributes,
    updateUserPoolClient_refreshTokenValidity,
    updateUserPoolClient_supportedIdentityProviders,
    updateUserPoolClient_tokenValidityUnits,
    updateUserPoolClient_writeAttributes,
    updateUserPoolClient_userPoolId,
    updateUserPoolClient_clientId,
    updateUserPoolClientResponse_userPoolClient,
    updateUserPoolClientResponse_httpStatus,

    -- ** UpdateUserPoolDomain
    updateUserPoolDomain_domain,
    updateUserPoolDomain_userPoolId,
    updateUserPoolDomain_customDomainConfig,
    updateUserPoolDomainResponse_cloudFrontDomain,
    updateUserPoolDomainResponse_httpStatus,

    -- ** VerifySoftwareToken
    verifySoftwareToken_accessToken,
    verifySoftwareToken_friendlyDeviceName,
    verifySoftwareToken_session,
    verifySoftwareToken_userCode,
    verifySoftwareTokenResponse_session,
    verifySoftwareTokenResponse_status,
    verifySoftwareTokenResponse_httpStatus,

    -- ** VerifyUserAttribute
    verifyUserAttribute_accessToken,
    verifyUserAttribute_attributeName,
    verifyUserAttribute_code,
    verifyUserAttributeResponse_httpStatus,

    -- * Types

    -- ** AccountRecoverySettingType
    accountRecoverySettingType_recoveryMechanisms,

    -- ** AccountTakeoverActionType
    accountTakeoverActionType_notify,
    accountTakeoverActionType_eventAction,

    -- ** AccountTakeoverActionsType
    accountTakeoverActionsType_highAction,
    accountTakeoverActionsType_lowAction,
    accountTakeoverActionsType_mediumAction,

    -- ** AccountTakeoverRiskConfigurationType
    accountTakeoverRiskConfigurationType_notifyConfiguration,
    accountTakeoverRiskConfigurationType_actions,

    -- ** AdminCreateUserConfigType
    adminCreateUserConfigType_allowAdminCreateUserOnly,
    adminCreateUserConfigType_inviteMessageTemplate,
    adminCreateUserConfigType_unusedAccountValidityDays,

    -- ** AnalyticsConfigurationType
    analyticsConfigurationType_applicationArn,
    analyticsConfigurationType_applicationId,
    analyticsConfigurationType_externalId,
    analyticsConfigurationType_roleArn,
    analyticsConfigurationType_userDataShared,

    -- ** AnalyticsMetadataType
    analyticsMetadataType_analyticsEndpointId,

    -- ** AttributeType
    attributeType_value,
    attributeType_name,

    -- ** AuthEventType
    authEventType_challengeResponses,
    authEventType_creationDate,
    authEventType_eventContextData,
    authEventType_eventFeedback,
    authEventType_eventId,
    authEventType_eventResponse,
    authEventType_eventRisk,
    authEventType_eventType,

    -- ** AuthenticationResultType
    authenticationResultType_accessToken,
    authenticationResultType_expiresIn,
    authenticationResultType_idToken,
    authenticationResultType_newDeviceMetadata,
    authenticationResultType_refreshToken,
    authenticationResultType_tokenType,

    -- ** ChallengeResponseType
    challengeResponseType_challengeName,
    challengeResponseType_challengeResponse,

    -- ** CodeDeliveryDetailsType
    codeDeliveryDetailsType_attributeName,
    codeDeliveryDetailsType_deliveryMedium,
    codeDeliveryDetailsType_destination,

    -- ** CompromisedCredentialsActionsType
    compromisedCredentialsActionsType_eventAction,

    -- ** CompromisedCredentialsRiskConfigurationType
    compromisedCredentialsRiskConfigurationType_eventFilter,
    compromisedCredentialsRiskConfigurationType_actions,

    -- ** ContextDataType
    contextDataType_encodedData,
    contextDataType_ipAddress,
    contextDataType_serverName,
    contextDataType_serverPath,
    contextDataType_httpHeaders,

    -- ** CustomDomainConfigType
    customDomainConfigType_certificateArn,

    -- ** CustomEmailLambdaVersionConfigType
    customEmailLambdaVersionConfigType_lambdaVersion,
    customEmailLambdaVersionConfigType_lambdaArn,

    -- ** CustomSMSLambdaVersionConfigType
    customSMSLambdaVersionConfigType_lambdaVersion,
    customSMSLambdaVersionConfigType_lambdaArn,

    -- ** DeviceConfigurationType
    deviceConfigurationType_challengeRequiredOnNewDevice,
    deviceConfigurationType_deviceOnlyRememberedOnUserPrompt,

    -- ** DeviceSecretVerifierConfigType
    deviceSecretVerifierConfigType_passwordVerifier,
    deviceSecretVerifierConfigType_salt,

    -- ** DeviceType
    deviceType_deviceAttributes,
    deviceType_deviceCreateDate,
    deviceType_deviceKey,
    deviceType_deviceLastAuthenticatedDate,
    deviceType_deviceLastModifiedDate,

    -- ** DomainDescriptionType
    domainDescriptionType_aWSAccountId,
    domainDescriptionType_cloudFrontDistribution,
    domainDescriptionType_customDomainConfig,
    domainDescriptionType_domain,
    domainDescriptionType_s3Bucket,
    domainDescriptionType_status,
    domainDescriptionType_userPoolId,
    domainDescriptionType_version,

    -- ** EmailConfigurationType
    emailConfigurationType_configurationSet,
    emailConfigurationType_emailSendingAccount,
    emailConfigurationType_from,
    emailConfigurationType_replyToEmailAddress,
    emailConfigurationType_sourceArn,

    -- ** EventContextDataType
    eventContextDataType_city,
    eventContextDataType_country,
    eventContextDataType_deviceName,
    eventContextDataType_ipAddress,
    eventContextDataType_timezone,

    -- ** EventFeedbackType
    eventFeedbackType_feedbackDate,
    eventFeedbackType_feedbackValue,
    eventFeedbackType_provider,

    -- ** EventRiskType
    eventRiskType_compromisedCredentialsDetected,
    eventRiskType_riskDecision,
    eventRiskType_riskLevel,

    -- ** GroupType
    groupType_creationDate,
    groupType_description,
    groupType_groupName,
    groupType_lastModifiedDate,
    groupType_precedence,
    groupType_roleArn,
    groupType_userPoolId,

    -- ** HttpHeader
    httpHeader_headerName,
    httpHeader_headerValue,

    -- ** IdentityProviderType
    identityProviderType_attributeMapping,
    identityProviderType_creationDate,
    identityProviderType_idpIdentifiers,
    identityProviderType_lastModifiedDate,
    identityProviderType_providerDetails,
    identityProviderType_providerName,
    identityProviderType_providerType,
    identityProviderType_userPoolId,

    -- ** LambdaConfigType
    lambdaConfigType_createAuthChallenge,
    lambdaConfigType_customEmailSender,
    lambdaConfigType_customMessage,
    lambdaConfigType_customSMSSender,
    lambdaConfigType_defineAuthChallenge,
    lambdaConfigType_kmsKeyID,
    lambdaConfigType_postAuthentication,
    lambdaConfigType_postConfirmation,
    lambdaConfigType_preAuthentication,
    lambdaConfigType_preSignUp,
    lambdaConfigType_preTokenGeneration,
    lambdaConfigType_userMigration,
    lambdaConfigType_verifyAuthChallengeResponse,

    -- ** MFAOptionType
    mfaOptionType_attributeName,
    mfaOptionType_deliveryMedium,

    -- ** MessageTemplateType
    messageTemplateType_emailMessage,
    messageTemplateType_emailSubject,
    messageTemplateType_sMSMessage,

    -- ** NewDeviceMetadataType
    newDeviceMetadataType_deviceGroupKey,
    newDeviceMetadataType_deviceKey,

    -- ** NotifyConfigurationType
    notifyConfigurationType_blockEmail,
    notifyConfigurationType_from,
    notifyConfigurationType_mfaEmail,
    notifyConfigurationType_noActionEmail,
    notifyConfigurationType_replyTo,
    notifyConfigurationType_sourceArn,

    -- ** NotifyEmailType
    notifyEmailType_htmlBody,
    notifyEmailType_textBody,
    notifyEmailType_subject,

    -- ** NumberAttributeConstraintsType
    numberAttributeConstraintsType_maxValue,
    numberAttributeConstraintsType_minValue,

    -- ** PasswordPolicyType
    passwordPolicyType_minimumLength,
    passwordPolicyType_requireLowercase,
    passwordPolicyType_requireNumbers,
    passwordPolicyType_requireSymbols,
    passwordPolicyType_requireUppercase,
    passwordPolicyType_temporaryPasswordValidityDays,

    -- ** ProviderDescription
    providerDescription_creationDate,
    providerDescription_lastModifiedDate,
    providerDescription_providerName,
    providerDescription_providerType,

    -- ** ProviderUserIdentifierType
    providerUserIdentifierType_providerAttributeName,
    providerUserIdentifierType_providerAttributeValue,
    providerUserIdentifierType_providerName,

    -- ** RecoveryOptionType
    recoveryOptionType_priority,
    recoveryOptionType_name,

    -- ** ResourceServerScopeType
    resourceServerScopeType_scopeName,
    resourceServerScopeType_scopeDescription,

    -- ** ResourceServerType
    resourceServerType_identifier,
    resourceServerType_name,
    resourceServerType_scopes,
    resourceServerType_userPoolId,

    -- ** RiskConfigurationType
    riskConfigurationType_accountTakeoverRiskConfiguration,
    riskConfigurationType_clientId,
    riskConfigurationType_compromisedCredentialsRiskConfiguration,
    riskConfigurationType_lastModifiedDate,
    riskConfigurationType_riskExceptionConfiguration,
    riskConfigurationType_userPoolId,

    -- ** RiskExceptionConfigurationType
    riskExceptionConfigurationType_blockedIPRangeList,
    riskExceptionConfigurationType_skippedIPRangeList,

    -- ** SMSMfaSettingsType
    sMSMfaSettingsType_enabled,
    sMSMfaSettingsType_preferredMfa,

    -- ** SchemaAttributeType
    schemaAttributeType_attributeDataType,
    schemaAttributeType_developerOnlyAttribute,
    schemaAttributeType_mutable,
    schemaAttributeType_name,
    schemaAttributeType_numberAttributeConstraints,
    schemaAttributeType_required,
    schemaAttributeType_stringAttributeConstraints,

    -- ** SmsConfigurationType
    smsConfigurationType_externalId,
    smsConfigurationType_snsRegion,
    smsConfigurationType_snsCallerArn,

    -- ** SmsMfaConfigType
    smsMfaConfigType_smsAuthenticationMessage,
    smsMfaConfigType_smsConfiguration,

    -- ** SoftwareTokenMfaConfigType
    softwareTokenMfaConfigType_enabled,

    -- ** SoftwareTokenMfaSettingsType
    softwareTokenMfaSettingsType_enabled,
    softwareTokenMfaSettingsType_preferredMfa,

    -- ** StringAttributeConstraintsType
    stringAttributeConstraintsType_maxLength,
    stringAttributeConstraintsType_minLength,

    -- ** TokenValidityUnitsType
    tokenValidityUnitsType_accessToken,
    tokenValidityUnitsType_idToken,
    tokenValidityUnitsType_refreshToken,

    -- ** UICustomizationType
    uICustomizationType_css,
    uICustomizationType_cSSVersion,
    uICustomizationType_clientId,
    uICustomizationType_creationDate,
    uICustomizationType_imageUrl,
    uICustomizationType_lastModifiedDate,
    uICustomizationType_userPoolId,

    -- ** UserAttributeUpdateSettingsType
    userAttributeUpdateSettingsType_attributesRequireVerificationBeforeUpdate,

    -- ** UserContextDataType
    userContextDataType_encodedData,
    userContextDataType_ipAddress,

    -- ** UserImportJobType
    userImportJobType_cloudWatchLogsRoleArn,
    userImportJobType_completionDate,
    userImportJobType_completionMessage,
    userImportJobType_creationDate,
    userImportJobType_failedUsers,
    userImportJobType_importedUsers,
    userImportJobType_jobId,
    userImportJobType_jobName,
    userImportJobType_preSignedUrl,
    userImportJobType_skippedUsers,
    userImportJobType_startDate,
    userImportJobType_status,
    userImportJobType_userPoolId,

    -- ** UserPoolAddOnsType
    userPoolAddOnsType_advancedSecurityMode,

    -- ** UserPoolClientDescription
    userPoolClientDescription_clientId,
    userPoolClientDescription_clientName,
    userPoolClientDescription_userPoolId,

    -- ** UserPoolClientType
    userPoolClientType_accessTokenValidity,
    userPoolClientType_allowedOAuthFlows,
    userPoolClientType_allowedOAuthFlowsUserPoolClient,
    userPoolClientType_allowedOAuthScopes,
    userPoolClientType_analyticsConfiguration,
    userPoolClientType_authSessionValidity,
    userPoolClientType_callbackURLs,
    userPoolClientType_clientId,
    userPoolClientType_clientName,
    userPoolClientType_clientSecret,
    userPoolClientType_creationDate,
    userPoolClientType_defaultRedirectURI,
    userPoolClientType_enablePropagateAdditionalUserContextData,
    userPoolClientType_enableTokenRevocation,
    userPoolClientType_explicitAuthFlows,
    userPoolClientType_idTokenValidity,
    userPoolClientType_lastModifiedDate,
    userPoolClientType_logoutURLs,
    userPoolClientType_preventUserExistenceErrors,
    userPoolClientType_readAttributes,
    userPoolClientType_refreshTokenValidity,
    userPoolClientType_supportedIdentityProviders,
    userPoolClientType_tokenValidityUnits,
    userPoolClientType_userPoolId,
    userPoolClientType_writeAttributes,

    -- ** UserPoolDescriptionType
    userPoolDescriptionType_creationDate,
    userPoolDescriptionType_id,
    userPoolDescriptionType_lambdaConfig,
    userPoolDescriptionType_lastModifiedDate,
    userPoolDescriptionType_name,
    userPoolDescriptionType_status,

    -- ** UserPoolPolicyType
    userPoolPolicyType_passwordPolicy,

    -- ** UserPoolType
    userPoolType_accountRecoverySetting,
    userPoolType_adminCreateUserConfig,
    userPoolType_aliasAttributes,
    userPoolType_arn,
    userPoolType_autoVerifiedAttributes,
    userPoolType_creationDate,
    userPoolType_customDomain,
    userPoolType_deletionProtection,
    userPoolType_deviceConfiguration,
    userPoolType_domain,
    userPoolType_emailConfiguration,
    userPoolType_emailConfigurationFailure,
    userPoolType_emailVerificationMessage,
    userPoolType_emailVerificationSubject,
    userPoolType_estimatedNumberOfUsers,
    userPoolType_id,
    userPoolType_lambdaConfig,
    userPoolType_lastModifiedDate,
    userPoolType_mfaConfiguration,
    userPoolType_name,
    userPoolType_policies,
    userPoolType_schemaAttributes,
    userPoolType_smsAuthenticationMessage,
    userPoolType_smsConfiguration,
    userPoolType_smsConfigurationFailure,
    userPoolType_smsVerificationMessage,
    userPoolType_status,
    userPoolType_userAttributeUpdateSettings,
    userPoolType_userPoolAddOns,
    userPoolType_userPoolTags,
    userPoolType_usernameAttributes,
    userPoolType_usernameConfiguration,
    userPoolType_verificationMessageTemplate,

    -- ** UserType
    userType_attributes,
    userType_enabled,
    userType_mfaOptions,
    userType_userCreateDate,
    userType_userLastModifiedDate,
    userType_userStatus,
    userType_username,

    -- ** UsernameConfigurationType
    usernameConfigurationType_caseSensitive,

    -- ** VerificationMessageTemplateType
    verificationMessageTemplateType_defaultEmailOption,
    verificationMessageTemplateType_emailMessage,
    verificationMessageTemplateType_emailMessageByLink,
    verificationMessageTemplateType_emailSubject,
    verificationMessageTemplateType_emailSubjectByLink,
    verificationMessageTemplateType_smsMessage,
  )
where

import Amazonka.CognitoIdentityProvider.AddCustomAttributes
import Amazonka.CognitoIdentityProvider.AdminAddUserToGroup
import Amazonka.CognitoIdentityProvider.AdminConfirmSignUp
import Amazonka.CognitoIdentityProvider.AdminCreateUser
import Amazonka.CognitoIdentityProvider.AdminDeleteUser
import Amazonka.CognitoIdentityProvider.AdminDeleteUserAttributes
import Amazonka.CognitoIdentityProvider.AdminDisableProviderForUser
import Amazonka.CognitoIdentityProvider.AdminDisableUser
import Amazonka.CognitoIdentityProvider.AdminEnableUser
import Amazonka.CognitoIdentityProvider.AdminForgetDevice
import Amazonka.CognitoIdentityProvider.AdminGetDevice
import Amazonka.CognitoIdentityProvider.AdminGetUser
import Amazonka.CognitoIdentityProvider.AdminInitiateAuth
import Amazonka.CognitoIdentityProvider.AdminLinkProviderForUser
import Amazonka.CognitoIdentityProvider.AdminListDevices
import Amazonka.CognitoIdentityProvider.AdminListGroupsForUser
import Amazonka.CognitoIdentityProvider.AdminListUserAuthEvents
import Amazonka.CognitoIdentityProvider.AdminRemoveUserFromGroup
import Amazonka.CognitoIdentityProvider.AdminResetUserPassword
import Amazonka.CognitoIdentityProvider.AdminRespondToAuthChallenge
import Amazonka.CognitoIdentityProvider.AdminSetUserMFAPreference
import Amazonka.CognitoIdentityProvider.AdminSetUserPassword
import Amazonka.CognitoIdentityProvider.AdminSetUserSettings
import Amazonka.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
import Amazonka.CognitoIdentityProvider.AdminUpdateDeviceStatus
import Amazonka.CognitoIdentityProvider.AdminUpdateUserAttributes
import Amazonka.CognitoIdentityProvider.AdminUserGlobalSignOut
import Amazonka.CognitoIdentityProvider.AssociateSoftwareToken
import Amazonka.CognitoIdentityProvider.ChangePassword
import Amazonka.CognitoIdentityProvider.ConfirmDevice
import Amazonka.CognitoIdentityProvider.ConfirmForgotPassword
import Amazonka.CognitoIdentityProvider.ConfirmSignUp
import Amazonka.CognitoIdentityProvider.CreateGroup
import Amazonka.CognitoIdentityProvider.CreateIdentityProvider
import Amazonka.CognitoIdentityProvider.CreateResourceServer
import Amazonka.CognitoIdentityProvider.CreateUserImportJob
import Amazonka.CognitoIdentityProvider.CreateUserPool
import Amazonka.CognitoIdentityProvider.CreateUserPoolClient
import Amazonka.CognitoIdentityProvider.CreateUserPoolDomain
import Amazonka.CognitoIdentityProvider.DeleteGroup
import Amazonka.CognitoIdentityProvider.DeleteIdentityProvider
import Amazonka.CognitoIdentityProvider.DeleteResourceServer
import Amazonka.CognitoIdentityProvider.DeleteUser
import Amazonka.CognitoIdentityProvider.DeleteUserAttributes
import Amazonka.CognitoIdentityProvider.DeleteUserPool
import Amazonka.CognitoIdentityProvider.DeleteUserPoolClient
import Amazonka.CognitoIdentityProvider.DeleteUserPoolDomain
import Amazonka.CognitoIdentityProvider.DescribeIdentityProvider
import Amazonka.CognitoIdentityProvider.DescribeResourceServer
import Amazonka.CognitoIdentityProvider.DescribeRiskConfiguration
import Amazonka.CognitoIdentityProvider.DescribeUserImportJob
import Amazonka.CognitoIdentityProvider.DescribeUserPool
import Amazonka.CognitoIdentityProvider.DescribeUserPoolClient
import Amazonka.CognitoIdentityProvider.DescribeUserPoolDomain
import Amazonka.CognitoIdentityProvider.ForgetDevice
import Amazonka.CognitoIdentityProvider.ForgotPassword
import Amazonka.CognitoIdentityProvider.GetCSVHeader
import Amazonka.CognitoIdentityProvider.GetDevice
import Amazonka.CognitoIdentityProvider.GetGroup
import Amazonka.CognitoIdentityProvider.GetIdentityProviderByIdentifier
import Amazonka.CognitoIdentityProvider.GetSigningCertificate
import Amazonka.CognitoIdentityProvider.GetUICustomization
import Amazonka.CognitoIdentityProvider.GetUser
import Amazonka.CognitoIdentityProvider.GetUserAttributeVerificationCode
import Amazonka.CognitoIdentityProvider.GetUserPoolMfaConfig
import Amazonka.CognitoIdentityProvider.GlobalSignOut
import Amazonka.CognitoIdentityProvider.InitiateAuth
import Amazonka.CognitoIdentityProvider.ListDevices
import Amazonka.CognitoIdentityProvider.ListGroups
import Amazonka.CognitoIdentityProvider.ListIdentityProviders
import Amazonka.CognitoIdentityProvider.ListResourceServers
import Amazonka.CognitoIdentityProvider.ListTagsForResource
import Amazonka.CognitoIdentityProvider.ListUserImportJobs
import Amazonka.CognitoIdentityProvider.ListUserPoolClients
import Amazonka.CognitoIdentityProvider.ListUserPools
import Amazonka.CognitoIdentityProvider.ListUsers
import Amazonka.CognitoIdentityProvider.ListUsersInGroup
import Amazonka.CognitoIdentityProvider.ResendConfirmationCode
import Amazonka.CognitoIdentityProvider.RespondToAuthChallenge
import Amazonka.CognitoIdentityProvider.RevokeToken
import Amazonka.CognitoIdentityProvider.SetRiskConfiguration
import Amazonka.CognitoIdentityProvider.SetUICustomization
import Amazonka.CognitoIdentityProvider.SetUserMFAPreference
import Amazonka.CognitoIdentityProvider.SetUserPoolMfaConfig
import Amazonka.CognitoIdentityProvider.SetUserSettings
import Amazonka.CognitoIdentityProvider.SignUp
import Amazonka.CognitoIdentityProvider.StartUserImportJob
import Amazonka.CognitoIdentityProvider.StopUserImportJob
import Amazonka.CognitoIdentityProvider.TagResource
import Amazonka.CognitoIdentityProvider.Types.AccountRecoverySettingType
import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionType
import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionsType
import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
import Amazonka.CognitoIdentityProvider.Types.AdminCreateUserConfigType
import Amazonka.CognitoIdentityProvider.Types.AnalyticsConfigurationType
import Amazonka.CognitoIdentityProvider.Types.AnalyticsMetadataType
import Amazonka.CognitoIdentityProvider.Types.AttributeType
import Amazonka.CognitoIdentityProvider.Types.AuthEventType
import Amazonka.CognitoIdentityProvider.Types.AuthenticationResultType
import Amazonka.CognitoIdentityProvider.Types.ChallengeResponseType
import Amazonka.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
import Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
import Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
import Amazonka.CognitoIdentityProvider.Types.ContextDataType
import Amazonka.CognitoIdentityProvider.Types.CustomDomainConfigType
import Amazonka.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
import Amazonka.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
import Amazonka.CognitoIdentityProvider.Types.DeviceConfigurationType
import Amazonka.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
import Amazonka.CognitoIdentityProvider.Types.DeviceType
import Amazonka.CognitoIdentityProvider.Types.DomainDescriptionType
import Amazonka.CognitoIdentityProvider.Types.EmailConfigurationType
import Amazonka.CognitoIdentityProvider.Types.EventContextDataType
import Amazonka.CognitoIdentityProvider.Types.EventFeedbackType
import Amazonka.CognitoIdentityProvider.Types.EventRiskType
import Amazonka.CognitoIdentityProvider.Types.GroupType
import Amazonka.CognitoIdentityProvider.Types.HttpHeader
import Amazonka.CognitoIdentityProvider.Types.IdentityProviderType
import Amazonka.CognitoIdentityProvider.Types.LambdaConfigType
import Amazonka.CognitoIdentityProvider.Types.MFAOptionType
import Amazonka.CognitoIdentityProvider.Types.MessageTemplateType
import Amazonka.CognitoIdentityProvider.Types.NewDeviceMetadataType
import Amazonka.CognitoIdentityProvider.Types.NotifyConfigurationType
import Amazonka.CognitoIdentityProvider.Types.NotifyEmailType
import Amazonka.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
import Amazonka.CognitoIdentityProvider.Types.PasswordPolicyType
import Amazonka.CognitoIdentityProvider.Types.ProviderDescription
import Amazonka.CognitoIdentityProvider.Types.ProviderUserIdentifierType
import Amazonka.CognitoIdentityProvider.Types.RecoveryOptionType
import Amazonka.CognitoIdentityProvider.Types.ResourceServerScopeType
import Amazonka.CognitoIdentityProvider.Types.ResourceServerType
import Amazonka.CognitoIdentityProvider.Types.RiskConfigurationType
import Amazonka.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
import Amazonka.CognitoIdentityProvider.Types.SMSMfaSettingsType
import Amazonka.CognitoIdentityProvider.Types.SchemaAttributeType
import Amazonka.CognitoIdentityProvider.Types.SmsConfigurationType
import Amazonka.CognitoIdentityProvider.Types.SmsMfaConfigType
import Amazonka.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType
import Amazonka.CognitoIdentityProvider.Types.SoftwareTokenMfaSettingsType
import Amazonka.CognitoIdentityProvider.Types.StringAttributeConstraintsType
import Amazonka.CognitoIdentityProvider.Types.TokenValidityUnitsType
import Amazonka.CognitoIdentityProvider.Types.UICustomizationType
import Amazonka.CognitoIdentityProvider.Types.UserAttributeUpdateSettingsType
import Amazonka.CognitoIdentityProvider.Types.UserContextDataType
import Amazonka.CognitoIdentityProvider.Types.UserImportJobType
import Amazonka.CognitoIdentityProvider.Types.UserPoolAddOnsType
import Amazonka.CognitoIdentityProvider.Types.UserPoolClientDescription
import Amazonka.CognitoIdentityProvider.Types.UserPoolClientType
import Amazonka.CognitoIdentityProvider.Types.UserPoolDescriptionType
import Amazonka.CognitoIdentityProvider.Types.UserPoolPolicyType
import Amazonka.CognitoIdentityProvider.Types.UserPoolType
import Amazonka.CognitoIdentityProvider.Types.UserType
import Amazonka.CognitoIdentityProvider.Types.UsernameConfigurationType
import Amazonka.CognitoIdentityProvider.Types.VerificationMessageTemplateType
import Amazonka.CognitoIdentityProvider.UntagResource
import Amazonka.CognitoIdentityProvider.UpdateAuthEventFeedback
import Amazonka.CognitoIdentityProvider.UpdateDeviceStatus
import Amazonka.CognitoIdentityProvider.UpdateGroup
import Amazonka.CognitoIdentityProvider.UpdateIdentityProvider
import Amazonka.CognitoIdentityProvider.UpdateResourceServer
import Amazonka.CognitoIdentityProvider.UpdateUserAttributes
import Amazonka.CognitoIdentityProvider.UpdateUserPool
import Amazonka.CognitoIdentityProvider.UpdateUserPoolClient
import Amazonka.CognitoIdentityProvider.UpdateUserPoolDomain
import Amazonka.CognitoIdentityProvider.VerifySoftwareToken
import Amazonka.CognitoIdentityProvider.VerifyUserAttribute
