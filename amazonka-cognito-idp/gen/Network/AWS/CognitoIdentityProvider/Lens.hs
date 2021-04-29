{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Lens
  ( -- * Operations

    -- ** GetUserAttributeVerificationCode
    getUserAttributeVerificationCode_clientMetadata,
    getUserAttributeVerificationCode_accessToken,
    getUserAttributeVerificationCode_attributeName,
    getUserAttributeVerificationCodeResponse_codeDeliveryDetails,
    getUserAttributeVerificationCodeResponse_httpStatus,

    -- ** AdminDeleteUser
    adminDeleteUser_userPoolId,
    adminDeleteUser_username,

    -- ** CreateUserImportJob
    createUserImportJob_jobName,
    createUserImportJob_userPoolId,
    createUserImportJob_cloudWatchLogsRoleArn,
    createUserImportJobResponse_userImportJob,
    createUserImportJobResponse_httpStatus,

    -- ** GetUser
    getUser_accessToken,
    getUserResponse_preferredMfaSetting,
    getUserResponse_userMFASettingList,
    getUserResponse_mfaOptions,
    getUserResponse_httpStatus,
    getUserResponse_username,
    getUserResponse_userAttributes,

    -- ** SetUserPoolMfaConfig
    setUserPoolMfaConfig_softwareTokenMfaConfiguration,
    setUserPoolMfaConfig_smsMfaConfiguration,
    setUserPoolMfaConfig_mfaConfiguration,
    setUserPoolMfaConfig_userPoolId,
    setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
    setUserPoolMfaConfigResponse_smsMfaConfiguration,
    setUserPoolMfaConfigResponse_mfaConfiguration,
    setUserPoolMfaConfigResponse_httpStatus,

    -- ** UpdateUserAttributes
    updateUserAttributes_clientMetadata,
    updateUserAttributes_userAttributes,
    updateUserAttributes_accessToken,
    updateUserAttributesResponse_codeDeliveryDetailsList,
    updateUserAttributesResponse_httpStatus,

    -- ** DeleteUserAttributes
    deleteUserAttributes_userAttributeNames,
    deleteUserAttributes_accessToken,
    deleteUserAttributesResponse_httpStatus,

    -- ** UpdateAuthEventFeedback
    updateAuthEventFeedback_userPoolId,
    updateAuthEventFeedback_username,
    updateAuthEventFeedback_eventId,
    updateAuthEventFeedback_feedbackToken,
    updateAuthEventFeedback_feedbackValue,
    updateAuthEventFeedbackResponse_httpStatus,

    -- ** ListUserPools
    listUserPools_nextToken,
    listUserPools_maxResults,
    listUserPoolsResponse_nextToken,
    listUserPoolsResponse_userPools,
    listUserPoolsResponse_httpStatus,

    -- ** ConfirmSignUp
    confirmSignUp_clientMetadata,
    confirmSignUp_userContextData,
    confirmSignUp_forceAliasCreation,
    confirmSignUp_secretHash,
    confirmSignUp_analyticsMetadata,
    confirmSignUp_clientId,
    confirmSignUp_username,
    confirmSignUp_confirmationCode,
    confirmSignUpResponse_httpStatus,

    -- ** AdminLinkProviderForUser
    adminLinkProviderForUser_userPoolId,
    adminLinkProviderForUser_destinationUser,
    adminLinkProviderForUser_sourceUser,
    adminLinkProviderForUserResponse_httpStatus,

    -- ** UpdateUserPool
    updateUserPool_userPoolTags,
    updateUserPool_emailVerificationSubject,
    updateUserPool_autoVerifiedAttributes,
    updateUserPool_policies,
    updateUserPool_adminCreateUserConfig,
    updateUserPool_deviceConfiguration,
    updateUserPool_smsConfiguration,
    updateUserPool_lambdaConfig,
    updateUserPool_smsVerificationMessage,
    updateUserPool_accountRecoverySetting,
    updateUserPool_emailConfiguration,
    updateUserPool_emailVerificationMessage,
    updateUserPool_userPoolAddOns,
    updateUserPool_smsAuthenticationMessage,
    updateUserPool_mfaConfiguration,
    updateUserPool_verificationMessageTemplate,
    updateUserPool_userPoolId,
    updateUserPoolResponse_httpStatus,

    -- ** DeleteUserPool
    deleteUserPool_userPoolId,

    -- ** UpdateUserPoolDomain
    updateUserPoolDomain_domain,
    updateUserPoolDomain_userPoolId,
    updateUserPoolDomain_customDomainConfig,
    updateUserPoolDomainResponse_cloudFrontDomain,
    updateUserPoolDomainResponse_httpStatus,

    -- ** DeleteUserPoolDomain
    deleteUserPoolDomain_domain,
    deleteUserPoolDomain_userPoolId,
    deleteUserPoolDomainResponse_httpStatus,

    -- ** CreateUserPoolDomain
    createUserPoolDomain_customDomainConfig,
    createUserPoolDomain_domain,
    createUserPoolDomain_userPoolId,
    createUserPoolDomainResponse_cloudFrontDomain,
    createUserPoolDomainResponse_httpStatus,

    -- ** ChangePassword
    changePassword_previousPassword,
    changePassword_proposedPassword,
    changePassword_accessToken,
    changePasswordResponse_httpStatus,

    -- ** RespondToAuthChallenge
    respondToAuthChallenge_clientMetadata,
    respondToAuthChallenge_userContextData,
    respondToAuthChallenge_challengeResponses,
    respondToAuthChallenge_session,
    respondToAuthChallenge_analyticsMetadata,
    respondToAuthChallenge_clientId,
    respondToAuthChallenge_challengeName,
    respondToAuthChallengeResponse_authenticationResult,
    respondToAuthChallengeResponse_challengeName,
    respondToAuthChallengeResponse_challengeParameters,
    respondToAuthChallengeResponse_session,
    respondToAuthChallengeResponse_httpStatus,

    -- ** ListDevices
    listDevices_paginationToken,
    listDevices_limit,
    listDevices_accessToken,
    listDevicesResponse_devices,
    listDevicesResponse_paginationToken,
    listDevicesResponse_httpStatus,

    -- ** AdminGetDevice
    adminGetDevice_deviceKey,
    adminGetDevice_userPoolId,
    adminGetDevice_username,
    adminGetDeviceResponse_httpStatus,
    adminGetDeviceResponse_device,

    -- ** CreateUserPool
    createUserPool_userPoolTags,
    createUserPool_schema,
    createUserPool_usernameAttributes,
    createUserPool_emailVerificationSubject,
    createUserPool_autoVerifiedAttributes,
    createUserPool_policies,
    createUserPool_adminCreateUserConfig,
    createUserPool_deviceConfiguration,
    createUserPool_smsConfiguration,
    createUserPool_lambdaConfig,
    createUserPool_smsVerificationMessage,
    createUserPool_accountRecoverySetting,
    createUserPool_emailConfiguration,
    createUserPool_aliasAttributes,
    createUserPool_emailVerificationMessage,
    createUserPool_userPoolAddOns,
    createUserPool_usernameConfiguration,
    createUserPool_smsAuthenticationMessage,
    createUserPool_mfaConfiguration,
    createUserPool_verificationMessageTemplate,
    createUserPool_poolName,
    createUserPoolResponse_userPool,
    createUserPoolResponse_httpStatus,

    -- ** AdminRemoveUserFromGroup
    adminRemoveUserFromGroup_userPoolId,
    adminRemoveUserFromGroup_username,
    adminRemoveUserFromGroup_groupName,

    -- ** SetRiskConfiguration
    setRiskConfiguration_accountTakeoverRiskConfiguration,
    setRiskConfiguration_clientId,
    setRiskConfiguration_riskExceptionConfiguration,
    setRiskConfiguration_compromisedCredentialsRiskConfiguration,
    setRiskConfiguration_userPoolId,
    setRiskConfigurationResponse_httpStatus,
    setRiskConfigurationResponse_riskConfiguration,

    -- ** ListGroups
    listGroups_nextToken,
    listGroups_limit,
    listGroups_userPoolId,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,

    -- ** UpdateIdentityProvider
    updateIdentityProvider_providerDetails,
    updateIdentityProvider_idpIdentifiers,
    updateIdentityProvider_attributeMapping,
    updateIdentityProvider_userPoolId,
    updateIdentityProvider_providerName,
    updateIdentityProviderResponse_httpStatus,
    updateIdentityProviderResponse_identityProvider,

    -- ** ListIdentityProviders
    listIdentityProviders_nextToken,
    listIdentityProviders_maxResults,
    listIdentityProviders_userPoolId,
    listIdentityProvidersResponse_nextToken,
    listIdentityProvidersResponse_httpStatus,
    listIdentityProvidersResponse_providers,

    -- ** GetDevice
    getDevice_accessToken,
    getDevice_deviceKey,
    getDeviceResponse_httpStatus,
    getDeviceResponse_device,

    -- ** DeleteIdentityProvider
    deleteIdentityProvider_userPoolId,
    deleteIdentityProvider_providerName,

    -- ** AdminListDevices
    adminListDevices_paginationToken,
    adminListDevices_limit,
    adminListDevices_userPoolId,
    adminListDevices_username,
    adminListDevicesResponse_devices,
    adminListDevicesResponse_paginationToken,
    adminListDevicesResponse_httpStatus,

    -- ** AdminConfirmSignUp
    adminConfirmSignUp_clientMetadata,
    adminConfirmSignUp_userPoolId,
    adminConfirmSignUp_username,
    adminConfirmSignUpResponse_httpStatus,

    -- ** SetUICustomization
    setUICustomization_clientId,
    setUICustomization_imageFile,
    setUICustomization_css,
    setUICustomization_userPoolId,
    setUICustomizationResponse_httpStatus,
    setUICustomizationResponse_uICustomization,

    -- ** AdminListUserAuthEvents
    adminListUserAuthEvents_nextToken,
    adminListUserAuthEvents_maxResults,
    adminListUserAuthEvents_userPoolId,
    adminListUserAuthEvents_username,
    adminListUserAuthEventsResponse_nextToken,
    adminListUserAuthEventsResponse_authEvents,
    adminListUserAuthEventsResponse_httpStatus,

    -- ** AdminAddUserToGroup
    adminAddUserToGroup_userPoolId,
    adminAddUserToGroup_username,
    adminAddUserToGroup_groupName,

    -- ** VerifySoftwareToken
    verifySoftwareToken_friendlyDeviceName,
    verifySoftwareToken_accessToken,
    verifySoftwareToken_session,
    verifySoftwareToken_userCode,
    verifySoftwareTokenResponse_status,
    verifySoftwareTokenResponse_session,
    verifySoftwareTokenResponse_httpStatus,

    -- ** StopUserImportJob
    stopUserImportJob_userPoolId,
    stopUserImportJob_jobId,
    stopUserImportJobResponse_userImportJob,
    stopUserImportJobResponse_httpStatus,

    -- ** CreateIdentityProvider
    createIdentityProvider_idpIdentifiers,
    createIdentityProvider_attributeMapping,
    createIdentityProvider_userPoolId,
    createIdentityProvider_providerName,
    createIdentityProvider_providerType,
    createIdentityProvider_providerDetails,
    createIdentityProviderResponse_httpStatus,
    createIdentityProviderResponse_identityProvider,

    -- ** InitiateAuth
    initiateAuth_clientMetadata,
    initiateAuth_userContextData,
    initiateAuth_analyticsMetadata,
    initiateAuth_authParameters,
    initiateAuth_authFlow,
    initiateAuth_clientId,
    initiateAuthResponse_authenticationResult,
    initiateAuthResponse_challengeName,
    initiateAuthResponse_challengeParameters,
    initiateAuthResponse_session,
    initiateAuthResponse_httpStatus,

    -- ** AdminSetUserPassword
    adminSetUserPassword_permanent,
    adminSetUserPassword_userPoolId,
    adminSetUserPassword_username,
    adminSetUserPassword_password,
    adminSetUserPasswordResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** AdminListGroupsForUser
    adminListGroupsForUser_nextToken,
    adminListGroupsForUser_limit,
    adminListGroupsForUser_username,
    adminListGroupsForUser_userPoolId,
    adminListGroupsForUserResponse_groups,
    adminListGroupsForUserResponse_nextToken,
    adminListGroupsForUserResponse_httpStatus,

    -- ** AdminUpdateAuthEventFeedback
    adminUpdateAuthEventFeedback_userPoolId,
    adminUpdateAuthEventFeedback_username,
    adminUpdateAuthEventFeedback_eventId,
    adminUpdateAuthEventFeedback_feedbackValue,
    adminUpdateAuthEventFeedbackResponse_httpStatus,

    -- ** CreateGroup
    createGroup_roleArn,
    createGroup_description,
    createGroup_precedence,
    createGroup_groupName,
    createGroup_userPoolId,
    createGroupResponse_group,
    createGroupResponse_httpStatus,

    -- ** StartUserImportJob
    startUserImportJob_userPoolId,
    startUserImportJob_jobId,
    startUserImportJobResponse_userImportJob,
    startUserImportJobResponse_httpStatus,

    -- ** DescribeUserPoolDomain
    describeUserPoolDomain_domain,
    describeUserPoolDomainResponse_domainDescription,
    describeUserPoolDomainResponse_httpStatus,

    -- ** ListUsersInGroup
    listUsersInGroup_nextToken,
    listUsersInGroup_limit,
    listUsersInGroup_userPoolId,
    listUsersInGroup_groupName,
    listUsersInGroupResponse_nextToken,
    listUsersInGroupResponse_users,
    listUsersInGroupResponse_httpStatus,

    -- ** AdminUserGlobalSignOut
    adminUserGlobalSignOut_userPoolId,
    adminUserGlobalSignOut_username,
    adminUserGlobalSignOutResponse_httpStatus,

    -- ** DescribeUserPool
    describeUserPool_userPoolId,
    describeUserPoolResponse_userPool,
    describeUserPoolResponse_httpStatus,

    -- ** AdminGetUser
    adminGetUser_userPoolId,
    adminGetUser_username,
    adminGetUserResponse_preferredMfaSetting,
    adminGetUserResponse_userCreateDate,
    adminGetUserResponse_userLastModifiedDate,
    adminGetUserResponse_enabled,
    adminGetUserResponse_userMFASettingList,
    adminGetUserResponse_userAttributes,
    adminGetUserResponse_userStatus,
    adminGetUserResponse_mfaOptions,
    adminGetUserResponse_httpStatus,
    adminGetUserResponse_username,

    -- ** GetSigningCertificate
    getSigningCertificate_userPoolId,
    getSigningCertificateResponse_certificate,
    getSigningCertificateResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** AssociateSoftwareToken
    associateSoftwareToken_accessToken,
    associateSoftwareToken_session,
    associateSoftwareTokenResponse_secretCode,
    associateSoftwareTokenResponse_session,
    associateSoftwareTokenResponse_httpStatus,

    -- ** ForgotPassword
    forgotPassword_clientMetadata,
    forgotPassword_userContextData,
    forgotPassword_secretHash,
    forgotPassword_analyticsMetadata,
    forgotPassword_clientId,
    forgotPassword_username,
    forgotPasswordResponse_codeDeliveryDetails,
    forgotPasswordResponse_httpStatus,

    -- ** UpdateDeviceStatus
    updateDeviceStatus_deviceRememberedStatus,
    updateDeviceStatus_accessToken,
    updateDeviceStatus_deviceKey,
    updateDeviceStatusResponse_httpStatus,

    -- ** DeleteUserPoolClient
    deleteUserPoolClient_userPoolId,
    deleteUserPoolClient_clientId,

    -- ** UpdateUserPoolClient
    updateUserPoolClient_refreshTokenValidity,
    updateUserPoolClient_idTokenValidity,
    updateUserPoolClient_allowedOAuthScopes,
    updateUserPoolClient_clientName,
    updateUserPoolClient_analyticsConfiguration,
    updateUserPoolClient_readAttributes,
    updateUserPoolClient_logoutURLs,
    updateUserPoolClient_writeAttributes,
    updateUserPoolClient_supportedIdentityProviders,
    updateUserPoolClient_explicitAuthFlows,
    updateUserPoolClient_defaultRedirectURI,
    updateUserPoolClient_tokenValidityUnits,
    updateUserPoolClient_callbackURLs,
    updateUserPoolClient_allowedOAuthFlows,
    updateUserPoolClient_accessTokenValidity,
    updateUserPoolClient_preventUserExistenceErrors,
    updateUserPoolClient_allowedOAuthFlowsUserPoolClient,
    updateUserPoolClient_userPoolId,
    updateUserPoolClient_clientId,
    updateUserPoolClientResponse_userPoolClient,
    updateUserPoolClientResponse_httpStatus,

    -- ** ForgetDevice
    forgetDevice_accessToken,
    forgetDevice_deviceKey,

    -- ** AdminDisableUser
    adminDisableUser_userPoolId,
    adminDisableUser_username,
    adminDisableUserResponse_httpStatus,

    -- ** AdminCreateUser
    adminCreateUser_clientMetadata,
    adminCreateUser_messageAction,
    adminCreateUser_forceAliasCreation,
    adminCreateUser_desiredDeliveryMediums,
    adminCreateUser_temporaryPassword,
    adminCreateUser_userAttributes,
    adminCreateUser_validationData,
    adminCreateUser_userPoolId,
    adminCreateUser_username,
    adminCreateUserResponse_user,
    adminCreateUserResponse_httpStatus,

    -- ** AdminUpdateDeviceStatus
    adminUpdateDeviceStatus_deviceRememberedStatus,
    adminUpdateDeviceStatus_userPoolId,
    adminUpdateDeviceStatus_username,
    adminUpdateDeviceStatus_deviceKey,
    adminUpdateDeviceStatusResponse_httpStatus,

    -- ** AdminForgetDevice
    adminForgetDevice_userPoolId,
    adminForgetDevice_username,
    adminForgetDevice_deviceKey,

    -- ** DescribeIdentityProvider
    describeIdentityProvider_userPoolId,
    describeIdentityProvider_providerName,
    describeIdentityProviderResponse_httpStatus,
    describeIdentityProviderResponse_identityProvider,

    -- ** ListUserImportJobs
    listUserImportJobs_paginationToken,
    listUserImportJobs_userPoolId,
    listUserImportJobs_maxResults,
    listUserImportJobsResponse_userImportJobs,
    listUserImportJobsResponse_paginationToken,
    listUserImportJobsResponse_httpStatus,

    -- ** GetUICustomization
    getUICustomization_clientId,
    getUICustomization_userPoolId,
    getUICustomizationResponse_httpStatus,
    getUICustomizationResponse_uICustomization,

    -- ** GetCSVHeader
    getCSVHeader_userPoolId,
    getCSVHeaderResponse_userPoolId,
    getCSVHeaderResponse_cSVHeader,
    getCSVHeaderResponse_httpStatus,

    -- ** AdminEnableUser
    adminEnableUser_userPoolId,
    adminEnableUser_username,
    adminEnableUserResponse_httpStatus,

    -- ** AdminInitiateAuth
    adminInitiateAuth_clientMetadata,
    adminInitiateAuth_contextData,
    adminInitiateAuth_analyticsMetadata,
    adminInitiateAuth_authParameters,
    adminInitiateAuth_userPoolId,
    adminInitiateAuth_clientId,
    adminInitiateAuth_authFlow,
    adminInitiateAuthResponse_authenticationResult,
    adminInitiateAuthResponse_challengeName,
    adminInitiateAuthResponse_challengeParameters,
    adminInitiateAuthResponse_session,
    adminInitiateAuthResponse_httpStatus,

    -- ** AdminResetUserPassword
    adminResetUserPassword_clientMetadata,
    adminResetUserPassword_userPoolId,
    adminResetUserPassword_username,
    adminResetUserPasswordResponse_httpStatus,

    -- ** GetIdentityProviderByIdentifier
    getIdentityProviderByIdentifier_userPoolId,
    getIdentityProviderByIdentifier_idpIdentifier,
    getIdentityProviderByIdentifierResponse_httpStatus,
    getIdentityProviderByIdentifierResponse_identityProvider,

    -- ** AdminSetUserSettings
    adminSetUserSettings_userPoolId,
    adminSetUserSettings_username,
    adminSetUserSettings_mfaOptions,
    adminSetUserSettingsResponse_httpStatus,

    -- ** DescribeUserPoolClient
    describeUserPoolClient_userPoolId,
    describeUserPoolClient_clientId,
    describeUserPoolClientResponse_userPoolClient,
    describeUserPoolClientResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupName,
    getGroup_userPoolId,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** ResendConfirmationCode
    resendConfirmationCode_clientMetadata,
    resendConfirmationCode_userContextData,
    resendConfirmationCode_secretHash,
    resendConfirmationCode_analyticsMetadata,
    resendConfirmationCode_clientId,
    resendConfirmationCode_username,
    resendConfirmationCodeResponse_codeDeliveryDetails,
    resendConfirmationCodeResponse_httpStatus,

    -- ** AdminRespondToAuthChallenge
    adminRespondToAuthChallenge_clientMetadata,
    adminRespondToAuthChallenge_challengeResponses,
    adminRespondToAuthChallenge_contextData,
    adminRespondToAuthChallenge_session,
    adminRespondToAuthChallenge_analyticsMetadata,
    adminRespondToAuthChallenge_userPoolId,
    adminRespondToAuthChallenge_clientId,
    adminRespondToAuthChallenge_challengeName,
    adminRespondToAuthChallengeResponse_authenticationResult,
    adminRespondToAuthChallengeResponse_challengeName,
    adminRespondToAuthChallengeResponse_challengeParameters,
    adminRespondToAuthChallengeResponse_session,
    adminRespondToAuthChallengeResponse_httpStatus,

    -- ** SignUp
    signUp_clientMetadata,
    signUp_userContextData,
    signUp_secretHash,
    signUp_userAttributes,
    signUp_validationData,
    signUp_analyticsMetadata,
    signUp_clientId,
    signUp_username,
    signUp_password,
    signUpResponse_codeDeliveryDetails,
    signUpResponse_httpStatus,
    signUpResponse_userConfirmed,
    signUpResponse_userSub,

    -- ** DescribeUserImportJob
    describeUserImportJob_userPoolId,
    describeUserImportJob_jobId,
    describeUserImportJobResponse_userImportJob,
    describeUserImportJobResponse_httpStatus,

    -- ** UpdateGroup
    updateGroup_roleArn,
    updateGroup_description,
    updateGroup_precedence,
    updateGroup_groupName,
    updateGroup_userPoolId,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** UpdateResourceServer
    updateResourceServer_scopes,
    updateResourceServer_userPoolId,
    updateResourceServer_identifier,
    updateResourceServer_name,
    updateResourceServerResponse_httpStatus,
    updateResourceServerResponse_resourceServer,

    -- ** DeleteGroup
    deleteGroup_groupName,
    deleteGroup_userPoolId,

    -- ** SetUserSettings
    setUserSettings_accessToken,
    setUserSettings_mfaOptions,
    setUserSettingsResponse_httpStatus,

    -- ** ListResourceServers
    listResourceServers_nextToken,
    listResourceServers_maxResults,
    listResourceServers_userPoolId,
    listResourceServersResponse_nextToken,
    listResourceServersResponse_httpStatus,
    listResourceServersResponse_resourceServers,

    -- ** DeleteResourceServer
    deleteResourceServer_userPoolId,
    deleteResourceServer_identifier,

    -- ** DescribeRiskConfiguration
    describeRiskConfiguration_clientId,
    describeRiskConfiguration_userPoolId,
    describeRiskConfigurationResponse_httpStatus,
    describeRiskConfigurationResponse_riskConfiguration,

    -- ** GlobalSignOut
    globalSignOut_accessToken,
    globalSignOutResponse_httpStatus,

    -- ** GetUserPoolMfaConfig
    getUserPoolMfaConfig_userPoolId,
    getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
    getUserPoolMfaConfigResponse_smsMfaConfiguration,
    getUserPoolMfaConfigResponse_mfaConfiguration,
    getUserPoolMfaConfigResponse_httpStatus,

    -- ** CreateResourceServer
    createResourceServer_scopes,
    createResourceServer_userPoolId,
    createResourceServer_identifier,
    createResourceServer_name,
    createResourceServerResponse_httpStatus,
    createResourceServerResponse_resourceServer,

    -- ** AdminUpdateUserAttributes
    adminUpdateUserAttributes_clientMetadata,
    adminUpdateUserAttributes_userPoolId,
    adminUpdateUserAttributes_username,
    adminUpdateUserAttributes_userAttributes,
    adminUpdateUserAttributesResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_accessToken,

    -- ** AdminDeleteUserAttributes
    adminDeleteUserAttributes_userPoolId,
    adminDeleteUserAttributes_username,
    adminDeleteUserAttributes_userAttributeNames,
    adminDeleteUserAttributesResponse_httpStatus,

    -- ** ListUsers
    listUsers_paginationToken,
    listUsers_filter,
    listUsers_limit,
    listUsers_attributesToGet,
    listUsers_userPoolId,
    listUsersResponse_paginationToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** AdminDisableProviderForUser
    adminDisableProviderForUser_userPoolId,
    adminDisableProviderForUser_user,
    adminDisableProviderForUserResponse_httpStatus,

    -- ** CreateUserPoolClient
    createUserPoolClient_refreshTokenValidity,
    createUserPoolClient_idTokenValidity,
    createUserPoolClient_allowedOAuthScopes,
    createUserPoolClient_analyticsConfiguration,
    createUserPoolClient_readAttributes,
    createUserPoolClient_logoutURLs,
    createUserPoolClient_writeAttributes,
    createUserPoolClient_supportedIdentityProviders,
    createUserPoolClient_explicitAuthFlows,
    createUserPoolClient_defaultRedirectURI,
    createUserPoolClient_tokenValidityUnits,
    createUserPoolClient_callbackURLs,
    createUserPoolClient_allowedOAuthFlows,
    createUserPoolClient_accessTokenValidity,
    createUserPoolClient_generateSecret,
    createUserPoolClient_preventUserExistenceErrors,
    createUserPoolClient_allowedOAuthFlowsUserPoolClient,
    createUserPoolClient_userPoolId,
    createUserPoolClient_clientName,
    createUserPoolClientResponse_userPoolClient,
    createUserPoolClientResponse_httpStatus,

    -- ** ListUserPoolClients
    listUserPoolClients_nextToken,
    listUserPoolClients_maxResults,
    listUserPoolClients_userPoolId,
    listUserPoolClientsResponse_userPoolClients,
    listUserPoolClientsResponse_nextToken,
    listUserPoolClientsResponse_httpStatus,

    -- ** AddCustomAttributes
    addCustomAttributes_userPoolId,
    addCustomAttributes_customAttributes,
    addCustomAttributesResponse_httpStatus,

    -- ** AdminSetUserMFAPreference
    adminSetUserMFAPreference_softwareTokenMfaSettings,
    adminSetUserMFAPreference_sMSMfaSettings,
    adminSetUserMFAPreference_username,
    adminSetUserMFAPreference_userPoolId,
    adminSetUserMFAPreferenceResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ConfirmForgotPassword
    confirmForgotPassword_clientMetadata,
    confirmForgotPassword_userContextData,
    confirmForgotPassword_secretHash,
    confirmForgotPassword_analyticsMetadata,
    confirmForgotPassword_clientId,
    confirmForgotPassword_username,
    confirmForgotPassword_confirmationCode,
    confirmForgotPassword_password,
    confirmForgotPasswordResponse_httpStatus,

    -- ** SetUserMFAPreference
    setUserMFAPreference_softwareTokenMfaSettings,
    setUserMFAPreference_sMSMfaSettings,
    setUserMFAPreference_accessToken,
    setUserMFAPreferenceResponse_httpStatus,

    -- ** VerifyUserAttribute
    verifyUserAttribute_accessToken,
    verifyUserAttribute_attributeName,
    verifyUserAttribute_code,
    verifyUserAttributeResponse_httpStatus,

    -- ** ConfirmDevice
    confirmDevice_deviceSecretVerifierConfig,
    confirmDevice_deviceName,
    confirmDevice_accessToken,
    confirmDevice_deviceKey,
    confirmDeviceResponse_userConfirmationNecessary,
    confirmDeviceResponse_httpStatus,

    -- ** DescribeResourceServer
    describeResourceServer_userPoolId,
    describeResourceServer_identifier,
    describeResourceServerResponse_httpStatus,
    describeResourceServerResponse_resourceServer,

    -- * Types

    -- ** AccountRecoverySettingType
    accountRecoverySettingType_recoveryMechanisms,

    -- ** AccountTakeoverActionType
    accountTakeoverActionType_notify,
    accountTakeoverActionType_eventAction,

    -- ** AccountTakeoverActionsType
    accountTakeoverActionsType_lowAction,
    accountTakeoverActionsType_mediumAction,
    accountTakeoverActionsType_highAction,

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
    analyticsConfigurationType_roleArn,
    analyticsConfigurationType_userDataShared,
    analyticsConfigurationType_externalId,

    -- ** AnalyticsMetadataType
    analyticsMetadataType_analyticsEndpointId,

    -- ** AttributeType
    attributeType_value,
    attributeType_name,

    -- ** AuthEventType
    authEventType_eventType,
    authEventType_eventId,
    authEventType_challengeResponses,
    authEventType_eventContextData,
    authEventType_creationDate,
    authEventType_eventRisk,
    authEventType_eventResponse,
    authEventType_eventFeedback,

    -- ** AuthenticationResultType
    authenticationResultType_expiresIn,
    authenticationResultType_tokenType,
    authenticationResultType_accessToken,
    authenticationResultType_newDeviceMetadata,
    authenticationResultType_idToken,
    authenticationResultType_refreshToken,

    -- ** ChallengeResponseType
    challengeResponseType_challengeResponse,
    challengeResponseType_challengeName,

    -- ** CodeDeliveryDetailsType
    codeDeliveryDetailsType_deliveryMedium,
    codeDeliveryDetailsType_attributeName,
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
    deviceType_deviceLastModifiedDate,
    deviceType_deviceKey,
    deviceType_deviceLastAuthenticatedDate,
    deviceType_deviceCreateDate,
    deviceType_deviceAttributes,

    -- ** DomainDescriptionType
    domainDescriptionType_status,
    domainDescriptionType_customDomainConfig,
    domainDescriptionType_aWSAccountId,
    domainDescriptionType_s3Bucket,
    domainDescriptionType_userPoolId,
    domainDescriptionType_domain,
    domainDescriptionType_cloudFrontDistribution,
    domainDescriptionType_version,

    -- ** EmailConfigurationType
    emailConfigurationType_emailSendingAccount,
    emailConfigurationType_replyToEmailAddress,
    emailConfigurationType_from,
    emailConfigurationType_sourceArn,
    emailConfigurationType_configurationSet,

    -- ** EventContextDataType
    eventContextDataType_ipAddress,
    eventContextDataType_city,
    eventContextDataType_deviceName,
    eventContextDataType_timezone,
    eventContextDataType_country,

    -- ** EventFeedbackType
    eventFeedbackType_feedbackDate,
    eventFeedbackType_feedbackValue,
    eventFeedbackType_provider,

    -- ** EventRiskType
    eventRiskType_compromisedCredentialsDetected,
    eventRiskType_riskDecision,
    eventRiskType_riskLevel,

    -- ** GroupType
    groupType_lastModifiedDate,
    groupType_roleArn,
    groupType_groupName,
    groupType_userPoolId,
    groupType_creationDate,
    groupType_description,
    groupType_precedence,

    -- ** HttpHeader
    httpHeader_headerName,
    httpHeader_headerValue,

    -- ** IdentityProviderType
    identityProviderType_lastModifiedDate,
    identityProviderType_providerType,
    identityProviderType_providerName,
    identityProviderType_providerDetails,
    identityProviderType_userPoolId,
    identityProviderType_creationDate,
    identityProviderType_idpIdentifiers,
    identityProviderType_attributeMapping,

    -- ** LambdaConfigType
    lambdaConfigType_customEmailSender,
    lambdaConfigType_preSignUp,
    lambdaConfigType_defineAuthChallenge,
    lambdaConfigType_postAuthentication,
    lambdaConfigType_customSMSSender,
    lambdaConfigType_createAuthChallenge,
    lambdaConfigType_postConfirmation,
    lambdaConfigType_preAuthentication,
    lambdaConfigType_kmsKeyID,
    lambdaConfigType_verifyAuthChallengeResponse,
    lambdaConfigType_customMessage,
    lambdaConfigType_userMigration,
    lambdaConfigType_preTokenGeneration,

    -- ** MFAOptionType
    mfaOptionType_deliveryMedium,
    mfaOptionType_attributeName,

    -- ** MessageTemplateType
    messageTemplateType_emailSubject,
    messageTemplateType_emailMessage,
    messageTemplateType_sMSMessage,

    -- ** NewDeviceMetadataType
    newDeviceMetadataType_deviceKey,
    newDeviceMetadataType_deviceGroupKey,

    -- ** NotifyConfigurationType
    notifyConfigurationType_mfaEmail,
    notifyConfigurationType_blockEmail,
    notifyConfigurationType_replyTo,
    notifyConfigurationType_from,
    notifyConfigurationType_noActionEmail,
    notifyConfigurationType_sourceArn,

    -- ** NotifyEmailType
    notifyEmailType_htmlBody,
    notifyEmailType_textBody,
    notifyEmailType_subject,

    -- ** NumberAttributeConstraintsType
    numberAttributeConstraintsType_maxValue,
    numberAttributeConstraintsType_minValue,

    -- ** PasswordPolicyType
    passwordPolicyType_temporaryPasswordValidityDays,
    passwordPolicyType_requireUppercase,
    passwordPolicyType_minimumLength,
    passwordPolicyType_requireSymbols,
    passwordPolicyType_requireNumbers,
    passwordPolicyType_requireLowercase,

    -- ** ProviderDescription
    providerDescription_lastModifiedDate,
    providerDescription_providerType,
    providerDescription_providerName,
    providerDescription_creationDate,

    -- ** ProviderUserIdentifierType
    providerUserIdentifierType_providerName,
    providerUserIdentifierType_providerAttributeName,
    providerUserIdentifierType_providerAttributeValue,

    -- ** RecoveryOptionType
    recoveryOptionType_priority,
    recoveryOptionType_name,

    -- ** ResourceServerScopeType
    resourceServerScopeType_scopeName,
    resourceServerScopeType_scopeDescription,

    -- ** ResourceServerType
    resourceServerType_scopes,
    resourceServerType_identifier,
    resourceServerType_userPoolId,
    resourceServerType_name,

    -- ** RiskConfigurationType
    riskConfigurationType_accountTakeoverRiskConfiguration,
    riskConfigurationType_lastModifiedDate,
    riskConfigurationType_clientId,
    riskConfigurationType_riskExceptionConfiguration,
    riskConfigurationType_userPoolId,
    riskConfigurationType_compromisedCredentialsRiskConfiguration,

    -- ** RiskExceptionConfigurationType
    riskExceptionConfigurationType_skippedIPRangeList,
    riskExceptionConfigurationType_blockedIPRangeList,

    -- ** SMSMfaSettingsType
    sMSMfaSettingsType_enabled,
    sMSMfaSettingsType_preferredMfa,

    -- ** SchemaAttributeType
    schemaAttributeType_attributeDataType,
    schemaAttributeType_required,
    schemaAttributeType_numberAttributeConstraints,
    schemaAttributeType_developerOnlyAttribute,
    schemaAttributeType_stringAttributeConstraints,
    schemaAttributeType_name,
    schemaAttributeType_mutable,

    -- ** SmsConfigurationType
    smsConfigurationType_externalId,
    smsConfigurationType_snsCallerArn,

    -- ** SmsMfaConfigType
    smsMfaConfigType_smsConfiguration,
    smsMfaConfigType_smsAuthenticationMessage,

    -- ** SoftwareTokenMfaConfigType
    softwareTokenMfaConfigType_enabled,

    -- ** SoftwareTokenMfaSettingsType
    softwareTokenMfaSettingsType_enabled,
    softwareTokenMfaSettingsType_preferredMfa,

    -- ** StringAttributeConstraintsType
    stringAttributeConstraintsType_minLength,
    stringAttributeConstraintsType_maxLength,

    -- ** TokenValidityUnitsType
    tokenValidityUnitsType_accessToken,
    tokenValidityUnitsType_idToken,
    tokenValidityUnitsType_refreshToken,

    -- ** UICustomizationType
    uICustomizationType_lastModifiedDate,
    uICustomizationType_clientId,
    uICustomizationType_cSSVersion,
    uICustomizationType_userPoolId,
    uICustomizationType_creationDate,
    uICustomizationType_imageUrl,
    uICustomizationType_css,

    -- ** UserContextDataType
    userContextDataType_encodedData,

    -- ** UserImportJobType
    userImportJobType_completionMessage,
    userImportJobType_status,
    userImportJobType_startDate,
    userImportJobType_userPoolId,
    userImportJobType_creationDate,
    userImportJobType_skippedUsers,
    userImportJobType_failedUsers,
    userImportJobType_importedUsers,
    userImportJobType_cloudWatchLogsRoleArn,
    userImportJobType_preSignedUrl,
    userImportJobType_completionDate,
    userImportJobType_jobName,
    userImportJobType_jobId,

    -- ** UserPoolAddOnsType
    userPoolAddOnsType_advancedSecurityMode,

    -- ** UserPoolClientDescription
    userPoolClientDescription_clientId,
    userPoolClientDescription_clientName,
    userPoolClientDescription_userPoolId,

    -- ** UserPoolClientType
    userPoolClientType_lastModifiedDate,
    userPoolClientType_clientSecret,
    userPoolClientType_refreshTokenValidity,
    userPoolClientType_clientId,
    userPoolClientType_idTokenValidity,
    userPoolClientType_allowedOAuthScopes,
    userPoolClientType_clientName,
    userPoolClientType_analyticsConfiguration,
    userPoolClientType_userPoolId,
    userPoolClientType_readAttributes,
    userPoolClientType_creationDate,
    userPoolClientType_logoutURLs,
    userPoolClientType_writeAttributes,
    userPoolClientType_supportedIdentityProviders,
    userPoolClientType_explicitAuthFlows,
    userPoolClientType_defaultRedirectURI,
    userPoolClientType_tokenValidityUnits,
    userPoolClientType_callbackURLs,
    userPoolClientType_allowedOAuthFlows,
    userPoolClientType_accessTokenValidity,
    userPoolClientType_preventUserExistenceErrors,
    userPoolClientType_allowedOAuthFlowsUserPoolClient,

    -- ** UserPoolDescriptionType
    userPoolDescriptionType_lastModifiedDate,
    userPoolDescriptionType_status,
    userPoolDescriptionType_id,
    userPoolDescriptionType_creationDate,
    userPoolDescriptionType_lambdaConfig,
    userPoolDescriptionType_name,

    -- ** UserPoolPolicyType
    userPoolPolicyType_passwordPolicy,

    -- ** UserPoolType
    userPoolType_lastModifiedDate,
    userPoolType_userPoolTags,
    userPoolType_status,
    userPoolType_usernameAttributes,
    userPoolType_emailVerificationSubject,
    userPoolType_autoVerifiedAttributes,
    userPoolType_policies,
    userPoolType_customDomain,
    userPoolType_domain,
    userPoolType_adminCreateUserConfig,
    userPoolType_deviceConfiguration,
    userPoolType_arn,
    userPoolType_id,
    userPoolType_creationDate,
    userPoolType_smsConfiguration,
    userPoolType_lambdaConfig,
    userPoolType_estimatedNumberOfUsers,
    userPoolType_smsVerificationMessage,
    userPoolType_name,
    userPoolType_accountRecoverySetting,
    userPoolType_emailConfiguration,
    userPoolType_emailConfigurationFailure,
    userPoolType_aliasAttributes,
    userPoolType_emailVerificationMessage,
    userPoolType_userPoolAddOns,
    userPoolType_usernameConfiguration,
    userPoolType_smsAuthenticationMessage,
    userPoolType_schemaAttributes,
    userPoolType_smsConfigurationFailure,
    userPoolType_mfaConfiguration,
    userPoolType_verificationMessageTemplate,

    -- ** UserType
    userType_userCreateDate,
    userType_userLastModifiedDate,
    userType_enabled,
    userType_attributes,
    userType_username,
    userType_userStatus,
    userType_mfaOptions,

    -- ** UsernameConfigurationType
    usernameConfigurationType_caseSensitive,

    -- ** VerificationMessageTemplateType
    verificationMessageTemplateType_defaultEmailOption,
    verificationMessageTemplateType_emailSubjectByLink,
    verificationMessageTemplateType_emailSubject,
    verificationMessageTemplateType_emailMessage,
    verificationMessageTemplateType_emailMessageByLink,
    verificationMessageTemplateType_smsMessage,
  )
where

import Network.AWS.CognitoIdentityProvider.AddCustomAttributes
import Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
import Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
import Network.AWS.CognitoIdentityProvider.AdminCreateUser
import Network.AWS.CognitoIdentityProvider.AdminDeleteUser
import Network.AWS.CognitoIdentityProvider.AdminDeleteUserAttributes
import Network.AWS.CognitoIdentityProvider.AdminDisableProviderForUser
import Network.AWS.CognitoIdentityProvider.AdminDisableUser
import Network.AWS.CognitoIdentityProvider.AdminEnableUser
import Network.AWS.CognitoIdentityProvider.AdminForgetDevice
import Network.AWS.CognitoIdentityProvider.AdminGetDevice
import Network.AWS.CognitoIdentityProvider.AdminGetUser
import Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
import Network.AWS.CognitoIdentityProvider.AdminLinkProviderForUser
import Network.AWS.CognitoIdentityProvider.AdminListDevices
import Network.AWS.CognitoIdentityProvider.AdminListGroupsForUser
import Network.AWS.CognitoIdentityProvider.AdminListUserAuthEvents
import Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
import Network.AWS.CognitoIdentityProvider.AdminResetUserPassword
import Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
import Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
import Network.AWS.CognitoIdentityProvider.AdminSetUserPassword
import Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
import Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
import Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
import Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
import Network.AWS.CognitoIdentityProvider.AdminUserGlobalSignOut
import Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
import Network.AWS.CognitoIdentityProvider.ChangePassword
import Network.AWS.CognitoIdentityProvider.ConfirmDevice
import Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
import Network.AWS.CognitoIdentityProvider.ConfirmSignUp
import Network.AWS.CognitoIdentityProvider.CreateGroup
import Network.AWS.CognitoIdentityProvider.CreateIdentityProvider
import Network.AWS.CognitoIdentityProvider.CreateResourceServer
import Network.AWS.CognitoIdentityProvider.CreateUserImportJob
import Network.AWS.CognitoIdentityProvider.CreateUserPool
import Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
import Network.AWS.CognitoIdentityProvider.CreateUserPoolDomain
import Network.AWS.CognitoIdentityProvider.DeleteGroup
import Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
import Network.AWS.CognitoIdentityProvider.DeleteResourceServer
import Network.AWS.CognitoIdentityProvider.DeleteUser
import Network.AWS.CognitoIdentityProvider.DeleteUserAttributes
import Network.AWS.CognitoIdentityProvider.DeleteUserPool
import Network.AWS.CognitoIdentityProvider.DeleteUserPoolClient
import Network.AWS.CognitoIdentityProvider.DeleteUserPoolDomain
import Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
import Network.AWS.CognitoIdentityProvider.DescribeResourceServer
import Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
import Network.AWS.CognitoIdentityProvider.DescribeUserImportJob
import Network.AWS.CognitoIdentityProvider.DescribeUserPool
import Network.AWS.CognitoIdentityProvider.DescribeUserPoolClient
import Network.AWS.CognitoIdentityProvider.DescribeUserPoolDomain
import Network.AWS.CognitoIdentityProvider.ForgetDevice
import Network.AWS.CognitoIdentityProvider.ForgotPassword
import Network.AWS.CognitoIdentityProvider.GetCSVHeader
import Network.AWS.CognitoIdentityProvider.GetDevice
import Network.AWS.CognitoIdentityProvider.GetGroup
import Network.AWS.CognitoIdentityProvider.GetIdentityProviderByIdentifier
import Network.AWS.CognitoIdentityProvider.GetSigningCertificate
import Network.AWS.CognitoIdentityProvider.GetUICustomization
import Network.AWS.CognitoIdentityProvider.GetUser
import Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
import Network.AWS.CognitoIdentityProvider.GetUserPoolMfaConfig
import Network.AWS.CognitoIdentityProvider.GlobalSignOut
import Network.AWS.CognitoIdentityProvider.InitiateAuth
import Network.AWS.CognitoIdentityProvider.ListDevices
import Network.AWS.CognitoIdentityProvider.ListGroups
import Network.AWS.CognitoIdentityProvider.ListIdentityProviders
import Network.AWS.CognitoIdentityProvider.ListResourceServers
import Network.AWS.CognitoIdentityProvider.ListTagsForResource
import Network.AWS.CognitoIdentityProvider.ListUserImportJobs
import Network.AWS.CognitoIdentityProvider.ListUserPoolClients
import Network.AWS.CognitoIdentityProvider.ListUserPools
import Network.AWS.CognitoIdentityProvider.ListUsers
import Network.AWS.CognitoIdentityProvider.ListUsersInGroup
import Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
import Network.AWS.CognitoIdentityProvider.RespondToAuthChallenge
import Network.AWS.CognitoIdentityProvider.SetRiskConfiguration
import Network.AWS.CognitoIdentityProvider.SetUICustomization
import Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
import Network.AWS.CognitoIdentityProvider.SetUserPoolMfaConfig
import Network.AWS.CognitoIdentityProvider.SetUserSettings
import Network.AWS.CognitoIdentityProvider.SignUp
import Network.AWS.CognitoIdentityProvider.StartUserImportJob
import Network.AWS.CognitoIdentityProvider.StopUserImportJob
import Network.AWS.CognitoIdentityProvider.TagResource
import Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType
import Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType
import Network.AWS.CognitoIdentityProvider.Types.AttributeType
import Network.AWS.CognitoIdentityProvider.Types.AuthEventType
import Network.AWS.CognitoIdentityProvider.Types.AuthenticationResultType
import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType
import Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.ContextDataType
import Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
import Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
import Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
import Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
import Network.AWS.CognitoIdentityProvider.Types.DeviceType
import Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType
import Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
import Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
import Network.AWS.CognitoIdentityProvider.Types.EventRiskType
import Network.AWS.CognitoIdentityProvider.Types.GroupType
import Network.AWS.CognitoIdentityProvider.Types.HttpHeader
import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType
import Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
import Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
import Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
import Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
import Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
import Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
import Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
import Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
import Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
import Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.SMSMfaSettingsType
import Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
import Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.SmsMfaConfigType
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaSettingsType
import Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
import Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType
import Network.AWS.CognitoIdentityProvider.Types.UICustomizationType
import Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
import Network.AWS.CognitoIdentityProvider.Types.UserImportJobType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
import Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolDescriptionType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolType
import Network.AWS.CognitoIdentityProvider.Types.UserType
import Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
import Network.AWS.CognitoIdentityProvider.UntagResource
import Network.AWS.CognitoIdentityProvider.UpdateAuthEventFeedback
import Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
import Network.AWS.CognitoIdentityProvider.UpdateGroup
import Network.AWS.CognitoIdentityProvider.UpdateIdentityProvider
import Network.AWS.CognitoIdentityProvider.UpdateResourceServer
import Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
import Network.AWS.CognitoIdentityProvider.UpdateUserPool
import Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
import Network.AWS.CognitoIdentityProvider.UpdateUserPoolDomain
import Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
import Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
