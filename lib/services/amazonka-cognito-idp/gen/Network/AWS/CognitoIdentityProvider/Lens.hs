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

    -- ** DeleteUserPool
    deleteUserPool_userPoolId,

    -- ** UpdateUserPool
    updateUserPool_userPoolTags,
    updateUserPool_verificationMessageTemplate,
    updateUserPool_emailVerificationMessage,
    updateUserPool_smsAuthenticationMessage,
    updateUserPool_userPoolAddOns,
    updateUserPool_emailVerificationSubject,
    updateUserPool_accountRecoverySetting,
    updateUserPool_emailConfiguration,
    updateUserPool_smsVerificationMessage,
    updateUserPool_mfaConfiguration,
    updateUserPool_lambdaConfig,
    updateUserPool_smsConfiguration,
    updateUserPool_adminCreateUserConfig,
    updateUserPool_deviceConfiguration,
    updateUserPool_autoVerifiedAttributes,
    updateUserPool_policies,
    updateUserPool_userPoolId,
    updateUserPoolResponse_httpStatus,

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

    -- ** AdminInitiateAuth
    adminInitiateAuth_clientMetadata,
    adminInitiateAuth_contextData,
    adminInitiateAuth_analyticsMetadata,
    adminInitiateAuth_authParameters,
    adminInitiateAuth_userPoolId,
    adminInitiateAuth_clientId,
    adminInitiateAuth_authFlow,
    adminInitiateAuthResponse_challengeName,
    adminInitiateAuthResponse_challengeParameters,
    adminInitiateAuthResponse_authenticationResult,
    adminInitiateAuthResponse_session,
    adminInitiateAuthResponse_httpStatus,

    -- ** AdminLinkProviderForUser
    adminLinkProviderForUser_userPoolId,
    adminLinkProviderForUser_destinationUser,
    adminLinkProviderForUser_sourceUser,
    adminLinkProviderForUserResponse_httpStatus,

    -- ** AdminEnableUser
    adminEnableUser_userPoolId,
    adminEnableUser_username,
    adminEnableUserResponse_httpStatus,

    -- ** GetUserAttributeVerificationCode
    getUserAttributeVerificationCode_clientMetadata,
    getUserAttributeVerificationCode_accessToken,
    getUserAttributeVerificationCode_attributeName,
    getUserAttributeVerificationCodeResponse_codeDeliveryDetails,
    getUserAttributeVerificationCodeResponse_httpStatus,

    -- ** SetUserPoolMfaConfig
    setUserPoolMfaConfig_smsMfaConfiguration,
    setUserPoolMfaConfig_softwareTokenMfaConfiguration,
    setUserPoolMfaConfig_mfaConfiguration,
    setUserPoolMfaConfig_userPoolId,
    setUserPoolMfaConfigResponse_smsMfaConfiguration,
    setUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
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

    -- ** VerifyUserAttribute
    verifyUserAttribute_accessToken,
    verifyUserAttribute_attributeName,
    verifyUserAttribute_code,
    verifyUserAttributeResponse_httpStatus,

    -- ** AdminDisableUser
    adminDisableUser_userPoolId,
    adminDisableUser_username,
    adminDisableUserResponse_httpStatus,

    -- ** ConfirmDevice
    confirmDevice_deviceSecretVerifierConfig,
    confirmDevice_deviceName,
    confirmDevice_accessToken,
    confirmDevice_deviceKey,
    confirmDeviceResponse_userConfirmationNecessary,
    confirmDeviceResponse_httpStatus,

    -- ** ConfirmForgotPassword
    confirmForgotPassword_clientMetadata,
    confirmForgotPassword_analyticsMetadata,
    confirmForgotPassword_userContextData,
    confirmForgotPassword_secretHash,
    confirmForgotPassword_clientId,
    confirmForgotPassword_username,
    confirmForgotPassword_confirmationCode,
    confirmForgotPassword_password,
    confirmForgotPasswordResponse_httpStatus,

    -- ** ListUserImportJobs
    listUserImportJobs_paginationToken,
    listUserImportJobs_userPoolId,
    listUserImportJobs_maxResults,
    listUserImportJobsResponse_paginationToken,
    listUserImportJobsResponse_userImportJobs,
    listUserImportJobsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeIdentityProvider
    describeIdentityProvider_userPoolId,
    describeIdentityProvider_providerName,
    describeIdentityProviderResponse_httpStatus,
    describeIdentityProviderResponse_identityProvider,

    -- ** ListUsers
    listUsers_paginationToken,
    listUsers_attributesToGet,
    listUsers_limit,
    listUsers_filter,
    listUsers_userPoolId,
    listUsersResponse_paginationToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** AdminDeleteUserAttributes
    adminDeleteUserAttributes_userPoolId,
    adminDeleteUserAttributes_username,
    adminDeleteUserAttributes_userAttributeNames,
    adminDeleteUserAttributesResponse_httpStatus,

    -- ** DescribeUserPoolDomain
    describeUserPoolDomain_domain,
    describeUserPoolDomainResponse_domainDescription,
    describeUserPoolDomainResponse_httpStatus,

    -- ** AdminUpdateUserAttributes
    adminUpdateUserAttributes_clientMetadata,
    adminUpdateUserAttributes_userPoolId,
    adminUpdateUserAttributes_username,
    adminUpdateUserAttributes_userAttributes,
    adminUpdateUserAttributesResponse_httpStatus,

    -- ** AdminGetUser
    adminGetUser_userPoolId,
    adminGetUser_username,
    adminGetUserResponse_enabled,
    adminGetUserResponse_userStatus,
    adminGetUserResponse_userAttributes,
    adminGetUserResponse_userCreateDate,
    adminGetUserResponse_userMFASettingList,
    adminGetUserResponse_mfaOptions,
    adminGetUserResponse_userLastModifiedDate,
    adminGetUserResponse_preferredMfaSetting,
    adminGetUserResponse_httpStatus,
    adminGetUserResponse_username,

    -- ** AdminUserGlobalSignOut
    adminUserGlobalSignOut_userPoolId,
    adminUserGlobalSignOut_username,
    adminUserGlobalSignOutResponse_httpStatus,

    -- ** ListUsersInGroup
    listUsersInGroup_nextToken,
    listUsersInGroup_limit,
    listUsersInGroup_userPoolId,
    listUsersInGroup_groupName,
    listUsersInGroupResponse_users,
    listUsersInGroupResponse_nextToken,
    listUsersInGroupResponse_httpStatus,

    -- ** AssociateSoftwareToken
    associateSoftwareToken_accessToken,
    associateSoftwareToken_session,
    associateSoftwareTokenResponse_secretCode,
    associateSoftwareTokenResponse_session,
    associateSoftwareTokenResponse_httpStatus,

    -- ** AdminDisableProviderForUser
    adminDisableProviderForUser_userPoolId,
    adminDisableProviderForUser_user,
    adminDisableProviderForUserResponse_httpStatus,

    -- ** ForgotPassword
    forgotPassword_clientMetadata,
    forgotPassword_analyticsMetadata,
    forgotPassword_userContextData,
    forgotPassword_secretHash,
    forgotPassword_clientId,
    forgotPassword_username,
    forgotPasswordResponse_codeDeliveryDetails,
    forgotPasswordResponse_httpStatus,

    -- ** DescribeUserPool
    describeUserPool_userPoolId,
    describeUserPoolResponse_userPool,
    describeUserPoolResponse_httpStatus,

    -- ** InitiateAuth
    initiateAuth_clientMetadata,
    initiateAuth_analyticsMetadata,
    initiateAuth_userContextData,
    initiateAuth_authParameters,
    initiateAuth_authFlow,
    initiateAuth_clientId,
    initiateAuthResponse_challengeName,
    initiateAuthResponse_challengeParameters,
    initiateAuthResponse_authenticationResult,
    initiateAuthResponse_session,
    initiateAuthResponse_httpStatus,

    -- ** AdminListGroupsForUser
    adminListGroupsForUser_nextToken,
    adminListGroupsForUser_limit,
    adminListGroupsForUser_username,
    adminListGroupsForUser_userPoolId,
    adminListGroupsForUserResponse_groups,
    adminListGroupsForUserResponse_nextToken,
    adminListGroupsForUserResponse_httpStatus,

    -- ** AdminConfirmSignUp
    adminConfirmSignUp_clientMetadata,
    adminConfirmSignUp_userPoolId,
    adminConfirmSignUp_username,
    adminConfirmSignUpResponse_httpStatus,

    -- ** AdminUpdateAuthEventFeedback
    adminUpdateAuthEventFeedback_userPoolId,
    adminUpdateAuthEventFeedback_username,
    adminUpdateAuthEventFeedback_eventId,
    adminUpdateAuthEventFeedback_feedbackValue,
    adminUpdateAuthEventFeedbackResponse_httpStatus,

    -- ** AdminSetUserPassword
    adminSetUserPassword_permanent,
    adminSetUserPassword_userPoolId,
    adminSetUserPassword_username,
    adminSetUserPassword_password,
    adminSetUserPasswordResponse_httpStatus,

    -- ** StartUserImportJob
    startUserImportJob_userPoolId,
    startUserImportJob_jobId,
    startUserImportJobResponse_userImportJob,
    startUserImportJobResponse_httpStatus,

    -- ** CreateIdentityProvider
    createIdentityProvider_idpIdentifiers,
    createIdentityProvider_attributeMapping,
    createIdentityProvider_userPoolId,
    createIdentityProvider_providerName,
    createIdentityProvider_providerType,
    createIdentityProvider_providerDetails,
    createIdentityProviderResponse_httpStatus,
    createIdentityProviderResponse_identityProvider,

    -- ** SetUICustomization
    setUICustomization_clientId,
    setUICustomization_css,
    setUICustomization_imageFile,
    setUICustomization_userPoolId,
    setUICustomizationResponse_httpStatus,
    setUICustomizationResponse_uICustomization,

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

    -- ** SignUp
    signUp_clientMetadata,
    signUp_analyticsMetadata,
    signUp_userContextData,
    signUp_userAttributes,
    signUp_secretHash,
    signUp_validationData,
    signUp_clientId,
    signUp_username,
    signUp_password,
    signUpResponse_codeDeliveryDetails,
    signUpResponse_httpStatus,
    signUpResponse_userConfirmed,
    signUpResponse_userSub,

    -- ** DeleteResourceServer
    deleteResourceServer_userPoolId,
    deleteResourceServer_identifier,

    -- ** UpdateResourceServer
    updateResourceServer_scopes,
    updateResourceServer_userPoolId,
    updateResourceServer_identifier,
    updateResourceServer_name,
    updateResourceServerResponse_httpStatus,
    updateResourceServerResponse_resourceServer,

    -- ** ChangePassword
    changePassword_previousPassword,
    changePassword_proposedPassword,
    changePassword_accessToken,
    changePasswordResponse_httpStatus,

    -- ** CreateUserPoolDomain
    createUserPoolDomain_customDomainConfig,
    createUserPoolDomain_domain,
    createUserPoolDomain_userPoolId,
    createUserPoolDomainResponse_cloudFrontDomain,
    createUserPoolDomainResponse_httpStatus,

    -- ** RespondToAuthChallenge
    respondToAuthChallenge_clientMetadata,
    respondToAuthChallenge_analyticsMetadata,
    respondToAuthChallenge_challengeResponses,
    respondToAuthChallenge_userContextData,
    respondToAuthChallenge_session,
    respondToAuthChallenge_clientId,
    respondToAuthChallenge_challengeName,
    respondToAuthChallengeResponse_challengeName,
    respondToAuthChallengeResponse_challengeParameters,
    respondToAuthChallengeResponse_authenticationResult,
    respondToAuthChallengeResponse_session,
    respondToAuthChallengeResponse_httpStatus,

    -- ** CreateUserPool
    createUserPool_userPoolTags,
    createUserPool_verificationMessageTemplate,
    createUserPool_emailVerificationMessage,
    createUserPool_smsAuthenticationMessage,
    createUserPool_userPoolAddOns,
    createUserPool_emailVerificationSubject,
    createUserPool_usernameAttributes,
    createUserPool_aliasAttributes,
    createUserPool_schema,
    createUserPool_accountRecoverySetting,
    createUserPool_emailConfiguration,
    createUserPool_smsVerificationMessage,
    createUserPool_mfaConfiguration,
    createUserPool_lambdaConfig,
    createUserPool_smsConfiguration,
    createUserPool_adminCreateUserConfig,
    createUserPool_deviceConfiguration,
    createUserPool_autoVerifiedAttributes,
    createUserPool_policies,
    createUserPool_usernameConfiguration,
    createUserPool_poolName,
    createUserPoolResponse_userPool,
    createUserPoolResponse_httpStatus,

    -- ** AdminGetDevice
    adminGetDevice_deviceKey,
    adminGetDevice_userPoolId,
    adminGetDevice_username,
    adminGetDeviceResponse_httpStatus,
    adminGetDeviceResponse_device,

    -- ** GetIdentityProviderByIdentifier
    getIdentityProviderByIdentifier_userPoolId,
    getIdentityProviderByIdentifier_idpIdentifier,
    getIdentityProviderByIdentifierResponse_httpStatus,
    getIdentityProviderByIdentifierResponse_identityProvider,

    -- ** AdminRemoveUserFromGroup
    adminRemoveUserFromGroup_userPoolId,
    adminRemoveUserFromGroup_username,
    adminRemoveUserFromGroup_groupName,

    -- ** SetRiskConfiguration
    setRiskConfiguration_riskExceptionConfiguration,
    setRiskConfiguration_clientId,
    setRiskConfiguration_accountTakeoverRiskConfiguration,
    setRiskConfiguration_compromisedCredentialsRiskConfiguration,
    setRiskConfiguration_userPoolId,
    setRiskConfigurationResponse_httpStatus,
    setRiskConfigurationResponse_riskConfiguration,

    -- ** ConfirmSignUp
    confirmSignUp_clientMetadata,
    confirmSignUp_forceAliasCreation,
    confirmSignUp_analyticsMetadata,
    confirmSignUp_userContextData,
    confirmSignUp_secretHash,
    confirmSignUp_clientId,
    confirmSignUp_username,
    confirmSignUp_confirmationCode,
    confirmSignUpResponse_httpStatus,

    -- ** ListUserPools
    listUserPools_nextToken,
    listUserPools_maxResults,
    listUserPoolsResponse_userPools,
    listUserPoolsResponse_nextToken,
    listUserPoolsResponse_httpStatus,

    -- ** AdminResetUserPassword
    adminResetUserPassword_clientMetadata,
    adminResetUserPassword_userPoolId,
    adminResetUserPassword_username,
    adminResetUserPasswordResponse_httpStatus,

    -- ** UpdateAuthEventFeedback
    updateAuthEventFeedback_userPoolId,
    updateAuthEventFeedback_username,
    updateAuthEventFeedback_eventId,
    updateAuthEventFeedback_feedbackToken,
    updateAuthEventFeedback_feedbackValue,
    updateAuthEventFeedbackResponse_httpStatus,

    -- ** CreateUserImportJob
    createUserImportJob_jobName,
    createUserImportJob_userPoolId,
    createUserImportJob_cloudWatchLogsRoleArn,
    createUserImportJobResponse_userImportJob,
    createUserImportJobResponse_httpStatus,

    -- ** GetUser
    getUser_accessToken,
    getUserResponse_userMFASettingList,
    getUserResponse_mfaOptions,
    getUserResponse_preferredMfaSetting,
    getUserResponse_httpStatus,
    getUserResponse_username,
    getUserResponse_userAttributes,

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

    -- ** AdminDeleteUser
    adminDeleteUser_userPoolId,
    adminDeleteUser_username,

    -- ** AdminForgetDevice
    adminForgetDevice_userPoolId,
    adminForgetDevice_username,
    adminForgetDevice_deviceKey,

    -- ** DescribeResourceServer
    describeResourceServer_userPoolId,
    describeResourceServer_identifier,
    describeResourceServerResponse_httpStatus,
    describeResourceServerResponse_resourceServer,

    -- ** SetUserMFAPreference
    setUserMFAPreference_sMSMfaSettings,
    setUserMFAPreference_softwareTokenMfaSettings,
    setUserMFAPreference_accessToken,
    setUserMFAPreferenceResponse_httpStatus,

    -- ** AdminUpdateDeviceStatus
    adminUpdateDeviceStatus_deviceRememberedStatus,
    adminUpdateDeviceStatus_userPoolId,
    adminUpdateDeviceStatus_username,
    adminUpdateDeviceStatus_deviceKey,
    adminUpdateDeviceStatusResponse_httpStatus,

    -- ** AdminCreateUser
    adminCreateUser_clientMetadata,
    adminCreateUser_temporaryPassword,
    adminCreateUser_forceAliasCreation,
    adminCreateUser_desiredDeliveryMediums,
    adminCreateUser_messageAction,
    adminCreateUser_userAttributes,
    adminCreateUser_validationData,
    adminCreateUser_userPoolId,
    adminCreateUser_username,
    adminCreateUserResponse_user,
    adminCreateUserResponse_httpStatus,

    -- ** AddCustomAttributes
    addCustomAttributes_userPoolId,
    addCustomAttributes_customAttributes,
    addCustomAttributesResponse_httpStatus,

    -- ** ListUserPoolClients
    listUserPoolClients_nextToken,
    listUserPoolClients_maxResults,
    listUserPoolClients_userPoolId,
    listUserPoolClientsResponse_nextToken,
    listUserPoolClientsResponse_userPoolClients,
    listUserPoolClientsResponse_httpStatus,

    -- ** AdminSetUserMFAPreference
    adminSetUserMFAPreference_sMSMfaSettings,
    adminSetUserMFAPreference_softwareTokenMfaSettings,
    adminSetUserMFAPreference_username,
    adminSetUserMFAPreference_userPoolId,
    adminSetUserMFAPreferenceResponse_httpStatus,

    -- ** UpdateUserPoolClient
    updateUserPoolClient_refreshTokenValidity,
    updateUserPoolClient_explicitAuthFlows,
    updateUserPoolClient_supportedIdentityProviders,
    updateUserPoolClient_logoutURLs,
    updateUserPoolClient_allowedOAuthFlowsUserPoolClient,
    updateUserPoolClient_idTokenValidity,
    updateUserPoolClient_tokenValidityUnits,
    updateUserPoolClient_defaultRedirectURI,
    updateUserPoolClient_enableTokenRevocation,
    updateUserPoolClient_writeAttributes,
    updateUserPoolClient_preventUserExistenceErrors,
    updateUserPoolClient_accessTokenValidity,
    updateUserPoolClient_readAttributes,
    updateUserPoolClient_allowedOAuthScopes,
    updateUserPoolClient_allowedOAuthFlows,
    updateUserPoolClient_analyticsConfiguration,
    updateUserPoolClient_clientName,
    updateUserPoolClient_callbackURLs,
    updateUserPoolClient_userPoolId,
    updateUserPoolClient_clientId,
    updateUserPoolClientResponse_userPoolClient,
    updateUserPoolClientResponse_httpStatus,

    -- ** DeleteUserPoolClient
    deleteUserPoolClient_userPoolId,
    deleteUserPoolClient_clientId,

    -- ** UpdateDeviceStatus
    updateDeviceStatus_deviceRememberedStatus,
    updateDeviceStatus_accessToken,
    updateDeviceStatus_deviceKey,
    updateDeviceStatusResponse_httpStatus,

    -- ** ForgetDevice
    forgetDevice_accessToken,
    forgetDevice_deviceKey,

    -- ** GetSigningCertificate
    getSigningCertificate_userPoolId,
    getSigningCertificateResponse_certificate,
    getSigningCertificateResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_accessToken,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreateUserPoolClient
    createUserPoolClient_refreshTokenValidity,
    createUserPoolClient_explicitAuthFlows,
    createUserPoolClient_supportedIdentityProviders,
    createUserPoolClient_logoutURLs,
    createUserPoolClient_allowedOAuthFlowsUserPoolClient,
    createUserPoolClient_generateSecret,
    createUserPoolClient_idTokenValidity,
    createUserPoolClient_tokenValidityUnits,
    createUserPoolClient_defaultRedirectURI,
    createUserPoolClient_enableTokenRevocation,
    createUserPoolClient_writeAttributes,
    createUserPoolClient_preventUserExistenceErrors,
    createUserPoolClient_accessTokenValidity,
    createUserPoolClient_readAttributes,
    createUserPoolClient_allowedOAuthScopes,
    createUserPoolClient_allowedOAuthFlows,
    createUserPoolClient_analyticsConfiguration,
    createUserPoolClient_callbackURLs,
    createUserPoolClient_userPoolId,
    createUserPoolClient_clientName,
    createUserPoolClientResponse_userPoolClient,
    createUserPoolClientResponse_httpStatus,

    -- ** GetUserPoolMfaConfig
    getUserPoolMfaConfig_userPoolId,
    getUserPoolMfaConfigResponse_smsMfaConfiguration,
    getUserPoolMfaConfigResponse_softwareTokenMfaConfiguration,
    getUserPoolMfaConfigResponse_mfaConfiguration,
    getUserPoolMfaConfigResponse_httpStatus,

    -- ** CreateResourceServer
    createResourceServer_scopes,
    createResourceServer_userPoolId,
    createResourceServer_identifier,
    createResourceServer_name,
    createResourceServerResponse_httpStatus,
    createResourceServerResponse_resourceServer,

    -- ** AdminListUserAuthEvents
    adminListUserAuthEvents_nextToken,
    adminListUserAuthEvents_maxResults,
    adminListUserAuthEvents_userPoolId,
    adminListUserAuthEvents_username,
    adminListUserAuthEventsResponse_nextToken,
    adminListUserAuthEventsResponse_authEvents,
    adminListUserAuthEventsResponse_httpStatus,

    -- ** CreateGroup
    createGroup_precedence,
    createGroup_description,
    createGroup_roleArn,
    createGroup_groupName,
    createGroup_userPoolId,
    createGroupResponse_group,
    createGroupResponse_httpStatus,

    -- ** AdminAddUserToGroup
    adminAddUserToGroup_userPoolId,
    adminAddUserToGroup_username,
    adminAddUserToGroup_groupName,

    -- ** VerifySoftwareToken
    verifySoftwareToken_accessToken,
    verifySoftwareToken_friendlyDeviceName,
    verifySoftwareToken_session,
    verifySoftwareToken_userCode,
    verifySoftwareTokenResponse_status,
    verifySoftwareTokenResponse_session,
    verifySoftwareTokenResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** RevokeToken
    revokeToken_clientSecret,
    revokeToken_token,
    revokeToken_clientId,
    revokeTokenResponse_httpStatus,

    -- ** StopUserImportJob
    stopUserImportJob_userPoolId,
    stopUserImportJob_jobId,
    stopUserImportJobResponse_userImportJob,
    stopUserImportJobResponse_httpStatus,

    -- ** DescribeUserImportJob
    describeUserImportJob_userPoolId,
    describeUserImportJob_jobId,
    describeUserImportJobResponse_userImportJob,
    describeUserImportJobResponse_httpStatus,

    -- ** DescribeRiskConfiguration
    describeRiskConfiguration_clientId,
    describeRiskConfiguration_userPoolId,
    describeRiskConfigurationResponse_httpStatus,
    describeRiskConfigurationResponse_riskConfiguration,

    -- ** DeleteGroup
    deleteGroup_groupName,
    deleteGroup_userPoolId,

    -- ** UpdateGroup
    updateGroup_precedence,
    updateGroup_description,
    updateGroup_roleArn,
    updateGroup_groupName,
    updateGroup_userPoolId,
    updateGroupResponse_group,
    updateGroupResponse_httpStatus,

    -- ** GlobalSignOut
    globalSignOut_accessToken,
    globalSignOutResponse_httpStatus,

    -- ** ListGroups
    listGroups_nextToken,
    listGroups_limit,
    listGroups_userPoolId,
    listGroupsResponse_groups,
    listGroupsResponse_nextToken,
    listGroupsResponse_httpStatus,

    -- ** UpdateIdentityProvider
    updateIdentityProvider_idpIdentifiers,
    updateIdentityProvider_attributeMapping,
    updateIdentityProvider_providerDetails,
    updateIdentityProvider_userPoolId,
    updateIdentityProvider_providerName,
    updateIdentityProviderResponse_httpStatus,
    updateIdentityProviderResponse_identityProvider,

    -- ** DeleteIdentityProvider
    deleteIdentityProvider_userPoolId,
    deleteIdentityProvider_providerName,

    -- ** ListResourceServers
    listResourceServers_nextToken,
    listResourceServers_maxResults,
    listResourceServers_userPoolId,
    listResourceServersResponse_nextToken,
    listResourceServersResponse_httpStatus,
    listResourceServersResponse_resourceServers,

    -- ** AdminRespondToAuthChallenge
    adminRespondToAuthChallenge_clientMetadata,
    adminRespondToAuthChallenge_contextData,
    adminRespondToAuthChallenge_analyticsMetadata,
    adminRespondToAuthChallenge_challengeResponses,
    adminRespondToAuthChallenge_session,
    adminRespondToAuthChallenge_userPoolId,
    adminRespondToAuthChallenge_clientId,
    adminRespondToAuthChallenge_challengeName,
    adminRespondToAuthChallengeResponse_challengeName,
    adminRespondToAuthChallengeResponse_challengeParameters,
    adminRespondToAuthChallengeResponse_authenticationResult,
    adminRespondToAuthChallengeResponse_session,
    adminRespondToAuthChallengeResponse_httpStatus,

    -- ** SetUserSettings
    setUserSettings_accessToken,
    setUserSettings_mfaOptions,
    setUserSettingsResponse_httpStatus,

    -- ** AdminListDevices
    adminListDevices_paginationToken,
    adminListDevices_limit,
    adminListDevices_userPoolId,
    adminListDevices_username,
    adminListDevicesResponse_paginationToken,
    adminListDevicesResponse_devices,
    adminListDevicesResponse_httpStatus,

    -- ** DescribeUserPoolClient
    describeUserPoolClient_userPoolId,
    describeUserPoolClient_clientId,
    describeUserPoolClientResponse_userPoolClient,
    describeUserPoolClientResponse_httpStatus,

    -- ** ResendConfirmationCode
    resendConfirmationCode_clientMetadata,
    resendConfirmationCode_analyticsMetadata,
    resendConfirmationCode_userContextData,
    resendConfirmationCode_secretHash,
    resendConfirmationCode_clientId,
    resendConfirmationCode_username,
    resendConfirmationCodeResponse_codeDeliveryDetails,
    resendConfirmationCodeResponse_httpStatus,

    -- ** GetGroup
    getGroup_groupName,
    getGroup_userPoolId,
    getGroupResponse_group,
    getGroupResponse_httpStatus,

    -- ** AdminSetUserSettings
    adminSetUserSettings_userPoolId,
    adminSetUserSettings_username,
    adminSetUserSettings_mfaOptions,
    adminSetUserSettingsResponse_httpStatus,

    -- ** ListDevices
    listDevices_paginationToken,
    listDevices_limit,
    listDevices_accessToken,
    listDevicesResponse_paginationToken,
    listDevicesResponse_devices,
    listDevicesResponse_httpStatus,

    -- * Types

    -- ** AccountRecoverySettingType
    accountRecoverySettingType_recoveryMechanisms,

    -- ** AccountTakeoverActionType
    accountTakeoverActionType_notify,
    accountTakeoverActionType_eventAction,

    -- ** AccountTakeoverActionsType
    accountTakeoverActionsType_lowAction,
    accountTakeoverActionsType_highAction,
    accountTakeoverActionsType_mediumAction,

    -- ** AccountTakeoverRiskConfigurationType
    accountTakeoverRiskConfigurationType_notifyConfiguration,
    accountTakeoverRiskConfigurationType_actions,

    -- ** AdminCreateUserConfigType
    adminCreateUserConfigType_allowAdminCreateUserOnly,
    adminCreateUserConfigType_unusedAccountValidityDays,
    adminCreateUserConfigType_inviteMessageTemplate,

    -- ** AnalyticsConfigurationType
    analyticsConfigurationType_applicationArn,
    analyticsConfigurationType_userDataShared,
    analyticsConfigurationType_applicationId,
    analyticsConfigurationType_externalId,
    analyticsConfigurationType_roleArn,

    -- ** AnalyticsMetadataType
    analyticsMetadataType_analyticsEndpointId,

    -- ** AttributeType
    attributeType_value,
    attributeType_name,

    -- ** AuthEventType
    authEventType_eventRisk,
    authEventType_eventResponse,
    authEventType_eventContextData,
    authEventType_challengeResponses,
    authEventType_eventType,
    authEventType_creationDate,
    authEventType_eventFeedback,
    authEventType_eventId,

    -- ** AuthenticationResultType
    authenticationResultType_accessToken,
    authenticationResultType_refreshToken,
    authenticationResultType_newDeviceMetadata,
    authenticationResultType_expiresIn,
    authenticationResultType_tokenType,
    authenticationResultType_idToken,

    -- ** ChallengeResponseType
    challengeResponseType_challengeName,
    challengeResponseType_challengeResponse,

    -- ** CodeDeliveryDetailsType
    codeDeliveryDetailsType_destination,
    codeDeliveryDetailsType_deliveryMedium,
    codeDeliveryDetailsType_attributeName,

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
    deviceType_deviceCreateDate,
    deviceType_deviceAttributes,
    deviceType_deviceKey,
    deviceType_deviceLastAuthenticatedDate,

    -- ** DomainDescriptionType
    domainDescriptionType_status,
    domainDescriptionType_cloudFrontDistribution,
    domainDescriptionType_userPoolId,
    domainDescriptionType_domain,
    domainDescriptionType_aWSAccountId,
    domainDescriptionType_customDomainConfig,
    domainDescriptionType_version,
    domainDescriptionType_s3Bucket,

    -- ** EmailConfigurationType
    emailConfigurationType_sourceArn,
    emailConfigurationType_from,
    emailConfigurationType_configurationSet,
    emailConfigurationType_replyToEmailAddress,
    emailConfigurationType_emailSendingAccount,

    -- ** EventContextDataType
    eventContextDataType_ipAddress,
    eventContextDataType_country,
    eventContextDataType_city,
    eventContextDataType_deviceName,
    eventContextDataType_timezone,

    -- ** EventFeedbackType
    eventFeedbackType_feedbackDate,
    eventFeedbackType_feedbackValue,
    eventFeedbackType_provider,

    -- ** EventRiskType
    eventRiskType_compromisedCredentialsDetected,
    eventRiskType_riskLevel,
    eventRiskType_riskDecision,

    -- ** GroupType
    groupType_lastModifiedDate,
    groupType_userPoolId,
    groupType_creationDate,
    groupType_precedence,
    groupType_groupName,
    groupType_description,
    groupType_roleArn,

    -- ** HttpHeader
    httpHeader_headerValue,
    httpHeader_headerName,

    -- ** IdentityProviderType
    identityProviderType_lastModifiedDate,
    identityProviderType_userPoolId,
    identityProviderType_providerType,
    identityProviderType_creationDate,
    identityProviderType_idpIdentifiers,
    identityProviderType_attributeMapping,
    identityProviderType_providerDetails,
    identityProviderType_providerName,

    -- ** LambdaConfigType
    lambdaConfigType_preAuthentication,
    lambdaConfigType_createAuthChallenge,
    lambdaConfigType_verifyAuthChallengeResponse,
    lambdaConfigType_customSMSSender,
    lambdaConfigType_postAuthentication,
    lambdaConfigType_customMessage,
    lambdaConfigType_defineAuthChallenge,
    lambdaConfigType_customEmailSender,
    lambdaConfigType_kmsKeyID,
    lambdaConfigType_postConfirmation,
    lambdaConfigType_preTokenGeneration,
    lambdaConfigType_userMigration,
    lambdaConfigType_preSignUp,

    -- ** MFAOptionType
    mfaOptionType_deliveryMedium,
    mfaOptionType_attributeName,

    -- ** MessageTemplateType
    messageTemplateType_emailSubject,
    messageTemplateType_sMSMessage,
    messageTemplateType_emailMessage,

    -- ** NewDeviceMetadataType
    newDeviceMetadataType_deviceGroupKey,
    newDeviceMetadataType_deviceKey,

    -- ** NotifyConfigurationType
    notifyConfigurationType_noActionEmail,
    notifyConfigurationType_from,
    notifyConfigurationType_replyTo,
    notifyConfigurationType_blockEmail,
    notifyConfigurationType_mfaEmail,
    notifyConfigurationType_sourceArn,

    -- ** NotifyEmailType
    notifyEmailType_textBody,
    notifyEmailType_htmlBody,
    notifyEmailType_subject,

    -- ** NumberAttributeConstraintsType
    numberAttributeConstraintsType_maxValue,
    numberAttributeConstraintsType_minValue,

    -- ** PasswordPolicyType
    passwordPolicyType_requireNumbers,
    passwordPolicyType_requireUppercase,
    passwordPolicyType_requireLowercase,
    passwordPolicyType_minimumLength,
    passwordPolicyType_requireSymbols,
    passwordPolicyType_temporaryPasswordValidityDays,

    -- ** ProviderDescription
    providerDescription_lastModifiedDate,
    providerDescription_providerType,
    providerDescription_creationDate,
    providerDescription_providerName,

    -- ** ProviderUserIdentifierType
    providerUserIdentifierType_providerAttributeValue,
    providerUserIdentifierType_providerAttributeName,
    providerUserIdentifierType_providerName,

    -- ** RecoveryOptionType
    recoveryOptionType_priority,
    recoveryOptionType_name,

    -- ** ResourceServerScopeType
    resourceServerScopeType_scopeName,
    resourceServerScopeType_scopeDescription,

    -- ** ResourceServerType
    resourceServerType_userPoolId,
    resourceServerType_identifier,
    resourceServerType_scopes,
    resourceServerType_name,

    -- ** RiskConfigurationType
    riskConfigurationType_riskExceptionConfiguration,
    riskConfigurationType_clientId,
    riskConfigurationType_accountTakeoverRiskConfiguration,
    riskConfigurationType_lastModifiedDate,
    riskConfigurationType_userPoolId,
    riskConfigurationType_compromisedCredentialsRiskConfiguration,

    -- ** RiskExceptionConfigurationType
    riskExceptionConfigurationType_skippedIPRangeList,
    riskExceptionConfigurationType_blockedIPRangeList,

    -- ** SMSMfaSettingsType
    sMSMfaSettingsType_enabled,
    sMSMfaSettingsType_preferredMfa,

    -- ** SchemaAttributeType
    schemaAttributeType_numberAttributeConstraints,
    schemaAttributeType_required,
    schemaAttributeType_attributeDataType,
    schemaAttributeType_stringAttributeConstraints,
    schemaAttributeType_name,
    schemaAttributeType_developerOnlyAttribute,
    schemaAttributeType_mutable,

    -- ** SmsConfigurationType
    smsConfigurationType_externalId,
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
    tokenValidityUnitsType_refreshToken,
    tokenValidityUnitsType_idToken,

    -- ** UICustomizationType
    uICustomizationType_clientId,
    uICustomizationType_lastModifiedDate,
    uICustomizationType_userPoolId,
    uICustomizationType_css,
    uICustomizationType_cSSVersion,
    uICustomizationType_imageUrl,
    uICustomizationType_creationDate,

    -- ** UserContextDataType
    userContextDataType_encodedData,

    -- ** UserImportJobType
    userImportJobType_status,
    userImportJobType_skippedUsers,
    userImportJobType_jobId,
    userImportJobType_userPoolId,
    userImportJobType_jobName,
    userImportJobType_preSignedUrl,
    userImportJobType_failedUsers,
    userImportJobType_startDate,
    userImportJobType_completionMessage,
    userImportJobType_creationDate,
    userImportJobType_completionDate,
    userImportJobType_cloudWatchLogsRoleArn,
    userImportJobType_importedUsers,

    -- ** UserPoolAddOnsType
    userPoolAddOnsType_advancedSecurityMode,

    -- ** UserPoolClientDescription
    userPoolClientDescription_clientId,
    userPoolClientDescription_userPoolId,
    userPoolClientDescription_clientName,

    -- ** UserPoolClientType
    userPoolClientType_refreshTokenValidity,
    userPoolClientType_clientId,
    userPoolClientType_explicitAuthFlows,
    userPoolClientType_clientSecret,
    userPoolClientType_lastModifiedDate,
    userPoolClientType_supportedIdentityProviders,
    userPoolClientType_logoutURLs,
    userPoolClientType_allowedOAuthFlowsUserPoolClient,
    userPoolClientType_userPoolId,
    userPoolClientType_idTokenValidity,
    userPoolClientType_tokenValidityUnits,
    userPoolClientType_defaultRedirectURI,
    userPoolClientType_enableTokenRevocation,
    userPoolClientType_writeAttributes,
    userPoolClientType_preventUserExistenceErrors,
    userPoolClientType_accessTokenValidity,
    userPoolClientType_creationDate,
    userPoolClientType_readAttributes,
    userPoolClientType_allowedOAuthScopes,
    userPoolClientType_allowedOAuthFlows,
    userPoolClientType_analyticsConfiguration,
    userPoolClientType_clientName,
    userPoolClientType_callbackURLs,

    -- ** UserPoolDescriptionType
    userPoolDescriptionType_status,
    userPoolDescriptionType_lastModifiedDate,
    userPoolDescriptionType_name,
    userPoolDescriptionType_id,
    userPoolDescriptionType_creationDate,
    userPoolDescriptionType_lambdaConfig,

    -- ** UserPoolPolicyType
    userPoolPolicyType_passwordPolicy,

    -- ** UserPoolType
    userPoolType_status,
    userPoolType_userPoolTags,
    userPoolType_emailConfigurationFailure,
    userPoolType_lastModifiedDate,
    userPoolType_verificationMessageTemplate,
    userPoolType_estimatedNumberOfUsers,
    userPoolType_arn,
    userPoolType_domain,
    userPoolType_customDomain,
    userPoolType_emailVerificationMessage,
    userPoolType_smsAuthenticationMessage,
    userPoolType_userPoolAddOns,
    userPoolType_schemaAttributes,
    userPoolType_emailVerificationSubject,
    userPoolType_usernameAttributes,
    userPoolType_aliasAttributes,
    userPoolType_accountRecoverySetting,
    userPoolType_emailConfiguration,
    userPoolType_smsVerificationMessage,
    userPoolType_name,
    userPoolType_mfaConfiguration,
    userPoolType_id,
    userPoolType_smsConfigurationFailure,
    userPoolType_creationDate,
    userPoolType_lambdaConfig,
    userPoolType_smsConfiguration,
    userPoolType_adminCreateUserConfig,
    userPoolType_deviceConfiguration,
    userPoolType_autoVerifiedAttributes,
    userPoolType_policies,
    userPoolType_usernameConfiguration,

    -- ** UserType
    userType_enabled,
    userType_userStatus,
    userType_username,
    userType_userCreateDate,
    userType_attributes,
    userType_mfaOptions,
    userType_userLastModifiedDate,

    -- ** UsernameConfigurationType
    usernameConfigurationType_caseSensitive,

    -- ** VerificationMessageTemplateType
    verificationMessageTemplateType_defaultEmailOption,
    verificationMessageTemplateType_emailSubject,
    verificationMessageTemplateType_emailSubjectByLink,
    verificationMessageTemplateType_smsMessage,
    verificationMessageTemplateType_emailMessageByLink,
    verificationMessageTemplateType_emailMessage,
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
import Network.AWS.CognitoIdentityProvider.RevokeToken
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
