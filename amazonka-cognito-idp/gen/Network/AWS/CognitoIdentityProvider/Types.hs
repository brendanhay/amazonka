{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _PasswordResetRequiredException,
    _UsernameExistsException,
    _CodeDeliveryFailureException,
    _ScopeDoesNotExistException,
    _CodeMismatchException,
    _InternalErrorException,
    _EnableSoftwareTokenMFAException,
    _UserNotConfirmedException,
    _ExpiredCodeException,
    _TooManyFailedAttemptsException,
    _UnexpectedLambdaException,
    _UserNotFoundException,
    _InvalidLambdaResponseException,
    _ConcurrentModificationException,
    _UserLambdaValidationException,
    _SoftwareTokenMFANotFoundException,
    _PreconditionNotMetException,
    _UserPoolTaggingException,
    _InvalidSmsRoleTrustRelationshipException,
    _InvalidParameterException,
    _UnsupportedUserStateException,
    _LimitExceededException,
    _InvalidPasswordException,
    _MFAMethodNotFoundException,
    _AliasExistsException,
    _DuplicateProviderException,
    _UserImportInProgressException,
    _ResourceNotFoundException,
    _InvalidSmsRoleAccessPolicyException,
    _InvalidUserPoolConfigurationException,
    _InvalidOAuthFlowException,
    _GroupExistsException,
    _UnsupportedIdentityProviderException,
    _NotAuthorizedException,
    _TooManyRequestsException,
    _InvalidEmailRoleAccessPolicyException,
    _UserPoolAddOnNotEnabledException,

    -- * AccountTakeoverEventActionType
    AccountTakeoverEventActionType (..),

    -- * AdvancedSecurityModeType
    AdvancedSecurityModeType (..),

    -- * AliasAttributeType
    AliasAttributeType (..),

    -- * AttributeDataType
    AttributeDataType (..),

    -- * AuthFlowType
    AuthFlowType (..),

    -- * ChallengeName
    ChallengeName (..),

    -- * ChallengeNameType
    ChallengeNameType (..),

    -- * ChallengeResponse
    ChallengeResponse (..),

    -- * CompromisedCredentialsEventActionType
    CompromisedCredentialsEventActionType (..),

    -- * CustomEmailSenderLambdaVersionType
    CustomEmailSenderLambdaVersionType (..),

    -- * CustomSMSSenderLambdaVersionType
    CustomSMSSenderLambdaVersionType (..),

    -- * DefaultEmailOptionType
    DefaultEmailOptionType (..),

    -- * DeliveryMediumType
    DeliveryMediumType (..),

    -- * DeviceRememberedStatusType
    DeviceRememberedStatusType (..),

    -- * DomainStatusType
    DomainStatusType (..),

    -- * EmailSendingAccountType
    EmailSendingAccountType (..),

    -- * EventFilterType
    EventFilterType (..),

    -- * EventResponseType
    EventResponseType (..),

    -- * EventType
    EventType (..),

    -- * ExplicitAuthFlowsType
    ExplicitAuthFlowsType (..),

    -- * FeedbackValueType
    FeedbackValueType (..),

    -- * IdentityProviderTypeType
    IdentityProviderTypeType (..),

    -- * MessageActionType
    MessageActionType (..),

    -- * OAuthFlowType
    OAuthFlowType (..),

    -- * PreventUserExistenceErrorTypes
    PreventUserExistenceErrorTypes (..),

    -- * RecoveryOptionNameType
    RecoveryOptionNameType (..),

    -- * RiskDecisionType
    RiskDecisionType (..),

    -- * RiskLevelType
    RiskLevelType (..),

    -- * StatusType
    StatusType (..),

    -- * TimeUnitsType
    TimeUnitsType (..),

    -- * UserImportJobStatusType
    UserImportJobStatusType (..),

    -- * UserPoolMfaType
    UserPoolMfaType (..),

    -- * UserStatusType
    UserStatusType (..),

    -- * UsernameAttributeType
    UsernameAttributeType (..),

    -- * VerifiedAttributeType
    VerifiedAttributeType (..),

    -- * VerifySoftwareTokenResponseType
    VerifySoftwareTokenResponseType (..),

    -- * AccountRecoverySettingType
    AccountRecoverySettingType (..),
    newAccountRecoverySettingType,
    accountRecoverySettingType_recoveryMechanisms,

    -- * AccountTakeoverActionType
    AccountTakeoverActionType (..),
    newAccountTakeoverActionType,
    accountTakeoverActionType_notify,
    accountTakeoverActionType_eventAction,

    -- * AccountTakeoverActionsType
    AccountTakeoverActionsType (..),
    newAccountTakeoverActionsType,
    accountTakeoverActionsType_lowAction,
    accountTakeoverActionsType_mediumAction,
    accountTakeoverActionsType_highAction,

    -- * AccountTakeoverRiskConfigurationType
    AccountTakeoverRiskConfigurationType (..),
    newAccountTakeoverRiskConfigurationType,
    accountTakeoverRiskConfigurationType_notifyConfiguration,
    accountTakeoverRiskConfigurationType_actions,

    -- * AdminCreateUserConfigType
    AdminCreateUserConfigType (..),
    newAdminCreateUserConfigType,
    adminCreateUserConfigType_allowAdminCreateUserOnly,
    adminCreateUserConfigType_inviteMessageTemplate,
    adminCreateUserConfigType_unusedAccountValidityDays,

    -- * AnalyticsConfigurationType
    AnalyticsConfigurationType (..),
    newAnalyticsConfigurationType,
    analyticsConfigurationType_applicationArn,
    analyticsConfigurationType_applicationId,
    analyticsConfigurationType_roleArn,
    analyticsConfigurationType_userDataShared,
    analyticsConfigurationType_externalId,

    -- * AnalyticsMetadataType
    AnalyticsMetadataType (..),
    newAnalyticsMetadataType,
    analyticsMetadataType_analyticsEndpointId,

    -- * AttributeType
    AttributeType (..),
    newAttributeType,
    attributeType_value,
    attributeType_name,

    -- * AuthEventType
    AuthEventType (..),
    newAuthEventType,
    authEventType_eventType,
    authEventType_eventId,
    authEventType_challengeResponses,
    authEventType_eventContextData,
    authEventType_creationDate,
    authEventType_eventRisk,
    authEventType_eventResponse,
    authEventType_eventFeedback,

    -- * AuthenticationResultType
    AuthenticationResultType (..),
    newAuthenticationResultType,
    authenticationResultType_expiresIn,
    authenticationResultType_tokenType,
    authenticationResultType_accessToken,
    authenticationResultType_newDeviceMetadata,
    authenticationResultType_idToken,
    authenticationResultType_refreshToken,

    -- * ChallengeResponseType
    ChallengeResponseType (..),
    newChallengeResponseType,
    challengeResponseType_challengeResponse,
    challengeResponseType_challengeName,

    -- * CodeDeliveryDetailsType
    CodeDeliveryDetailsType (..),
    newCodeDeliveryDetailsType,
    codeDeliveryDetailsType_deliveryMedium,
    codeDeliveryDetailsType_attributeName,
    codeDeliveryDetailsType_destination,

    -- * CompromisedCredentialsActionsType
    CompromisedCredentialsActionsType (..),
    newCompromisedCredentialsActionsType,
    compromisedCredentialsActionsType_eventAction,

    -- * CompromisedCredentialsRiskConfigurationType
    CompromisedCredentialsRiskConfigurationType (..),
    newCompromisedCredentialsRiskConfigurationType,
    compromisedCredentialsRiskConfigurationType_eventFilter,
    compromisedCredentialsRiskConfigurationType_actions,

    -- * ContextDataType
    ContextDataType (..),
    newContextDataType,
    contextDataType_encodedData,
    contextDataType_ipAddress,
    contextDataType_serverName,
    contextDataType_serverPath,
    contextDataType_httpHeaders,

    -- * CustomDomainConfigType
    CustomDomainConfigType (..),
    newCustomDomainConfigType,
    customDomainConfigType_certificateArn,

    -- * CustomEmailLambdaVersionConfigType
    CustomEmailLambdaVersionConfigType (..),
    newCustomEmailLambdaVersionConfigType,
    customEmailLambdaVersionConfigType_lambdaVersion,
    customEmailLambdaVersionConfigType_lambdaArn,

    -- * CustomSMSLambdaVersionConfigType
    CustomSMSLambdaVersionConfigType (..),
    newCustomSMSLambdaVersionConfigType,
    customSMSLambdaVersionConfigType_lambdaVersion,
    customSMSLambdaVersionConfigType_lambdaArn,

    -- * DeviceConfigurationType
    DeviceConfigurationType (..),
    newDeviceConfigurationType,
    deviceConfigurationType_challengeRequiredOnNewDevice,
    deviceConfigurationType_deviceOnlyRememberedOnUserPrompt,

    -- * DeviceSecretVerifierConfigType
    DeviceSecretVerifierConfigType (..),
    newDeviceSecretVerifierConfigType,
    deviceSecretVerifierConfigType_passwordVerifier,
    deviceSecretVerifierConfigType_salt,

    -- * DeviceType
    DeviceType (..),
    newDeviceType,
    deviceType_deviceLastModifiedDate,
    deviceType_deviceKey,
    deviceType_deviceLastAuthenticatedDate,
    deviceType_deviceCreateDate,
    deviceType_deviceAttributes,

    -- * DomainDescriptionType
    DomainDescriptionType (..),
    newDomainDescriptionType,
    domainDescriptionType_status,
    domainDescriptionType_customDomainConfig,
    domainDescriptionType_aWSAccountId,
    domainDescriptionType_s3Bucket,
    domainDescriptionType_userPoolId,
    domainDescriptionType_domain,
    domainDescriptionType_cloudFrontDistribution,
    domainDescriptionType_version,

    -- * EmailConfigurationType
    EmailConfigurationType (..),
    newEmailConfigurationType,
    emailConfigurationType_emailSendingAccount,
    emailConfigurationType_replyToEmailAddress,
    emailConfigurationType_from,
    emailConfigurationType_sourceArn,
    emailConfigurationType_configurationSet,

    -- * EventContextDataType
    EventContextDataType (..),
    newEventContextDataType,
    eventContextDataType_ipAddress,
    eventContextDataType_city,
    eventContextDataType_deviceName,
    eventContextDataType_timezone,
    eventContextDataType_country,

    -- * EventFeedbackType
    EventFeedbackType (..),
    newEventFeedbackType,
    eventFeedbackType_feedbackDate,
    eventFeedbackType_feedbackValue,
    eventFeedbackType_provider,

    -- * EventRiskType
    EventRiskType (..),
    newEventRiskType,
    eventRiskType_compromisedCredentialsDetected,
    eventRiskType_riskDecision,
    eventRiskType_riskLevel,

    -- * GroupType
    GroupType (..),
    newGroupType,
    groupType_lastModifiedDate,
    groupType_roleArn,
    groupType_groupName,
    groupType_userPoolId,
    groupType_creationDate,
    groupType_description,
    groupType_precedence,

    -- * HttpHeader
    HttpHeader (..),
    newHttpHeader,
    httpHeader_headerName,
    httpHeader_headerValue,

    -- * IdentityProviderType
    IdentityProviderType (..),
    newIdentityProviderType,
    identityProviderType_lastModifiedDate,
    identityProviderType_providerType,
    identityProviderType_providerName,
    identityProviderType_providerDetails,
    identityProviderType_userPoolId,
    identityProviderType_creationDate,
    identityProviderType_idpIdentifiers,
    identityProviderType_attributeMapping,

    -- * LambdaConfigType
    LambdaConfigType (..),
    newLambdaConfigType,
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

    -- * MFAOptionType
    MFAOptionType (..),
    newMFAOptionType,
    mfaOptionType_deliveryMedium,
    mfaOptionType_attributeName,

    -- * MessageTemplateType
    MessageTemplateType (..),
    newMessageTemplateType,
    messageTemplateType_emailSubject,
    messageTemplateType_emailMessage,
    messageTemplateType_sMSMessage,

    -- * NewDeviceMetadataType
    NewDeviceMetadataType (..),
    newNewDeviceMetadataType,
    newDeviceMetadataType_deviceKey,
    newDeviceMetadataType_deviceGroupKey,

    -- * NotifyConfigurationType
    NotifyConfigurationType (..),
    newNotifyConfigurationType,
    notifyConfigurationType_mfaEmail,
    notifyConfigurationType_blockEmail,
    notifyConfigurationType_replyTo,
    notifyConfigurationType_from,
    notifyConfigurationType_noActionEmail,
    notifyConfigurationType_sourceArn,

    -- * NotifyEmailType
    NotifyEmailType (..),
    newNotifyEmailType,
    notifyEmailType_htmlBody,
    notifyEmailType_textBody,
    notifyEmailType_subject,

    -- * NumberAttributeConstraintsType
    NumberAttributeConstraintsType (..),
    newNumberAttributeConstraintsType,
    numberAttributeConstraintsType_maxValue,
    numberAttributeConstraintsType_minValue,

    -- * PasswordPolicyType
    PasswordPolicyType (..),
    newPasswordPolicyType,
    passwordPolicyType_temporaryPasswordValidityDays,
    passwordPolicyType_requireUppercase,
    passwordPolicyType_minimumLength,
    passwordPolicyType_requireSymbols,
    passwordPolicyType_requireNumbers,
    passwordPolicyType_requireLowercase,

    -- * ProviderDescription
    ProviderDescription (..),
    newProviderDescription,
    providerDescription_lastModifiedDate,
    providerDescription_providerType,
    providerDescription_providerName,
    providerDescription_creationDate,

    -- * ProviderUserIdentifierType
    ProviderUserIdentifierType (..),
    newProviderUserIdentifierType,
    providerUserIdentifierType_providerName,
    providerUserIdentifierType_providerAttributeName,
    providerUserIdentifierType_providerAttributeValue,

    -- * RecoveryOptionType
    RecoveryOptionType (..),
    newRecoveryOptionType,
    recoveryOptionType_priority,
    recoveryOptionType_name,

    -- * ResourceServerScopeType
    ResourceServerScopeType (..),
    newResourceServerScopeType,
    resourceServerScopeType_scopeName,
    resourceServerScopeType_scopeDescription,

    -- * ResourceServerType
    ResourceServerType (..),
    newResourceServerType,
    resourceServerType_scopes,
    resourceServerType_identifier,
    resourceServerType_userPoolId,
    resourceServerType_name,

    -- * RiskConfigurationType
    RiskConfigurationType (..),
    newRiskConfigurationType,
    riskConfigurationType_accountTakeoverRiskConfiguration,
    riskConfigurationType_lastModifiedDate,
    riskConfigurationType_clientId,
    riskConfigurationType_riskExceptionConfiguration,
    riskConfigurationType_userPoolId,
    riskConfigurationType_compromisedCredentialsRiskConfiguration,

    -- * RiskExceptionConfigurationType
    RiskExceptionConfigurationType (..),
    newRiskExceptionConfigurationType,
    riskExceptionConfigurationType_skippedIPRangeList,
    riskExceptionConfigurationType_blockedIPRangeList,

    -- * SMSMfaSettingsType
    SMSMfaSettingsType (..),
    newSMSMfaSettingsType,
    sMSMfaSettingsType_enabled,
    sMSMfaSettingsType_preferredMfa,

    -- * SchemaAttributeType
    SchemaAttributeType (..),
    newSchemaAttributeType,
    schemaAttributeType_attributeDataType,
    schemaAttributeType_required,
    schemaAttributeType_numberAttributeConstraints,
    schemaAttributeType_developerOnlyAttribute,
    schemaAttributeType_stringAttributeConstraints,
    schemaAttributeType_name,
    schemaAttributeType_mutable,

    -- * SmsConfigurationType
    SmsConfigurationType (..),
    newSmsConfigurationType,
    smsConfigurationType_externalId,
    smsConfigurationType_snsCallerArn,

    -- * SmsMfaConfigType
    SmsMfaConfigType (..),
    newSmsMfaConfigType,
    smsMfaConfigType_smsConfiguration,
    smsMfaConfigType_smsAuthenticationMessage,

    -- * SoftwareTokenMfaConfigType
    SoftwareTokenMfaConfigType (..),
    newSoftwareTokenMfaConfigType,
    softwareTokenMfaConfigType_enabled,

    -- * SoftwareTokenMfaSettingsType
    SoftwareTokenMfaSettingsType (..),
    newSoftwareTokenMfaSettingsType,
    softwareTokenMfaSettingsType_enabled,
    softwareTokenMfaSettingsType_preferredMfa,

    -- * StringAttributeConstraintsType
    StringAttributeConstraintsType (..),
    newStringAttributeConstraintsType,
    stringAttributeConstraintsType_minLength,
    stringAttributeConstraintsType_maxLength,

    -- * TokenValidityUnitsType
    TokenValidityUnitsType (..),
    newTokenValidityUnitsType,
    tokenValidityUnitsType_accessToken,
    tokenValidityUnitsType_idToken,
    tokenValidityUnitsType_refreshToken,

    -- * UICustomizationType
    UICustomizationType (..),
    newUICustomizationType,
    uICustomizationType_lastModifiedDate,
    uICustomizationType_clientId,
    uICustomizationType_cSSVersion,
    uICustomizationType_userPoolId,
    uICustomizationType_creationDate,
    uICustomizationType_imageUrl,
    uICustomizationType_css,

    -- * UserContextDataType
    UserContextDataType (..),
    newUserContextDataType,
    userContextDataType_encodedData,

    -- * UserImportJobType
    UserImportJobType (..),
    newUserImportJobType,
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

    -- * UserPoolAddOnsType
    UserPoolAddOnsType (..),
    newUserPoolAddOnsType,
    userPoolAddOnsType_advancedSecurityMode,

    -- * UserPoolClientDescription
    UserPoolClientDescription (..),
    newUserPoolClientDescription,
    userPoolClientDescription_clientId,
    userPoolClientDescription_clientName,
    userPoolClientDescription_userPoolId,

    -- * UserPoolClientType
    UserPoolClientType (..),
    newUserPoolClientType,
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

    -- * UserPoolDescriptionType
    UserPoolDescriptionType (..),
    newUserPoolDescriptionType,
    userPoolDescriptionType_lastModifiedDate,
    userPoolDescriptionType_status,
    userPoolDescriptionType_id,
    userPoolDescriptionType_creationDate,
    userPoolDescriptionType_lambdaConfig,
    userPoolDescriptionType_name,

    -- * UserPoolPolicyType
    UserPoolPolicyType (..),
    newUserPoolPolicyType,
    userPoolPolicyType_passwordPolicy,

    -- * UserPoolType
    UserPoolType (..),
    newUserPoolType,
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

    -- * UserType
    UserType (..),
    newUserType,
    userType_userCreateDate,
    userType_userLastModifiedDate,
    userType_enabled,
    userType_attributes,
    userType_username,
    userType_userStatus,
    userType_mfaOptions,

    -- * UsernameConfigurationType
    UsernameConfigurationType (..),
    newUsernameConfigurationType,
    usernameConfigurationType_caseSensitive,

    -- * VerificationMessageTemplateType
    VerificationMessageTemplateType (..),
    newVerificationMessageTemplateType,
    verificationMessageTemplateType_defaultEmailOption,
    verificationMessageTemplateType_emailSubjectByLink,
    verificationMessageTemplateType_emailSubject,
    verificationMessageTemplateType_emailMessage,
    verificationMessageTemplateType_emailMessageByLink,
    verificationMessageTemplateType_smsMessage,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType
import Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
import Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType
import Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType
import Network.AWS.CognitoIdentityProvider.Types.AttributeDataType
import Network.AWS.CognitoIdentityProvider.Types.AttributeType
import Network.AWS.CognitoIdentityProvider.Types.AuthEventType
import Network.AWS.CognitoIdentityProvider.Types.AuthFlowType
import Network.AWS.CognitoIdentityProvider.Types.AuthenticationResultType
import Network.AWS.CognitoIdentityProvider.Types.ChallengeName
import Network.AWS.CognitoIdentityProvider.Types.ChallengeNameType
import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse
import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType
import Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.ContextDataType
import Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
import Network.AWS.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
import Network.AWS.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType
import Network.AWS.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
import Network.AWS.CognitoIdentityProvider.Types.CustomSMSSenderLambdaVersionType
import Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType
import Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
import Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.DeviceRememberedStatusType
import Network.AWS.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
import Network.AWS.CognitoIdentityProvider.Types.DeviceType
import Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType
import Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
import Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
import Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
import Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
import Network.AWS.CognitoIdentityProvider.Types.EventFilterType
import Network.AWS.CognitoIdentityProvider.Types.EventResponseType
import Network.AWS.CognitoIdentityProvider.Types.EventRiskType
import Network.AWS.CognitoIdentityProvider.Types.EventType
import Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
import Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType
import Network.AWS.CognitoIdentityProvider.Types.GroupType
import Network.AWS.CognitoIdentityProvider.Types.HttpHeader
import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType
import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
import Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
import Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
import Network.AWS.CognitoIdentityProvider.Types.MessageActionType
import Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
import Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
import Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
import Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
import Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType
import Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
import Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
import Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
import Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
import Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
import Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
import Network.AWS.CognitoIdentityProvider.Types.SMSMfaSettingsType
import Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
import Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.SmsMfaConfigType
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMfaSettingsType
import Network.AWS.CognitoIdentityProvider.Types.StatusType
import Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
import Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType
import Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType
import Network.AWS.CognitoIdentityProvider.Types.UICustomizationType
import Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
import Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
import Network.AWS.CognitoIdentityProvider.Types.UserImportJobType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
import Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolDescriptionType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolMfaType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolType
import Network.AWS.CognitoIdentityProvider.Types.UserStatusType
import Network.AWS.CognitoIdentityProvider.Types.UserType
import Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
import Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
import Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
import Network.AWS.CognitoIdentityProvider.Types.VerifySoftwareTokenResponseType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-04-18@ of the Amazon Cognito Identity Provider SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "CognitoIdentityProvider",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "cognito-idp",
      Prelude._svcSigningName = "cognito-idp",
      Prelude._svcVersion = "2016-04-18",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CognitoIdentityProvider",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | This exception is thrown when a password reset is required.
_PasswordResetRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PasswordResetRequiredException =
  Prelude._MatchServiceError
    defaultService
    "PasswordResetRequiredException"

-- | This exception is thrown when Amazon Cognito encounters a user name that
-- already exists in the user pool.
_UsernameExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UsernameExistsException =
  Prelude._MatchServiceError
    defaultService
    "UsernameExistsException"

-- | This exception is thrown when a verification code fails to deliver
-- successfully.
_CodeDeliveryFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CodeDeliveryFailureException =
  Prelude._MatchServiceError
    defaultService
    "CodeDeliveryFailureException"

-- | This exception is thrown when the specified scope does not exist.
_ScopeDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ScopeDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "ScopeDoesNotExistException"

-- | This exception is thrown if the provided code does not match what the
-- server was expecting.
_CodeMismatchException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CodeMismatchException =
  Prelude._MatchServiceError
    defaultService
    "CodeMismatchException"

-- | This exception is thrown when Amazon Cognito encounters an internal
-- error.
_InternalErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalErrorException"

-- | This exception is thrown when there is a code mismatch and the service
-- fails to configure the software token TOTP multi-factor authentication
-- (MFA).
_EnableSoftwareTokenMFAException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EnableSoftwareTokenMFAException =
  Prelude._MatchServiceError
    defaultService
    "EnableSoftwareTokenMFAException"

-- | This exception is thrown when a user is not confirmed successfully.
_UserNotConfirmedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UserNotConfirmedException =
  Prelude._MatchServiceError
    defaultService
    "UserNotConfirmedException"

-- | This exception is thrown if a code has expired.
_ExpiredCodeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExpiredCodeException =
  Prelude._MatchServiceError
    defaultService
    "ExpiredCodeException"

-- | This exception is thrown when the user has made too many failed attempts
-- for a given action (e.g., sign in).
_TooManyFailedAttemptsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyFailedAttemptsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyFailedAttemptsException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- unexpected exception with the AWS Lambda service.
_UnexpectedLambdaException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnexpectedLambdaException =
  Prelude._MatchServiceError
    defaultService
    "UnexpectedLambdaException"

-- | This exception is thrown when a user is not found.
_UserNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UserNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "UserNotFoundException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid AWS Lambda response.
_InvalidLambdaResponseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLambdaResponseException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLambdaResponseException"

-- | This exception is thrown if two or more modifications are happening
-- concurrently.
_ConcurrentModificationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentModificationException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | This exception is thrown when the Amazon Cognito service encounters a
-- user validation exception with the AWS Lambda service.
_UserLambdaValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UserLambdaValidationException =
  Prelude._MatchServiceError
    defaultService
    "UserLambdaValidationException"

-- | This exception is thrown when the software token TOTP multi-factor
-- authentication (MFA) is not enabled for the user pool.
_SoftwareTokenMFANotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SoftwareTokenMFANotFoundException =
  Prelude._MatchServiceError
    defaultService
    "SoftwareTokenMFANotFoundException"

-- | This exception is thrown when a precondition is not met.
_PreconditionNotMetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PreconditionNotMetException =
  Prelude._MatchServiceError
    defaultService
    "PreconditionNotMetException"

-- | This exception is thrown when a user pool tag cannot be set or updated.
_UserPoolTaggingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UserPoolTaggingException =
  Prelude._MatchServiceError
    defaultService
    "UserPoolTaggingException"

-- | This exception is thrown when the trust relationship is invalid for the
-- role provided for SMS configuration. This can happen if you do not trust
-- __cognito-idp.amazonaws.com__ or the external ID provided in the role
-- does not match what is provided in the SMS configuration for the user
-- pool.
_InvalidSmsRoleTrustRelationshipException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSmsRoleTrustRelationshipException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSmsRoleTrustRelationshipException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid parameter.
_InvalidParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The request failed because the user is in an unsupported state.
_UnsupportedUserStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedUserStateException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedUserStateException"

-- | This exception is thrown when a user exceeds the limit for a requested
-- AWS resource.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid password.
_InvalidPasswordException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPasswordException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPasswordException"

-- | This exception is thrown when Amazon Cognito cannot find a multi-factor
-- authentication (MFA) method.
_MFAMethodNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MFAMethodNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "MFAMethodNotFoundException"

-- | This exception is thrown when a user tries to confirm the account with
-- an email or phone number that has already been supplied as an alias from
-- a different account. This exception tells user that an account with this
-- email or phone already exists.
_AliasExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AliasExistsException =
  Prelude._MatchServiceError
    defaultService
    "AliasExistsException"

-- | This exception is thrown when the provider is already supported by the
-- user pool.
_DuplicateProviderException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateProviderException =
  Prelude._MatchServiceError
    defaultService
    "DuplicateProviderException"

-- | This exception is thrown when you are trying to modify a user pool while
-- a user import job is in progress for that pool.
_UserImportInProgressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UserImportInProgressException =
  Prelude._MatchServiceError
    defaultService
    "UserImportInProgressException"

-- | This exception is thrown when the Amazon Cognito service cannot find the
-- requested resource.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | This exception is returned when the role provided for SMS configuration
-- does not have permission to publish using Amazon SNS.
_InvalidSmsRoleAccessPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSmsRoleAccessPolicyException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSmsRoleAccessPolicyException"

-- | This exception is thrown when the user pool configuration is invalid.
_InvalidUserPoolConfigurationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidUserPoolConfigurationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidUserPoolConfigurationException"

-- | This exception is thrown when the specified OAuth flow is invalid.
_InvalidOAuthFlowException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOAuthFlowException =
  Prelude._MatchServiceError
    defaultService
    "InvalidOAuthFlowException"

-- | This exception is thrown when Amazon Cognito encounters a group that
-- already exists in the user pool.
_GroupExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_GroupExistsException =
  Prelude._MatchServiceError
    defaultService
    "GroupExistsException"

-- | This exception is thrown when the specified identifier is not supported.
_UnsupportedIdentityProviderException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedIdentityProviderException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedIdentityProviderException"

-- | This exception is thrown when a user is not authorized.
_NotAuthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotAuthorizedException =
  Prelude._MatchServiceError
    defaultService
    "NotAuthorizedException"

-- | This exception is thrown when the user has made too many requests for a
-- given operation.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | This exception is thrown when Amazon Cognito is not allowed to use your
-- email identity. HTTP status code: 400.
_InvalidEmailRoleAccessPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidEmailRoleAccessPolicyException =
  Prelude._MatchServiceError
    defaultService
    "InvalidEmailRoleAccessPolicyException"

-- | This exception is thrown when user pool add-ons are not enabled.
_UserPoolAddOnNotEnabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UserPoolAddOnNotEnabledException =
  Prelude._MatchServiceError
    defaultService
    "UserPoolAddOnNotEnabledException"
