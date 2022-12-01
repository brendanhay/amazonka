{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentityProvider.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CodeMismatchException,
    _UnsupportedOperationException,
    _UsernameExistsException,
    _PasswordResetRequiredException,
    _UserPoolAddOnNotEnabledException,
    _CodeDeliveryFailureException,
    _ConcurrentModificationException,
    _UserPoolTaggingException,
    _UnauthorizedException,
    _EnableSoftwareTokenMFAException,
    _InternalErrorException,
    _UserNotFoundException,
    _DuplicateProviderException,
    _PreconditionNotMetException,
    _TooManyFailedAttemptsException,
    _UserLambdaValidationException,
    _InvalidPasswordException,
    _ResourceNotFoundException,
    _UnexpectedLambdaException,
    _UnsupportedUserStateException,
    _LimitExceededException,
    _InvalidUserPoolConfigurationException,
    _AliasExistsException,
    _ForbiddenException,
    _ScopeDoesNotExistException,
    _InvalidLambdaResponseException,
    _GroupExistsException,
    _ExpiredCodeException,
    _UserImportInProgressException,
    _InvalidSmsRoleTrustRelationshipException,
    _SoftwareTokenMFANotFoundException,
    _MFAMethodNotFoundException,
    _NotAuthorizedException,
    _InvalidOAuthFlowException,
    _UnsupportedIdentityProviderException,
    _InvalidEmailRoleAccessPolicyException,
    _UnsupportedTokenTypeException,
    _InvalidSmsRoleAccessPolicyException,
    _TooManyRequestsException,
    _UserNotConfirmedException,
    _InvalidParameterException,

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

    -- * DeletionProtectionType
    DeletionProtectionType (..),

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
    adminCreateUserConfigType_inviteMessageTemplate,
    adminCreateUserConfigType_allowAdminCreateUserOnly,
    adminCreateUserConfigType_unusedAccountValidityDays,

    -- * AnalyticsConfigurationType
    AnalyticsConfigurationType (..),
    newAnalyticsConfigurationType,
    analyticsConfigurationType_userDataShared,
    analyticsConfigurationType_roleArn,
    analyticsConfigurationType_applicationArn,
    analyticsConfigurationType_externalId,
    analyticsConfigurationType_applicationId,

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
    authEventType_eventResponse,
    authEventType_eventContextData,
    authEventType_creationDate,
    authEventType_eventRisk,
    authEventType_eventId,
    authEventType_eventFeedback,
    authEventType_challengeResponses,

    -- * AuthenticationResultType
    AuthenticationResultType (..),
    newAuthenticationResultType,
    authenticationResultType_accessToken,
    authenticationResultType_expiresIn,
    authenticationResultType_idToken,
    authenticationResultType_newDeviceMetadata,
    authenticationResultType_tokenType,
    authenticationResultType_refreshToken,

    -- * ChallengeResponseType
    ChallengeResponseType (..),
    newChallengeResponseType,
    challengeResponseType_challengeResponse,
    challengeResponseType_challengeName,

    -- * CodeDeliveryDetailsType
    CodeDeliveryDetailsType (..),
    newCodeDeliveryDetailsType,
    codeDeliveryDetailsType_destination,
    codeDeliveryDetailsType_deliveryMedium,
    codeDeliveryDetailsType_attributeName,

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
    deviceConfigurationType_deviceOnlyRememberedOnUserPrompt,
    deviceConfigurationType_challengeRequiredOnNewDevice,

    -- * DeviceSecretVerifierConfigType
    DeviceSecretVerifierConfigType (..),
    newDeviceSecretVerifierConfigType,
    deviceSecretVerifierConfigType_salt,
    deviceSecretVerifierConfigType_passwordVerifier,

    -- * DeviceType
    DeviceType (..),
    newDeviceType,
    deviceType_deviceKey,
    deviceType_deviceLastAuthenticatedDate,
    deviceType_deviceAttributes,
    deviceType_deviceLastModifiedDate,
    deviceType_deviceCreateDate,

    -- * DomainDescriptionType
    DomainDescriptionType (..),
    newDomainDescriptionType,
    domainDescriptionType_aWSAccountId,
    domainDescriptionType_s3Bucket,
    domainDescriptionType_domain,
    domainDescriptionType_customDomainConfig,
    domainDescriptionType_status,
    domainDescriptionType_userPoolId,
    domainDescriptionType_cloudFrontDistribution,
    domainDescriptionType_version,

    -- * EmailConfigurationType
    EmailConfigurationType (..),
    newEmailConfigurationType,
    emailConfigurationType_from,
    emailConfigurationType_replyToEmailAddress,
    emailConfigurationType_sourceArn,
    emailConfigurationType_emailSendingAccount,
    emailConfigurationType_configurationSet,

    -- * EventContextDataType
    EventContextDataType (..),
    newEventContextDataType,
    eventContextDataType_country,
    eventContextDataType_deviceName,
    eventContextDataType_timezone,
    eventContextDataType_city,
    eventContextDataType_ipAddress,

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
    eventRiskType_riskLevel,
    eventRiskType_riskDecision,

    -- * GroupType
    GroupType (..),
    newGroupType,
    groupType_roleArn,
    groupType_lastModifiedDate,
    groupType_creationDate,
    groupType_groupName,
    groupType_description,
    groupType_userPoolId,
    groupType_precedence,

    -- * HttpHeader
    HttpHeader (..),
    newHttpHeader,
    httpHeader_headerValue,
    httpHeader_headerName,

    -- * IdentityProviderType
    IdentityProviderType (..),
    newIdentityProviderType,
    identityProviderType_providerName,
    identityProviderType_lastModifiedDate,
    identityProviderType_attributeMapping,
    identityProviderType_providerDetails,
    identityProviderType_idpIdentifiers,
    identityProviderType_creationDate,
    identityProviderType_providerType,
    identityProviderType_userPoolId,

    -- * LambdaConfigType
    LambdaConfigType (..),
    newLambdaConfigType,
    lambdaConfigType_customSMSSender,
    lambdaConfigType_userMigration,
    lambdaConfigType_postAuthentication,
    lambdaConfigType_defineAuthChallenge,
    lambdaConfigType_postConfirmation,
    lambdaConfigType_preSignUp,
    lambdaConfigType_verifyAuthChallengeResponse,
    lambdaConfigType_customMessage,
    lambdaConfigType_kmsKeyID,
    lambdaConfigType_preAuthentication,
    lambdaConfigType_preTokenGeneration,
    lambdaConfigType_createAuthChallenge,
    lambdaConfigType_customEmailSender,

    -- * MFAOptionType
    MFAOptionType (..),
    newMFAOptionType,
    mfaOptionType_deliveryMedium,
    mfaOptionType_attributeName,

    -- * MessageTemplateType
    MessageTemplateType (..),
    newMessageTemplateType,
    messageTemplateType_emailSubject,
    messageTemplateType_sMSMessage,
    messageTemplateType_emailMessage,

    -- * NewDeviceMetadataType
    NewDeviceMetadataType (..),
    newNewDeviceMetadataType,
    newDeviceMetadataType_deviceGroupKey,
    newDeviceMetadataType_deviceKey,

    -- * NotifyConfigurationType
    NotifyConfigurationType (..),
    newNotifyConfigurationType,
    notifyConfigurationType_from,
    notifyConfigurationType_mfaEmail,
    notifyConfigurationType_blockEmail,
    notifyConfigurationType_noActionEmail,
    notifyConfigurationType_replyTo,
    notifyConfigurationType_sourceArn,

    -- * NotifyEmailType
    NotifyEmailType (..),
    newNotifyEmailType,
    notifyEmailType_textBody,
    notifyEmailType_htmlBody,
    notifyEmailType_subject,

    -- * NumberAttributeConstraintsType
    NumberAttributeConstraintsType (..),
    newNumberAttributeConstraintsType,
    numberAttributeConstraintsType_minValue,
    numberAttributeConstraintsType_maxValue,

    -- * PasswordPolicyType
    PasswordPolicyType (..),
    newPasswordPolicyType,
    passwordPolicyType_requireLowercase,
    passwordPolicyType_requireNumbers,
    passwordPolicyType_requireSymbols,
    passwordPolicyType_requireUppercase,
    passwordPolicyType_minimumLength,
    passwordPolicyType_temporaryPasswordValidityDays,

    -- * ProviderDescription
    ProviderDescription (..),
    newProviderDescription,
    providerDescription_providerName,
    providerDescription_lastModifiedDate,
    providerDescription_creationDate,
    providerDescription_providerType,

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
    resourceServerType_name,
    resourceServerType_identifier,
    resourceServerType_userPoolId,
    resourceServerType_scopes,

    -- * RiskConfigurationType
    RiskConfigurationType (..),
    newRiskConfigurationType,
    riskConfigurationType_clientId,
    riskConfigurationType_lastModifiedDate,
    riskConfigurationType_accountTakeoverRiskConfiguration,
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
    sMSMfaSettingsType_preferredMfa,
    sMSMfaSettingsType_enabled,

    -- * SchemaAttributeType
    SchemaAttributeType (..),
    newSchemaAttributeType,
    schemaAttributeType_numberAttributeConstraints,
    schemaAttributeType_name,
    schemaAttributeType_required,
    schemaAttributeType_attributeDataType,
    schemaAttributeType_stringAttributeConstraints,
    schemaAttributeType_mutable,
    schemaAttributeType_developerOnlyAttribute,

    -- * SmsConfigurationType
    SmsConfigurationType (..),
    newSmsConfigurationType,
    smsConfigurationType_externalId,
    smsConfigurationType_snsRegion,
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
    softwareTokenMfaSettingsType_preferredMfa,
    softwareTokenMfaSettingsType_enabled,

    -- * StringAttributeConstraintsType
    StringAttributeConstraintsType (..),
    newStringAttributeConstraintsType,
    stringAttributeConstraintsType_maxLength,
    stringAttributeConstraintsType_minLength,

    -- * TokenValidityUnitsType
    TokenValidityUnitsType (..),
    newTokenValidityUnitsType,
    tokenValidityUnitsType_accessToken,
    tokenValidityUnitsType_idToken,
    tokenValidityUnitsType_refreshToken,

    -- * UICustomizationType
    UICustomizationType (..),
    newUICustomizationType,
    uICustomizationType_cSSVersion,
    uICustomizationType_clientId,
    uICustomizationType_lastModifiedDate,
    uICustomizationType_imageUrl,
    uICustomizationType_creationDate,
    uICustomizationType_userPoolId,
    uICustomizationType_css,

    -- * UserAttributeUpdateSettingsType
    UserAttributeUpdateSettingsType (..),
    newUserAttributeUpdateSettingsType,
    userAttributeUpdateSettingsType_attributesRequireVerificationBeforeUpdate,

    -- * UserContextDataType
    UserContextDataType (..),
    newUserContextDataType,
    userContextDataType_encodedData,
    userContextDataType_ipAddress,

    -- * UserImportJobType
    UserImportJobType (..),
    newUserImportJobType,
    userImportJobType_skippedUsers,
    userImportJobType_failedUsers,
    userImportJobType_jobName,
    userImportJobType_completionDate,
    userImportJobType_creationDate,
    userImportJobType_jobId,
    userImportJobType_status,
    userImportJobType_importedUsers,
    userImportJobType_startDate,
    userImportJobType_userPoolId,
    userImportJobType_cloudWatchLogsRoleArn,
    userImportJobType_preSignedUrl,
    userImportJobType_completionMessage,

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
    userPoolClientType_authSessionValidity,
    userPoolClientType_defaultRedirectURI,
    userPoolClientType_clientSecret,
    userPoolClientType_accessTokenValidity,
    userPoolClientType_clientId,
    userPoolClientType_lastModifiedDate,
    userPoolClientType_explicitAuthFlows,
    userPoolClientType_callbackURLs,
    userPoolClientType_allowedOAuthScopes,
    userPoolClientType_creationDate,
    userPoolClientType_idTokenValidity,
    userPoolClientType_allowedOAuthFlowsUserPoolClient,
    userPoolClientType_refreshTokenValidity,
    userPoolClientType_analyticsConfiguration,
    userPoolClientType_preventUserExistenceErrors,
    userPoolClientType_enablePropagateAdditionalUserContextData,
    userPoolClientType_tokenValidityUnits,
    userPoolClientType_clientName,
    userPoolClientType_enableTokenRevocation,
    userPoolClientType_allowedOAuthFlows,
    userPoolClientType_userPoolId,
    userPoolClientType_writeAttributes,
    userPoolClientType_logoutURLs,
    userPoolClientType_supportedIdentityProviders,
    userPoolClientType_readAttributes,

    -- * UserPoolDescriptionType
    UserPoolDescriptionType (..),
    newUserPoolDescriptionType,
    userPoolDescriptionType_name,
    userPoolDescriptionType_lastModifiedDate,
    userPoolDescriptionType_creationDate,
    userPoolDescriptionType_status,
    userPoolDescriptionType_id,
    userPoolDescriptionType_lambdaConfig,

    -- * UserPoolPolicyType
    UserPoolPolicyType (..),
    newUserPoolPolicyType,
    userPoolPolicyType_passwordPolicy,

    -- * UserPoolType
    UserPoolType (..),
    newUserPoolType,
    userPoolType_emailConfiguration,
    userPoolType_schemaAttributes,
    userPoolType_name,
    userPoolType_adminCreateUserConfig,
    userPoolType_verificationMessageTemplate,
    userPoolType_aliasAttributes,
    userPoolType_deviceConfiguration,
    userPoolType_mfaConfiguration,
    userPoolType_lastModifiedDate,
    userPoolType_smsConfigurationFailure,
    userPoolType_usernameConfiguration,
    userPoolType_domain,
    userPoolType_arn,
    userPoolType_creationDate,
    userPoolType_status,
    userPoolType_id,
    userPoolType_autoVerifiedAttributes,
    userPoolType_smsConfiguration,
    userPoolType_policies,
    userPoolType_smsVerificationMessage,
    userPoolType_emailVerificationSubject,
    userPoolType_accountRecoverySetting,
    userPoolType_userAttributeUpdateSettings,
    userPoolType_userPoolTags,
    userPoolType_deletionProtection,
    userPoolType_emailVerificationMessage,
    userPoolType_smsAuthenticationMessage,
    userPoolType_emailConfigurationFailure,
    userPoolType_usernameAttributes,
    userPoolType_estimatedNumberOfUsers,
    userPoolType_lambdaConfig,
    userPoolType_customDomain,
    userPoolType_userPoolAddOns,

    -- * UserType
    UserType (..),
    newUserType,
    userType_username,
    userType_userLastModifiedDate,
    userType_userCreateDate,
    userType_enabled,
    userType_attributes,
    userType_userStatus,
    userType_mfaOptions,

    -- * UsernameConfigurationType
    UsernameConfigurationType (..),
    newUsernameConfigurationType,
    usernameConfigurationType_caseSensitive,

    -- * VerificationMessageTemplateType
    VerificationMessageTemplateType (..),
    newVerificationMessageTemplateType,
    verificationMessageTemplateType_emailSubject,
    verificationMessageTemplateType_smsMessage,
    verificationMessageTemplateType_emailSubjectByLink,
    verificationMessageTemplateType_emailMessageByLink,
    verificationMessageTemplateType_defaultEmailOption,
    verificationMessageTemplateType_emailMessage,
  )
where

import Amazonka.CognitoIdentityProvider.Types.AccountRecoverySettingType
import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionType
import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverActionsType
import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
import Amazonka.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
import Amazonka.CognitoIdentityProvider.Types.AdminCreateUserConfigType
import Amazonka.CognitoIdentityProvider.Types.AdvancedSecurityModeType
import Amazonka.CognitoIdentityProvider.Types.AliasAttributeType
import Amazonka.CognitoIdentityProvider.Types.AnalyticsConfigurationType
import Amazonka.CognitoIdentityProvider.Types.AnalyticsMetadataType
import Amazonka.CognitoIdentityProvider.Types.AttributeDataType
import Amazonka.CognitoIdentityProvider.Types.AttributeType
import Amazonka.CognitoIdentityProvider.Types.AuthEventType
import Amazonka.CognitoIdentityProvider.Types.AuthFlowType
import Amazonka.CognitoIdentityProvider.Types.AuthenticationResultType
import Amazonka.CognitoIdentityProvider.Types.ChallengeName
import Amazonka.CognitoIdentityProvider.Types.ChallengeNameType
import Amazonka.CognitoIdentityProvider.Types.ChallengeResponse
import Amazonka.CognitoIdentityProvider.Types.ChallengeResponseType
import Amazonka.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
import Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
import Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
import Amazonka.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
import Amazonka.CognitoIdentityProvider.Types.ContextDataType
import Amazonka.CognitoIdentityProvider.Types.CustomDomainConfigType
import Amazonka.CognitoIdentityProvider.Types.CustomEmailLambdaVersionConfigType
import Amazonka.CognitoIdentityProvider.Types.CustomEmailSenderLambdaVersionType
import Amazonka.CognitoIdentityProvider.Types.CustomSMSLambdaVersionConfigType
import Amazonka.CognitoIdentityProvider.Types.CustomSMSSenderLambdaVersionType
import Amazonka.CognitoIdentityProvider.Types.DefaultEmailOptionType
import Amazonka.CognitoIdentityProvider.Types.DeletionProtectionType
import Amazonka.CognitoIdentityProvider.Types.DeliveryMediumType
import Amazonka.CognitoIdentityProvider.Types.DeviceConfigurationType
import Amazonka.CognitoIdentityProvider.Types.DeviceRememberedStatusType
import Amazonka.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
import Amazonka.CognitoIdentityProvider.Types.DeviceType
import Amazonka.CognitoIdentityProvider.Types.DomainDescriptionType
import Amazonka.CognitoIdentityProvider.Types.DomainStatusType
import Amazonka.CognitoIdentityProvider.Types.EmailConfigurationType
import Amazonka.CognitoIdentityProvider.Types.EmailSendingAccountType
import Amazonka.CognitoIdentityProvider.Types.EventContextDataType
import Amazonka.CognitoIdentityProvider.Types.EventFeedbackType
import Amazonka.CognitoIdentityProvider.Types.EventFilterType
import Amazonka.CognitoIdentityProvider.Types.EventResponseType
import Amazonka.CognitoIdentityProvider.Types.EventRiskType
import Amazonka.CognitoIdentityProvider.Types.EventType
import Amazonka.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
import Amazonka.CognitoIdentityProvider.Types.FeedbackValueType
import Amazonka.CognitoIdentityProvider.Types.GroupType
import Amazonka.CognitoIdentityProvider.Types.HttpHeader
import Amazonka.CognitoIdentityProvider.Types.IdentityProviderType
import Amazonka.CognitoIdentityProvider.Types.IdentityProviderTypeType
import Amazonka.CognitoIdentityProvider.Types.LambdaConfigType
import Amazonka.CognitoIdentityProvider.Types.MFAOptionType
import Amazonka.CognitoIdentityProvider.Types.MessageActionType
import Amazonka.CognitoIdentityProvider.Types.MessageTemplateType
import Amazonka.CognitoIdentityProvider.Types.NewDeviceMetadataType
import Amazonka.CognitoIdentityProvider.Types.NotifyConfigurationType
import Amazonka.CognitoIdentityProvider.Types.NotifyEmailType
import Amazonka.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
import Amazonka.CognitoIdentityProvider.Types.OAuthFlowType
import Amazonka.CognitoIdentityProvider.Types.PasswordPolicyType
import Amazonka.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
import Amazonka.CognitoIdentityProvider.Types.ProviderDescription
import Amazonka.CognitoIdentityProvider.Types.ProviderUserIdentifierType
import Amazonka.CognitoIdentityProvider.Types.RecoveryOptionNameType
import Amazonka.CognitoIdentityProvider.Types.RecoveryOptionType
import Amazonka.CognitoIdentityProvider.Types.ResourceServerScopeType
import Amazonka.CognitoIdentityProvider.Types.ResourceServerType
import Amazonka.CognitoIdentityProvider.Types.RiskConfigurationType
import Amazonka.CognitoIdentityProvider.Types.RiskDecisionType
import Amazonka.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
import Amazonka.CognitoIdentityProvider.Types.RiskLevelType
import Amazonka.CognitoIdentityProvider.Types.SMSMfaSettingsType
import Amazonka.CognitoIdentityProvider.Types.SchemaAttributeType
import Amazonka.CognitoIdentityProvider.Types.SmsConfigurationType
import Amazonka.CognitoIdentityProvider.Types.SmsMfaConfigType
import Amazonka.CognitoIdentityProvider.Types.SoftwareTokenMfaConfigType
import Amazonka.CognitoIdentityProvider.Types.SoftwareTokenMfaSettingsType
import Amazonka.CognitoIdentityProvider.Types.StatusType
import Amazonka.CognitoIdentityProvider.Types.StringAttributeConstraintsType
import Amazonka.CognitoIdentityProvider.Types.TimeUnitsType
import Amazonka.CognitoIdentityProvider.Types.TokenValidityUnitsType
import Amazonka.CognitoIdentityProvider.Types.UICustomizationType
import Amazonka.CognitoIdentityProvider.Types.UserAttributeUpdateSettingsType
import Amazonka.CognitoIdentityProvider.Types.UserContextDataType
import Amazonka.CognitoIdentityProvider.Types.UserImportJobStatusType
import Amazonka.CognitoIdentityProvider.Types.UserImportJobType
import Amazonka.CognitoIdentityProvider.Types.UserPoolAddOnsType
import Amazonka.CognitoIdentityProvider.Types.UserPoolClientDescription
import Amazonka.CognitoIdentityProvider.Types.UserPoolClientType
import Amazonka.CognitoIdentityProvider.Types.UserPoolDescriptionType
import Amazonka.CognitoIdentityProvider.Types.UserPoolMfaType
import Amazonka.CognitoIdentityProvider.Types.UserPoolPolicyType
import Amazonka.CognitoIdentityProvider.Types.UserPoolType
import Amazonka.CognitoIdentityProvider.Types.UserStatusType
import Amazonka.CognitoIdentityProvider.Types.UserType
import Amazonka.CognitoIdentityProvider.Types.UsernameAttributeType
import Amazonka.CognitoIdentityProvider.Types.UsernameConfigurationType
import Amazonka.CognitoIdentityProvider.Types.VerificationMessageTemplateType
import Amazonka.CognitoIdentityProvider.Types.VerifiedAttributeType
import Amazonka.CognitoIdentityProvider.Types.VerifySoftwareTokenResponseType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-04-18@ of the Amazon Cognito Identity Provider SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "CognitoIdentityProvider",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cognito-idp",
      Core.signingName = "cognito-idp",
      Core.version = "2016-04-18",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "CognitoIdentityProvider",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | This exception is thrown if the provided code doesn\'t match what the
-- server was expecting.
_CodeMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeMismatchException =
  Core._MatchServiceError
    defaultService
    "CodeMismatchException"

-- | Exception that is thrown when you attempt to perform an operation that
-- isn\'t enabled for the user pool client.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | This exception is thrown when Amazon Cognito encounters a user name that
-- already exists in the user pool.
_UsernameExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UsernameExistsException =
  Core._MatchServiceError
    defaultService
    "UsernameExistsException"

-- | This exception is thrown when a password reset is required.
_PasswordResetRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PasswordResetRequiredException =
  Core._MatchServiceError
    defaultService
    "PasswordResetRequiredException"

-- | This exception is thrown when user pool add-ons aren\'t enabled.
_UserPoolAddOnNotEnabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserPoolAddOnNotEnabledException =
  Core._MatchServiceError
    defaultService
    "UserPoolAddOnNotEnabledException"

-- | This exception is thrown when a verification code fails to deliver
-- successfully.
_CodeDeliveryFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeDeliveryFailureException =
  Core._MatchServiceError
    defaultService
    "CodeDeliveryFailureException"

-- | This exception is thrown if two or more modifications are happening
-- concurrently.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | This exception is thrown when a user pool tag can\'t be set or updated.
_UserPoolTaggingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserPoolTaggingException =
  Core._MatchServiceError
    defaultService
    "UserPoolTaggingException"

-- | Exception that is thrown when the request isn\'t authorized. This can
-- happen due to an invalid access token in the request.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"

-- | This exception is thrown when there is a code mismatch and the service
-- fails to configure the software token TOTP multi-factor authentication
-- (MFA).
_EnableSoftwareTokenMFAException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EnableSoftwareTokenMFAException =
  Core._MatchServiceError
    defaultService
    "EnableSoftwareTokenMFAException"

-- | This exception is thrown when Amazon Cognito encounters an internal
-- error.
_InternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | This exception is thrown when a user isn\'t found.
_UserNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotFoundException =
  Core._MatchServiceError
    defaultService
    "UserNotFoundException"

-- | This exception is thrown when the provider is already supported by the
-- user pool.
_DuplicateProviderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateProviderException =
  Core._MatchServiceError
    defaultService
    "DuplicateProviderException"

-- | This exception is thrown when a precondition is not met.
_PreconditionNotMetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionNotMetException =
  Core._MatchServiceError
    defaultService
    "PreconditionNotMetException"

-- | This exception is thrown when the user has made too many failed attempts
-- for a given action, such as sign-in.
_TooManyFailedAttemptsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyFailedAttemptsException =
  Core._MatchServiceError
    defaultService
    "TooManyFailedAttemptsException"

-- | This exception is thrown when the Amazon Cognito service encounters a
-- user validation exception with the Lambda service.
_UserLambdaValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserLambdaValidationException =
  Core._MatchServiceError
    defaultService
    "UserLambdaValidationException"

-- | This exception is thrown when Amazon Cognito encounters an invalid
-- password.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"

-- | This exception is thrown when the Amazon Cognito service can\'t find the
-- requested resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | This exception is thrown when Amazon Cognito encounters an unexpected
-- exception with Lambda.
_UnexpectedLambdaException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnexpectedLambdaException =
  Core._MatchServiceError
    defaultService
    "UnexpectedLambdaException"

-- | The request failed because the user is in an unsupported state.
_UnsupportedUserStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedUserStateException =
  Core._MatchServiceError
    defaultService
    "UnsupportedUserStateException"

-- | This exception is thrown when a user exceeds the limit for a requested
-- Amazon Web Services resource.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | This exception is thrown when the user pool configuration is not valid.
_InvalidUserPoolConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUserPoolConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidUserPoolConfigurationException"

-- | This exception is thrown when a user tries to confirm the account with
-- an email address or phone number that has already been supplied as an
-- alias for a different user profile. This exception indicates that an
-- account with this email address or phone already exists in a user pool
-- that you\'ve configured to use email address or phone number as a
-- sign-in alias.
_AliasExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AliasExistsException =
  Core._MatchServiceError
    defaultService
    "AliasExistsException"

-- | This exception is thrown when WAF doesn\'t allow your request based on a
-- web ACL that\'s associated with your user pool.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"

-- | This exception is thrown when the specified scope doesn\'t exist.
_ScopeDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ScopeDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ScopeDoesNotExistException"

-- | This exception is thrown when Amazon Cognito encounters an invalid
-- Lambda response.
_InvalidLambdaResponseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLambdaResponseException =
  Core._MatchServiceError
    defaultService
    "InvalidLambdaResponseException"

-- | This exception is thrown when Amazon Cognito encounters a group that
-- already exists in the user pool.
_GroupExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GroupExistsException =
  Core._MatchServiceError
    defaultService
    "GroupExistsException"

-- | This exception is thrown if a code has expired.
_ExpiredCodeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExpiredCodeException =
  Core._MatchServiceError
    defaultService
    "ExpiredCodeException"

-- | This exception is thrown when you\'re trying to modify a user pool while
-- a user import job is in progress for that pool.
_UserImportInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserImportInProgressException =
  Core._MatchServiceError
    defaultService
    "UserImportInProgressException"

-- | This exception is thrown when the trust relationship is not valid for
-- the role provided for SMS configuration. This can happen if you don\'t
-- trust @cognito-idp.amazonaws.com@ or the external ID provided in the
-- role does not match what is provided in the SMS configuration for the
-- user pool.
_InvalidSmsRoleTrustRelationshipException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSmsRoleTrustRelationshipException =
  Core._MatchServiceError
    defaultService
    "InvalidSmsRoleTrustRelationshipException"

-- | This exception is thrown when the software token time-based one-time
-- password (TOTP) multi-factor authentication (MFA) isn\'t activated for
-- the user pool.
_SoftwareTokenMFANotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SoftwareTokenMFANotFoundException =
  Core._MatchServiceError
    defaultService
    "SoftwareTokenMFANotFoundException"

-- | This exception is thrown when Amazon Cognito can\'t find a multi-factor
-- authentication (MFA) method.
_MFAMethodNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MFAMethodNotFoundException =
  Core._MatchServiceError
    defaultService
    "MFAMethodNotFoundException"

-- | This exception is thrown when a user isn\'t authorized.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"

-- | This exception is thrown when the specified OAuth flow is not valid.
_InvalidOAuthFlowException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOAuthFlowException =
  Core._MatchServiceError
    defaultService
    "InvalidOAuthFlowException"

-- | This exception is thrown when the specified identifier isn\'t supported.
_UnsupportedIdentityProviderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedIdentityProviderException =
  Core._MatchServiceError
    defaultService
    "UnsupportedIdentityProviderException"

-- | This exception is thrown when Amazon Cognito isn\'t allowed to use your
-- email identity. HTTP status code: 400.
_InvalidEmailRoleAccessPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEmailRoleAccessPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidEmailRoleAccessPolicyException"

-- | Exception that is thrown when an unsupported token is passed to an
-- operation.
_UnsupportedTokenTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedTokenTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedTokenTypeException"

-- | This exception is returned when the role provided for SMS configuration
-- doesn\'t have permission to publish using Amazon SNS.
_InvalidSmsRoleAccessPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSmsRoleAccessPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidSmsRoleAccessPolicyException"

-- | This exception is thrown when the user has made too many requests for a
-- given operation.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | This exception is thrown when a user isn\'t confirmed successfully.
_UserNotConfirmedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotConfirmedException =
  Core._MatchServiceError
    defaultService
    "UserNotConfirmedException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid parameter.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
