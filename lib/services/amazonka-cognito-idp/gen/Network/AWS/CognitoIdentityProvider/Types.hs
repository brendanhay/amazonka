{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentityProvider.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UnsupportedUserStateException,
    _PasswordResetRequiredException,
    _InvalidParameterException,
    _UnsupportedOperationException,
    _InvalidLambdaResponseException,
    _InvalidEmailRoleAccessPolicyException,
    _UnsupportedIdentityProviderException,
    _UserNotFoundException,
    _UnexpectedLambdaException,
    _NotAuthorizedException,
    _InternalErrorException,
    _InvalidUserPoolConfigurationException,
    _InvalidSmsRoleAccessPolicyException,
    _InvalidOAuthFlowException,
    _CodeMismatchException,
    _UserImportInProgressException,
    _InvalidSmsRoleTrustRelationshipException,
    _UserPoolTaggingException,
    _SoftwareTokenMFANotFoundException,
    _TooManyRequestsException,
    _ConcurrentModificationException,
    _UserPoolAddOnNotEnabledException,
    _UserLambdaValidationException,
    _PreconditionNotMetException,
    _ExpiredCodeException,
    _TooManyFailedAttemptsException,
    _EnableSoftwareTokenMFAException,
    _UserNotConfirmedException,
    _GroupExistsException,
    _UnauthorizedException,
    _CodeDeliveryFailureException,
    _ScopeDoesNotExistException,
    _ResourceNotFoundException,
    _MFAMethodNotFoundException,
    _AliasExistsException,
    _UnsupportedTokenTypeException,
    _DuplicateProviderException,
    _LimitExceededException,
    _InvalidPasswordException,
    _UsernameExistsException,

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
    accountTakeoverActionsType_highAction,
    accountTakeoverActionsType_mediumAction,

    -- * AccountTakeoverRiskConfigurationType
    AccountTakeoverRiskConfigurationType (..),
    newAccountTakeoverRiskConfigurationType,
    accountTakeoverRiskConfigurationType_notifyConfiguration,
    accountTakeoverRiskConfigurationType_actions,

    -- * AdminCreateUserConfigType
    AdminCreateUserConfigType (..),
    newAdminCreateUserConfigType,
    adminCreateUserConfigType_allowAdminCreateUserOnly,
    adminCreateUserConfigType_unusedAccountValidityDays,
    adminCreateUserConfigType_inviteMessageTemplate,

    -- * AnalyticsConfigurationType
    AnalyticsConfigurationType (..),
    newAnalyticsConfigurationType,
    analyticsConfigurationType_applicationArn,
    analyticsConfigurationType_userDataShared,
    analyticsConfigurationType_applicationId,
    analyticsConfigurationType_externalId,
    analyticsConfigurationType_roleArn,

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
    authEventType_eventRisk,
    authEventType_eventResponse,
    authEventType_eventContextData,
    authEventType_challengeResponses,
    authEventType_eventType,
    authEventType_creationDate,
    authEventType_eventFeedback,
    authEventType_eventId,

    -- * AuthenticationResultType
    AuthenticationResultType (..),
    newAuthenticationResultType,
    authenticationResultType_accessToken,
    authenticationResultType_refreshToken,
    authenticationResultType_newDeviceMetadata,
    authenticationResultType_expiresIn,
    authenticationResultType_tokenType,
    authenticationResultType_idToken,

    -- * ChallengeResponseType
    ChallengeResponseType (..),
    newChallengeResponseType,
    challengeResponseType_challengeName,
    challengeResponseType_challengeResponse,

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
    deviceType_deviceCreateDate,
    deviceType_deviceAttributes,
    deviceType_deviceKey,
    deviceType_deviceLastAuthenticatedDate,

    -- * DomainDescriptionType
    DomainDescriptionType (..),
    newDomainDescriptionType,
    domainDescriptionType_status,
    domainDescriptionType_cloudFrontDistribution,
    domainDescriptionType_userPoolId,
    domainDescriptionType_domain,
    domainDescriptionType_aWSAccountId,
    domainDescriptionType_customDomainConfig,
    domainDescriptionType_version,
    domainDescriptionType_s3Bucket,

    -- * EmailConfigurationType
    EmailConfigurationType (..),
    newEmailConfigurationType,
    emailConfigurationType_sourceArn,
    emailConfigurationType_from,
    emailConfigurationType_configurationSet,
    emailConfigurationType_replyToEmailAddress,
    emailConfigurationType_emailSendingAccount,

    -- * EventContextDataType
    EventContextDataType (..),
    newEventContextDataType,
    eventContextDataType_ipAddress,
    eventContextDataType_country,
    eventContextDataType_city,
    eventContextDataType_deviceName,
    eventContextDataType_timezone,

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
    groupType_lastModifiedDate,
    groupType_userPoolId,
    groupType_creationDate,
    groupType_precedence,
    groupType_groupName,
    groupType_description,
    groupType_roleArn,

    -- * HttpHeader
    HttpHeader (..),
    newHttpHeader,
    httpHeader_headerValue,
    httpHeader_headerName,

    -- * IdentityProviderType
    IdentityProviderType (..),
    newIdentityProviderType,
    identityProviderType_lastModifiedDate,
    identityProviderType_userPoolId,
    identityProviderType_providerType,
    identityProviderType_creationDate,
    identityProviderType_idpIdentifiers,
    identityProviderType_attributeMapping,
    identityProviderType_providerDetails,
    identityProviderType_providerName,

    -- * LambdaConfigType
    LambdaConfigType (..),
    newLambdaConfigType,
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
    notifyConfigurationType_noActionEmail,
    notifyConfigurationType_from,
    notifyConfigurationType_replyTo,
    notifyConfigurationType_blockEmail,
    notifyConfigurationType_mfaEmail,
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
    numberAttributeConstraintsType_maxValue,
    numberAttributeConstraintsType_minValue,

    -- * PasswordPolicyType
    PasswordPolicyType (..),
    newPasswordPolicyType,
    passwordPolicyType_requireNumbers,
    passwordPolicyType_requireUppercase,
    passwordPolicyType_requireLowercase,
    passwordPolicyType_minimumLength,
    passwordPolicyType_requireSymbols,
    passwordPolicyType_temporaryPasswordValidityDays,

    -- * ProviderDescription
    ProviderDescription (..),
    newProviderDescription,
    providerDescription_lastModifiedDate,
    providerDescription_providerType,
    providerDescription_creationDate,
    providerDescription_providerName,

    -- * ProviderUserIdentifierType
    ProviderUserIdentifierType (..),
    newProviderUserIdentifierType,
    providerUserIdentifierType_providerAttributeValue,
    providerUserIdentifierType_providerAttributeName,
    providerUserIdentifierType_providerName,

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
    resourceServerType_userPoolId,
    resourceServerType_identifier,
    resourceServerType_scopes,
    resourceServerType_name,

    -- * RiskConfigurationType
    RiskConfigurationType (..),
    newRiskConfigurationType,
    riskConfigurationType_riskExceptionConfiguration,
    riskConfigurationType_clientId,
    riskConfigurationType_accountTakeoverRiskConfiguration,
    riskConfigurationType_lastModifiedDate,
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
    schemaAttributeType_numberAttributeConstraints,
    schemaAttributeType_required,
    schemaAttributeType_attributeDataType,
    schemaAttributeType_stringAttributeConstraints,
    schemaAttributeType_name,
    schemaAttributeType_developerOnlyAttribute,
    schemaAttributeType_mutable,

    -- * SmsConfigurationType
    SmsConfigurationType (..),
    newSmsConfigurationType,
    smsConfigurationType_externalId,
    smsConfigurationType_snsCallerArn,

    -- * SmsMfaConfigType
    SmsMfaConfigType (..),
    newSmsMfaConfigType,
    smsMfaConfigType_smsAuthenticationMessage,
    smsMfaConfigType_smsConfiguration,

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
    stringAttributeConstraintsType_maxLength,
    stringAttributeConstraintsType_minLength,

    -- * TokenValidityUnitsType
    TokenValidityUnitsType (..),
    newTokenValidityUnitsType,
    tokenValidityUnitsType_accessToken,
    tokenValidityUnitsType_refreshToken,
    tokenValidityUnitsType_idToken,

    -- * UICustomizationType
    UICustomizationType (..),
    newUICustomizationType,
    uICustomizationType_clientId,
    uICustomizationType_lastModifiedDate,
    uICustomizationType_userPoolId,
    uICustomizationType_css,
    uICustomizationType_cSSVersion,
    uICustomizationType_imageUrl,
    uICustomizationType_creationDate,

    -- * UserContextDataType
    UserContextDataType (..),
    newUserContextDataType,
    userContextDataType_encodedData,

    -- * UserImportJobType
    UserImportJobType (..),
    newUserImportJobType,
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

    -- * UserPoolAddOnsType
    UserPoolAddOnsType (..),
    newUserPoolAddOnsType,
    userPoolAddOnsType_advancedSecurityMode,

    -- * UserPoolClientDescription
    UserPoolClientDescription (..),
    newUserPoolClientDescription,
    userPoolClientDescription_clientId,
    userPoolClientDescription_userPoolId,
    userPoolClientDescription_clientName,

    -- * UserPoolClientType
    UserPoolClientType (..),
    newUserPoolClientType,
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

    -- * UserPoolDescriptionType
    UserPoolDescriptionType (..),
    newUserPoolDescriptionType,
    userPoolDescriptionType_status,
    userPoolDescriptionType_lastModifiedDate,
    userPoolDescriptionType_name,
    userPoolDescriptionType_id,
    userPoolDescriptionType_creationDate,
    userPoolDescriptionType_lambdaConfig,

    -- * UserPoolPolicyType
    UserPoolPolicyType (..),
    newUserPoolPolicyType,
    userPoolPolicyType_passwordPolicy,

    -- * UserPoolType
    UserPoolType (..),
    newUserPoolType,
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

    -- * UserType
    UserType (..),
    newUserType,
    userType_enabled,
    userType_userStatus,
    userType_username,
    userType_userCreateDate,
    userType_attributes,
    userType_mfaOptions,
    userType_userLastModifiedDate,

    -- * UsernameConfigurationType
    UsernameConfigurationType (..),
    newUsernameConfigurationType,
    usernameConfigurationType_caseSensitive,

    -- * VerificationMessageTemplateType
    VerificationMessageTemplateType (..),
    newVerificationMessageTemplateType,
    verificationMessageTemplateType_defaultEmailOption,
    verificationMessageTemplateType_emailSubject,
    verificationMessageTemplateType_emailSubjectByLink,
    verificationMessageTemplateType_smsMessage,
    verificationMessageTemplateType_emailMessageByLink,
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-04-18@ of the Amazon Cognito Identity Provider SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CognitoIdentityProvider",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cognito-idp",
      Core._serviceSigningName = "cognito-idp",
      Core._serviceVersion = "2016-04-18",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CognitoIdentityProvider",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request failed because the user is in an unsupported state.
_UnsupportedUserStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedUserStateException =
  Core._MatchServiceError
    defaultService
    "UnsupportedUserStateException"

-- | This exception is thrown when a password reset is required.
_PasswordResetRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PasswordResetRequiredException =
  Core._MatchServiceError
    defaultService
    "PasswordResetRequiredException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid parameter.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | This exception is thrown when you attempt to perform an operation that
-- is not enabled for the user pool client.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid Lambda response.
_InvalidLambdaResponseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLambdaResponseException =
  Core._MatchServiceError
    defaultService
    "InvalidLambdaResponseException"

-- | This exception is thrown when Amazon Cognito is not allowed to use your
-- email identity. HTTP status code: 400.
_InvalidEmailRoleAccessPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEmailRoleAccessPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidEmailRoleAccessPolicyException"

-- | This exception is thrown when the specified identifier is not supported.
_UnsupportedIdentityProviderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedIdentityProviderException =
  Core._MatchServiceError
    defaultService
    "UnsupportedIdentityProviderException"

-- | This exception is thrown when a user is not found.
_UserNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotFoundException =
  Core._MatchServiceError
    defaultService
    "UserNotFoundException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- unexpected exception with the Lambda service.
_UnexpectedLambdaException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnexpectedLambdaException =
  Core._MatchServiceError
    defaultService
    "UnexpectedLambdaException"

-- | This exception is thrown when a user is not authorized.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"

-- | This exception is thrown when Amazon Cognito encounters an internal
-- error.
_InternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | This exception is thrown when the user pool configuration is invalid.
_InvalidUserPoolConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUserPoolConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidUserPoolConfigurationException"

-- | This exception is returned when the role provided for SMS configuration
-- does not have permission to publish using Amazon SNS.
_InvalidSmsRoleAccessPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSmsRoleAccessPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidSmsRoleAccessPolicyException"

-- | This exception is thrown when the specified OAuth flow is invalid.
_InvalidOAuthFlowException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOAuthFlowException =
  Core._MatchServiceError
    defaultService
    "InvalidOAuthFlowException"

-- | This exception is thrown if the provided code does not match what the
-- server was expecting.
_CodeMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeMismatchException =
  Core._MatchServiceError
    defaultService
    "CodeMismatchException"

-- | This exception is thrown when you are trying to modify a user pool while
-- a user import job is in progress for that pool.
_UserImportInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserImportInProgressException =
  Core._MatchServiceError
    defaultService
    "UserImportInProgressException"

-- | This exception is thrown when the trust relationship is invalid for the
-- role provided for SMS configuration. This can happen if you do not trust
-- @cognito-idp.amazonaws.com@ or the external ID provided in the role does
-- not match what is provided in the SMS configuration for the user pool.
_InvalidSmsRoleTrustRelationshipException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSmsRoleTrustRelationshipException =
  Core._MatchServiceError
    defaultService
    "InvalidSmsRoleTrustRelationshipException"

-- | This exception is thrown when a user pool tag cannot be set or updated.
_UserPoolTaggingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserPoolTaggingException =
  Core._MatchServiceError
    defaultService
    "UserPoolTaggingException"

-- | This exception is thrown when the software token TOTP multi-factor
-- authentication (MFA) is not enabled for the user pool.
_SoftwareTokenMFANotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SoftwareTokenMFANotFoundException =
  Core._MatchServiceError
    defaultService
    "SoftwareTokenMFANotFoundException"

-- | This exception is thrown when the user has made too many requests for a
-- given operation.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | This exception is thrown if two or more modifications are happening
-- concurrently.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | This exception is thrown when user pool add-ons are not enabled.
_UserPoolAddOnNotEnabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserPoolAddOnNotEnabledException =
  Core._MatchServiceError
    defaultService
    "UserPoolAddOnNotEnabledException"

-- | This exception is thrown when the Amazon Cognito service encounters a
-- user validation exception with the Lambda service.
_UserLambdaValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserLambdaValidationException =
  Core._MatchServiceError
    defaultService
    "UserLambdaValidationException"

-- | This exception is thrown when a precondition is not met.
_PreconditionNotMetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PreconditionNotMetException =
  Core._MatchServiceError
    defaultService
    "PreconditionNotMetException"

-- | This exception is thrown if a code has expired.
_ExpiredCodeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExpiredCodeException =
  Core._MatchServiceError
    defaultService
    "ExpiredCodeException"

-- | This exception is thrown when the user has made too many failed attempts
-- for a given action (e.g., sign in).
_TooManyFailedAttemptsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyFailedAttemptsException =
  Core._MatchServiceError
    defaultService
    "TooManyFailedAttemptsException"

-- | This exception is thrown when there is a code mismatch and the service
-- fails to configure the software token TOTP multi-factor authentication
-- (MFA).
_EnableSoftwareTokenMFAException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EnableSoftwareTokenMFAException =
  Core._MatchServiceError
    defaultService
    "EnableSoftwareTokenMFAException"

-- | This exception is thrown when a user is not confirmed successfully.
_UserNotConfirmedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotConfirmedException =
  Core._MatchServiceError
    defaultService
    "UserNotConfirmedException"

-- | This exception is thrown when Amazon Cognito encounters a group that
-- already exists in the user pool.
_GroupExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GroupExistsException =
  Core._MatchServiceError
    defaultService
    "GroupExistsException"

-- | This exception is thrown when the request is not authorized. This can
-- happen due to an invalid access token in the request.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"

-- | This exception is thrown when a verification code fails to deliver
-- successfully.
_CodeDeliveryFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeDeliveryFailureException =
  Core._MatchServiceError
    defaultService
    "CodeDeliveryFailureException"

-- | This exception is thrown when the specified scope does not exist.
_ScopeDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ScopeDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ScopeDoesNotExistException"

-- | This exception is thrown when the Amazon Cognito service cannot find the
-- requested resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | This exception is thrown when Amazon Cognito cannot find a multi-factor
-- authentication (MFA) method.
_MFAMethodNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MFAMethodNotFoundException =
  Core._MatchServiceError
    defaultService
    "MFAMethodNotFoundException"

-- | This exception is thrown when a user tries to confirm the account with
-- an email or phone number that has already been supplied as an alias from
-- a different account. This exception tells user that an account with this
-- email or phone already exists.
_AliasExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AliasExistsException =
  Core._MatchServiceError
    defaultService
    "AliasExistsException"

-- | This exception is thrown when an unsupported token is passed to an
-- operation.
_UnsupportedTokenTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedTokenTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedTokenTypeException"

-- | This exception is thrown when the provider is already supported by the
-- user pool.
_DuplicateProviderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateProviderException =
  Core._MatchServiceError
    defaultService
    "DuplicateProviderException"

-- | This exception is thrown when a user exceeds the limit for a requested
-- Amazon Web Services resource.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid password.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"

-- | This exception is thrown when Amazon Cognito encounters a user name that
-- already exists in the user pool.
_UsernameExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UsernameExistsException =
  Core._MatchServiceError
    defaultService
    "UsernameExistsException"
