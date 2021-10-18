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
    _UnsupportedTokenTypeException,
    _ScopeDoesNotExistException,
    _CodeMismatchException,
    _CodeDeliveryFailureException,
    _UnauthorizedException,
    _InternalErrorException,
    _UserNotConfirmedException,
    _EnableSoftwareTokenMFAException,
    _ExpiredCodeException,
    _UnexpectedLambdaException,
    _UserNotFoundException,
    _TooManyFailedAttemptsException,
    _InvalidLambdaResponseException,
    _SoftwareTokenMFANotFoundException,
    _ConcurrentModificationException,
    _UserLambdaValidationException,
    _PreconditionNotMetException,
    _InvalidSmsRoleTrustRelationshipException,
    _UnsupportedOperationException,
    _UserPoolTaggingException,
    _InvalidParameterException,
    _UnsupportedUserStateException,
    _InvalidPasswordException,
    _MFAMethodNotFoundException,
    _UserImportInProgressException,
    _AliasExistsException,
    _DuplicateProviderException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _InvalidOAuthFlowException,
    _InvalidUserPoolConfigurationException,
    _InvalidSmsRoleAccessPolicyException,
    _GroupExistsException,
    _NotAuthorizedException,
    _UnsupportedIdentityProviderException,
    _InvalidEmailRoleAccessPolicyException,
    _UserPoolAddOnNotEnabledException,
    _TooManyRequestsException,

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
    analyticsConfigurationType_applicationId,
    analyticsConfigurationType_applicationArn,
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
    authenticationResultType_refreshToken,
    authenticationResultType_idToken,

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
    domainDescriptionType_aWSAccountId,
    domainDescriptionType_customDomainConfig,
    domainDescriptionType_s3Bucket,
    domainDescriptionType_userPoolId,
    domainDescriptionType_domain,
    domainDescriptionType_version,
    domainDescriptionType_cloudFrontDistribution,

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
    eventContextDataType_deviceName,
    eventContextDataType_city,
    eventContextDataType_ipAddress,
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
    groupType_userPoolId,
    groupType_groupName,
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
    lambdaConfigType_postConfirmation,
    lambdaConfigType_preAuthentication,
    lambdaConfigType_kmsKeyID,
    lambdaConfigType_createAuthChallenge,
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
    riskConfigurationType_lastModifiedDate,
    riskConfigurationType_accountTakeoverRiskConfiguration,
    riskConfigurationType_riskExceptionConfiguration,
    riskConfigurationType_clientId,
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
    schemaAttributeType_numberAttributeConstraints,
    schemaAttributeType_required,
    schemaAttributeType_developerOnlyAttribute,
    schemaAttributeType_name,
    schemaAttributeType_stringAttributeConstraints,
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
    tokenValidityUnitsType_refreshToken,
    tokenValidityUnitsType_idToken,

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
    userImportJobType_preSignedUrl,
    userImportJobType_cloudWatchLogsRoleArn,
    userImportJobType_importedUsers,
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
    userPoolClientType_idTokenValidity,
    userPoolClientType_refreshTokenValidity,
    userPoolClientType_clientId,
    userPoolClientType_clientName,
    userPoolClientType_allowedOAuthScopes,
    userPoolClientType_analyticsConfiguration,
    userPoolClientType_userPoolId,
    userPoolClientType_creationDate,
    userPoolClientType_readAttributes,
    userPoolClientType_supportedIdentityProviders,
    userPoolClientType_writeAttributes,
    userPoolClientType_logoutURLs,
    userPoolClientType_enableTokenRevocation,
    userPoolClientType_defaultRedirectURI,
    userPoolClientType_explicitAuthFlows,
    userPoolClientType_tokenValidityUnits,
    userPoolClientType_callbackURLs,
    userPoolClientType_allowedOAuthFlows,
    userPoolClientType_accessTokenValidity,
    userPoolClientType_allowedOAuthFlowsUserPoolClient,
    userPoolClientType_preventUserExistenceErrors,

    -- * UserPoolDescriptionType
    UserPoolDescriptionType (..),
    newUserPoolDescriptionType,
    userPoolDescriptionType_lastModifiedDate,
    userPoolDescriptionType_status,
    userPoolDescriptionType_creationDate,
    userPoolDescriptionType_lambdaConfig,
    userPoolDescriptionType_id,
    userPoolDescriptionType_name,

    -- * UserPoolPolicyType
    UserPoolPolicyType (..),
    newUserPoolPolicyType,
    userPoolPolicyType_passwordPolicy,

    -- * UserPoolType
    UserPoolType (..),
    newUserPoolType,
    userPoolType_lastModifiedDate,
    userPoolType_status,
    userPoolType_userPoolTags,
    userPoolType_usernameAttributes,
    userPoolType_emailVerificationSubject,
    userPoolType_autoVerifiedAttributes,
    userPoolType_policies,
    userPoolType_customDomain,
    userPoolType_deviceConfiguration,
    userPoolType_domain,
    userPoolType_adminCreateUserConfig,
    userPoolType_arn,
    userPoolType_smsConfiguration,
    userPoolType_creationDate,
    userPoolType_lambdaConfig,
    userPoolType_id,
    userPoolType_estimatedNumberOfUsers,
    userPoolType_name,
    userPoolType_smsVerificationMessage,
    userPoolType_accountRecoverySetting,
    userPoolType_emailConfiguration,
    userPoolType_emailConfigurationFailure,
    userPoolType_aliasAttributes,
    userPoolType_usernameConfiguration,
    userPoolType_smsAuthenticationMessage,
    userPoolType_userPoolAddOns,
    userPoolType_schemaAttributes,
    userPoolType_emailVerificationMessage,
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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | This exception is thrown when a password reset is required.
_PasswordResetRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PasswordResetRequiredException =
  Core._MatchServiceError
    defaultService
    "PasswordResetRequiredException"

-- | This exception is thrown when Amazon Cognito encounters a user name that
-- already exists in the user pool.
_UsernameExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UsernameExistsException =
  Core._MatchServiceError
    defaultService
    "UsernameExistsException"

-- | This exception is thrown when an unsupported token is passed to an
-- operation.
_UnsupportedTokenTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedTokenTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedTokenTypeException"

-- | This exception is thrown when the specified scope does not exist.
_ScopeDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ScopeDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ScopeDoesNotExistException"

-- | This exception is thrown if the provided code does not match what the
-- server was expecting.
_CodeMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeMismatchException =
  Core._MatchServiceError
    defaultService
    "CodeMismatchException"

-- | This exception is thrown when a verification code fails to deliver
-- successfully.
_CodeDeliveryFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeDeliveryFailureException =
  Core._MatchServiceError
    defaultService
    "CodeDeliveryFailureException"

-- | This exception is thrown when the request is not authorized. This can
-- happen due to an invalid access token in the request.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"

-- | This exception is thrown when Amazon Cognito encounters an internal
-- error.
_InternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalErrorException =
  Core._MatchServiceError
    defaultService
    "InternalErrorException"

-- | This exception is thrown when a user is not confirmed successfully.
_UserNotConfirmedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotConfirmedException =
  Core._MatchServiceError
    defaultService
    "UserNotConfirmedException"

-- | This exception is thrown when there is a code mismatch and the service
-- fails to configure the software token TOTP multi-factor authentication
-- (MFA).
_EnableSoftwareTokenMFAException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EnableSoftwareTokenMFAException =
  Core._MatchServiceError
    defaultService
    "EnableSoftwareTokenMFAException"

-- | This exception is thrown if a code has expired.
_ExpiredCodeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExpiredCodeException =
  Core._MatchServiceError
    defaultService
    "ExpiredCodeException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- unexpected exception with the Lambda service.
_UnexpectedLambdaException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnexpectedLambdaException =
  Core._MatchServiceError
    defaultService
    "UnexpectedLambdaException"

-- | This exception is thrown when a user is not found.
_UserNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotFoundException =
  Core._MatchServiceError
    defaultService
    "UserNotFoundException"

-- | This exception is thrown when the user has made too many failed attempts
-- for a given action (e.g., sign in).
_TooManyFailedAttemptsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyFailedAttemptsException =
  Core._MatchServiceError
    defaultService
    "TooManyFailedAttemptsException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid Lambda response.
_InvalidLambdaResponseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLambdaResponseException =
  Core._MatchServiceError
    defaultService
    "InvalidLambdaResponseException"

-- | This exception is thrown when the software token TOTP multi-factor
-- authentication (MFA) is not enabled for the user pool.
_SoftwareTokenMFANotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SoftwareTokenMFANotFoundException =
  Core._MatchServiceError
    defaultService
    "SoftwareTokenMFANotFoundException"

-- | This exception is thrown if two or more modifications are happening
-- concurrently.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

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

-- | This exception is thrown when the trust relationship is invalid for the
-- role provided for SMS configuration. This can happen if you do not trust
-- @cognito-idp.amazonaws.com@ or the external ID provided in the role does
-- not match what is provided in the SMS configuration for the user pool.
_InvalidSmsRoleTrustRelationshipException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSmsRoleTrustRelationshipException =
  Core._MatchServiceError
    defaultService
    "InvalidSmsRoleTrustRelationshipException"

-- | This exception is thrown when you attempt to perform an operation that
-- is not enabled for the user pool client.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | This exception is thrown when a user pool tag cannot be set or updated.
_UserPoolTaggingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserPoolTaggingException =
  Core._MatchServiceError
    defaultService
    "UserPoolTaggingException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid parameter.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The request failed because the user is in an unsupported state.
_UnsupportedUserStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedUserStateException =
  Core._MatchServiceError
    defaultService
    "UnsupportedUserStateException"

-- | This exception is thrown when the Amazon Cognito service encounters an
-- invalid password.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"

-- | This exception is thrown when Amazon Cognito cannot find a multi-factor
-- authentication (MFA) method.
_MFAMethodNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MFAMethodNotFoundException =
  Core._MatchServiceError
    defaultService
    "MFAMethodNotFoundException"

-- | This exception is thrown when you are trying to modify a user pool while
-- a user import job is in progress for that pool.
_UserImportInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserImportInProgressException =
  Core._MatchServiceError
    defaultService
    "UserImportInProgressException"

-- | This exception is thrown when a user tries to confirm the account with
-- an email or phone number that has already been supplied as an alias from
-- a different account. This exception tells user that an account with this
-- email or phone already exists.
_AliasExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AliasExistsException =
  Core._MatchServiceError
    defaultService
    "AliasExistsException"

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

-- | This exception is thrown when the Amazon Cognito service cannot find the
-- requested resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | This exception is thrown when the specified OAuth flow is invalid.
_InvalidOAuthFlowException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOAuthFlowException =
  Core._MatchServiceError
    defaultService
    "InvalidOAuthFlowException"

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

-- | This exception is thrown when Amazon Cognito encounters a group that
-- already exists in the user pool.
_GroupExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GroupExistsException =
  Core._MatchServiceError
    defaultService
    "GroupExistsException"

-- | This exception is thrown when a user is not authorized.
_NotAuthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotAuthorizedException =
  Core._MatchServiceError
    defaultService
    "NotAuthorizedException"

-- | This exception is thrown when the specified identifier is not supported.
_UnsupportedIdentityProviderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedIdentityProviderException =
  Core._MatchServiceError
    defaultService
    "UnsupportedIdentityProviderException"

-- | This exception is thrown when Amazon Cognito is not allowed to use your
-- email identity. HTTP status code: 400.
_InvalidEmailRoleAccessPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEmailRoleAccessPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidEmailRoleAccessPolicyException"

-- | This exception is thrown when user pool add-ons are not enabled.
_UserPoolAddOnNotEnabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserPoolAddOnNotEnabledException =
  Core._MatchServiceError
    defaultService
    "UserPoolAddOnNotEnabledException"

-- | This exception is thrown when the user has made too many requests for a
-- given operation.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
