{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types
  ( -- * Service Configuration
    cognitoIdentityProvider,

    -- * Errors

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

    -- * UserPoolMFAType
    UserPoolMFAType (..),

    -- * UserStatusType
    UserStatusType (..),

    -- * UsernameAttributeType
    UsernameAttributeType (..),

    -- * VerifiedAttributeType
    VerifiedAttributeType (..),

    -- * VerifySoftwareTokenResponseType
    VerifySoftwareTokenResponseType (..),

    -- * AccountRecoverySettingType
    AccountRecoverySettingType,
    accountRecoverySettingType,
    arstRecoveryMechanisms,

    -- * AccountTakeoverActionType
    AccountTakeoverActionType,
    accountTakeoverActionType,
    atatNotify,
    atatEventAction,

    -- * AccountTakeoverActionsType
    AccountTakeoverActionsType,
    accountTakeoverActionsType,
    atatLowAction,
    atatHighAction,
    atatMediumAction,

    -- * AccountTakeoverRiskConfigurationType
    AccountTakeoverRiskConfigurationType,
    accountTakeoverRiskConfigurationType,
    atrctNotifyConfiguration,
    atrctActions,

    -- * AdminCreateUserConfigType
    AdminCreateUserConfigType,
    adminCreateUserConfigType,
    acuctAllowAdminCreateUserOnly,
    acuctUnusedAccountValidityDays,
    acuctInviteMessageTemplate,

    -- * AnalyticsConfigurationType
    AnalyticsConfigurationType,
    analyticsConfigurationType,
    actApplicationARN,
    actUserDataShared,
    actApplicationId,
    actExternalId,
    actRoleARN,

    -- * AnalyticsMetadataType
    AnalyticsMetadataType,
    analyticsMetadataType,
    amtAnalyticsEndpointId,

    -- * AttributeType
    AttributeType,
    attributeType,
    atValue,
    atName,

    -- * AuthEventType
    AuthEventType,
    authEventType,
    aetEventRisk,
    aetEventResponse,
    aetEventContextData,
    aetChallengeResponses,
    aetEventType,
    aetCreationDate,
    aetEventFeedback,
    aetEventId,

    -- * AuthenticationResultType
    AuthenticationResultType,
    authenticationResultType,
    artAccessToken,
    artRefreshToken,
    artNewDeviceMetadata,
    artExpiresIn,
    artTokenType,
    artIdToken,

    -- * ChallengeResponseType
    ChallengeResponseType,
    challengeResponseType,
    crtChallengeName,
    crtChallengeResponse,

    -- * CodeDeliveryDetailsType
    CodeDeliveryDetailsType,
    codeDeliveryDetailsType,
    cddtDestination,
    cddtDeliveryMedium,
    cddtAttributeName,

    -- * CompromisedCredentialsActionsType
    CompromisedCredentialsActionsType,
    compromisedCredentialsActionsType,
    ccatEventAction,

    -- * CompromisedCredentialsRiskConfigurationType
    CompromisedCredentialsRiskConfigurationType,
    compromisedCredentialsRiskConfigurationType,
    ccrctEventFilter,
    ccrctActions,

    -- * ContextDataType
    ContextDataType,
    contextDataType,
    cdtEncodedData,
    cdtIPAddress,
    cdtServerName,
    cdtServerPath,
    cdtHTTPHeaders,

    -- * CustomDomainConfigType
    CustomDomainConfigType,
    customDomainConfigType,
    cdctCertificateARN,

    -- * CustomEmailLambdaVersionConfigType
    CustomEmailLambdaVersionConfigType,
    customEmailLambdaVersionConfigType,
    celvctLambdaVersion,
    celvctLambdaARN,

    -- * CustomSMSLambdaVersionConfigType
    CustomSMSLambdaVersionConfigType,
    customSMSLambdaVersionConfigType,
    csmslvctLambdaVersion,
    csmslvctLambdaARN,

    -- * DeviceConfigurationType
    DeviceConfigurationType,
    deviceConfigurationType,
    dctChallengeRequiredOnNewDevice,
    dctDeviceOnlyRememberedOnUserPrompt,

    -- * DeviceSecretVerifierConfigType
    DeviceSecretVerifierConfigType,
    deviceSecretVerifierConfigType,
    dsvctPasswordVerifier,
    dsvctSalt,

    -- * DeviceType
    DeviceType,
    deviceType,
    dtDeviceLastModifiedDate,
    dtDeviceCreateDate,
    dtDeviceAttributes,
    dtDeviceKey,
    dtDeviceLastAuthenticatedDate,

    -- * DomainDescriptionType
    DomainDescriptionType,
    domainDescriptionType,
    ddtStatus,
    ddtCloudFrontDistribution,
    ddtUserPoolId,
    ddtDomain,
    ddtAWSAccountId,
    ddtCustomDomainConfig,
    ddtVersion,
    ddtS3Bucket,

    -- * EmailConfigurationType
    EmailConfigurationType,
    emailConfigurationType,
    ectSourceARN,
    ectFrom,
    ectConfigurationSet,
    ectReplyToEmailAddress,
    ectEmailSendingAccount,

    -- * EventContextDataType
    EventContextDataType,
    eventContextDataType,
    ecdtIPAddress,
    ecdtCountry,
    ecdtCity,
    ecdtDeviceName,
    ecdtTimezone,

    -- * EventFeedbackType
    EventFeedbackType,
    eventFeedbackType,
    eftFeedbackDate,
    eftFeedbackValue,
    eftProvider,

    -- * EventRiskType
    EventRiskType,
    eventRiskType,
    ertCompromisedCredentialsDetected,
    ertRiskLevel,
    ertRiskDecision,

    -- * GroupType
    GroupType,
    groupType,
    gtLastModifiedDate,
    gtUserPoolId,
    gtCreationDate,
    gtPrecedence,
    gtGroupName,
    gtDescription,
    gtRoleARN,

    -- * HTTPHeader
    HTTPHeader,
    hTTPHeader,
    httphHeaderValue,
    httphHeaderName,

    -- * IdentityProviderType
    IdentityProviderType,
    identityProviderType,
    iptLastModifiedDate,
    iptUserPoolId,
    iptProviderType,
    iptCreationDate,
    iptIdpIdentifiers,
    iptAttributeMapping,
    iptProviderDetails,
    iptProviderName,

    -- * LambdaConfigType
    LambdaConfigType,
    lambdaConfigType,
    lctPreAuthentication,
    lctCreateAuthChallenge,
    lctVerifyAuthChallengeResponse,
    lctCustomSMSSender,
    lctPostAuthentication,
    lctCustomMessage,
    lctDefineAuthChallenge,
    lctCustomEmailSender,
    lctKMSKeyId,
    lctPostConfirmation,
    lctPreTokenGeneration,
    lctUserMigration,
    lctPreSignUp,

    -- * MFAOptionType
    MFAOptionType,
    mfaOptionType,
    motDeliveryMedium,
    motAttributeName,

    -- * MessageTemplateType
    MessageTemplateType,
    messageTemplateType,
    mttEmailSubject,
    mttSMSMessage,
    mttEmailMessage,

    -- * NewDeviceMetadataType
    NewDeviceMetadataType,
    newDeviceMetadataType,
    ndmtDeviceGroupKey,
    ndmtDeviceKey,

    -- * NotifyConfigurationType
    NotifyConfigurationType,
    notifyConfigurationType,
    nctNoActionEmail,
    nctFrom,
    nctReplyTo,
    nctBlockEmail,
    nctMFAEmail,
    nctSourceARN,

    -- * NotifyEmailType
    NotifyEmailType,
    notifyEmailType,
    netTextBody,
    netHTMLBody,
    netSubject,

    -- * NumberAttributeConstraintsType
    NumberAttributeConstraintsType,
    numberAttributeConstraintsType,
    nactMaxValue,
    nactMinValue,

    -- * PasswordPolicyType
    PasswordPolicyType,
    passwordPolicyType,
    pptRequireNumbers,
    pptRequireUppercase,
    pptRequireLowercase,
    pptMinimumLength,
    pptRequireSymbols,
    pptTemporaryPasswordValidityDays,

    -- * ProviderDescription
    ProviderDescription,
    providerDescription,
    pdLastModifiedDate,
    pdProviderType,
    pdCreationDate,
    pdProviderName,

    -- * ProviderUserIdentifierType
    ProviderUserIdentifierType,
    providerUserIdentifierType,
    puitProviderAttributeValue,
    puitProviderAttributeName,
    puitProviderName,

    -- * RecoveryOptionType
    RecoveryOptionType,
    recoveryOptionType,
    rotPriority,
    rotName,

    -- * ResourceServerScopeType
    ResourceServerScopeType,
    resourceServerScopeType,
    rsstScopeName,
    rsstScopeDescription,

    -- * ResourceServerType
    ResourceServerType,
    resourceServerType,
    rstUserPoolId,
    rstIdentifier,
    rstScopes,
    rstName,

    -- * RiskConfigurationType
    RiskConfigurationType,
    riskConfigurationType,
    rctRiskExceptionConfiguration,
    rctClientId,
    rctAccountTakeoverRiskConfiguration,
    rctLastModifiedDate,
    rctUserPoolId,
    rctCompromisedCredentialsRiskConfiguration,

    -- * RiskExceptionConfigurationType
    RiskExceptionConfigurationType,
    riskExceptionConfigurationType,
    rectSkippedIPRangeList,
    rectBlockedIPRangeList,

    -- * SMSMFASettingsType
    SMSMFASettingsType,
    sMSMFASettingsType,
    smsmstEnabled,
    smsmstPreferredMFA,

    -- * SchemaAttributeType
    SchemaAttributeType,
    schemaAttributeType,
    satNumberAttributeConstraints,
    satRequired,
    satAttributeDataType,
    satStringAttributeConstraints,
    satName,
    satDeveloperOnlyAttribute,
    satMutable,

    -- * SmsConfigurationType
    SmsConfigurationType,
    smsConfigurationType,
    sctExternalId,
    sctSNSCallerARN,

    -- * SmsMFAConfigType
    SmsMFAConfigType,
    smsMFAConfigType,
    smctSmsAuthenticationMessage,
    smctSmsConfiguration,

    -- * SoftwareTokenMFAConfigType
    SoftwareTokenMFAConfigType,
    softwareTokenMFAConfigType,
    stmctEnabled,

    -- * SoftwareTokenMFASettingsType
    SoftwareTokenMFASettingsType,
    softwareTokenMFASettingsType,
    stmstEnabled,
    stmstPreferredMFA,

    -- * StringAttributeConstraintsType
    StringAttributeConstraintsType,
    stringAttributeConstraintsType,
    sactMaxLength,
    sactMinLength,

    -- * TokenValidityUnitsType
    TokenValidityUnitsType,
    tokenValidityUnitsType,
    tvutAccessToken,
    tvutRefreshToken,
    tvutIdToken,

    -- * UICustomizationType
    UICustomizationType,
    uICustomizationType,
    uictClientId,
    uictLastModifiedDate,
    uictUserPoolId,
    uictCSS,
    uictCSSVersion,
    uictImageURL,
    uictCreationDate,

    -- * UserContextDataType
    UserContextDataType,
    userContextDataType,
    ucdtEncodedData,

    -- * UserImportJobType
    UserImportJobType,
    userImportJobType,
    uijtStatus,
    uijtSkippedUsers,
    uijtJobId,
    uijtUserPoolId,
    uijtJobName,
    uijtPreSignedURL,
    uijtFailedUsers,
    uijtStartDate,
    uijtCompletionMessage,
    uijtCreationDate,
    uijtCompletionDate,
    uijtCloudWatchLogsRoleARN,
    uijtImportedUsers,

    -- * UserPoolAddOnsType
    UserPoolAddOnsType,
    userPoolAddOnsType,
    upaotAdvancedSecurityMode,

    -- * UserPoolClientDescription
    UserPoolClientDescription,
    userPoolClientDescription,
    upcdClientId,
    upcdUserPoolId,
    upcdClientName,

    -- * UserPoolClientType
    UserPoolClientType,
    userPoolClientType,
    upctRefreshTokenValidity,
    upctClientId,
    upctExplicitAuthFlows,
    upctClientSecret,
    upctLastModifiedDate,
    upctSupportedIdentityProviders,
    upctLogoutURLs,
    upctAllowedOAuthFlowsUserPoolClient,
    upctUserPoolId,
    upctIdTokenValidity,
    upctTokenValidityUnits,
    upctDefaultRedirectURI,
    upctWriteAttributes,
    upctPreventUserExistenceErrors,
    upctAccessTokenValidity,
    upctCreationDate,
    upctReadAttributes,
    upctAllowedOAuthScopes,
    upctAllowedOAuthFlows,
    upctAnalyticsConfiguration,
    upctClientName,
    upctCallbackURLs,

    -- * UserPoolDescriptionType
    UserPoolDescriptionType,
    userPoolDescriptionType,
    updtStatus,
    updtLastModifiedDate,
    updtName,
    updtId,
    updtCreationDate,
    updtLambdaConfig,

    -- * UserPoolPolicyType
    UserPoolPolicyType,
    userPoolPolicyType,
    upptPasswordPolicy,

    -- * UserPoolType
    UserPoolType,
    userPoolType,
    uptStatus,
    uptUserPoolTags,
    uptEmailConfigurationFailure,
    uptLastModifiedDate,
    uptVerificationMessageTemplate,
    uptEstimatedNumberOfUsers,
    uptARN,
    uptDomain,
    uptCustomDomain,
    uptEmailVerificationMessage,
    uptSmsAuthenticationMessage,
    uptUserPoolAddOns,
    uptSchemaAttributes,
    uptEmailVerificationSubject,
    uptUsernameAttributes,
    uptAliasAttributes,
    uptAccountRecoverySetting,
    uptEmailConfiguration,
    uptSmsVerificationMessage,
    uptName,
    uptMFAConfiguration,
    uptId,
    uptSmsConfigurationFailure,
    uptCreationDate,
    uptLambdaConfig,
    uptSmsConfiguration,
    uptAdminCreateUserConfig,
    uptDeviceConfiguration,
    uptAutoVerifiedAttributes,
    uptPolicies,
    uptUsernameConfiguration,

    -- * UserType
    UserType,
    userType,
    utEnabled,
    utUserStatus,
    utUsername,
    utUserCreateDate,
    utAttributes,
    utMFAOptions,
    utUserLastModifiedDate,

    -- * UsernameConfigurationType
    UsernameConfigurationType,
    usernameConfigurationType,
    uctCaseSensitive,

    -- * VerificationMessageTemplateType
    VerificationMessageTemplateType,
    verificationMessageTemplateType,
    vmttDefaultEmailOption,
    vmttEmailSubject,
    vmttEmailSubjectByLink,
    vmttSmsMessage,
    vmttEmailMessageByLink,
    vmttEmailMessage,
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
import Network.AWS.CognitoIdentityProvider.Types.HTTPHeader
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
import Network.AWS.CognitoIdentityProvider.Types.SMSMFASettingsType
import Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
import Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.SmsMFAConfigType
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFAConfigType
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFASettingsType
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
import Network.AWS.CognitoIdentityProvider.Types.UserPoolMFAType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolType
import Network.AWS.CognitoIdentityProvider.Types.UserStatusType
import Network.AWS.CognitoIdentityProvider.Types.UserType
import Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
import Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
import Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
import Network.AWS.CognitoIdentityProvider.Types.VerifySoftwareTokenResponseType
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-04-18@ of the Amazon Cognito Identity Provider SDK configuration.
cognitoIdentityProvider :: Service
cognitoIdentityProvider =
  Service
    { _svcAbbrev = "CognitoIdentityProvider",
      _svcSigner = v4,
      _svcPrefix = "cognito-idp",
      _svcVersion = "2016-04-18",
      _svcEndpoint = defaultEndpoint cognitoIdentityProvider,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CognitoIdentityProvider",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
