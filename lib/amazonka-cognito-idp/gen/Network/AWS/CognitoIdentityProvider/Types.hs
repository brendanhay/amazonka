-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types
  ( -- * Service configuration
    cognitoIdentityProviderService,

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
    AccountRecoverySettingType (..),
    mkAccountRecoverySettingType,
    arstRecoveryMechanisms,

    -- * AccountTakeoverActionType
    AccountTakeoverActionType (..),
    mkAccountTakeoverActionType,
    atatEventAction,
    atatNotify,

    -- * AccountTakeoverActionsType
    AccountTakeoverActionsType (..),
    mkAccountTakeoverActionsType,
    atatLowAction,
    atatHighAction,
    atatMediumAction,

    -- * AccountTakeoverRiskConfigurationType
    AccountTakeoverRiskConfigurationType (..),
    mkAccountTakeoverRiskConfigurationType,
    atrctActions,
    atrctNotifyConfiguration,

    -- * AdminCreateUserConfigType
    AdminCreateUserConfigType (..),
    mkAdminCreateUserConfigType,
    acuctAllowAdminCreateUserOnly,
    acuctUnusedAccountValidityDays,
    acuctInviteMessageTemplate,

    -- * AnalyticsConfigurationType
    AnalyticsConfigurationType (..),
    mkAnalyticsConfigurationType,
    actApplicationARN,
    actUserDataShared,
    actApplicationId,
    actExternalId,
    actRoleARN,

    -- * AnalyticsMetadataType
    AnalyticsMetadataType (..),
    mkAnalyticsMetadataType,
    amtAnalyticsEndpointId,

    -- * AttributeType
    AttributeType (..),
    mkAttributeType,
    atValue,
    atName,

    -- * AuthEventType
    AuthEventType (..),
    mkAuthEventType,
    aetEventRisk,
    aetEventResponse,
    aetEventContextData,
    aetChallengeResponses,
    aetEventType,
    aetCreationDate,
    aetEventFeedback,
    aetEventId,

    -- * AuthenticationResultType
    AuthenticationResultType (..),
    mkAuthenticationResultType,
    artAccessToken,
    artRefreshToken,
    artNewDeviceMetadata,
    artExpiresIn,
    artTokenType,
    artIdToken,

    -- * ChallengeResponseType
    ChallengeResponseType (..),
    mkChallengeResponseType,
    crtChallengeName,
    crtChallengeResponse,

    -- * CodeDeliveryDetailsType
    CodeDeliveryDetailsType (..),
    mkCodeDeliveryDetailsType,
    cddtDestination,
    cddtDeliveryMedium,
    cddtAttributeName,

    -- * CompromisedCredentialsActionsType
    CompromisedCredentialsActionsType (..),
    mkCompromisedCredentialsActionsType,
    ccatEventAction,

    -- * CompromisedCredentialsRiskConfigurationType
    CompromisedCredentialsRiskConfigurationType (..),
    mkCompromisedCredentialsRiskConfigurationType,
    ccrctActions,
    ccrctEventFilter,

    -- * ContextDataType
    ContextDataType (..),
    mkContextDataType,
    cdtHTTPHeaders,
    cdtIPAddress,
    cdtServerName,
    cdtEncodedData,
    cdtServerPath,

    -- * CustomDomainConfigType
    CustomDomainConfigType (..),
    mkCustomDomainConfigType,
    cdctCertificateARN,

    -- * CustomEmailLambdaVersionConfigType
    CustomEmailLambdaVersionConfigType (..),
    mkCustomEmailLambdaVersionConfigType,
    celvctLambdaARN,
    celvctLambdaVersion,

    -- * CustomSMSLambdaVersionConfigType
    CustomSMSLambdaVersionConfigType (..),
    mkCustomSMSLambdaVersionConfigType,
    csmslvctLambdaARN,
    csmslvctLambdaVersion,

    -- * DeviceConfigurationType
    DeviceConfigurationType (..),
    mkDeviceConfigurationType,
    dctChallengeRequiredOnNewDevice,
    dctDeviceOnlyRememberedOnUserPrompt,

    -- * DeviceSecretVerifierConfigType
    DeviceSecretVerifierConfigType (..),
    mkDeviceSecretVerifierConfigType,
    dsvctPasswordVerifier,
    dsvctSalt,

    -- * DeviceType
    DeviceType (..),
    mkDeviceType,
    dtDeviceLastModifiedDate,
    dtDeviceCreateDate,
    dtDeviceAttributes,
    dtDeviceKey,
    dtDeviceLastAuthenticatedDate,

    -- * DomainDescriptionType
    DomainDescriptionType (..),
    mkDomainDescriptionType,
    ddtStatus,
    ddtCloudFrontDistribution,
    ddtUserPoolId,
    ddtDomain,
    ddtAWSAccountId,
    ddtCustomDomainConfig,
    ddtVersion,
    ddtS3Bucket,

    -- * EmailConfigurationType
    EmailConfigurationType (..),
    mkEmailConfigurationType,
    ectSourceARN,
    ectFrom,
    ectConfigurationSet,
    ectReplyToEmailAddress,
    ectEmailSendingAccount,

    -- * EventContextDataType
    EventContextDataType (..),
    mkEventContextDataType,
    ecdtIPAddress,
    ecdtCountry,
    ecdtCity,
    ecdtDeviceName,
    ecdtTimezone,

    -- * EventFeedbackType
    EventFeedbackType (..),
    mkEventFeedbackType,
    eftFeedbackValue,
    eftFeedbackDate,
    eftProvider,

    -- * EventRiskType
    EventRiskType (..),
    mkEventRiskType,
    ertCompromisedCredentialsDetected,
    ertRiskLevel,
    ertRiskDecision,

    -- * GroupType
    GroupType (..),
    mkGroupType,
    gtLastModifiedDate,
    gtUserPoolId,
    gtCreationDate,
    gtPrecedence,
    gtGroupName,
    gtDescription,
    gtRoleARN,

    -- * HTTPHeader
    HTTPHeader (..),
    mkHTTPHeader,
    httphHeaderValue,
    httphHeaderName,

    -- * IdentityProviderType
    IdentityProviderType (..),
    mkIdentityProviderType,
    iptLastModifiedDate,
    iptUserPoolId,
    iptProviderType,
    iptCreationDate,
    iptIdpIdentifiers,
    iptAttributeMapping,
    iptProviderDetails,
    iptProviderName,

    -- * LambdaConfigType
    LambdaConfigType (..),
    mkLambdaConfigType,
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
    MFAOptionType (..),
    mkMFAOptionType,
    motDeliveryMedium,
    motAttributeName,

    -- * MessageTemplateType
    MessageTemplateType (..),
    mkMessageTemplateType,
    mttEmailSubject,
    mttSMSMessage,
    mttEmailMessage,

    -- * NewDeviceMetadataType
    NewDeviceMetadataType (..),
    mkNewDeviceMetadataType,
    ndmtDeviceGroupKey,
    ndmtDeviceKey,

    -- * NotifyConfigurationType
    NotifyConfigurationType (..),
    mkNotifyConfigurationType,
    nctSourceARN,
    nctNoActionEmail,
    nctFrom,
    nctReplyTo,
    nctBlockEmail,
    nctMFAEmail,

    -- * NotifyEmailType
    NotifyEmailType (..),
    mkNotifyEmailType,
    netSubject,
    netTextBody,
    netHTMLBody,

    -- * NumberAttributeConstraintsType
    NumberAttributeConstraintsType (..),
    mkNumberAttributeConstraintsType,
    nactMaxValue,
    nactMinValue,

    -- * PasswordPolicyType
    PasswordPolicyType (..),
    mkPasswordPolicyType,
    pptRequireNumbers,
    pptRequireUppercase,
    pptRequireLowercase,
    pptMinimumLength,
    pptRequireSymbols,
    pptTemporaryPasswordValidityDays,

    -- * ProviderDescription
    ProviderDescription (..),
    mkProviderDescription,
    pdLastModifiedDate,
    pdProviderType,
    pdCreationDate,
    pdProviderName,

    -- * ProviderUserIdentifierType
    ProviderUserIdentifierType (..),
    mkProviderUserIdentifierType,
    puitProviderAttributeValue,
    puitProviderAttributeName,
    puitProviderName,

    -- * RecoveryOptionType
    RecoveryOptionType (..),
    mkRecoveryOptionType,
    rotPriority,
    rotName,

    -- * ResourceServerScopeType
    ResourceServerScopeType (..),
    mkResourceServerScopeType,
    rsstScopeName,
    rsstScopeDescription,

    -- * ResourceServerType
    ResourceServerType (..),
    mkResourceServerType,
    rstUserPoolId,
    rstIdentifier,
    rstScopes,
    rstName,

    -- * RiskConfigurationType
    RiskConfigurationType (..),
    mkRiskConfigurationType,
    rctRiskExceptionConfiguration,
    rctClientId,
    rctAccountTakeoverRiskConfiguration,
    rctLastModifiedDate,
    rctUserPoolId,
    rctCompromisedCredentialsRiskConfiguration,

    -- * RiskExceptionConfigurationType
    RiskExceptionConfigurationType (..),
    mkRiskExceptionConfigurationType,
    rectSkippedIPRangeList,
    rectBlockedIPRangeList,

    -- * SMSMFASettingsType
    SMSMFASettingsType (..),
    mkSMSMFASettingsType,
    smsmstEnabled,
    smsmstPreferredMFA,

    -- * SchemaAttributeType
    SchemaAttributeType (..),
    mkSchemaAttributeType,
    satNumberAttributeConstraints,
    satRequired,
    satAttributeDataType,
    satStringAttributeConstraints,
    satName,
    satDeveloperOnlyAttribute,
    satMutable,

    -- * SmsConfigurationType
    SmsConfigurationType (..),
    mkSmsConfigurationType,
    sctSNSCallerARN,
    sctExternalId,

    -- * SmsMFAConfigType
    SmsMFAConfigType (..),
    mkSmsMFAConfigType,
    smctSmsAuthenticationMessage,
    smctSmsConfiguration,

    -- * SoftwareTokenMFAConfigType
    SoftwareTokenMFAConfigType (..),
    mkSoftwareTokenMFAConfigType,
    stmctEnabled,

    -- * SoftwareTokenMFASettingsType
    SoftwareTokenMFASettingsType (..),
    mkSoftwareTokenMFASettingsType,
    stmstEnabled,
    stmstPreferredMFA,

    -- * StringAttributeConstraintsType
    StringAttributeConstraintsType (..),
    mkStringAttributeConstraintsType,
    sactMaxLength,
    sactMinLength,

    -- * TokenValidityUnitsType
    TokenValidityUnitsType (..),
    mkTokenValidityUnitsType,
    tvutAccessToken,
    tvutRefreshToken,
    tvutIdToken,

    -- * UICustomizationType
    UICustomizationType (..),
    mkUICustomizationType,
    uictClientId,
    uictLastModifiedDate,
    uictUserPoolId,
    uictCSS,
    uictCSSVersion,
    uictImageURL,
    uictCreationDate,

    -- * UserContextDataType
    UserContextDataType (..),
    mkUserContextDataType,
    ucdtEncodedData,

    -- * UserImportJobType
    UserImportJobType (..),
    mkUserImportJobType,
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
    UserPoolAddOnsType (..),
    mkUserPoolAddOnsType,
    upaotAdvancedSecurityMode,

    -- * UserPoolClientDescription
    UserPoolClientDescription (..),
    mkUserPoolClientDescription,
    upcdClientId,
    upcdUserPoolId,
    upcdClientName,

    -- * UserPoolClientType
    UserPoolClientType (..),
    mkUserPoolClientType,
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
    UserPoolDescriptionType (..),
    mkUserPoolDescriptionType,
    updtStatus,
    updtLastModifiedDate,
    updtName,
    updtId,
    updtCreationDate,
    updtLambdaConfig,

    -- * UserPoolPolicyType
    UserPoolPolicyType (..),
    mkUserPoolPolicyType,
    upptPasswordPolicy,

    -- * UserPoolType
    UserPoolType (..),
    mkUserPoolType,
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
    UserType (..),
    mkUserType,
    utEnabled,
    utUserStatus,
    utUsername,
    utUserCreateDate,
    utAttributes,
    utMFAOptions,
    utUserLastModifiedDate,

    -- * UsernameConfigurationType
    UsernameConfigurationType (..),
    mkUsernameConfigurationType,
    uctCaseSensitive,

    -- * VerificationMessageTemplateType
    VerificationMessageTemplateType (..),
    mkVerificationMessageTemplateType,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-04-18@ of the Amazon Cognito Identity Provider SDK configuration.
cognitoIdentityProviderService :: Lude.Service
cognitoIdentityProviderService =
  Lude.Service
    { Lude._svcAbbrev = "CognitoIdentityProvider",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cognito-idp",
      Lude._svcVersion = "2016-04-18",
      Lude._svcEndpoint =
        Lude.defaultEndpoint cognitoIdentityProviderService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CognitoIdentityProvider",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
