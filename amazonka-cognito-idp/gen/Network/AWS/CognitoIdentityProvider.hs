{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Using the Amazon Cognito User Pools API, you can create a user pool to
-- manage directories and users. You can authenticate a user to obtain
-- tokens related to user identity and access policies.
--
-- This API reference provides information about user pools in Amazon
-- Cognito User Pools.
--
-- For more information, see the Amazon Cognito Documentation.
module Network.AWS.CognitoIdentityProvider
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** PasswordResetRequiredException
    _PasswordResetRequiredException,

    -- ** UsernameExistsException
    _UsernameExistsException,

    -- ** CodeDeliveryFailureException
    _CodeDeliveryFailureException,

    -- ** ScopeDoesNotExistException
    _ScopeDoesNotExistException,

    -- ** CodeMismatchException
    _CodeMismatchException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** EnableSoftwareTokenMFAException
    _EnableSoftwareTokenMFAException,

    -- ** UserNotConfirmedException
    _UserNotConfirmedException,

    -- ** ExpiredCodeException
    _ExpiredCodeException,

    -- ** TooManyFailedAttemptsException
    _TooManyFailedAttemptsException,

    -- ** UnexpectedLambdaException
    _UnexpectedLambdaException,

    -- ** UserNotFoundException
    _UserNotFoundException,

    -- ** InvalidLambdaResponseException
    _InvalidLambdaResponseException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** UserLambdaValidationException
    _UserLambdaValidationException,

    -- ** SoftwareTokenMFANotFoundException
    _SoftwareTokenMFANotFoundException,

    -- ** PreconditionNotMetException
    _PreconditionNotMetException,

    -- ** UserPoolTaggingException
    _UserPoolTaggingException,

    -- ** InvalidSmsRoleTrustRelationshipException
    _InvalidSmsRoleTrustRelationshipException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** UnsupportedUserStateException
    _UnsupportedUserStateException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InvalidPasswordException
    _InvalidPasswordException,

    -- ** MFAMethodNotFoundException
    _MFAMethodNotFoundException,

    -- ** AliasExistsException
    _AliasExistsException,

    -- ** DuplicateProviderException
    _DuplicateProviderException,

    -- ** UserImportInProgressException
    _UserImportInProgressException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidSmsRoleAccessPolicyException
    _InvalidSmsRoleAccessPolicyException,

    -- ** InvalidUserPoolConfigurationException
    _InvalidUserPoolConfigurationException,

    -- ** InvalidOAuthFlowException
    _InvalidOAuthFlowException,

    -- ** GroupExistsException
    _GroupExistsException,

    -- ** UnsupportedIdentityProviderException
    _UnsupportedIdentityProviderException,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InvalidEmailRoleAccessPolicyException
    _InvalidEmailRoleAccessPolicyException,

    -- ** UserPoolAddOnNotEnabledException
    _UserPoolAddOnNotEnabledException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetUserAttributeVerificationCode
    GetUserAttributeVerificationCode (GetUserAttributeVerificationCode'),
    newGetUserAttributeVerificationCode,
    GetUserAttributeVerificationCodeResponse (GetUserAttributeVerificationCodeResponse'),
    newGetUserAttributeVerificationCodeResponse,

    -- ** AdminDeleteUser
    AdminDeleteUser (AdminDeleteUser'),
    newAdminDeleteUser,
    AdminDeleteUserResponse (AdminDeleteUserResponse'),
    newAdminDeleteUserResponse,

    -- ** CreateUserImportJob
    CreateUserImportJob (CreateUserImportJob'),
    newCreateUserImportJob,
    CreateUserImportJobResponse (CreateUserImportJobResponse'),
    newCreateUserImportJobResponse,

    -- ** GetUser
    GetUser (GetUser'),
    newGetUser,
    GetUserResponse (GetUserResponse'),
    newGetUserResponse,

    -- ** SetUserPoolMfaConfig
    SetUserPoolMfaConfig (SetUserPoolMfaConfig'),
    newSetUserPoolMfaConfig,
    SetUserPoolMfaConfigResponse (SetUserPoolMfaConfigResponse'),
    newSetUserPoolMfaConfigResponse,

    -- ** UpdateUserAttributes
    UpdateUserAttributes (UpdateUserAttributes'),
    newUpdateUserAttributes,
    UpdateUserAttributesResponse (UpdateUserAttributesResponse'),
    newUpdateUserAttributesResponse,

    -- ** DeleteUserAttributes
    DeleteUserAttributes (DeleteUserAttributes'),
    newDeleteUserAttributes,
    DeleteUserAttributesResponse (DeleteUserAttributesResponse'),
    newDeleteUserAttributesResponse,

    -- ** UpdateAuthEventFeedback
    UpdateAuthEventFeedback (UpdateAuthEventFeedback'),
    newUpdateAuthEventFeedback,
    UpdateAuthEventFeedbackResponse (UpdateAuthEventFeedbackResponse'),
    newUpdateAuthEventFeedbackResponse,

    -- ** ListUserPools (Paginated)
    ListUserPools (ListUserPools'),
    newListUserPools,
    ListUserPoolsResponse (ListUserPoolsResponse'),
    newListUserPoolsResponse,

    -- ** ConfirmSignUp
    ConfirmSignUp (ConfirmSignUp'),
    newConfirmSignUp,
    ConfirmSignUpResponse (ConfirmSignUpResponse'),
    newConfirmSignUpResponse,

    -- ** AdminLinkProviderForUser
    AdminLinkProviderForUser (AdminLinkProviderForUser'),
    newAdminLinkProviderForUser,
    AdminLinkProviderForUserResponse (AdminLinkProviderForUserResponse'),
    newAdminLinkProviderForUserResponse,

    -- ** UpdateUserPool
    UpdateUserPool (UpdateUserPool'),
    newUpdateUserPool,
    UpdateUserPoolResponse (UpdateUserPoolResponse'),
    newUpdateUserPoolResponse,

    -- ** DeleteUserPool
    DeleteUserPool (DeleteUserPool'),
    newDeleteUserPool,
    DeleteUserPoolResponse (DeleteUserPoolResponse'),
    newDeleteUserPoolResponse,

    -- ** UpdateUserPoolDomain
    UpdateUserPoolDomain (UpdateUserPoolDomain'),
    newUpdateUserPoolDomain,
    UpdateUserPoolDomainResponse (UpdateUserPoolDomainResponse'),
    newUpdateUserPoolDomainResponse,

    -- ** DeleteUserPoolDomain
    DeleteUserPoolDomain (DeleteUserPoolDomain'),
    newDeleteUserPoolDomain,
    DeleteUserPoolDomainResponse (DeleteUserPoolDomainResponse'),
    newDeleteUserPoolDomainResponse,

    -- ** CreateUserPoolDomain
    CreateUserPoolDomain (CreateUserPoolDomain'),
    newCreateUserPoolDomain,
    CreateUserPoolDomainResponse (CreateUserPoolDomainResponse'),
    newCreateUserPoolDomainResponse,

    -- ** ChangePassword
    ChangePassword (ChangePassword'),
    newChangePassword,
    ChangePasswordResponse (ChangePasswordResponse'),
    newChangePasswordResponse,

    -- ** RespondToAuthChallenge
    RespondToAuthChallenge (RespondToAuthChallenge'),
    newRespondToAuthChallenge,
    RespondToAuthChallengeResponse (RespondToAuthChallengeResponse'),
    newRespondToAuthChallengeResponse,

    -- ** ListDevices
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- ** AdminGetDevice
    AdminGetDevice (AdminGetDevice'),
    newAdminGetDevice,
    AdminGetDeviceResponse (AdminGetDeviceResponse'),
    newAdminGetDeviceResponse,

    -- ** CreateUserPool
    CreateUserPool (CreateUserPool'),
    newCreateUserPool,
    CreateUserPoolResponse (CreateUserPoolResponse'),
    newCreateUserPoolResponse,

    -- ** AdminRemoveUserFromGroup
    AdminRemoveUserFromGroup (AdminRemoveUserFromGroup'),
    newAdminRemoveUserFromGroup,
    AdminRemoveUserFromGroupResponse (AdminRemoveUserFromGroupResponse'),
    newAdminRemoveUserFromGroupResponse,

    -- ** SetRiskConfiguration
    SetRiskConfiguration (SetRiskConfiguration'),
    newSetRiskConfiguration,
    SetRiskConfigurationResponse (SetRiskConfigurationResponse'),
    newSetRiskConfigurationResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** UpdateIdentityProvider
    UpdateIdentityProvider (UpdateIdentityProvider'),
    newUpdateIdentityProvider,
    UpdateIdentityProviderResponse (UpdateIdentityProviderResponse'),
    newUpdateIdentityProviderResponse,

    -- ** ListIdentityProviders (Paginated)
    ListIdentityProviders (ListIdentityProviders'),
    newListIdentityProviders,
    ListIdentityProvidersResponse (ListIdentityProvidersResponse'),
    newListIdentityProvidersResponse,

    -- ** GetDevice
    GetDevice (GetDevice'),
    newGetDevice,
    GetDeviceResponse (GetDeviceResponse'),
    newGetDeviceResponse,

    -- ** DeleteIdentityProvider
    DeleteIdentityProvider (DeleteIdentityProvider'),
    newDeleteIdentityProvider,
    DeleteIdentityProviderResponse (DeleteIdentityProviderResponse'),
    newDeleteIdentityProviderResponse,

    -- ** AdminListDevices
    AdminListDevices (AdminListDevices'),
    newAdminListDevices,
    AdminListDevicesResponse (AdminListDevicesResponse'),
    newAdminListDevicesResponse,

    -- ** AdminConfirmSignUp
    AdminConfirmSignUp (AdminConfirmSignUp'),
    newAdminConfirmSignUp,
    AdminConfirmSignUpResponse (AdminConfirmSignUpResponse'),
    newAdminConfirmSignUpResponse,

    -- ** SetUICustomization
    SetUICustomization (SetUICustomization'),
    newSetUICustomization,
    SetUICustomizationResponse (SetUICustomizationResponse'),
    newSetUICustomizationResponse,

    -- ** AdminListUserAuthEvents (Paginated)
    AdminListUserAuthEvents (AdminListUserAuthEvents'),
    newAdminListUserAuthEvents,
    AdminListUserAuthEventsResponse (AdminListUserAuthEventsResponse'),
    newAdminListUserAuthEventsResponse,

    -- ** AdminAddUserToGroup
    AdminAddUserToGroup (AdminAddUserToGroup'),
    newAdminAddUserToGroup,
    AdminAddUserToGroupResponse (AdminAddUserToGroupResponse'),
    newAdminAddUserToGroupResponse,

    -- ** VerifySoftwareToken
    VerifySoftwareToken (VerifySoftwareToken'),
    newVerifySoftwareToken,
    VerifySoftwareTokenResponse (VerifySoftwareTokenResponse'),
    newVerifySoftwareTokenResponse,

    -- ** StopUserImportJob
    StopUserImportJob (StopUserImportJob'),
    newStopUserImportJob,
    StopUserImportJobResponse (StopUserImportJobResponse'),
    newStopUserImportJobResponse,

    -- ** CreateIdentityProvider
    CreateIdentityProvider (CreateIdentityProvider'),
    newCreateIdentityProvider,
    CreateIdentityProviderResponse (CreateIdentityProviderResponse'),
    newCreateIdentityProviderResponse,

    -- ** InitiateAuth
    InitiateAuth (InitiateAuth'),
    newInitiateAuth,
    InitiateAuthResponse (InitiateAuthResponse'),
    newInitiateAuthResponse,

    -- ** AdminSetUserPassword
    AdminSetUserPassword (AdminSetUserPassword'),
    newAdminSetUserPassword,
    AdminSetUserPasswordResponse (AdminSetUserPasswordResponse'),
    newAdminSetUserPasswordResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** AdminListGroupsForUser (Paginated)
    AdminListGroupsForUser (AdminListGroupsForUser'),
    newAdminListGroupsForUser,
    AdminListGroupsForUserResponse (AdminListGroupsForUserResponse'),
    newAdminListGroupsForUserResponse,

    -- ** AdminUpdateAuthEventFeedback
    AdminUpdateAuthEventFeedback (AdminUpdateAuthEventFeedback'),
    newAdminUpdateAuthEventFeedback,
    AdminUpdateAuthEventFeedbackResponse (AdminUpdateAuthEventFeedbackResponse'),
    newAdminUpdateAuthEventFeedbackResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** StartUserImportJob
    StartUserImportJob (StartUserImportJob'),
    newStartUserImportJob,
    StartUserImportJobResponse (StartUserImportJobResponse'),
    newStartUserImportJobResponse,

    -- ** DescribeUserPoolDomain
    DescribeUserPoolDomain (DescribeUserPoolDomain'),
    newDescribeUserPoolDomain,
    DescribeUserPoolDomainResponse (DescribeUserPoolDomainResponse'),
    newDescribeUserPoolDomainResponse,

    -- ** ListUsersInGroup (Paginated)
    ListUsersInGroup (ListUsersInGroup'),
    newListUsersInGroup,
    ListUsersInGroupResponse (ListUsersInGroupResponse'),
    newListUsersInGroupResponse,

    -- ** AdminUserGlobalSignOut
    AdminUserGlobalSignOut (AdminUserGlobalSignOut'),
    newAdminUserGlobalSignOut,
    AdminUserGlobalSignOutResponse (AdminUserGlobalSignOutResponse'),
    newAdminUserGlobalSignOutResponse,

    -- ** DescribeUserPool
    DescribeUserPool (DescribeUserPool'),
    newDescribeUserPool,
    DescribeUserPoolResponse (DescribeUserPoolResponse'),
    newDescribeUserPoolResponse,

    -- ** AdminGetUser
    AdminGetUser (AdminGetUser'),
    newAdminGetUser,
    AdminGetUserResponse (AdminGetUserResponse'),
    newAdminGetUserResponse,

    -- ** GetSigningCertificate
    GetSigningCertificate (GetSigningCertificate'),
    newGetSigningCertificate,
    GetSigningCertificateResponse (GetSigningCertificateResponse'),
    newGetSigningCertificateResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** AssociateSoftwareToken
    AssociateSoftwareToken (AssociateSoftwareToken'),
    newAssociateSoftwareToken,
    AssociateSoftwareTokenResponse (AssociateSoftwareTokenResponse'),
    newAssociateSoftwareTokenResponse,

    -- ** ForgotPassword
    ForgotPassword (ForgotPassword'),
    newForgotPassword,
    ForgotPasswordResponse (ForgotPasswordResponse'),
    newForgotPasswordResponse,

    -- ** UpdateDeviceStatus
    UpdateDeviceStatus (UpdateDeviceStatus'),
    newUpdateDeviceStatus,
    UpdateDeviceStatusResponse (UpdateDeviceStatusResponse'),
    newUpdateDeviceStatusResponse,

    -- ** DeleteUserPoolClient
    DeleteUserPoolClient (DeleteUserPoolClient'),
    newDeleteUserPoolClient,
    DeleteUserPoolClientResponse (DeleteUserPoolClientResponse'),
    newDeleteUserPoolClientResponse,

    -- ** UpdateUserPoolClient
    UpdateUserPoolClient (UpdateUserPoolClient'),
    newUpdateUserPoolClient,
    UpdateUserPoolClientResponse (UpdateUserPoolClientResponse'),
    newUpdateUserPoolClientResponse,

    -- ** ForgetDevice
    ForgetDevice (ForgetDevice'),
    newForgetDevice,
    ForgetDeviceResponse (ForgetDeviceResponse'),
    newForgetDeviceResponse,

    -- ** AdminDisableUser
    AdminDisableUser (AdminDisableUser'),
    newAdminDisableUser,
    AdminDisableUserResponse (AdminDisableUserResponse'),
    newAdminDisableUserResponse,

    -- ** AdminCreateUser
    AdminCreateUser (AdminCreateUser'),
    newAdminCreateUser,
    AdminCreateUserResponse (AdminCreateUserResponse'),
    newAdminCreateUserResponse,

    -- ** AdminUpdateDeviceStatus
    AdminUpdateDeviceStatus (AdminUpdateDeviceStatus'),
    newAdminUpdateDeviceStatus,
    AdminUpdateDeviceStatusResponse (AdminUpdateDeviceStatusResponse'),
    newAdminUpdateDeviceStatusResponse,

    -- ** AdminForgetDevice
    AdminForgetDevice (AdminForgetDevice'),
    newAdminForgetDevice,
    AdminForgetDeviceResponse (AdminForgetDeviceResponse'),
    newAdminForgetDeviceResponse,

    -- ** DescribeIdentityProvider
    DescribeIdentityProvider (DescribeIdentityProvider'),
    newDescribeIdentityProvider,
    DescribeIdentityProviderResponse (DescribeIdentityProviderResponse'),
    newDescribeIdentityProviderResponse,

    -- ** ListUserImportJobs
    ListUserImportJobs (ListUserImportJobs'),
    newListUserImportJobs,
    ListUserImportJobsResponse (ListUserImportJobsResponse'),
    newListUserImportJobsResponse,

    -- ** GetUICustomization
    GetUICustomization (GetUICustomization'),
    newGetUICustomization,
    GetUICustomizationResponse (GetUICustomizationResponse'),
    newGetUICustomizationResponse,

    -- ** GetCSVHeader
    GetCSVHeader (GetCSVHeader'),
    newGetCSVHeader,
    GetCSVHeaderResponse (GetCSVHeaderResponse'),
    newGetCSVHeaderResponse,

    -- ** AdminEnableUser
    AdminEnableUser (AdminEnableUser'),
    newAdminEnableUser,
    AdminEnableUserResponse (AdminEnableUserResponse'),
    newAdminEnableUserResponse,

    -- ** AdminInitiateAuth
    AdminInitiateAuth (AdminInitiateAuth'),
    newAdminInitiateAuth,
    AdminInitiateAuthResponse (AdminInitiateAuthResponse'),
    newAdminInitiateAuthResponse,

    -- ** AdminResetUserPassword
    AdminResetUserPassword (AdminResetUserPassword'),
    newAdminResetUserPassword,
    AdminResetUserPasswordResponse (AdminResetUserPasswordResponse'),
    newAdminResetUserPasswordResponse,

    -- ** GetIdentityProviderByIdentifier
    GetIdentityProviderByIdentifier (GetIdentityProviderByIdentifier'),
    newGetIdentityProviderByIdentifier,
    GetIdentityProviderByIdentifierResponse (GetIdentityProviderByIdentifierResponse'),
    newGetIdentityProviderByIdentifierResponse,

    -- ** AdminSetUserSettings
    AdminSetUserSettings (AdminSetUserSettings'),
    newAdminSetUserSettings,
    AdminSetUserSettingsResponse (AdminSetUserSettingsResponse'),
    newAdminSetUserSettingsResponse,

    -- ** DescribeUserPoolClient
    DescribeUserPoolClient (DescribeUserPoolClient'),
    newDescribeUserPoolClient,
    DescribeUserPoolClientResponse (DescribeUserPoolClientResponse'),
    newDescribeUserPoolClientResponse,

    -- ** GetGroup
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** ResendConfirmationCode
    ResendConfirmationCode (ResendConfirmationCode'),
    newResendConfirmationCode,
    ResendConfirmationCodeResponse (ResendConfirmationCodeResponse'),
    newResendConfirmationCodeResponse,

    -- ** AdminRespondToAuthChallenge
    AdminRespondToAuthChallenge (AdminRespondToAuthChallenge'),
    newAdminRespondToAuthChallenge,
    AdminRespondToAuthChallengeResponse (AdminRespondToAuthChallengeResponse'),
    newAdminRespondToAuthChallengeResponse,

    -- ** SignUp
    SignUp (SignUp'),
    newSignUp,
    SignUpResponse (SignUpResponse'),
    newSignUpResponse,

    -- ** DescribeUserImportJob
    DescribeUserImportJob (DescribeUserImportJob'),
    newDescribeUserImportJob,
    DescribeUserImportJobResponse (DescribeUserImportJobResponse'),
    newDescribeUserImportJobResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** UpdateResourceServer
    UpdateResourceServer (UpdateResourceServer'),
    newUpdateResourceServer,
    UpdateResourceServerResponse (UpdateResourceServerResponse'),
    newUpdateResourceServerResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** SetUserSettings
    SetUserSettings (SetUserSettings'),
    newSetUserSettings,
    SetUserSettingsResponse (SetUserSettingsResponse'),
    newSetUserSettingsResponse,

    -- ** ListResourceServers (Paginated)
    ListResourceServers (ListResourceServers'),
    newListResourceServers,
    ListResourceServersResponse (ListResourceServersResponse'),
    newListResourceServersResponse,

    -- ** DeleteResourceServer
    DeleteResourceServer (DeleteResourceServer'),
    newDeleteResourceServer,
    DeleteResourceServerResponse (DeleteResourceServerResponse'),
    newDeleteResourceServerResponse,

    -- ** DescribeRiskConfiguration
    DescribeRiskConfiguration (DescribeRiskConfiguration'),
    newDescribeRiskConfiguration,
    DescribeRiskConfigurationResponse (DescribeRiskConfigurationResponse'),
    newDescribeRiskConfigurationResponse,

    -- ** GlobalSignOut
    GlobalSignOut (GlobalSignOut'),
    newGlobalSignOut,
    GlobalSignOutResponse (GlobalSignOutResponse'),
    newGlobalSignOutResponse,

    -- ** GetUserPoolMfaConfig
    GetUserPoolMfaConfig (GetUserPoolMfaConfig'),
    newGetUserPoolMfaConfig,
    GetUserPoolMfaConfigResponse (GetUserPoolMfaConfigResponse'),
    newGetUserPoolMfaConfigResponse,

    -- ** CreateResourceServer
    CreateResourceServer (CreateResourceServer'),
    newCreateResourceServer,
    CreateResourceServerResponse (CreateResourceServerResponse'),
    newCreateResourceServerResponse,

    -- ** AdminUpdateUserAttributes
    AdminUpdateUserAttributes (AdminUpdateUserAttributes'),
    newAdminUpdateUserAttributes,
    AdminUpdateUserAttributesResponse (AdminUpdateUserAttributesResponse'),
    newAdminUpdateUserAttributesResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** AdminDeleteUserAttributes
    AdminDeleteUserAttributes (AdminDeleteUserAttributes'),
    newAdminDeleteUserAttributes,
    AdminDeleteUserAttributesResponse (AdminDeleteUserAttributesResponse'),
    newAdminDeleteUserAttributesResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** AdminDisableProviderForUser
    AdminDisableProviderForUser (AdminDisableProviderForUser'),
    newAdminDisableProviderForUser,
    AdminDisableProviderForUserResponse (AdminDisableProviderForUserResponse'),
    newAdminDisableProviderForUserResponse,

    -- ** CreateUserPoolClient
    CreateUserPoolClient (CreateUserPoolClient'),
    newCreateUserPoolClient,
    CreateUserPoolClientResponse (CreateUserPoolClientResponse'),
    newCreateUserPoolClientResponse,

    -- ** ListUserPoolClients (Paginated)
    ListUserPoolClients (ListUserPoolClients'),
    newListUserPoolClients,
    ListUserPoolClientsResponse (ListUserPoolClientsResponse'),
    newListUserPoolClientsResponse,

    -- ** AddCustomAttributes
    AddCustomAttributes (AddCustomAttributes'),
    newAddCustomAttributes,
    AddCustomAttributesResponse (AddCustomAttributesResponse'),
    newAddCustomAttributesResponse,

    -- ** AdminSetUserMFAPreference
    AdminSetUserMFAPreference (AdminSetUserMFAPreference'),
    newAdminSetUserMFAPreference,
    AdminSetUserMFAPreferenceResponse (AdminSetUserMFAPreferenceResponse'),
    newAdminSetUserMFAPreferenceResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ConfirmForgotPassword
    ConfirmForgotPassword (ConfirmForgotPassword'),
    newConfirmForgotPassword,
    ConfirmForgotPasswordResponse (ConfirmForgotPasswordResponse'),
    newConfirmForgotPasswordResponse,

    -- ** SetUserMFAPreference
    SetUserMFAPreference (SetUserMFAPreference'),
    newSetUserMFAPreference,
    SetUserMFAPreferenceResponse (SetUserMFAPreferenceResponse'),
    newSetUserMFAPreferenceResponse,

    -- ** VerifyUserAttribute
    VerifyUserAttribute (VerifyUserAttribute'),
    newVerifyUserAttribute,
    VerifyUserAttributeResponse (VerifyUserAttributeResponse'),
    newVerifyUserAttributeResponse,

    -- ** ConfirmDevice
    ConfirmDevice (ConfirmDevice'),
    newConfirmDevice,
    ConfirmDeviceResponse (ConfirmDeviceResponse'),
    newConfirmDeviceResponse,

    -- ** DescribeResourceServer
    DescribeResourceServer (DescribeResourceServer'),
    newDescribeResourceServer,
    DescribeResourceServerResponse (DescribeResourceServerResponse'),
    newDescribeResourceServerResponse,

    -- * Types

    -- ** AccountTakeoverEventActionType
    AccountTakeoverEventActionType (..),

    -- ** AdvancedSecurityModeType
    AdvancedSecurityModeType (..),

    -- ** AliasAttributeType
    AliasAttributeType (..),

    -- ** AttributeDataType
    AttributeDataType (..),

    -- ** AuthFlowType
    AuthFlowType (..),

    -- ** ChallengeName
    ChallengeName (..),

    -- ** ChallengeNameType
    ChallengeNameType (..),

    -- ** ChallengeResponse
    ChallengeResponse (..),

    -- ** CompromisedCredentialsEventActionType
    CompromisedCredentialsEventActionType (..),

    -- ** CustomEmailSenderLambdaVersionType
    CustomEmailSenderLambdaVersionType (..),

    -- ** CustomSMSSenderLambdaVersionType
    CustomSMSSenderLambdaVersionType (..),

    -- ** DefaultEmailOptionType
    DefaultEmailOptionType (..),

    -- ** DeliveryMediumType
    DeliveryMediumType (..),

    -- ** DeviceRememberedStatusType
    DeviceRememberedStatusType (..),

    -- ** DomainStatusType
    DomainStatusType (..),

    -- ** EmailSendingAccountType
    EmailSendingAccountType (..),

    -- ** EventFilterType
    EventFilterType (..),

    -- ** EventResponseType
    EventResponseType (..),

    -- ** EventType
    EventType (..),

    -- ** ExplicitAuthFlowsType
    ExplicitAuthFlowsType (..),

    -- ** FeedbackValueType
    FeedbackValueType (..),

    -- ** IdentityProviderTypeType
    IdentityProviderTypeType (..),

    -- ** MessageActionType
    MessageActionType (..),

    -- ** OAuthFlowType
    OAuthFlowType (..),

    -- ** PreventUserExistenceErrorTypes
    PreventUserExistenceErrorTypes (..),

    -- ** RecoveryOptionNameType
    RecoveryOptionNameType (..),

    -- ** RiskDecisionType
    RiskDecisionType (..),

    -- ** RiskLevelType
    RiskLevelType (..),

    -- ** StatusType
    StatusType (..),

    -- ** TimeUnitsType
    TimeUnitsType (..),

    -- ** UserImportJobStatusType
    UserImportJobStatusType (..),

    -- ** UserPoolMfaType
    UserPoolMfaType (..),

    -- ** UserStatusType
    UserStatusType (..),

    -- ** UsernameAttributeType
    UsernameAttributeType (..),

    -- ** VerifiedAttributeType
    VerifiedAttributeType (..),

    -- ** VerifySoftwareTokenResponseType
    VerifySoftwareTokenResponseType (..),

    -- ** AccountRecoverySettingType
    AccountRecoverySettingType (AccountRecoverySettingType'),
    newAccountRecoverySettingType,

    -- ** AccountTakeoverActionType
    AccountTakeoverActionType (AccountTakeoverActionType'),
    newAccountTakeoverActionType,

    -- ** AccountTakeoverActionsType
    AccountTakeoverActionsType (AccountTakeoverActionsType'),
    newAccountTakeoverActionsType,

    -- ** AccountTakeoverRiskConfigurationType
    AccountTakeoverRiskConfigurationType (AccountTakeoverRiskConfigurationType'),
    newAccountTakeoverRiskConfigurationType,

    -- ** AdminCreateUserConfigType
    AdminCreateUserConfigType (AdminCreateUserConfigType'),
    newAdminCreateUserConfigType,

    -- ** AnalyticsConfigurationType
    AnalyticsConfigurationType (AnalyticsConfigurationType'),
    newAnalyticsConfigurationType,

    -- ** AnalyticsMetadataType
    AnalyticsMetadataType (AnalyticsMetadataType'),
    newAnalyticsMetadataType,

    -- ** AttributeType
    AttributeType (AttributeType'),
    newAttributeType,

    -- ** AuthEventType
    AuthEventType (AuthEventType'),
    newAuthEventType,

    -- ** AuthenticationResultType
    AuthenticationResultType (AuthenticationResultType'),
    newAuthenticationResultType,

    -- ** ChallengeResponseType
    ChallengeResponseType (ChallengeResponseType'),
    newChallengeResponseType,

    -- ** CodeDeliveryDetailsType
    CodeDeliveryDetailsType (CodeDeliveryDetailsType'),
    newCodeDeliveryDetailsType,

    -- ** CompromisedCredentialsActionsType
    CompromisedCredentialsActionsType (CompromisedCredentialsActionsType'),
    newCompromisedCredentialsActionsType,

    -- ** CompromisedCredentialsRiskConfigurationType
    CompromisedCredentialsRiskConfigurationType (CompromisedCredentialsRiskConfigurationType'),
    newCompromisedCredentialsRiskConfigurationType,

    -- ** ContextDataType
    ContextDataType (ContextDataType'),
    newContextDataType,

    -- ** CustomDomainConfigType
    CustomDomainConfigType (CustomDomainConfigType'),
    newCustomDomainConfigType,

    -- ** CustomEmailLambdaVersionConfigType
    CustomEmailLambdaVersionConfigType (CustomEmailLambdaVersionConfigType'),
    newCustomEmailLambdaVersionConfigType,

    -- ** CustomSMSLambdaVersionConfigType
    CustomSMSLambdaVersionConfigType (CustomSMSLambdaVersionConfigType'),
    newCustomSMSLambdaVersionConfigType,

    -- ** DeviceConfigurationType
    DeviceConfigurationType (DeviceConfigurationType'),
    newDeviceConfigurationType,

    -- ** DeviceSecretVerifierConfigType
    DeviceSecretVerifierConfigType (DeviceSecretVerifierConfigType'),
    newDeviceSecretVerifierConfigType,

    -- ** DeviceType
    DeviceType (DeviceType'),
    newDeviceType,

    -- ** DomainDescriptionType
    DomainDescriptionType (DomainDescriptionType'),
    newDomainDescriptionType,

    -- ** EmailConfigurationType
    EmailConfigurationType (EmailConfigurationType'),
    newEmailConfigurationType,

    -- ** EventContextDataType
    EventContextDataType (EventContextDataType'),
    newEventContextDataType,

    -- ** EventFeedbackType
    EventFeedbackType (EventFeedbackType'),
    newEventFeedbackType,

    -- ** EventRiskType
    EventRiskType (EventRiskType'),
    newEventRiskType,

    -- ** GroupType
    GroupType (GroupType'),
    newGroupType,

    -- ** HttpHeader
    HttpHeader (HttpHeader'),
    newHttpHeader,

    -- ** IdentityProviderType
    IdentityProviderType (IdentityProviderType'),
    newIdentityProviderType,

    -- ** LambdaConfigType
    LambdaConfigType (LambdaConfigType'),
    newLambdaConfigType,

    -- ** MFAOptionType
    MFAOptionType (MFAOptionType'),
    newMFAOptionType,

    -- ** MessageTemplateType
    MessageTemplateType (MessageTemplateType'),
    newMessageTemplateType,

    -- ** NewDeviceMetadataType
    NewDeviceMetadataType (NewDeviceMetadataType'),
    newNewDeviceMetadataType,

    -- ** NotifyConfigurationType
    NotifyConfigurationType (NotifyConfigurationType'),
    newNotifyConfigurationType,

    -- ** NotifyEmailType
    NotifyEmailType (NotifyEmailType'),
    newNotifyEmailType,

    -- ** NumberAttributeConstraintsType
    NumberAttributeConstraintsType (NumberAttributeConstraintsType'),
    newNumberAttributeConstraintsType,

    -- ** PasswordPolicyType
    PasswordPolicyType (PasswordPolicyType'),
    newPasswordPolicyType,

    -- ** ProviderDescription
    ProviderDescription (ProviderDescription'),
    newProviderDescription,

    -- ** ProviderUserIdentifierType
    ProviderUserIdentifierType (ProviderUserIdentifierType'),
    newProviderUserIdentifierType,

    -- ** RecoveryOptionType
    RecoveryOptionType (RecoveryOptionType'),
    newRecoveryOptionType,

    -- ** ResourceServerScopeType
    ResourceServerScopeType (ResourceServerScopeType'),
    newResourceServerScopeType,

    -- ** ResourceServerType
    ResourceServerType (ResourceServerType'),
    newResourceServerType,

    -- ** RiskConfigurationType
    RiskConfigurationType (RiskConfigurationType'),
    newRiskConfigurationType,

    -- ** RiskExceptionConfigurationType
    RiskExceptionConfigurationType (RiskExceptionConfigurationType'),
    newRiskExceptionConfigurationType,

    -- ** SMSMfaSettingsType
    SMSMfaSettingsType (SMSMfaSettingsType'),
    newSMSMfaSettingsType,

    -- ** SchemaAttributeType
    SchemaAttributeType (SchemaAttributeType'),
    newSchemaAttributeType,

    -- ** SmsConfigurationType
    SmsConfigurationType (SmsConfigurationType'),
    newSmsConfigurationType,

    -- ** SmsMfaConfigType
    SmsMfaConfigType (SmsMfaConfigType'),
    newSmsMfaConfigType,

    -- ** SoftwareTokenMfaConfigType
    SoftwareTokenMfaConfigType (SoftwareTokenMfaConfigType'),
    newSoftwareTokenMfaConfigType,

    -- ** SoftwareTokenMfaSettingsType
    SoftwareTokenMfaSettingsType (SoftwareTokenMfaSettingsType'),
    newSoftwareTokenMfaSettingsType,

    -- ** StringAttributeConstraintsType
    StringAttributeConstraintsType (StringAttributeConstraintsType'),
    newStringAttributeConstraintsType,

    -- ** TokenValidityUnitsType
    TokenValidityUnitsType (TokenValidityUnitsType'),
    newTokenValidityUnitsType,

    -- ** UICustomizationType
    UICustomizationType (UICustomizationType'),
    newUICustomizationType,

    -- ** UserContextDataType
    UserContextDataType (UserContextDataType'),
    newUserContextDataType,

    -- ** UserImportJobType
    UserImportJobType (UserImportJobType'),
    newUserImportJobType,

    -- ** UserPoolAddOnsType
    UserPoolAddOnsType (UserPoolAddOnsType'),
    newUserPoolAddOnsType,

    -- ** UserPoolClientDescription
    UserPoolClientDescription (UserPoolClientDescription'),
    newUserPoolClientDescription,

    -- ** UserPoolClientType
    UserPoolClientType (UserPoolClientType'),
    newUserPoolClientType,

    -- ** UserPoolDescriptionType
    UserPoolDescriptionType (UserPoolDescriptionType'),
    newUserPoolDescriptionType,

    -- ** UserPoolPolicyType
    UserPoolPolicyType (UserPoolPolicyType'),
    newUserPoolPolicyType,

    -- ** UserPoolType
    UserPoolType (UserPoolType'),
    newUserPoolType,

    -- ** UserType
    UserType (UserType'),
    newUserType,

    -- ** UsernameConfigurationType
    UsernameConfigurationType (UsernameConfigurationType'),
    newUsernameConfigurationType,

    -- ** VerificationMessageTemplateType
    VerificationMessageTemplateType (VerificationMessageTemplateType'),
    newVerificationMessageTemplateType,
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
import Network.AWS.CognitoIdentityProvider.Lens
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
import Network.AWS.CognitoIdentityProvider.Types
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
import Network.AWS.CognitoIdentityProvider.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CognitoIdentityProvider'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
