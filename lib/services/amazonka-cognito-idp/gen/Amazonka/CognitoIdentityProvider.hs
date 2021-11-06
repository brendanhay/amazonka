{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CognitoIdentityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-04-18@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Using the Amazon Cognito User Pools API, you can create a user pool to
-- manage directories and users. You can authenticate a user to obtain
-- tokens related to user identity and access policies.
--
-- This API reference provides information about user pools in Amazon
-- Cognito User Pools.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/what-is-amazon-cognito.html Amazon Cognito Documentation>.
module Amazonka.CognitoIdentityProvider
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** UnsupportedUserStateException
    _UnsupportedUserStateException,

    -- ** PasswordResetRequiredException
    _PasswordResetRequiredException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** InvalidLambdaResponseException
    _InvalidLambdaResponseException,

    -- ** InvalidEmailRoleAccessPolicyException
    _InvalidEmailRoleAccessPolicyException,

    -- ** UnsupportedIdentityProviderException
    _UnsupportedIdentityProviderException,

    -- ** UserNotFoundException
    _UserNotFoundException,

    -- ** UnexpectedLambdaException
    _UnexpectedLambdaException,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** InvalidUserPoolConfigurationException
    _InvalidUserPoolConfigurationException,

    -- ** InvalidSmsRoleAccessPolicyException
    _InvalidSmsRoleAccessPolicyException,

    -- ** InvalidOAuthFlowException
    _InvalidOAuthFlowException,

    -- ** CodeMismatchException
    _CodeMismatchException,

    -- ** UserImportInProgressException
    _UserImportInProgressException,

    -- ** InvalidSmsRoleTrustRelationshipException
    _InvalidSmsRoleTrustRelationshipException,

    -- ** UserPoolTaggingException
    _UserPoolTaggingException,

    -- ** SoftwareTokenMFANotFoundException
    _SoftwareTokenMFANotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** UserPoolAddOnNotEnabledException
    _UserPoolAddOnNotEnabledException,

    -- ** UserLambdaValidationException
    _UserLambdaValidationException,

    -- ** PreconditionNotMetException
    _PreconditionNotMetException,

    -- ** ExpiredCodeException
    _ExpiredCodeException,

    -- ** TooManyFailedAttemptsException
    _TooManyFailedAttemptsException,

    -- ** EnableSoftwareTokenMFAException
    _EnableSoftwareTokenMFAException,

    -- ** UserNotConfirmedException
    _UserNotConfirmedException,

    -- ** GroupExistsException
    _GroupExistsException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** CodeDeliveryFailureException
    _CodeDeliveryFailureException,

    -- ** ScopeDoesNotExistException
    _ScopeDoesNotExistException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** MFAMethodNotFoundException
    _MFAMethodNotFoundException,

    -- ** AliasExistsException
    _AliasExistsException,

    -- ** UnsupportedTokenTypeException
    _UnsupportedTokenTypeException,

    -- ** DuplicateProviderException
    _DuplicateProviderException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InvalidPasswordException
    _InvalidPasswordException,

    -- ** UsernameExistsException
    _UsernameExistsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteUserPool
    DeleteUserPool (DeleteUserPool'),
    newDeleteUserPool,
    DeleteUserPoolResponse (DeleteUserPoolResponse'),
    newDeleteUserPoolResponse,

    -- ** UpdateUserPool
    UpdateUserPool (UpdateUserPool'),
    newUpdateUserPool,
    UpdateUserPoolResponse (UpdateUserPoolResponse'),
    newUpdateUserPoolResponse,

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

    -- ** AdminInitiateAuth
    AdminInitiateAuth (AdminInitiateAuth'),
    newAdminInitiateAuth,
    AdminInitiateAuthResponse (AdminInitiateAuthResponse'),
    newAdminInitiateAuthResponse,

    -- ** AdminLinkProviderForUser
    AdminLinkProviderForUser (AdminLinkProviderForUser'),
    newAdminLinkProviderForUser,
    AdminLinkProviderForUserResponse (AdminLinkProviderForUserResponse'),
    newAdminLinkProviderForUserResponse,

    -- ** AdminEnableUser
    AdminEnableUser (AdminEnableUser'),
    newAdminEnableUser,
    AdminEnableUserResponse (AdminEnableUserResponse'),
    newAdminEnableUserResponse,

    -- ** GetUserAttributeVerificationCode
    GetUserAttributeVerificationCode (GetUserAttributeVerificationCode'),
    newGetUserAttributeVerificationCode,
    GetUserAttributeVerificationCodeResponse (GetUserAttributeVerificationCodeResponse'),
    newGetUserAttributeVerificationCodeResponse,

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

    -- ** VerifyUserAttribute
    VerifyUserAttribute (VerifyUserAttribute'),
    newVerifyUserAttribute,
    VerifyUserAttributeResponse (VerifyUserAttributeResponse'),
    newVerifyUserAttributeResponse,

    -- ** AdminDisableUser
    AdminDisableUser (AdminDisableUser'),
    newAdminDisableUser,
    AdminDisableUserResponse (AdminDisableUserResponse'),
    newAdminDisableUserResponse,

    -- ** ConfirmDevice
    ConfirmDevice (ConfirmDevice'),
    newConfirmDevice,
    ConfirmDeviceResponse (ConfirmDeviceResponse'),
    newConfirmDeviceResponse,

    -- ** ConfirmForgotPassword
    ConfirmForgotPassword (ConfirmForgotPassword'),
    newConfirmForgotPassword,
    ConfirmForgotPasswordResponse (ConfirmForgotPasswordResponse'),
    newConfirmForgotPasswordResponse,

    -- ** ListUserImportJobs
    ListUserImportJobs (ListUserImportJobs'),
    newListUserImportJobs,
    ListUserImportJobsResponse (ListUserImportJobsResponse'),
    newListUserImportJobsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeIdentityProvider
    DescribeIdentityProvider (DescribeIdentityProvider'),
    newDescribeIdentityProvider,
    DescribeIdentityProviderResponse (DescribeIdentityProviderResponse'),
    newDescribeIdentityProviderResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** AdminDeleteUserAttributes
    AdminDeleteUserAttributes (AdminDeleteUserAttributes'),
    newAdminDeleteUserAttributes,
    AdminDeleteUserAttributesResponse (AdminDeleteUserAttributesResponse'),
    newAdminDeleteUserAttributesResponse,

    -- ** DescribeUserPoolDomain
    DescribeUserPoolDomain (DescribeUserPoolDomain'),
    newDescribeUserPoolDomain,
    DescribeUserPoolDomainResponse (DescribeUserPoolDomainResponse'),
    newDescribeUserPoolDomainResponse,

    -- ** AdminUpdateUserAttributes
    AdminUpdateUserAttributes (AdminUpdateUserAttributes'),
    newAdminUpdateUserAttributes,
    AdminUpdateUserAttributesResponse (AdminUpdateUserAttributesResponse'),
    newAdminUpdateUserAttributesResponse,

    -- ** AdminGetUser
    AdminGetUser (AdminGetUser'),
    newAdminGetUser,
    AdminGetUserResponse (AdminGetUserResponse'),
    newAdminGetUserResponse,

    -- ** AdminUserGlobalSignOut
    AdminUserGlobalSignOut (AdminUserGlobalSignOut'),
    newAdminUserGlobalSignOut,
    AdminUserGlobalSignOutResponse (AdminUserGlobalSignOutResponse'),
    newAdminUserGlobalSignOutResponse,

    -- ** ListUsersInGroup (Paginated)
    ListUsersInGroup (ListUsersInGroup'),
    newListUsersInGroup,
    ListUsersInGroupResponse (ListUsersInGroupResponse'),
    newListUsersInGroupResponse,

    -- ** AssociateSoftwareToken
    AssociateSoftwareToken (AssociateSoftwareToken'),
    newAssociateSoftwareToken,
    AssociateSoftwareTokenResponse (AssociateSoftwareTokenResponse'),
    newAssociateSoftwareTokenResponse,

    -- ** AdminDisableProviderForUser
    AdminDisableProviderForUser (AdminDisableProviderForUser'),
    newAdminDisableProviderForUser,
    AdminDisableProviderForUserResponse (AdminDisableProviderForUserResponse'),
    newAdminDisableProviderForUserResponse,

    -- ** ForgotPassword
    ForgotPassword (ForgotPassword'),
    newForgotPassword,
    ForgotPasswordResponse (ForgotPasswordResponse'),
    newForgotPasswordResponse,

    -- ** DescribeUserPool
    DescribeUserPool (DescribeUserPool'),
    newDescribeUserPool,
    DescribeUserPoolResponse (DescribeUserPoolResponse'),
    newDescribeUserPoolResponse,

    -- ** InitiateAuth
    InitiateAuth (InitiateAuth'),
    newInitiateAuth,
    InitiateAuthResponse (InitiateAuthResponse'),
    newInitiateAuthResponse,

    -- ** AdminListGroupsForUser (Paginated)
    AdminListGroupsForUser (AdminListGroupsForUser'),
    newAdminListGroupsForUser,
    AdminListGroupsForUserResponse (AdminListGroupsForUserResponse'),
    newAdminListGroupsForUserResponse,

    -- ** AdminConfirmSignUp
    AdminConfirmSignUp (AdminConfirmSignUp'),
    newAdminConfirmSignUp,
    AdminConfirmSignUpResponse (AdminConfirmSignUpResponse'),
    newAdminConfirmSignUpResponse,

    -- ** AdminUpdateAuthEventFeedback
    AdminUpdateAuthEventFeedback (AdminUpdateAuthEventFeedback'),
    newAdminUpdateAuthEventFeedback,
    AdminUpdateAuthEventFeedbackResponse (AdminUpdateAuthEventFeedbackResponse'),
    newAdminUpdateAuthEventFeedbackResponse,

    -- ** AdminSetUserPassword
    AdminSetUserPassword (AdminSetUserPassword'),
    newAdminSetUserPassword,
    AdminSetUserPasswordResponse (AdminSetUserPasswordResponse'),
    newAdminSetUserPasswordResponse,

    -- ** StartUserImportJob
    StartUserImportJob (StartUserImportJob'),
    newStartUserImportJob,
    StartUserImportJobResponse (StartUserImportJobResponse'),
    newStartUserImportJobResponse,

    -- ** CreateIdentityProvider
    CreateIdentityProvider (CreateIdentityProvider'),
    newCreateIdentityProvider,
    CreateIdentityProviderResponse (CreateIdentityProviderResponse'),
    newCreateIdentityProviderResponse,

    -- ** SetUICustomization
    SetUICustomization (SetUICustomization'),
    newSetUICustomization,
    SetUICustomizationResponse (SetUICustomizationResponse'),
    newSetUICustomizationResponse,

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

    -- ** SignUp
    SignUp (SignUp'),
    newSignUp,
    SignUpResponse (SignUpResponse'),
    newSignUpResponse,

    -- ** DeleteResourceServer
    DeleteResourceServer (DeleteResourceServer'),
    newDeleteResourceServer,
    DeleteResourceServerResponse (DeleteResourceServerResponse'),
    newDeleteResourceServerResponse,

    -- ** UpdateResourceServer
    UpdateResourceServer (UpdateResourceServer'),
    newUpdateResourceServer,
    UpdateResourceServerResponse (UpdateResourceServerResponse'),
    newUpdateResourceServerResponse,

    -- ** ChangePassword
    ChangePassword (ChangePassword'),
    newChangePassword,
    ChangePasswordResponse (ChangePasswordResponse'),
    newChangePasswordResponse,

    -- ** CreateUserPoolDomain
    CreateUserPoolDomain (CreateUserPoolDomain'),
    newCreateUserPoolDomain,
    CreateUserPoolDomainResponse (CreateUserPoolDomainResponse'),
    newCreateUserPoolDomainResponse,

    -- ** RespondToAuthChallenge
    RespondToAuthChallenge (RespondToAuthChallenge'),
    newRespondToAuthChallenge,
    RespondToAuthChallengeResponse (RespondToAuthChallengeResponse'),
    newRespondToAuthChallengeResponse,

    -- ** CreateUserPool
    CreateUserPool (CreateUserPool'),
    newCreateUserPool,
    CreateUserPoolResponse (CreateUserPoolResponse'),
    newCreateUserPoolResponse,

    -- ** AdminGetDevice
    AdminGetDevice (AdminGetDevice'),
    newAdminGetDevice,
    AdminGetDeviceResponse (AdminGetDeviceResponse'),
    newAdminGetDeviceResponse,

    -- ** GetIdentityProviderByIdentifier
    GetIdentityProviderByIdentifier (GetIdentityProviderByIdentifier'),
    newGetIdentityProviderByIdentifier,
    GetIdentityProviderByIdentifierResponse (GetIdentityProviderByIdentifierResponse'),
    newGetIdentityProviderByIdentifierResponse,

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

    -- ** ConfirmSignUp
    ConfirmSignUp (ConfirmSignUp'),
    newConfirmSignUp,
    ConfirmSignUpResponse (ConfirmSignUpResponse'),
    newConfirmSignUpResponse,

    -- ** ListUserPools (Paginated)
    ListUserPools (ListUserPools'),
    newListUserPools,
    ListUserPoolsResponse (ListUserPoolsResponse'),
    newListUserPoolsResponse,

    -- ** AdminResetUserPassword
    AdminResetUserPassword (AdminResetUserPassword'),
    newAdminResetUserPassword,
    AdminResetUserPasswordResponse (AdminResetUserPasswordResponse'),
    newAdminResetUserPasswordResponse,

    -- ** UpdateAuthEventFeedback
    UpdateAuthEventFeedback (UpdateAuthEventFeedback'),
    newUpdateAuthEventFeedback,
    UpdateAuthEventFeedbackResponse (UpdateAuthEventFeedbackResponse'),
    newUpdateAuthEventFeedbackResponse,

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

    -- ** AdminDeleteUser
    AdminDeleteUser (AdminDeleteUser'),
    newAdminDeleteUser,
    AdminDeleteUserResponse (AdminDeleteUserResponse'),
    newAdminDeleteUserResponse,

    -- ** AdminForgetDevice
    AdminForgetDevice (AdminForgetDevice'),
    newAdminForgetDevice,
    AdminForgetDeviceResponse (AdminForgetDeviceResponse'),
    newAdminForgetDeviceResponse,

    -- ** DescribeResourceServer
    DescribeResourceServer (DescribeResourceServer'),
    newDescribeResourceServer,
    DescribeResourceServerResponse (DescribeResourceServerResponse'),
    newDescribeResourceServerResponse,

    -- ** SetUserMFAPreference
    SetUserMFAPreference (SetUserMFAPreference'),
    newSetUserMFAPreference,
    SetUserMFAPreferenceResponse (SetUserMFAPreferenceResponse'),
    newSetUserMFAPreferenceResponse,

    -- ** AdminUpdateDeviceStatus
    AdminUpdateDeviceStatus (AdminUpdateDeviceStatus'),
    newAdminUpdateDeviceStatus,
    AdminUpdateDeviceStatusResponse (AdminUpdateDeviceStatusResponse'),
    newAdminUpdateDeviceStatusResponse,

    -- ** AdminCreateUser
    AdminCreateUser (AdminCreateUser'),
    newAdminCreateUser,
    AdminCreateUserResponse (AdminCreateUserResponse'),
    newAdminCreateUserResponse,

    -- ** AddCustomAttributes
    AddCustomAttributes (AddCustomAttributes'),
    newAddCustomAttributes,
    AddCustomAttributesResponse (AddCustomAttributesResponse'),
    newAddCustomAttributesResponse,

    -- ** ListUserPoolClients (Paginated)
    ListUserPoolClients (ListUserPoolClients'),
    newListUserPoolClients,
    ListUserPoolClientsResponse (ListUserPoolClientsResponse'),
    newListUserPoolClientsResponse,

    -- ** AdminSetUserMFAPreference
    AdminSetUserMFAPreference (AdminSetUserMFAPreference'),
    newAdminSetUserMFAPreference,
    AdminSetUserMFAPreferenceResponse (AdminSetUserMFAPreferenceResponse'),
    newAdminSetUserMFAPreferenceResponse,

    -- ** UpdateUserPoolClient
    UpdateUserPoolClient (UpdateUserPoolClient'),
    newUpdateUserPoolClient,
    UpdateUserPoolClientResponse (UpdateUserPoolClientResponse'),
    newUpdateUserPoolClientResponse,

    -- ** DeleteUserPoolClient
    DeleteUserPoolClient (DeleteUserPoolClient'),
    newDeleteUserPoolClient,
    DeleteUserPoolClientResponse (DeleteUserPoolClientResponse'),
    newDeleteUserPoolClientResponse,

    -- ** UpdateDeviceStatus
    UpdateDeviceStatus (UpdateDeviceStatus'),
    newUpdateDeviceStatus,
    UpdateDeviceStatusResponse (UpdateDeviceStatusResponse'),
    newUpdateDeviceStatusResponse,

    -- ** ForgetDevice
    ForgetDevice (ForgetDevice'),
    newForgetDevice,
    ForgetDeviceResponse (ForgetDeviceResponse'),
    newForgetDeviceResponse,

    -- ** GetSigningCertificate
    GetSigningCertificate (GetSigningCertificate'),
    newGetSigningCertificate,
    GetSigningCertificateResponse (GetSigningCertificateResponse'),
    newGetSigningCertificateResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateUserPoolClient
    CreateUserPoolClient (CreateUserPoolClient'),
    newCreateUserPoolClient,
    CreateUserPoolClientResponse (CreateUserPoolClientResponse'),
    newCreateUserPoolClientResponse,

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

    -- ** AdminListUserAuthEvents (Paginated)
    AdminListUserAuthEvents (AdminListUserAuthEvents'),
    newAdminListUserAuthEvents,
    AdminListUserAuthEventsResponse (AdminListUserAuthEventsResponse'),
    newAdminListUserAuthEventsResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

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

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** RevokeToken
    RevokeToken (RevokeToken'),
    newRevokeToken,
    RevokeTokenResponse (RevokeTokenResponse'),
    newRevokeTokenResponse,

    -- ** StopUserImportJob
    StopUserImportJob (StopUserImportJob'),
    newStopUserImportJob,
    StopUserImportJobResponse (StopUserImportJobResponse'),
    newStopUserImportJobResponse,

    -- ** DescribeUserImportJob
    DescribeUserImportJob (DescribeUserImportJob'),
    newDescribeUserImportJob,
    DescribeUserImportJobResponse (DescribeUserImportJobResponse'),
    newDescribeUserImportJobResponse,

    -- ** DescribeRiskConfiguration
    DescribeRiskConfiguration (DescribeRiskConfiguration'),
    newDescribeRiskConfiguration,
    DescribeRiskConfigurationResponse (DescribeRiskConfigurationResponse'),
    newDescribeRiskConfigurationResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** GlobalSignOut
    GlobalSignOut (GlobalSignOut'),
    newGlobalSignOut,
    GlobalSignOutResponse (GlobalSignOutResponse'),
    newGlobalSignOutResponse,

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

    -- ** DeleteIdentityProvider
    DeleteIdentityProvider (DeleteIdentityProvider'),
    newDeleteIdentityProvider,
    DeleteIdentityProviderResponse (DeleteIdentityProviderResponse'),
    newDeleteIdentityProviderResponse,

    -- ** ListResourceServers (Paginated)
    ListResourceServers (ListResourceServers'),
    newListResourceServers,
    ListResourceServersResponse (ListResourceServersResponse'),
    newListResourceServersResponse,

    -- ** AdminRespondToAuthChallenge
    AdminRespondToAuthChallenge (AdminRespondToAuthChallenge'),
    newAdminRespondToAuthChallenge,
    AdminRespondToAuthChallengeResponse (AdminRespondToAuthChallengeResponse'),
    newAdminRespondToAuthChallengeResponse,

    -- ** SetUserSettings
    SetUserSettings (SetUserSettings'),
    newSetUserSettings,
    SetUserSettingsResponse (SetUserSettingsResponse'),
    newSetUserSettingsResponse,

    -- ** AdminListDevices
    AdminListDevices (AdminListDevices'),
    newAdminListDevices,
    AdminListDevicesResponse (AdminListDevicesResponse'),
    newAdminListDevicesResponse,

    -- ** DescribeUserPoolClient
    DescribeUserPoolClient (DescribeUserPoolClient'),
    newDescribeUserPoolClient,
    DescribeUserPoolClientResponse (DescribeUserPoolClientResponse'),
    newDescribeUserPoolClientResponse,

    -- ** ResendConfirmationCode
    ResendConfirmationCode (ResendConfirmationCode'),
    newResendConfirmationCode,
    ResendConfirmationCodeResponse (ResendConfirmationCodeResponse'),
    newResendConfirmationCodeResponse,

    -- ** GetGroup
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** AdminSetUserSettings
    AdminSetUserSettings (AdminSetUserSettings'),
    newAdminSetUserSettings,
    AdminSetUserSettingsResponse (AdminSetUserSettingsResponse'),
    newAdminSetUserSettingsResponse,

    -- ** ListDevices
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

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
import Amazonka.CognitoIdentityProvider.Lens
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
import Amazonka.CognitoIdentityProvider.Types
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
import Amazonka.CognitoIdentityProvider.Waiters

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
