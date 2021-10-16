{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.SES
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2010-12-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Simple Email Service
--
-- This document contains reference information for the
-- <https://aws.amazon.com/ses/ Amazon Simple Email Service> (Amazon SES)
-- API, version 2010-12-01. This document is best used in conjunction with
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html Amazon SES Developer Guide>.
--
-- For a list of Amazon SES endpoints to use in service requests, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/regions.html Regions and Amazon SES>
-- in the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html Amazon SES Developer Guide>.
module Network.AWS.SES
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CustomVerificationEmailTemplateAlreadyExistsException
    _CustomVerificationEmailTemplateAlreadyExistsException,

    -- ** ConfigurationSetSendingPausedException
    _ConfigurationSetSendingPausedException,

    -- ** InvalidConfigurationSetException
    _InvalidConfigurationSetException,

    -- ** InvalidSNSDestinationException
    _InvalidSNSDestinationException,

    -- ** AccountSendingPausedException
    _AccountSendingPausedException,

    -- ** EventDestinationDoesNotExistException
    _EventDestinationDoesNotExistException,

    -- ** CustomVerificationEmailInvalidContentException
    _CustomVerificationEmailInvalidContentException,

    -- ** InvalidTemplateException
    _InvalidTemplateException,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** ConfigurationSetAlreadyExistsException
    _ConfigurationSetAlreadyExistsException,

    -- ** MailFromDomainNotVerifiedException
    _MailFromDomainNotVerifiedException,

    -- ** RuleSetDoesNotExistException
    _RuleSetDoesNotExistException,

    -- ** FromEmailAddressNotVerifiedException
    _FromEmailAddressNotVerifiedException,

    -- ** MessageRejected
    _MessageRejected,

    -- ** InvalidCloudWatchDestinationException
    _InvalidCloudWatchDestinationException,

    -- ** InvalidDeliveryOptionsException
    _InvalidDeliveryOptionsException,

    -- ** CannotDeleteException
    _CannotDeleteException,

    -- ** TemplateDoesNotExistException
    _TemplateDoesNotExistException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** InvalidTrackingOptionsException
    _InvalidTrackingOptionsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** EventDestinationAlreadyExistsException
    _EventDestinationAlreadyExistsException,

    -- ** InvalidSnsTopicException
    _InvalidSnsTopicException,

    -- ** InvalidS3ConfigurationException
    _InvalidS3ConfigurationException,

    -- ** TrackingOptionsAlreadyExistsException
    _TrackingOptionsAlreadyExistsException,

    -- ** ConfigurationSetDoesNotExistException
    _ConfigurationSetDoesNotExistException,

    -- ** TrackingOptionsDoesNotExistException
    _TrackingOptionsDoesNotExistException,

    -- ** InvalidLambdaFunctionException
    _InvalidLambdaFunctionException,

    -- ** InvalidFirehoseDestinationException
    _InvalidFirehoseDestinationException,

    -- ** InvalidRenderingParameterException
    _InvalidRenderingParameterException,

    -- ** MissingRenderingAttributeException
    _MissingRenderingAttributeException,

    -- ** CustomVerificationEmailTemplateDoesNotExistException
    _CustomVerificationEmailTemplateDoesNotExistException,

    -- ** RuleDoesNotExistException
    _RuleDoesNotExistException,

    -- ** ProductionAccessNotGrantedException
    _ProductionAccessNotGrantedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutConfigurationSetDeliveryOptions
    PutConfigurationSetDeliveryOptions (PutConfigurationSetDeliveryOptions'),
    newPutConfigurationSetDeliveryOptions,
    PutConfigurationSetDeliveryOptionsResponse (PutConfigurationSetDeliveryOptionsResponse'),
    newPutConfigurationSetDeliveryOptionsResponse,

    -- ** DescribeConfigurationSet
    DescribeConfigurationSet (DescribeConfigurationSet'),
    newDescribeConfigurationSet,
    DescribeConfigurationSetResponse (DescribeConfigurationSetResponse'),
    newDescribeConfigurationSetResponse,

    -- ** DescribeReceiptRule
    DescribeReceiptRule (DescribeReceiptRule'),
    newDescribeReceiptRule,
    DescribeReceiptRuleResponse (DescribeReceiptRuleResponse'),
    newDescribeReceiptRuleResponse,

    -- ** DeleteIdentityPolicy
    DeleteIdentityPolicy (DeleteIdentityPolicy'),
    newDeleteIdentityPolicy,
    DeleteIdentityPolicyResponse (DeleteIdentityPolicyResponse'),
    newDeleteIdentityPolicyResponse,

    -- ** GetSendStatistics
    GetSendStatistics (GetSendStatistics'),
    newGetSendStatistics,
    GetSendStatisticsResponse (GetSendStatisticsResponse'),
    newGetSendStatisticsResponse,

    -- ** CreateTemplate
    CreateTemplate (CreateTemplate'),
    newCreateTemplate,
    CreateTemplateResponse (CreateTemplateResponse'),
    newCreateTemplateResponse,

    -- ** SetIdentityHeadersInNotificationsEnabled
    SetIdentityHeadersInNotificationsEnabled (SetIdentityHeadersInNotificationsEnabled'),
    newSetIdentityHeadersInNotificationsEnabled,
    SetIdentityHeadersInNotificationsEnabledResponse (SetIdentityHeadersInNotificationsEnabledResponse'),
    newSetIdentityHeadersInNotificationsEnabledResponse,

    -- ** GetIdentityDkimAttributes
    GetIdentityDkimAttributes (GetIdentityDkimAttributes'),
    newGetIdentityDkimAttributes,
    GetIdentityDkimAttributesResponse (GetIdentityDkimAttributesResponse'),
    newGetIdentityDkimAttributesResponse,

    -- ** GetSendQuota
    GetSendQuota (GetSendQuota'),
    newGetSendQuota,
    GetSendQuotaResponse (GetSendQuotaResponse'),
    newGetSendQuotaResponse,

    -- ** CreateReceiptRuleSet
    CreateReceiptRuleSet (CreateReceiptRuleSet'),
    newCreateReceiptRuleSet,
    CreateReceiptRuleSetResponse (CreateReceiptRuleSetResponse'),
    newCreateReceiptRuleSetResponse,

    -- ** CreateConfigurationSetTrackingOptions
    CreateConfigurationSetTrackingOptions (CreateConfigurationSetTrackingOptions'),
    newCreateConfigurationSetTrackingOptions,
    CreateConfigurationSetTrackingOptionsResponse (CreateConfigurationSetTrackingOptionsResponse'),
    newCreateConfigurationSetTrackingOptionsResponse,

    -- ** DeleteReceiptRuleSet
    DeleteReceiptRuleSet (DeleteReceiptRuleSet'),
    newDeleteReceiptRuleSet,
    DeleteReceiptRuleSetResponse (DeleteReceiptRuleSetResponse'),
    newDeleteReceiptRuleSetResponse,

    -- ** VerifyDomainIdentity
    VerifyDomainIdentity (VerifyDomainIdentity'),
    newVerifyDomainIdentity,
    VerifyDomainIdentityResponse (VerifyDomainIdentityResponse'),
    newVerifyDomainIdentityResponse,

    -- ** UpdateAccountSendingEnabled
    UpdateAccountSendingEnabled (UpdateAccountSendingEnabled'),
    newUpdateAccountSendingEnabled,
    UpdateAccountSendingEnabledResponse (UpdateAccountSendingEnabledResponse'),
    newUpdateAccountSendingEnabledResponse,

    -- ** DeleteTemplate
    DeleteTemplate (DeleteTemplate'),
    newDeleteTemplate,
    DeleteTemplateResponse (DeleteTemplateResponse'),
    newDeleteTemplateResponse,

    -- ** UpdateTemplate
    UpdateTemplate (UpdateTemplate'),
    newUpdateTemplate,
    UpdateTemplateResponse (UpdateTemplateResponse'),
    newUpdateTemplateResponse,

    -- ** SetReceiptRulePosition
    SetReceiptRulePosition (SetReceiptRulePosition'),
    newSetReceiptRulePosition,
    SetReceiptRulePositionResponse (SetReceiptRulePositionResponse'),
    newSetReceiptRulePositionResponse,

    -- ** GetIdentityPolicies
    GetIdentityPolicies (GetIdentityPolicies'),
    newGetIdentityPolicies,
    GetIdentityPoliciesResponse (GetIdentityPoliciesResponse'),
    newGetIdentityPoliciesResponse,

    -- ** GetIdentityVerificationAttributes
    GetIdentityVerificationAttributes (GetIdentityVerificationAttributes'),
    newGetIdentityVerificationAttributes,
    GetIdentityVerificationAttributesResponse (GetIdentityVerificationAttributesResponse'),
    newGetIdentityVerificationAttributesResponse,

    -- ** GetAccountSendingEnabled
    GetAccountSendingEnabled (GetAccountSendingEnabled'),
    newGetAccountSendingEnabled,
    GetAccountSendingEnabledResponse (GetAccountSendingEnabledResponse'),
    newGetAccountSendingEnabledResponse,

    -- ** CreateConfigurationSetEventDestination
    CreateConfigurationSetEventDestination (CreateConfigurationSetEventDestination'),
    newCreateConfigurationSetEventDestination,
    CreateConfigurationSetEventDestinationResponse (CreateConfigurationSetEventDestinationResponse'),
    newCreateConfigurationSetEventDestinationResponse,

    -- ** CreateConfigurationSet
    CreateConfigurationSet (CreateConfigurationSet'),
    newCreateConfigurationSet,
    CreateConfigurationSetResponse (CreateConfigurationSetResponse'),
    newCreateConfigurationSetResponse,

    -- ** DeleteConfigurationSet
    DeleteConfigurationSet (DeleteConfigurationSet'),
    newDeleteConfigurationSet,
    DeleteConfigurationSetResponse (DeleteConfigurationSetResponse'),
    newDeleteConfigurationSetResponse,

    -- ** CloneReceiptRuleSet
    CloneReceiptRuleSet (CloneReceiptRuleSet'),
    newCloneReceiptRuleSet,
    CloneReceiptRuleSetResponse (CloneReceiptRuleSetResponse'),
    newCloneReceiptRuleSetResponse,

    -- ** UpdateConfigurationSetEventDestination
    UpdateConfigurationSetEventDestination (UpdateConfigurationSetEventDestination'),
    newUpdateConfigurationSetEventDestination,
    UpdateConfigurationSetEventDestinationResponse (UpdateConfigurationSetEventDestinationResponse'),
    newUpdateConfigurationSetEventDestinationResponse,

    -- ** DeleteReceiptRule
    DeleteReceiptRule (DeleteReceiptRule'),
    newDeleteReceiptRule,
    DeleteReceiptRuleResponse (DeleteReceiptRuleResponse'),
    newDeleteReceiptRuleResponse,

    -- ** UpdateReceiptRule
    UpdateReceiptRule (UpdateReceiptRule'),
    newUpdateReceiptRule,
    UpdateReceiptRuleResponse (UpdateReceiptRuleResponse'),
    newUpdateReceiptRuleResponse,

    -- ** DeleteConfigurationSetEventDestination
    DeleteConfigurationSetEventDestination (DeleteConfigurationSetEventDestination'),
    newDeleteConfigurationSetEventDestination,
    DeleteConfigurationSetEventDestinationResponse (DeleteConfigurationSetEventDestinationResponse'),
    newDeleteConfigurationSetEventDestinationResponse,

    -- ** SetIdentityFeedbackForwardingEnabled
    SetIdentityFeedbackForwardingEnabled (SetIdentityFeedbackForwardingEnabled'),
    newSetIdentityFeedbackForwardingEnabled,
    SetIdentityFeedbackForwardingEnabledResponse (SetIdentityFeedbackForwardingEnabledResponse'),
    newSetIdentityFeedbackForwardingEnabledResponse,

    -- ** SendEmail
    SendEmail (SendEmail'),
    newSendEmail,
    SendEmailResponse (SendEmailResponse'),
    newSendEmailResponse,

    -- ** VerifyEmailAddress
    VerifyEmailAddress (VerifyEmailAddress'),
    newVerifyEmailAddress,
    VerifyEmailAddressResponse (VerifyEmailAddressResponse'),
    newVerifyEmailAddressResponse,

    -- ** DeleteVerifiedEmailAddress
    DeleteVerifiedEmailAddress (DeleteVerifiedEmailAddress'),
    newDeleteVerifiedEmailAddress,
    DeleteVerifiedEmailAddressResponse (DeleteVerifiedEmailAddressResponse'),
    newDeleteVerifiedEmailAddressResponse,

    -- ** ListIdentityPolicies
    ListIdentityPolicies (ListIdentityPolicies'),
    newListIdentityPolicies,
    ListIdentityPoliciesResponse (ListIdentityPoliciesResponse'),
    newListIdentityPoliciesResponse,

    -- ** SetIdentityDkimEnabled
    SetIdentityDkimEnabled (SetIdentityDkimEnabled'),
    newSetIdentityDkimEnabled,
    SetIdentityDkimEnabledResponse (SetIdentityDkimEnabledResponse'),
    newSetIdentityDkimEnabledResponse,

    -- ** UpdateConfigurationSetReputationMetricsEnabled
    UpdateConfigurationSetReputationMetricsEnabled (UpdateConfigurationSetReputationMetricsEnabled'),
    newUpdateConfigurationSetReputationMetricsEnabled,
    UpdateConfigurationSetReputationMetricsEnabledResponse (UpdateConfigurationSetReputationMetricsEnabledResponse'),
    newUpdateConfigurationSetReputationMetricsEnabledResponse,

    -- ** CreateCustomVerificationEmailTemplate
    CreateCustomVerificationEmailTemplate (CreateCustomVerificationEmailTemplate'),
    newCreateCustomVerificationEmailTemplate,
    CreateCustomVerificationEmailTemplateResponse (CreateCustomVerificationEmailTemplateResponse'),
    newCreateCustomVerificationEmailTemplateResponse,

    -- ** DeleteCustomVerificationEmailTemplate
    DeleteCustomVerificationEmailTemplate (DeleteCustomVerificationEmailTemplate'),
    newDeleteCustomVerificationEmailTemplate,
    DeleteCustomVerificationEmailTemplateResponse (DeleteCustomVerificationEmailTemplateResponse'),
    newDeleteCustomVerificationEmailTemplateResponse,

    -- ** UpdateCustomVerificationEmailTemplate
    UpdateCustomVerificationEmailTemplate (UpdateCustomVerificationEmailTemplate'),
    newUpdateCustomVerificationEmailTemplate,
    UpdateCustomVerificationEmailTemplateResponse (UpdateCustomVerificationEmailTemplateResponse'),
    newUpdateCustomVerificationEmailTemplateResponse,

    -- ** ListCustomVerificationEmailTemplates (Paginated)
    ListCustomVerificationEmailTemplates (ListCustomVerificationEmailTemplates'),
    newListCustomVerificationEmailTemplates,
    ListCustomVerificationEmailTemplatesResponse (ListCustomVerificationEmailTemplatesResponse'),
    newListCustomVerificationEmailTemplatesResponse,

    -- ** DeleteIdentity
    DeleteIdentity (DeleteIdentity'),
    newDeleteIdentity,
    DeleteIdentityResponse (DeleteIdentityResponse'),
    newDeleteIdentityResponse,

    -- ** PutIdentityPolicy
    PutIdentityPolicy (PutIdentityPolicy'),
    newPutIdentityPolicy,
    PutIdentityPolicyResponse (PutIdentityPolicyResponse'),
    newPutIdentityPolicyResponse,

    -- ** DeleteConfigurationSetTrackingOptions
    DeleteConfigurationSetTrackingOptions (DeleteConfigurationSetTrackingOptions'),
    newDeleteConfigurationSetTrackingOptions,
    DeleteConfigurationSetTrackingOptionsResponse (DeleteConfigurationSetTrackingOptionsResponse'),
    newDeleteConfigurationSetTrackingOptionsResponse,

    -- ** SendBulkTemplatedEmail
    SendBulkTemplatedEmail (SendBulkTemplatedEmail'),
    newSendBulkTemplatedEmail,
    SendBulkTemplatedEmailResponse (SendBulkTemplatedEmailResponse'),
    newSendBulkTemplatedEmailResponse,

    -- ** VerifyDomainDkim
    VerifyDomainDkim (VerifyDomainDkim'),
    newVerifyDomainDkim,
    VerifyDomainDkimResponse (VerifyDomainDkimResponse'),
    newVerifyDomainDkimResponse,

    -- ** SendTemplatedEmail
    SendTemplatedEmail (SendTemplatedEmail'),
    newSendTemplatedEmail,
    SendTemplatedEmailResponse (SendTemplatedEmailResponse'),
    newSendTemplatedEmailResponse,

    -- ** SendRawEmail
    SendRawEmail (SendRawEmail'),
    newSendRawEmail,
    SendRawEmailResponse (SendRawEmailResponse'),
    newSendRawEmailResponse,

    -- ** SendBounce
    SendBounce (SendBounce'),
    newSendBounce,
    SendBounceResponse (SendBounceResponse'),
    newSendBounceResponse,

    -- ** UpdateConfigurationSetTrackingOptions
    UpdateConfigurationSetTrackingOptions (UpdateConfigurationSetTrackingOptions'),
    newUpdateConfigurationSetTrackingOptions,
    UpdateConfigurationSetTrackingOptionsResponse (UpdateConfigurationSetTrackingOptionsResponse'),
    newUpdateConfigurationSetTrackingOptionsResponse,

    -- ** TestRenderTemplate
    TestRenderTemplate (TestRenderTemplate'),
    newTestRenderTemplate,
    TestRenderTemplateResponse (TestRenderTemplateResponse'),
    newTestRenderTemplateResponse,

    -- ** ReorderReceiptRuleSet
    ReorderReceiptRuleSet (ReorderReceiptRuleSet'),
    newReorderReceiptRuleSet,
    ReorderReceiptRuleSetResponse (ReorderReceiptRuleSetResponse'),
    newReorderReceiptRuleSetResponse,

    -- ** ListReceiptRuleSets (Paginated)
    ListReceiptRuleSets (ListReceiptRuleSets'),
    newListReceiptRuleSets,
    ListReceiptRuleSetsResponse (ListReceiptRuleSetsResponse'),
    newListReceiptRuleSetsResponse,

    -- ** ListTemplates (Paginated)
    ListTemplates (ListTemplates'),
    newListTemplates,
    ListTemplatesResponse (ListTemplatesResponse'),
    newListTemplatesResponse,

    -- ** DescribeActiveReceiptRuleSet
    DescribeActiveReceiptRuleSet (DescribeActiveReceiptRuleSet'),
    newDescribeActiveReceiptRuleSet,
    DescribeActiveReceiptRuleSetResponse (DescribeActiveReceiptRuleSetResponse'),
    newDescribeActiveReceiptRuleSetResponse,

    -- ** GetTemplate
    GetTemplate (GetTemplate'),
    newGetTemplate,
    GetTemplateResponse (GetTemplateResponse'),
    newGetTemplateResponse,

    -- ** CreateReceiptRule
    CreateReceiptRule (CreateReceiptRule'),
    newCreateReceiptRule,
    CreateReceiptRuleResponse (CreateReceiptRuleResponse'),
    newCreateReceiptRuleResponse,

    -- ** SetActiveReceiptRuleSet
    SetActiveReceiptRuleSet (SetActiveReceiptRuleSet'),
    newSetActiveReceiptRuleSet,
    SetActiveReceiptRuleSetResponse (SetActiveReceiptRuleSetResponse'),
    newSetActiveReceiptRuleSetResponse,

    -- ** ListConfigurationSets (Paginated)
    ListConfigurationSets (ListConfigurationSets'),
    newListConfigurationSets,
    ListConfigurationSetsResponse (ListConfigurationSetsResponse'),
    newListConfigurationSetsResponse,

    -- ** SetIdentityMailFromDomain
    SetIdentityMailFromDomain (SetIdentityMailFromDomain'),
    newSetIdentityMailFromDomain,
    SetIdentityMailFromDomainResponse (SetIdentityMailFromDomainResponse'),
    newSetIdentityMailFromDomainResponse,

    -- ** GetIdentityMailFromDomainAttributes
    GetIdentityMailFromDomainAttributes (GetIdentityMailFromDomainAttributes'),
    newGetIdentityMailFromDomainAttributes,
    GetIdentityMailFromDomainAttributesResponse (GetIdentityMailFromDomainAttributesResponse'),
    newGetIdentityMailFromDomainAttributesResponse,

    -- ** GetCustomVerificationEmailTemplate
    GetCustomVerificationEmailTemplate (GetCustomVerificationEmailTemplate'),
    newGetCustomVerificationEmailTemplate,
    GetCustomVerificationEmailTemplateResponse (GetCustomVerificationEmailTemplateResponse'),
    newGetCustomVerificationEmailTemplateResponse,

    -- ** SetIdentityNotificationTopic
    SetIdentityNotificationTopic (SetIdentityNotificationTopic'),
    newSetIdentityNotificationTopic,
    SetIdentityNotificationTopicResponse (SetIdentityNotificationTopicResponse'),
    newSetIdentityNotificationTopicResponse,

    -- ** CreateReceiptFilter
    CreateReceiptFilter (CreateReceiptFilter'),
    newCreateReceiptFilter,
    CreateReceiptFilterResponse (CreateReceiptFilterResponse'),
    newCreateReceiptFilterResponse,

    -- ** ListVerifiedEmailAddresses
    ListVerifiedEmailAddresses (ListVerifiedEmailAddresses'),
    newListVerifiedEmailAddresses,
    ListVerifiedEmailAddressesResponse (ListVerifiedEmailAddressesResponse'),
    newListVerifiedEmailAddressesResponse,

    -- ** DeleteReceiptFilter
    DeleteReceiptFilter (DeleteReceiptFilter'),
    newDeleteReceiptFilter,
    DeleteReceiptFilterResponse (DeleteReceiptFilterResponse'),
    newDeleteReceiptFilterResponse,

    -- ** DescribeReceiptRuleSet
    DescribeReceiptRuleSet (DescribeReceiptRuleSet'),
    newDescribeReceiptRuleSet,
    DescribeReceiptRuleSetResponse (DescribeReceiptRuleSetResponse'),
    newDescribeReceiptRuleSetResponse,

    -- ** ListReceiptFilters
    ListReceiptFilters (ListReceiptFilters'),
    newListReceiptFilters,
    ListReceiptFiltersResponse (ListReceiptFiltersResponse'),
    newListReceiptFiltersResponse,

    -- ** VerifyEmailIdentity
    VerifyEmailIdentity (VerifyEmailIdentity'),
    newVerifyEmailIdentity,
    VerifyEmailIdentityResponse (VerifyEmailIdentityResponse'),
    newVerifyEmailIdentityResponse,

    -- ** UpdateConfigurationSetSendingEnabled
    UpdateConfigurationSetSendingEnabled (UpdateConfigurationSetSendingEnabled'),
    newUpdateConfigurationSetSendingEnabled,
    UpdateConfigurationSetSendingEnabledResponse (UpdateConfigurationSetSendingEnabledResponse'),
    newUpdateConfigurationSetSendingEnabledResponse,

    -- ** ListIdentities (Paginated)
    ListIdentities (ListIdentities'),
    newListIdentities,
    ListIdentitiesResponse (ListIdentitiesResponse'),
    newListIdentitiesResponse,

    -- ** GetIdentityNotificationAttributes
    GetIdentityNotificationAttributes (GetIdentityNotificationAttributes'),
    newGetIdentityNotificationAttributes,
    GetIdentityNotificationAttributesResponse (GetIdentityNotificationAttributesResponse'),
    newGetIdentityNotificationAttributesResponse,

    -- ** SendCustomVerificationEmail
    SendCustomVerificationEmail (SendCustomVerificationEmail'),
    newSendCustomVerificationEmail,
    SendCustomVerificationEmailResponse (SendCustomVerificationEmailResponse'),
    newSendCustomVerificationEmailResponse,

    -- * Types

    -- ** BehaviorOnMXFailure
    BehaviorOnMXFailure (..),

    -- ** BounceType
    BounceType (..),

    -- ** BulkEmailStatus
    BulkEmailStatus (..),

    -- ** ConfigurationSetAttribute
    ConfigurationSetAttribute (..),

    -- ** CustomMailFromStatus
    CustomMailFromStatus (..),

    -- ** DimensionValueSource
    DimensionValueSource (..),

    -- ** DsnAction
    DsnAction (..),

    -- ** EventType
    EventType (..),

    -- ** IdentityType
    IdentityType (..),

    -- ** InvocationType
    InvocationType (..),

    -- ** NotificationType
    NotificationType (..),

    -- ** ReceiptFilterPolicy
    ReceiptFilterPolicy (..),

    -- ** SNSActionEncoding
    SNSActionEncoding (..),

    -- ** StopScope
    StopScope (..),

    -- ** TlsPolicy
    TlsPolicy (..),

    -- ** VerificationStatus
    VerificationStatus (..),

    -- ** AddHeaderAction
    AddHeaderAction (AddHeaderAction'),
    newAddHeaderAction,

    -- ** Body
    Body (Body'),
    newBody,

    -- ** BounceAction
    BounceAction (BounceAction'),
    newBounceAction,

    -- ** BouncedRecipientInfo
    BouncedRecipientInfo (BouncedRecipientInfo'),
    newBouncedRecipientInfo,

    -- ** BulkEmailDestination
    BulkEmailDestination (BulkEmailDestination'),
    newBulkEmailDestination,

    -- ** BulkEmailDestinationStatus
    BulkEmailDestinationStatus (BulkEmailDestinationStatus'),
    newBulkEmailDestinationStatus,

    -- ** CloudWatchDestination
    CloudWatchDestination (CloudWatchDestination'),
    newCloudWatchDestination,

    -- ** CloudWatchDimensionConfiguration
    CloudWatchDimensionConfiguration (CloudWatchDimensionConfiguration'),
    newCloudWatchDimensionConfiguration,

    -- ** ConfigurationSet
    ConfigurationSet (ConfigurationSet'),
    newConfigurationSet,

    -- ** Content
    Content (Content'),
    newContent,

    -- ** CustomVerificationEmailTemplate
    CustomVerificationEmailTemplate (CustomVerificationEmailTemplate'),
    newCustomVerificationEmailTemplate,

    -- ** DeliveryOptions
    DeliveryOptions (DeliveryOptions'),
    newDeliveryOptions,

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** EventDestination
    EventDestination (EventDestination'),
    newEventDestination,

    -- ** ExtensionField
    ExtensionField (ExtensionField'),
    newExtensionField,

    -- ** IdentityDkimAttributes
    IdentityDkimAttributes (IdentityDkimAttributes'),
    newIdentityDkimAttributes,

    -- ** IdentityMailFromDomainAttributes
    IdentityMailFromDomainAttributes (IdentityMailFromDomainAttributes'),
    newIdentityMailFromDomainAttributes,

    -- ** IdentityNotificationAttributes
    IdentityNotificationAttributes (IdentityNotificationAttributes'),
    newIdentityNotificationAttributes,

    -- ** IdentityVerificationAttributes
    IdentityVerificationAttributes (IdentityVerificationAttributes'),
    newIdentityVerificationAttributes,

    -- ** KinesisFirehoseDestination
    KinesisFirehoseDestination (KinesisFirehoseDestination'),
    newKinesisFirehoseDestination,

    -- ** LambdaAction
    LambdaAction (LambdaAction'),
    newLambdaAction,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** MessageDsn
    MessageDsn (MessageDsn'),
    newMessageDsn,

    -- ** MessageTag
    MessageTag (MessageTag'),
    newMessageTag,

    -- ** RawMessage
    RawMessage (RawMessage'),
    newRawMessage,

    -- ** ReceiptAction
    ReceiptAction (ReceiptAction'),
    newReceiptAction,

    -- ** ReceiptFilter
    ReceiptFilter (ReceiptFilter'),
    newReceiptFilter,

    -- ** ReceiptIpFilter
    ReceiptIpFilter (ReceiptIpFilter'),
    newReceiptIpFilter,

    -- ** ReceiptRule
    ReceiptRule (ReceiptRule'),
    newReceiptRule,

    -- ** ReceiptRuleSetMetadata
    ReceiptRuleSetMetadata (ReceiptRuleSetMetadata'),
    newReceiptRuleSetMetadata,

    -- ** RecipientDsnFields
    RecipientDsnFields (RecipientDsnFields'),
    newRecipientDsnFields,

    -- ** ReputationOptions
    ReputationOptions (ReputationOptions'),
    newReputationOptions,

    -- ** S3Action
    S3Action (S3Action'),
    newS3Action,

    -- ** SNSAction
    SNSAction (SNSAction'),
    newSNSAction,

    -- ** SNSDestination
    SNSDestination (SNSDestination'),
    newSNSDestination,

    -- ** SendDataPoint
    SendDataPoint (SendDataPoint'),
    newSendDataPoint,

    -- ** StopAction
    StopAction (StopAction'),
    newStopAction,

    -- ** Template
    Template (Template'),
    newTemplate,

    -- ** TemplateMetadata
    TemplateMetadata (TemplateMetadata'),
    newTemplateMetadata,

    -- ** TrackingOptions
    TrackingOptions (TrackingOptions'),
    newTrackingOptions,

    -- ** WorkmailAction
    WorkmailAction (WorkmailAction'),
    newWorkmailAction,
  )
where

import Network.AWS.SES.CloneReceiptRuleSet
import Network.AWS.SES.CreateConfigurationSet
import Network.AWS.SES.CreateConfigurationSetEventDestination
import Network.AWS.SES.CreateConfigurationSetTrackingOptions
import Network.AWS.SES.CreateCustomVerificationEmailTemplate
import Network.AWS.SES.CreateReceiptFilter
import Network.AWS.SES.CreateReceiptRule
import Network.AWS.SES.CreateReceiptRuleSet
import Network.AWS.SES.CreateTemplate
import Network.AWS.SES.DeleteConfigurationSet
import Network.AWS.SES.DeleteConfigurationSetEventDestination
import Network.AWS.SES.DeleteConfigurationSetTrackingOptions
import Network.AWS.SES.DeleteCustomVerificationEmailTemplate
import Network.AWS.SES.DeleteIdentity
import Network.AWS.SES.DeleteIdentityPolicy
import Network.AWS.SES.DeleteReceiptFilter
import Network.AWS.SES.DeleteReceiptRule
import Network.AWS.SES.DeleteReceiptRuleSet
import Network.AWS.SES.DeleteTemplate
import Network.AWS.SES.DeleteVerifiedEmailAddress
import Network.AWS.SES.DescribeActiveReceiptRuleSet
import Network.AWS.SES.DescribeConfigurationSet
import Network.AWS.SES.DescribeReceiptRule
import Network.AWS.SES.DescribeReceiptRuleSet
import Network.AWS.SES.GetAccountSendingEnabled
import Network.AWS.SES.GetCustomVerificationEmailTemplate
import Network.AWS.SES.GetIdentityDkimAttributes
import Network.AWS.SES.GetIdentityMailFromDomainAttributes
import Network.AWS.SES.GetIdentityNotificationAttributes
import Network.AWS.SES.GetIdentityPolicies
import Network.AWS.SES.GetIdentityVerificationAttributes
import Network.AWS.SES.GetSendQuota
import Network.AWS.SES.GetSendStatistics
import Network.AWS.SES.GetTemplate
import Network.AWS.SES.Lens
import Network.AWS.SES.ListConfigurationSets
import Network.AWS.SES.ListCustomVerificationEmailTemplates
import Network.AWS.SES.ListIdentities
import Network.AWS.SES.ListIdentityPolicies
import Network.AWS.SES.ListReceiptFilters
import Network.AWS.SES.ListReceiptRuleSets
import Network.AWS.SES.ListTemplates
import Network.AWS.SES.ListVerifiedEmailAddresses
import Network.AWS.SES.PutConfigurationSetDeliveryOptions
import Network.AWS.SES.PutIdentityPolicy
import Network.AWS.SES.ReorderReceiptRuleSet
import Network.AWS.SES.SendBounce
import Network.AWS.SES.SendBulkTemplatedEmail
import Network.AWS.SES.SendCustomVerificationEmail
import Network.AWS.SES.SendEmail
import Network.AWS.SES.SendRawEmail
import Network.AWS.SES.SendTemplatedEmail
import Network.AWS.SES.SetActiveReceiptRuleSet
import Network.AWS.SES.SetIdentityDkimEnabled
import Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
import Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled
import Network.AWS.SES.SetIdentityMailFromDomain
import Network.AWS.SES.SetIdentityNotificationTopic
import Network.AWS.SES.SetReceiptRulePosition
import Network.AWS.SES.TestRenderTemplate
import Network.AWS.SES.Types
import Network.AWS.SES.UpdateAccountSendingEnabled
import Network.AWS.SES.UpdateConfigurationSetEventDestination
import Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled
import Network.AWS.SES.UpdateConfigurationSetSendingEnabled
import Network.AWS.SES.UpdateConfigurationSetTrackingOptions
import Network.AWS.SES.UpdateCustomVerificationEmailTemplate
import Network.AWS.SES.UpdateReceiptRule
import Network.AWS.SES.UpdateTemplate
import Network.AWS.SES.VerifyDomainDkim
import Network.AWS.SES.VerifyDomainIdentity
import Network.AWS.SES.VerifyEmailAddress
import Network.AWS.SES.VerifyEmailIdentity
import Network.AWS.SES.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SES'.

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
