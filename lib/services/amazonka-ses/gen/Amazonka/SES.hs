{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SES
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.SES
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccountSendingPausedException
    _AccountSendingPausedException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** CannotDeleteException
    _CannotDeleteException,

    -- ** ConfigurationSetAlreadyExistsException
    _ConfigurationSetAlreadyExistsException,

    -- ** ConfigurationSetDoesNotExistException
    _ConfigurationSetDoesNotExistException,

    -- ** ConfigurationSetSendingPausedException
    _ConfigurationSetSendingPausedException,

    -- ** CustomVerificationEmailInvalidContentException
    _CustomVerificationEmailInvalidContentException,

    -- ** CustomVerificationEmailTemplateAlreadyExistsException
    _CustomVerificationEmailTemplateAlreadyExistsException,

    -- ** CustomVerificationEmailTemplateDoesNotExistException
    _CustomVerificationEmailTemplateDoesNotExistException,

    -- ** EventDestinationAlreadyExistsException
    _EventDestinationAlreadyExistsException,

    -- ** EventDestinationDoesNotExistException
    _EventDestinationDoesNotExistException,

    -- ** FromEmailAddressNotVerifiedException
    _FromEmailAddressNotVerifiedException,

    -- ** InvalidCloudWatchDestinationException
    _InvalidCloudWatchDestinationException,

    -- ** InvalidConfigurationSetException
    _InvalidConfigurationSetException,

    -- ** InvalidDeliveryOptionsException
    _InvalidDeliveryOptionsException,

    -- ** InvalidFirehoseDestinationException
    _InvalidFirehoseDestinationException,

    -- ** InvalidLambdaFunctionException
    _InvalidLambdaFunctionException,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** InvalidRenderingParameterException
    _InvalidRenderingParameterException,

    -- ** InvalidS3ConfigurationException
    _InvalidS3ConfigurationException,

    -- ** InvalidSNSDestinationException
    _InvalidSNSDestinationException,

    -- ** InvalidSnsTopicException
    _InvalidSnsTopicException,

    -- ** InvalidTemplateException
    _InvalidTemplateException,

    -- ** InvalidTrackingOptionsException
    _InvalidTrackingOptionsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MailFromDomainNotVerifiedException
    _MailFromDomainNotVerifiedException,

    -- ** MessageRejected
    _MessageRejected,

    -- ** MissingRenderingAttributeException
    _MissingRenderingAttributeException,

    -- ** ProductionAccessNotGrantedException
    _ProductionAccessNotGrantedException,

    -- ** RuleDoesNotExistException
    _RuleDoesNotExistException,

    -- ** RuleSetDoesNotExistException
    _RuleSetDoesNotExistException,

    -- ** TemplateDoesNotExistException
    _TemplateDoesNotExistException,

    -- ** TrackingOptionsAlreadyExistsException
    _TrackingOptionsAlreadyExistsException,

    -- ** TrackingOptionsDoesNotExistException
    _TrackingOptionsDoesNotExistException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CloneReceiptRuleSet
    CloneReceiptRuleSet (CloneReceiptRuleSet'),
    newCloneReceiptRuleSet,
    CloneReceiptRuleSetResponse (CloneReceiptRuleSetResponse'),
    newCloneReceiptRuleSetResponse,

    -- ** CreateConfigurationSet
    CreateConfigurationSet (CreateConfigurationSet'),
    newCreateConfigurationSet,
    CreateConfigurationSetResponse (CreateConfigurationSetResponse'),
    newCreateConfigurationSetResponse,

    -- ** CreateConfigurationSetEventDestination
    CreateConfigurationSetEventDestination (CreateConfigurationSetEventDestination'),
    newCreateConfigurationSetEventDestination,
    CreateConfigurationSetEventDestinationResponse (CreateConfigurationSetEventDestinationResponse'),
    newCreateConfigurationSetEventDestinationResponse,

    -- ** CreateConfigurationSetTrackingOptions
    CreateConfigurationSetTrackingOptions (CreateConfigurationSetTrackingOptions'),
    newCreateConfigurationSetTrackingOptions,
    CreateConfigurationSetTrackingOptionsResponse (CreateConfigurationSetTrackingOptionsResponse'),
    newCreateConfigurationSetTrackingOptionsResponse,

    -- ** CreateCustomVerificationEmailTemplate
    CreateCustomVerificationEmailTemplate (CreateCustomVerificationEmailTemplate'),
    newCreateCustomVerificationEmailTemplate,
    CreateCustomVerificationEmailTemplateResponse (CreateCustomVerificationEmailTemplateResponse'),
    newCreateCustomVerificationEmailTemplateResponse,

    -- ** CreateReceiptFilter
    CreateReceiptFilter (CreateReceiptFilter'),
    newCreateReceiptFilter,
    CreateReceiptFilterResponse (CreateReceiptFilterResponse'),
    newCreateReceiptFilterResponse,

    -- ** CreateReceiptRule
    CreateReceiptRule (CreateReceiptRule'),
    newCreateReceiptRule,
    CreateReceiptRuleResponse (CreateReceiptRuleResponse'),
    newCreateReceiptRuleResponse,

    -- ** CreateReceiptRuleSet
    CreateReceiptRuleSet (CreateReceiptRuleSet'),
    newCreateReceiptRuleSet,
    CreateReceiptRuleSetResponse (CreateReceiptRuleSetResponse'),
    newCreateReceiptRuleSetResponse,

    -- ** CreateTemplate
    CreateTemplate (CreateTemplate'),
    newCreateTemplate,
    CreateTemplateResponse (CreateTemplateResponse'),
    newCreateTemplateResponse,

    -- ** DeleteConfigurationSet
    DeleteConfigurationSet (DeleteConfigurationSet'),
    newDeleteConfigurationSet,
    DeleteConfigurationSetResponse (DeleteConfigurationSetResponse'),
    newDeleteConfigurationSetResponse,

    -- ** DeleteConfigurationSetEventDestination
    DeleteConfigurationSetEventDestination (DeleteConfigurationSetEventDestination'),
    newDeleteConfigurationSetEventDestination,
    DeleteConfigurationSetEventDestinationResponse (DeleteConfigurationSetEventDestinationResponse'),
    newDeleteConfigurationSetEventDestinationResponse,

    -- ** DeleteConfigurationSetTrackingOptions
    DeleteConfigurationSetTrackingOptions (DeleteConfigurationSetTrackingOptions'),
    newDeleteConfigurationSetTrackingOptions,
    DeleteConfigurationSetTrackingOptionsResponse (DeleteConfigurationSetTrackingOptionsResponse'),
    newDeleteConfigurationSetTrackingOptionsResponse,

    -- ** DeleteCustomVerificationEmailTemplate
    DeleteCustomVerificationEmailTemplate (DeleteCustomVerificationEmailTemplate'),
    newDeleteCustomVerificationEmailTemplate,
    DeleteCustomVerificationEmailTemplateResponse (DeleteCustomVerificationEmailTemplateResponse'),
    newDeleteCustomVerificationEmailTemplateResponse,

    -- ** DeleteIdentity
    DeleteIdentity (DeleteIdentity'),
    newDeleteIdentity,
    DeleteIdentityResponse (DeleteIdentityResponse'),
    newDeleteIdentityResponse,

    -- ** DeleteIdentityPolicy
    DeleteIdentityPolicy (DeleteIdentityPolicy'),
    newDeleteIdentityPolicy,
    DeleteIdentityPolicyResponse (DeleteIdentityPolicyResponse'),
    newDeleteIdentityPolicyResponse,

    -- ** DeleteReceiptFilter
    DeleteReceiptFilter (DeleteReceiptFilter'),
    newDeleteReceiptFilter,
    DeleteReceiptFilterResponse (DeleteReceiptFilterResponse'),
    newDeleteReceiptFilterResponse,

    -- ** DeleteReceiptRule
    DeleteReceiptRule (DeleteReceiptRule'),
    newDeleteReceiptRule,
    DeleteReceiptRuleResponse (DeleteReceiptRuleResponse'),
    newDeleteReceiptRuleResponse,

    -- ** DeleteReceiptRuleSet
    DeleteReceiptRuleSet (DeleteReceiptRuleSet'),
    newDeleteReceiptRuleSet,
    DeleteReceiptRuleSetResponse (DeleteReceiptRuleSetResponse'),
    newDeleteReceiptRuleSetResponse,

    -- ** DeleteTemplate
    DeleteTemplate (DeleteTemplate'),
    newDeleteTemplate,
    DeleteTemplateResponse (DeleteTemplateResponse'),
    newDeleteTemplateResponse,

    -- ** DeleteVerifiedEmailAddress
    DeleteVerifiedEmailAddress (DeleteVerifiedEmailAddress'),
    newDeleteVerifiedEmailAddress,
    DeleteVerifiedEmailAddressResponse (DeleteVerifiedEmailAddressResponse'),
    newDeleteVerifiedEmailAddressResponse,

    -- ** DescribeActiveReceiptRuleSet
    DescribeActiveReceiptRuleSet (DescribeActiveReceiptRuleSet'),
    newDescribeActiveReceiptRuleSet,
    DescribeActiveReceiptRuleSetResponse (DescribeActiveReceiptRuleSetResponse'),
    newDescribeActiveReceiptRuleSetResponse,

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

    -- ** DescribeReceiptRuleSet
    DescribeReceiptRuleSet (DescribeReceiptRuleSet'),
    newDescribeReceiptRuleSet,
    DescribeReceiptRuleSetResponse (DescribeReceiptRuleSetResponse'),
    newDescribeReceiptRuleSetResponse,

    -- ** GetAccountSendingEnabled
    GetAccountSendingEnabled (GetAccountSendingEnabled'),
    newGetAccountSendingEnabled,
    GetAccountSendingEnabledResponse (GetAccountSendingEnabledResponse'),
    newGetAccountSendingEnabledResponse,

    -- ** GetCustomVerificationEmailTemplate
    GetCustomVerificationEmailTemplate (GetCustomVerificationEmailTemplate'),
    newGetCustomVerificationEmailTemplate,
    GetCustomVerificationEmailTemplateResponse (GetCustomVerificationEmailTemplateResponse'),
    newGetCustomVerificationEmailTemplateResponse,

    -- ** GetIdentityDkimAttributes
    GetIdentityDkimAttributes (GetIdentityDkimAttributes'),
    newGetIdentityDkimAttributes,
    GetIdentityDkimAttributesResponse (GetIdentityDkimAttributesResponse'),
    newGetIdentityDkimAttributesResponse,

    -- ** GetIdentityMailFromDomainAttributes
    GetIdentityMailFromDomainAttributes (GetIdentityMailFromDomainAttributes'),
    newGetIdentityMailFromDomainAttributes,
    GetIdentityMailFromDomainAttributesResponse (GetIdentityMailFromDomainAttributesResponse'),
    newGetIdentityMailFromDomainAttributesResponse,

    -- ** GetIdentityNotificationAttributes
    GetIdentityNotificationAttributes (GetIdentityNotificationAttributes'),
    newGetIdentityNotificationAttributes,
    GetIdentityNotificationAttributesResponse (GetIdentityNotificationAttributesResponse'),
    newGetIdentityNotificationAttributesResponse,

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

    -- ** GetSendQuota
    GetSendQuota (GetSendQuota'),
    newGetSendQuota,
    GetSendQuotaResponse (GetSendQuotaResponse'),
    newGetSendQuotaResponse,

    -- ** GetSendStatistics
    GetSendStatistics (GetSendStatistics'),
    newGetSendStatistics,
    GetSendStatisticsResponse (GetSendStatisticsResponse'),
    newGetSendStatisticsResponse,

    -- ** GetTemplate
    GetTemplate (GetTemplate'),
    newGetTemplate,
    GetTemplateResponse (GetTemplateResponse'),
    newGetTemplateResponse,

    -- ** ListConfigurationSets (Paginated)
    ListConfigurationSets (ListConfigurationSets'),
    newListConfigurationSets,
    ListConfigurationSetsResponse (ListConfigurationSetsResponse'),
    newListConfigurationSetsResponse,

    -- ** ListCustomVerificationEmailTemplates (Paginated)
    ListCustomVerificationEmailTemplates (ListCustomVerificationEmailTemplates'),
    newListCustomVerificationEmailTemplates,
    ListCustomVerificationEmailTemplatesResponse (ListCustomVerificationEmailTemplatesResponse'),
    newListCustomVerificationEmailTemplatesResponse,

    -- ** ListIdentities (Paginated)
    ListIdentities (ListIdentities'),
    newListIdentities,
    ListIdentitiesResponse (ListIdentitiesResponse'),
    newListIdentitiesResponse,

    -- ** ListIdentityPolicies
    ListIdentityPolicies (ListIdentityPolicies'),
    newListIdentityPolicies,
    ListIdentityPoliciesResponse (ListIdentityPoliciesResponse'),
    newListIdentityPoliciesResponse,

    -- ** ListReceiptFilters
    ListReceiptFilters (ListReceiptFilters'),
    newListReceiptFilters,
    ListReceiptFiltersResponse (ListReceiptFiltersResponse'),
    newListReceiptFiltersResponse,

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

    -- ** ListVerifiedEmailAddresses
    ListVerifiedEmailAddresses (ListVerifiedEmailAddresses'),
    newListVerifiedEmailAddresses,
    ListVerifiedEmailAddressesResponse (ListVerifiedEmailAddressesResponse'),
    newListVerifiedEmailAddressesResponse,

    -- ** PutConfigurationSetDeliveryOptions
    PutConfigurationSetDeliveryOptions (PutConfigurationSetDeliveryOptions'),
    newPutConfigurationSetDeliveryOptions,
    PutConfigurationSetDeliveryOptionsResponse (PutConfigurationSetDeliveryOptionsResponse'),
    newPutConfigurationSetDeliveryOptionsResponse,

    -- ** PutIdentityPolicy
    PutIdentityPolicy (PutIdentityPolicy'),
    newPutIdentityPolicy,
    PutIdentityPolicyResponse (PutIdentityPolicyResponse'),
    newPutIdentityPolicyResponse,

    -- ** ReorderReceiptRuleSet
    ReorderReceiptRuleSet (ReorderReceiptRuleSet'),
    newReorderReceiptRuleSet,
    ReorderReceiptRuleSetResponse (ReorderReceiptRuleSetResponse'),
    newReorderReceiptRuleSetResponse,

    -- ** SendBounce
    SendBounce (SendBounce'),
    newSendBounce,
    SendBounceResponse (SendBounceResponse'),
    newSendBounceResponse,

    -- ** SendBulkTemplatedEmail
    SendBulkTemplatedEmail (SendBulkTemplatedEmail'),
    newSendBulkTemplatedEmail,
    SendBulkTemplatedEmailResponse (SendBulkTemplatedEmailResponse'),
    newSendBulkTemplatedEmailResponse,

    -- ** SendCustomVerificationEmail
    SendCustomVerificationEmail (SendCustomVerificationEmail'),
    newSendCustomVerificationEmail,
    SendCustomVerificationEmailResponse (SendCustomVerificationEmailResponse'),
    newSendCustomVerificationEmailResponse,

    -- ** SendEmail
    SendEmail (SendEmail'),
    newSendEmail,
    SendEmailResponse (SendEmailResponse'),
    newSendEmailResponse,

    -- ** SendRawEmail
    SendRawEmail (SendRawEmail'),
    newSendRawEmail,
    SendRawEmailResponse (SendRawEmailResponse'),
    newSendRawEmailResponse,

    -- ** SendTemplatedEmail
    SendTemplatedEmail (SendTemplatedEmail'),
    newSendTemplatedEmail,
    SendTemplatedEmailResponse (SendTemplatedEmailResponse'),
    newSendTemplatedEmailResponse,

    -- ** SetActiveReceiptRuleSet
    SetActiveReceiptRuleSet (SetActiveReceiptRuleSet'),
    newSetActiveReceiptRuleSet,
    SetActiveReceiptRuleSetResponse (SetActiveReceiptRuleSetResponse'),
    newSetActiveReceiptRuleSetResponse,

    -- ** SetIdentityDkimEnabled
    SetIdentityDkimEnabled (SetIdentityDkimEnabled'),
    newSetIdentityDkimEnabled,
    SetIdentityDkimEnabledResponse (SetIdentityDkimEnabledResponse'),
    newSetIdentityDkimEnabledResponse,

    -- ** SetIdentityFeedbackForwardingEnabled
    SetIdentityFeedbackForwardingEnabled (SetIdentityFeedbackForwardingEnabled'),
    newSetIdentityFeedbackForwardingEnabled,
    SetIdentityFeedbackForwardingEnabledResponse (SetIdentityFeedbackForwardingEnabledResponse'),
    newSetIdentityFeedbackForwardingEnabledResponse,

    -- ** SetIdentityHeadersInNotificationsEnabled
    SetIdentityHeadersInNotificationsEnabled (SetIdentityHeadersInNotificationsEnabled'),
    newSetIdentityHeadersInNotificationsEnabled,
    SetIdentityHeadersInNotificationsEnabledResponse (SetIdentityHeadersInNotificationsEnabledResponse'),
    newSetIdentityHeadersInNotificationsEnabledResponse,

    -- ** SetIdentityMailFromDomain
    SetIdentityMailFromDomain (SetIdentityMailFromDomain'),
    newSetIdentityMailFromDomain,
    SetIdentityMailFromDomainResponse (SetIdentityMailFromDomainResponse'),
    newSetIdentityMailFromDomainResponse,

    -- ** SetIdentityNotificationTopic
    SetIdentityNotificationTopic (SetIdentityNotificationTopic'),
    newSetIdentityNotificationTopic,
    SetIdentityNotificationTopicResponse (SetIdentityNotificationTopicResponse'),
    newSetIdentityNotificationTopicResponse,

    -- ** SetReceiptRulePosition
    SetReceiptRulePosition (SetReceiptRulePosition'),
    newSetReceiptRulePosition,
    SetReceiptRulePositionResponse (SetReceiptRulePositionResponse'),
    newSetReceiptRulePositionResponse,

    -- ** TestRenderTemplate
    TestRenderTemplate (TestRenderTemplate'),
    newTestRenderTemplate,
    TestRenderTemplateResponse (TestRenderTemplateResponse'),
    newTestRenderTemplateResponse,

    -- ** UpdateAccountSendingEnabled
    UpdateAccountSendingEnabled (UpdateAccountSendingEnabled'),
    newUpdateAccountSendingEnabled,
    UpdateAccountSendingEnabledResponse (UpdateAccountSendingEnabledResponse'),
    newUpdateAccountSendingEnabledResponse,

    -- ** UpdateConfigurationSetEventDestination
    UpdateConfigurationSetEventDestination (UpdateConfigurationSetEventDestination'),
    newUpdateConfigurationSetEventDestination,
    UpdateConfigurationSetEventDestinationResponse (UpdateConfigurationSetEventDestinationResponse'),
    newUpdateConfigurationSetEventDestinationResponse,

    -- ** UpdateConfigurationSetReputationMetricsEnabled
    UpdateConfigurationSetReputationMetricsEnabled (UpdateConfigurationSetReputationMetricsEnabled'),
    newUpdateConfigurationSetReputationMetricsEnabled,
    UpdateConfigurationSetReputationMetricsEnabledResponse (UpdateConfigurationSetReputationMetricsEnabledResponse'),
    newUpdateConfigurationSetReputationMetricsEnabledResponse,

    -- ** UpdateConfigurationSetSendingEnabled
    UpdateConfigurationSetSendingEnabled (UpdateConfigurationSetSendingEnabled'),
    newUpdateConfigurationSetSendingEnabled,
    UpdateConfigurationSetSendingEnabledResponse (UpdateConfigurationSetSendingEnabledResponse'),
    newUpdateConfigurationSetSendingEnabledResponse,

    -- ** UpdateConfigurationSetTrackingOptions
    UpdateConfigurationSetTrackingOptions (UpdateConfigurationSetTrackingOptions'),
    newUpdateConfigurationSetTrackingOptions,
    UpdateConfigurationSetTrackingOptionsResponse (UpdateConfigurationSetTrackingOptionsResponse'),
    newUpdateConfigurationSetTrackingOptionsResponse,

    -- ** UpdateCustomVerificationEmailTemplate
    UpdateCustomVerificationEmailTemplate (UpdateCustomVerificationEmailTemplate'),
    newUpdateCustomVerificationEmailTemplate,
    UpdateCustomVerificationEmailTemplateResponse (UpdateCustomVerificationEmailTemplateResponse'),
    newUpdateCustomVerificationEmailTemplateResponse,

    -- ** UpdateReceiptRule
    UpdateReceiptRule (UpdateReceiptRule'),
    newUpdateReceiptRule,
    UpdateReceiptRuleResponse (UpdateReceiptRuleResponse'),
    newUpdateReceiptRuleResponse,

    -- ** UpdateTemplate
    UpdateTemplate (UpdateTemplate'),
    newUpdateTemplate,
    UpdateTemplateResponse (UpdateTemplateResponse'),
    newUpdateTemplateResponse,

    -- ** VerifyDomainDkim
    VerifyDomainDkim (VerifyDomainDkim'),
    newVerifyDomainDkim,
    VerifyDomainDkimResponse (VerifyDomainDkimResponse'),
    newVerifyDomainDkimResponse,

    -- ** VerifyDomainIdentity
    VerifyDomainIdentity (VerifyDomainIdentity'),
    newVerifyDomainIdentity,
    VerifyDomainIdentityResponse (VerifyDomainIdentityResponse'),
    newVerifyDomainIdentityResponse,

    -- ** VerifyEmailAddress
    VerifyEmailAddress (VerifyEmailAddress'),
    newVerifyEmailAddress,
    VerifyEmailAddressResponse (VerifyEmailAddressResponse'),
    newVerifyEmailAddressResponse,

    -- ** VerifyEmailIdentity
    VerifyEmailIdentity (VerifyEmailIdentity'),
    newVerifyEmailIdentity,
    VerifyEmailIdentityResponse (VerifyEmailIdentityResponse'),
    newVerifyEmailIdentityResponse,

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

import Amazonka.SES.CloneReceiptRuleSet
import Amazonka.SES.CreateConfigurationSet
import Amazonka.SES.CreateConfigurationSetEventDestination
import Amazonka.SES.CreateConfigurationSetTrackingOptions
import Amazonka.SES.CreateCustomVerificationEmailTemplate
import Amazonka.SES.CreateReceiptFilter
import Amazonka.SES.CreateReceiptRule
import Amazonka.SES.CreateReceiptRuleSet
import Amazonka.SES.CreateTemplate
import Amazonka.SES.DeleteConfigurationSet
import Amazonka.SES.DeleteConfigurationSetEventDestination
import Amazonka.SES.DeleteConfigurationSetTrackingOptions
import Amazonka.SES.DeleteCustomVerificationEmailTemplate
import Amazonka.SES.DeleteIdentity
import Amazonka.SES.DeleteIdentityPolicy
import Amazonka.SES.DeleteReceiptFilter
import Amazonka.SES.DeleteReceiptRule
import Amazonka.SES.DeleteReceiptRuleSet
import Amazonka.SES.DeleteTemplate
import Amazonka.SES.DeleteVerifiedEmailAddress
import Amazonka.SES.DescribeActiveReceiptRuleSet
import Amazonka.SES.DescribeConfigurationSet
import Amazonka.SES.DescribeReceiptRule
import Amazonka.SES.DescribeReceiptRuleSet
import Amazonka.SES.GetAccountSendingEnabled
import Amazonka.SES.GetCustomVerificationEmailTemplate
import Amazonka.SES.GetIdentityDkimAttributes
import Amazonka.SES.GetIdentityMailFromDomainAttributes
import Amazonka.SES.GetIdentityNotificationAttributes
import Amazonka.SES.GetIdentityPolicies
import Amazonka.SES.GetIdentityVerificationAttributes
import Amazonka.SES.GetSendQuota
import Amazonka.SES.GetSendStatistics
import Amazonka.SES.GetTemplate
import Amazonka.SES.Lens
import Amazonka.SES.ListConfigurationSets
import Amazonka.SES.ListCustomVerificationEmailTemplates
import Amazonka.SES.ListIdentities
import Amazonka.SES.ListIdentityPolicies
import Amazonka.SES.ListReceiptFilters
import Amazonka.SES.ListReceiptRuleSets
import Amazonka.SES.ListTemplates
import Amazonka.SES.ListVerifiedEmailAddresses
import Amazonka.SES.PutConfigurationSetDeliveryOptions
import Amazonka.SES.PutIdentityPolicy
import Amazonka.SES.ReorderReceiptRuleSet
import Amazonka.SES.SendBounce
import Amazonka.SES.SendBulkTemplatedEmail
import Amazonka.SES.SendCustomVerificationEmail
import Amazonka.SES.SendEmail
import Amazonka.SES.SendRawEmail
import Amazonka.SES.SendTemplatedEmail
import Amazonka.SES.SetActiveReceiptRuleSet
import Amazonka.SES.SetIdentityDkimEnabled
import Amazonka.SES.SetIdentityFeedbackForwardingEnabled
import Amazonka.SES.SetIdentityHeadersInNotificationsEnabled
import Amazonka.SES.SetIdentityMailFromDomain
import Amazonka.SES.SetIdentityNotificationTopic
import Amazonka.SES.SetReceiptRulePosition
import Amazonka.SES.TestRenderTemplate
import Amazonka.SES.Types
import Amazonka.SES.UpdateAccountSendingEnabled
import Amazonka.SES.UpdateConfigurationSetEventDestination
import Amazonka.SES.UpdateConfigurationSetReputationMetricsEnabled
import Amazonka.SES.UpdateConfigurationSetSendingEnabled
import Amazonka.SES.UpdateConfigurationSetTrackingOptions
import Amazonka.SES.UpdateCustomVerificationEmailTemplate
import Amazonka.SES.UpdateReceiptRule
import Amazonka.SES.UpdateTemplate
import Amazonka.SES.VerifyDomainDkim
import Amazonka.SES.VerifyDomainIdentity
import Amazonka.SES.VerifyEmailAddress
import Amazonka.SES.VerifyEmailIdentity
import Amazonka.SES.Waiters

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
