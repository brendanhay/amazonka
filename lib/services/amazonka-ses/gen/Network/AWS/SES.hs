{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SES
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
module Amazonka.SES
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidConfigurationSetException
    _InvalidConfigurationSetException,

    -- ** InvalidSNSDestinationException
    _InvalidSNSDestinationException,

    -- ** TemplateDoesNotExistException
    _TemplateDoesNotExistException,

    -- ** ConfigurationSetSendingPausedException
    _ConfigurationSetSendingPausedException,

    -- ** CannotDeleteException
    _CannotDeleteException,

    -- ** ProductionAccessNotGrantedException
    _ProductionAccessNotGrantedException,

    -- ** RuleDoesNotExistException
    _RuleDoesNotExistException,

    -- ** MessageRejected
    _MessageRejected,

    -- ** InvalidRenderingParameterException
    _InvalidRenderingParameterException,

    -- ** MissingRenderingAttributeException
    _MissingRenderingAttributeException,

    -- ** FromEmailAddressNotVerifiedException
    _FromEmailAddressNotVerifiedException,

    -- ** RuleSetDoesNotExistException
    _RuleSetDoesNotExistException,

    -- ** MailFromDomainNotVerifiedException
    _MailFromDomainNotVerifiedException,

    -- ** InvalidFirehoseDestinationException
    _InvalidFirehoseDestinationException,

    -- ** ConfigurationSetAlreadyExistsException
    _ConfigurationSetAlreadyExistsException,

    -- ** CustomVerificationEmailInvalidContentException
    _CustomVerificationEmailInvalidContentException,

    -- ** InvalidTrackingOptionsException
    _InvalidTrackingOptionsException,

    -- ** AccountSendingPausedException
    _AccountSendingPausedException,

    -- ** EventDestinationDoesNotExistException
    _EventDestinationDoesNotExistException,

    -- ** CustomVerificationEmailTemplateAlreadyExistsException
    _CustomVerificationEmailTemplateAlreadyExistsException,

    -- ** CustomVerificationEmailTemplateDoesNotExistException
    _CustomVerificationEmailTemplateDoesNotExistException,

    -- ** InvalidCloudWatchDestinationException
    _InvalidCloudWatchDestinationException,

    -- ** InvalidDeliveryOptionsException
    _InvalidDeliveryOptionsException,

    -- ** InvalidLambdaFunctionException
    _InvalidLambdaFunctionException,

    -- ** TrackingOptionsDoesNotExistException
    _TrackingOptionsDoesNotExistException,

    -- ** InvalidTemplateException
    _InvalidTemplateException,

    -- ** ConfigurationSetDoesNotExistException
    _ConfigurationSetDoesNotExistException,

    -- ** InvalidPolicyException
    _InvalidPolicyException,

    -- ** InvalidS3ConfigurationException
    _InvalidS3ConfigurationException,

    -- ** TrackingOptionsAlreadyExistsException
    _TrackingOptionsAlreadyExistsException,

    -- ** InvalidSnsTopicException
    _InvalidSnsTopicException,

    -- ** EventDestinationAlreadyExistsException
    _EventDestinationAlreadyExistsException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateTemplate
    CreateTemplate (CreateTemplate'),
    newCreateTemplate,
    CreateTemplateResponse (CreateTemplateResponse'),
    newCreateTemplateResponse,

    -- ** DeleteConfigurationSetTrackingOptions
    DeleteConfigurationSetTrackingOptions (DeleteConfigurationSetTrackingOptions'),
    newDeleteConfigurationSetTrackingOptions,
    DeleteConfigurationSetTrackingOptionsResponse (DeleteConfigurationSetTrackingOptionsResponse'),
    newDeleteConfigurationSetTrackingOptionsResponse,

    -- ** UpdateConfigurationSetTrackingOptions
    UpdateConfigurationSetTrackingOptions (UpdateConfigurationSetTrackingOptions'),
    newUpdateConfigurationSetTrackingOptions,
    UpdateConfigurationSetTrackingOptionsResponse (UpdateConfigurationSetTrackingOptionsResponse'),
    newUpdateConfigurationSetTrackingOptionsResponse,

    -- ** CreateReceiptRuleSet
    CreateReceiptRuleSet (CreateReceiptRuleSet'),
    newCreateReceiptRuleSet,
    CreateReceiptRuleSetResponse (CreateReceiptRuleSetResponse'),
    newCreateReceiptRuleSetResponse,

    -- ** SetIdentityHeadersInNotificationsEnabled
    SetIdentityHeadersInNotificationsEnabled (SetIdentityHeadersInNotificationsEnabled'),
    newSetIdentityHeadersInNotificationsEnabled,
    SetIdentityHeadersInNotificationsEnabledResponse (SetIdentityHeadersInNotificationsEnabledResponse'),
    newSetIdentityHeadersInNotificationsEnabledResponse,

    -- ** GetSendQuota
    GetSendQuota (GetSendQuota'),
    newGetSendQuota,
    GetSendQuotaResponse (GetSendQuotaResponse'),
    newGetSendQuotaResponse,

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

    -- ** PutIdentityPolicy
    PutIdentityPolicy (PutIdentityPolicy'),
    newPutIdentityPolicy,
    PutIdentityPolicyResponse (PutIdentityPolicyResponse'),
    newPutIdentityPolicyResponse,

    -- ** DeleteCustomVerificationEmailTemplate
    DeleteCustomVerificationEmailTemplate (DeleteCustomVerificationEmailTemplate'),
    newDeleteCustomVerificationEmailTemplate,
    DeleteCustomVerificationEmailTemplateResponse (DeleteCustomVerificationEmailTemplateResponse'),
    newDeleteCustomVerificationEmailTemplateResponse,

    -- ** DeleteIdentityPolicy
    DeleteIdentityPolicy (DeleteIdentityPolicy'),
    newDeleteIdentityPolicy,
    DeleteIdentityPolicyResponse (DeleteIdentityPolicyResponse'),
    newDeleteIdentityPolicyResponse,

    -- ** UpdateCustomVerificationEmailTemplate
    UpdateCustomVerificationEmailTemplate (UpdateCustomVerificationEmailTemplate'),
    newUpdateCustomVerificationEmailTemplate,
    UpdateCustomVerificationEmailTemplateResponse (UpdateCustomVerificationEmailTemplateResponse'),
    newUpdateCustomVerificationEmailTemplateResponse,

    -- ** SendCustomVerificationEmail
    SendCustomVerificationEmail (SendCustomVerificationEmail'),
    newSendCustomVerificationEmail,
    SendCustomVerificationEmailResponse (SendCustomVerificationEmailResponse'),
    newSendCustomVerificationEmailResponse,

    -- ** GetIdentityNotificationAttributes
    GetIdentityNotificationAttributes (GetIdentityNotificationAttributes'),
    newGetIdentityNotificationAttributes,
    GetIdentityNotificationAttributesResponse (GetIdentityNotificationAttributesResponse'),
    newGetIdentityNotificationAttributesResponse,

    -- ** UpdateConfigurationSetReputationMetricsEnabled
    UpdateConfigurationSetReputationMetricsEnabled (UpdateConfigurationSetReputationMetricsEnabled'),
    newUpdateConfigurationSetReputationMetricsEnabled,
    UpdateConfigurationSetReputationMetricsEnabledResponse (UpdateConfigurationSetReputationMetricsEnabledResponse'),
    newUpdateConfigurationSetReputationMetricsEnabledResponse,

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

    -- ** ListReceiptFilters
    ListReceiptFilters (ListReceiptFilters'),
    newListReceiptFilters,
    ListReceiptFiltersResponse (ListReceiptFiltersResponse'),
    newListReceiptFiltersResponse,

    -- ** DescribeReceiptRuleSet
    DescribeReceiptRuleSet (DescribeReceiptRuleSet'),
    newDescribeReceiptRuleSet,
    DescribeReceiptRuleSetResponse (DescribeReceiptRuleSetResponse'),
    newDescribeReceiptRuleSetResponse,

    -- ** GetIdentityMailFromDomainAttributes
    GetIdentityMailFromDomainAttributes (GetIdentityMailFromDomainAttributes'),
    newGetIdentityMailFromDomainAttributes,
    GetIdentityMailFromDomainAttributesResponse (GetIdentityMailFromDomainAttributesResponse'),
    newGetIdentityMailFromDomainAttributesResponse,

    -- ** CreateReceiptFilter
    CreateReceiptFilter (CreateReceiptFilter'),
    newCreateReceiptFilter,
    CreateReceiptFilterResponse (CreateReceiptFilterResponse'),
    newCreateReceiptFilterResponse,

    -- ** UpdateConfigurationSetEventDestination
    UpdateConfigurationSetEventDestination (UpdateConfigurationSetEventDestination'),
    newUpdateConfigurationSetEventDestination,
    UpdateConfigurationSetEventDestinationResponse (UpdateConfigurationSetEventDestinationResponse'),
    newUpdateConfigurationSetEventDestinationResponse,

    -- ** DeleteConfigurationSetEventDestination
    DeleteConfigurationSetEventDestination (DeleteConfigurationSetEventDestination'),
    newDeleteConfigurationSetEventDestination,
    DeleteConfigurationSetEventDestinationResponse (DeleteConfigurationSetEventDestinationResponse'),
    newDeleteConfigurationSetEventDestinationResponse,

    -- ** SetIdentityMailFromDomain
    SetIdentityMailFromDomain (SetIdentityMailFromDomain'),
    newSetIdentityMailFromDomain,
    SetIdentityMailFromDomainResponse (SetIdentityMailFromDomainResponse'),
    newSetIdentityMailFromDomainResponse,

    -- ** SetIdentityFeedbackForwardingEnabled
    SetIdentityFeedbackForwardingEnabled (SetIdentityFeedbackForwardingEnabled'),
    newSetIdentityFeedbackForwardingEnabled,
    SetIdentityFeedbackForwardingEnabledResponse (SetIdentityFeedbackForwardingEnabledResponse'),
    newSetIdentityFeedbackForwardingEnabledResponse,

    -- ** ListConfigurationSets (Paginated)
    ListConfigurationSets (ListConfigurationSets'),
    newListConfigurationSets,
    ListConfigurationSetsResponse (ListConfigurationSetsResponse'),
    newListConfigurationSetsResponse,

    -- ** DeleteConfigurationSet
    DeleteConfigurationSet (DeleteConfigurationSet'),
    newDeleteConfigurationSet,
    DeleteConfigurationSetResponse (DeleteConfigurationSetResponse'),
    newDeleteConfigurationSetResponse,

    -- ** GetIdentityVerificationAttributes
    GetIdentityVerificationAttributes (GetIdentityVerificationAttributes'),
    newGetIdentityVerificationAttributes,
    GetIdentityVerificationAttributesResponse (GetIdentityVerificationAttributesResponse'),
    newGetIdentityVerificationAttributesResponse,

    -- ** GetIdentityPolicies
    GetIdentityPolicies (GetIdentityPolicies'),
    newGetIdentityPolicies,
    GetIdentityPoliciesResponse (GetIdentityPoliciesResponse'),
    newGetIdentityPoliciesResponse,

    -- ** ListTemplates (Paginated)
    ListTemplates (ListTemplates'),
    newListTemplates,
    ListTemplatesResponse (ListTemplatesResponse'),
    newListTemplatesResponse,

    -- ** VerifyDomainIdentity
    VerifyDomainIdentity (VerifyDomainIdentity'),
    newVerifyDomainIdentity,
    VerifyDomainIdentityResponse (VerifyDomainIdentityResponse'),
    newVerifyDomainIdentityResponse,

    -- ** UpdateTemplate
    UpdateTemplate (UpdateTemplate'),
    newUpdateTemplate,
    UpdateTemplateResponse (UpdateTemplateResponse'),
    newUpdateTemplateResponse,

    -- ** DeleteTemplate
    DeleteTemplate (DeleteTemplate'),
    newDeleteTemplate,
    DeleteTemplateResponse (DeleteTemplateResponse'),
    newDeleteTemplateResponse,

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

    -- ** DeleteReceiptRuleSet
    DeleteReceiptRuleSet (DeleteReceiptRuleSet'),
    newDeleteReceiptRuleSet,
    DeleteReceiptRuleSetResponse (DeleteReceiptRuleSetResponse'),
    newDeleteReceiptRuleSetResponse,

    -- ** SetReceiptRulePosition
    SetReceiptRulePosition (SetReceiptRulePosition'),
    newSetReceiptRulePosition,
    SetReceiptRulePositionResponse (SetReceiptRulePositionResponse'),
    newSetReceiptRulePositionResponse,

    -- ** SendBounce
    SendBounce (SendBounce'),
    newSendBounce,
    SendBounceResponse (SendBounceResponse'),
    newSendBounceResponse,

    -- ** GetIdentityDkimAttributes
    GetIdentityDkimAttributes (GetIdentityDkimAttributes'),
    newGetIdentityDkimAttributes,
    GetIdentityDkimAttributesResponse (GetIdentityDkimAttributesResponse'),
    newGetIdentityDkimAttributesResponse,

    -- ** SendTemplatedEmail
    SendTemplatedEmail (SendTemplatedEmail'),
    newSendTemplatedEmail,
    SendTemplatedEmailResponse (SendTemplatedEmailResponse'),
    newSendTemplatedEmailResponse,

    -- ** VerifyDomainDkim
    VerifyDomainDkim (VerifyDomainDkim'),
    newVerifyDomainDkim,
    VerifyDomainDkimResponse (VerifyDomainDkimResponse'),
    newVerifyDomainDkimResponse,

    -- ** TestRenderTemplate
    TestRenderTemplate (TestRenderTemplate'),
    newTestRenderTemplate,
    TestRenderTemplateResponse (TestRenderTemplateResponse'),
    newTestRenderTemplateResponse,

    -- ** SendBulkTemplatedEmail
    SendBulkTemplatedEmail (SendBulkTemplatedEmail'),
    newSendBulkTemplatedEmail,
    SendBulkTemplatedEmailResponse (SendBulkTemplatedEmailResponse'),
    newSendBulkTemplatedEmailResponse,

    -- ** SendRawEmail
    SendRawEmail (SendRawEmail'),
    newSendRawEmail,
    SendRawEmailResponse (SendRawEmailResponse'),
    newSendRawEmailResponse,

    -- ** GetSendStatistics
    GetSendStatistics (GetSendStatistics'),
    newGetSendStatistics,
    GetSendStatisticsResponse (GetSendStatisticsResponse'),
    newGetSendStatisticsResponse,

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

    -- ** DescribeReceiptRule
    DescribeReceiptRule (DescribeReceiptRule'),
    newDescribeReceiptRule,
    DescribeReceiptRuleResponse (DescribeReceiptRuleResponse'),
    newDescribeReceiptRuleResponse,

    -- ** ListIdentities (Paginated)
    ListIdentities (ListIdentities'),
    newListIdentities,
    ListIdentitiesResponse (ListIdentitiesResponse'),
    newListIdentitiesResponse,

    -- ** UpdateConfigurationSetSendingEnabled
    UpdateConfigurationSetSendingEnabled (UpdateConfigurationSetSendingEnabled'),
    newUpdateConfigurationSetSendingEnabled,
    UpdateConfigurationSetSendingEnabledResponse (UpdateConfigurationSetSendingEnabledResponse'),
    newUpdateConfigurationSetSendingEnabledResponse,

    -- ** CreateCustomVerificationEmailTemplate
    CreateCustomVerificationEmailTemplate (CreateCustomVerificationEmailTemplate'),
    newCreateCustomVerificationEmailTemplate,
    CreateCustomVerificationEmailTemplateResponse (CreateCustomVerificationEmailTemplateResponse'),
    newCreateCustomVerificationEmailTemplateResponse,

    -- ** VerifyEmailIdentity
    VerifyEmailIdentity (VerifyEmailIdentity'),
    newVerifyEmailIdentity,
    VerifyEmailIdentityResponse (VerifyEmailIdentityResponse'),
    newVerifyEmailIdentityResponse,

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

    -- ** DeleteReceiptFilter
    DeleteReceiptFilter (DeleteReceiptFilter'),
    newDeleteReceiptFilter,
    DeleteReceiptFilterResponse (DeleteReceiptFilterResponse'),
    newDeleteReceiptFilterResponse,

    -- ** ListVerifiedEmailAddresses
    ListVerifiedEmailAddresses (ListVerifiedEmailAddresses'),
    newListVerifiedEmailAddresses,
    ListVerifiedEmailAddressesResponse (ListVerifiedEmailAddressesResponse'),
    newListVerifiedEmailAddressesResponse,

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

    -- ** SendEmail
    SendEmail (SendEmail'),
    newSendEmail,
    SendEmailResponse (SendEmailResponse'),
    newSendEmailResponse,

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

    -- ** CloneReceiptRuleSet
    CloneReceiptRuleSet (CloneReceiptRuleSet'),
    newCloneReceiptRuleSet,
    CloneReceiptRuleSetResponse (CloneReceiptRuleSetResponse'),
    newCloneReceiptRuleSetResponse,

    -- ** CreateConfigurationSetEventDestination
    CreateConfigurationSetEventDestination (CreateConfigurationSetEventDestination'),
    newCreateConfigurationSetEventDestination,
    CreateConfigurationSetEventDestinationResponse (CreateConfigurationSetEventDestinationResponse'),
    newCreateConfigurationSetEventDestinationResponse,

    -- ** GetAccountSendingEnabled
    GetAccountSendingEnabled (GetAccountSendingEnabled'),
    newGetAccountSendingEnabled,
    GetAccountSendingEnabledResponse (GetAccountSendingEnabledResponse'),
    newGetAccountSendingEnabledResponse,

    -- ** CreateReceiptRule
    CreateReceiptRule (CreateReceiptRule'),
    newCreateReceiptRule,
    CreateReceiptRuleResponse (CreateReceiptRuleResponse'),
    newCreateReceiptRuleResponse,

    -- ** GetTemplate
    GetTemplate (GetTemplate'),
    newGetTemplate,
    GetTemplateResponse (GetTemplateResponse'),
    newGetTemplateResponse,

    -- ** SetActiveReceiptRuleSet
    SetActiveReceiptRuleSet (SetActiveReceiptRuleSet'),
    newSetActiveReceiptRuleSet,
    SetActiveReceiptRuleSetResponse (SetActiveReceiptRuleSetResponse'),
    newSetActiveReceiptRuleSetResponse,

    -- ** CreateConfigurationSet
    CreateConfigurationSet (CreateConfigurationSet'),
    newCreateConfigurationSet,
    CreateConfigurationSetResponse (CreateConfigurationSetResponse'),
    newCreateConfigurationSetResponse,

    -- ** UpdateAccountSendingEnabled
    UpdateAccountSendingEnabled (UpdateAccountSendingEnabled'),
    newUpdateAccountSendingEnabled,
    UpdateAccountSendingEnabledResponse (UpdateAccountSendingEnabledResponse'),
    newUpdateAccountSendingEnabledResponse,

    -- ** CreateConfigurationSetTrackingOptions
    CreateConfigurationSetTrackingOptions (CreateConfigurationSetTrackingOptions'),
    newCreateConfigurationSetTrackingOptions,
    CreateConfigurationSetTrackingOptionsResponse (CreateConfigurationSetTrackingOptionsResponse'),
    newCreateConfigurationSetTrackingOptionsResponse,

    -- ** DescribeActiveReceiptRuleSet
    DescribeActiveReceiptRuleSet (DescribeActiveReceiptRuleSet'),
    newDescribeActiveReceiptRuleSet,
    DescribeActiveReceiptRuleSetResponse (DescribeActiveReceiptRuleSetResponse'),
    newDescribeActiveReceiptRuleSetResponse,

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
