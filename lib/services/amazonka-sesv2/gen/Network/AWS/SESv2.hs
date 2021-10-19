{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.SESv2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-09-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon SES API v2
--
-- <http://aws.amazon.com/ses Amazon SES> is an Amazon Web Services service
-- that you can use to send email messages to your customers.
--
-- If you\'re new to Amazon SES API v2, you might find it helpful to review
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/ Amazon Simple Email Service Developer Guide>.
-- The /Amazon SES Developer Guide/ provides information and code samples
-- that demonstrate how to use Amazon SES API v2 features programmatically.
module Network.AWS.SESv2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** MessageRejected
    _MessageRejected,

    -- ** MailFromDomainNotVerifiedException
    _MailFromDomainNotVerifiedException,

    -- ** ConflictException
    _ConflictException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** AccountSuspendedException
    _AccountSuspendedException,

    -- ** SendingPausedException
    _SendingPausedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetConfigurationSet
    GetConfigurationSet (GetConfigurationSet'),
    newGetConfigurationSet,
    GetConfigurationSetResponse (GetConfigurationSetResponse'),
    newGetConfigurationSetResponse,

    -- ** GetImportJob
    GetImportJob (GetImportJob'),
    newGetImportJob,
    GetImportJobResponse (GetImportJobResponse'),
    newGetImportJobResponse,

    -- ** PutConfigurationSetTrackingOptions
    PutConfigurationSetTrackingOptions (PutConfigurationSetTrackingOptions'),
    newPutConfigurationSetTrackingOptions,
    PutConfigurationSetTrackingOptionsResponse (PutConfigurationSetTrackingOptionsResponse'),
    newPutConfigurationSetTrackingOptionsResponse,

    -- ** PutEmailIdentityDkimSigningAttributes
    PutEmailIdentityDkimSigningAttributes (PutEmailIdentityDkimSigningAttributes'),
    newPutEmailIdentityDkimSigningAttributes,
    PutEmailIdentityDkimSigningAttributesResponse (PutEmailIdentityDkimSigningAttributesResponse'),
    newPutEmailIdentityDkimSigningAttributesResponse,

    -- ** PutEmailIdentityDkimAttributes
    PutEmailIdentityDkimAttributes (PutEmailIdentityDkimAttributes'),
    newPutEmailIdentityDkimAttributes,
    PutEmailIdentityDkimAttributesResponse (PutEmailIdentityDkimAttributesResponse'),
    newPutEmailIdentityDkimAttributesResponse,

    -- ** PutConfigurationSetDeliveryOptions
    PutConfigurationSetDeliveryOptions (PutConfigurationSetDeliveryOptions'),
    newPutConfigurationSetDeliveryOptions,
    PutConfigurationSetDeliveryOptionsResponse (PutConfigurationSetDeliveryOptionsResponse'),
    newPutConfigurationSetDeliveryOptionsResponse,

    -- ** ListDedicatedIpPools
    ListDedicatedIpPools (ListDedicatedIpPools'),
    newListDedicatedIpPools,
    ListDedicatedIpPoolsResponse (ListDedicatedIpPoolsResponse'),
    newListDedicatedIpPoolsResponse,

    -- ** GetDomainDeliverabilityCampaign
    GetDomainDeliverabilityCampaign (GetDomainDeliverabilityCampaign'),
    newGetDomainDeliverabilityCampaign,
    GetDomainDeliverabilityCampaignResponse (GetDomainDeliverabilityCampaignResponse'),
    newGetDomainDeliverabilityCampaignResponse,

    -- ** GetDedicatedIps
    GetDedicatedIps (GetDedicatedIps'),
    newGetDedicatedIps,
    GetDedicatedIpsResponse (GetDedicatedIpsResponse'),
    newGetDedicatedIpsResponse,

    -- ** PutConfigurationSetSendingOptions
    PutConfigurationSetSendingOptions (PutConfigurationSetSendingOptions'),
    newPutConfigurationSetSendingOptions,
    PutConfigurationSetSendingOptionsResponse (PutConfigurationSetSendingOptionsResponse'),
    newPutConfigurationSetSendingOptionsResponse,

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

    -- ** CreateDedicatedIpPool
    CreateDedicatedIpPool (CreateDedicatedIpPool'),
    newCreateDedicatedIpPool,
    CreateDedicatedIpPoolResponse (CreateDedicatedIpPoolResponse'),
    newCreateDedicatedIpPoolResponse,

    -- ** SendCustomVerificationEmail
    SendCustomVerificationEmail (SendCustomVerificationEmail'),
    newSendCustomVerificationEmail,
    SendCustomVerificationEmailResponse (SendCustomVerificationEmailResponse'),
    newSendCustomVerificationEmailResponse,

    -- ** GetSuppressedDestination
    GetSuppressedDestination (GetSuppressedDestination'),
    newGetSuppressedDestination,
    GetSuppressedDestinationResponse (GetSuppressedDestinationResponse'),
    newGetSuppressedDestinationResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetEmailTemplate
    GetEmailTemplate (GetEmailTemplate'),
    newGetEmailTemplate,
    GetEmailTemplateResponse (GetEmailTemplateResponse'),
    newGetEmailTemplateResponse,

    -- ** ListSuppressedDestinations
    ListSuppressedDestinations (ListSuppressedDestinations'),
    newListSuppressedDestinations,
    ListSuppressedDestinationsResponse (ListSuppressedDestinationsResponse'),
    newListSuppressedDestinationsResponse,

    -- ** PutEmailIdentityFeedbackAttributes
    PutEmailIdentityFeedbackAttributes (PutEmailIdentityFeedbackAttributes'),
    newPutEmailIdentityFeedbackAttributes,
    PutEmailIdentityFeedbackAttributesResponse (PutEmailIdentityFeedbackAttributesResponse'),
    newPutEmailIdentityFeedbackAttributesResponse,

    -- ** ListEmailTemplates
    ListEmailTemplates (ListEmailTemplates'),
    newListEmailTemplates,
    ListEmailTemplatesResponse (ListEmailTemplatesResponse'),
    newListEmailTemplatesResponse,

    -- ** PutConfigurationSetReputationOptions
    PutConfigurationSetReputationOptions (PutConfigurationSetReputationOptions'),
    newPutConfigurationSetReputationOptions,
    PutConfigurationSetReputationOptionsResponse (PutConfigurationSetReputationOptionsResponse'),
    newPutConfigurationSetReputationOptionsResponse,

    -- ** PutDedicatedIpInPool
    PutDedicatedIpInPool (PutDedicatedIpInPool'),
    newPutDedicatedIpInPool,
    PutDedicatedIpInPoolResponse (PutDedicatedIpInPoolResponse'),
    newPutDedicatedIpInPoolResponse,

    -- ** CreateEmailTemplate
    CreateEmailTemplate (CreateEmailTemplate'),
    newCreateEmailTemplate,
    CreateEmailTemplateResponse (CreateEmailTemplateResponse'),
    newCreateEmailTemplateResponse,

    -- ** PutAccountSendingAttributes
    PutAccountSendingAttributes (PutAccountSendingAttributes'),
    newPutAccountSendingAttributes,
    PutAccountSendingAttributesResponse (PutAccountSendingAttributesResponse'),
    newPutAccountSendingAttributesResponse,

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

    -- ** ListConfigurationSets
    ListConfigurationSets (ListConfigurationSets'),
    newListConfigurationSets,
    ListConfigurationSetsResponse (ListConfigurationSetsResponse'),
    newListConfigurationSetsResponse,

    -- ** DeleteEmailIdentity
    DeleteEmailIdentity (DeleteEmailIdentity'),
    newDeleteEmailIdentity,
    DeleteEmailIdentityResponse (DeleteEmailIdentityResponse'),
    newDeleteEmailIdentityResponse,

    -- ** DeleteContactList
    DeleteContactList (DeleteContactList'),
    newDeleteContactList,
    DeleteContactListResponse (DeleteContactListResponse'),
    newDeleteContactListResponse,

    -- ** UpdateContactList
    UpdateContactList (UpdateContactList'),
    newUpdateContactList,
    UpdateContactListResponse (UpdateContactListResponse'),
    newUpdateContactListResponse,

    -- ** ListImportJobs
    ListImportJobs (ListImportJobs'),
    newListImportJobs,
    ListImportJobsResponse (ListImportJobsResponse'),
    newListImportJobsResponse,

    -- ** DeleteConfigurationSet
    DeleteConfigurationSet (DeleteConfigurationSet'),
    newDeleteConfigurationSet,
    DeleteConfigurationSetResponse (DeleteConfigurationSetResponse'),
    newDeleteConfigurationSetResponse,

    -- ** CreateEmailIdentity
    CreateEmailIdentity (CreateEmailIdentity'),
    newCreateEmailIdentity,
    CreateEmailIdentityResponse (CreateEmailIdentityResponse'),
    newCreateEmailIdentityResponse,

    -- ** GetBlacklistReports
    GetBlacklistReports (GetBlacklistReports'),
    newGetBlacklistReports,
    GetBlacklistReportsResponse (GetBlacklistReportsResponse'),
    newGetBlacklistReportsResponse,

    -- ** CreateContactList
    CreateContactList (CreateContactList'),
    newCreateContactList,
    CreateContactListResponse (CreateContactListResponse'),
    newCreateContactListResponse,

    -- ** ListEmailIdentities
    ListEmailIdentities (ListEmailIdentities'),
    newListEmailIdentities,
    ListEmailIdentitiesResponse (ListEmailIdentitiesResponse'),
    newListEmailIdentitiesResponse,

    -- ** GetContact
    GetContact (GetContact'),
    newGetContact,
    GetContactResponse (GetContactResponse'),
    newGetContactResponse,

    -- ** DeleteContact
    DeleteContact (DeleteContact'),
    newDeleteContact,
    DeleteContactResponse (DeleteContactResponse'),
    newDeleteContactResponse,

    -- ** UpdateContact
    UpdateContact (UpdateContact'),
    newUpdateContact,
    UpdateContactResponse (UpdateContactResponse'),
    newUpdateContactResponse,

    -- ** GetContactList
    GetContactList (GetContactList'),
    newGetContactList,
    GetContactListResponse (GetContactListResponse'),
    newGetContactListResponse,

    -- ** GetDedicatedIp
    GetDedicatedIp (GetDedicatedIp'),
    newGetDedicatedIp,
    GetDedicatedIpResponse (GetDedicatedIpResponse'),
    newGetDedicatedIpResponse,

    -- ** CreateContact
    CreateContact (CreateContact'),
    newCreateContact,
    CreateContactResponse (CreateContactResponse'),
    newCreateContactResponse,

    -- ** GetEmailIdentity
    GetEmailIdentity (GetEmailIdentity'),
    newGetEmailIdentity,
    GetEmailIdentityResponse (GetEmailIdentityResponse'),
    newGetEmailIdentityResponse,

    -- ** GetConfigurationSetEventDestinations
    GetConfigurationSetEventDestinations (GetConfigurationSetEventDestinations'),
    newGetConfigurationSetEventDestinations,
    GetConfigurationSetEventDestinationsResponse (GetConfigurationSetEventDestinationsResponse'),
    newGetConfigurationSetEventDestinationsResponse,

    -- ** ListCustomVerificationEmailTemplates
    ListCustomVerificationEmailTemplates (ListCustomVerificationEmailTemplates'),
    newListCustomVerificationEmailTemplates,
    ListCustomVerificationEmailTemplatesResponse (ListCustomVerificationEmailTemplatesResponse'),
    newListCustomVerificationEmailTemplatesResponse,

    -- ** GetAccount
    GetAccount (GetAccount'),
    newGetAccount,
    GetAccountResponse (GetAccountResponse'),
    newGetAccountResponse,

    -- ** DeleteDedicatedIpPool
    DeleteDedicatedIpPool (DeleteDedicatedIpPool'),
    newDeleteDedicatedIpPool,
    DeleteDedicatedIpPoolResponse (DeleteDedicatedIpPoolResponse'),
    newDeleteDedicatedIpPoolResponse,

    -- ** GetEmailIdentityPolicies
    GetEmailIdentityPolicies (GetEmailIdentityPolicies'),
    newGetEmailIdentityPolicies,
    GetEmailIdentityPoliciesResponse (GetEmailIdentityPoliciesResponse'),
    newGetEmailIdentityPoliciesResponse,

    -- ** PutConfigurationSetSuppressionOptions
    PutConfigurationSetSuppressionOptions (PutConfigurationSetSuppressionOptions'),
    newPutConfigurationSetSuppressionOptions,
    PutConfigurationSetSuppressionOptionsResponse (PutConfigurationSetSuppressionOptionsResponse'),
    newPutConfigurationSetSuppressionOptionsResponse,

    -- ** CreateCustomVerificationEmailTemplate
    CreateCustomVerificationEmailTemplate (CreateCustomVerificationEmailTemplate'),
    newCreateCustomVerificationEmailTemplate,
    CreateCustomVerificationEmailTemplateResponse (CreateCustomVerificationEmailTemplateResponse'),
    newCreateCustomVerificationEmailTemplateResponse,

    -- ** PutAccountDetails
    PutAccountDetails (PutAccountDetails'),
    newPutAccountDetails,
    PutAccountDetailsResponse (PutAccountDetailsResponse'),
    newPutAccountDetailsResponse,

    -- ** DeleteSuppressedDestination
    DeleteSuppressedDestination (DeleteSuppressedDestination'),
    newDeleteSuppressedDestination,
    DeleteSuppressedDestinationResponse (DeleteSuppressedDestinationResponse'),
    newDeleteSuppressedDestinationResponse,

    -- ** PutSuppressedDestination
    PutSuppressedDestination (PutSuppressedDestination'),
    newPutSuppressedDestination,
    PutSuppressedDestinationResponse (PutSuppressedDestinationResponse'),
    newPutSuppressedDestinationResponse,

    -- ** GetDomainStatisticsReport
    GetDomainStatisticsReport (GetDomainStatisticsReport'),
    newGetDomainStatisticsReport,
    GetDomainStatisticsReportResponse (GetDomainStatisticsReportResponse'),
    newGetDomainStatisticsReportResponse,

    -- ** DeleteEmailTemplate
    DeleteEmailTemplate (DeleteEmailTemplate'),
    newDeleteEmailTemplate,
    DeleteEmailTemplateResponse (DeleteEmailTemplateResponse'),
    newDeleteEmailTemplateResponse,

    -- ** UpdateEmailTemplate
    UpdateEmailTemplate (UpdateEmailTemplate'),
    newUpdateEmailTemplate,
    UpdateEmailTemplateResponse (UpdateEmailTemplateResponse'),
    newUpdateEmailTemplateResponse,

    -- ** GetDeliverabilityDashboardOptions
    GetDeliverabilityDashboardOptions (GetDeliverabilityDashboardOptions'),
    newGetDeliverabilityDashboardOptions,
    GetDeliverabilityDashboardOptionsResponse (GetDeliverabilityDashboardOptionsResponse'),
    newGetDeliverabilityDashboardOptionsResponse,

    -- ** GetCustomVerificationEmailTemplate
    GetCustomVerificationEmailTemplate (GetCustomVerificationEmailTemplate'),
    newGetCustomVerificationEmailTemplate,
    GetCustomVerificationEmailTemplateResponse (GetCustomVerificationEmailTemplateResponse'),
    newGetCustomVerificationEmailTemplateResponse,

    -- ** ListDomainDeliverabilityCampaigns
    ListDomainDeliverabilityCampaigns (ListDomainDeliverabilityCampaigns'),
    newListDomainDeliverabilityCampaigns,
    ListDomainDeliverabilityCampaignsResponse (ListDomainDeliverabilityCampaignsResponse'),
    newListDomainDeliverabilityCampaignsResponse,

    -- ** SendBulkEmail
    SendBulkEmail (SendBulkEmail'),
    newSendBulkEmail,
    SendBulkEmailResponse (SendBulkEmailResponse'),
    newSendBulkEmailResponse,

    -- ** TestRenderEmailTemplate
    TestRenderEmailTemplate (TestRenderEmailTemplate'),
    newTestRenderEmailTemplate,
    TestRenderEmailTemplateResponse (TestRenderEmailTemplateResponse'),
    newTestRenderEmailTemplateResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** SendEmail
    SendEmail (SendEmail'),
    newSendEmail,
    SendEmailResponse (SendEmailResponse'),
    newSendEmailResponse,

    -- ** PutDedicatedIpWarmupAttributes
    PutDedicatedIpWarmupAttributes (PutDedicatedIpWarmupAttributes'),
    newPutDedicatedIpWarmupAttributes,
    PutDedicatedIpWarmupAttributesResponse (PutDedicatedIpWarmupAttributesResponse'),
    newPutDedicatedIpWarmupAttributesResponse,

    -- ** DeleteEmailIdentityPolicy
    DeleteEmailIdentityPolicy (DeleteEmailIdentityPolicy'),
    newDeleteEmailIdentityPolicy,
    DeleteEmailIdentityPolicyResponse (DeleteEmailIdentityPolicyResponse'),
    newDeleteEmailIdentityPolicyResponse,

    -- ** UpdateEmailIdentityPolicy
    UpdateEmailIdentityPolicy (UpdateEmailIdentityPolicy'),
    newUpdateEmailIdentityPolicy,
    UpdateEmailIdentityPolicyResponse (UpdateEmailIdentityPolicyResponse'),
    newUpdateEmailIdentityPolicyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateDeliverabilityTestReport
    CreateDeliverabilityTestReport (CreateDeliverabilityTestReport'),
    newCreateDeliverabilityTestReport,
    CreateDeliverabilityTestReportResponse (CreateDeliverabilityTestReportResponse'),
    newCreateDeliverabilityTestReportResponse,

    -- ** PutEmailIdentityMailFromAttributes
    PutEmailIdentityMailFromAttributes (PutEmailIdentityMailFromAttributes'),
    newPutEmailIdentityMailFromAttributes,
    PutEmailIdentityMailFromAttributesResponse (PutEmailIdentityMailFromAttributesResponse'),
    newPutEmailIdentityMailFromAttributesResponse,

    -- ** ListContactLists
    ListContactLists (ListContactLists'),
    newListContactLists,
    ListContactListsResponse (ListContactListsResponse'),
    newListContactListsResponse,

    -- ** CreateEmailIdentityPolicy
    CreateEmailIdentityPolicy (CreateEmailIdentityPolicy'),
    newCreateEmailIdentityPolicy,
    CreateEmailIdentityPolicyResponse (CreateEmailIdentityPolicyResponse'),
    newCreateEmailIdentityPolicyResponse,

    -- ** CreateConfigurationSetEventDestination
    CreateConfigurationSetEventDestination (CreateConfigurationSetEventDestination'),
    newCreateConfigurationSetEventDestination,
    CreateConfigurationSetEventDestinationResponse (CreateConfigurationSetEventDestinationResponse'),
    newCreateConfigurationSetEventDestinationResponse,

    -- ** PutEmailIdentityConfigurationSetAttributes
    PutEmailIdentityConfigurationSetAttributes (PutEmailIdentityConfigurationSetAttributes'),
    newPutEmailIdentityConfigurationSetAttributes,
    PutEmailIdentityConfigurationSetAttributesResponse (PutEmailIdentityConfigurationSetAttributesResponse'),
    newPutEmailIdentityConfigurationSetAttributesResponse,

    -- ** PutAccountSuppressionAttributes
    PutAccountSuppressionAttributes (PutAccountSuppressionAttributes'),
    newPutAccountSuppressionAttributes,
    PutAccountSuppressionAttributesResponse (PutAccountSuppressionAttributesResponse'),
    newPutAccountSuppressionAttributesResponse,

    -- ** CreateImportJob
    CreateImportJob (CreateImportJob'),
    newCreateImportJob,
    CreateImportJobResponse (CreateImportJobResponse'),
    newCreateImportJobResponse,

    -- ** ListDeliverabilityTestReports
    ListDeliverabilityTestReports (ListDeliverabilityTestReports'),
    newListDeliverabilityTestReports,
    ListDeliverabilityTestReportsResponse (ListDeliverabilityTestReportsResponse'),
    newListDeliverabilityTestReportsResponse,

    -- ** CreateConfigurationSet
    CreateConfigurationSet (CreateConfigurationSet'),
    newCreateConfigurationSet,
    CreateConfigurationSetResponse (CreateConfigurationSetResponse'),
    newCreateConfigurationSetResponse,

    -- ** GetDeliverabilityTestReport
    GetDeliverabilityTestReport (GetDeliverabilityTestReport'),
    newGetDeliverabilityTestReport,
    GetDeliverabilityTestReportResponse (GetDeliverabilityTestReportResponse'),
    newGetDeliverabilityTestReportResponse,

    -- ** PutDeliverabilityDashboardOption
    PutDeliverabilityDashboardOption (PutDeliverabilityDashboardOption'),
    newPutDeliverabilityDashboardOption,
    PutDeliverabilityDashboardOptionResponse (PutDeliverabilityDashboardOptionResponse'),
    newPutDeliverabilityDashboardOptionResponse,

    -- ** PutAccountDedicatedIpWarmupAttributes
    PutAccountDedicatedIpWarmupAttributes (PutAccountDedicatedIpWarmupAttributes'),
    newPutAccountDedicatedIpWarmupAttributes,
    PutAccountDedicatedIpWarmupAttributesResponse (PutAccountDedicatedIpWarmupAttributesResponse'),
    newPutAccountDedicatedIpWarmupAttributesResponse,

    -- ** ListContacts
    ListContacts (ListContacts'),
    newListContacts,
    ListContactsResponse (ListContactsResponse'),
    newListContactsResponse,

    -- * Types

    -- ** BehaviorOnMxFailure
    BehaviorOnMxFailure (..),

    -- ** BulkEmailStatus
    BulkEmailStatus (..),

    -- ** ContactLanguage
    ContactLanguage (..),

    -- ** ContactListImportAction
    ContactListImportAction (..),

    -- ** DataFormat
    DataFormat (..),

    -- ** DeliverabilityDashboardAccountStatus
    DeliverabilityDashboardAccountStatus (..),

    -- ** DeliverabilityTestStatus
    DeliverabilityTestStatus (..),

    -- ** DimensionValueSource
    DimensionValueSource (..),

    -- ** DkimSigningAttributesOrigin
    DkimSigningAttributesOrigin (..),

    -- ** DkimSigningKeyLength
    DkimSigningKeyLength (..),

    -- ** DkimStatus
    DkimStatus (..),

    -- ** EventType
    EventType (..),

    -- ** IdentityType
    IdentityType (..),

    -- ** ImportDestinationType
    ImportDestinationType (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** MailFromDomainStatus
    MailFromDomainStatus (..),

    -- ** MailType
    MailType (..),

    -- ** ReviewStatus
    ReviewStatus (..),

    -- ** SubscriptionStatus
    SubscriptionStatus (..),

    -- ** SuppressionListImportAction
    SuppressionListImportAction (..),

    -- ** SuppressionListReason
    SuppressionListReason (..),

    -- ** TlsPolicy
    TlsPolicy (..),

    -- ** WarmupStatus
    WarmupStatus (..),

    -- ** AccountDetails
    AccountDetails (AccountDetails'),
    newAccountDetails,

    -- ** BlacklistEntry
    BlacklistEntry (BlacklistEntry'),
    newBlacklistEntry,

    -- ** Body
    Body (Body'),
    newBody,

    -- ** BulkEmailContent
    BulkEmailContent (BulkEmailContent'),
    newBulkEmailContent,

    -- ** BulkEmailEntry
    BulkEmailEntry (BulkEmailEntry'),
    newBulkEmailEntry,

    -- ** BulkEmailEntryResult
    BulkEmailEntryResult (BulkEmailEntryResult'),
    newBulkEmailEntryResult,

    -- ** CloudWatchDestination
    CloudWatchDestination (CloudWatchDestination'),
    newCloudWatchDestination,

    -- ** CloudWatchDimensionConfiguration
    CloudWatchDimensionConfiguration (CloudWatchDimensionConfiguration'),
    newCloudWatchDimensionConfiguration,

    -- ** Contact
    Contact (Contact'),
    newContact,

    -- ** ContactList
    ContactList (ContactList'),
    newContactList,

    -- ** ContactListDestination
    ContactListDestination (ContactListDestination'),
    newContactListDestination,

    -- ** Content
    Content (Content'),
    newContent,

    -- ** CustomVerificationEmailTemplateMetadata
    CustomVerificationEmailTemplateMetadata (CustomVerificationEmailTemplateMetadata'),
    newCustomVerificationEmailTemplateMetadata,

    -- ** DailyVolume
    DailyVolume (DailyVolume'),
    newDailyVolume,

    -- ** DedicatedIp
    DedicatedIp (DedicatedIp'),
    newDedicatedIp,

    -- ** DeliverabilityTestReport
    DeliverabilityTestReport (DeliverabilityTestReport'),
    newDeliverabilityTestReport,

    -- ** DeliveryOptions
    DeliveryOptions (DeliveryOptions'),
    newDeliveryOptions,

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** DkimAttributes
    DkimAttributes (DkimAttributes'),
    newDkimAttributes,

    -- ** DkimSigningAttributes
    DkimSigningAttributes (DkimSigningAttributes'),
    newDkimSigningAttributes,

    -- ** DomainDeliverabilityCampaign
    DomainDeliverabilityCampaign (DomainDeliverabilityCampaign'),
    newDomainDeliverabilityCampaign,

    -- ** DomainDeliverabilityTrackingOption
    DomainDeliverabilityTrackingOption (DomainDeliverabilityTrackingOption'),
    newDomainDeliverabilityTrackingOption,

    -- ** DomainIspPlacement
    DomainIspPlacement (DomainIspPlacement'),
    newDomainIspPlacement,

    -- ** EmailContent
    EmailContent (EmailContent'),
    newEmailContent,

    -- ** EmailTemplateContent
    EmailTemplateContent (EmailTemplateContent'),
    newEmailTemplateContent,

    -- ** EmailTemplateMetadata
    EmailTemplateMetadata (EmailTemplateMetadata'),
    newEmailTemplateMetadata,

    -- ** EventDestination
    EventDestination (EventDestination'),
    newEventDestination,

    -- ** EventDestinationDefinition
    EventDestinationDefinition (EventDestinationDefinition'),
    newEventDestinationDefinition,

    -- ** FailureInfo
    FailureInfo (FailureInfo'),
    newFailureInfo,

    -- ** IdentityInfo
    IdentityInfo (IdentityInfo'),
    newIdentityInfo,

    -- ** ImportDataSource
    ImportDataSource (ImportDataSource'),
    newImportDataSource,

    -- ** ImportDestination
    ImportDestination (ImportDestination'),
    newImportDestination,

    -- ** ImportJobSummary
    ImportJobSummary (ImportJobSummary'),
    newImportJobSummary,

    -- ** InboxPlacementTrackingOption
    InboxPlacementTrackingOption (InboxPlacementTrackingOption'),
    newInboxPlacementTrackingOption,

    -- ** IspPlacement
    IspPlacement (IspPlacement'),
    newIspPlacement,

    -- ** KinesisFirehoseDestination
    KinesisFirehoseDestination (KinesisFirehoseDestination'),
    newKinesisFirehoseDestination,

    -- ** ListContactsFilter
    ListContactsFilter (ListContactsFilter'),
    newListContactsFilter,

    -- ** ListManagementOptions
    ListManagementOptions (ListManagementOptions'),
    newListManagementOptions,

    -- ** MailFromAttributes
    MailFromAttributes (MailFromAttributes'),
    newMailFromAttributes,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** MessageTag
    MessageTag (MessageTag'),
    newMessageTag,

    -- ** OverallVolume
    OverallVolume (OverallVolume'),
    newOverallVolume,

    -- ** PinpointDestination
    PinpointDestination (PinpointDestination'),
    newPinpointDestination,

    -- ** PlacementStatistics
    PlacementStatistics (PlacementStatistics'),
    newPlacementStatistics,

    -- ** RawMessage
    RawMessage (RawMessage'),
    newRawMessage,

    -- ** ReplacementEmailContent
    ReplacementEmailContent (ReplacementEmailContent'),
    newReplacementEmailContent,

    -- ** ReplacementTemplate
    ReplacementTemplate (ReplacementTemplate'),
    newReplacementTemplate,

    -- ** ReputationOptions
    ReputationOptions (ReputationOptions'),
    newReputationOptions,

    -- ** ReviewDetails
    ReviewDetails (ReviewDetails'),
    newReviewDetails,

    -- ** SendQuota
    SendQuota (SendQuota'),
    newSendQuota,

    -- ** SendingOptions
    SendingOptions (SendingOptions'),
    newSendingOptions,

    -- ** SnsDestination
    SnsDestination (SnsDestination'),
    newSnsDestination,

    -- ** SuppressedDestination
    SuppressedDestination (SuppressedDestination'),
    newSuppressedDestination,

    -- ** SuppressedDestinationAttributes
    SuppressedDestinationAttributes (SuppressedDestinationAttributes'),
    newSuppressedDestinationAttributes,

    -- ** SuppressedDestinationSummary
    SuppressedDestinationSummary (SuppressedDestinationSummary'),
    newSuppressedDestinationSummary,

    -- ** SuppressionAttributes
    SuppressionAttributes (SuppressionAttributes'),
    newSuppressionAttributes,

    -- ** SuppressionListDestination
    SuppressionListDestination (SuppressionListDestination'),
    newSuppressionListDestination,

    -- ** SuppressionOptions
    SuppressionOptions (SuppressionOptions'),
    newSuppressionOptions,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Template
    Template (Template'),
    newTemplate,

    -- ** Topic
    Topic (Topic'),
    newTopic,

    -- ** TopicFilter
    TopicFilter (TopicFilter'),
    newTopicFilter,

    -- ** TopicPreference
    TopicPreference (TopicPreference'),
    newTopicPreference,

    -- ** TrackingOptions
    TrackingOptions (TrackingOptions'),
    newTrackingOptions,

    -- ** VolumeStatistics
    VolumeStatistics (VolumeStatistics'),
    newVolumeStatistics,
  )
where

import Network.AWS.SESv2.CreateConfigurationSet
import Network.AWS.SESv2.CreateConfigurationSetEventDestination
import Network.AWS.SESv2.CreateContact
import Network.AWS.SESv2.CreateContactList
import Network.AWS.SESv2.CreateCustomVerificationEmailTemplate
import Network.AWS.SESv2.CreateDedicatedIpPool
import Network.AWS.SESv2.CreateDeliverabilityTestReport
import Network.AWS.SESv2.CreateEmailIdentity
import Network.AWS.SESv2.CreateEmailIdentityPolicy
import Network.AWS.SESv2.CreateEmailTemplate
import Network.AWS.SESv2.CreateImportJob
import Network.AWS.SESv2.DeleteConfigurationSet
import Network.AWS.SESv2.DeleteConfigurationSetEventDestination
import Network.AWS.SESv2.DeleteContact
import Network.AWS.SESv2.DeleteContactList
import Network.AWS.SESv2.DeleteCustomVerificationEmailTemplate
import Network.AWS.SESv2.DeleteDedicatedIpPool
import Network.AWS.SESv2.DeleteEmailIdentity
import Network.AWS.SESv2.DeleteEmailIdentityPolicy
import Network.AWS.SESv2.DeleteEmailTemplate
import Network.AWS.SESv2.DeleteSuppressedDestination
import Network.AWS.SESv2.GetAccount
import Network.AWS.SESv2.GetBlacklistReports
import Network.AWS.SESv2.GetConfigurationSet
import Network.AWS.SESv2.GetConfigurationSetEventDestinations
import Network.AWS.SESv2.GetContact
import Network.AWS.SESv2.GetContactList
import Network.AWS.SESv2.GetCustomVerificationEmailTemplate
import Network.AWS.SESv2.GetDedicatedIp
import Network.AWS.SESv2.GetDedicatedIps
import Network.AWS.SESv2.GetDeliverabilityDashboardOptions
import Network.AWS.SESv2.GetDeliverabilityTestReport
import Network.AWS.SESv2.GetDomainDeliverabilityCampaign
import Network.AWS.SESv2.GetDomainStatisticsReport
import Network.AWS.SESv2.GetEmailIdentity
import Network.AWS.SESv2.GetEmailIdentityPolicies
import Network.AWS.SESv2.GetEmailTemplate
import Network.AWS.SESv2.GetImportJob
import Network.AWS.SESv2.GetSuppressedDestination
import Network.AWS.SESv2.Lens
import Network.AWS.SESv2.ListConfigurationSets
import Network.AWS.SESv2.ListContactLists
import Network.AWS.SESv2.ListContacts
import Network.AWS.SESv2.ListCustomVerificationEmailTemplates
import Network.AWS.SESv2.ListDedicatedIpPools
import Network.AWS.SESv2.ListDeliverabilityTestReports
import Network.AWS.SESv2.ListDomainDeliverabilityCampaigns
import Network.AWS.SESv2.ListEmailIdentities
import Network.AWS.SESv2.ListEmailTemplates
import Network.AWS.SESv2.ListImportJobs
import Network.AWS.SESv2.ListSuppressedDestinations
import Network.AWS.SESv2.ListTagsForResource
import Network.AWS.SESv2.PutAccountDedicatedIpWarmupAttributes
import Network.AWS.SESv2.PutAccountDetails
import Network.AWS.SESv2.PutAccountSendingAttributes
import Network.AWS.SESv2.PutAccountSuppressionAttributes
import Network.AWS.SESv2.PutConfigurationSetDeliveryOptions
import Network.AWS.SESv2.PutConfigurationSetReputationOptions
import Network.AWS.SESv2.PutConfigurationSetSendingOptions
import Network.AWS.SESv2.PutConfigurationSetSuppressionOptions
import Network.AWS.SESv2.PutConfigurationSetTrackingOptions
import Network.AWS.SESv2.PutDedicatedIpInPool
import Network.AWS.SESv2.PutDedicatedIpWarmupAttributes
import Network.AWS.SESv2.PutDeliverabilityDashboardOption
import Network.AWS.SESv2.PutEmailIdentityConfigurationSetAttributes
import Network.AWS.SESv2.PutEmailIdentityDkimAttributes
import Network.AWS.SESv2.PutEmailIdentityDkimSigningAttributes
import Network.AWS.SESv2.PutEmailIdentityFeedbackAttributes
import Network.AWS.SESv2.PutEmailIdentityMailFromAttributes
import Network.AWS.SESv2.PutSuppressedDestination
import Network.AWS.SESv2.SendBulkEmail
import Network.AWS.SESv2.SendCustomVerificationEmail
import Network.AWS.SESv2.SendEmail
import Network.AWS.SESv2.TagResource
import Network.AWS.SESv2.TestRenderEmailTemplate
import Network.AWS.SESv2.Types
import Network.AWS.SESv2.UntagResource
import Network.AWS.SESv2.UpdateConfigurationSetEventDestination
import Network.AWS.SESv2.UpdateContact
import Network.AWS.SESv2.UpdateContactList
import Network.AWS.SESv2.UpdateCustomVerificationEmailTemplate
import Network.AWS.SESv2.UpdateEmailIdentityPolicy
import Network.AWS.SESv2.UpdateEmailTemplate
import Network.AWS.SESv2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SESv2'.

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
