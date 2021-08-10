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
-- Welcome to the Amazon SES API v2 Reference. This guide provides
-- information about the Amazon SES API v2, including supported operations,
-- data types, parameters, and schemas.
--
-- <https://aws.amazon.com/pinpoint Amazon SES> is an AWS service that you
-- can use to send email messages to your customers.
--
-- If you\'re new to Amazon SES API v2, you might find it helpful to also
-- review the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/ Amazon Simple Email Service Developer Guide>.
-- The /Amazon SES Developer Guide/ provides information and code samples
-- that demonstrate how to use Amazon SES API v2 features programmatically.
--
-- The Amazon SES API v2 is available in several AWS Regions and it
-- provides an endpoint for each of these Regions. For a list of all the
-- Regions and endpoints where the API is currently available, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#ses_region AWS Service Endpoints>
-- in the /Amazon Web Services General Reference/. To learn more about AWS
-- Regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html Managing AWS Regions>
-- in the /Amazon Web Services General Reference/.
--
-- In each Region, AWS maintains multiple Availability Zones. These
-- Availability Zones are physically isolated from each other, but are
-- united by private, low-latency, high-throughput, and highly redundant
-- network connections. These Availability Zones enable us to provide very
-- high levels of availability and redundancy, while also minimizing
-- latency. To learn more about the number of Availability Zones that are
-- available in each Region, see
-- <http://aws.amazon.com/about-aws/global-infrastructure/ AWS Global Infrastructure>.
module Network.AWS.SESv2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** MailFromDomainNotVerifiedException
    _MailFromDomainNotVerifiedException,

    -- ** SendingPausedException
    _SendingPausedException,

    -- ** MessageRejected
    _MessageRejected,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** AccountSuspendedException
    _AccountSuspendedException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListDedicatedIpPools
    ListDedicatedIpPools (ListDedicatedIpPools'),
    newListDedicatedIpPools,
    ListDedicatedIpPoolsResponse (ListDedicatedIpPoolsResponse'),
    newListDedicatedIpPoolsResponse,

    -- ** PutEmailIdentityDkimAttributes
    PutEmailIdentityDkimAttributes (PutEmailIdentityDkimAttributes'),
    newPutEmailIdentityDkimAttributes,
    PutEmailIdentityDkimAttributesResponse (PutEmailIdentityDkimAttributesResponse'),
    newPutEmailIdentityDkimAttributesResponse,

    -- ** DeleteDedicatedIpPool
    DeleteDedicatedIpPool (DeleteDedicatedIpPool'),
    newDeleteDedicatedIpPool,
    DeleteDedicatedIpPoolResponse (DeleteDedicatedIpPoolResponse'),
    newDeleteDedicatedIpPoolResponse,

    -- ** PutConfigurationSetDeliveryOptions
    PutConfigurationSetDeliveryOptions (PutConfigurationSetDeliveryOptions'),
    newPutConfigurationSetDeliveryOptions,
    PutConfigurationSetDeliveryOptionsResponse (PutConfigurationSetDeliveryOptionsResponse'),
    newPutConfigurationSetDeliveryOptionsResponse,

    -- ** CreateContact
    CreateContact (CreateContact'),
    newCreateContact,
    CreateContactResponse (CreateContactResponse'),
    newCreateContactResponse,

    -- ** PutConfigurationSetTrackingOptions
    PutConfigurationSetTrackingOptions (PutConfigurationSetTrackingOptions'),
    newPutConfigurationSetTrackingOptions,
    PutConfigurationSetTrackingOptionsResponse (PutConfigurationSetTrackingOptionsResponse'),
    newPutConfigurationSetTrackingOptionsResponse,

    -- ** GetImportJob
    GetImportJob (GetImportJob'),
    newGetImportJob,
    GetImportJobResponse (GetImportJobResponse'),
    newGetImportJobResponse,

    -- ** PutAccountDedicatedIpWarmupAttributes
    PutAccountDedicatedIpWarmupAttributes (PutAccountDedicatedIpWarmupAttributes'),
    newPutAccountDedicatedIpWarmupAttributes,
    PutAccountDedicatedIpWarmupAttributesResponse (PutAccountDedicatedIpWarmupAttributesResponse'),
    newPutAccountDedicatedIpWarmupAttributesResponse,

    -- ** GetDeliverabilityTestReport
    GetDeliverabilityTestReport (GetDeliverabilityTestReport'),
    newGetDeliverabilityTestReport,
    GetDeliverabilityTestReportResponse (GetDeliverabilityTestReportResponse'),
    newGetDeliverabilityTestReportResponse,

    -- ** PutEmailIdentityConfigurationSetAttributes
    PutEmailIdentityConfigurationSetAttributes (PutEmailIdentityConfigurationSetAttributes'),
    newPutEmailIdentityConfigurationSetAttributes,
    PutEmailIdentityConfigurationSetAttributesResponse (PutEmailIdentityConfigurationSetAttributesResponse'),
    newPutEmailIdentityConfigurationSetAttributesResponse,

    -- ** CreateConfigurationSetEventDestination
    CreateConfigurationSetEventDestination (CreateConfigurationSetEventDestination'),
    newCreateConfigurationSetEventDestination,
    CreateConfigurationSetEventDestinationResponse (CreateConfigurationSetEventDestinationResponse'),
    newCreateConfigurationSetEventDestinationResponse,

    -- ** ListEmailIdentities
    ListEmailIdentities (ListEmailIdentities'),
    newListEmailIdentities,
    ListEmailIdentitiesResponse (ListEmailIdentitiesResponse'),
    newListEmailIdentitiesResponse,

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

    -- ** CreateConfigurationSet
    CreateConfigurationSet (CreateConfigurationSet'),
    newCreateConfigurationSet,
    CreateConfigurationSetResponse (CreateConfigurationSetResponse'),
    newCreateConfigurationSetResponse,

    -- ** CreateContactList
    CreateContactList (CreateContactList'),
    newCreateContactList,
    CreateContactListResponse (CreateContactListResponse'),
    newCreateContactListResponse,

    -- ** DeleteConfigurationSet
    DeleteConfigurationSet (DeleteConfigurationSet'),
    newDeleteConfigurationSet,
    DeleteConfigurationSetResponse (DeleteConfigurationSetResponse'),
    newDeleteConfigurationSetResponse,

    -- ** ListImportJobs
    ListImportJobs (ListImportJobs'),
    newListImportJobs,
    ListImportJobsResponse (ListImportJobsResponse'),
    newListImportJobsResponse,

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

    -- ** DeleteEmailIdentityPolicy
    DeleteEmailIdentityPolicy (DeleteEmailIdentityPolicy'),
    newDeleteEmailIdentityPolicy,
    DeleteEmailIdentityPolicyResponse (DeleteEmailIdentityPolicyResponse'),
    newDeleteEmailIdentityPolicyResponse,

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

    -- ** TestRenderEmailTemplate
    TestRenderEmailTemplate (TestRenderEmailTemplate'),
    newTestRenderEmailTemplate,
    TestRenderEmailTemplateResponse (TestRenderEmailTemplateResponse'),
    newTestRenderEmailTemplateResponse,

    -- ** SendEmail
    SendEmail (SendEmail'),
    newSendEmail,
    SendEmailResponse (SendEmailResponse'),
    newSendEmailResponse,

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

    -- ** SendBulkEmail
    SendBulkEmail (SendBulkEmail'),
    newSendBulkEmail,
    SendBulkEmailResponse (SendBulkEmailResponse'),
    newSendBulkEmailResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListDomainDeliverabilityCampaigns
    ListDomainDeliverabilityCampaigns (ListDomainDeliverabilityCampaigns'),
    newListDomainDeliverabilityCampaigns,
    ListDomainDeliverabilityCampaignsResponse (ListDomainDeliverabilityCampaignsResponse'),
    newListDomainDeliverabilityCampaignsResponse,

    -- ** PutSuppressedDestination
    PutSuppressedDestination (PutSuppressedDestination'),
    newPutSuppressedDestination,
    PutSuppressedDestinationResponse (PutSuppressedDestinationResponse'),
    newPutSuppressedDestinationResponse,

    -- ** PutAccountDetails
    PutAccountDetails (PutAccountDetails'),
    newPutAccountDetails,
    PutAccountDetailsResponse (PutAccountDetailsResponse'),
    newPutAccountDetailsResponse,

    -- ** CreateCustomVerificationEmailTemplate
    CreateCustomVerificationEmailTemplate (CreateCustomVerificationEmailTemplate'),
    newCreateCustomVerificationEmailTemplate,
    CreateCustomVerificationEmailTemplateResponse (CreateCustomVerificationEmailTemplateResponse'),
    newCreateCustomVerificationEmailTemplateResponse,

    -- ** PutConfigurationSetSuppressionOptions
    PutConfigurationSetSuppressionOptions (PutConfigurationSetSuppressionOptions'),
    newPutConfigurationSetSuppressionOptions,
    PutConfigurationSetSuppressionOptionsResponse (PutConfigurationSetSuppressionOptionsResponse'),
    newPutConfigurationSetSuppressionOptionsResponse,

    -- ** GetSuppressedDestination
    GetSuppressedDestination (GetSuppressedDestination'),
    newGetSuppressedDestination,
    GetSuppressedDestinationResponse (GetSuppressedDestinationResponse'),
    newGetSuppressedDestinationResponse,

    -- ** GetEmailTemplate
    GetEmailTemplate (GetEmailTemplate'),
    newGetEmailTemplate,
    GetEmailTemplateResponse (GetEmailTemplateResponse'),
    newGetEmailTemplateResponse,

    -- ** ListCustomVerificationEmailTemplates
    ListCustomVerificationEmailTemplates (ListCustomVerificationEmailTemplates'),
    newListCustomVerificationEmailTemplates,
    ListCustomVerificationEmailTemplatesResponse (ListCustomVerificationEmailTemplatesResponse'),
    newListCustomVerificationEmailTemplatesResponse,

    -- ** GetDedicatedIps
    GetDedicatedIps (GetDedicatedIps'),
    newGetDedicatedIps,
    GetDedicatedIpsResponse (GetDedicatedIpsResponse'),
    newGetDedicatedIpsResponse,

    -- ** DeleteCustomVerificationEmailTemplate
    DeleteCustomVerificationEmailTemplate (DeleteCustomVerificationEmailTemplate'),
    newDeleteCustomVerificationEmailTemplate,
    DeleteCustomVerificationEmailTemplateResponse (DeleteCustomVerificationEmailTemplateResponse'),
    newDeleteCustomVerificationEmailTemplateResponse,

    -- ** GetConfigurationSetEventDestinations
    GetConfigurationSetEventDestinations (GetConfigurationSetEventDestinations'),
    newGetConfigurationSetEventDestinations,
    GetConfigurationSetEventDestinationsResponse (GetConfigurationSetEventDestinationsResponse'),
    newGetConfigurationSetEventDestinationsResponse,

    -- ** PutConfigurationSetSendingOptions
    PutConfigurationSetSendingOptions (PutConfigurationSetSendingOptions'),
    newPutConfigurationSetSendingOptions,
    PutConfigurationSetSendingOptionsResponse (PutConfigurationSetSendingOptionsResponse'),
    newPutConfigurationSetSendingOptionsResponse,

    -- ** GetAccount
    GetAccount (GetAccount'),
    newGetAccount,
    GetAccountResponse (GetAccountResponse'),
    newGetAccountResponse,

    -- ** UpdateCustomVerificationEmailTemplate
    UpdateCustomVerificationEmailTemplate (UpdateCustomVerificationEmailTemplate'),
    newUpdateCustomVerificationEmailTemplate,
    UpdateCustomVerificationEmailTemplateResponse (UpdateCustomVerificationEmailTemplateResponse'),
    newUpdateCustomVerificationEmailTemplateResponse,

    -- ** GetDomainDeliverabilityCampaign
    GetDomainDeliverabilityCampaign (GetDomainDeliverabilityCampaign'),
    newGetDomainDeliverabilityCampaign,
    GetDomainDeliverabilityCampaignResponse (GetDomainDeliverabilityCampaignResponse'),
    newGetDomainDeliverabilityCampaignResponse,

    -- ** GetDedicatedIp
    GetDedicatedIp (GetDedicatedIp'),
    newGetDedicatedIp,
    GetDedicatedIpResponse (GetDedicatedIpResponse'),
    newGetDedicatedIpResponse,

    -- ** PutEmailIdentityDkimSigningAttributes
    PutEmailIdentityDkimSigningAttributes (PutEmailIdentityDkimSigningAttributes'),
    newPutEmailIdentityDkimSigningAttributes,
    PutEmailIdentityDkimSigningAttributesResponse (PutEmailIdentityDkimSigningAttributesResponse'),
    newPutEmailIdentityDkimSigningAttributesResponse,

    -- ** GetConfigurationSet
    GetConfigurationSet (GetConfigurationSet'),
    newGetConfigurationSet,
    GetConfigurationSetResponse (GetConfigurationSetResponse'),
    newGetConfigurationSetResponse,

    -- ** GetContactList
    GetContactList (GetContactList'),
    newGetContactList,
    GetContactListResponse (GetContactListResponse'),
    newGetContactListResponse,

    -- ** GetEmailIdentity
    GetEmailIdentity (GetEmailIdentity'),
    newGetEmailIdentity,
    GetEmailIdentityResponse (GetEmailIdentityResponse'),
    newGetEmailIdentityResponse,

    -- ** UpdateContact
    UpdateContact (UpdateContact'),
    newUpdateContact,
    UpdateContactResponse (UpdateContactResponse'),
    newUpdateContactResponse,

    -- ** ListContacts
    ListContacts (ListContacts'),
    newListContacts,
    ListContactsResponse (ListContactsResponse'),
    newListContactsResponse,

    -- ** DeleteContact
    DeleteContact (DeleteContact'),
    newDeleteContact,
    DeleteContactResponse (DeleteContactResponse'),
    newDeleteContactResponse,

    -- ** PutDeliverabilityDashboardOption
    PutDeliverabilityDashboardOption (PutDeliverabilityDashboardOption'),
    newPutDeliverabilityDashboardOption,
    PutDeliverabilityDashboardOptionResponse (PutDeliverabilityDashboardOptionResponse'),
    newPutDeliverabilityDashboardOptionResponse,

    -- ** ListDeliverabilityTestReports
    ListDeliverabilityTestReports (ListDeliverabilityTestReports'),
    newListDeliverabilityTestReports,
    ListDeliverabilityTestReportsResponse (ListDeliverabilityTestReportsResponse'),
    newListDeliverabilityTestReportsResponse,

    -- ** CreateEmailIdentityPolicy
    CreateEmailIdentityPolicy (CreateEmailIdentityPolicy'),
    newCreateEmailIdentityPolicy,
    CreateEmailIdentityPolicyResponse (CreateEmailIdentityPolicyResponse'),
    newCreateEmailIdentityPolicyResponse,

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

    -- ** GetContact
    GetContact (GetContact'),
    newGetContact,
    GetContactResponse (GetContactResponse'),
    newGetContactResponse,

    -- ** ListConfigurationSets
    ListConfigurationSets (ListConfigurationSets'),
    newListConfigurationSets,
    ListConfigurationSetsResponse (ListConfigurationSetsResponse'),
    newListConfigurationSetsResponse,

    -- ** UpdateContactList
    UpdateContactList (UpdateContactList'),
    newUpdateContactList,
    UpdateContactListResponse (UpdateContactListResponse'),
    newUpdateContactListResponse,

    -- ** PutAccountSendingAttributes
    PutAccountSendingAttributes (PutAccountSendingAttributes'),
    newPutAccountSendingAttributes,
    PutAccountSendingAttributesResponse (PutAccountSendingAttributesResponse'),
    newPutAccountSendingAttributesResponse,

    -- ** PutEmailIdentityMailFromAttributes
    PutEmailIdentityMailFromAttributes (PutEmailIdentityMailFromAttributes'),
    newPutEmailIdentityMailFromAttributes,
    PutEmailIdentityMailFromAttributesResponse (PutEmailIdentityMailFromAttributesResponse'),
    newPutEmailIdentityMailFromAttributesResponse,

    -- ** CreateDeliverabilityTestReport
    CreateDeliverabilityTestReport (CreateDeliverabilityTestReport'),
    newCreateDeliverabilityTestReport,
    CreateDeliverabilityTestReportResponse (CreateDeliverabilityTestReportResponse'),
    newCreateDeliverabilityTestReportResponse,

    -- ** DeleteEmailIdentity
    DeleteEmailIdentity (DeleteEmailIdentity'),
    newDeleteEmailIdentity,
    DeleteEmailIdentityResponse (DeleteEmailIdentityResponse'),
    newDeleteEmailIdentityResponse,

    -- ** ListContactLists
    ListContactLists (ListContactLists'),
    newListContactLists,
    ListContactListsResponse (ListContactListsResponse'),
    newListContactListsResponse,

    -- ** DeleteContactList
    DeleteContactList (DeleteContactList'),
    newDeleteContactList,
    DeleteContactListResponse (DeleteContactListResponse'),
    newDeleteContactListResponse,

    -- ** PutDedicatedIpWarmupAttributes
    PutDedicatedIpWarmupAttributes (PutDedicatedIpWarmupAttributes'),
    newPutDedicatedIpWarmupAttributes,
    PutDedicatedIpWarmupAttributesResponse (PutDedicatedIpWarmupAttributesResponse'),
    newPutDedicatedIpWarmupAttributesResponse,

    -- ** GetCustomVerificationEmailTemplate
    GetCustomVerificationEmailTemplate (GetCustomVerificationEmailTemplate'),
    newGetCustomVerificationEmailTemplate,
    GetCustomVerificationEmailTemplateResponse (GetCustomVerificationEmailTemplateResponse'),
    newGetCustomVerificationEmailTemplateResponse,

    -- ** CreateEmailTemplate
    CreateEmailTemplate (CreateEmailTemplate'),
    newCreateEmailTemplate,
    CreateEmailTemplateResponse (CreateEmailTemplateResponse'),
    newCreateEmailTemplateResponse,

    -- ** ListEmailTemplates
    ListEmailTemplates (ListEmailTemplates'),
    newListEmailTemplates,
    ListEmailTemplatesResponse (ListEmailTemplatesResponse'),
    newListEmailTemplatesResponse,

    -- ** UpdateEmailTemplate
    UpdateEmailTemplate (UpdateEmailTemplate'),
    newUpdateEmailTemplate,
    UpdateEmailTemplateResponse (UpdateEmailTemplateResponse'),
    newUpdateEmailTemplateResponse,

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

    -- ** PutEmailIdentityFeedbackAttributes
    PutEmailIdentityFeedbackAttributes (PutEmailIdentityFeedbackAttributes'),
    newPutEmailIdentityFeedbackAttributes,
    PutEmailIdentityFeedbackAttributesResponse (PutEmailIdentityFeedbackAttributesResponse'),
    newPutEmailIdentityFeedbackAttributesResponse,

    -- ** ListSuppressedDestinations
    ListSuppressedDestinations (ListSuppressedDestinations'),
    newListSuppressedDestinations,
    ListSuppressedDestinationsResponse (ListSuppressedDestinationsResponse'),
    newListSuppressedDestinationsResponse,

    -- ** DeleteSuppressedDestination
    DeleteSuppressedDestination (DeleteSuppressedDestination'),
    newDeleteSuppressedDestination,
    DeleteSuppressedDestinationResponse (DeleteSuppressedDestinationResponse'),
    newDeleteSuppressedDestinationResponse,

    -- ** GetDeliverabilityDashboardOptions
    GetDeliverabilityDashboardOptions (GetDeliverabilityDashboardOptions'),
    newGetDeliverabilityDashboardOptions,
    GetDeliverabilityDashboardOptionsResponse (GetDeliverabilityDashboardOptionsResponse'),
    newGetDeliverabilityDashboardOptionsResponse,

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

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetEmailIdentityPolicies
    GetEmailIdentityPolicies (GetEmailIdentityPolicies'),
    newGetEmailIdentityPolicies,
    GetEmailIdentityPoliciesResponse (GetEmailIdentityPoliciesResponse'),
    newGetEmailIdentityPoliciesResponse,

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
