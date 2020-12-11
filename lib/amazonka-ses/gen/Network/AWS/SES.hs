{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Simple Email Service__
--
-- This document contains reference information for the <https://aws.amazon.com/ses/ Amazon Simple Email Service> (Amazon SES) API, version 2010-12-01. This document is best used in conjunction with the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/Welcome.html Amazon SES Developer Guide> .
module Network.AWS.SES
  ( -- * Service configuration
    sesService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateTemplate
    module Network.AWS.SES.CreateTemplate,

    -- ** DeleteConfigurationSetTrackingOptions
    module Network.AWS.SES.DeleteConfigurationSetTrackingOptions,

    -- ** UpdateConfigurationSetTrackingOptions
    module Network.AWS.SES.UpdateConfigurationSetTrackingOptions,

    -- ** CreateReceiptRuleSet
    module Network.AWS.SES.CreateReceiptRuleSet,

    -- ** SetIdentityHeadersInNotificationsEnabled
    module Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled,

    -- ** GetSendQuota
    module Network.AWS.SES.GetSendQuota,

    -- ** PutConfigurationSetDeliveryOptions
    module Network.AWS.SES.PutConfigurationSetDeliveryOptions,

    -- ** DescribeConfigurationSet
    module Network.AWS.SES.DescribeConfigurationSet,

    -- ** PutIdentityPolicy
    module Network.AWS.SES.PutIdentityPolicy,

    -- ** DeleteCustomVerificationEmailTemplate
    module Network.AWS.SES.DeleteCustomVerificationEmailTemplate,

    -- ** DeleteIdentityPolicy
    module Network.AWS.SES.DeleteIdentityPolicy,

    -- ** UpdateCustomVerificationEmailTemplate
    module Network.AWS.SES.UpdateCustomVerificationEmailTemplate,

    -- ** SendCustomVerificationEmail
    module Network.AWS.SES.SendCustomVerificationEmail,

    -- ** GetIdentityNotificationAttributes
    module Network.AWS.SES.GetIdentityNotificationAttributes,

    -- ** UpdateConfigurationSetReputationMetricsEnabled
    module Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled,

    -- ** ListIdentityPolicies
    module Network.AWS.SES.ListIdentityPolicies,

    -- ** SetIdentityDkimEnabled
    module Network.AWS.SES.SetIdentityDkimEnabled,

    -- ** ListReceiptFilters
    module Network.AWS.SES.ListReceiptFilters,

    -- ** DescribeReceiptRuleSet
    module Network.AWS.SES.DescribeReceiptRuleSet,

    -- ** GetIdentityMailFromDomainAttributes
    module Network.AWS.SES.GetIdentityMailFromDomainAttributes,

    -- ** CreateReceiptFilter
    module Network.AWS.SES.CreateReceiptFilter,

    -- ** UpdateConfigurationSetEventDestination
    module Network.AWS.SES.UpdateConfigurationSetEventDestination,

    -- ** DeleteConfigurationSetEventDestination
    module Network.AWS.SES.DeleteConfigurationSetEventDestination,

    -- ** SetIdentityMailFromDomain
    module Network.AWS.SES.SetIdentityMailFromDomain,

    -- ** SetIdentityFeedbackForwardingEnabled
    module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled,

    -- ** ListConfigurationSets (Paginated)
    module Network.AWS.SES.ListConfigurationSets,

    -- ** DeleteConfigurationSet
    module Network.AWS.SES.DeleteConfigurationSet,

    -- ** GetIdentityVerificationAttributes
    module Network.AWS.SES.GetIdentityVerificationAttributes,

    -- ** GetIdentityPolicies
    module Network.AWS.SES.GetIdentityPolicies,

    -- ** ListTemplates (Paginated)
    module Network.AWS.SES.ListTemplates,

    -- ** VerifyDomainIdentity
    module Network.AWS.SES.VerifyDomainIdentity,

    -- ** UpdateTemplate
    module Network.AWS.SES.UpdateTemplate,

    -- ** DeleteTemplate
    module Network.AWS.SES.DeleteTemplate,

    -- ** ReorderReceiptRuleSet
    module Network.AWS.SES.ReorderReceiptRuleSet,

    -- ** ListReceiptRuleSets (Paginated)
    module Network.AWS.SES.ListReceiptRuleSets,

    -- ** DeleteReceiptRuleSet
    module Network.AWS.SES.DeleteReceiptRuleSet,

    -- ** SetReceiptRulePosition
    module Network.AWS.SES.SetReceiptRulePosition,

    -- ** SendBounce
    module Network.AWS.SES.SendBounce,

    -- ** GetIdentityDkimAttributes
    module Network.AWS.SES.GetIdentityDkimAttributes,

    -- ** SendTemplatedEmail
    module Network.AWS.SES.SendTemplatedEmail,

    -- ** VerifyDomainDkim
    module Network.AWS.SES.VerifyDomainDkim,

    -- ** TestRenderTemplate
    module Network.AWS.SES.TestRenderTemplate,

    -- ** SendBulkTemplatedEmail
    module Network.AWS.SES.SendBulkTemplatedEmail,

    -- ** SendRawEmail
    module Network.AWS.SES.SendRawEmail,

    -- ** GetSendStatistics
    module Network.AWS.SES.GetSendStatistics,

    -- ** ListCustomVerificationEmailTemplates (Paginated)
    module Network.AWS.SES.ListCustomVerificationEmailTemplates,

    -- ** DeleteIdentity
    module Network.AWS.SES.DeleteIdentity,

    -- ** DescribeReceiptRule
    module Network.AWS.SES.DescribeReceiptRule,

    -- ** ListIdentities (Paginated)
    module Network.AWS.SES.ListIdentities,

    -- ** UpdateConfigurationSetSendingEnabled
    module Network.AWS.SES.UpdateConfigurationSetSendingEnabled,

    -- ** CreateCustomVerificationEmailTemplate
    module Network.AWS.SES.CreateCustomVerificationEmailTemplate,

    -- ** VerifyEmailIdentity
    module Network.AWS.SES.VerifyEmailIdentity,

    -- ** VerifyEmailAddress
    module Network.AWS.SES.VerifyEmailAddress,

    -- ** DeleteVerifiedEmailAddress
    module Network.AWS.SES.DeleteVerifiedEmailAddress,

    -- ** DeleteReceiptFilter
    module Network.AWS.SES.DeleteReceiptFilter,

    -- ** ListVerifiedEmailAddresses
    module Network.AWS.SES.ListVerifiedEmailAddresses,

    -- ** GetCustomVerificationEmailTemplate
    module Network.AWS.SES.GetCustomVerificationEmailTemplate,

    -- ** SetIdentityNotificationTopic
    module Network.AWS.SES.SetIdentityNotificationTopic,

    -- ** SendEmail
    module Network.AWS.SES.SendEmail,

    -- ** DeleteReceiptRule
    module Network.AWS.SES.DeleteReceiptRule,

    -- ** UpdateReceiptRule
    module Network.AWS.SES.UpdateReceiptRule,

    -- ** CloneReceiptRuleSet
    module Network.AWS.SES.CloneReceiptRuleSet,

    -- ** CreateConfigurationSetEventDestination
    module Network.AWS.SES.CreateConfigurationSetEventDestination,

    -- ** GetAccountSendingEnabled
    module Network.AWS.SES.GetAccountSendingEnabled,

    -- ** CreateReceiptRule
    module Network.AWS.SES.CreateReceiptRule,

    -- ** GetTemplate
    module Network.AWS.SES.GetTemplate,

    -- ** SetActiveReceiptRuleSet
    module Network.AWS.SES.SetActiveReceiptRuleSet,

    -- ** CreateConfigurationSet
    module Network.AWS.SES.CreateConfigurationSet,

    -- ** UpdateAccountSendingEnabled
    module Network.AWS.SES.UpdateAccountSendingEnabled,

    -- ** CreateConfigurationSetTrackingOptions
    module Network.AWS.SES.CreateConfigurationSetTrackingOptions,

    -- ** DescribeActiveReceiptRuleSet
    module Network.AWS.SES.DescribeActiveReceiptRuleSet,

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

    -- ** TLSPolicy
    TLSPolicy (..),

    -- ** VerificationStatus
    VerificationStatus (..),

    -- ** AddHeaderAction
    AddHeaderAction (..),
    mkAddHeaderAction,
    ahaHeaderName,
    ahaHeaderValue,

    -- ** Body
    Body (..),
    mkBody,
    bText,
    bHTML,

    -- ** BounceAction
    BounceAction (..),
    mkBounceAction,
    baTopicARN,
    baStatusCode,
    baSmtpReplyCode,
    baMessage,
    baSender,

    -- ** BouncedRecipientInfo
    BouncedRecipientInfo (..),
    mkBouncedRecipientInfo,
    briBounceType,
    briRecipientDsnFields,
    briRecipientARN,
    briRecipient,

    -- ** BulkEmailDestination
    BulkEmailDestination (..),
    mkBulkEmailDestination,
    bedReplacementTemplateData,
    bedReplacementTags,
    bedDestination,

    -- ** BulkEmailDestinationStatus
    BulkEmailDestinationStatus (..),
    mkBulkEmailDestinationStatus,
    bedsStatus,
    bedsError,
    bedsMessageId,

    -- ** CloudWatchDestination
    CloudWatchDestination (..),
    mkCloudWatchDestination,
    cwdDimensionConfigurations,

    -- ** CloudWatchDimensionConfiguration
    CloudWatchDimensionConfiguration (..),
    mkCloudWatchDimensionConfiguration,
    cwdcDimensionName,
    cwdcDimensionValueSource,
    cwdcDefaultDimensionValue,

    -- ** ConfigurationSet
    ConfigurationSet (..),
    mkConfigurationSet,
    csName,

    -- ** Content
    Content (..),
    mkContent,
    cCharset,
    cData,

    -- ** CustomVerificationEmailTemplate
    CustomVerificationEmailTemplate (..),
    mkCustomVerificationEmailTemplate,
    cvetFromEmailAddress,
    cvetTemplateName,
    cvetFailureRedirectionURL,
    cvetTemplateSubject,
    cvetSuccessRedirectionURL,

    -- ** DeliveryOptions
    DeliveryOptions (..),
    mkDeliveryOptions,
    doTLSPolicy,

    -- ** Destination
    Destination (..),
    mkDestination,
    dBCCAddresses,
    dCCAddresses,
    dToAddresses,

    -- ** EventDestination
    EventDestination (..),
    mkEventDestination,
    edEnabled,
    edKinesisFirehoseDestination,
    edCloudWatchDestination,
    edSNSDestination,
    edName,
    edMatchingEventTypes,

    -- ** ExtensionField
    ExtensionField (..),
    mkExtensionField,
    efName,
    efValue,

    -- ** IdentityDkimAttributes
    IdentityDkimAttributes (..),
    mkIdentityDkimAttributes,
    idaDkimTokens,
    idaDkimEnabled,
    idaDkimVerificationStatus,

    -- ** IdentityMailFromDomainAttributes
    IdentityMailFromDomainAttributes (..),
    mkIdentityMailFromDomainAttributes,
    imfdaMailFromDomain,
    imfdaMailFromDomainStatus,
    imfdaBehaviorOnMXFailure,

    -- ** IdentityNotificationAttributes
    IdentityNotificationAttributes (..),
    mkIdentityNotificationAttributes,
    inaHeadersInDeliveryNotificationsEnabled,
    inaHeadersInComplaintNotificationsEnabled,
    inaHeadersInBounceNotificationsEnabled,
    inaBounceTopic,
    inaComplaintTopic,
    inaDeliveryTopic,
    inaForwardingEnabled,

    -- ** IdentityVerificationAttributes
    IdentityVerificationAttributes (..),
    mkIdentityVerificationAttributes,
    ivaVerificationToken,
    ivaVerificationStatus,

    -- ** KinesisFirehoseDestination
    KinesisFirehoseDestination (..),
    mkKinesisFirehoseDestination,
    kfdIAMRoleARN,
    kfdDeliveryStreamARN,

    -- ** LambdaAction
    LambdaAction (..),
    mkLambdaAction,
    laInvocationType,
    laTopicARN,
    laFunctionARN,

    -- ** Message
    Message (..),
    mkMessage,
    mSubject,
    mBody,

    -- ** MessageDsn
    MessageDsn (..),
    mkMessageDsn,
    mdArrivalDate,
    mdExtensionFields,
    mdReportingMta,

    -- ** MessageTag
    MessageTag (..),
    mkMessageTag,
    mtName,
    mtValue,

    -- ** RawMessage
    RawMessage (..),
    mkRawMessage,
    rmData,

    -- ** ReceiptAction
    ReceiptAction (..),
    mkReceiptAction,
    raAddHeaderAction,
    raSNSAction,
    raWorkmailAction,
    raBounceAction,
    raLambdaAction,
    raStopAction,
    raS3Action,

    -- ** ReceiptFilter
    ReceiptFilter (..),
    mkReceiptFilter,
    rfName,
    rfIPFilter,

    -- ** ReceiptIPFilter
    ReceiptIPFilter (..),
    mkReceiptIPFilter,
    rifPolicy,
    rifCidr,

    -- ** ReceiptRule
    ReceiptRule (..),
    mkReceiptRule,
    rrScanEnabled,
    rrEnabled,
    rrActions,
    rrRecipients,
    rrTLSPolicy,
    rrName,

    -- ** ReceiptRuleSetMetadata
    ReceiptRuleSetMetadata (..),
    mkReceiptRuleSetMetadata,
    rrsmName,
    rrsmCreatedTimestamp,

    -- ** RecipientDsnFields
    RecipientDsnFields (..),
    mkRecipientDsnFields,
    rdfDiagnosticCode,
    rdfRemoteMta,
    rdfFinalRecipient,
    rdfExtensionFields,
    rdfLastAttemptDate,
    rdfAction,
    rdfStatus,

    -- ** ReputationOptions
    ReputationOptions (..),
    mkReputationOptions,
    roLastFreshStart,
    roReputationMetricsEnabled,
    roSendingEnabled,

    -- ** S3Action
    S3Action (..),
    mkS3Action,
    s3KMSKeyARN,
    s3TopicARN,
    s3ObjectKeyPrefix,
    s3BucketName,

    -- ** SNSAction
    SNSAction (..),
    mkSNSAction,
    saEncoding,
    saTopicARN,

    -- ** SNSDestination
    SNSDestination (..),
    mkSNSDestination,
    sdTopicARN,

    -- ** SendDataPoint
    SendDataPoint (..),
    mkSendDataPoint,
    sdpRejects,
    sdpComplaints,
    sdpDeliveryAttempts,
    sdpBounces,
    sdpTimestamp,

    -- ** StopAction
    StopAction (..),
    mkStopAction,
    sTopicARN,
    sScope,

    -- ** Template
    Template (..),
    mkTemplate,
    tTextPart,
    tSubjectPart,
    tHTMLPart,
    tTemplateName,

    -- ** TemplateMetadata
    TemplateMetadata (..),
    mkTemplateMetadata,
    tmName,
    tmCreatedTimestamp,

    -- ** TrackingOptions
    TrackingOptions (..),
    mkTrackingOptions,
    toCustomRedirectDomain,

    -- ** WorkmailAction
    WorkmailAction (..),
    mkWorkmailAction,
    waTopicARN,
    waOrganizationARN,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import qualified Network.AWS.Prelude as Lude
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
