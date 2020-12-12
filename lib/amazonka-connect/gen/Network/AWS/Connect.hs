{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Connect is a cloud-based contact center solution that makes it easy to set up and manage a customer contact center and provide reliable customer engagement at any scale.
--
-- Amazon Connect provides rich metrics and real-time reporting that allow you to optimize contact routing. You can also resolve customer issues more efficiently by putting customers in touch with the right agents.
-- There are limits to the number of Amazon Connect resources that you can create and limits to the number of requests that you can make per second. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html Amazon Connect Service Quotas> in the /Amazon Connect Administrator Guide/ .
-- To connect programmatically to an AWS service, you use an endpoint. For a list of Amazon Connect endpoints, see <https://docs.aws.amazon.com/general/latest/gr/connect_region.html Amazon Connect Endpoints> .
module Network.AWS.Connect
  ( -- * Service configuration
    connectService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeInstance
    module Network.AWS.Connect.DescribeInstance,

    -- ** ListSecurityProfiles (Paginated)
    module Network.AWS.Connect.ListSecurityProfiles,

    -- ** AssociateLexBot
    module Network.AWS.Connect.AssociateLexBot,

    -- ** UpdateInstanceAttribute
    module Network.AWS.Connect.UpdateInstanceAttribute,

    -- ** UpdateRoutingProfileQueues
    module Network.AWS.Connect.UpdateRoutingProfileQueues,

    -- ** ListInstanceAttributes (Paginated)
    module Network.AWS.Connect.ListInstanceAttributes,

    -- ** DescribeInstanceStorageConfig
    module Network.AWS.Connect.DescribeInstanceStorageConfig,

    -- ** DescribeContactFlow
    module Network.AWS.Connect.DescribeContactFlow,

    -- ** UpdateUserHierarchy
    module Network.AWS.Connect.UpdateUserHierarchy,

    -- ** UpdateUserRoutingProfile
    module Network.AWS.Connect.UpdateUserRoutingProfile,

    -- ** UpdateUserHierarchyGroupName
    module Network.AWS.Connect.UpdateUserHierarchyGroupName,

    -- ** DescribeRoutingProfile
    module Network.AWS.Connect.DescribeRoutingProfile,

    -- ** DisassociateLexBot
    module Network.AWS.Connect.DisassociateLexBot,

    -- ** StartOutboundVoiceContact
    module Network.AWS.Connect.StartOutboundVoiceContact,

    -- ** ListTagsForResource
    module Network.AWS.Connect.ListTagsForResource,

    -- ** GetMetricData (Paginated)
    module Network.AWS.Connect.GetMetricData,

    -- ** StartContactRecording
    module Network.AWS.Connect.StartContactRecording,

    -- ** CreateInstance
    module Network.AWS.Connect.CreateInstance,

    -- ** ListUsers (Paginated)
    module Network.AWS.Connect.ListUsers,

    -- ** ListUserHierarchyGroups (Paginated)
    module Network.AWS.Connect.ListUserHierarchyGroups,

    -- ** ListQueues (Paginated)
    module Network.AWS.Connect.ListQueues,

    -- ** DescribeInstanceAttribute
    module Network.AWS.Connect.DescribeInstanceAttribute,

    -- ** DeleteInstance
    module Network.AWS.Connect.DeleteInstance,

    -- ** DisassociateInstanceStorageConfig
    module Network.AWS.Connect.DisassociateInstanceStorageConfig,

    -- ** CreateRoutingProfile
    module Network.AWS.Connect.CreateRoutingProfile,

    -- ** UpdateInstanceStorageConfig
    module Network.AWS.Connect.UpdateInstanceStorageConfig,

    -- ** GetCurrentMetricData
    module Network.AWS.Connect.GetCurrentMetricData,

    -- ** CreateContactFlow
    module Network.AWS.Connect.CreateContactFlow,

    -- ** ListRoutingProfiles (Paginated)
    module Network.AWS.Connect.ListRoutingProfiles,

    -- ** UpdateUserPhoneConfig
    module Network.AWS.Connect.UpdateUserPhoneConfig,

    -- ** ListApprovedOrigins (Paginated)
    module Network.AWS.Connect.ListApprovedOrigins,

    -- ** DescribeUserHierarchyStructure
    module Network.AWS.Connect.DescribeUserHierarchyStructure,

    -- ** ListPhoneNumbers (Paginated)
    module Network.AWS.Connect.ListPhoneNumbers,

    -- ** UpdateContactAttributes
    module Network.AWS.Connect.UpdateContactAttributes,

    -- ** StartChatContact
    module Network.AWS.Connect.StartChatContact,

    -- ** UpdateUserSecurityProfiles
    module Network.AWS.Connect.UpdateUserSecurityProfiles,

    -- ** GetContactAttributes
    module Network.AWS.Connect.GetContactAttributes,

    -- ** ListLambdaFunctions (Paginated)
    module Network.AWS.Connect.ListLambdaFunctions,

    -- ** DescribeUserHierarchyGroup
    module Network.AWS.Connect.DescribeUserHierarchyGroup,

    -- ** DescribeUser
    module Network.AWS.Connect.DescribeUser,

    -- ** ResumeContactRecording
    module Network.AWS.Connect.ResumeContactRecording,

    -- ** UpdateContactFlowName
    module Network.AWS.Connect.UpdateContactFlowName,

    -- ** SuspendContactRecording
    module Network.AWS.Connect.SuspendContactRecording,

    -- ** ListRoutingProfileQueues (Paginated)
    module Network.AWS.Connect.ListRoutingProfileQueues,

    -- ** DisassociateRoutingProfileQueues
    module Network.AWS.Connect.DisassociateRoutingProfileQueues,

    -- ** DisassociateLambdaFunction
    module Network.AWS.Connect.DisassociateLambdaFunction,

    -- ** UpdateContactFlowContent
    module Network.AWS.Connect.UpdateContactFlowContent,

    -- ** UpdateUserHierarchyStructure
    module Network.AWS.Connect.UpdateUserHierarchyStructure,

    -- ** CreateUserHierarchyGroup
    module Network.AWS.Connect.CreateUserHierarchyGroup,

    -- ** CreateUser
    module Network.AWS.Connect.CreateUser,

    -- ** ListPrompts (Paginated)
    module Network.AWS.Connect.ListPrompts,

    -- ** AssociateSecurityKey
    module Network.AWS.Connect.AssociateSecurityKey,

    -- ** StopContactRecording
    module Network.AWS.Connect.StopContactRecording,

    -- ** DisassociateApprovedOrigin
    module Network.AWS.Connect.DisassociateApprovedOrigin,

    -- ** ListSecurityKeys (Paginated)
    module Network.AWS.Connect.ListSecurityKeys,

    -- ** GetFederationToken
    module Network.AWS.Connect.GetFederationToken,

    -- ** StopContact
    module Network.AWS.Connect.StopContact,

    -- ** DeleteUser
    module Network.AWS.Connect.DeleteUser,

    -- ** TagResource
    module Network.AWS.Connect.TagResource,

    -- ** UpdateUserIdentityInfo
    module Network.AWS.Connect.UpdateUserIdentityInfo,

    -- ** ListInstances (Paginated)
    module Network.AWS.Connect.ListInstances,

    -- ** DeleteUserHierarchyGroup
    module Network.AWS.Connect.DeleteUserHierarchyGroup,

    -- ** UpdateRoutingProfileDefaultOutboundQueue
    module Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue,

    -- ** ListContactFlows (Paginated)
    module Network.AWS.Connect.ListContactFlows,

    -- ** UntagResource
    module Network.AWS.Connect.UntagResource,

    -- ** AssociateApprovedOrigin
    module Network.AWS.Connect.AssociateApprovedOrigin,

    -- ** DisassociateSecurityKey
    module Network.AWS.Connect.DisassociateSecurityKey,

    -- ** UpdateRoutingProfileConcurrency
    module Network.AWS.Connect.UpdateRoutingProfileConcurrency,

    -- ** ListInstanceStorageConfigs (Paginated)
    module Network.AWS.Connect.ListInstanceStorageConfigs,

    -- ** AssociateInstanceStorageConfig
    module Network.AWS.Connect.AssociateInstanceStorageConfig,

    -- ** ListHoursOfOperations (Paginated)
    module Network.AWS.Connect.ListHoursOfOperations,

    -- ** UpdateRoutingProfileName
    module Network.AWS.Connect.UpdateRoutingProfileName,

    -- ** ListLexBots (Paginated)
    module Network.AWS.Connect.ListLexBots,

    -- ** AssociateLambdaFunction
    module Network.AWS.Connect.AssociateLambdaFunction,

    -- ** AssociateRoutingProfileQueues
    module Network.AWS.Connect.AssociateRoutingProfileQueues,

    -- * Types

    -- ** Channel
    Channel (..),

    -- ** Comparison
    Comparison (..),

    -- ** ContactFlowType
    ContactFlowType (..),

    -- ** CurrentMetricName
    CurrentMetricName (..),

    -- ** DirectoryType
    DirectoryType (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** Grouping
    Grouping (..),

    -- ** HistoricalMetricName
    HistoricalMetricName (..),

    -- ** InstanceAttributeType
    InstanceAttributeType (..),

    -- ** InstanceStatus
    InstanceStatus (..),

    -- ** InstanceStorageResourceType
    InstanceStorageResourceType (..),

    -- ** PhoneNumberCountryCode
    PhoneNumberCountryCode (..),

    -- ** PhoneNumberType
    PhoneNumberType (..),

    -- ** PhoneType
    PhoneType (..),

    -- ** QueueType
    QueueType (..),

    -- ** Statistic
    Statistic (..),

    -- ** StorageType
    StorageType (..),

    -- ** Unit
    Unit (..),

    -- ** VoiceRecordingTrack
    VoiceRecordingTrack (..),

    -- ** Attribute
    Attribute (..),
    mkAttribute,
    aValue,
    aAttributeType,

    -- ** ChatMessage
    ChatMessage (..),
    mkChatMessage,
    cmContentType,
    cmContent,

    -- ** ContactFlow
    ContactFlow (..),
    mkContactFlow,
    cfARN,
    cfContent,
    cfName,
    cfId,
    cfType,
    cfDescription,
    cfTags,

    -- ** ContactFlowSummary
    ContactFlowSummary (..),
    mkContactFlowSummary,
    cfsARN,
    cfsName,
    cfsContactFlowType,
    cfsId,

    -- ** Credentials
    Credentials (..),
    mkCredentials,
    cAccessTokenExpiration,
    cAccessToken,
    cRefreshToken,
    cRefreshTokenExpiration,

    -- ** CurrentMetric
    CurrentMetric (..),
    mkCurrentMetric,
    cmName,
    cmUnit,

    -- ** CurrentMetricData
    CurrentMetricData (..),
    mkCurrentMetricData,
    cmdValue,
    cmdMetric,

    -- ** CurrentMetricResult
    CurrentMetricResult (..),
    mkCurrentMetricResult,
    cmrCollections,
    cmrDimensions,

    -- ** Dimensions
    Dimensions (..),
    mkDimensions,
    dChannel,
    dQueue,

    -- ** EncryptionConfig
    EncryptionConfig (..),
    mkEncryptionConfig,
    ecEncryptionType,
    ecKeyId,

    -- ** Filters
    Filters (..),
    mkFilters,
    fQueues,
    fChannels,

    -- ** HierarchyGroup
    HierarchyGroup (..),
    mkHierarchyGroup,
    hgARN,
    hgName,
    hgHierarchyPath,
    hgId,
    hgLevelId,

    -- ** HierarchyGroupSummary
    HierarchyGroupSummary (..),
    mkHierarchyGroupSummary,
    hgsARN,
    hgsName,
    hgsId,

    -- ** HierarchyLevel
    HierarchyLevel (..),
    mkHierarchyLevel,
    hlARN,
    hlName,
    hlId,

    -- ** HierarchyLevelUpdate
    HierarchyLevelUpdate (..),
    mkHierarchyLevelUpdate,
    hluName,

    -- ** HierarchyPath
    HierarchyPath (..),
    mkHierarchyPath,
    hpLevelFive,
    hpLevelThree,
    hpLevelFour,
    hpLevelTwo,
    hpLevelOne,

    -- ** HierarchyStructure
    HierarchyStructure (..),
    mkHierarchyStructure,
    hsLevelFive,
    hsLevelThree,
    hsLevelFour,
    hsLevelTwo,
    hsLevelOne,

    -- ** HierarchyStructureUpdate
    HierarchyStructureUpdate (..),
    mkHierarchyStructureUpdate,
    hsuLevelFive,
    hsuLevelThree,
    hsuLevelFour,
    hsuLevelTwo,
    hsuLevelOne,

    -- ** HistoricalMetric
    HistoricalMetric (..),
    mkHistoricalMetric,
    hmName,
    hmThreshold,
    hmUnit,
    hmStatistic,

    -- ** HistoricalMetricData
    HistoricalMetricData (..),
    mkHistoricalMetricData,
    hmdValue,
    hmdMetric,

    -- ** HistoricalMetricResult
    HistoricalMetricResult (..),
    mkHistoricalMetricResult,
    hmrCollections,
    hmrDimensions,

    -- ** HoursOfOperationSummary
    HoursOfOperationSummary (..),
    mkHoursOfOperationSummary,
    hoosARN,
    hoosName,
    hoosId,

    -- ** Instance
    Instance (..),
    mkInstance,
    iARN,
    iCreatedTime,
    iOutboundCallsEnabled,
    iInboundCallsEnabled,
    iInstanceAlias,
    iId,
    iInstanceStatus,
    iIdentityManagementType,
    iStatusReason,
    iServiceRole,

    -- ** InstanceStatusReason
    InstanceStatusReason (..),
    mkInstanceStatusReason,
    isrMessage,

    -- ** InstanceStorageConfig
    InstanceStorageConfig (..),
    mkInstanceStorageConfig,
    iscAssociationId,
    iscKinesisStreamConfig,
    iscKinesisVideoStreamConfig,
    iscS3Config,
    iscKinesisFirehoseConfig,
    iscStorageType,

    -- ** InstanceSummary
    InstanceSummary (..),
    mkInstanceSummary,
    isARN,
    isCreatedTime,
    isOutboundCallsEnabled,
    isInboundCallsEnabled,
    isInstanceAlias,
    isId,
    isInstanceStatus,
    isIdentityManagementType,
    isServiceRole,

    -- ** KinesisFirehoseConfig
    KinesisFirehoseConfig (..),
    mkKinesisFirehoseConfig,
    kfcFirehoseARN,

    -- ** KinesisStreamConfig
    KinesisStreamConfig (..),
    mkKinesisStreamConfig,
    kscStreamARN,

    -- ** KinesisVideoStreamConfig
    KinesisVideoStreamConfig (..),
    mkKinesisVideoStreamConfig,
    kvscPrefix,
    kvscRetentionPeriodHours,
    kvscEncryptionConfig,

    -- ** LexBot
    LexBot (..),
    mkLexBot,
    lbLexRegion,
    lbName,

    -- ** MediaConcurrency
    MediaConcurrency (..),
    mkMediaConcurrency,
    mcChannel,
    mcConcurrency,

    -- ** ParticipantDetails
    ParticipantDetails (..),
    mkParticipantDetails,
    pdDisplayName,

    -- ** PhoneNumberSummary
    PhoneNumberSummary (..),
    mkPhoneNumberSummary,
    pnsPhoneNumberType,
    pnsARN,
    pnsPhoneNumber,
    pnsPhoneNumberCountryCode,
    pnsId,

    -- ** PromptSummary
    PromptSummary (..),
    mkPromptSummary,
    psARN,
    psName,
    psId,

    -- ** QueueReference
    QueueReference (..),
    mkQueueReference,
    qrARN,
    qrId,

    -- ** QueueSummary
    QueueSummary (..),
    mkQueueSummary,
    qsARN,
    qsName,
    qsId,
    qsQueueType,

    -- ** RoutingProfile
    RoutingProfile (..),
    mkRoutingProfile,
    rpInstanceId,
    rpRoutingProfileARN,
    rpRoutingProfileId,
    rpDefaultOutboundQueueId,
    rpName,
    rpMediaConcurrencies,
    rpDescription,
    rpTags,

    -- ** RoutingProfileQueueConfig
    RoutingProfileQueueConfig (..),
    mkRoutingProfileQueueConfig,
    rpqcQueueReference,
    rpqcPriority,
    rpqcDelay,

    -- ** RoutingProfileQueueConfigSummary
    RoutingProfileQueueConfigSummary (..),
    mkRoutingProfileQueueConfigSummary,
    rpqcsQueueId,
    rpqcsQueueARN,
    rpqcsQueueName,
    rpqcsPriority,
    rpqcsDelay,
    rpqcsChannel,

    -- ** RoutingProfileQueueReference
    RoutingProfileQueueReference (..),
    mkRoutingProfileQueueReference,
    rpqrQueueId,
    rpqrChannel,

    -- ** RoutingProfileSummary
    RoutingProfileSummary (..),
    mkRoutingProfileSummary,
    rpsARN,
    rpsName,
    rpsId,

    -- ** S3Config
    S3Config (..),
    mkS3Config,
    scEncryptionConfig,
    scBucketName,
    scBucketPrefix,

    -- ** SecurityKey
    SecurityKey (..),
    mkSecurityKey,
    skCreationTime,
    skAssociationId,
    skKey,

    -- ** SecurityProfileSummary
    SecurityProfileSummary (..),
    mkSecurityProfileSummary,
    spsARN,
    spsName,
    spsId,

    -- ** Threshold
    Threshold (..),
    mkThreshold,
    tThresholdValue,
    tComparison,

    -- ** User
    User (..),
    mkUser,
    uRoutingProfileId,
    uDirectoryUserId,
    uARN,
    uIdentityInfo,
    uSecurityProfileIds,
    uUsername,
    uId,
    uHierarchyGroupId,
    uPhoneConfig,
    uTags,

    -- ** UserIdentityInfo
    UserIdentityInfo (..),
    mkUserIdentityInfo,
    uiiEmail,
    uiiLastName,
    uiiFirstName,

    -- ** UserPhoneConfig
    UserPhoneConfig (..),
    mkUserPhoneConfig,
    upcAutoAccept,
    upcAfterContactWorkTimeLimit,
    upcDeskPhoneNumber,
    upcPhoneType,

    -- ** UserSummary
    UserSummary (..),
    mkUserSummary,
    usARN,
    usUsername,
    usId,

    -- ** VoiceRecordingConfiguration
    VoiceRecordingConfiguration (..),
    mkVoiceRecordingConfiguration,
    vrcVoiceRecordingTrack,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.Connect.AssociateApprovedOrigin
import Network.AWS.Connect.AssociateInstanceStorageConfig
import Network.AWS.Connect.AssociateLambdaFunction
import Network.AWS.Connect.AssociateLexBot
import Network.AWS.Connect.AssociateRoutingProfileQueues
import Network.AWS.Connect.AssociateSecurityKey
import Network.AWS.Connect.CreateContactFlow
import Network.AWS.Connect.CreateInstance
import Network.AWS.Connect.CreateRoutingProfile
import Network.AWS.Connect.CreateUser
import Network.AWS.Connect.CreateUserHierarchyGroup
import Network.AWS.Connect.DeleteInstance
import Network.AWS.Connect.DeleteUser
import Network.AWS.Connect.DeleteUserHierarchyGroup
import Network.AWS.Connect.DescribeContactFlow
import Network.AWS.Connect.DescribeInstance
import Network.AWS.Connect.DescribeInstanceAttribute
import Network.AWS.Connect.DescribeInstanceStorageConfig
import Network.AWS.Connect.DescribeRoutingProfile
import Network.AWS.Connect.DescribeUser
import Network.AWS.Connect.DescribeUserHierarchyGroup
import Network.AWS.Connect.DescribeUserHierarchyStructure
import Network.AWS.Connect.DisassociateApprovedOrigin
import Network.AWS.Connect.DisassociateInstanceStorageConfig
import Network.AWS.Connect.DisassociateLambdaFunction
import Network.AWS.Connect.DisassociateLexBot
import Network.AWS.Connect.DisassociateRoutingProfileQueues
import Network.AWS.Connect.DisassociateSecurityKey
import Network.AWS.Connect.GetContactAttributes
import Network.AWS.Connect.GetCurrentMetricData
import Network.AWS.Connect.GetFederationToken
import Network.AWS.Connect.GetMetricData
import Network.AWS.Connect.ListApprovedOrigins
import Network.AWS.Connect.ListContactFlows
import Network.AWS.Connect.ListHoursOfOperations
import Network.AWS.Connect.ListInstanceAttributes
import Network.AWS.Connect.ListInstanceStorageConfigs
import Network.AWS.Connect.ListInstances
import Network.AWS.Connect.ListLambdaFunctions
import Network.AWS.Connect.ListLexBots
import Network.AWS.Connect.ListPhoneNumbers
import Network.AWS.Connect.ListPrompts
import Network.AWS.Connect.ListQueues
import Network.AWS.Connect.ListRoutingProfileQueues
import Network.AWS.Connect.ListRoutingProfiles
import Network.AWS.Connect.ListSecurityKeys
import Network.AWS.Connect.ListSecurityProfiles
import Network.AWS.Connect.ListTagsForResource
import Network.AWS.Connect.ListUserHierarchyGroups
import Network.AWS.Connect.ListUsers
import Network.AWS.Connect.ResumeContactRecording
import Network.AWS.Connect.StartChatContact
import Network.AWS.Connect.StartContactRecording
import Network.AWS.Connect.StartOutboundVoiceContact
import Network.AWS.Connect.StopContact
import Network.AWS.Connect.StopContactRecording
import Network.AWS.Connect.SuspendContactRecording
import Network.AWS.Connect.TagResource
import Network.AWS.Connect.Types
import Network.AWS.Connect.UntagResource
import Network.AWS.Connect.UpdateContactAttributes
import Network.AWS.Connect.UpdateContactFlowContent
import Network.AWS.Connect.UpdateContactFlowName
import Network.AWS.Connect.UpdateInstanceAttribute
import Network.AWS.Connect.UpdateInstanceStorageConfig
import Network.AWS.Connect.UpdateRoutingProfileConcurrency
import Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
import Network.AWS.Connect.UpdateRoutingProfileName
import Network.AWS.Connect.UpdateRoutingProfileQueues
import Network.AWS.Connect.UpdateUserHierarchy
import Network.AWS.Connect.UpdateUserHierarchyGroupName
import Network.AWS.Connect.UpdateUserHierarchyStructure
import Network.AWS.Connect.UpdateUserIdentityInfo
import Network.AWS.Connect.UpdateUserPhoneConfig
import Network.AWS.Connect.UpdateUserRoutingProfile
import Network.AWS.Connect.UpdateUserSecurityProfiles
import Network.AWS.Connect.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Connect'.

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
