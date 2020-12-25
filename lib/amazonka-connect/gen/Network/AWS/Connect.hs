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
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidContactFlowException
    _InvalidContactFlowException,

    -- ** OutboundContactNotPermittedException
    _OutboundContactNotPermittedException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** DuplicateResourceException
    _DuplicateResourceException,

    -- ** UserNotFoundException
    _UserNotFoundException,

    -- ** ContactFlowNotPublishedException
    _ContactFlowNotPublishedException,

    -- ** DestinationNotAllowedException
    _DestinationNotAllowedException,

    -- ** ContactNotFoundException
    _ContactNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

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

    -- ** AgentFirstName
    AgentFirstName (..),

    -- ** AssociationId
    AssociationId (..),

    -- ** ParticipantToken
    ParticipantToken (..),

    -- ** InstanceId
    InstanceId (..),

    -- ** ContactFlowDescription
    ContactFlowDescription (..),

    -- ** Origin
    Origin (..),

    -- ** Attribute
    Attribute (..),
    mkAttribute,
    aAttributeType,
    aValue,

    -- ** HierarchyGroupSummary
    HierarchyGroupSummary (..),
    mkHierarchyGroupSummary,
    hgsArn,
    hgsId,
    hgsName,

    -- ** RoutingProfileQueueConfig
    RoutingProfileQueueConfig (..),
    mkRoutingProfileQueueConfig,
    rpqcQueueReference,
    rpqcPriority,
    rpqcDelay,

    -- ** Email
    Email (..),

    -- ** AgentLastName
    AgentLastName (..),

    -- ** DirectoryId
    DirectoryId (..),

    -- ** PhoneNumberType
    PhoneNumberType (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** ChatContentType
    ChatContentType (..),

    -- ** AttributeValue
    AttributeValue (..),

    -- ** HistoricalMetricName
    HistoricalMetricName (..),

    -- ** FunctionArn
    FunctionArn (..),

    -- ** HierarchyStructure
    HierarchyStructure (..),
    mkHierarchyStructure,
    hsLevelFive,
    hsLevelFour,
    hsLevelOne,
    hsLevelThree,
    hsLevelTwo,

    -- ** InstanceStorageConfig
    InstanceStorageConfig (..),
    mkInstanceStorageConfig,
    iscStorageType,
    iscAssociationId,
    iscKinesisFirehoseConfig,
    iscKinesisStreamConfig,
    iscKinesisVideoStreamConfig,
    iscS3Config,

    -- ** KeyId
    KeyId (..),

    -- ** ParticipantId
    ParticipantId (..),

    -- ** InstanceStorageResourceType
    InstanceStorageResourceType (..),

    -- ** ContactFlow
    ContactFlow (..),
    mkContactFlow,
    cfArn,
    cfContent,
    cfDescription,
    cfId,
    cfName,
    cfTags,
    cfType,

    -- ** PhoneNumberId
    PhoneNumberId (..),

    -- ** ClientToken
    ClientToken (..),

    -- ** CurrentMetricName
    CurrentMetricName (..),

    -- ** RoutingProfileId
    RoutingProfileId (..),

    -- ** DirectoryUserId
    DirectoryUserId (..),

    -- ** ChatMessage
    ChatMessage (..),
    mkChatMessage,
    cmContentType,
    cmContent,

    -- ** SecurityToken
    SecurityToken (..),

    -- ** InstanceSummary
    InstanceSummary (..),
    mkInstanceSummary,
    isArn,
    isCreatedTime,
    isId,
    isIdentityManagementType,
    isInboundCallsEnabled,
    isInstanceAlias,
    isInstanceStatus,
    isOutboundCallsEnabled,
    isServiceRole,

    -- ** ARN
    ARN (..),

    -- ** RoutingProfileQueueReference
    RoutingProfileQueueReference (..),
    mkRoutingProfileQueueReference,
    rpqrQueueId,
    rpqrChannel,

    -- ** String
    String (..),

    -- ** LexBot
    LexBot (..),
    mkLexBot,
    lbLexRegion,
    lbName,

    -- ** PEM
    PEM (..),

    -- ** Credentials
    Credentials (..),
    mkCredentials,
    cAccessToken,
    cAccessTokenExpiration,
    cRefreshToken,
    cRefreshTokenExpiration,

    -- ** PromptSummary
    PromptSummary (..),
    mkPromptSummary,
    psArn,
    psId,
    psName,

    -- ** HierarchyGroup
    HierarchyGroup (..),
    mkHierarchyGroup,
    hgArn,
    hgHierarchyPath,
    hgId,
    hgLevelId,
    hgName,

    -- ** RoutingProfileName
    RoutingProfileName (..),

    -- ** Prefix
    Prefix (..),

    -- ** RoutingProfileQueueConfigSummary
    RoutingProfileQueueConfigSummary (..),
    mkRoutingProfileQueueConfigSummary,
    rpqcsQueueId,
    rpqcsQueueArn,
    rpqcsQueueName,
    rpqcsPriority,
    rpqcsDelay,
    rpqcsChannel,

    -- ** VoiceRecordingConfiguration
    VoiceRecordingConfiguration (..),
    mkVoiceRecordingConfiguration,
    vrcVoiceRecordingTrack,

    -- ** Filters
    Filters (..),
    mkFilters,
    fChannels,
    fQueues,

    -- ** ContactFlowContent
    ContactFlowContent (..),

    -- ** PhoneNumberSummary
    PhoneNumberSummary (..),
    mkPhoneNumberSummary,
    pnsArn,
    pnsId,
    pnsPhoneNumber,
    pnsPhoneNumberCountryCode,
    pnsPhoneNumberType,

    -- ** RoutingProfileSummary
    RoutingProfileSummary (..),
    mkRoutingProfileSummary,
    rpsArn,
    rpsId,
    rpsName,

    -- ** Channel
    Channel (..),

    -- ** QueueName
    QueueName (..),

    -- ** KinesisStreamConfig
    KinesisStreamConfig (..),
    mkKinesisStreamConfig,
    kscStreamArn,

    -- ** BotName
    BotName (..),

    -- ** HierarchyLevelName
    HierarchyLevelName (..),

    -- ** InstanceAttributeValue
    InstanceAttributeValue (..),

    -- ** HierarchyLevel
    HierarchyLevel (..),
    mkHierarchyLevel,
    hlArn,
    hlId,
    hlName,

    -- ** InstanceStatusReason
    InstanceStatusReason (..),
    mkInstanceStatusReason,
    isrMessage,

    -- ** TagValue
    TagValue (..),

    -- ** RoutingProfileDescription
    RoutingProfileDescription (..),

    -- ** DirectoryType
    DirectoryType (..),

    -- ** User
    User (..),
    mkUser,
    uArn,
    uDirectoryUserId,
    uHierarchyGroupId,
    uId,
    uIdentityInfo,
    uPhoneConfig,
    uRoutingProfileId,
    uSecurityProfileIds,
    uTags,
    uUsername,

    -- ** LexRegion
    LexRegion (..),

    -- ** MediaConcurrency
    MediaConcurrency (..),
    mkMediaConcurrency,
    mcChannel,
    mcConcurrency,

    -- ** Comparison
    Comparison (..),

    -- ** UserIdentityInfo
    UserIdentityInfo (..),
    mkUserIdentityInfo,
    uiiEmail,
    uiiFirstName,
    uiiLastName,

    -- ** BucketName
    BucketName (..),

    -- ** UserId
    UserId (..),

    -- ** NextToken
    NextToken (..),

    -- ** CurrentMetricResult
    CurrentMetricResult (..),
    mkCurrentMetricResult,
    cmrCollections,
    cmrDimensions,

    -- ** VoiceRecordingTrack
    VoiceRecordingTrack (..),

    -- ** PhoneType
    PhoneType (..),

    -- ** QueueId
    QueueId (..),

    -- ** Grouping
    Grouping (..),

    -- ** ContactFlowId
    ContactFlowId (..),

    -- ** DirectoryAlias
    DirectoryAlias (..),

    -- ** KinesisVideoStreamConfig
    KinesisVideoStreamConfig (..),
    mkKinesisVideoStreamConfig,
    kvscPrefix,
    kvscRetentionPeriodHours,
    kvscEncryptionConfig,

    -- ** AgentUsername
    AgentUsername (..),

    -- ** RoutingProfile
    RoutingProfile (..),
    mkRoutingProfile,
    rpDefaultOutboundQueueId,
    rpDescription,
    rpInstanceId,
    rpMediaConcurrencies,
    rpName,
    rpRoutingProfileArn,
    rpRoutingProfileId,
    rpTags,

    -- ** UserPhoneConfig
    UserPhoneConfig (..),
    mkUserPhoneConfig,
    upcPhoneType,
    upcAfterContactWorkTimeLimit,
    upcAutoAccept,
    upcDeskPhoneNumber,

    -- ** PhoneNumber
    PhoneNumber (..),

    -- ** Threshold
    Threshold (..),
    mkThreshold,
    tComparison,
    tThresholdValue,

    -- ** HierarchyGroupName
    HierarchyGroupName (..),

    -- ** Password
    Password (..),

    -- ** PhoneNumberCountryCode
    PhoneNumberCountryCode (..),

    -- ** HierarchyPath
    HierarchyPath (..),
    mkHierarchyPath,
    hpLevelFive,
    hpLevelFour,
    hpLevelOne,
    hpLevelThree,
    hpLevelTwo,

    -- ** ContactFlowType
    ContactFlowType (..),

    -- ** QueueReference
    QueueReference (..),
    mkQueueReference,
    qrArn,
    qrId,

    -- ** SecurityProfileName
    SecurityProfileName (..),

    -- ** S3Config
    S3Config (..),
    mkS3Config,
    scBucketName,
    scBucketPrefix,
    scEncryptionConfig,

    -- ** HoursOfOperationId
    HoursOfOperationId (..),

    -- ** ContactFlowName
    ContactFlowName (..),

    -- ** HierarchyGroupId
    HierarchyGroupId (..),

    -- ** UserSummary
    UserSummary (..),
    mkUserSummary,
    usArn,
    usId,
    usUsername,

    -- ** InstanceStatus
    InstanceStatus (..),

    -- ** DisplayName
    DisplayName (..),

    -- ** ContactId
    ContactId (..),

    -- ** ParticipantDetails
    ParticipantDetails (..),
    mkParticipantDetails,
    pdDisplayName,

    -- ** EncryptionConfig
    EncryptionConfig (..),
    mkEncryptionConfig,
    ecEncryptionType,
    ecKeyId,

    -- ** CurrentMetric
    CurrentMetric (..),
    mkCurrentMetric,
    cmName,
    cmUnit,

    -- ** QueueSummary
    QueueSummary (..),
    mkQueueSummary,
    qsArn,
    qsId,
    qsName,
    qsQueueType,

    -- ** InstanceAttributeType
    InstanceAttributeType (..),

    -- ** CurrentMetricData
    CurrentMetricData (..),
    mkCurrentMetricData,
    cmdMetric,
    cmdValue,

    -- ** TagKey
    TagKey (..),

    -- ** HierarchyLevelUpdate
    HierarchyLevelUpdate (..),
    mkHierarchyLevelUpdate,
    hluName,

    -- ** HistoricalMetric
    HistoricalMetric (..),
    mkHistoricalMetric,
    hmName,
    hmStatistic,
    hmThreshold,
    hmUnit,

    -- ** HistoricalMetricData
    HistoricalMetricData (..),
    mkHistoricalMetricData,
    hmdMetric,
    hmdValue,

    -- ** SecurityProfileId
    SecurityProfileId (..),

    -- ** HoursOfOperationName
    HoursOfOperationName (..),

    -- ** HierarchyStructureUpdate
    HierarchyStructureUpdate (..),
    mkHierarchyStructureUpdate,
    hsuLevelFive,
    hsuLevelFour,
    hsuLevelOne,
    hsuLevelThree,
    hsuLevelTwo,

    -- ** AttributeName
    AttributeName (..),

    -- ** Dimensions
    Dimensions (..),
    mkDimensions,
    dChannel,
    dQueue,

    -- ** KinesisFirehoseConfig
    KinesisFirehoseConfig (..),
    mkKinesisFirehoseConfig,
    kfcFirehoseArn,

    -- ** HoursOfOperationSummary
    HoursOfOperationSummary (..),
    mkHoursOfOperationSummary,
    hoosArn,
    hoosId,
    hoosName,

    -- ** Unit
    Unit (..),

    -- ** Statistic
    Statistic (..),

    -- ** ContactFlowSummary
    ContactFlowSummary (..),
    mkContactFlowSummary,
    cfsArn,
    cfsContactFlowType,
    cfsId,
    cfsName,

    -- ** QueueType
    QueueType (..),

    -- ** Instance
    Instance (..),
    mkInstance,
    iArn,
    iCreatedTime,
    iId,
    iIdentityManagementType,
    iInboundCallsEnabled,
    iInstanceAlias,
    iInstanceStatus,
    iOutboundCallsEnabled,
    iServiceRole,
    iStatusReason,

    -- ** HistoricalMetricResult
    HistoricalMetricResult (..),
    mkHistoricalMetricResult,
    hmrCollections,
    hmrDimensions,

    -- ** SecurityKey
    SecurityKey (..),
    mkSecurityKey,
    skAssociationId,
    skCreationTime,
    skKey,

    -- ** StorageType
    StorageType (..),

    -- ** SecurityProfileSummary
    SecurityProfileSummary (..),
    mkSecurityProfileSummary,
    spsArn,
    spsId,
    spsName,

    -- ** InitialContactId
    InitialContactId (..),

    -- ** Arn
    Arn (..),

    -- ** Id
    Id (..),

    -- ** Name
    Name (..),

    -- ** InstanceAlias
    InstanceAlias (..),

    -- ** Content
    Content (..),

    -- ** Description
    Description (..),

    -- ** DefaultOutboundQueueId
    DefaultOutboundQueueId (..),

    -- ** ServiceRole
    ServiceRole (..),

    -- ** LevelId
    LevelId (..),

    -- ** Username
    Username (..),

    -- ** ParentGroupId
    ParentGroupId (..),

    -- ** DeskPhoneNumber
    DeskPhoneNumber (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
