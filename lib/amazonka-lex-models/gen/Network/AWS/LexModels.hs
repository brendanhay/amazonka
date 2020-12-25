{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Lex Build-Time Actions__
--
-- Amazon Lex is an AWS service for building conversational voice and text interfaces. Use these actions to create, update, and delete conversational bots for new and existing client applications.
module Network.AWS.LexModels
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** PreconditionFailedException
    _PreconditionFailedException,

    -- ** ConflictException
    _ConflictException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteIntentVersion
    module Network.AWS.LexModels.DeleteIntentVersion,

    -- ** GetBotAliases (Paginated)
    module Network.AWS.LexModels.GetBotAliases,

    -- ** DeleteBotChannelAssociation
    module Network.AWS.LexModels.DeleteBotChannelAssociation,

    -- ** CreateSlotTypeVersion
    module Network.AWS.LexModels.CreateSlotTypeVersion,

    -- ** ListTagsForResource
    module Network.AWS.LexModels.ListTagsForResource,

    -- ** GetIntent
    module Network.AWS.LexModels.GetIntent,

    -- ** PutIntent
    module Network.AWS.LexModels.PutIntent,

    -- ** DeleteIntent
    module Network.AWS.LexModels.DeleteIntent,

    -- ** GetBuiltinIntents (Paginated)
    module Network.AWS.LexModels.GetBuiltinIntents,

    -- ** PutBot
    module Network.AWS.LexModels.PutBot,

    -- ** DeleteBot
    module Network.AWS.LexModels.DeleteBot,

    -- ** GetImport
    module Network.AWS.LexModels.GetImport,

    -- ** GetIntentVersions (Paginated)
    module Network.AWS.LexModels.GetIntentVersions,

    -- ** GetBuiltinIntent
    module Network.AWS.LexModels.GetBuiltinIntent,

    -- ** PutBotAlias
    module Network.AWS.LexModels.PutBotAlias,

    -- ** GetBotVersions (Paginated)
    module Network.AWS.LexModels.GetBotVersions,

    -- ** GetBotChannelAssociations (Paginated)
    module Network.AWS.LexModels.GetBotChannelAssociations,

    -- ** DeleteBotAlias
    module Network.AWS.LexModels.DeleteBotAlias,

    -- ** GetSlotTypes (Paginated)
    module Network.AWS.LexModels.GetSlotTypes,

    -- ** DeleteUtterances
    module Network.AWS.LexModels.DeleteUtterances,

    -- ** GetBots (Paginated)
    module Network.AWS.LexModels.GetBots,

    -- ** GetBot
    module Network.AWS.LexModels.GetBot,

    -- ** CreateBotVersion
    module Network.AWS.LexModels.CreateBotVersion,

    -- ** DeleteSlotTypeVersion
    module Network.AWS.LexModels.DeleteSlotTypeVersion,

    -- ** DeleteBotVersion
    module Network.AWS.LexModels.DeleteBotVersion,

    -- ** GetSlotType
    module Network.AWS.LexModels.GetSlotType,

    -- ** GetExport
    module Network.AWS.LexModels.GetExport,

    -- ** CreateIntentVersion
    module Network.AWS.LexModels.CreateIntentVersion,

    -- ** DeleteSlotType
    module Network.AWS.LexModels.DeleteSlotType,

    -- ** StartImport
    module Network.AWS.LexModels.StartImport,

    -- ** GetBotChannelAssociation
    module Network.AWS.LexModels.GetBotChannelAssociation,

    -- ** PutSlotType
    module Network.AWS.LexModels.PutSlotType,

    -- ** GetBuiltinSlotTypes (Paginated)
    module Network.AWS.LexModels.GetBuiltinSlotTypes,

    -- ** TagResource
    module Network.AWS.LexModels.TagResource,

    -- ** GetUtterancesView
    module Network.AWS.LexModels.GetUtterancesView,

    -- ** GetSlotTypeVersions (Paginated)
    module Network.AWS.LexModels.GetSlotTypeVersions,

    -- ** UntagResource
    module Network.AWS.LexModels.UntagResource,

    -- ** GetIntents (Paginated)
    module Network.AWS.LexModels.GetIntents,

    -- ** GetBotAlias
    module Network.AWS.LexModels.GetBotAlias,

    -- * Types

    -- ** SlotDefaultValue
    SlotDefaultValue (..),
    mkSlotDefaultValue,
    sdvDefaultValue,

    -- ** LexStatus
    LexStatus (..),

    -- ** IamRoleArn
    IamRoleArn (..),

    -- ** ContentString
    ContentString (..),

    -- ** CustomOrBuiltinSlotTypeName
    CustomOrBuiltinSlotTypeName (..),

    -- ** Prompt
    Prompt (..),
    mkPrompt,
    pMessages,
    pMaxAttempts,
    pResponseCard,

    -- ** Destination
    Destination (..),

    -- ** FulfillmentActivity
    FulfillmentActivity (..),
    mkFulfillmentActivity,
    faType,
    faCodeHook,

    -- ** QueryFilterString
    QueryFilterString (..),

    -- ** ResponseCard
    ResponseCard (..),

    -- ** KmsKeyArn
    KmsKeyArn (..),

    -- ** ChannelStatus
    ChannelStatus (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** LambdaARN
    LambdaARN (..),

    -- ** EnumerationValue
    EnumerationValue (..),
    mkEnumerationValue,
    evValue,
    evSynonyms,

    -- ** ResourceType
    ResourceType (..),

    -- ** LogSettingsResponse
    LogSettingsResponse (..),
    mkLogSettingsResponse,
    lsrDestination,
    lsrKmsKeyArn,
    lsrLogType,
    lsrResourceArn,
    lsrResourcePrefix,

    -- ** ObfuscationSetting
    ObfuscationSetting (..),

    -- ** String
    String (..),

    -- ** Statement
    Statement (..),
    mkStatement,
    sMessages,
    sResponseCard,

    -- ** BotChannelName
    BotChannelName (..),

    -- ** ExportStatus
    ExportStatus (..),

    -- ** Locale
    Locale (..),

    -- ** OutputContext
    OutputContext (..),
    mkOutputContext,
    ocName,
    ocTimeToLiveInSeconds,
    ocTurnsToLive,

    -- ** LogSettingsRequest
    LogSettingsRequest (..),
    mkLogSettingsRequest,
    lLogType,
    lDestination,
    lResourceArn,
    lKmsKeyArn,

    -- ** BuiltinIntentMetadata
    BuiltinIntentMetadata (..),
    mkBuiltinIntentMetadata,
    bimSignature,
    bimSupportedLocales,

    -- ** BotChannelAssociation
    BotChannelAssociation (..),
    mkBotChannelAssociation,
    bcaBotAlias,
    bcaBotConfiguration,
    bcaBotName,
    bcaCreatedDate,
    bcaDescription,
    bcaFailureReason,
    bcaName,
    bcaStatus,
    bcaType,

    -- ** SlotName
    SlotName (..),

    -- ** IntentName
    IntentName (..),

    -- ** ConversationLogsRequest
    ConversationLogsRequest (..),
    mkConversationLogsRequest,
    cLogSettings,
    cIamRoleArn,

    -- ** Value
    Value (..),

    -- ** CodeHook
    CodeHook (..),
    mkCodeHook,
    chUri,
    chMessageVersion,

    -- ** UtteranceString
    UtteranceString (..),

    -- ** BotName
    BotName (..),

    -- ** TagValue
    TagValue (..),

    -- ** SlotConstraint
    SlotConstraint (..),

    -- ** BuiltinIntentSlot
    BuiltinIntentSlot (..),
    mkBuiltinIntentSlot,
    bisName,

    -- ** AliasName
    AliasName (..),

    -- ** SlotTypeRegexConfiguration
    SlotTypeRegexConfiguration (..),
    mkSlotTypeRegexConfiguration,
    strcPattern,

    -- ** IntentMetadata
    IntentMetadata (..),
    mkIntentMetadata,
    imCreatedDate,
    imDescription,
    imLastUpdatedDate,
    imName,
    imVersion,

    -- ** BuiltinIntentSignature
    BuiltinIntentSignature (..),

    -- ** LogType
    LogType (..),

    -- ** SlotValueSelectionStrategy
    SlotValueSelectionStrategy (..),

    -- ** UserId
    UserId (..),

    -- ** ExportType
    ExportType (..),

    -- ** NextToken
    NextToken (..),

    -- ** NumericalVersion
    NumericalVersion (..),

    -- ** SlotTypeConfiguration
    SlotTypeConfiguration (..),
    mkSlotTypeConfiguration,
    stcRegexConfiguration,

    -- ** BotMetadata
    BotMetadata (..),
    mkBotMetadata,
    bmCreatedDate,
    bmDescription,
    bmLastUpdatedDate,
    bmName,
    bmStatus,
    bmVersion,

    -- ** Slot
    Slot (..),
    mkSlot,
    sfName,
    sfSlotConstraint,
    sfDefaultValueSpec,
    sfDescription,
    sfObfuscationSetting,
    sfPriority,
    sfResponseCard,
    sfSampleUtterances,
    sfSlotType,
    sfSlotTypeVersion,
    sfValueElicitationPrompt,

    -- ** Intent
    Intent (..),
    mkIntent,
    iIntentName,
    iIntentVersion,

    -- ** KendraIndexArn
    KendraIndexArn (..),

    -- ** StatusType
    StatusType (..),

    -- ** ProcessBehavior
    ProcessBehavior (..),

    -- ** FulfillmentActivityType
    FulfillmentActivityType (..),

    -- ** ResourceArn
    ResourceArn (..),

    -- ** KendraConfiguration
    KendraConfiguration (..),
    mkKendraConfiguration,
    kcKendraIndex,
    kcRole,
    kcQueryFilterString,

    -- ** Name
    Name (..),

    -- ** OutputContextName
    OutputContextName (..),

    -- ** Version
    Version (..),

    -- ** BotAliasMetadata
    BotAliasMetadata (..),
    mkBotAliasMetadata,
    bamBotName,
    bamBotVersion,
    bamChecksum,
    bamConversationLogs,
    bamCreatedDate,
    bamDescription,
    bamLastUpdatedDate,
    bamName,

    -- ** ResourcePrefix
    ResourcePrefix (..),

    -- ** TagKey
    TagKey (..),

    -- ** BuiltinSlotTypeMetadata
    BuiltinSlotTypeMetadata (..),
    mkBuiltinSlotTypeMetadata,
    bstmSignature,
    bstmSupportedLocales,

    -- ** FollowUpPrompt
    FollowUpPrompt (..),
    mkFollowUpPrompt,
    fupPrompt,
    fupRejectionStatement,

    -- ** UtteranceData
    UtteranceData (..),
    mkUtteranceData,
    udCount,
    udDistinctUsers,
    udFirstUtteredDate,
    udLastUtteredDate,
    udUtteranceString,

    -- ** MergeStrategy
    MergeStrategy (..),

    -- ** Message
    Message (..),
    mkMessage,
    mContentType,
    mContent,
    mGroupNumber,

    -- ** AmazonResourceName
    AmazonResourceName (..),

    -- ** SlotTypeName
    SlotTypeName (..),

    -- ** Utterance
    Utterance (..),

    -- ** ChannelType
    ChannelType (..),

    -- ** Description
    Description (..),

    -- ** InputContextName
    InputContextName (..),

    -- ** MessageVersion
    MessageVersion (..),

    -- ** SlotTypeMetadata
    SlotTypeMetadata (..),
    mkSlotTypeMetadata,
    stmCreatedDate,
    stmDescription,
    stmLastUpdatedDate,
    stmName,
    stmVersion,

    -- ** ContentType
    ContentType (..),

    -- ** InputContext
    InputContext (..),
    mkInputContext,
    icName,

    -- ** UtteranceList
    UtteranceList (..),
    mkUtteranceList,
    ulBotVersion,
    ulUtterances,

    -- ** ImportStatus
    ImportStatus (..),

    -- ** SlotDefaultValueSpec
    SlotDefaultValueSpec (..),
    mkSlotDefaultValueSpec,
    sdvsDefaultValueList,

    -- ** ConversationLogsResponse
    ConversationLogsResponse (..),
    mkConversationLogsResponse,
    clrIamRoleArn,
    clrLogSettings,

    -- ** DefaultValue
    DefaultValue (..),

    -- ** Checksum
    Checksum (..),

    -- ** ParentIntentSignature
    ParentIntentSignature (..),

    -- ** BotAlias
    BotAlias (..),

    -- ** NameContains
    NameContains (..),

    -- ** BotVersion
    BotVersion (..),

    -- ** Key
    Key (..),

    -- ** Signature
    Signature (..),

    -- ** ImportId
    ImportId (..),

    -- ** Pattern
    Pattern (..),

    -- ** SlotTypeVersion
    SlotTypeVersion (..),

    -- ** IntentVersion
    IntentVersion (..),

    -- ** Role
    Role (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.LexModels.CreateBotVersion
import Network.AWS.LexModels.CreateIntentVersion
import Network.AWS.LexModels.CreateSlotTypeVersion
import Network.AWS.LexModels.DeleteBot
import Network.AWS.LexModels.DeleteBotAlias
import Network.AWS.LexModels.DeleteBotChannelAssociation
import Network.AWS.LexModels.DeleteBotVersion
import Network.AWS.LexModels.DeleteIntent
import Network.AWS.LexModels.DeleteIntentVersion
import Network.AWS.LexModels.DeleteSlotType
import Network.AWS.LexModels.DeleteSlotTypeVersion
import Network.AWS.LexModels.DeleteUtterances
import Network.AWS.LexModels.GetBot
import Network.AWS.LexModels.GetBotAlias
import Network.AWS.LexModels.GetBotAliases
import Network.AWS.LexModels.GetBotChannelAssociation
import Network.AWS.LexModels.GetBotChannelAssociations
import Network.AWS.LexModels.GetBotVersions
import Network.AWS.LexModels.GetBots
import Network.AWS.LexModels.GetBuiltinIntent
import Network.AWS.LexModels.GetBuiltinIntents
import Network.AWS.LexModels.GetBuiltinSlotTypes
import Network.AWS.LexModels.GetExport
import Network.AWS.LexModels.GetImport
import Network.AWS.LexModels.GetIntent
import Network.AWS.LexModels.GetIntentVersions
import Network.AWS.LexModels.GetIntents
import Network.AWS.LexModels.GetSlotType
import Network.AWS.LexModels.GetSlotTypeVersions
import Network.AWS.LexModels.GetSlotTypes
import Network.AWS.LexModels.GetUtterancesView
import Network.AWS.LexModels.ListTagsForResource
import Network.AWS.LexModels.PutBot
import Network.AWS.LexModels.PutBotAlias
import Network.AWS.LexModels.PutIntent
import Network.AWS.LexModels.PutSlotType
import Network.AWS.LexModels.StartImport
import Network.AWS.LexModels.TagResource
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.UntagResource
import Network.AWS.LexModels.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LexModels'.

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
