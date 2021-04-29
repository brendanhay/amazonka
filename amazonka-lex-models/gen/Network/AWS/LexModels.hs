{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Lex Build-Time Actions
--
-- Amazon Lex is an AWS service for building conversational voice and text
-- interfaces. Use these actions to create, update, and delete
-- conversational bots for new and existing client applications.
module Network.AWS.LexModels
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** PreconditionFailedException
    _PreconditionFailedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteSlotTypeVersion
    DeleteSlotTypeVersion (DeleteSlotTypeVersion'),
    newDeleteSlotTypeVersion,
    DeleteSlotTypeVersionResponse (DeleteSlotTypeVersionResponse'),
    newDeleteSlotTypeVersionResponse,

    -- ** GetBots (Paginated)
    GetBots (GetBots'),
    newGetBots,
    GetBotsResponse (GetBotsResponse'),
    newGetBotsResponse,

    -- ** GetSlotTypes (Paginated)
    GetSlotTypes (GetSlotTypes'),
    newGetSlotTypes,
    GetSlotTypesResponse (GetSlotTypesResponse'),
    newGetSlotTypesResponse,

    -- ** DeleteUtterances
    DeleteUtterances (DeleteUtterances'),
    newDeleteUtterances,
    DeleteUtterancesResponse (DeleteUtterancesResponse'),
    newDeleteUtterancesResponse,

    -- ** GetBotAlias
    GetBotAlias (GetBotAlias'),
    newGetBotAlias,
    GetBotAliasResponse (GetBotAliasResponse'),
    newGetBotAliasResponse,

    -- ** GetBotChannelAssociations (Paginated)
    GetBotChannelAssociations (GetBotChannelAssociations'),
    newGetBotChannelAssociations,
    GetBotChannelAssociationsResponse (GetBotChannelAssociationsResponse'),
    newGetBotChannelAssociationsResponse,

    -- ** PutBotAlias
    PutBotAlias (PutBotAlias'),
    newPutBotAlias,
    PutBotAliasResponse (PutBotAliasResponse'),
    newPutBotAliasResponse,

    -- ** GetUtterancesView
    GetUtterancesView (GetUtterancesView'),
    newGetUtterancesView,
    GetUtterancesViewResponse (GetUtterancesViewResponse'),
    newGetUtterancesViewResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetBuiltinIntent
    GetBuiltinIntent (GetBuiltinIntent'),
    newGetBuiltinIntent,
    GetBuiltinIntentResponse (GetBuiltinIntentResponse'),
    newGetBuiltinIntentResponse,

    -- ** GetSlotTypeVersions (Paginated)
    GetSlotTypeVersions (GetSlotTypeVersions'),
    newGetSlotTypeVersions,
    GetSlotTypeVersionsResponse (GetSlotTypeVersionsResponse'),
    newGetSlotTypeVersionsResponse,

    -- ** GetBuiltinSlotTypes (Paginated)
    GetBuiltinSlotTypes (GetBuiltinSlotTypes'),
    newGetBuiltinSlotTypes,
    GetBuiltinSlotTypesResponse (GetBuiltinSlotTypesResponse'),
    newGetBuiltinSlotTypesResponse,

    -- ** PutBot
    PutBot (PutBot'),
    newPutBot,
    PutBotResponse (PutBotResponse'),
    newPutBotResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DeleteSlotType
    DeleteSlotType (DeleteSlotType'),
    newDeleteSlotType,
    DeleteSlotTypeResponse (DeleteSlotTypeResponse'),
    newDeleteSlotTypeResponse,

    -- ** PutIntent
    PutIntent (PutIntent'),
    newPutIntent,
    PutIntentResponse (PutIntentResponse'),
    newPutIntentResponse,

    -- ** GetBotChannelAssociation
    GetBotChannelAssociation (GetBotChannelAssociation'),
    newGetBotChannelAssociation,
    GetBotChannelAssociationResponse (GetBotChannelAssociationResponse'),
    newGetBotChannelAssociationResponse,

    -- ** CreateIntentVersion
    CreateIntentVersion (CreateIntentVersion'),
    newCreateIntentVersion,
    CreateIntentVersionResponse (CreateIntentVersionResponse'),
    newCreateIntentVersionResponse,

    -- ** GetExport
    GetExport (GetExport'),
    newGetExport,
    GetExportResponse (GetExportResponse'),
    newGetExportResponse,

    -- ** GetSlotType
    GetSlotType (GetSlotType'),
    newGetSlotType,
    GetSlotTypeResponse (GetSlotTypeResponse'),
    newGetSlotTypeResponse,

    -- ** DeleteIntentVersion
    DeleteIntentVersion (DeleteIntentVersion'),
    newDeleteIntentVersion,
    DeleteIntentVersionResponse (DeleteIntentVersionResponse'),
    newDeleteIntentVersionResponse,

    -- ** CreateBotVersion
    CreateBotVersion (CreateBotVersion'),
    newCreateBotVersion,
    CreateBotVersionResponse (CreateBotVersionResponse'),
    newCreateBotVersionResponse,

    -- ** GetBot
    GetBot (GetBot'),
    newGetBot,
    GetBotResponse (GetBotResponse'),
    newGetBotResponse,

    -- ** GetBotAliases (Paginated)
    GetBotAliases (GetBotAliases'),
    newGetBotAliases,
    GetBotAliasesResponse (GetBotAliasesResponse'),
    newGetBotAliasesResponse,

    -- ** GetIntents (Paginated)
    GetIntents (GetIntents'),
    newGetIntents,
    GetIntentsResponse (GetIntentsResponse'),
    newGetIntentsResponse,

    -- ** GetBotVersions (Paginated)
    GetBotVersions (GetBotVersions'),
    newGetBotVersions,
    GetBotVersionsResponse (GetBotVersionsResponse'),
    newGetBotVersionsResponse,

    -- ** DeleteBotAlias
    DeleteBotAlias (DeleteBotAlias'),
    newDeleteBotAlias,
    DeleteBotAliasResponse (DeleteBotAliasResponse'),
    newDeleteBotAliasResponse,

    -- ** GetImport
    GetImport (GetImport'),
    newGetImport,
    GetImportResponse (GetImportResponse'),
    newGetImportResponse,

    -- ** GetIntentVersions (Paginated)
    GetIntentVersions (GetIntentVersions'),
    newGetIntentVersions,
    GetIntentVersionsResponse (GetIntentVersionsResponse'),
    newGetIntentVersionsResponse,

    -- ** GetBuiltinIntents (Paginated)
    GetBuiltinIntents (GetBuiltinIntents'),
    newGetBuiltinIntents,
    GetBuiltinIntentsResponse (GetBuiltinIntentsResponse'),
    newGetBuiltinIntentsResponse,

    -- ** DeleteBot
    DeleteBot (DeleteBot'),
    newDeleteBot,
    DeleteBotResponse (DeleteBotResponse'),
    newDeleteBotResponse,

    -- ** PutSlotType
    PutSlotType (PutSlotType'),
    newPutSlotType,
    PutSlotTypeResponse (PutSlotTypeResponse'),
    newPutSlotTypeResponse,

    -- ** StartImport
    StartImport (StartImport'),
    newStartImport,
    StartImportResponse (StartImportResponse'),
    newStartImportResponse,

    -- ** DeleteIntent
    DeleteIntent (DeleteIntent'),
    newDeleteIntent,
    DeleteIntentResponse (DeleteIntentResponse'),
    newDeleteIntentResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateSlotTypeVersion
    CreateSlotTypeVersion (CreateSlotTypeVersion'),
    newCreateSlotTypeVersion,
    CreateSlotTypeVersionResponse (CreateSlotTypeVersionResponse'),
    newCreateSlotTypeVersionResponse,

    -- ** GetIntent
    GetIntent (GetIntent'),
    newGetIntent,
    GetIntentResponse (GetIntentResponse'),
    newGetIntentResponse,

    -- ** DeleteBotVersion
    DeleteBotVersion (DeleteBotVersion'),
    newDeleteBotVersion,
    DeleteBotVersionResponse (DeleteBotVersionResponse'),
    newDeleteBotVersionResponse,

    -- ** DeleteBotChannelAssociation
    DeleteBotChannelAssociation (DeleteBotChannelAssociation'),
    newDeleteBotChannelAssociation,
    DeleteBotChannelAssociationResponse (DeleteBotChannelAssociationResponse'),
    newDeleteBotChannelAssociationResponse,

    -- * Types

    -- ** ChannelStatus
    ChannelStatus (..),

    -- ** ChannelType
    ChannelType (..),

    -- ** ContentType
    ContentType (..),

    -- ** Destination
    Destination (..),

    -- ** ExportStatus
    ExportStatus (..),

    -- ** ExportType
    ExportType (..),

    -- ** FulfillmentActivityType
    FulfillmentActivityType (..),

    -- ** ImportStatus
    ImportStatus (..),

    -- ** LexStatus
    LexStatus (..),

    -- ** Locale
    Locale (..),

    -- ** LogType
    LogType (..),

    -- ** MergeStrategy
    MergeStrategy (..),

    -- ** ObfuscationSetting
    ObfuscationSetting (..),

    -- ** ProcessBehavior
    ProcessBehavior (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** SlotConstraint
    SlotConstraint (..),

    -- ** SlotValueSelectionStrategy
    SlotValueSelectionStrategy (..),

    -- ** StatusType
    StatusType (..),

    -- ** BotAliasMetadata
    BotAliasMetadata (BotAliasMetadata'),
    newBotAliasMetadata,

    -- ** BotChannelAssociation
    BotChannelAssociation (BotChannelAssociation'),
    newBotChannelAssociation,

    -- ** BotMetadata
    BotMetadata (BotMetadata'),
    newBotMetadata,

    -- ** BuiltinIntentMetadata
    BuiltinIntentMetadata (BuiltinIntentMetadata'),
    newBuiltinIntentMetadata,

    -- ** BuiltinIntentSlot
    BuiltinIntentSlot (BuiltinIntentSlot'),
    newBuiltinIntentSlot,

    -- ** BuiltinSlotTypeMetadata
    BuiltinSlotTypeMetadata (BuiltinSlotTypeMetadata'),
    newBuiltinSlotTypeMetadata,

    -- ** CodeHook
    CodeHook (CodeHook'),
    newCodeHook,

    -- ** ConversationLogsRequest
    ConversationLogsRequest (ConversationLogsRequest'),
    newConversationLogsRequest,

    -- ** ConversationLogsResponse
    ConversationLogsResponse (ConversationLogsResponse'),
    newConversationLogsResponse,

    -- ** EnumerationValue
    EnumerationValue (EnumerationValue'),
    newEnumerationValue,

    -- ** FollowUpPrompt
    FollowUpPrompt (FollowUpPrompt'),
    newFollowUpPrompt,

    -- ** FulfillmentActivity
    FulfillmentActivity (FulfillmentActivity'),
    newFulfillmentActivity,

    -- ** InputContext
    InputContext (InputContext'),
    newInputContext,

    -- ** Intent
    Intent (Intent'),
    newIntent,

    -- ** IntentMetadata
    IntentMetadata (IntentMetadata'),
    newIntentMetadata,

    -- ** KendraConfiguration
    KendraConfiguration (KendraConfiguration'),
    newKendraConfiguration,

    -- ** LogSettingsRequest
    LogSettingsRequest (LogSettingsRequest'),
    newLogSettingsRequest,

    -- ** LogSettingsResponse
    LogSettingsResponse (LogSettingsResponse'),
    newLogSettingsResponse,

    -- ** Message
    Message (Message'),
    newMessage,

    -- ** OutputContext
    OutputContext (OutputContext'),
    newOutputContext,

    -- ** Prompt
    Prompt (Prompt'),
    newPrompt,

    -- ** Slot
    Slot (Slot'),
    newSlot,

    -- ** SlotDefaultValue
    SlotDefaultValue (SlotDefaultValue'),
    newSlotDefaultValue,

    -- ** SlotDefaultValueSpec
    SlotDefaultValueSpec (SlotDefaultValueSpec'),
    newSlotDefaultValueSpec,

    -- ** SlotTypeConfiguration
    SlotTypeConfiguration (SlotTypeConfiguration'),
    newSlotTypeConfiguration,

    -- ** SlotTypeMetadata
    SlotTypeMetadata (SlotTypeMetadata'),
    newSlotTypeMetadata,

    -- ** SlotTypeRegexConfiguration
    SlotTypeRegexConfiguration (SlotTypeRegexConfiguration'),
    newSlotTypeRegexConfiguration,

    -- ** Statement
    Statement (Statement'),
    newStatement,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UtteranceData
    UtteranceData (UtteranceData'),
    newUtteranceData,

    -- ** UtteranceList
    UtteranceList (UtteranceList'),
    newUtteranceList,
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
import Network.AWS.LexModels.Lens
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
