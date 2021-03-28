-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _PreconditionFailedException
    , _ConflictException
    , _NotFoundException
    , _InternalFailureException
    , _BadRequestException
    , _LimitExceededException
    , _ResourceInUseException

    -- * SlotDefaultValue
    , SlotDefaultValue (..)
    , mkSlotDefaultValue
    , sdvDefaultValue

    -- * LexStatus
    , LexStatus (..)

    -- * IamRoleArn
    , IamRoleArn (..)

    -- * ContentString
    , ContentString (..)

    -- * CustomOrBuiltinSlotTypeName
    , CustomOrBuiltinSlotTypeName (..)

    -- * Prompt
    , Prompt (..)
    , mkPrompt
    , pMessages
    , pMaxAttempts
    , pResponseCard

    -- * Destination
    , Destination (..)

    -- * FulfillmentActivity
    , FulfillmentActivity (..)
    , mkFulfillmentActivity
    , faType
    , faCodeHook

    -- * QueryFilterString
    , QueryFilterString (..)

    -- * ResponseCard
    , ResponseCard (..)

    -- * KmsKeyArn
    , KmsKeyArn (..)

    -- * ChannelStatus
    , ChannelStatus (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * LambdaARN
    , LambdaARN (..)

    -- * EnumerationValue
    , EnumerationValue (..)
    , mkEnumerationValue
    , evValue
    , evSynonyms

    -- * ResourceType
    , ResourceType (..)

    -- * LogSettingsResponse
    , LogSettingsResponse (..)
    , mkLogSettingsResponse
    , lsrDestination
    , lsrKmsKeyArn
    , lsrLogType
    , lsrResourceArn
    , lsrResourcePrefix

    -- * ObfuscationSetting
    , ObfuscationSetting (..)

    -- * Statement
    , Statement (..)
    , mkStatement
    , sMessages
    , sResponseCard

    -- * BotChannelName
    , BotChannelName (..)

    -- * ExportStatus
    , ExportStatus (..)

    -- * Locale
    , Locale (..)

    -- * OutputContext
    , OutputContext (..)
    , mkOutputContext
    , ocName
    , ocTimeToLiveInSeconds
    , ocTurnsToLive

    -- * LogSettingsRequest
    , LogSettingsRequest (..)
    , mkLogSettingsRequest
    , lLogType
    , lDestination
    , lResourceArn
    , lKmsKeyArn

    -- * BuiltinIntentMetadata
    , BuiltinIntentMetadata (..)
    , mkBuiltinIntentMetadata
    , bimSignature
    , bimSupportedLocales

    -- * BotChannelAssociation
    , BotChannelAssociation (..)
    , mkBotChannelAssociation
    , bcaBotAlias
    , bcaBotConfiguration
    , bcaBotName
    , bcaCreatedDate
    , bcaDescription
    , bcaFailureReason
    , bcaName
    , bcaStatus
    , bcaType

    -- * SlotName
    , SlotName (..)

    -- * IntentName
    , IntentName (..)

    -- * ConversationLogsRequest
    , ConversationLogsRequest (..)
    , mkConversationLogsRequest
    , cLogSettings
    , cIamRoleArn

    -- * Value
    , Value (..)

    -- * CodeHook
    , CodeHook (..)
    , mkCodeHook
    , chUri
    , chMessageVersion

    -- * UtteranceString
    , UtteranceString (..)

    -- * BotName
    , BotName (..)

    -- * TagValue
    , TagValue (..)

    -- * SlotConstraint
    , SlotConstraint (..)

    -- * BuiltinIntentSlot
    , BuiltinIntentSlot (..)
    , mkBuiltinIntentSlot
    , bisName

    -- * AliasName
    , AliasName (..)

    -- * SlotTypeRegexConfiguration
    , SlotTypeRegexConfiguration (..)
    , mkSlotTypeRegexConfiguration
    , strcPattern

    -- * IntentMetadata
    , IntentMetadata (..)
    , mkIntentMetadata
    , imCreatedDate
    , imDescription
    , imLastUpdatedDate
    , imName
    , imVersion

    -- * BuiltinIntentSignature
    , BuiltinIntentSignature (..)

    -- * LogType
    , LogType (..)

    -- * SlotValueSelectionStrategy
    , SlotValueSelectionStrategy (..)

    -- * UserId
    , UserId (..)

    -- * ExportType
    , ExportType (..)

    -- * NextToken
    , NextToken (..)

    -- * NumericalVersion
    , NumericalVersion (..)

    -- * SlotTypeConfiguration
    , SlotTypeConfiguration (..)
    , mkSlotTypeConfiguration
    , stcRegexConfiguration

    -- * BotMetadata
    , BotMetadata (..)
    , mkBotMetadata
    , bmCreatedDate
    , bmDescription
    , bmLastUpdatedDate
    , bmName
    , bmStatus
    , bmVersion

    -- * Slot
    , Slot (..)
    , mkSlot
    , sfName
    , sfSlotConstraint
    , sfDefaultValueSpec
    , sfDescription
    , sfObfuscationSetting
    , sfPriority
    , sfResponseCard
    , sfSampleUtterances
    , sfSlotType
    , sfSlotTypeVersion
    , sfValueElicitationPrompt

    -- * Intent
    , Intent (..)
    , mkIntent
    , iIntentName
    , iIntentVersion

    -- * KendraIndexArn
    , KendraIndexArn (..)

    -- * StatusType
    , StatusType (..)

    -- * ProcessBehavior
    , ProcessBehavior (..)

    -- * FulfillmentActivityType
    , FulfillmentActivityType (..)

    -- * ResourceArn
    , ResourceArn (..)

    -- * KendraConfiguration
    , KendraConfiguration (..)
    , mkKendraConfiguration
    , kcKendraIndex
    , kcRole
    , kcQueryFilterString

    -- * Name
    , Name (..)

    -- * OutputContextName
    , OutputContextName (..)

    -- * Version
    , Version (..)

    -- * BotAliasMetadata
    , BotAliasMetadata (..)
    , mkBotAliasMetadata
    , bamBotName
    , bamBotVersion
    , bamChecksum
    , bamConversationLogs
    , bamCreatedDate
    , bamDescription
    , bamLastUpdatedDate
    , bamName

    -- * ResourcePrefix
    , ResourcePrefix (..)

    -- * TagKey
    , TagKey (..)

    -- * BuiltinSlotTypeMetadata
    , BuiltinSlotTypeMetadata (..)
    , mkBuiltinSlotTypeMetadata
    , bstmSignature
    , bstmSupportedLocales

    -- * FollowUpPrompt
    , FollowUpPrompt (..)
    , mkFollowUpPrompt
    , fupPrompt
    , fupRejectionStatement

    -- * UtteranceData
    , UtteranceData (..)
    , mkUtteranceData
    , udCount
    , udDistinctUsers
    , udFirstUtteredDate
    , udLastUtteredDate
    , udUtteranceString

    -- * MergeStrategy
    , MergeStrategy (..)

    -- * Message
    , Message (..)
    , mkMessage
    , mContentType
    , mContent
    , mGroupNumber

    -- * AmazonResourceName
    , AmazonResourceName (..)

    -- * SlotTypeName
    , SlotTypeName (..)

    -- * Utterance
    , Utterance (..)

    -- * ChannelType
    , ChannelType (..)

    -- * Description
    , Description (..)

    -- * InputContextName
    , InputContextName (..)

    -- * MessageVersion
    , MessageVersion (..)

    -- * SlotTypeMetadata
    , SlotTypeMetadata (..)
    , mkSlotTypeMetadata
    , stmCreatedDate
    , stmDescription
    , stmLastUpdatedDate
    , stmName
    , stmVersion

    -- * ContentType
    , ContentType (..)

    -- * InputContext
    , InputContext (..)
    , mkInputContext
    , icName

    -- * UtteranceList
    , UtteranceList (..)
    , mkUtteranceList
    , ulBotVersion
    , ulUtterances

    -- * ImportStatus
    , ImportStatus (..)

    -- * SlotDefaultValueSpec
    , SlotDefaultValueSpec (..)
    , mkSlotDefaultValueSpec
    , sdvsDefaultValueList

    -- * ConversationLogsResponse
    , ConversationLogsResponse (..)
    , mkConversationLogsResponse
    , clrIamRoleArn
    , clrLogSettings

    -- * DefaultValue
    , DefaultValue (..)

    -- * ParentIntentSignature
    , ParentIntentSignature (..)

    -- * BotAlias
    , BotAlias (..)

    -- * NameContains
    , NameContains (..)

    -- * BotVersion
    , BotVersion (..)

    -- * Key
    , Key (..)

    -- * Signature
    , Signature (..)

    -- * Pattern
    , Pattern (..)

    -- * SlotTypeVersion
    , SlotTypeVersion (..)

    -- * IntentVersion
    , IntentVersion (..)

    -- * Role
    , Role (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.LexModels.Types.SlotDefaultValue
  
import Network.AWS.LexModels.Types.LexStatus
  
import Network.AWS.LexModels.Types.IamRoleArn
  
import Network.AWS.LexModels.Types.ContentString
  
import Network.AWS.LexModels.Types.CustomOrBuiltinSlotTypeName
  
import Network.AWS.LexModels.Types.Prompt
  
import Network.AWS.LexModels.Types.Destination
  
import Network.AWS.LexModels.Types.FulfillmentActivity
  
import Network.AWS.LexModels.Types.QueryFilterString
  
import Network.AWS.LexModels.Types.ResponseCard
  
import Network.AWS.LexModels.Types.KmsKeyArn
  
import Network.AWS.LexModels.Types.ChannelStatus
  
import Network.AWS.LexModels.Types.Tag
  
import Network.AWS.LexModels.Types.LambdaARN
  
import Network.AWS.LexModels.Types.EnumerationValue
  
import Network.AWS.LexModels.Types.ResourceType
  
import Network.AWS.LexModels.Types.LogSettingsResponse
  
import Network.AWS.LexModels.Types.ObfuscationSetting
  
import Network.AWS.LexModels.Types.Statement
  
import Network.AWS.LexModels.Types.BotChannelName
  
import Network.AWS.LexModels.Types.ExportStatus
  
import Network.AWS.LexModels.Types.Locale
  
  
import Network.AWS.LexModels.Types.OutputContext
  
import Network.AWS.LexModels.Types.LogSettingsRequest
  
import Network.AWS.LexModels.Types.BuiltinIntentMetadata
  
import Network.AWS.LexModels.Types.BotChannelAssociation
  
import Network.AWS.LexModels.Types.SlotName
  
import Network.AWS.LexModels.Types.IntentName
  
import Network.AWS.LexModels.Types.ConversationLogsRequest
  
import Network.AWS.LexModels.Types.Value
  
import Network.AWS.LexModels.Types.CodeHook
  
import Network.AWS.LexModels.Types.UtteranceString
  
import Network.AWS.LexModels.Types.BotName
  
import Network.AWS.LexModels.Types.TagValue
  
import Network.AWS.LexModels.Types.SlotConstraint
  
  
import Network.AWS.LexModels.Types.BuiltinIntentSlot
  
import Network.AWS.LexModels.Types.AliasName
  
import Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
  
import Network.AWS.LexModels.Types.IntentMetadata
  
import Network.AWS.LexModels.Types.BuiltinIntentSignature
  
import Network.AWS.LexModels.Types.LogType
  
import Network.AWS.LexModels.Types.SlotValueSelectionStrategy
  
import Network.AWS.LexModels.Types.UserId
  
import Network.AWS.LexModels.Types.ExportType
  
import Network.AWS.LexModels.Types.NextToken
  
import Network.AWS.LexModels.Types.NumericalVersion
  
import Network.AWS.LexModels.Types.SlotTypeConfiguration
  
import Network.AWS.LexModels.Types.BotMetadata
  
import Network.AWS.LexModels.Types.Slot
  
  
import Network.AWS.LexModels.Types.Intent
  
import Network.AWS.LexModels.Types.KendraIndexArn
  
import Network.AWS.LexModels.Types.StatusType
  
import Network.AWS.LexModels.Types.ProcessBehavior
  
import Network.AWS.LexModels.Types.FulfillmentActivityType
  
import Network.AWS.LexModels.Types.ResourceArn
  
import Network.AWS.LexModels.Types.KendraConfiguration
  
import Network.AWS.LexModels.Types.Name
  
import Network.AWS.LexModels.Types.OutputContextName
  
import Network.AWS.LexModels.Types.Version
  
import Network.AWS.LexModels.Types.BotAliasMetadata
  
import Network.AWS.LexModels.Types.ResourcePrefix
  
import Network.AWS.LexModels.Types.TagKey
  
  
import Network.AWS.LexModels.Types.BuiltinSlotTypeMetadata
  
import Network.AWS.LexModels.Types.FollowUpPrompt
  
import Network.AWS.LexModels.Types.UtteranceData
  
import Network.AWS.LexModels.Types.MergeStrategy
  
import Network.AWS.LexModels.Types.Message
  
import Network.AWS.LexModels.Types.AmazonResourceName
  
import Network.AWS.LexModels.Types.SlotTypeName
  
import Network.AWS.LexModels.Types.Utterance
  
import Network.AWS.LexModels.Types.ChannelType
  
import Network.AWS.LexModels.Types.Description
  
import Network.AWS.LexModels.Types.InputContextName
  
import Network.AWS.LexModels.Types.MessageVersion
  
  
import Network.AWS.LexModels.Types.SlotTypeMetadata
  
import Network.AWS.LexModels.Types.ContentType
  
import Network.AWS.LexModels.Types.InputContext
  
import Network.AWS.LexModels.Types.UtteranceList
  
  
  
import Network.AWS.LexModels.Types.ImportStatus
  
import Network.AWS.LexModels.Types.SlotDefaultValueSpec
  
import Network.AWS.LexModels.Types.ConversationLogsResponse
  
import Network.AWS.LexModels.Types.DefaultValue
  
import Network.AWS.LexModels.Types.ParentIntentSignature
  
import Network.AWS.LexModels.Types.BotAlias
  
import Network.AWS.LexModels.Types.NameContains
  
import Network.AWS.LexModels.Types.BotVersion
  
import Network.AWS.LexModels.Types.Key
  
import Network.AWS.LexModels.Types.Signature
  
import Network.AWS.LexModels.Types.Pattern
  
import Network.AWS.LexModels.Types.SlotTypeVersion
  
import Network.AWS.LexModels.Types.IntentVersion
  
import Network.AWS.LexModels.Types.Role
  

-- | API version @2017-04-19@ of the Amazon Lex Model Building Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "LexModels",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "models.lex",
                 Core._svcVersion = "2017-04-19", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "LexModels",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The checksum of the resource that you are trying to change does not match the checksum in the request. Check the resource's checksum and try again.
_PreconditionFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PreconditionFailedException
  = Core._MatchServiceError mkServiceConfig
      "PreconditionFailedException"
      Core.. Core.hasStatues 412
{-# INLINEABLE _PreconditionFailedException #-}
{-# DEPRECATED _PreconditionFailedException "Use generic-lens or generic-optics instead"  #-}

-- | There was a conflict processing the request. Try your request again. 
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException
  = Core._MatchServiceError mkServiceConfig "ConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _ConflictException #-}
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead"  #-}

-- | The resource specified in the request was not found. Check the resource and try again.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException
  = Core._MatchServiceError mkServiceConfig "NotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NotFoundException #-}
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | An internal Amazon Lex error occurred. Try your request again.
_InternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalFailureException
  = Core._MatchServiceError mkServiceConfig
      "InternalFailureException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalFailureException #-}
{-# DEPRECATED _InternalFailureException "Use generic-lens or generic-optics instead"  #-}

-- | The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException
  = Core._MatchServiceError mkServiceConfig "BadRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _BadRequestException #-}
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead"  #-}

-- | The request exceeded a limit. Try your request again.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 429
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The resource that you are attempting to delete is referred to by another resource. Use this information to remove references to the resource that you are trying to delete.
--
-- The body of the exception contains a JSON object that describes the resource.
-- @{ "resourceType": BOT | BOTALIAS | BOTCHANNEL | INTENT,@ 
-- @"resourceReference": {@ 
-- @"name": /string/ , "version": /string/ } }@ 
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
