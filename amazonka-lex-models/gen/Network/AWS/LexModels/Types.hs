{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types
    (
    -- * Service Configuration
      lexModels

    -- * Errors
    , _PreconditionFailedException
    , _ConflictException
    , _NotFoundException
    , _InternalFailureException
    , _BadRequestException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ChannelStatus
    , ChannelStatus (..)

    -- * ChannelType
    , ChannelType (..)

    -- * ContentType
    , ContentType (..)

    -- * ExportStatus
    , ExportStatus (..)

    -- * ExportType
    , ExportType (..)

    -- * FulfillmentActivityType
    , FulfillmentActivityType (..)

    -- * ImportStatus
    , ImportStatus (..)

    -- * LexStatus
    , LexStatus (..)

    -- * Locale
    , Locale (..)

    -- * MergeStrategy
    , MergeStrategy (..)

    -- * ProcessBehavior
    , ProcessBehavior (..)

    -- * ResourceType
    , ResourceType (..)

    -- * SlotConstraint
    , SlotConstraint (..)

    -- * SlotValueSelectionStrategy
    , SlotValueSelectionStrategy (..)

    -- * StatusType
    , StatusType (..)

    -- * BotAliasMetadata
    , BotAliasMetadata
    , botAliasMetadata
    , bamChecksum
    , bamBotVersion
    , bamBotName
    , bamCreatedDate
    , bamName
    , bamLastUpdatedDate
    , bamDescription

    -- * BotChannelAssociation
    , BotChannelAssociation
    , botChannelAssociation
    , bcaFailureReason
    , bcaStatus
    , bcaBotAlias
    , bcaBotName
    , bcaBotConfiguration
    , bcaCreatedDate
    , bcaName
    , bcaType
    , bcaDescription

    -- * BotMetadata
    , BotMetadata
    , botMetadata
    , bmStatus
    , bmCreatedDate
    , bmName
    , bmVersion
    , bmLastUpdatedDate
    , bmDescription

    -- * BuiltinIntentMetadata
    , BuiltinIntentMetadata
    , builtinIntentMetadata
    , bimSignature
    , bimSupportedLocales

    -- * BuiltinIntentSlot
    , BuiltinIntentSlot
    , builtinIntentSlot
    , bisName

    -- * BuiltinSlotTypeMetadata
    , BuiltinSlotTypeMetadata
    , builtinSlotTypeMetadata
    , bstmSignature
    , bstmSupportedLocales

    -- * CodeHook
    , CodeHook
    , codeHook
    , chUri
    , chMessageVersion

    -- * EnumerationValue
    , EnumerationValue
    , enumerationValue
    , evSynonyms
    , evValue

    -- * FollowUpPrompt
    , FollowUpPrompt
    , followUpPrompt
    , fupPrompt
    , fupRejectionStatement

    -- * FulfillmentActivity
    , FulfillmentActivity
    , fulfillmentActivity
    , faCodeHook
    , faType

    -- * Intent
    , Intent
    , intent
    , iIntentName
    , iIntentVersion

    -- * IntentMetadata
    , IntentMetadata
    , intentMetadata
    , imCreatedDate
    , imName
    , imVersion
    , imLastUpdatedDate
    , imDescription

    -- * Message
    , Message
    , message
    , mGroupNumber
    , mContentType
    , mContent

    -- * Prompt
    , Prompt
    , prompt
    , pResponseCard
    , pMessages
    , pMaxAttempts

    -- * Slot
    , Slot
    , slot
    , sSlotType
    , sValueElicitationPrompt
    , sResponseCard
    , sPriority
    , sSlotTypeVersion
    , sSampleUtterances
    , sDescription
    , sName
    , sSlotConstraint

    -- * SlotTypeMetadata
    , SlotTypeMetadata
    , slotTypeMetadata
    , stmCreatedDate
    , stmName
    , stmVersion
    , stmLastUpdatedDate
    , stmDescription

    -- * Statement
    , Statement
    , statement
    , staResponseCard
    , staMessages

    -- * UtteranceData
    , UtteranceData
    , utteranceData
    , udFirstUtteredDate
    , udCount
    , udUtteranceString
    , udLastUtteredDate
    , udDistinctUsers

    -- * UtteranceList
    , UtteranceList
    , utteranceList
    , ulBotVersion
    , ulUtterances
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.Product
import Network.AWS.LexModels.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-04-19@ of the Amazon Lex Model Building Service SDK configuration.
lexModels :: Service
lexModels =
  Service
    { _svcAbbrev = "LexModels"
    , _svcSigner = v4
    , _svcPrefix = "models.lex"
    , _svcVersion = "2017-04-19"
    , _svcEndpoint = defaultEndpoint lexModels
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "LexModels"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The checksum of the resource that you are trying to change does not match the checksum in the request. Check the resource's checksum and try again.
--
--
_PreconditionFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_PreconditionFailedException =
  _MatchServiceError lexModels "PreconditionFailedException" . hasStatus 412


-- | There was a conflict processing the request. Try your request again.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException =
  _MatchServiceError lexModels "ConflictException" . hasStatus 409


-- | The resource specified in the request was not found. Check the resource and try again.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException =
  _MatchServiceError lexModels "NotFoundException" . hasStatus 404


-- | An internal Amazon Lex error occurred. Try your request again.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
  _MatchServiceError lexModels "InternalFailureException" . hasStatus 500


-- | The request is not well formed. For example, a value is invalid or a required field is missing. Check the field values, and try again.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
  _MatchServiceError lexModels "BadRequestException" . hasStatus 400


-- | The request exceeded a limit. Try your request again.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError lexModels "LimitExceededException" . hasStatus 429


-- | The resource that you are attempting to delete is referred to by another resource. Use this information to remove references to the resource that you are trying to delete.
--
--
-- The body of the exception contains a JSON object that describes the resource.
--
-- @{ "resourceType": BOT | BOTALIAS | BOTCHANNEL | INTENT,@
--
-- @"resourceReference": {@
--
-- @"name": /string/ , "version": /string/ } }@
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
  _MatchServiceError lexModels "ResourceInUseException" . hasStatus 400

