{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Lex Build-Time Actions__
--
-- Amazon Lex is an AWS service for building conversational voice and text interfaces. Use these actions to create, update, and delete conversational bots for new and existing client applications.
--
module Network.AWS.LexModels
    (
    -- * Service Configuration
      lexModels

    -- * Errors
    -- $errors

    -- ** PreconditionFailedException
    , _PreconditionFailedException

    -- ** ConflictException
    , _ConflictException

    -- ** NotFoundException
    , _NotFoundException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** BadRequestException
    , _BadRequestException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteIntentVersion
    , module Network.AWS.LexModels.DeleteIntentVersion

    -- ** GetBotAliases (Paginated)
    , module Network.AWS.LexModels.GetBotAliases

    -- ** DeleteBotChannelAssociation
    , module Network.AWS.LexModels.DeleteBotChannelAssociation

    -- ** CreateSlotTypeVersion
    , module Network.AWS.LexModels.CreateSlotTypeVersion

    -- ** GetIntent
    , module Network.AWS.LexModels.GetIntent

    -- ** PutIntent
    , module Network.AWS.LexModels.PutIntent

    -- ** DeleteIntent
    , module Network.AWS.LexModels.DeleteIntent

    -- ** GetBuiltinIntents (Paginated)
    , module Network.AWS.LexModels.GetBuiltinIntents

    -- ** PutBot
    , module Network.AWS.LexModels.PutBot

    -- ** DeleteBot
    , module Network.AWS.LexModels.DeleteBot

    -- ** GetImport
    , module Network.AWS.LexModels.GetImport

    -- ** GetIntentVersions (Paginated)
    , module Network.AWS.LexModels.GetIntentVersions

    -- ** GetBuiltinIntent
    , module Network.AWS.LexModels.GetBuiltinIntent

    -- ** PutBotAlias
    , module Network.AWS.LexModels.PutBotAlias

    -- ** GetBotVersions (Paginated)
    , module Network.AWS.LexModels.GetBotVersions

    -- ** GetBotChannelAssociations (Paginated)
    , module Network.AWS.LexModels.GetBotChannelAssociations

    -- ** DeleteBotAlias
    , module Network.AWS.LexModels.DeleteBotAlias

    -- ** GetSlotTypes (Paginated)
    , module Network.AWS.LexModels.GetSlotTypes

    -- ** DeleteUtterances
    , module Network.AWS.LexModels.DeleteUtterances

    -- ** GetBots (Paginated)
    , module Network.AWS.LexModels.GetBots

    -- ** GetBot
    , module Network.AWS.LexModels.GetBot

    -- ** CreateBotVersion
    , module Network.AWS.LexModels.CreateBotVersion

    -- ** DeleteSlotTypeVersion
    , module Network.AWS.LexModels.DeleteSlotTypeVersion

    -- ** DeleteBotVersion
    , module Network.AWS.LexModels.DeleteBotVersion

    -- ** GetSlotType
    , module Network.AWS.LexModels.GetSlotType

    -- ** GetExport
    , module Network.AWS.LexModels.GetExport

    -- ** CreateIntentVersion
    , module Network.AWS.LexModels.CreateIntentVersion

    -- ** DeleteSlotType
    , module Network.AWS.LexModels.DeleteSlotType

    -- ** StartImport
    , module Network.AWS.LexModels.StartImport

    -- ** GetBotChannelAssociation
    , module Network.AWS.LexModels.GetBotChannelAssociation

    -- ** PutSlotType
    , module Network.AWS.LexModels.PutSlotType

    -- ** GetBuiltinSlotTypes (Paginated)
    , module Network.AWS.LexModels.GetBuiltinSlotTypes

    -- ** GetUtterancesView
    , module Network.AWS.LexModels.GetUtterancesView

    -- ** GetSlotTypeVersions (Paginated)
    , module Network.AWS.LexModels.GetSlotTypeVersions

    -- ** GetIntents (Paginated)
    , module Network.AWS.LexModels.GetIntents

    -- ** GetBotAlias
    , module Network.AWS.LexModels.GetBotAlias

    -- * Types

    -- ** ChannelStatus
    , ChannelStatus (..)

    -- ** ChannelType
    , ChannelType (..)

    -- ** ContentType
    , ContentType (..)

    -- ** ExportStatus
    , ExportStatus (..)

    -- ** ExportType
    , ExportType (..)

    -- ** FulfillmentActivityType
    , FulfillmentActivityType (..)

    -- ** ImportStatus
    , ImportStatus (..)

    -- ** LexStatus
    , LexStatus (..)

    -- ** Locale
    , Locale (..)

    -- ** MergeStrategy
    , MergeStrategy (..)

    -- ** ProcessBehavior
    , ProcessBehavior (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** SlotConstraint
    , SlotConstraint (..)

    -- ** SlotValueSelectionStrategy
    , SlotValueSelectionStrategy (..)

    -- ** StatusType
    , StatusType (..)

    -- ** BotAliasMetadata
    , BotAliasMetadata
    , botAliasMetadata
    , bamChecksum
    , bamBotVersion
    , bamBotName
    , bamCreatedDate
    , bamName
    , bamLastUpdatedDate
    , bamDescription

    -- ** BotChannelAssociation
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

    -- ** BotMetadata
    , BotMetadata
    , botMetadata
    , bmStatus
    , bmCreatedDate
    , bmName
    , bmVersion
    , bmLastUpdatedDate
    , bmDescription

    -- ** BuiltinIntentMetadata
    , BuiltinIntentMetadata
    , builtinIntentMetadata
    , bimSignature
    , bimSupportedLocales

    -- ** BuiltinIntentSlot
    , BuiltinIntentSlot
    , builtinIntentSlot
    , bisName

    -- ** BuiltinSlotTypeMetadata
    , BuiltinSlotTypeMetadata
    , builtinSlotTypeMetadata
    , bstmSignature
    , bstmSupportedLocales

    -- ** CodeHook
    , CodeHook
    , codeHook
    , chUri
    , chMessageVersion

    -- ** EnumerationValue
    , EnumerationValue
    , enumerationValue
    , evSynonyms
    , evValue

    -- ** FollowUpPrompt
    , FollowUpPrompt
    , followUpPrompt
    , fupPrompt
    , fupRejectionStatement

    -- ** FulfillmentActivity
    , FulfillmentActivity
    , fulfillmentActivity
    , faCodeHook
    , faType

    -- ** Intent
    , Intent
    , intent
    , iIntentName
    , iIntentVersion

    -- ** IntentMetadata
    , IntentMetadata
    , intentMetadata
    , imCreatedDate
    , imName
    , imVersion
    , imLastUpdatedDate
    , imDescription

    -- ** Message
    , Message
    , message
    , mGroupNumber
    , mContentType
    , mContent

    -- ** Prompt
    , Prompt
    , prompt
    , pResponseCard
    , pMessages
    , pMaxAttempts

    -- ** Slot
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

    -- ** SlotTypeMetadata
    , SlotTypeMetadata
    , slotTypeMetadata
    , stmCreatedDate
    , stmName
    , stmVersion
    , stmLastUpdatedDate
    , stmDescription

    -- ** Statement
    , Statement
    , statement
    , staResponseCard
    , staMessages

    -- ** UtteranceData
    , UtteranceData
    , utteranceData
    , udFirstUtteredDate
    , udCount
    , udUtteranceString
    , udLastUtteredDate
    , udDistinctUsers

    -- ** UtteranceList
    , UtteranceList
    , utteranceList
    , ulBotVersion
    , ulUtterances
    ) where

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
import Network.AWS.LexModels.GetBots
import Network.AWS.LexModels.GetBotVersions
import Network.AWS.LexModels.GetBuiltinIntent
import Network.AWS.LexModels.GetBuiltinIntents
import Network.AWS.LexModels.GetBuiltinSlotTypes
import Network.AWS.LexModels.GetExport
import Network.AWS.LexModels.GetImport
import Network.AWS.LexModels.GetIntent
import Network.AWS.LexModels.GetIntents
import Network.AWS.LexModels.GetIntentVersions
import Network.AWS.LexModels.GetSlotType
import Network.AWS.LexModels.GetSlotTypes
import Network.AWS.LexModels.GetSlotTypeVersions
import Network.AWS.LexModels.GetUtterancesView
import Network.AWS.LexModels.PutBot
import Network.AWS.LexModels.PutBotAlias
import Network.AWS.LexModels.PutIntent
import Network.AWS.LexModels.PutSlotType
import Network.AWS.LexModels.StartImport
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'LexModels'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
