{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides translation between one source language and another of the same set of languages.
--
--
module Network.AWS.Translate
    (
    -- * Service Configuration
      translate

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** UnsupportedLanguagePairException
    , _UnsupportedLanguagePairException

    -- ** DetectedLanguageLowConfidenceException
    , _DetectedLanguageLowConfidenceException

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** InternalServerException
    , _InternalServerException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** TextSizeLimitExceededException
    , _TextSizeLimitExceededException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListTerminologies (Paginated)
    , module Network.AWS.Translate.ListTerminologies

    -- ** GetTerminology
    , module Network.AWS.Translate.GetTerminology

    -- ** TranslateText
    , module Network.AWS.Translate.TranslateText

    -- ** ImportTerminology
    , module Network.AWS.Translate.ImportTerminology

    -- ** DeleteTerminology
    , module Network.AWS.Translate.DeleteTerminology

    -- * Types

    -- ** EncryptionKeyType
    , EncryptionKeyType (..)

    -- ** MergeStrategy
    , MergeStrategy (..)

    -- ** TerminologyDataFormat
    , TerminologyDataFormat (..)

    -- ** AppliedTerminology
    , AppliedTerminology
    , appliedTerminology
    , atTerms
    , atName

    -- ** EncryptionKey
    , EncryptionKey
    , encryptionKey
    , ekType
    , ekId

    -- ** Term
    , Term
    , term
    , tTargetText
    , tSourceText

    -- ** TerminologyData
    , TerminologyData
    , terminologyData
    , tdFile
    , tdFormat

    -- ** TerminologyDataLocation
    , TerminologyDataLocation
    , terminologyDataLocation
    , tdlRepositoryType
    , tdlLocation

    -- ** TerminologyProperties
    , TerminologyProperties
    , terminologyProperties
    , tpSizeBytes
    , tpLastUpdatedAt
    , tpARN
    , tpTargetLanguageCodes
    , tpCreatedAt
    , tpName
    , tpSourceLanguageCode
    , tpTermCount
    , tpEncryptionKey
    , tpDescription
    ) where

import Network.AWS.Translate.DeleteTerminology
import Network.AWS.Translate.GetTerminology
import Network.AWS.Translate.ImportTerminology
import Network.AWS.Translate.ListTerminologies
import Network.AWS.Translate.TranslateText
import Network.AWS.Translate.Types
import Network.AWS.Translate.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Translate'.
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
