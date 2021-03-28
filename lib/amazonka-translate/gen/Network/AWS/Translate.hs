{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides translation between one source language and another of the same set of languages.
module Network.AWS.Translate
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** UnsupportedLanguagePairException
    , _UnsupportedLanguagePairException

    -- ** DetectedLanguageLowConfidenceException
    , _DetectedLanguageLowConfidenceException

    -- ** ConflictException
    , _ConflictException

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** InternalServerException
    , _InternalServerException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** InvalidFilterException
    , _InvalidFilterException

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

    -- ** DescribeTextTranslationJob 
    , module Network.AWS.Translate.DescribeTextTranslationJob

    -- ** ListTerminologies (Paginated)
    , module Network.AWS.Translate.ListTerminologies

    -- ** CreateParallelData 
    , module Network.AWS.Translate.CreateParallelData

    -- ** UpdateParallelData 
    , module Network.AWS.Translate.UpdateParallelData

    -- ** DeleteParallelData 
    , module Network.AWS.Translate.DeleteParallelData

    -- ** GetParallelData 
    , module Network.AWS.Translate.GetParallelData

    -- ** GetTerminology 
    , module Network.AWS.Translate.GetTerminology

    -- ** TranslateText 
    , module Network.AWS.Translate.TranslateText

    -- ** ImportTerminology 
    , module Network.AWS.Translate.ImportTerminology

    -- ** StopTextTranslationJob 
    , module Network.AWS.Translate.StopTextTranslationJob

    -- ** DeleteTerminology 
    , module Network.AWS.Translate.DeleteTerminology

    -- ** ListTextTranslationJobs 
    , module Network.AWS.Translate.ListTextTranslationJobs

    -- ** StartTextTranslationJob 
    , module Network.AWS.Translate.StartTextTranslationJob

    -- ** ListParallelData 
    , module Network.AWS.Translate.ListParallelData

    -- * Types

    -- ** IamRoleArn
    , IamRoleArn (..)

    -- ** ClientTokenString
    , ClientTokenString (..)

    -- ** TerminologyProperties
    , TerminologyProperties (..)
    , mkTerminologyProperties
    , tpArn
    , tpCreatedAt
    , tpDescription
    , tpEncryptionKey
    , tpLastUpdatedAt
    , tpName
    , tpSizeBytes
    , tpSourceLanguageCode
    , tpTargetLanguageCodes
    , tpTermCount

    -- ** LanguageCodeString
    , LanguageCodeString (..)

    -- ** ParallelDataProperties
    , ParallelDataProperties (..)
    , mkParallelDataProperties
    , pdpArn
    , pdpCreatedAt
    , pdpDescription
    , pdpEncryptionKey
    , pdpFailedRecordCount
    , pdpImportedDataSize
    , pdpImportedRecordCount
    , pdpLastUpdatedAt
    , pdpLatestUpdateAttemptAt
    , pdpLatestUpdateAttemptStatus
    , pdpMessage
    , pdpName
    , pdpParallelDataConfig
    , pdpSkippedRecordCount
    , pdpSourceLanguageCode
    , pdpStatus
    , pdpTargetLanguageCodes

    -- ** JobId
    , JobId (..)

    -- ** ResourceName
    , ResourceName (..)

    -- ** EncryptionKeyID
    , EncryptionKeyID (..)

    -- ** TextTranslationJobProperties
    , TextTranslationJobProperties (..)
    , mkTextTranslationJobProperties
    , ttjpDataAccessRoleArn
    , ttjpEndTime
    , ttjpInputDataConfig
    , ttjpJobDetails
    , ttjpJobId
    , ttjpJobName
    , ttjpJobStatus
    , ttjpMessage
    , ttjpOutputDataConfig
    , ttjpParallelDataNames
    , ttjpSourceLanguageCode
    , ttjpSubmittedTime
    , ttjpTargetLanguageCodes
    , ttjpTerminologyNames

    -- ** JobName
    , JobName (..)

    -- ** EncryptionKeyType
    , EncryptionKeyType (..)

    -- ** ParallelDataDataLocation
    , ParallelDataDataLocation (..)
    , mkParallelDataDataLocation
    , pddlRepositoryType
    , pddlLocation

    -- ** Term
    , Term (..)
    , mkTerm
    , tSourceText
    , tTargetText

    -- ** TerminologyDataLocation
    , TerminologyDataLocation (..)
    , mkTerminologyDataLocation
    , tdlRepositoryType
    , tdlLocation

    -- ** TextTranslationJobFilter
    , TextTranslationJobFilter (..)
    , mkTextTranslationJobFilter
    , ttjfJobName
    , ttjfJobStatus
    , ttjfSubmittedAfterTime
    , ttjfSubmittedBeforeTime

    -- ** NextToken
    , NextToken (..)

    -- ** InputDataConfig
    , InputDataConfig (..)
    , mkInputDataConfig
    , idcS3Uri
    , idcContentType

    -- ** ParallelDataStatus
    , ParallelDataStatus (..)

    -- ** OutputDataConfig
    , OutputDataConfig (..)
    , mkOutputDataConfig
    , odcS3Uri

    -- ** JobDetails
    , JobDetails (..)
    , mkJobDetails
    , jdDocumentsWithErrorsCount
    , jdInputDocumentsCount
    , jdTranslatedDocumentsCount

    -- ** EncryptionKey
    , EncryptionKey (..)
    , mkEncryptionKey
    , ekType
    , ekId

    -- ** TerminologyDataFormat
    , TerminologyDataFormat (..)

    -- ** ParallelDataFormat
    , ParallelDataFormat (..)

    -- ** JobStatus
    , JobStatus (..)

    -- ** MergeStrategy
    , MergeStrategy (..)

    -- ** TerminologyData
    , TerminologyData (..)
    , mkTerminologyData
    , tdFile
    , tdFormat

    -- ** Description
    , Description (..)

    -- ** S3Uri
    , S3Uri (..)

    -- ** AppliedTerminology
    , AppliedTerminology (..)
    , mkAppliedTerminology
    , atName
    , atTerms

    -- ** ContentType
    , ContentType (..)

    -- ** ParallelDataConfig
    , ParallelDataConfig (..)
    , mkParallelDataConfig
    , pdcS3Uri
    , pdcFormat

    -- ** Name
    , Name (..)

    -- ** Arn
    , Arn (..)

    -- ** SourceLanguageCode
    , SourceLanguageCode (..)

    -- ** Text
    , Text (..)

    -- ** TargetLanguageCode
    , TargetLanguageCode (..)

    -- ** Message
    , Message (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Translate.Types
import Network.AWS.Translate.Waiters
import Network.AWS.Translate.DescribeTextTranslationJob
import Network.AWS.Translate.ListTerminologies
import Network.AWS.Translate.CreateParallelData
import Network.AWS.Translate.UpdateParallelData
import Network.AWS.Translate.DeleteParallelData
import Network.AWS.Translate.GetParallelData
import Network.AWS.Translate.GetTerminology
import Network.AWS.Translate.TranslateText
import Network.AWS.Translate.ImportTerminology
import Network.AWS.Translate.StopTextTranslationJob
import Network.AWS.Translate.DeleteTerminology
import Network.AWS.Translate.ListTextTranslationJobs
import Network.AWS.Translate.StartTextTranslationJob
import Network.AWS.Translate.ListParallelData
import qualified Network.AWS.Prelude as Lude

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
