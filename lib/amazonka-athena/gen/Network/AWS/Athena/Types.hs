-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidRequestException
    , _MetadataException
    , _TooManyRequestsException
    , _InternalServerException
    , _ResourceNotFoundException

    -- * WorkGroupConfigurationUpdates
    , WorkGroupConfigurationUpdates (..)
    , mkWorkGroupConfigurationUpdates
    , wgcuBytesScannedCutoffPerQuery
    , wgcuEnforceWorkGroupConfiguration
    , wgcuPublishCloudWatchMetricsEnabled
    , wgcuRemoveBytesScannedCutoffPerQuery
    , wgcuRequesterPaysEnabled
    , wgcuResultConfigurationUpdates

    -- * NamedQueryId
    , NamedQueryId (..)

    -- * EncryptionOption
    , EncryptionOption (..)

    -- * IdempotencyToken
    , IdempotencyToken (..)

    -- * QueryExecutionStatus
    , QueryExecutionStatus (..)
    , mkQueryExecutionStatus
    , qesCompletionDateTime
    , qesState
    , qesStateChangeReason
    , qesSubmissionDateTime

    -- * UnprocessedQueryExecutionId
    , UnprocessedQueryExecutionId (..)
    , mkUnprocessedQueryExecutionId
    , uqeiErrorCode
    , uqeiErrorMessage
    , uqeiQueryExecutionId

    -- * QueryExecutionState
    , QueryExecutionState (..)

    -- * QueryExecutionContext
    , QueryExecutionContext (..)
    , mkQueryExecutionContext
    , qecCatalog
    , qecDatabase

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * WorkGroupSummary
    , WorkGroupSummary (..)
    , mkWorkGroupSummary
    , wgsCreationTime
    , wgsDescription
    , wgsName
    , wgsState

    -- * Database
    , Database (..)
    , mkDatabase
    , dName
    , dDescription
    , dParameters

    -- * WorkGroupDescriptionString
    , WorkGroupDescriptionString (..)

    -- * ParametersMapValue
    , ParametersMapValue (..)

    -- * DatabaseString
    , DatabaseString (..)

    -- * QueryExecution
    , QueryExecution (..)
    , mkQueryExecution
    , qeQuery
    , qeQueryExecutionContext
    , qeQueryExecutionId
    , qeResultConfiguration
    , qeStatementType
    , qeStatistics
    , qeStatus
    , qeWorkGroup

    -- * ColumnInfo
    , ColumnInfo (..)
    , mkColumnInfo
    , ciName
    , ciType
    , ciCaseSensitive
    , ciCatalogName
    , ciLabel
    , ciNullable
    , ciPrecision
    , ciScale
    , ciSchemaName
    , ciTableName

    -- * Token
    , Token (..)

    -- * DataCatalog
    , DataCatalog (..)
    , mkDataCatalog
    , dcName
    , dcType
    , dcDescription
    , dcParameters

    -- * ResultConfigurationUpdates
    , ResultConfigurationUpdates (..)
    , mkResultConfigurationUpdates
    , rcuEncryptionConfiguration
    , rcuOutputLocation
    , rcuRemoveEncryptionConfiguration
    , rcuRemoveOutputLocation

    -- * DescriptionString
    , DescriptionString (..)

    -- * WorkGroupConfiguration
    , WorkGroupConfiguration (..)
    , mkWorkGroupConfiguration
    , wgcBytesScannedCutoffPerQuery
    , wgcEnforceWorkGroupConfiguration
    , wgcPublishCloudWatchMetricsEnabled
    , wgcRequesterPaysEnabled
    , wgcResultConfiguration

    -- * CommentString
    , CommentString (..)

    -- * WorkGroupState
    , WorkGroupState (..)

    -- * ResultConfiguration
    , ResultConfiguration (..)
    , mkResultConfiguration
    , rcEncryptionConfiguration
    , rcOutputLocation

    -- * DatumString
    , DatumString (..)

    -- * DataCatalogSummary
    , DataCatalogSummary (..)
    , mkDataCatalogSummary
    , dcsCatalogName
    , dcsType

    -- * ColumnNullable
    , ColumnNullable (..)

    -- * QueryString
    , QueryString (..)

    -- * EncryptionConfiguration
    , EncryptionConfiguration (..)
    , mkEncryptionConfiguration
    , ecEncryptionOption
    , ecKmsKey

    -- * NameString
    , NameString (..)

    -- * KeyString
    , KeyString (..)

    -- * Row
    , Row (..)
    , mkRow
    , rData

    -- * ErrorCode
    , ErrorCode (..)

    -- * StatementType
    , StatementType (..)

    -- * QueryExecutionStatistics
    , QueryExecutionStatistics (..)
    , mkQueryExecutionStatistics
    , qesDataManifestLocation
    , qesDataScannedInBytes
    , qesEngineExecutionTimeInMillis
    , qesQueryPlanningTimeInMillis
    , qesQueryQueueTimeInMillis
    , qesServiceProcessingTimeInMillis
    , qesTotalExecutionTimeInMillis

    -- * UnprocessedNamedQueryId
    , UnprocessedNamedQueryId (..)
    , mkUnprocessedNamedQueryId
    , unqiErrorCode
    , unqiErrorMessage
    , unqiNamedQueryId

    -- * TypeString
    , TypeString (..)

    -- * QueryExecutionId
    , QueryExecutionId (..)

    -- * TagKey
    , TagKey (..)

    -- * TableMetadata
    , TableMetadata (..)
    , mkTableMetadata
    , tmName
    , tmColumns
    , tmCreateTime
    , tmLastAccessTime
    , tmParameters
    , tmPartitionKeys
    , tmTableType

    -- * ResultSet
    , ResultSet (..)
    , mkResultSet
    , rsResultSetMetadata
    , rsRows

    -- * ErrorMessage
    , ErrorMessage (..)

    -- * AmazonResourceName
    , AmazonResourceName (..)

    -- * DataCatalogType
    , DataCatalogType (..)

    -- * ResultSetMetadata
    , ResultSetMetadata (..)
    , mkResultSetMetadata
    , rsmColumnInfo

    -- * Column
    , Column (..)
    , mkColumn
    , cName
    , cComment
    , cType

    -- * WorkGroup
    , WorkGroup (..)
    , mkWorkGroup
    , wgName
    , wgConfiguration
    , wgCreationTime
    , wgDescription
    , wgState

    -- * WorkGroupName
    , WorkGroupName (..)

    -- * NamedQuery
    , NamedQuery (..)
    , mkNamedQuery
    , nqName
    , nqDatabase
    , nqQueryString
    , nqDescription
    , nqNamedQueryId
    , nqWorkGroup

    -- * Datum
    , Datum (..)
    , mkDatum
    , dVarCharValue

    -- * Name
    , Name (..)

    -- * Description
    , Description (..)

    -- * Catalog
    , Catalog (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * Query
    , Query (..)

    -- * NextToken
    , NextToken (..)

    -- * CatalogName
    , CatalogName (..)

    -- * DatabaseName
    , DatabaseName (..)

    -- * TableName
    , TableName (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * Expression
    , Expression (..)

    -- * TableType
    , TableType (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
  
import Network.AWS.Athena.Types.NamedQueryId
  
import Network.AWS.Athena.Types.EncryptionOption
  
import Network.AWS.Athena.Types.IdempotencyToken
  
import Network.AWS.Athena.Types.QueryExecutionStatus
  
import Network.AWS.Athena.Types.UnprocessedQueryExecutionId
  
  
import Network.AWS.Athena.Types.QueryExecutionState
  
import Network.AWS.Athena.Types.QueryExecutionContext
  
  
import Network.AWS.Athena.Types.Tag
  
import Network.AWS.Athena.Types.WorkGroupSummary
  
import Network.AWS.Athena.Types.Database
  
import Network.AWS.Athena.Types.WorkGroupDescriptionString
  
import Network.AWS.Athena.Types.ParametersMapValue
  
import Network.AWS.Athena.Types.DatabaseString
  
import Network.AWS.Athena.Types.QueryExecution
  
import Network.AWS.Athena.Types.ColumnInfo
  
import Network.AWS.Athena.Types.Token
  
import Network.AWS.Athena.Types.DataCatalog
  
import Network.AWS.Athena.Types.ResultConfigurationUpdates
  
import Network.AWS.Athena.Types.DescriptionString
  
import Network.AWS.Athena.Types.WorkGroupConfiguration
  
import Network.AWS.Athena.Types.CommentString
  
import Network.AWS.Athena.Types.WorkGroupState
  
import Network.AWS.Athena.Types.ResultConfiguration
  
import Network.AWS.Athena.Types.DatumString
  
import Network.AWS.Athena.Types.DataCatalogSummary
  
import Network.AWS.Athena.Types.ColumnNullable
  
import Network.AWS.Athena.Types.QueryString
  
import Network.AWS.Athena.Types.EncryptionConfiguration
  
import Network.AWS.Athena.Types.NameString
  
  
import Network.AWS.Athena.Types.KeyString
  
import Network.AWS.Athena.Types.Row
  
import Network.AWS.Athena.Types.ErrorCode
  
import Network.AWS.Athena.Types.StatementType
  
import Network.AWS.Athena.Types.QueryExecutionStatistics
  
import Network.AWS.Athena.Types.UnprocessedNamedQueryId
  
import Network.AWS.Athena.Types.TypeString
  
  
import Network.AWS.Athena.Types.QueryExecutionId
  
import Network.AWS.Athena.Types.TagKey
  
import Network.AWS.Athena.Types.TableMetadata
  
import Network.AWS.Athena.Types.ResultSet
  
import Network.AWS.Athena.Types.ErrorMessage
  
import Network.AWS.Athena.Types.AmazonResourceName
  
import Network.AWS.Athena.Types.DataCatalogType
  
import Network.AWS.Athena.Types.ResultSetMetadata
  
import Network.AWS.Athena.Types.Column
  
import Network.AWS.Athena.Types.WorkGroup
  
  
import Network.AWS.Athena.Types.WorkGroupName
  
import Network.AWS.Athena.Types.NamedQuery
  
import Network.AWS.Athena.Types.Datum
  
import Network.AWS.Athena.Types.Name
  
import Network.AWS.Athena.Types.Description
  
import Network.AWS.Athena.Types.Catalog
  
import Network.AWS.Athena.Types.Key
  
import Network.AWS.Athena.Types.Value
  
import Network.AWS.Athena.Types.Query
  
import Network.AWS.Athena.Types.NextToken
  
import Network.AWS.Athena.Types.CatalogName
  
import Network.AWS.Athena.Types.DatabaseName
  
import Network.AWS.Athena.Types.TableName
  
import Network.AWS.Athena.Types.ResourceARN
  
import Network.AWS.Athena.Types.Expression
  
import Network.AWS.Athena.Types.TableType
  

-- | API version @2017-05-18@ of the Amazon Athena SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "Athena",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "athena",
                 Core._svcVersion = "2017-05-18", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "Athena",
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

-- | Indicates that something is wrong with the input to the request. For example, a required parameter may be missing or out of range.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException
  = Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# INLINEABLE _InvalidRequestException #-}
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead"  #-}

-- | An exception that Athena received when it called a custom metastore. Occurs if the error is not caused by user input (@InvalidRequestException@ ) or from the Athena platform (@InternalServerException@ ). For example, if a user-created Lambda function is missing permissions, the Lambda @4XX@ exception is returned in a @MetadataException@ .
_MetadataException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MetadataException
  = Core._MatchServiceError mkServiceConfig "MetadataException"
{-# INLINEABLE _MetadataException #-}
{-# DEPRECATED _MetadataException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the request was throttled.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException
  = Core._MatchServiceError mkServiceConfig
      "TooManyRequestsException"
{-# INLINEABLE _TooManyRequestsException #-}
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates a platform issue, which may be due to a transient condition or outage.
_InternalServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerException
  = Core._MatchServiceError mkServiceConfig "InternalServerException"
{-# INLINEABLE _InternalServerException #-}
{-# DEPRECATED _InternalServerException "Use generic-lens or generic-optics instead"  #-}

-- | A resource, such as a workgroup, was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}
