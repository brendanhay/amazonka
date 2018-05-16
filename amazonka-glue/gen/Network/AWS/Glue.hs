{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Glue__
--
-- Defines the public endpoint for the AWS Glue service.
--
module Network.AWS.Glue
    (
    -- * Service Configuration
      glue

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** CrawlerRunningException
    , _CrawlerRunningException

    -- ** SchedulerTransitioningException
    , _SchedulerTransitioningException

    -- ** SchedulerRunningException
    , _SchedulerRunningException

    -- ** ConcurrentRunsExceededException
    , _ConcurrentRunsExceededException

    -- ** NoScheduleException
    , _NoScheduleException

    -- ** OperationTimeoutException
    , _OperationTimeoutException

    -- ** CrawlerNotRunningException
    , _CrawlerNotRunningException

    -- ** VersionMismatchException
    , _VersionMismatchException

    -- ** EntityNotFoundException
    , _EntityNotFoundException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** SchedulerNotRunningException
    , _SchedulerNotRunningException

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** ResourceNumberLimitExceededException
    , _ResourceNumberLimitExceededException

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

    -- ** CrawlerStoppingException
    , _CrawlerStoppingException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StartCrawler
    , module Network.AWS.Glue.StartCrawler

    -- ** GetCatalogImportStatus
    , module Network.AWS.Glue.GetCatalogImportStatus

    -- ** GetPartition
    , module Network.AWS.Glue.GetPartition

    -- ** CreateTrigger
    , module Network.AWS.Glue.CreateTrigger

    -- ** DeleteTable
    , module Network.AWS.Glue.DeleteTable

    -- ** UpdateTable
    , module Network.AWS.Glue.UpdateTable

    -- ** DeleteConnection
    , module Network.AWS.Glue.DeleteConnection

    -- ** UpdateConnection
    , module Network.AWS.Glue.UpdateConnection

    -- ** GetUserDefinedFunctions (Paginated)
    , module Network.AWS.Glue.GetUserDefinedFunctions

    -- ** BatchCreatePartition
    , module Network.AWS.Glue.BatchCreatePartition

    -- ** GetMapping
    , module Network.AWS.Glue.GetMapping

    -- ** GetTableVersion
    , module Network.AWS.Glue.GetTableVersion

    -- ** GetJobs (Paginated)
    , module Network.AWS.Glue.GetJobs

    -- ** GetClassifiers (Paginated)
    , module Network.AWS.Glue.GetClassifiers

    -- ** CreateConnection
    , module Network.AWS.Glue.CreateConnection

    -- ** DeleteTableVersion
    , module Network.AWS.Glue.DeleteTableVersion

    -- ** DeleteDevEndpoint
    , module Network.AWS.Glue.DeleteDevEndpoint

    -- ** UpdateDevEndpoint
    , module Network.AWS.Glue.UpdateDevEndpoint

    -- ** DeleteCrawler
    , module Network.AWS.Glue.DeleteCrawler

    -- ** UpdateCrawler
    , module Network.AWS.Glue.UpdateCrawler

    -- ** GetConnection
    , module Network.AWS.Glue.GetConnection

    -- ** BatchGetPartition
    , module Network.AWS.Glue.BatchGetPartition

    -- ** StopTrigger
    , module Network.AWS.Glue.StopTrigger

    -- ** UpdateCrawlerSchedule
    , module Network.AWS.Glue.UpdateCrawlerSchedule

    -- ** DeleteUserDefinedFunction
    , module Network.AWS.Glue.DeleteUserDefinedFunction

    -- ** UpdateUserDefinedFunction
    , module Network.AWS.Glue.UpdateUserDefinedFunction

    -- ** BatchDeleteTable
    , module Network.AWS.Glue.BatchDeleteTable

    -- ** GetTables (Paginated)
    , module Network.AWS.Glue.GetTables

    -- ** CreateClassifier
    , module Network.AWS.Glue.CreateClassifier

    -- ** BatchDeleteConnection
    , module Network.AWS.Glue.BatchDeleteConnection

    -- ** CreateJob
    , module Network.AWS.Glue.CreateJob

    -- ** GetJobRuns (Paginated)
    , module Network.AWS.Glue.GetJobRuns

    -- ** CreateUserDefinedFunction
    , module Network.AWS.Glue.CreateUserDefinedFunction

    -- ** ResetJobBookmark
    , module Network.AWS.Glue.ResetJobBookmark

    -- ** DeleteJob
    , module Network.AWS.Glue.DeleteJob

    -- ** UpdateJob
    , module Network.AWS.Glue.UpdateJob

    -- ** GetCrawlers (Paginated)
    , module Network.AWS.Glue.GetCrawlers

    -- ** GetClassifier
    , module Network.AWS.Glue.GetClassifier

    -- ** GetJob
    , module Network.AWS.Glue.GetJob

    -- ** BatchDeleteTableVersion
    , module Network.AWS.Glue.BatchDeleteTableVersion

    -- ** GetDevEndpoints (Paginated)
    , module Network.AWS.Glue.GetDevEndpoints

    -- ** StartCrawlerSchedule
    , module Network.AWS.Glue.StartCrawlerSchedule

    -- ** GetUserDefinedFunction
    , module Network.AWS.Glue.GetUserDefinedFunction

    -- ** DeleteDatabase
    , module Network.AWS.Glue.DeleteDatabase

    -- ** UpdateDatabase
    , module Network.AWS.Glue.UpdateDatabase

    -- ** StopCrawler
    , module Network.AWS.Glue.StopCrawler

    -- ** GetPartitions (Paginated)
    , module Network.AWS.Glue.GetPartitions

    -- ** BatchDeletePartition
    , module Network.AWS.Glue.BatchDeletePartition

    -- ** GetCrawler
    , module Network.AWS.Glue.GetCrawler

    -- ** BatchStopJobRun
    , module Network.AWS.Glue.BatchStopJobRun

    -- ** GetDevEndpoint
    , module Network.AWS.Glue.GetDevEndpoint

    -- ** CreateTable
    , module Network.AWS.Glue.CreateTable

    -- ** GetCrawlerMetrics (Paginated)
    , module Network.AWS.Glue.GetCrawlerMetrics

    -- ** GetPlan
    , module Network.AWS.Glue.GetPlan

    -- ** GetTriggers (Paginated)
    , module Network.AWS.Glue.GetTriggers

    -- ** StartTrigger
    , module Network.AWS.Glue.StartTrigger

    -- ** GetDataflowGraph
    , module Network.AWS.Glue.GetDataflowGraph

    -- ** GetDatabases (Paginated)
    , module Network.AWS.Glue.GetDatabases

    -- ** GetTable
    , module Network.AWS.Glue.GetTable

    -- ** CreateCrawler
    , module Network.AWS.Glue.CreateCrawler

    -- ** GetJobRun
    , module Network.AWS.Glue.GetJobRun

    -- ** CreateDevEndpoint
    , module Network.AWS.Glue.CreateDevEndpoint

    -- ** GetDatabase
    , module Network.AWS.Glue.GetDatabase

    -- ** DeletePartition
    , module Network.AWS.Glue.DeletePartition

    -- ** UpdatePartition
    , module Network.AWS.Glue.UpdatePartition

    -- ** CreateScript
    , module Network.AWS.Glue.CreateScript

    -- ** GetConnections (Paginated)
    , module Network.AWS.Glue.GetConnections

    -- ** GetTrigger
    , module Network.AWS.Glue.GetTrigger

    -- ** ImportCatalogToGlue
    , module Network.AWS.Glue.ImportCatalogToGlue

    -- ** DeleteClassifier
    , module Network.AWS.Glue.DeleteClassifier

    -- ** UpdateClassifier
    , module Network.AWS.Glue.UpdateClassifier

    -- ** StartJobRun
    , module Network.AWS.Glue.StartJobRun

    -- ** CreatePartition
    , module Network.AWS.Glue.CreatePartition

    -- ** StopCrawlerSchedule
    , module Network.AWS.Glue.StopCrawlerSchedule

    -- ** CreateDatabase
    , module Network.AWS.Glue.CreateDatabase

    -- ** GetTableVersions (Paginated)
    , module Network.AWS.Glue.GetTableVersions

    -- ** DeleteTrigger
    , module Network.AWS.Glue.DeleteTrigger

    -- ** UpdateTrigger
    , module Network.AWS.Glue.UpdateTrigger

    -- * Types

    -- ** ConnectionPropertyKey
    , ConnectionPropertyKey (..)

    -- ** ConnectionType
    , ConnectionType (..)

    -- ** CrawlerState
    , CrawlerState (..)

    -- ** DeleteBehavior
    , DeleteBehavior (..)

    -- ** JobRunState
    , JobRunState (..)

    -- ** Language
    , Language (..)

    -- ** LastCrawlStatus
    , LastCrawlStatus (..)

    -- ** Logical
    , Logical (..)

    -- ** LogicalOperator
    , LogicalOperator (..)

    -- ** PrincipalType
    , PrincipalType (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** ScheduleState
    , ScheduleState (..)

    -- ** TriggerState
    , TriggerState (..)

    -- ** TriggerType
    , TriggerType (..)

    -- ** UpdateBehavior
    , UpdateBehavior (..)

    -- ** Action
    , Action
    , action
    , aArguments
    , aJobName
    , aTimeout

    -- ** BatchStopJobRunError
    , BatchStopJobRunError
    , batchStopJobRunError
    , bsjreJobName
    , bsjreJobRunId
    , bsjreErrorDetail

    -- ** BatchStopJobRunSuccessfulSubmission
    , BatchStopJobRunSuccessfulSubmission
    , batchStopJobRunSuccessfulSubmission
    , bsjrssJobName
    , bsjrssJobRunId

    -- ** CatalogEntry
    , CatalogEntry
    , catalogEntry
    , ceDatabaseName
    , ceTableName

    -- ** CatalogImportStatus
    , CatalogImportStatus
    , catalogImportStatus
    , cisImportedBy
    , cisImportTime
    , cisImportCompleted

    -- ** Classifier
    , Classifier
    , classifier
    , cGrokClassifier
    , cXMLClassifier
    , cJSONClassifier

    -- ** CodeGenEdge
    , CodeGenEdge
    , codeGenEdge
    , cgeTargetParameter
    , cgeSource
    , cgeTarget

    -- ** CodeGenNode
    , CodeGenNode
    , codeGenNode
    , cgnLineNumber
    , cgnId
    , cgnNodeType
    , cgnArgs

    -- ** CodeGenNodeArg
    , CodeGenNodeArg
    , codeGenNodeArg
    , cgnaParam
    , cgnaName
    , cgnaValue

    -- ** Column
    , Column
    , column
    , cType
    , cComment
    , cName

    -- ** Condition
    , Condition
    , condition
    , cState
    , cJobName
    , cLogicalOperator

    -- ** Connection
    , Connection
    , connection
    , conCreationTime
    , conLastUpdatedBy
    , conConnectionProperties
    , conLastUpdatedTime
    , conMatchCriteria
    , conPhysicalConnectionRequirements
    , conName
    , conDescription
    , conConnectionType

    -- ** ConnectionInput
    , ConnectionInput
    , connectionInput
    , ciMatchCriteria
    , ciPhysicalConnectionRequirements
    , ciDescription
    , ciName
    , ciConnectionType
    , ciConnectionProperties

    -- ** ConnectionsList
    , ConnectionsList
    , connectionsList
    , clConnections

    -- ** Crawler
    , Crawler
    , crawler
    , craCreationTime
    , craState
    , craSchemaChangePolicy
    , craLastUpdated
    , craSchedule
    , craLastCrawl
    , craCrawlElapsedTime
    , craClassifiers
    , craRole
    , craName
    , craTargets
    , craVersion
    , craDatabaseName
    , craConfiguration
    , craTablePrefix
    , craDescription

    -- ** CrawlerMetrics
    , CrawlerMetrics
    , crawlerMetrics
    , cmLastRuntimeSeconds
    , cmTablesCreated
    , cmStillEstimating
    , cmMedianRuntimeSeconds
    , cmTimeLeftSeconds
    , cmTablesDeleted
    , cmTablesUpdated
    , cmCrawlerName

    -- ** CrawlerTargets
    , CrawlerTargets
    , crawlerTargets
    , ctS3Targets
    , ctJdbcTargets

    -- ** CreateGrokClassifierRequest
    , CreateGrokClassifierRequest
    , createGrokClassifierRequest
    , cgcrCustomPatterns
    , cgcrClassification
    , cgcrName
    , cgcrGrokPattern

    -- ** CreateJSONClassifierRequest
    , CreateJSONClassifierRequest
    , createJSONClassifierRequest
    , cjcrName
    , cjcrJSONPath

    -- ** CreateXMLClassifierRequest
    , CreateXMLClassifierRequest
    , createXMLClassifierRequest
    , cxcrRowTag
    , cxcrClassification
    , cxcrName

    -- ** Database
    , Database
    , database
    , dLocationURI
    , dParameters
    , dDescription
    , dCreateTime
    , dName

    -- ** DatabaseInput
    , DatabaseInput
    , databaseInput
    , diLocationURI
    , diParameters
    , diDescription
    , diName

    -- ** DevEndpoint
    , DevEndpoint
    , devEndpoint
    , deStatus
    , deFailureReason
    , deEndpointName
    , deExtraPythonLibsS3Path
    , deLastUpdateStatus
    , deSecurityGroupIds
    , deLastModifiedTimestamp
    , deVPCId
    , dePrivateAddress
    , dePublicKey
    , deSubnetId
    , deNumberOfNodes
    , dePublicAddress
    , deAvailabilityZone
    , deZeppelinRemoteSparkInterpreterPort
    , deExtraJARsS3Path
    , deCreatedTimestamp
    , deYarnEndpointAddress
    , deRoleARN

    -- ** DevEndpointCustomLibraries
    , DevEndpointCustomLibraries
    , devEndpointCustomLibraries
    , declExtraPythonLibsS3Path
    , declExtraJARsS3Path

    -- ** ErrorDetail
    , ErrorDetail
    , errorDetail
    , edErrorCode
    , edErrorMessage

    -- ** ExecutionProperty
    , ExecutionProperty
    , executionProperty
    , epMaxConcurrentRuns

    -- ** GetConnectionsFilter
    , GetConnectionsFilter
    , getConnectionsFilter
    , gcfMatchCriteria
    , gcfConnectionType

    -- ** GrokClassifier
    , GrokClassifier
    , grokClassifier
    , gcCreationTime
    , gcLastUpdated
    , gcVersion
    , gcCustomPatterns
    , gcName
    , gcClassification
    , gcGrokPattern

    -- ** JSONClassifier
    , JSONClassifier
    , jsonClassifier
    , jcCreationTime
    , jcLastUpdated
    , jcVersion
    , jcName
    , jcJSONPath

    -- ** JdbcTarget
    , JdbcTarget
    , jdbcTarget
    , jtPath
    , jtConnectionName
    , jtExclusions

    -- ** Job
    , Job
    , job
    , jCommand
    , jLastModifiedOn
    , jConnections
    , jRole
    , jName
    , jLogURI
    , jMaxRetries
    , jExecutionProperty
    , jAllocatedCapacity
    , jTimeout
    , jDefaultArguments
    , jDescription
    , jCreatedOn

    -- ** JobBookmarkEntry
    , JobBookmarkEntry
    , jobBookmarkEntry
    , jbeJobName
    , jbeRun
    , jbeVersion
    , jbeAttempt
    , jbeJobBookmark

    -- ** JobCommand
    , JobCommand
    , jobCommand
    , jobScriptLocation
    , jobName

    -- ** JobRun
    , JobRun
    , jobRun
    , jrCompletedOn
    , jrTriggerName
    , jrLastModifiedOn
    , jrArguments
    , jrJobName
    , jrStartedOn
    , jrJobRunState
    , jrExecutionTime
    , jrPredecessorRuns
    , jrPreviousRunId
    , jrId
    , jrAttempt
    , jrAllocatedCapacity
    , jrTimeout
    , jrErrorMessage

    -- ** JobUpdate
    , JobUpdate
    , jobUpdate
    , juCommand
    , juConnections
    , juRole
    , juLogURI
    , juMaxRetries
    , juExecutionProperty
    , juAllocatedCapacity
    , juTimeout
    , juDefaultArguments
    , juDescription

    -- ** LastCrawlInfo
    , LastCrawlInfo
    , lastCrawlInfo
    , lciStatus
    , lciStartTime
    , lciLogStream
    , lciLogGroup
    , lciMessagePrefix
    , lciErrorMessage

    -- ** Location
    , Location
    , location
    , lJdbc
    , lS3

    -- ** MappingEntry
    , MappingEntry
    , mappingEntry
    , meTargetTable
    , meSourceType
    , meSourceTable
    , meTargetType
    , meTargetPath
    , meSourcePath

    -- ** Order
    , Order
    , order
    , oColumn
    , oSortOrder

    -- ** Partition
    , Partition
    , partition
    , pCreationTime
    , pValues
    , pLastAnalyzedTime
    , pStorageDescriptor
    , pDatabaseName
    , pParameters
    , pLastAccessTime
    , pTableName

    -- ** PartitionError
    , PartitionError
    , partitionError
    , pePartitionValues
    , peErrorDetail

    -- ** PartitionInput
    , PartitionInput
    , partitionInput
    , piValues
    , piLastAnalyzedTime
    , piStorageDescriptor
    , piParameters
    , piLastAccessTime

    -- ** PartitionValueList
    , PartitionValueList
    , partitionValueList
    , pvlValues

    -- ** PhysicalConnectionRequirements
    , PhysicalConnectionRequirements
    , physicalConnectionRequirements
    , pcrSecurityGroupIdList
    , pcrSubnetId
    , pcrAvailabilityZone

    -- ** Predecessor
    , Predecessor
    , predecessor
    , pJobName
    , pRunId

    -- ** Predicate
    , Predicate
    , predicate
    , pLogical
    , pConditions

    -- ** ResourceURI
    , ResourceURI
    , resourceURI
    , ruResourceType
    , ruURI

    -- ** S3Target
    , S3Target
    , s3Target
    , stPath
    , stExclusions

    -- ** Schedule
    , Schedule
    , schedule
    , sState
    , sScheduleExpression

    -- ** SchemaChangePolicy
    , SchemaChangePolicy
    , schemaChangePolicy
    , scpDeleteBehavior
    , scpUpdateBehavior

    -- ** Segment
    , Segment
    , segment
    , sSegmentNumber
    , sTotalSegments

    -- ** SerDeInfo
    , SerDeInfo
    , serDeInfo
    , sdiSerializationLibrary
    , sdiName
    , sdiParameters

    -- ** SkewedInfo
    , SkewedInfo
    , skewedInfo
    , siSkewedColumnValueLocationMaps
    , siSkewedColumnValues
    , siSkewedColumnNames

    -- ** StorageDescriptor
    , StorageDescriptor
    , storageDescriptor
    , sdSortColumns
    , sdCompressed
    , sdLocation
    , sdBucketColumns
    , sdSerdeInfo
    , sdOutputFormat
    , sdNumberOfBuckets
    , sdStoredAsSubDirectories
    , sdParameters
    , sdInputFormat
    , sdSkewedInfo
    , sdColumns

    -- ** Table
    , Table
    , table
    , tRetention
    , tCreatedBy
    , tTableType
    , tOwner
    , tViewOriginalText
    , tUpdateTime
    , tViewExpandedText
    , tLastAnalyzedTime
    , tStorageDescriptor
    , tDatabaseName
    , tParameters
    , tLastAccessTime
    , tDescription
    , tPartitionKeys
    , tCreateTime
    , tName

    -- ** TableError
    , TableError
    , tableError
    , teTableName
    , teErrorDetail

    -- ** TableInput
    , TableInput
    , tableInput
    , tiRetention
    , tiTableType
    , tiOwner
    , tiViewOriginalText
    , tiViewExpandedText
    , tiLastAnalyzedTime
    , tiStorageDescriptor
    , tiParameters
    , tiLastAccessTime
    , tiDescription
    , tiPartitionKeys
    , tiName

    -- ** TableVersion
    , TableVersion
    , tableVersion
    , tvVersionId
    , tvTable

    -- ** TableVersionError
    , TableVersionError
    , tableVersionError
    , tveVersionId
    , tveTableName
    , tveErrorDetail

    -- ** Trigger
    , Trigger
    , trigger
    , triState
    , triActions
    , triSchedule
    , triPredicate
    , triName
    , triId
    , triType
    , triDescription

    -- ** TriggerUpdate
    , TriggerUpdate
    , triggerUpdate
    , tuActions
    , tuSchedule
    , tuPredicate
    , tuName
    , tuDescription

    -- ** UpdateGrokClassifierRequest
    , UpdateGrokClassifierRequest
    , updateGrokClassifierRequest
    , ugcrClassification
    , ugcrCustomPatterns
    , ugcrGrokPattern
    , ugcrName

    -- ** UpdateJSONClassifierRequest
    , UpdateJSONClassifierRequest
    , updateJSONClassifierRequest
    , ujcrJSONPath
    , ujcrName

    -- ** UpdateXMLClassifierRequest
    , UpdateXMLClassifierRequest
    , updateXMLClassifierRequest
    , uxcrClassification
    , uxcrRowTag
    , uxcrName

    -- ** UserDefinedFunction
    , UserDefinedFunction
    , userDefinedFunction
    , udfOwnerName
    , udfResourceURIs
    , udfFunctionName
    , udfOwnerType
    , udfCreateTime
    , udfClassName

    -- ** UserDefinedFunctionInput
    , UserDefinedFunctionInput
    , userDefinedFunctionInput
    , udfiOwnerName
    , udfiResourceURIs
    , udfiFunctionName
    , udfiOwnerType
    , udfiClassName

    -- ** XMLClassifier
    , XMLClassifier
    , xmlClassifier
    , xcCreationTime
    , xcLastUpdated
    , xcVersion
    , xcRowTag
    , xcName
    , xcClassification
    ) where

import Network.AWS.Glue.BatchCreatePartition
import Network.AWS.Glue.BatchDeleteConnection
import Network.AWS.Glue.BatchDeletePartition
import Network.AWS.Glue.BatchDeleteTable
import Network.AWS.Glue.BatchDeleteTableVersion
import Network.AWS.Glue.BatchGetPartition
import Network.AWS.Glue.BatchStopJobRun
import Network.AWS.Glue.CreateClassifier
import Network.AWS.Glue.CreateConnection
import Network.AWS.Glue.CreateCrawler
import Network.AWS.Glue.CreateDatabase
import Network.AWS.Glue.CreateDevEndpoint
import Network.AWS.Glue.CreateJob
import Network.AWS.Glue.CreatePartition
import Network.AWS.Glue.CreateScript
import Network.AWS.Glue.CreateTable
import Network.AWS.Glue.CreateTrigger
import Network.AWS.Glue.CreateUserDefinedFunction
import Network.AWS.Glue.DeleteClassifier
import Network.AWS.Glue.DeleteConnection
import Network.AWS.Glue.DeleteCrawler
import Network.AWS.Glue.DeleteDatabase
import Network.AWS.Glue.DeleteDevEndpoint
import Network.AWS.Glue.DeleteJob
import Network.AWS.Glue.DeletePartition
import Network.AWS.Glue.DeleteTable
import Network.AWS.Glue.DeleteTableVersion
import Network.AWS.Glue.DeleteTrigger
import Network.AWS.Glue.DeleteUserDefinedFunction
import Network.AWS.Glue.GetCatalogImportStatus
import Network.AWS.Glue.GetClassifier
import Network.AWS.Glue.GetClassifiers
import Network.AWS.Glue.GetConnection
import Network.AWS.Glue.GetConnections
import Network.AWS.Glue.GetCrawler
import Network.AWS.Glue.GetCrawlerMetrics
import Network.AWS.Glue.GetCrawlers
import Network.AWS.Glue.GetDatabase
import Network.AWS.Glue.GetDatabases
import Network.AWS.Glue.GetDataflowGraph
import Network.AWS.Glue.GetDevEndpoint
import Network.AWS.Glue.GetDevEndpoints
import Network.AWS.Glue.GetJob
import Network.AWS.Glue.GetJobRun
import Network.AWS.Glue.GetJobRuns
import Network.AWS.Glue.GetJobs
import Network.AWS.Glue.GetMapping
import Network.AWS.Glue.GetPartition
import Network.AWS.Glue.GetPartitions
import Network.AWS.Glue.GetPlan
import Network.AWS.Glue.GetTable
import Network.AWS.Glue.GetTables
import Network.AWS.Glue.GetTableVersion
import Network.AWS.Glue.GetTableVersions
import Network.AWS.Glue.GetTrigger
import Network.AWS.Glue.GetTriggers
import Network.AWS.Glue.GetUserDefinedFunction
import Network.AWS.Glue.GetUserDefinedFunctions
import Network.AWS.Glue.ImportCatalogToGlue
import Network.AWS.Glue.ResetJobBookmark
import Network.AWS.Glue.StartCrawler
import Network.AWS.Glue.StartCrawlerSchedule
import Network.AWS.Glue.StartJobRun
import Network.AWS.Glue.StartTrigger
import Network.AWS.Glue.StopCrawler
import Network.AWS.Glue.StopCrawlerSchedule
import Network.AWS.Glue.StopTrigger
import Network.AWS.Glue.Types
import Network.AWS.Glue.UpdateClassifier
import Network.AWS.Glue.UpdateConnection
import Network.AWS.Glue.UpdateCrawler
import Network.AWS.Glue.UpdateCrawlerSchedule
import Network.AWS.Glue.UpdateDatabase
import Network.AWS.Glue.UpdateDevEndpoint
import Network.AWS.Glue.UpdateJob
import Network.AWS.Glue.UpdatePartition
import Network.AWS.Glue.UpdateTable
import Network.AWS.Glue.UpdateTrigger
import Network.AWS.Glue.UpdateUserDefinedFunction
import Network.AWS.Glue.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Glue'.
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
