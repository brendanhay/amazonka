{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Kinesis Analytics is the easiest way to process streaming data in real time with standard SQL without having to learn new programming languages or processing frameworks. Amazon Kinesis Analytics enables you to create and run SQL queries on streaming data so that you can gain actionable insights and respond to your business and customer needs promptly.
--
--
-- Amazon Kinesis Analytics takes care of everything required to run your queries continuously and scales automatically to match the volume and throughput rate of your incoming data. With Amazon Kinesis Analytics, you only pay for the resources your queries consume. There is no minimum fee or setup cost.
module Network.AWS.KinesisAnalytics
    (
    -- * Service Configuration
      kinesisAnalytics

    -- * Errors
    -- $errors

    -- ** InvalidApplicationConfigurationException
    , _InvalidApplicationConfigurationException

    -- ** ResourceProvisionedThroughputExceededException
    , _ResourceProvisionedThroughputExceededException

    -- ** InvalidArgumentException
    , _InvalidArgumentException

    -- ** CodeValidationException
    , _CodeValidationException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** UnableToDetectSchemaException
    , _UnableToDetectSchemaException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddApplicationOutput
    , module Network.AWS.KinesisAnalytics.AddApplicationOutput

    -- ** DiscoverInputSchema
    , module Network.AWS.KinesisAnalytics.DiscoverInputSchema

    -- ** DescribeApplication
    , module Network.AWS.KinesisAnalytics.DescribeApplication

    -- ** StartApplication
    , module Network.AWS.KinesisAnalytics.StartApplication

    -- ** DeleteApplicationReferenceDataSource
    , module Network.AWS.KinesisAnalytics.DeleteApplicationReferenceDataSource

    -- ** DeleteApplication
    , module Network.AWS.KinesisAnalytics.DeleteApplication

    -- ** UpdateApplication
    , module Network.AWS.KinesisAnalytics.UpdateApplication

    -- ** DeleteApplicationCloudWatchLoggingOption
    , module Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption

    -- ** AddApplicationInputProcessingConfiguration
    , module Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration

    -- ** CreateApplication
    , module Network.AWS.KinesisAnalytics.CreateApplication

    -- ** DeleteApplicationOutput
    , module Network.AWS.KinesisAnalytics.DeleteApplicationOutput

    -- ** StopApplication
    , module Network.AWS.KinesisAnalytics.StopApplication

    -- ** AddApplicationReferenceDataSource
    , module Network.AWS.KinesisAnalytics.AddApplicationReferenceDataSource

    -- ** AddApplicationInput
    , module Network.AWS.KinesisAnalytics.AddApplicationInput

    -- ** AddApplicationCloudWatchLoggingOption
    , module Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption

    -- ** ListApplications
    , module Network.AWS.KinesisAnalytics.ListApplications

    -- ** DeleteApplicationInputProcessingConfiguration
    , module Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration

    -- * Types

    -- ** ApplicationStatus
    , ApplicationStatus (..)

    -- ** InputStartingPosition
    , InputStartingPosition (..)

    -- ** RecordFormatType
    , RecordFormatType (..)

    -- ** ApplicationDetail
    , ApplicationDetail
    , applicationDetail
    , adApplicationDescription
    , adOutputDescriptions
    , adCloudWatchLoggingOptionDescriptions
    , adReferenceDataSourceDescriptions
    , adInputDescriptions
    , adApplicationCode
    , adCreateTimestamp
    , adLastUpdateTimestamp
    , adApplicationName
    , adApplicationARN
    , adApplicationStatus
    , adApplicationVersionId

    -- ** ApplicationSummary
    , ApplicationSummary
    , applicationSummary
    , asApplicationName
    , asApplicationARN
    , asApplicationStatus

    -- ** ApplicationUpdate
    , ApplicationUpdate
    , applicationUpdate
    , auReferenceDataSourceUpdates
    , auInputUpdates
    , auCloudWatchLoggingOptionUpdates
    , auOutputUpdates
    , auApplicationCodeUpdate

    -- ** CSVMappingParameters
    , CSVMappingParameters
    , csvMappingParameters
    , cmpRecordRowDelimiter
    , cmpRecordColumnDelimiter

    -- ** CloudWatchLoggingOption
    , CloudWatchLoggingOption
    , cloudWatchLoggingOption
    , cwloLogStreamARN
    , cwloRoleARN

    -- ** CloudWatchLoggingOptionDescription
    , CloudWatchLoggingOptionDescription
    , cloudWatchLoggingOptionDescription
    , cwlodCloudWatchLoggingOptionId
    , cwlodLogStreamARN
    , cwlodRoleARN

    -- ** CloudWatchLoggingOptionUpdate
    , CloudWatchLoggingOptionUpdate
    , cloudWatchLoggingOptionUpdate
    , cwlouRoleARNUpdate
    , cwlouLogStreamARNUpdate
    , cwlouCloudWatchLoggingOptionId

    -- ** DestinationSchema
    , DestinationSchema
    , destinationSchema
    , dsRecordFormatType

    -- ** Input
    , Input
    , input
    , iInputParallelism
    , iInputProcessingConfiguration
    , iKinesisStreamsInput
    , iKinesisFirehoseInput
    , iNamePrefix
    , iInputSchema

    -- ** InputConfiguration
    , InputConfiguration
    , inputConfiguration
    , icId
    , icInputStartingPositionConfiguration

    -- ** InputDescription
    , InputDescription
    , inputDescription
    , idInputStartingPositionConfiguration
    , idInputParallelism
    , idInputId
    , idInAppStreamNames
    , idKinesisFirehoseInputDescription
    , idInputSchema
    , idKinesisStreamsInputDescription
    , idNamePrefix
    , idInputProcessingConfigurationDescription

    -- ** InputLambdaProcessor
    , InputLambdaProcessor
    , inputLambdaProcessor
    , ilpResourceARN
    , ilpRoleARN

    -- ** InputLambdaProcessorDescription
    , InputLambdaProcessorDescription
    , inputLambdaProcessorDescription
    , ilpdResourceARN
    , ilpdRoleARN

    -- ** InputLambdaProcessorUpdate
    , InputLambdaProcessorUpdate
    , inputLambdaProcessorUpdate
    , ilpuRoleARNUpdate
    , ilpuResourceARNUpdate

    -- ** InputParallelism
    , InputParallelism
    , inputParallelism
    , ipCount

    -- ** InputParallelismUpdate
    , InputParallelismUpdate
    , inputParallelismUpdate
    , ipuCountUpdate

    -- ** InputProcessingConfiguration
    , InputProcessingConfiguration
    , inputProcessingConfiguration
    , ipcInputLambdaProcessor

    -- ** InputProcessingConfigurationDescription
    , InputProcessingConfigurationDescription
    , inputProcessingConfigurationDescription
    , ipcdInputLambdaProcessorDescription

    -- ** InputProcessingConfigurationUpdate
    , InputProcessingConfigurationUpdate
    , inputProcessingConfigurationUpdate
    , ipcuInputLambdaProcessorUpdate

    -- ** InputSchemaUpdate
    , InputSchemaUpdate
    , inputSchemaUpdate
    , isuRecordFormatUpdate
    , isuRecordEncodingUpdate
    , isuRecordColumnUpdates

    -- ** InputStartingPositionConfiguration
    , InputStartingPositionConfiguration
    , inputStartingPositionConfiguration
    , ispcInputStartingPosition

    -- ** InputUpdate
    , InputUpdate
    , inputUpdate
    , iuInputProcessingConfigurationUpdate
    , iuKinesisStreamsInputUpdate
    , iuInputParallelismUpdate
    , iuNamePrefixUpdate
    , iuInputSchemaUpdate
    , iuKinesisFirehoseInputUpdate
    , iuInputId

    -- ** JSONMappingParameters
    , JSONMappingParameters
    , jsonMappingParameters
    , jmpRecordRowPath

    -- ** KinesisFirehoseInput
    , KinesisFirehoseInput
    , kinesisFirehoseInput
    , kfiResourceARN
    , kfiRoleARN

    -- ** KinesisFirehoseInputDescription
    , KinesisFirehoseInputDescription
    , kinesisFirehoseInputDescription
    , kfidResourceARN
    , kfidRoleARN

    -- ** KinesisFirehoseInputUpdate
    , KinesisFirehoseInputUpdate
    , kinesisFirehoseInputUpdate
    , kfiuRoleARNUpdate
    , kfiuResourceARNUpdate

    -- ** KinesisFirehoseOutput
    , KinesisFirehoseOutput
    , kinesisFirehoseOutput
    , kfoResourceARN
    , kfoRoleARN

    -- ** KinesisFirehoseOutputDescription
    , KinesisFirehoseOutputDescription
    , kinesisFirehoseOutputDescription
    , kfodResourceARN
    , kfodRoleARN

    -- ** KinesisFirehoseOutputUpdate
    , KinesisFirehoseOutputUpdate
    , kinesisFirehoseOutputUpdate
    , kfouRoleARNUpdate
    , kfouResourceARNUpdate

    -- ** KinesisStreamsInput
    , KinesisStreamsInput
    , kinesisStreamsInput
    , ksiResourceARN
    , ksiRoleARN

    -- ** KinesisStreamsInputDescription
    , KinesisStreamsInputDescription
    , kinesisStreamsInputDescription
    , ksidResourceARN
    , ksidRoleARN

    -- ** KinesisStreamsInputUpdate
    , KinesisStreamsInputUpdate
    , kinesisStreamsInputUpdate
    , ksiuRoleARNUpdate
    , ksiuResourceARNUpdate

    -- ** KinesisStreamsOutput
    , KinesisStreamsOutput
    , kinesisStreamsOutput
    , ksoResourceARN
    , ksoRoleARN

    -- ** KinesisStreamsOutputDescription
    , KinesisStreamsOutputDescription
    , kinesisStreamsOutputDescription
    , ksodResourceARN
    , ksodRoleARN

    -- ** KinesisStreamsOutputUpdate
    , KinesisStreamsOutputUpdate
    , kinesisStreamsOutputUpdate
    , ksouRoleARNUpdate
    , ksouResourceARNUpdate

    -- ** LambdaOutput
    , LambdaOutput
    , lambdaOutput
    , loResourceARN
    , loRoleARN

    -- ** LambdaOutputDescription
    , LambdaOutputDescription
    , lambdaOutputDescription
    , lodResourceARN
    , lodRoleARN

    -- ** LambdaOutputUpdate
    , LambdaOutputUpdate
    , lambdaOutputUpdate
    , louRoleARNUpdate
    , louResourceARNUpdate

    -- ** MappingParameters
    , MappingParameters
    , mappingParameters
    , mpCSVMappingParameters
    , mpJSONMappingParameters

    -- ** Output
    , Output
    , output
    , oLambdaOutput
    , oKinesisStreamsOutput
    , oKinesisFirehoseOutput
    , oName
    , oDestinationSchema

    -- ** OutputDescription
    , OutputDescription
    , outputDescription
    , odOutputId
    , odDestinationSchema
    , odKinesisFirehoseOutputDescription
    , odKinesisStreamsOutputDescription
    , odName
    , odLambdaOutputDescription

    -- ** OutputUpdate
    , OutputUpdate
    , outputUpdate
    , ouKinesisStreamsOutputUpdate
    , ouDestinationSchemaUpdate
    , ouKinesisFirehoseOutputUpdate
    , ouNameUpdate
    , ouLambdaOutputUpdate
    , ouOutputId

    -- ** RecordColumn
    , RecordColumn
    , recordColumn
    , rcMapping
    , rcName
    , rcSqlType

    -- ** RecordFormat
    , RecordFormat
    , recordFormat
    , rfMappingParameters
    , rfRecordFormatType

    -- ** ReferenceDataSource
    , ReferenceDataSource
    , referenceDataSource
    , rdsS3ReferenceDataSource
    , rdsTableName
    , rdsReferenceSchema

    -- ** ReferenceDataSourceDescription
    , ReferenceDataSourceDescription
    , referenceDataSourceDescription
    , rdsdReferenceSchema
    , rdsdReferenceId
    , rdsdTableName
    , rdsdS3ReferenceDataSourceDescription

    -- ** ReferenceDataSourceUpdate
    , ReferenceDataSourceUpdate
    , referenceDataSourceUpdate
    , rdsuTableNameUpdate
    , rdsuS3ReferenceDataSourceUpdate
    , rdsuReferenceSchemaUpdate
    , rdsuReferenceId

    -- ** S3Configuration
    , S3Configuration
    , s3Configuration
    , scRoleARN
    , scBucketARN
    , scFileKey

    -- ** S3ReferenceDataSource
    , S3ReferenceDataSource
    , s3ReferenceDataSource
    , srdsBucketARN
    , srdsFileKey
    , srdsReferenceRoleARN

    -- ** S3ReferenceDataSourceDescription
    , S3ReferenceDataSourceDescription
    , s3ReferenceDataSourceDescription
    , srdsdBucketARN
    , srdsdFileKey
    , srdsdReferenceRoleARN

    -- ** S3ReferenceDataSourceUpdate
    , S3ReferenceDataSourceUpdate
    , s3ReferenceDataSourceUpdate
    , srdsuBucketARNUpdate
    , srdsuFileKeyUpdate
    , srdsuReferenceRoleARNUpdate

    -- ** SourceSchema
    , SourceSchema
    , sourceSchema
    , ssRecordEncoding
    , ssRecordFormat
    , ssRecordColumns
    ) where

import Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
import Network.AWS.KinesisAnalytics.AddApplicationInput
import Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration
import Network.AWS.KinesisAnalytics.AddApplicationOutput
import Network.AWS.KinesisAnalytics.AddApplicationReferenceDataSource
import Network.AWS.KinesisAnalytics.CreateApplication
import Network.AWS.KinesisAnalytics.DeleteApplication
import Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
import Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
import Network.AWS.KinesisAnalytics.DeleteApplicationOutput
import Network.AWS.KinesisAnalytics.DeleteApplicationReferenceDataSource
import Network.AWS.KinesisAnalytics.DescribeApplication
import Network.AWS.KinesisAnalytics.DiscoverInputSchema
import Network.AWS.KinesisAnalytics.ListApplications
import Network.AWS.KinesisAnalytics.StartApplication
import Network.AWS.KinesisAnalytics.StopApplication
import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.UpdateApplication
import Network.AWS.KinesisAnalytics.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'KinesisAnalytics'.
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
