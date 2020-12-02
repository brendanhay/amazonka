{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS IoT Analytics provides advanced data analysis for AWS IoT. It allows you to collect large amounts of device data, process messages, store them, and then query the data and run sophisticated analytics to make accurate decisions in your IoT applications and machine learning use cases. AWS IoT Analytics enables advanced data exploration through integration with Jupyter Notebooks and data visualization through integration with Amazon QuickSight.
--
--
-- Traditional analytics and business intelligence tools are designed to process structured data. IoT data often comes from devices that record noisy processes (such as temperature, motion, or sound). As a result, the data from these devices can have significant gaps, corrupted messages, and false readings that must be cleaned up before analysis can occur. Also, IoT data is often only meaningful in the context of other data from external sources.
--
-- AWS IoT Analytics automates each of the steps required to analyze data from IoT devices. AWS IoT Analytics filters, transforms, and enriches IoT data before storing it in a time-series data store for analysis. You can set up the service to collect only the data you need from your devices, apply mathematical transforms to process the data, and enrich the data with device-specific metadata such as device type and location before storing it. Then, you can analyze your data by running queries using the built-in SQL query engine, or perform more complex analytics and machine learning inference. AWS IoT Analytics includes models for common IoT use cases so you can answer questions like which devices are about to fail or which customers are at risk of abandoning their wearable devices.
--
module Network.AWS.IoTAnalytics
    (
    -- * Service Configuration
      ioTAnalytics

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribePipeline
    , module Network.AWS.IoTAnalytics.DescribePipeline

    -- ** DescribeDataset
    , module Network.AWS.IoTAnalytics.DescribeDataset

    -- ** ListChannels
    , module Network.AWS.IoTAnalytics.ListChannels

    -- ** PutLoggingOptions
    , module Network.AWS.IoTAnalytics.PutLoggingOptions

    -- ** DeleteChannel
    , module Network.AWS.IoTAnalytics.DeleteChannel

    -- ** UpdateChannel
    , module Network.AWS.IoTAnalytics.UpdateChannel

    -- ** SampleChannelData
    , module Network.AWS.IoTAnalytics.SampleChannelData

    -- ** CancelPipelineReprocessing
    , module Network.AWS.IoTAnalytics.CancelPipelineReprocessing

    -- ** CreateDatastore
    , module Network.AWS.IoTAnalytics.CreateDatastore

    -- ** UpdatePipeline
    , module Network.AWS.IoTAnalytics.UpdatePipeline

    -- ** DeletePipeline
    , module Network.AWS.IoTAnalytics.DeletePipeline

    -- ** DeleteDataset
    , module Network.AWS.IoTAnalytics.DeleteDataset

    -- ** UpdateDataset
    , module Network.AWS.IoTAnalytics.UpdateDataset

    -- ** ListPipelines
    , module Network.AWS.IoTAnalytics.ListPipelines

    -- ** DeleteDatastore
    , module Network.AWS.IoTAnalytics.DeleteDatastore

    -- ** UpdateDatastore
    , module Network.AWS.IoTAnalytics.UpdateDatastore

    -- ** CreateDataset
    , module Network.AWS.IoTAnalytics.CreateDataset

    -- ** BatchPutMessage
    , module Network.AWS.IoTAnalytics.BatchPutMessage

    -- ** ListDatastores
    , module Network.AWS.IoTAnalytics.ListDatastores

    -- ** CreateDatasetContent
    , module Network.AWS.IoTAnalytics.CreateDatasetContent

    -- ** CreateChannel
    , module Network.AWS.IoTAnalytics.CreateChannel

    -- ** DeleteDatasetContent
    , module Network.AWS.IoTAnalytics.DeleteDatasetContent

    -- ** DescribeDatastore
    , module Network.AWS.IoTAnalytics.DescribeDatastore

    -- ** GetDatasetContent
    , module Network.AWS.IoTAnalytics.GetDatasetContent

    -- ** ListDatasets
    , module Network.AWS.IoTAnalytics.ListDatasets

    -- ** RunPipelineActivity
    , module Network.AWS.IoTAnalytics.RunPipelineActivity

    -- ** DescribeChannel
    , module Network.AWS.IoTAnalytics.DescribeChannel

    -- ** CreatePipeline
    , module Network.AWS.IoTAnalytics.CreatePipeline

    -- ** StartPipelineReprocessing
    , module Network.AWS.IoTAnalytics.StartPipelineReprocessing

    -- ** DescribeLoggingOptions
    , module Network.AWS.IoTAnalytics.DescribeLoggingOptions

    -- * Types

    -- ** ChannelStatus
    , ChannelStatus (..)

    -- ** DatasetContentState
    , DatasetContentState (..)

    -- ** DatasetStatus
    , DatasetStatus (..)

    -- ** DatastoreStatus
    , DatastoreStatus (..)

    -- ** LoggingLevel
    , LoggingLevel (..)

    -- ** ReprocessingStatus
    , ReprocessingStatus (..)

    -- ** AddAttributesActivity
    , AddAttributesActivity
    , addAttributesActivity
    , aaaNext
    , aaaName
    , aaaAttributes

    -- ** BatchPutMessageErrorEntry
    , BatchPutMessageErrorEntry
    , batchPutMessageErrorEntry
    , bpmeeErrorCode
    , bpmeeErrorMessage
    , bpmeeMessageId

    -- ** Channel
    , Channel
    , channel
    , cCreationTime
    , cStatus
    , cArn
    , cRetentionPeriod
    , cName
    , cLastUpdateTime

    -- ** ChannelActivity
    , ChannelActivity
    , channelActivity
    , caNext
    , caName
    , caChannelName

    -- ** ChannelSummary
    , ChannelSummary
    , channelSummary
    , csCreationTime
    , csStatus
    , csChannelName
    , csLastUpdateTime

    -- ** Dataset
    , Dataset
    , dataset
    , dCreationTime
    , dStatus
    , dArn
    , dActions
    , dTriggers
    , dName
    , dLastUpdateTime

    -- ** DatasetAction
    , DatasetAction
    , datasetAction
    , daQueryAction
    , daActionName

    -- ** DatasetContentStatus
    , DatasetContentStatus
    , datasetContentStatus
    , dcsState
    , dcsReason

    -- ** DatasetEntry
    , DatasetEntry
    , datasetEntry
    , deEntryName
    , deDataURI

    -- ** DatasetSummary
    , DatasetSummary
    , datasetSummary
    , dssCreationTime
    , dssStatus
    , dssDatasetName
    , dssLastUpdateTime

    -- ** DatasetTrigger
    , DatasetTrigger
    , datasetTrigger
    , dtSchedule

    -- ** Datastore
    , Datastore
    , datastore
    , datCreationTime
    , datStatus
    , datArn
    , datRetentionPeriod
    , datName
    , datLastUpdateTime

    -- ** DatastoreActivity
    , DatastoreActivity
    , datastoreActivity
    , daName
    , daDatastoreName

    -- ** DatastoreSummary
    , DatastoreSummary
    , datastoreSummary
    , dsCreationTime
    , dsStatus
    , dsDatastoreName
    , dsLastUpdateTime

    -- ** DeviceRegistryEnrichActivity
    , DeviceRegistryEnrichActivity
    , deviceRegistryEnrichActivity
    , dreaNext
    , dreaName
    , dreaAttribute
    , dreaThingName
    , dreaRoleARN

    -- ** DeviceShadowEnrichActivity
    , DeviceShadowEnrichActivity
    , deviceShadowEnrichActivity
    , dseaNext
    , dseaName
    , dseaAttribute
    , dseaThingName
    , dseaRoleARN

    -- ** FilterActivity
    , FilterActivity
    , filterActivity
    , faNext
    , faName
    , faFilter

    -- ** LambdaActivity
    , LambdaActivity
    , lambdaActivity
    , laNext
    , laName
    , laLambdaName
    , laBatchSize

    -- ** LoggingOptions
    , LoggingOptions
    , loggingOptions
    , loRoleARN
    , loLevel
    , loEnabled

    -- ** MathActivity
    , MathActivity
    , mathActivity
    , maNext
    , maName
    , maAttribute
    , maMath

    -- ** Message
    , Message
    , message
    , mMessageId
    , mPayload

    -- ** Pipeline
    , Pipeline
    , pipeline
    , pCreationTime
    , pArn
    , pActivities
    , pName
    , pReprocessingSummaries
    , pLastUpdateTime

    -- ** PipelineActivity
    , PipelineActivity
    , pipelineActivity
    , paSelectAttributes
    , paChannel
    , paAddAttributes
    , paDeviceRegistryEnrich
    , paRemoveAttributes
    , paLambda
    , paDatastore
    , paDeviceShadowEnrich
    , paFilter
    , paMath

    -- ** PipelineSummary
    , PipelineSummary
    , pipelineSummary
    , psCreationTime
    , psPipelineName
    , psReprocessingSummaries
    , psLastUpdateTime

    -- ** RemoveAttributesActivity
    , RemoveAttributesActivity
    , removeAttributesActivity
    , raaNext
    , raaName
    , raaAttributes

    -- ** ReprocessingSummary
    , ReprocessingSummary
    , reprocessingSummary
    , rsCreationTime
    , rsStatus
    , rsId

    -- ** RetentionPeriod
    , RetentionPeriod
    , retentionPeriod
    , rpUnlimited
    , rpNumberOfDays

    -- ** Schedule
    , Schedule
    , schedule
    , sExpression

    -- ** SelectAttributesActivity
    , SelectAttributesActivity
    , selectAttributesActivity
    , saaNext
    , saaName
    , saaAttributes

    -- ** SqlQueryDatasetAction
    , SqlQueryDatasetAction
    , sqlQueryDatasetAction
    , sqdaSqlQuery
    ) where

import Network.AWS.IoTAnalytics.BatchPutMessage
import Network.AWS.IoTAnalytics.CancelPipelineReprocessing
import Network.AWS.IoTAnalytics.CreateChannel
import Network.AWS.IoTAnalytics.CreateDataset
import Network.AWS.IoTAnalytics.CreateDatasetContent
import Network.AWS.IoTAnalytics.CreateDatastore
import Network.AWS.IoTAnalytics.CreatePipeline
import Network.AWS.IoTAnalytics.DeleteChannel
import Network.AWS.IoTAnalytics.DeleteDataset
import Network.AWS.IoTAnalytics.DeleteDatasetContent
import Network.AWS.IoTAnalytics.DeleteDatastore
import Network.AWS.IoTAnalytics.DeletePipeline
import Network.AWS.IoTAnalytics.DescribeChannel
import Network.AWS.IoTAnalytics.DescribeDataset
import Network.AWS.IoTAnalytics.DescribeDatastore
import Network.AWS.IoTAnalytics.DescribeLoggingOptions
import Network.AWS.IoTAnalytics.DescribePipeline
import Network.AWS.IoTAnalytics.GetDatasetContent
import Network.AWS.IoTAnalytics.ListChannels
import Network.AWS.IoTAnalytics.ListDatasets
import Network.AWS.IoTAnalytics.ListDatastores
import Network.AWS.IoTAnalytics.ListPipelines
import Network.AWS.IoTAnalytics.PutLoggingOptions
import Network.AWS.IoTAnalytics.RunPipelineActivity
import Network.AWS.IoTAnalytics.SampleChannelData
import Network.AWS.IoTAnalytics.StartPipelineReprocessing
import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.UpdateChannel
import Network.AWS.IoTAnalytics.UpdateDataset
import Network.AWS.IoTAnalytics.UpdateDatastore
import Network.AWS.IoTAnalytics.UpdatePipeline
import Network.AWS.IoTAnalytics.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'IoTAnalytics'.
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
