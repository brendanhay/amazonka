{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Definition of the public APIs exposed by SageMaker
module Network.AWS.SageMaker
    (
    -- * Service Configuration
      sageMaker

    -- * Errors
    -- $errors

    -- ** ResourceLimitExceeded
    , _ResourceLimitExceeded

    -- ** ResourceInUse
    , _ResourceInUse

    -- ** ResourceNotFound
    , _ResourceNotFound

    -- * Waiters
    -- $waiters

    -- ** NotebookInstanceDeleted
    , notebookInstanceDeleted

    -- ** EndpointDeleted
    , endpointDeleted

    -- ** EndpointInService
    , endpointInService

    -- ** NotebookInstanceInService
    , notebookInstanceInService

    -- ** TrainingJobCompletedOrStopped
    , trainingJobCompletedOrStopped

    -- ** NotebookInstanceStopped
    , notebookInstanceStopped

    -- * Operations
    -- $operations

    -- ** CreateNotebookInstance
    , module Network.AWS.SageMaker.CreateNotebookInstance

    -- ** DescribeEndpointConfig
    , module Network.AWS.SageMaker.DescribeEndpointConfig

    -- ** CreateEndpoint
    , module Network.AWS.SageMaker.CreateEndpoint

    -- ** DescribeTrainingJob
    , module Network.AWS.SageMaker.DescribeTrainingJob

    -- ** DeleteEndpoint
    , module Network.AWS.SageMaker.DeleteEndpoint

    -- ** UpdateEndpoint
    , module Network.AWS.SageMaker.UpdateEndpoint

    -- ** DeleteNotebookInstanceLifecycleConfig
    , module Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig

    -- ** UpdateNotebookInstanceLifecycleConfig
    , module Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig

    -- ** DescribeNotebookInstance
    , module Network.AWS.SageMaker.DescribeNotebookInstance

    -- ** CreateEndpointConfig
    , module Network.AWS.SageMaker.CreateEndpointConfig

    -- ** StopNotebookInstance
    , module Network.AWS.SageMaker.StopNotebookInstance

    -- ** UpdateEndpointWeightsAndCapacities
    , module Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities

    -- ** DeleteTags
    , module Network.AWS.SageMaker.DeleteTags

    -- ** DeleteEndpointConfig
    , module Network.AWS.SageMaker.DeleteEndpointConfig

    -- ** CreateModel
    , module Network.AWS.SageMaker.CreateModel

    -- ** DeleteModel
    , module Network.AWS.SageMaker.DeleteModel

    -- ** ListModels (Paginated)
    , module Network.AWS.SageMaker.ListModels

    -- ** DescribeNotebookInstanceLifecycleConfig
    , module Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig

    -- ** ListNotebookInstances (Paginated)
    , module Network.AWS.SageMaker.ListNotebookInstances

    -- ** DeleteNotebookInstance
    , module Network.AWS.SageMaker.DeleteNotebookInstance

    -- ** UpdateNotebookInstance
    , module Network.AWS.SageMaker.UpdateNotebookInstance

    -- ** StopTrainingJob
    , module Network.AWS.SageMaker.StopTrainingJob

    -- ** DescribeModel
    , module Network.AWS.SageMaker.DescribeModel

    -- ** ListEndpoints (Paginated)
    , module Network.AWS.SageMaker.ListEndpoints

    -- ** CreatePresignedNotebookInstanceURL
    , module Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL

    -- ** ListNotebookInstanceLifecycleConfigs
    , module Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs

    -- ** CreateNotebookInstanceLifecycleConfig
    , module Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig

    -- ** StartNotebookInstance
    , module Network.AWS.SageMaker.StartNotebookInstance

    -- ** AddTags
    , module Network.AWS.SageMaker.AddTags

    -- ** ListEndpointConfigs (Paginated)
    , module Network.AWS.SageMaker.ListEndpointConfigs

    -- ** ListTags (Paginated)
    , module Network.AWS.SageMaker.ListTags

    -- ** CreateTrainingJob
    , module Network.AWS.SageMaker.CreateTrainingJob

    -- ** DescribeEndpoint
    , module Network.AWS.SageMaker.DescribeEndpoint

    -- ** ListTrainingJobs (Paginated)
    , module Network.AWS.SageMaker.ListTrainingJobs

    -- * Types

    -- ** CompressionType
    , CompressionType (..)

    -- ** DirectInternetAccess
    , DirectInternetAccess (..)

    -- ** EndpointConfigSortKey
    , EndpointConfigSortKey (..)

    -- ** EndpointSortKey
    , EndpointSortKey (..)

    -- ** EndpointStatus
    , EndpointStatus (..)

    -- ** InstanceType
    , InstanceType (..)

    -- ** ModelSortKey
    , ModelSortKey (..)

    -- ** NotebookInstanceLifecycleConfigSortKey
    , NotebookInstanceLifecycleConfigSortKey (..)

    -- ** NotebookInstanceLifecycleConfigSortOrder
    , NotebookInstanceLifecycleConfigSortOrder (..)

    -- ** NotebookInstanceSortKey
    , NotebookInstanceSortKey (..)

    -- ** NotebookInstanceSortOrder
    , NotebookInstanceSortOrder (..)

    -- ** NotebookInstanceStatus
    , NotebookInstanceStatus (..)

    -- ** OrderKey
    , OrderKey (..)

    -- ** ProductionVariantInstanceType
    , ProductionVariantInstanceType (..)

    -- ** RecordWrapper
    , RecordWrapper (..)

    -- ** S3DataDistribution
    , S3DataDistribution (..)

    -- ** S3DataType
    , S3DataType (..)

    -- ** SecondaryStatus
    , SecondaryStatus (..)

    -- ** SortBy
    , SortBy (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** TrainingInputMode
    , TrainingInputMode (..)

    -- ** TrainingInstanceType
    , TrainingInstanceType (..)

    -- ** TrainingJobStatus
    , TrainingJobStatus (..)

    -- ** AlgorithmSpecification
    , AlgorithmSpecification
    , algorithmSpecification
    , asTrainingImage
    , asTrainingInputMode

    -- ** Channel
    , Channel
    , channel
    , cRecordWrapperType
    , cCompressionType
    , cContentType
    , cChannelName
    , cDataSource

    -- ** ContainerDefinition
    , ContainerDefinition
    , containerDefinition
    , cdModelDataURL
    , cdEnvironment
    , cdContainerHostname
    , cdImage

    -- ** DataSource
    , DataSource
    , dataSource
    , dsS3DataSource

    -- ** DesiredWeightAndCapacity
    , DesiredWeightAndCapacity
    , desiredWeightAndCapacity
    , dwacDesiredInstanceCount
    , dwacDesiredWeight
    , dwacVariantName

    -- ** EndpointConfigSummary
    , EndpointConfigSummary
    , endpointConfigSummary
    , ecsEndpointConfigName
    , ecsEndpointConfigARN
    , ecsCreationTime

    -- ** EndpointSummary
    , EndpointSummary
    , endpointSummary
    , esEndpointName
    , esEndpointARN
    , esCreationTime
    , esLastModifiedTime
    , esEndpointStatus

    -- ** ModelArtifacts
    , ModelArtifacts
    , modelArtifacts
    , maS3ModelArtifacts

    -- ** ModelSummary
    , ModelSummary
    , modelSummary
    , msModelName
    , msModelARN
    , msCreationTime

    -- ** NotebookInstanceLifecycleConfigSummary
    , NotebookInstanceLifecycleConfigSummary
    , notebookInstanceLifecycleConfigSummary
    , nilcsCreationTime
    , nilcsLastModifiedTime
    , nilcsNotebookInstanceLifecycleConfigName
    , nilcsNotebookInstanceLifecycleConfigARN

    -- ** NotebookInstanceLifecycleHook
    , NotebookInstanceLifecycleHook
    , notebookInstanceLifecycleHook
    , nilhContent

    -- ** NotebookInstanceSummary
    , NotebookInstanceSummary
    , notebookInstanceSummary
    , nisCreationTime
    , nisURL
    , nisLastModifiedTime
    , nisInstanceType
    , nisNotebookInstanceStatus
    , nisNotebookInstanceLifecycleConfigName
    , nisNotebookInstanceName
    , nisNotebookInstanceARN

    -- ** OutputDataConfig
    , OutputDataConfig
    , outputDataConfig
    , odcKMSKeyId
    , odcS3OutputPath

    -- ** ProductionVariant
    , ProductionVariant
    , productionVariant
    , pvInitialVariantWeight
    , pvVariantName
    , pvModelName
    , pvInitialInstanceCount
    , pvInstanceType

    -- ** ProductionVariantSummary
    , ProductionVariantSummary
    , productionVariantSummary
    , pvsDesiredInstanceCount
    , pvsDesiredWeight
    , pvsCurrentWeight
    , pvsCurrentInstanceCount
    , pvsVariantName

    -- ** ResourceConfig
    , ResourceConfig
    , resourceConfig
    , rcVolumeKMSKeyId
    , rcInstanceType
    , rcInstanceCount
    , rcVolumeSizeInGB

    -- ** S3DataSource
    , S3DataSource
    , s3DataSource
    , sdsS3DataDistributionType
    , sdsS3DataType
    , sdsS3URI

    -- ** StoppingCondition
    , StoppingCondition
    , stoppingCondition
    , scMaxRuntimeInSeconds

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TrainingJobSummary
    , TrainingJobSummary
    , trainingJobSummary
    , tjsTrainingEndTime
    , tjsLastModifiedTime
    , tjsTrainingJobName
    , tjsTrainingJobARN
    , tjsCreationTime
    , tjsTrainingJobStatus

    -- ** VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSecurityGroupIds
    , vcSubnets
    ) where

import Network.AWS.SageMaker.AddTags
import Network.AWS.SageMaker.CreateEndpoint
import Network.AWS.SageMaker.CreateEndpointConfig
import Network.AWS.SageMaker.CreateModel
import Network.AWS.SageMaker.CreateNotebookInstance
import Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL
import Network.AWS.SageMaker.CreateTrainingJob
import Network.AWS.SageMaker.DeleteEndpoint
import Network.AWS.SageMaker.DeleteEndpointConfig
import Network.AWS.SageMaker.DeleteModel
import Network.AWS.SageMaker.DeleteNotebookInstance
import Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DeleteTags
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeEndpointConfig
import Network.AWS.SageMaker.DescribeModel
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.ListEndpointConfigs
import Network.AWS.SageMaker.ListEndpoints
import Network.AWS.SageMaker.ListModels
import Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
import Network.AWS.SageMaker.ListNotebookInstances
import Network.AWS.SageMaker.ListTags
import Network.AWS.SageMaker.ListTrainingJobs
import Network.AWS.SageMaker.StartNotebookInstance
import Network.AWS.SageMaker.StopNotebookInstance
import Network.AWS.SageMaker.StopTrainingJob
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.UpdateEndpoint
import Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
import Network.AWS.SageMaker.UpdateNotebookInstance
import Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SageMaker'.
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
