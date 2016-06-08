{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Elasticsearch Configuration Service
--
-- Use the Amazon Elasticsearch configuration API to create, configure, and manage Elasticsearch domains.
--
-- The endpoint for configuration service requests is region-specific: es./region/.amazonaws.com. For example, es.us-east-1.amazonaws.com. For a current list of supported regions and endpoints, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#cloudsearch_region Regions and Endpoints>.
module Network.AWS.ElasticSearch
    (
    -- * Service Configuration
      elasticSearch

    -- * Errors
    -- $errors

    -- ** ValidationException
    , _ValidationException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** BaseException
    , _BaseException

    -- ** DisabledOperationException
    , _DisabledOperationException

    -- ** InternalException
    , _InternalException

    -- ** InvalidTypeException
    , _InvalidTypeException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateElasticsearchDomain
    , module Network.AWS.ElasticSearch.CreateElasticsearchDomain

    -- ** RemoveTags
    , module Network.AWS.ElasticSearch.RemoveTags

    -- ** DescribeElasticsearchDomains
    , module Network.AWS.ElasticSearch.DescribeElasticsearchDomains

    -- ** DescribeElasticsearchDomain
    , module Network.AWS.ElasticSearch.DescribeElasticsearchDomain

    -- ** ListDomainNames
    , module Network.AWS.ElasticSearch.ListDomainNames

    -- ** DescribeElasticsearchDomainConfig
    , module Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig

    -- ** DeleteElasticsearchDomain
    , module Network.AWS.ElasticSearch.DeleteElasticsearchDomain

    -- ** UpdateElasticsearchDomainConfig
    , module Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig

    -- ** AddTags
    , module Network.AWS.ElasticSearch.AddTags

    -- ** ListTags
    , module Network.AWS.ElasticSearch.ListTags

    -- * Types

    -- ** ESPartitionInstanceType
    , ESPartitionInstanceType (..)

    -- ** OptionState
    , OptionState (..)

    -- ** VolumeType
    , VolumeType (..)

    -- ** AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus

    -- ** AdvancedOptionsStatus
    , AdvancedOptionsStatus
    , advancedOptionsStatus
    , aosOptions
    , aosStatus

    -- ** DomainInfo
    , DomainInfo
    , domainInfo
    , diDomainName

    -- ** EBSOptions
    , EBSOptions
    , ebsOptions
    , eoVolumeSize
    , eoIOPS
    , eoVolumeType
    , eoEBSEnabled

    -- ** EBSOptionsStatus
    , EBSOptionsStatus
    , ebsOptionsStatus
    , eosOptions
    , eosStatus

    -- ** ElasticsearchClusterConfig
    , ElasticsearchClusterConfig
    , elasticsearchClusterConfig
    , eccDedicatedMasterCount
    , eccDedicatedMasterType
    , eccDedicatedMasterEnabled
    , eccInstanceCount
    , eccZoneAwarenessEnabled
    , eccInstanceType

    -- ** ElasticsearchClusterConfigStatus
    , ElasticsearchClusterConfigStatus
    , elasticsearchClusterConfigStatus
    , eccsOptions
    , eccsStatus

    -- ** ElasticsearchDomainConfig
    , ElasticsearchDomainConfig
    , elasticsearchDomainConfig
    , edcEBSOptions
    , edcAccessPolicies
    , edcElasticsearchClusterConfig
    , edcSnapshotOptions
    , edcAdvancedOptions

    -- ** ElasticsearchDomainStatus
    , ElasticsearchDomainStatus
    , elasticsearchDomainStatus
    , edsEBSOptions
    , edsAccessPolicies
    , edsCreated
    , edsSnapshotOptions
    , edsDeleted
    , edsProcessing
    , edsEndpoint
    , edsAdvancedOptions
    , edsDomainId
    , edsDomainName
    , edsARN
    , edsElasticsearchClusterConfig

    -- ** OptionStatus
    , OptionStatus
    , optionStatus
    , osPendingDeletion
    , osUpdateVersion
    , osCreationDate
    , osUpdateDate
    , osState

    -- ** SnapshotOptions
    , SnapshotOptions
    , snapshotOptions
    , soAutomatedSnapshotStartHour

    -- ** SnapshotOptionsStatus
    , SnapshotOptionsStatus
    , snapshotOptionsStatus
    , sosOptions
    , sosStatus

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import           Network.AWS.ElasticSearch.AddTags
import           Network.AWS.ElasticSearch.CreateElasticsearchDomain
import           Network.AWS.ElasticSearch.DeleteElasticsearchDomain
import           Network.AWS.ElasticSearch.DescribeElasticsearchDomain
import           Network.AWS.ElasticSearch.DescribeElasticsearchDomainConfig
import           Network.AWS.ElasticSearch.DescribeElasticsearchDomains
import           Network.AWS.ElasticSearch.ListDomainNames
import           Network.AWS.ElasticSearch.ListTags
import           Network.AWS.ElasticSearch.RemoveTags
import           Network.AWS.ElasticSearch.Types
import           Network.AWS.ElasticSearch.UpdateElasticsearchDomainConfig
import           Network.AWS.ElasticSearch.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ElasticSearch'.
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
