{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Database Migration Service
--
-- AWS Database Migration Service (AWS DMS) can migrate your data to and from the most widely used commercial and open-source databases such as Oracle, PostgreSQL, Microsoft SQL Server, MariaDB, Amazon Aurora, and MySQL. The service supports homogeneous migrations such as Oracle to Oracle, as well as heterogeneous migrations between different database platforms, such as Oracle to MySQL or SQL Server to PostgreSQL.
module Network.AWS.DMS
    (
    -- * Service Configuration
      dms

    -- * Errors
    -- $errors

    -- ** InvalidSubnet
    , _InvalidSubnet

    -- ** KMSKeyNotAccessibleFault
    , _KMSKeyNotAccessibleFault

    -- ** ReplicationSubnetGroupDoesNotCoverEnoughAZs
    , _ReplicationSubnetGroupDoesNotCoverEnoughAZs

    -- ** InvalidResourceStateFault
    , _InvalidResourceStateFault

    -- ** ResourceAlreadyExistsFault
    , _ResourceAlreadyExistsFault

    -- ** InsufficientResourceCapacityFault
    , _InsufficientResourceCapacityFault

    -- ** ResourceQuotaExceededFault
    , _ResourceQuotaExceededFault

    -- ** UpgradeDependencyFailureFault
    , _UpgradeDependencyFailureFault

    -- ** ResourceNotFoundFault
    , _ResourceNotFoundFault

    -- ** StorageQuotaExceededFault
    , _StorageQuotaExceededFault

    -- ** AccessDeniedFault
    , _AccessDeniedFault

    -- ** SubnetAlreadyInUse
    , _SubnetAlreadyInUse

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteReplicationInstance
    , module Network.AWS.DMS.DeleteReplicationInstance

    -- ** CreateEndpoint
    , module Network.AWS.DMS.CreateEndpoint

    -- ** DescribeSchemas
    , module Network.AWS.DMS.DescribeSchemas

    -- ** DeleteEndpoint
    , module Network.AWS.DMS.DeleteEndpoint

    -- ** ListTagsForResource
    , module Network.AWS.DMS.ListTagsForResource

    -- ** DescribeEndpointTypes
    , module Network.AWS.DMS.DescribeEndpointTypes

    -- ** DeleteReplicationTask
    , module Network.AWS.DMS.DeleteReplicationTask

    -- ** TestConnection
    , module Network.AWS.DMS.TestConnection

    -- ** DescribeConnections
    , module Network.AWS.DMS.DescribeConnections

    -- ** RemoveTagsFromResource
    , module Network.AWS.DMS.RemoveTagsFromResource

    -- ** ModifyEndpoint
    , module Network.AWS.DMS.ModifyEndpoint

    -- ** DescribeTableStatistics
    , module Network.AWS.DMS.DescribeTableStatistics

    -- ** DescribeReplicationSubnetGroups
    , module Network.AWS.DMS.DescribeReplicationSubnetGroups

    -- ** StartReplicationTask
    , module Network.AWS.DMS.StartReplicationTask

    -- ** AddTagsToResource
    , module Network.AWS.DMS.AddTagsToResource

    -- ** CreateReplicationSubnetGroup
    , module Network.AWS.DMS.CreateReplicationSubnetGroup

    -- ** RefreshSchemas
    , module Network.AWS.DMS.RefreshSchemas

    -- ** DescribeReplicationTasks
    , module Network.AWS.DMS.DescribeReplicationTasks

    -- ** DescribeOrderableReplicationInstances
    , module Network.AWS.DMS.DescribeOrderableReplicationInstances

    -- ** CreateReplicationTask
    , module Network.AWS.DMS.CreateReplicationTask

    -- ** DescribeEndpoints
    , module Network.AWS.DMS.DescribeEndpoints

    -- ** ModifyReplicationInstance
    , module Network.AWS.DMS.ModifyReplicationInstance

    -- ** ModifyReplicationSubnetGroup
    , module Network.AWS.DMS.ModifyReplicationSubnetGroup

    -- ** DescribeAccountAttributes
    , module Network.AWS.DMS.DescribeAccountAttributes

    -- ** DescribeReplicationInstances
    , module Network.AWS.DMS.DescribeReplicationInstances

    -- ** DescribeRefreshSchemasStatus
    , module Network.AWS.DMS.DescribeRefreshSchemasStatus

    -- ** StopReplicationTask
    , module Network.AWS.DMS.StopReplicationTask

    -- ** CreateReplicationInstance
    , module Network.AWS.DMS.CreateReplicationInstance

    -- ** DeleteReplicationSubnetGroup
    , module Network.AWS.DMS.DeleteReplicationSubnetGroup

    -- * Types

    -- ** MigrationTypeValue
    , MigrationTypeValue (..)

    -- ** RefreshSchemasStatusTypeValue
    , RefreshSchemasStatusTypeValue (..)

    -- ** ReplicationEndpointTypeValue
    , ReplicationEndpointTypeValue (..)

    -- ** StartReplicationTaskTypeValue
    , StartReplicationTaskTypeValue (..)

    -- ** AccountQuota
    , AccountQuota
    , accountQuota
    , aqMax
    , aqUsed
    , aqAccountQuotaName

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- ** Connection
    , Connection
    , connection
    , cStatus
    , cReplicationInstanceARN
    , cEndpointIdentifier
    , cReplicationInstanceIdentifier
    , cEndpointARN
    , cLastFailureMessage

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eStatus
    , eServerName
    , eExtraConnectionAttributes
    , eEndpointType
    , eUsername
    , eEngineName
    , eKMSKeyId
    , eDatabaseName
    , eEndpointIdentifier
    , eEndpointARN
    , ePort

    -- ** Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- ** OrderableReplicationInstance
    , OrderableReplicationInstance
    , orderableReplicationInstance
    , oriEngineVersion
    , oriMinAllocatedStorage
    , oriIncludedAllocatedStorage
    , oriMaxAllocatedStorage
    , oriReplicationInstanceClass
    , oriDefaultAllocatedStorage
    , oriStorageType

    -- ** RefreshSchemasStatus
    , RefreshSchemasStatus
    , refreshSchemasStatus
    , rssStatus
    , rssLastRefreshDate
    , rssReplicationInstanceARN
    , rssEndpointARN
    , rssLastFailureMessage

    -- ** ReplicationInstance
    , ReplicationInstance
    , replicationInstance
    , riEngineVersion
    , riPubliclyAccessible
    , riAutoMinorVersionUpgrade
    , riReplicationSubnetGroup
    , riInstanceCreateTime
    , riReplicationInstanceStatus
    , riPreferredMaintenanceWindow
    , riReplicationInstancePrivateIPAddress
    , riKMSKeyId
    , riAvailabilityZone
    , riReplicationInstanceARN
    , riAllocatedStorage
    , riReplicationInstancePublicIPAddress
    , riReplicationInstanceClass
    , riReplicationInstanceIdentifier
    , riPendingModifiedValues

    -- ** ReplicationPendingModifiedValues
    , ReplicationPendingModifiedValues
    , replicationPendingModifiedValues
    , rpmvEngineVersion
    , rpmvAllocatedStorage
    , rpmvReplicationInstanceClass

    -- ** ReplicationSubnetGroup
    , ReplicationSubnetGroup
    , replicationSubnetGroup
    , rsgVPCId
    , rsgSubnets
    , rsgReplicationSubnetGroupIdentifier
    , rsgSubnetGroupStatus
    , rsgReplicationSubnetGroupDescription

    -- ** ReplicationTask
    , ReplicationTask
    , replicationTask
    , rtReplicationTaskSettings
    , rtStatus
    , rtTargetEndpointARN
    , rtReplicationTaskIdentifier
    , rtReplicationTaskStartDate
    , rtSourceEndpointARN
    , rtTableMappings
    , rtReplicationTaskCreationDate
    , rtMigrationType
    , rtReplicationTaskARN
    , rtReplicationTaskStats
    , rtReplicationInstanceARN
    , rtLastFailureMessage

    -- ** ReplicationTaskStats
    , ReplicationTaskStats
    , replicationTaskStats
    , rtsFullLoadProgressPercent
    , rtsElapsedTimeMillis
    , rtsTablesErrored
    , rtsTablesLoaded
    , rtsTablesQueued
    , rtsTablesLoading

    -- ** Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- ** SupportedEndpointType
    , SupportedEndpointType
    , supportedEndpointType
    , setEndpointType
    , setEngineName
    , setSupportsCDC

    -- ** TableStatistics
    , TableStatistics
    , tableStatistics
    , tsFullLoadRows
    , tsInserts
    , tsSchemaName
    , tsTableState
    , tsDdls
    , tsDeletes
    , tsUpdates
    , tsLastUpdateTime
    , tsTableName

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import           Network.AWS.DMS.AddTagsToResource
import           Network.AWS.DMS.CreateEndpoint
import           Network.AWS.DMS.CreateReplicationInstance
import           Network.AWS.DMS.CreateReplicationSubnetGroup
import           Network.AWS.DMS.CreateReplicationTask
import           Network.AWS.DMS.DeleteEndpoint
import           Network.AWS.DMS.DeleteReplicationInstance
import           Network.AWS.DMS.DeleteReplicationSubnetGroup
import           Network.AWS.DMS.DeleteReplicationTask
import           Network.AWS.DMS.DescribeAccountAttributes
import           Network.AWS.DMS.DescribeConnections
import           Network.AWS.DMS.DescribeEndpoints
import           Network.AWS.DMS.DescribeEndpointTypes
import           Network.AWS.DMS.DescribeOrderableReplicationInstances
import           Network.AWS.DMS.DescribeRefreshSchemasStatus
import           Network.AWS.DMS.DescribeReplicationInstances
import           Network.AWS.DMS.DescribeReplicationSubnetGroups
import           Network.AWS.DMS.DescribeReplicationTasks
import           Network.AWS.DMS.DescribeSchemas
import           Network.AWS.DMS.DescribeTableStatistics
import           Network.AWS.DMS.ListTagsForResource
import           Network.AWS.DMS.ModifyEndpoint
import           Network.AWS.DMS.ModifyReplicationInstance
import           Network.AWS.DMS.ModifyReplicationSubnetGroup
import           Network.AWS.DMS.RefreshSchemas
import           Network.AWS.DMS.RemoveTagsFromResource
import           Network.AWS.DMS.StartReplicationTask
import           Network.AWS.DMS.StopReplicationTask
import           Network.AWS.DMS.TestConnection
import           Network.AWS.DMS.Types
import           Network.AWS.DMS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DMS'.
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
