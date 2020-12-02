{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Database Migration Service__
--
-- AWS Database Migration Service (AWS DMS) can migrate your data to and from the most widely used commercial and open-source databases such as Oracle, PostgreSQL, Microsoft SQL Server, Amazon Redshift, MariaDB, Amazon Aurora, MySQL, and SAP Adaptive Server Enterprise (ASE). The service supports homogeneous migrations such as Oracle to Oracle, as well as heterogeneous migrations between different database platforms, such as Oracle to MySQL or SQL Server to PostgreSQL.
--
-- For more information about AWS DMS, see the AWS DMS user guide at <http://docs.aws.amazon.com/dms/latest/userguide/Welcome.html What Is AWS Database Migration Service? >
--
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

    -- ** InvalidCertificateFault
    , _InvalidCertificateFault

    -- ** SNSNoAuthorizationFault
    , _SNSNoAuthorizationFault

    -- ** ResourceAlreadyExistsFault
    , _ResourceAlreadyExistsFault

    -- ** InsufficientResourceCapacityFault
    , _InsufficientResourceCapacityFault

    -- ** SNSInvalidTopicFault
    , _SNSInvalidTopicFault

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

    -- ** RebootReplicationInstance
    , module Network.AWS.DMS.RebootReplicationInstance

    -- ** ReloadTables
    , module Network.AWS.DMS.ReloadTables

    -- ** StartReplicationTaskAssessment
    , module Network.AWS.DMS.StartReplicationTaskAssessment

    -- ** CreateEndpoint
    , module Network.AWS.DMS.CreateEndpoint

    -- ** DescribeSchemas (Paginated)
    , module Network.AWS.DMS.DescribeSchemas

    -- ** ModifyEventSubscription
    , module Network.AWS.DMS.ModifyEventSubscription

    -- ** DescribeReplicationInstanceTaskLogs
    , module Network.AWS.DMS.DescribeReplicationInstanceTaskLogs

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.DMS.DescribeEvents

    -- ** DeleteEndpoint
    , module Network.AWS.DMS.DeleteEndpoint

    -- ** ListTagsForResource
    , module Network.AWS.DMS.ListTagsForResource

    -- ** DescribeEndpointTypes (Paginated)
    , module Network.AWS.DMS.DescribeEndpointTypes

    -- ** DeleteReplicationTask
    , module Network.AWS.DMS.DeleteReplicationTask

    -- ** DescribeReplicationTaskAssessmentResults (Paginated)
    , module Network.AWS.DMS.DescribeReplicationTaskAssessmentResults

    -- ** TestConnection
    , module Network.AWS.DMS.TestConnection

    -- ** DescribeConnections (Paginated)
    , module Network.AWS.DMS.DescribeConnections

    -- ** RemoveTagsFromResource
    , module Network.AWS.DMS.RemoveTagsFromResource

    -- ** ModifyEndpoint
    , module Network.AWS.DMS.ModifyEndpoint

    -- ** CreateEventSubscription
    , module Network.AWS.DMS.CreateEventSubscription

    -- ** DescribeCertificates (Paginated)
    , module Network.AWS.DMS.DescribeCertificates

    -- ** DeleteEventSubscription
    , module Network.AWS.DMS.DeleteEventSubscription

    -- ** DescribeTableStatistics (Paginated)
    , module Network.AWS.DMS.DescribeTableStatistics

    -- ** DescribeReplicationSubnetGroups (Paginated)
    , module Network.AWS.DMS.DescribeReplicationSubnetGroups

    -- ** StartReplicationTask
    , module Network.AWS.DMS.StartReplicationTask

    -- ** DescribeEventSubscriptions (Paginated)
    , module Network.AWS.DMS.DescribeEventSubscriptions

    -- ** AddTagsToResource
    , module Network.AWS.DMS.AddTagsToResource

    -- ** CreateReplicationSubnetGroup
    , module Network.AWS.DMS.CreateReplicationSubnetGroup

    -- ** DeleteCertificate
    , module Network.AWS.DMS.DeleteCertificate

    -- ** RefreshSchemas
    , module Network.AWS.DMS.RefreshSchemas

    -- ** DescribeReplicationTasks (Paginated)
    , module Network.AWS.DMS.DescribeReplicationTasks

    -- ** DescribeEventCategories
    , module Network.AWS.DMS.DescribeEventCategories

    -- ** DescribeOrderableReplicationInstances (Paginated)
    , module Network.AWS.DMS.DescribeOrderableReplicationInstances

    -- ** CreateReplicationTask
    , module Network.AWS.DMS.CreateReplicationTask

    -- ** DescribeEndpoints (Paginated)
    , module Network.AWS.DMS.DescribeEndpoints

    -- ** ModifyReplicationInstance
    , module Network.AWS.DMS.ModifyReplicationInstance

    -- ** ImportCertificate
    , module Network.AWS.DMS.ImportCertificate

    -- ** ModifyReplicationSubnetGroup
    , module Network.AWS.DMS.ModifyReplicationSubnetGroup

    -- ** DescribeAccountAttributes
    , module Network.AWS.DMS.DescribeAccountAttributes

    -- ** DescribeReplicationInstances (Paginated)
    , module Network.AWS.DMS.DescribeReplicationInstances

    -- ** DescribeRefreshSchemasStatus
    , module Network.AWS.DMS.DescribeRefreshSchemasStatus

    -- ** StopReplicationTask
    , module Network.AWS.DMS.StopReplicationTask

    -- ** ModifyReplicationTask
    , module Network.AWS.DMS.ModifyReplicationTask

    -- ** CreateReplicationInstance
    , module Network.AWS.DMS.CreateReplicationInstance

    -- ** DeleteReplicationSubnetGroup
    , module Network.AWS.DMS.DeleteReplicationSubnetGroup

    -- * Types

    -- ** AuthMechanismValue
    , AuthMechanismValue (..)

    -- ** AuthTypeValue
    , AuthTypeValue (..)

    -- ** CompressionTypeValue
    , CompressionTypeValue (..)

    -- ** DmsSSLModeValue
    , DmsSSLModeValue (..)

    -- ** MigrationTypeValue
    , MigrationTypeValue (..)

    -- ** NestingLevelValue
    , NestingLevelValue (..)

    -- ** RefreshSchemasStatusTypeValue
    , RefreshSchemasStatusTypeValue (..)

    -- ** ReplicationEndpointTypeValue
    , ReplicationEndpointTypeValue (..)

    -- ** SourceType
    , SourceType (..)

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

    -- ** Certificate
    , Certificate
    , certificate
    , cCertificateOwner
    , cSigningAlgorithm
    , cValidFromDate
    , cCertificatePem
    , cCertificateARN
    , cCertificateCreationDate
    , cCertificateIdentifier
    , cCertificateWallet
    , cKeyLength
    , cValidToDate

    -- ** Connection
    , Connection
    , connection
    , cStatus
    , cReplicationInstanceARN
    , cEndpointIdentifier
    , cReplicationInstanceIdentifier
    , cEndpointARN
    , cLastFailureMessage

    -- ** DynamoDBSettings
    , DynamoDBSettings
    , dynamoDBSettings
    , ddsServiceAccessRoleARN

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eStatus
    , eServerName
    , eCertificateARN
    , eServiceAccessRoleARN
    , eEngineDisplayName
    , eExtraConnectionAttributes
    , eEndpointType
    , eUsername
    , eExternalTableDefinition
    , eEngineName
    , eKMSKeyId
    , eMongoDBSettings
    , eSSLMode
    , eDatabaseName
    , eS3Settings
    , eEndpointIdentifier
    , eExternalId
    , eDynamoDBSettings
    , eEndpointARN
    , ePort

    -- ** Event
    , Event
    , event
    , eSourceType
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage

    -- ** EventCategoryGroup
    , EventCategoryGroup
    , eventCategoryGroup
    , ecgSourceType
    , ecgEventCategories

    -- ** EventSubscription
    , EventSubscription
    , eventSubscription
    , esStatus
    , esCustomerAWSId
    , esCustSubscriptionId
    , esSNSTopicARN
    , esEnabled
    , esSourceType
    , esSubscriptionCreationTime
    , esEventCategoriesList
    , esSourceIdsList

    -- ** Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- ** MongoDBSettings
    , MongoDBSettings
    , mongoDBSettings
    , mdsServerName
    , mdsAuthMechanism
    , mdsUsername
    , mdsKMSKeyId
    , mdsPassword
    , mdsNestingLevel
    , mdsDatabaseName
    , mdsDocsToInvestigate
    , mdsAuthSource
    , mdsExtractDocId
    , mdsAuthType
    , mdsPort

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
    , riReplicationInstancePublicIPAddresses
    , riReplicationSubnetGroup
    , riInstanceCreateTime
    , riFreeUntil
    , riReplicationInstanceStatus
    , riReplicationInstancePrivateIPAddresses
    , riPreferredMaintenanceWindow
    , riReplicationInstancePrivateIPAddress
    , riKMSKeyId
    , riAvailabilityZone
    , riVPCSecurityGroups
    , riMultiAZ
    , riSecondaryAvailabilityZone
    , riReplicationInstanceARN
    , riAllocatedStorage
    , riReplicationInstancePublicIPAddress
    , riReplicationInstanceClass
    , riReplicationInstanceIdentifier
    , riPendingModifiedValues

    -- ** ReplicationInstanceTaskLog
    , ReplicationInstanceTaskLog
    , replicationInstanceTaskLog
    , ritlReplicationTaskName
    , ritlReplicationTaskARN
    , ritlReplicationInstanceTaskLogSize

    -- ** ReplicationPendingModifiedValues
    , ReplicationPendingModifiedValues
    , replicationPendingModifiedValues
    , rpmvEngineVersion
    , rpmvMultiAZ
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
    , rReplicationTaskSettings
    , rStatus
    , rStopReason
    , rTargetEndpointARN
    , rReplicationTaskIdentifier
    , rCdcStartPosition
    , rReplicationTaskStartDate
    , rSourceEndpointARN
    , rRecoveryCheckpoint
    , rTableMappings
    , rReplicationTaskCreationDate
    , rMigrationType
    , rReplicationTaskARN
    , rCdcStopPosition
    , rReplicationTaskStats
    , rReplicationInstanceARN
    , rLastFailureMessage

    -- ** ReplicationTaskAssessmentResult
    , ReplicationTaskAssessmentResult
    , replicationTaskAssessmentResult
    , rtarAssessmentResults
    , rtarAssessmentResultsFile
    , rtarReplicationTaskIdentifier
    , rtarAssessmentStatus
    , rtarS3ObjectURL
    , rtarReplicationTaskLastAssessmentDate
    , rtarReplicationTaskARN

    -- ** ReplicationTaskStats
    , ReplicationTaskStats
    , replicationTaskStats
    , rtsFullLoadProgressPercent
    , rtsElapsedTimeMillis
    , rtsTablesErrored
    , rtsTablesLoaded
    , rtsTablesQueued
    , rtsTablesLoading

    -- ** S3Settings
    , S3Settings
    , s3Settings
    , ssCSVDelimiter
    , ssServiceAccessRoleARN
    , ssBucketFolder
    , ssExternalTableDefinition
    , ssBucketName
    , ssCSVRowDelimiter
    , ssCompressionType

    -- ** Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- ** SupportedEndpointType
    , SupportedEndpointType
    , supportedEndpointType
    , setEngineDisplayName
    , setEndpointType
    , setEngineName
    , setSupportsCDC

    -- ** TableStatistics
    , TableStatistics
    , tableStatistics
    , tsValidationState
    , tsFullLoadRows
    , tsInserts
    , tsFullLoadCondtnlChkFailedRows
    , tsValidationFailedRecords
    , tsValidationSuspendedRecords
    , tsSchemaName
    , tsTableState
    , tsFullLoadErrorRows
    , tsDdls
    , tsDeletes
    , tsUpdates
    , tsValidationPendingRecords
    , tsLastUpdateTime
    , tsTableName

    -- ** TableToReload
    , TableToReload
    , tableToReload
    , ttrSchemaName
    , ttrTableName

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import Network.AWS.DMS.AddTagsToResource
import Network.AWS.DMS.CreateEndpoint
import Network.AWS.DMS.CreateEventSubscription
import Network.AWS.DMS.CreateReplicationInstance
import Network.AWS.DMS.CreateReplicationSubnetGroup
import Network.AWS.DMS.CreateReplicationTask
import Network.AWS.DMS.DeleteCertificate
import Network.AWS.DMS.DeleteEndpoint
import Network.AWS.DMS.DeleteEventSubscription
import Network.AWS.DMS.DeleteReplicationInstance
import Network.AWS.DMS.DeleteReplicationSubnetGroup
import Network.AWS.DMS.DeleteReplicationTask
import Network.AWS.DMS.DescribeAccountAttributes
import Network.AWS.DMS.DescribeCertificates
import Network.AWS.DMS.DescribeConnections
import Network.AWS.DMS.DescribeEndpoints
import Network.AWS.DMS.DescribeEndpointTypes
import Network.AWS.DMS.DescribeEventCategories
import Network.AWS.DMS.DescribeEvents
import Network.AWS.DMS.DescribeEventSubscriptions
import Network.AWS.DMS.DescribeOrderableReplicationInstances
import Network.AWS.DMS.DescribeRefreshSchemasStatus
import Network.AWS.DMS.DescribeReplicationInstances
import Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
import Network.AWS.DMS.DescribeReplicationSubnetGroups
import Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
import Network.AWS.DMS.DescribeReplicationTasks
import Network.AWS.DMS.DescribeSchemas
import Network.AWS.DMS.DescribeTableStatistics
import Network.AWS.DMS.ImportCertificate
import Network.AWS.DMS.ListTagsForResource
import Network.AWS.DMS.ModifyEndpoint
import Network.AWS.DMS.ModifyEventSubscription
import Network.AWS.DMS.ModifyReplicationInstance
import Network.AWS.DMS.ModifyReplicationSubnetGroup
import Network.AWS.DMS.ModifyReplicationTask
import Network.AWS.DMS.RebootReplicationInstance
import Network.AWS.DMS.RefreshSchemas
import Network.AWS.DMS.ReloadTables
import Network.AWS.DMS.RemoveTagsFromResource
import Network.AWS.DMS.StartReplicationTask
import Network.AWS.DMS.StartReplicationTaskAssessment
import Network.AWS.DMS.StopReplicationTask
import Network.AWS.DMS.TestConnection
import Network.AWS.DMS.Types
import Network.AWS.DMS.Waiters

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
