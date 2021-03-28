-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _CacheSubnetGroupInUse
    , _ReservedCacheNodeAlreadyExistsFault
    , _CacheSecurityGroupNotFoundFault
    , _InvalidGlobalReplicationGroupStateFault
    , _CacheSubnetGroupAlreadyExistsFault
    , _GlobalReplicationGroupAlreadyExistsFault
    , _NodeGroupsPerReplicationGroupQuotaExceededFault
    , _CacheSubnetGroupQuotaExceededFault
    , _AuthorizationAlreadyExistsFault
    , _ReservedCacheNodeQuotaExceededFault
    , _ReservedCacheNodesOfferingNotFoundFault
    , _ReplicationGroupNotFoundFault
    , _InvalidSubnet
    , _InvalidUserGroupStateFault
    , _TagQuotaPerResourceExceeded
    , _UserAlreadyExistsFault
    , _InvalidUserStateFault
    , _SnapshotNotFoundFault
    , _InsufficientCacheClusterCapacityFault
    , _InvalidSnapshotStateFault
    , _SnapshotAlreadyExistsFault
    , _DefaultUserRequired
    , _TagNotFoundFault
    , _SnapshotQuotaExceededFault
    , _NodeQuotaForClusterExceededFault
    , _APICallRateForCustomerExceededFault
    , _NodeGroupNotFoundFault
    , _CacheParameterGroupAlreadyExistsFault
    , _ServiceLinkedRoleNotFoundFault
    , _InvalidKMSKeyFault
    , _GlobalReplicationGroupNotFoundFault
    , _ReservedCacheNodeNotFoundFault
    , _CacheSubnetGroupNotFoundFault
    , _SnapshotFeatureNotSupportedFault
    , _InvalidParameterValueException
    , _TestFailoverNotAvailableFault
    , _SubnetNotAllowedFault
    , _InvalidReplicationGroupStateFault
    , _ReplicationGroupAlreadyExistsFault
    , _InvalidVPCNetworkStateFault
    , _SubnetInUse
    , _UserGroupNotFoundFault
    , _CacheClusterNotFoundFault
    , _ClusterQuotaForCustomerExceededFault
    , _AuthorizationNotFoundFault
    , _UserGroupAlreadyExistsFault
    , _InvalidCacheClusterStateFault
    , _CacheSecurityGroupQuotaExceededFault
    , _CacheClusterAlreadyExistsFault
    , _CacheParameterGroupQuotaExceededFault
    , _ServiceUpdateNotFoundFault
    , _DefaultUserAssociatedToUserGroupFault
    , _UserNotFoundFault
    , _NodeQuotaForCustomerExceededFault
    , _CacheSubnetQuotaExceededFault
    , _ReplicationGroupNotUnderMigrationFault
    , _ReplicationGroupAlreadyUnderMigrationFault
    , _CacheParameterGroupNotFoundFault
    , _DuplicateUserNameFault
    , _UserQuotaExceededFault
    , _InvalidARNFault
    , _NoOperationFault
    , _InvalidCacheParameterGroupStateFault
    , _InvalidParameterCombinationException
    , _UserGroupQuotaExceededFault
    , _InvalidCacheSecurityGroupStateFault
    , _CacheSecurityGroupAlreadyExistsFault

    -- * NodeSnapshot
    , NodeSnapshot (..)
    , mkNodeSnapshot
    , nsCacheClusterId
    , nsCacheNodeCreateTime
    , nsCacheNodeId
    , nsCacheSize
    , nsNodeGroupConfiguration
    , nsNodeGroupId
    , nsSnapshotCreateTime

    -- * ServiceUpdateType
    , ServiceUpdateType (..)

    -- * GlobalNodeGroup
    , GlobalNodeGroup (..)
    , mkGlobalNodeGroup
    , gngGlobalNodeGroupId
    , gngSlots

    -- * UserGroupPendingChanges
    , UserGroupPendingChanges (..)
    , mkUserGroupPendingChanges
    , ugpcUserIdsToAdd
    , ugpcUserIdsToRemove

    -- * Snapshot
    , Snapshot (..)
    , mkSnapshot
    , sARN
    , sAutoMinorVersionUpgrade
    , sAutomaticFailover
    , sCacheClusterCreateTime
    , sCacheClusterId
    , sCacheNodeType
    , sCacheParameterGroupName
    , sCacheSubnetGroupName
    , sEngine
    , sEngineVersion
    , sKmsKeyId
    , sNodeSnapshots
    , sNumCacheNodes
    , sNumNodeGroups
    , sPort
    , sPreferredAvailabilityZone
    , sPreferredMaintenanceWindow
    , sPreferredOutpostArn
    , sReplicationGroupDescription
    , sReplicationGroupId
    , sSnapshotName
    , sSnapshotRetentionLimit
    , sSnapshotSource
    , sSnapshotStatus
    , sSnapshotWindow
    , sTopicArn
    , sVpcId

    -- * Event
    , Event (..)
    , mkEvent
    , eDate
    , eMessage
    , eSourceIdentifier
    , eSourceType

    -- * UpdateAction
    , UpdateAction (..)
    , mkUpdateAction
    , uaCacheClusterId
    , uaCacheNodeUpdateStatus
    , uaEngine
    , uaEstimatedUpdateTime
    , uaNodeGroupUpdateStatus
    , uaNodesUpdated
    , uaReplicationGroupId
    , uaServiceUpdateName
    , uaServiceUpdateRecommendedApplyByDate
    , uaServiceUpdateReleaseDate
    , uaServiceUpdateSeverity
    , uaServiceUpdateStatus
    , uaServiceUpdateType
    , uaSlaMet
    , uaUpdateActionAvailableDate
    , uaUpdateActionStatus
    , uaUpdateActionStatusModifiedDate

    -- * NodeGroupConfiguration
    , NodeGroupConfiguration (..)
    , mkNodeGroupConfiguration
    , ngcNodeGroupId
    , ngcPrimaryAvailabilityZone
    , ngcPrimaryOutpostArn
    , ngcReplicaAvailabilityZones
    , ngcReplicaCount
    , ngcReplicaOutpostArns
    , ngcSlots

    -- * FilterName
    , FilterName (..)

    -- * ReshardingStatus
    , ReshardingStatus (..)
    , mkReshardingStatus
    , rsSlotMigration

    -- * NodeGroup
    , NodeGroup (..)
    , mkNodeGroup
    , ngNodeGroupId
    , ngNodeGroupMembers
    , ngPrimaryEndpoint
    , ngReaderEndpoint
    , ngSlots
    , ngStatus

    -- * CacheNodeTypeSpecificValue
    , CacheNodeTypeSpecificValue (..)
    , mkCacheNodeTypeSpecificValue
    , cntsvCacheNodeType
    , cntsvValue

    -- * CustomerNodeEndpoint
    , CustomerNodeEndpoint (..)
    , mkCustomerNodeEndpoint
    , cneAddress
    , cnePort

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * NodeGroupMemberUpdateStatus
    , NodeGroupMemberUpdateStatus (..)
    , mkNodeGroupMemberUpdateStatus
    , ngmusCacheClusterId
    , ngmusCacheNodeId
    , ngmusNodeDeletionDate
    , ngmusNodeUpdateEndDate
    , ngmusNodeUpdateInitiatedBy
    , ngmusNodeUpdateInitiatedDate
    , ngmusNodeUpdateStartDate
    , ngmusNodeUpdateStatus
    , ngmusNodeUpdateStatusModifiedDate

    -- * AllowedNodeGroupId
    , AllowedNodeGroupId (..)

    -- * PendingAutomaticFailoverStatus
    , PendingAutomaticFailoverStatus (..)

    -- * SlaMet
    , SlaMet (..)

    -- * NotificationConfiguration
    , NotificationConfiguration (..)
    , mkNotificationConfiguration
    , ncTopicArn
    , ncTopicStatus

    -- * OutpostMode
    , OutpostMode (..)

    -- * ReplicationGroupPendingModifiedValues
    , ReplicationGroupPendingModifiedValues (..)
    , mkReplicationGroupPendingModifiedValues
    , rgpmvAuthTokenStatus
    , rgpmvAutomaticFailoverStatus
    , rgpmvPrimaryClusterId
    , rgpmvResharding
    , rgpmvUserGroups

    -- * EC2SecurityGroup
    , EC2SecurityGroup (..)
    , mkEC2SecurityGroup
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId
    , ecsgStatus

    -- * ParameterNameValue
    , ParameterNameValue (..)
    , mkParameterNameValue
    , pnvParameterName
    , pnvParameterValue

    -- * SourceType
    , SourceType (..)

    -- * NodeUpdateInitiatedBy
    , NodeUpdateInitiatedBy (..)

    -- * RegionalConfiguration
    , RegionalConfiguration (..)
    , mkRegionalConfiguration
    , rcReplicationGroupId
    , rcReplicationGroupRegion
    , rcReshardingConfiguration

    -- * GlobalReplicationGroup
    , GlobalReplicationGroup (..)
    , mkGlobalReplicationGroup
    , grgARN
    , grgAtRestEncryptionEnabled
    , grgAuthTokenEnabled
    , grgCacheNodeType
    , grgClusterEnabled
    , grgEngine
    , grgEngineVersion
    , grgGlobalNodeGroups
    , grgGlobalReplicationGroupDescription
    , grgGlobalReplicationGroupId
    , grgMembers
    , grgStatus
    , grgTransitEncryptionEnabled

    -- * CacheSubnetGroup
    , CacheSubnetGroup (..)
    , mkCacheSubnetGroup
    , csgARN
    , csgCacheSubnetGroupDescription
    , csgCacheSubnetGroupName
    , csgSubnets
    , csgVpcId

    -- * Authentication
    , Authentication (..)
    , mkAuthentication
    , aPasswordCount
    , aType

    -- * ReservedCacheNode
    , ReservedCacheNode (..)
    , mkReservedCacheNode
    , rcnCacheNodeCount
    , rcnCacheNodeType
    , rcnDuration
    , rcnFixedPrice
    , rcnOfferingType
    , rcnProductDescription
    , rcnRecurringCharges
    , rcnReservationARN
    , rcnReservedCacheNodeId
    , rcnReservedCacheNodesOfferingId
    , rcnStartTime
    , rcnState
    , rcnUsagePrice

    -- * Subnet
    , Subnet (..)
    , mkSubnet
    , sSubnetAvailabilityZone
    , sSubnetIdentifier
    , sSubnetOutpost

    -- * ReshardingConfiguration
    , ReshardingConfiguration (..)
    , mkReshardingConfiguration
    , rcNodeGroupId
    , rcPreferredAvailabilityZones

    -- * SecurityGroupMembership
    , SecurityGroupMembership (..)
    , mkSecurityGroupMembership
    , sgmSecurityGroupId
    , sgmStatus

    -- * UserGroupId
    , UserGroupId (..)

    -- * UpdateActionStatus
    , UpdateActionStatus (..)

    -- * UserGroup
    , UserGroup (..)
    , mkUserGroup
    , ugARN
    , ugEngine
    , ugPendingChanges
    , ugReplicationGroups
    , ugStatus
    , ugUserGroupId
    , ugUserIds

    -- * CacheCluster
    , CacheCluster (..)
    , mkCacheCluster
    , ccARN
    , ccAtRestEncryptionEnabled
    , ccAuthTokenEnabled
    , ccAuthTokenLastModifiedDate
    , ccAutoMinorVersionUpgrade
    , ccCacheClusterCreateTime
    , ccCacheClusterId
    , ccCacheClusterStatus
    , ccCacheNodeType
    , ccCacheNodes
    , ccCacheParameterGroup
    , ccCacheSecurityGroups
    , ccCacheSubnetGroupName
    , ccClientDownloadLandingPage
    , ccConfigurationEndpoint
    , ccEngine
    , ccEngineVersion
    , ccNotificationConfiguration
    , ccNumCacheNodes
    , ccPendingModifiedValues
    , ccPreferredAvailabilityZone
    , ccPreferredMaintenanceWindow
    , ccPreferredOutpostArn
    , ccReplicationGroupId
    , ccSecurityGroups
    , ccSnapshotRetentionLimit
    , ccSnapshotWindow
    , ccTransitEncryptionEnabled

    -- * MultiAZStatus
    , MultiAZStatus (..)

    -- * UpdateActionResultsMessage
    , UpdateActionResultsMessage (..)
    , mkUpdateActionResultsMessage
    , uarmProcessedUpdateActions
    , uarmUnprocessedUpdateActions

    -- * UserName
    , UserName (..)

    -- * EngineDefaults
    , EngineDefaults (..)
    , mkEngineDefaults
    , edCacheNodeTypeSpecificParameters
    , edCacheParameterGroupFamily
    , edMarker
    , edParameters

    -- * CacheParameterGroupStatus
    , CacheParameterGroupStatus (..)
    , mkCacheParameterGroupStatus
    , cpgsCacheNodeIdsToReboot
    , cpgsCacheParameterGroupName
    , cpgsParameterApplyStatus

    -- * AuthTokenUpdateStatus
    , AuthTokenUpdateStatus (..)

    -- * UnprocessedUpdateAction
    , UnprocessedUpdateAction (..)
    , mkUnprocessedUpdateAction
    , uuaCacheClusterId
    , uuaErrorMessage
    , uuaErrorType
    , uuaReplicationGroupId
    , uuaServiceUpdateName

    -- * AuthTokenUpdateStrategyType
    , AuthTokenUpdateStrategyType (..)

    -- * AccessString
    , AccessString (..)

    -- * User
    , User (..)
    , mkUser
    , uARN
    , uAccessString
    , uAuthentication
    , uEngine
    , uStatus
    , uUserGroupIds
    , uUserId
    , uUserName

    -- * CacheNodeUpdateStatus
    , CacheNodeUpdateStatus (..)
    , mkCacheNodeUpdateStatus
    , cnusCacheNodeId
    , cnusNodeDeletionDate
    , cnusNodeUpdateEndDate
    , cnusNodeUpdateInitiatedBy
    , cnusNodeUpdateInitiatedDate
    , cnusNodeUpdateStartDate
    , cnusNodeUpdateStatus
    , cnusNodeUpdateStatusModifiedDate

    -- * UserId
    , UserId (..)

    -- * ServiceUpdate
    , ServiceUpdate (..)
    , mkServiceUpdate
    , suAutoUpdateAfterRecommendedApplyByDate
    , suEngine
    , suEngineVersion
    , suEstimatedUpdateTime
    , suServiceUpdateDescription
    , suServiceUpdateEndDate
    , suServiceUpdateName
    , suServiceUpdateRecommendedApplyByDate
    , suServiceUpdateReleaseDate
    , suServiceUpdateSeverity
    , suServiceUpdateStatus
    , suServiceUpdateType

    -- * CacheNode
    , CacheNode (..)
    , mkCacheNode
    , cnCacheNodeCreateTime
    , cnCacheNodeId
    , cnCacheNodeStatus
    , cnCustomerAvailabilityZone
    , cnCustomerOutpostArn
    , cnEndpoint
    , cnParameterGroupStatus
    , cnSourceCacheNodeId

    -- * ConfigureShard
    , ConfigureShard (..)
    , mkConfigureShard
    , csNodeGroupId
    , csNewReplicaCount
    , csPreferredAvailabilityZones
    , csPreferredOutpostArns

    -- * CacheSecurityGroupMembership
    , CacheSecurityGroupMembership (..)
    , mkCacheSecurityGroupMembership
    , csgmCacheSecurityGroupName
    , csgmStatus

    -- * AvailabilityZone
    , AvailabilityZone (..)
    , mkAvailabilityZone
    , azName

    -- * ServiceUpdateSeverity
    , ServiceUpdateSeverity (..)

    -- * NodeGroupMember
    , NodeGroupMember (..)
    , mkNodeGroupMember
    , ngmCacheClusterId
    , ngmCacheNodeId
    , ngmCurrentRole
    , ngmPreferredAvailabilityZone
    , ngmPreferredOutpostArn
    , ngmReadEndpoint

    -- * TimeRangeFilter
    , TimeRangeFilter (..)
    , mkTimeRangeFilter
    , trfEndTime
    , trfStartTime

    -- * NodeGroupUpdateStatus
    , NodeGroupUpdateStatus (..)
    , mkNodeGroupUpdateStatus
    , ngusNodeGroupId
    , ngusNodeGroupMemberUpdateStatus

    -- * CacheParameterGroup
    , CacheParameterGroup (..)
    , mkCacheParameterGroup
    , cpgARN
    , cpgCacheParameterGroupFamily
    , cpgCacheParameterGroupName
    , cpgDescription
    , cpgIsGlobal

    -- * ProcessedUpdateAction
    , ProcessedUpdateAction (..)
    , mkProcessedUpdateAction
    , puaCacheClusterId
    , puaReplicationGroupId
    , puaServiceUpdateName
    , puaUpdateActionStatus

    -- * AutomaticFailoverStatus
    , AutomaticFailoverStatus (..)

    -- * SubnetOutpost
    , SubnetOutpost (..)
    , mkSubnetOutpost
    , soSubnetOutpostArn

    -- * NodeUpdateStatus
    , NodeUpdateStatus (..)

    -- * CacheSecurityGroup
    , CacheSecurityGroup (..)
    , mkCacheSecurityGroup
    , cARN
    , cCacheSecurityGroupName
    , cDescription
    , cEC2SecurityGroups
    , cOwnerId

    -- * CacheNodeTypeSpecificParameter
    , CacheNodeTypeSpecificParameter (..)
    , mkCacheNodeTypeSpecificParameter
    , cntspAllowedValues
    , cntspCacheNodeTypeSpecificValues
    , cntspChangeType
    , cntspDataType
    , cntspDescription
    , cntspIsModifiable
    , cntspMinimumEngineVersion
    , cntspParameterName
    , cntspSource

    -- * GlobalReplicationGroupMember
    , GlobalReplicationGroupMember (..)
    , mkGlobalReplicationGroupMember
    , grgmAutomaticFailover
    , grgmReplicationGroupId
    , grgmReplicationGroupRegion
    , grgmRole
    , grgmStatus

    -- * AuthenticationType
    , AuthenticationType (..)

    -- * AZMode
    , AZMode (..)

    -- * SlotMigration
    , SlotMigration (..)
    , mkSlotMigration
    , smProgressPercentage

    -- * UserGroupsUpdateStatus
    , UserGroupsUpdateStatus (..)
    , mkUserGroupsUpdateStatus
    , ugusUserGroupIdsToAdd
    , ugusUserGroupIdsToRemove

    -- * Filter
    , Filter (..)
    , mkFilter
    , fName
    , fValues

    -- * CacheEngineVersion
    , CacheEngineVersion (..)
    , mkCacheEngineVersion
    , cevCacheEngineDescription
    , cevCacheEngineVersionDescription
    , cevCacheParameterGroupFamily
    , cevEngine
    , cevEngineVersion

    -- * ServiceUpdateStatus
    , ServiceUpdateStatus (..)

    -- * ReplicationGroup
    , ReplicationGroup (..)
    , mkReplicationGroup
    , rgARN
    , rgAtRestEncryptionEnabled
    , rgAuthTokenEnabled
    , rgAuthTokenLastModifiedDate
    , rgAutomaticFailover
    , rgCacheNodeType
    , rgClusterEnabled
    , rgConfigurationEndpoint
    , rgDescription
    , rgGlobalReplicationGroupInfo
    , rgKmsKeyId
    , rgMemberClusters
    , rgMemberClustersOutpostArns
    , rgMultiAZ
    , rgNodeGroups
    , rgPendingModifiedValues
    , rgReplicationGroupId
    , rgSnapshotRetentionLimit
    , rgSnapshotWindow
    , rgSnapshottingClusterId
    , rgStatus
    , rgTransitEncryptionEnabled
    , rgUserGroupIds

    -- * FilterValue
    , FilterValue (..)

    -- * RecurringCharge
    , RecurringCharge (..)
    , mkRecurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReservedCacheNodesOffering
    , ReservedCacheNodesOffering (..)
    , mkReservedCacheNodesOffering
    , rcnoCacheNodeType
    , rcnoDuration
    , rcnoFixedPrice
    , rcnoOfferingType
    , rcnoProductDescription
    , rcnoRecurringCharges
    , rcnoReservedCacheNodesOfferingId
    , rcnoUsagePrice

    -- * TagListMessage
    , TagListMessage (..)
    , mkTagListMessage
    , tlmTagList

    -- * Endpoint
    , Endpoint (..)
    , mkEndpoint
    , eAddress
    , ePort

    -- * ChangeType
    , ChangeType (..)

    -- * EngineType
    , EngineType (..)

    -- * PendingModifiedValues
    , PendingModifiedValues (..)
    , mkPendingModifiedValues
    , pmvAuthTokenStatus
    , pmvCacheNodeIdsToRemove
    , pmvCacheNodeType
    , pmvEngineVersion
    , pmvNumCacheNodes

    -- * CacheParameterGroupNameMessage
    , CacheParameterGroupNameMessage (..)
    , mkCacheParameterGroupNameMessage
    , cpgnmCacheParameterGroupName

    -- * GlobalReplicationGroupInfo
    , GlobalReplicationGroupInfo (..)
    , mkGlobalReplicationGroupInfo
    , grgiGlobalReplicationGroupId
    , grgiGlobalReplicationGroupMemberRole

    -- * Parameter
    , Parameter (..)
    , mkParameter
    , pAllowedValues
    , pChangeType
    , pDataType
    , pDescription
    , pIsModifiable
    , pMinimumEngineVersion
    , pParameterName
    , pParameterValue
    , pSource

    -- * Engine
    , Engine (..)

    -- * NodeGroupId
    , NodeGroupId (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.ElastiCache.Types.NodeSnapshot
  
  
import Network.AWS.ElastiCache.Types.ServiceUpdateType
  
  
import Network.AWS.ElastiCache.Types.GlobalNodeGroup
  
import Network.AWS.ElastiCache.Types.UserGroupPendingChanges
  
import Network.AWS.ElastiCache.Types.Snapshot
  
  
  
  
import Network.AWS.ElastiCache.Types.Event
  
import Network.AWS.ElastiCache.Types.UpdateAction
  
import Network.AWS.ElastiCache.Types.NodeGroupConfiguration
  
import Network.AWS.ElastiCache.Types.FilterName
  
  
  
import Network.AWS.ElastiCache.Types.ReshardingStatus
  
import Network.AWS.ElastiCache.Types.NodeGroup
  
  
import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
  
  
import Network.AWS.ElastiCache.Types.CustomerNodeEndpoint
  
import Network.AWS.ElastiCache.Types.Tag
  
import Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
  
  
  
  
import Network.AWS.ElastiCache.Types.AllowedNodeGroupId
  
  
  
import Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
  
import Network.AWS.ElastiCache.Types.SlaMet
  
import Network.AWS.ElastiCache.Types.NotificationConfiguration
  
import Network.AWS.ElastiCache.Types.OutpostMode
  
import Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
  
  
  
import Network.AWS.ElastiCache.Types.EC2SecurityGroup
  
import Network.AWS.ElastiCache.Types.ParameterNameValue
  
import Network.AWS.ElastiCache.Types.SourceType
  
  
import Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
  
import Network.AWS.ElastiCache.Types.RegionalConfiguration
  
  
  
import Network.AWS.ElastiCache.Types.GlobalReplicationGroup
  
  
import Network.AWS.ElastiCache.Types.CacheSubnetGroup
  
import Network.AWS.ElastiCache.Types.Authentication
  
import Network.AWS.ElastiCache.Types.ReservedCacheNode
  
  
import Network.AWS.ElastiCache.Types.Subnet
  
import Network.AWS.ElastiCache.Types.ReshardingConfiguration
  
  
import Network.AWS.ElastiCache.Types.SecurityGroupMembership
  
  
import Network.AWS.ElastiCache.Types.UserGroupId
  
import Network.AWS.ElastiCache.Types.UpdateActionStatus
  
import Network.AWS.ElastiCache.Types.UserGroup
  
import Network.AWS.ElastiCache.Types.CacheCluster
  
  
import Network.AWS.ElastiCache.Types.MultiAZStatus
  
import Network.AWS.ElastiCache.Types.UpdateActionResultsMessage
  
import Network.AWS.ElastiCache.Types.UserName
  
  
  
import Network.AWS.ElastiCache.Types.EngineDefaults
  
  
import Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
  
import Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
  
import Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
  
import Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
  
  
  
import Network.AWS.ElastiCache.Types.AccessString
  
import Network.AWS.ElastiCache.Types.User
  
  
  
  
  
import Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
  
import Network.AWS.ElastiCache.Types.UserId
  
  
import Network.AWS.ElastiCache.Types.ServiceUpdate
  
import Network.AWS.ElastiCache.Types.CacheNode
  
import Network.AWS.ElastiCache.Types.ConfigureShard
  
  
  
  
import Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership
  
  
  
  
import Network.AWS.ElastiCache.Types.AvailabilityZone
  
  
import Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
  
  
import Network.AWS.ElastiCache.Types.NodeGroupMember
  
  
  
import Network.AWS.ElastiCache.Types.TimeRangeFilter
  
  
import Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
  
  
import Network.AWS.ElastiCache.Types.CacheParameterGroup
  
  
  
  
import Network.AWS.ElastiCache.Types.ProcessedUpdateAction
  
import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
  
import Network.AWS.ElastiCache.Types.SubnetOutpost
  
  
import Network.AWS.ElastiCache.Types.NodeUpdateStatus
  
import Network.AWS.ElastiCache.Types.CacheSecurityGroup
  
import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
  
  
  
  
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
  
  
import Network.AWS.ElastiCache.Types.AuthenticationType
  
import Network.AWS.ElastiCache.Types.AZMode
  
import Network.AWS.ElastiCache.Types.SlotMigration
  
import Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
  
import Network.AWS.ElastiCache.Types.Filter
  
import Network.AWS.ElastiCache.Types.CacheEngineVersion
  
  
import Network.AWS.ElastiCache.Types.ServiceUpdateStatus
  
import Network.AWS.ElastiCache.Types.ReplicationGroup
  
  
import Network.AWS.ElastiCache.Types.FilterValue
  
import Network.AWS.ElastiCache.Types.RecurringCharge
  
  
import Network.AWS.ElastiCache.Types.ReservedCacheNodesOffering
  
import Network.AWS.ElastiCache.Types.TagListMessage
  
import Network.AWS.ElastiCache.Types.Endpoint
  
  
  
  
  
import Network.AWS.ElastiCache.Types.ChangeType
  
import Network.AWS.ElastiCache.Types.EngineType
  
  
import Network.AWS.ElastiCache.Types.PendingModifiedValues
  
import Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage
  
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
  
  
import Network.AWS.ElastiCache.Types.Parameter
  
  
  
  
import Network.AWS.ElastiCache.Types.Engine
  
import Network.AWS.ElastiCache.Types.NodeGroupId
  

-- | API version @2015-02-02@ of the Amazon ElastiCache SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ElastiCache",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "elasticache",
                 Core._svcVersion = "2015-02-02", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "ElastiCache",
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

-- | The requested cache subnet group is currently in use.
_CacheSubnetGroupInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupInUse
  = Core._MatchServiceError mkServiceConfig "CacheSubnetGroupInUse"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheSubnetGroupInUse #-}
{-# DEPRECATED _CacheSubnetGroupInUse "Use generic-lens or generic-optics instead"  #-}

-- | You already have a reservation with the given identifier.
_ReservedCacheNodeAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodeAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "ReservedCacheNodeAlreadyExists"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ReservedCacheNodeAlreadyExistsFault #-}
{-# DEPRECATED _ReservedCacheNodeAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested cache security group name does not refer to an existing cache security group.
_CacheSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheSecurityGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "CacheSecurityGroupNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _CacheSecurityGroupNotFoundFault #-}
{-# DEPRECATED _CacheSecurityGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The Global Datastore is not available or in primary-only state.
_InvalidGlobalReplicationGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGlobalReplicationGroupStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidGlobalReplicationGroupState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidGlobalReplicationGroupStateFault #-}
{-# DEPRECATED _InvalidGlobalReplicationGroupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested cache subnet group name is already in use by an existing cache subnet group.
_CacheSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "CacheSubnetGroupAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheSubnetGroupAlreadyExistsFault #-}
{-# DEPRECATED _CacheSubnetGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The Global Datastore name already exists.
_GlobalReplicationGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalReplicationGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "GlobalReplicationGroupAlreadyExistsFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _GlobalReplicationGroupAlreadyExistsFault #-}
{-# DEPRECATED _GlobalReplicationGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the maximum allowed number of node groups (shards) in a single replication group. The default maximum is 90
_NodeGroupsPerReplicationGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NodeGroupsPerReplicationGroupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "NodeGroupsPerReplicationGroupQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _NodeGroupsPerReplicationGroupQuotaExceededFault #-}
{-# DEPRECATED _NodeGroupsPerReplicationGroupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the allowed number of cache subnet groups.
_CacheSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "CacheSubnetGroupQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheSubnetGroupQuotaExceededFault #-}
{-# DEPRECATED _CacheSubnetGroupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified Amazon EC2 security group is already authorized for the specified cache security group.
_AuthorizationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "AuthorizationAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _AuthorizationAlreadyExistsFault #-}
{-# DEPRECATED _AuthorizationAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the user's cache node quota.
_ReservedCacheNodeQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodeQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "ReservedCacheNodeQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ReservedCacheNodeQuotaExceededFault #-}
{-# DEPRECATED _ReservedCacheNodeQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested cache node offering does not exist.
_ReservedCacheNodesOfferingNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodesOfferingNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "ReservedCacheNodesOfferingNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ReservedCacheNodesOfferingNotFoundFault #-}
{-# DEPRECATED _ReservedCacheNodesOfferingNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified replication group does not exist.
_ReplicationGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "ReplicationGroupNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ReplicationGroupNotFoundFault #-}
{-# DEPRECATED _ReplicationGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | An invalid subnet identifier was specified.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet
  = Core._MatchServiceError mkServiceConfig "InvalidSubnet" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidSubnet #-}
{-# DEPRECATED _InvalidSubnet "Use generic-lens or generic-optics instead"  #-}

-- | The user group is not in an active state.
_InvalidUserGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUserGroupStateFault
  = Core._MatchServiceError mkServiceConfig "InvalidUserGroupState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidUserGroupStateFault #-}
{-# DEPRECATED _InvalidUserGroupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would cause the resource to have more than the allowed number of tags. The maximum number of tags permitted on a resource is 50.
_TagQuotaPerResourceExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagQuotaPerResourceExceeded
  = Core._MatchServiceError mkServiceConfig
      "TagQuotaPerResourceExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TagQuotaPerResourceExceeded #-}
{-# DEPRECATED _TagQuotaPerResourceExceeded "Use generic-lens or generic-optics instead"  #-}

-- | A user with this ID already exists.
_UserAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig "UserAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UserAlreadyExistsFault #-}
{-# DEPRECATED _UserAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The user is not in active state.
_InvalidUserStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUserStateFault
  = Core._MatchServiceError mkServiceConfig "InvalidUserState" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidUserStateFault #-}
{-# DEPRECATED _InvalidUserStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested snapshot name does not refer to an existing snapshot.
_SnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotNotFoundFault
  = Core._MatchServiceError mkServiceConfig "SnapshotNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _SnapshotNotFoundFault #-}
{-# DEPRECATED _SnapshotNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested cache node type is not available in the specified Availability Zone. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/ErrorMessages.html#ErrorMessages.INSUFFICIENT_CACHE_CLUSTER_CAPACITY InsufficientCacheClusterCapacity> in the ElastiCache User Guide.
_InsufficientCacheClusterCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientCacheClusterCapacityFault
  = Core._MatchServiceError mkServiceConfig
      "InsufficientCacheClusterCapacity"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InsufficientCacheClusterCapacityFault #-}
{-# DEPRECATED _InsufficientCacheClusterCapacityFault "Use generic-lens or generic-optics instead"  #-}

-- | The current state of the snapshot does not allow the requested operation to occur.
_InvalidSnapshotStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSnapshotStateFault
  = Core._MatchServiceError mkServiceConfig "InvalidSnapshotState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidSnapshotStateFault #-}
{-# DEPRECATED _InvalidSnapshotStateFault "Use generic-lens or generic-optics instead"  #-}

-- | You already have a snapshot with the given name.
_SnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "SnapshotAlreadyExistsFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SnapshotAlreadyExistsFault #-}
{-# DEPRECATED _SnapshotAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | You must add default user to a user group.
_DefaultUserRequired :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DefaultUserRequired
  = Core._MatchServiceError mkServiceConfig "DefaultUserRequired"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DefaultUserRequired #-}
{-# DEPRECATED _DefaultUserRequired "Use generic-lens or generic-optics instead"  #-}

-- | The requested tag was not found on this resource.
_TagNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagNotFoundFault
  = Core._MatchServiceError mkServiceConfig "TagNotFound" Core..
      Core.hasStatues 404
{-# INLINEABLE _TagNotFoundFault #-}
{-# DEPRECATED _TagNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the maximum number of snapshots.
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "SnapshotQuotaExceededFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SnapshotQuotaExceededFault #-}
{-# DEPRECATED _SnapshotQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the allowed number of cache nodes in a single cluster.
_NodeQuotaForClusterExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForClusterExceededFault
  = Core._MatchServiceError mkServiceConfig
      "NodeQuotaForClusterExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _NodeQuotaForClusterExceededFault #-}
{-# DEPRECATED _NodeQuotaForClusterExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The customer has exceeded the allowed rate of API calls.
_APICallRateForCustomerExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_APICallRateForCustomerExceededFault
  = Core._MatchServiceError mkServiceConfig
      "APICallRateForCustomerExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _APICallRateForCustomerExceededFault #-}
{-# DEPRECATED _APICallRateForCustomerExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The node group specified by the @NodeGroupId@ parameter could not be found. Please verify that the node group exists and that you spelled the @NodeGroupId@ value correctly.
_NodeGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NodeGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig "NodeGroupNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NodeGroupNotFoundFault #-}
{-# DEPRECATED _NodeGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | A cache parameter group with the requested name already exists.
_CacheParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheParameterGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "CacheParameterGroupAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheParameterGroupAlreadyExistsFault #-}
{-# DEPRECATED _CacheParameterGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified service linked role (SLR) was not found.
_ServiceLinkedRoleNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "ServiceLinkedRoleNotFoundFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ServiceLinkedRoleNotFoundFault #-}
{-# DEPRECATED _ServiceLinkedRoleNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The KMS key supplied is not valid.
_InvalidKMSKeyFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidKMSKeyFault
  = Core._MatchServiceError mkServiceConfig "InvalidKMSKeyFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidKMSKeyFault #-}
{-# DEPRECATED _InvalidKMSKeyFault "Use generic-lens or generic-optics instead"  #-}

-- | The Global Datastore does not exist
_GlobalReplicationGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalReplicationGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "GlobalReplicationGroupNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _GlobalReplicationGroupNotFoundFault #-}
{-# DEPRECATED _GlobalReplicationGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested reserved cache node was not found.
_ReservedCacheNodeNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodeNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "ReservedCacheNodeNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ReservedCacheNodeNotFoundFault #-}
{-# DEPRECATED _ReservedCacheNodeNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested cache subnet group name does not refer to an existing cache subnet group.
_CacheSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "CacheSubnetGroupNotFoundFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheSubnetGroupNotFoundFault #-}
{-# DEPRECATED _CacheSubnetGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | You attempted one of the following operations:
--
--
--     * Creating a snapshot of a Redis cluster running on a @cache.t1.micro@ cache node.
--
--
--     * Creating a snapshot of a cluster that is running Memcached rather than Redis.
--
--
-- Neither of these are supported by ElastiCache.
_SnapshotFeatureNotSupportedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotFeatureNotSupportedFault
  = Core._MatchServiceError mkServiceConfig
      "SnapshotFeatureNotSupportedFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SnapshotFeatureNotSupportedFault #-}
{-# DEPRECATED _SnapshotFeatureNotSupportedFault "Use generic-lens or generic-optics instead"  #-}

-- | The value for a parameter is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException
  = Core._MatchServiceError mkServiceConfig "InvalidParameterValue"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidParameterValueException #-}
{-# DEPRECATED _InvalidParameterValueException "Use generic-lens or generic-optics instead"  #-}

-- | The @TestFailover@ action is not available.
_TestFailoverNotAvailableFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TestFailoverNotAvailableFault
  = Core._MatchServiceError mkServiceConfig
      "TestFailoverNotAvailableFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TestFailoverNotAvailableFault #-}
{-# DEPRECATED _TestFailoverNotAvailableFault "Use generic-lens or generic-optics instead"  #-}

-- | At least one subnet ID does not match the other subnet IDs. This mismatch typically occurs when a user sets one subnet ID to a regional Availability Zone and a different one to an outpost. Or when a user sets the subnet ID to an Outpost when not subscribed on this service.
_SubnetNotAllowedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetNotAllowedFault
  = Core._MatchServiceError mkServiceConfig "SubnetNotAllowedFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SubnetNotAllowedFault #-}
{-# DEPRECATED _SubnetNotAllowedFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested replication group is not in the @available@ state.
_InvalidReplicationGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidReplicationGroupStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidReplicationGroupState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidReplicationGroupStateFault #-}
{-# DEPRECATED _InvalidReplicationGroupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified replication group already exists.
_ReplicationGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "ReplicationGroupAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ReplicationGroupAlreadyExistsFault #-}
{-# DEPRECATED _ReplicationGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The VPC network is in an invalid state.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidVPCNetworkStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidVPCNetworkStateFault #-}
{-# DEPRECATED _InvalidVPCNetworkStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested subnet is being used by another cache subnet group.
_SubnetInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetInUse
  = Core._MatchServiceError mkServiceConfig "SubnetInUse" Core..
      Core.hasStatues 400
{-# INLINEABLE _SubnetInUse #-}
{-# DEPRECATED _SubnetInUse "Use generic-lens or generic-optics instead"  #-}

-- | The user group was not found or does not exist
_UserGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig "UserGroupNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _UserGroupNotFoundFault #-}
{-# DEPRECATED _UserGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested cluster ID does not refer to an existing cluster.
_CacheClusterNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheClusterNotFoundFault
  = Core._MatchServiceError mkServiceConfig "CacheClusterNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _CacheClusterNotFoundFault #-}
{-# DEPRECATED _CacheClusterNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the allowed number of clusters per customer.
_ClusterQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterQuotaForCustomerExceededFault
  = Core._MatchServiceError mkServiceConfig
      "ClusterQuotaForCustomerExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ClusterQuotaForCustomerExceededFault #-}
{-# DEPRECATED _ClusterQuotaForCustomerExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified Amazon EC2 security group is not authorized for the specified cache security group.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault
  = Core._MatchServiceError mkServiceConfig "AuthorizationNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _AuthorizationNotFoundFault #-}
{-# DEPRECATED _AuthorizationNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The user group with this ID already exists.
_UserGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig "UserGroupAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UserGroupAlreadyExistsFault #-}
{-# DEPRECATED _UserGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested cluster is not in the @available@ state.
_InvalidCacheClusterStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCacheClusterStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidCacheClusterState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidCacheClusterStateFault #-}
{-# DEPRECATED _InvalidCacheClusterStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the allowed number of cache security groups.
_CacheSecurityGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheSecurityGroupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "QuotaExceeded.CacheSecurityGroup"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheSecurityGroupQuotaExceededFault #-}
{-# DEPRECATED _CacheSecurityGroupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | You already have a cluster with the given identifier.
_CacheClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheClusterAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "CacheClusterAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheClusterAlreadyExistsFault #-}
{-# DEPRECATED _CacheClusterAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the maximum number of cache security groups.
_CacheParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheParameterGroupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "CacheParameterGroupQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheParameterGroupQuotaExceededFault #-}
{-# DEPRECATED _CacheParameterGroupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The service update doesn't exist
_ServiceUpdateNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUpdateNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "ServiceUpdateNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ServiceUpdateNotFoundFault #-}
{-# DEPRECATED _ServiceUpdateNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | 
_DefaultUserAssociatedToUserGroupFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DefaultUserAssociatedToUserGroupFault
  = Core._MatchServiceError mkServiceConfig
      "DefaultUserAssociatedToUserGroup"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DefaultUserAssociatedToUserGroupFault #-}
{-# DEPRECATED _DefaultUserAssociatedToUserGroupFault "Use generic-lens or generic-optics instead"  #-}

-- | The user does not exist or could not be found.
_UserNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserNotFoundFault
  = Core._MatchServiceError mkServiceConfig "UserNotFound" Core..
      Core.hasStatues 404
{-# INLINEABLE _UserNotFoundFault #-}
{-# DEPRECATED _UserNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the allowed number of cache nodes per customer.
_NodeQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForCustomerExceededFault
  = Core._MatchServiceError mkServiceConfig
      "NodeQuotaForCustomerExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _NodeQuotaForCustomerExceededFault #-}
{-# DEPRECATED _NodeQuotaForCustomerExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The request cannot be processed because it would exceed the allowed number of subnets in a cache subnet group.
_CacheSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheSubnetQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "CacheSubnetQuotaExceededFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheSubnetQuotaExceededFault #-}
{-# DEPRECATED _CacheSubnetQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The designated replication group is not available for data migration.
_ReplicationGroupNotUnderMigrationFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupNotUnderMigrationFault
  = Core._MatchServiceError mkServiceConfig
      "ReplicationGroupNotUnderMigrationFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ReplicationGroupNotUnderMigrationFault #-}
{-# DEPRECATED _ReplicationGroupNotUnderMigrationFault "Use generic-lens or generic-optics instead"  #-}

-- | The targeted replication group is not available. 
_ReplicationGroupAlreadyUnderMigrationFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupAlreadyUnderMigrationFault
  = Core._MatchServiceError mkServiceConfig
      "ReplicationGroupAlreadyUnderMigrationFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ReplicationGroupAlreadyUnderMigrationFault #-}
{-# DEPRECATED _ReplicationGroupAlreadyUnderMigrationFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested cache parameter group name does not refer to an existing cache parameter group.
_CacheParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheParameterGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "CacheParameterGroupNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _CacheParameterGroupNotFoundFault #-}
{-# DEPRECATED _CacheParameterGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | A user with this username already exists.
_DuplicateUserNameFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateUserNameFault
  = Core._MatchServiceError mkServiceConfig "DuplicateUserName"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DuplicateUserNameFault #-}
{-# DEPRECATED _DuplicateUserNameFault "Use generic-lens or generic-optics instead"  #-}

-- | The quota of users has been exceeded.
_UserQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig "UserQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UserQuotaExceededFault #-}
{-# DEPRECATED _UserQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested Amazon Resource Name (ARN) does not refer to an existing resource.
_InvalidARNFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidARNFault
  = Core._MatchServiceError mkServiceConfig "InvalidARN" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidARNFault #-}
{-# DEPRECATED _InvalidARNFault "Use generic-lens or generic-optics instead"  #-}

-- | The operation was not performed because no changes were required.
_NoOperationFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoOperationFault
  = Core._MatchServiceError mkServiceConfig "NoOperationFault" Core..
      Core.hasStatues 400
{-# INLINEABLE _NoOperationFault #-}
{-# DEPRECATED _NoOperationFault "Use generic-lens or generic-optics instead"  #-}

-- | The current state of the cache parameter group does not allow the requested operation to occur.
_InvalidCacheParameterGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCacheParameterGroupStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidCacheParameterGroupState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidCacheParameterGroupStateFault #-}
{-# DEPRECATED _InvalidCacheParameterGroupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | Two or more incompatible parameters were specified.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterCombination"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidParameterCombinationException #-}
{-# DEPRECATED _InvalidParameterCombinationException "Use generic-lens or generic-optics instead"  #-}

-- | The number of users exceeds the user group limit.
_UserGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserGroupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig "UserGroupQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UserGroupQuotaExceededFault #-}
{-# DEPRECATED _UserGroupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The current state of the cache security group does not allow deletion.
_InvalidCacheSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCacheSecurityGroupStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidCacheSecurityGroupState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidCacheSecurityGroupStateFault #-}
{-# DEPRECATED _InvalidCacheSecurityGroupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | A cache security group with the specified name already exists.
_CacheSecurityGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CacheSecurityGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "CacheSecurityGroupAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CacheSecurityGroupAlreadyExistsFault #-}
{-# DEPRECATED _CacheSecurityGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}
