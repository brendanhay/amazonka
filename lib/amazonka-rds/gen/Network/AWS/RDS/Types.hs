-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _PointInTimeRestoreNotEnabledFault
    , _InvalidDBParameterGroupStateFault
    , _ReservedDBInstanceQuotaExceededFault
    , _SourceNotFoundFault
    , _CertificateNotFoundFault
    , _AuthorizationQuotaExceededFault
    , _DBClusterSnapshotAlreadyExistsFault
    , _DBParameterGroupAlreadyExistsFault
    , _DBInstanceRoleQuotaExceededFault
    , _DBInstanceRoleAlreadyExistsFault
    , _DBParameterGroupQuotaExceededFault
    , _BackupPolicyNotFoundFault
    , _InsufficientDBClusterCapacityFault
    , _ReservedDBInstanceAlreadyExistsFault
    , _ProvisionedIopsNotAvailableInAZFault
    , _DBProxyTargetAlreadyRegisteredFault
    , _AuthorizationAlreadyExistsFault
    , _SubscriptionCategoryNotFoundFault
    , _DBProxyTargetNotFoundFault
    , _SubscriptionNotFoundFault
    , _InvalidSubnet
    , _SharedSnapshotQuotaExceededFault
    , _DBSubnetQuotaExceededFault
    , _GlobalClusterAlreadyExistsFault
    , _OptionGroupNotFoundFault
    , _DBClusterNotFoundFault
    , _DBLogFileNotFoundFault
    , _DBProxyTargetGroupNotFoundFault
    , _InvalidS3BucketFault
    , _DBProxyQuotaExceededFault
    , _IamRoleNotFoundFault
    , _DBClusterAlreadyExistsFault
    , _StorageTypeNotSupportedFault
    , _DBSecurityGroupQuotaExceededFault
    , _OptionGroupAlreadyExistsFault
    , _ExportTaskNotFoundFault
    , _InsufficientAvailableIPsInSubnetFault
    , _DBProxyNotFoundFault
    , _OptionGroupQuotaExceededFault
    , _DBSecurityGroupAlreadyExistsFault
    , _SNSTopicArnNotFoundFault
    , _InvalidDBClusterEndpointStateFault
    , _InvalidEventSubscriptionStateFault
    , _InvalidDBInstanceAutomatedBackupStateFault
    , _KMSKeyNotAccessibleFault
    , _DBSnapshotNotFoundFault
    , _DBClusterParameterGroupNotFoundFault
    , _DBClusterQuotaExceededFault
    , _InvalidExportOnlyFault
    , _SnapshotQuotaExceededFault
    , _InvalidDBClusterCapacityFault
    , _DBSubnetGroupAlreadyExistsFault
    , _SNSNoAuthorizationFault
    , _DBSecurityGroupNotFoundFault
    , _DBSecurityGroupNotSupportedFault
    , _InvalidDBProxyStateFault
    , _InstanceQuotaExceededFault
    , _DBClusterBacktrackNotFoundFault
    , _DomainNotFoundFault
    , _DBParameterGroupNotFoundFault
    , _InvalidDBSubnetGroupFault
    , _ReservedDBInstancesOfferingNotFoundFault
    , _InvalidDBSubnetStateFault
    , _DBClusterSnapshotNotFoundFault
    , _SNSInvalidTopicFault
    , _InsufficientDBInstanceCapacityFault
    , _InvalidDBClusterSnapshotStateFault
    , _InstallationMediaAlreadyExistsFault
    , _SubscriptionAlreadyExistFault
    , _DBClusterRoleAlreadyExistsFault
    , _IamRoleMissingPermissionsFault
    , _DBClusterRoleQuotaExceededFault
    , _InvalidVPCNetworkStateFault
    , _DBInstanceRoleNotFoundFault
    , _AuthorizationNotFoundFault
    , _ReservedDBInstanceNotFoundFault
    , _DBSubnetGroupQuotaExceededFault
    , _CustomAvailabilityZoneNotFoundFault
    , _InvalidGlobalClusterStateFault
    , _DBSubnetGroupNotAllowedFault
    , _InvalidExportTaskStateFault
    , _InvalidExportSourceStateFault
    , _ExportTaskAlreadyExistsFault
    , _EventSubscriptionQuotaExceededFault
    , _InsufficientStorageClusterCapacityFault
    , _DBClusterEndpointQuotaExceededFault
    , _InvalidOptionGroupStateFault
    , _DBInstanceAutomatedBackupQuotaExceededFault
    , _CustomAvailabilityZoneAlreadyExistsFault
    , _InvalidDBClusterStateFault
    , _GlobalClusterNotFoundFault
    , _DBInstanceAlreadyExistsFault
    , _InvalidRestoreFault
    , _InvalidDBSecurityGroupStateFault
    , _ResourceNotFoundFault
    , _DBSubnetGroupNotFoundFault
    , _DBUpgradeDependencyFailureFault
    , _CustomAvailabilityZoneQuotaExceededFault
    , _InvalidDBInstanceStateFault
    , _DBClusterEndpointAlreadyExistsFault
    , _DBSnapshotAlreadyExistsFault
    , _DBInstanceNotFoundFault
    , _StorageQuotaExceededFault
    , _DBProxyAlreadyExistsFault
    , _DBInstanceAutomatedBackupNotFoundFault
    , _InvalidDBSnapshotStateFault
    , _InvalidDBSubnetGroupStateFault
    , _GlobalClusterQuotaExceededFault
    , _DBClusterEndpointNotFoundFault
    , _InstallationMediaNotFoundFault
    , _DBSubnetGroupDoesNotCoverEnoughAZs
    , _SubnetAlreadyInUse
    , _DBClusterRoleNotFoundFault

    -- * RestoreWindow
    , RestoreWindow (..)
    , mkRestoreWindow
    , rwEarliestTime
    , rwLatestTime

    -- * DBProxyTargetGroup
    , DBProxyTargetGroup (..)
    , mkDBProxyTargetGroup
    , dbptgConnectionPoolConfig
    , dbptgCreatedDate
    , dbptgDBProxyName
    , dbptgIsDefault
    , dbptgStatus
    , dbptgTargetGroupArn
    , dbptgTargetGroupName
    , dbptgUpdatedDate

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction (..)
    , mkPendingMaintenanceAction
    , pmaAction
    , pmaAutoAppliedAfterDate
    , pmaCurrentApplyDate
    , pmaDescription
    , pmaForcedApplyDate
    , pmaOptInStatus

    -- * DBCluster
    , DBCluster (..)
    , mkDBCluster
    , dbcActivityStreamKinesisStreamName
    , dbcActivityStreamKmsKeyId
    , dbcActivityStreamMode
    , dbcActivityStreamStatus
    , dbcAllocatedStorage
    , dbcAssociatedRoles
    , dbcAvailabilityZones
    , dbcBacktrackConsumedChangeRecords
    , dbcBacktrackWindow
    , dbcBackupRetentionPeriod
    , dbcCapacity
    , dbcCharacterSetName
    , dbcCloneGroupId
    , dbcClusterCreateTime
    , dbcCopyTagsToSnapshot
    , dbcCrossAccountClone
    , dbcCustomEndpoints
    , dbcDBClusterArn
    , dbcDBClusterIdentifier
    , dbcDBClusterMembers
    , dbcDBClusterOptionGroupMemberships
    , dbcDBClusterParameterGroup
    , dbcDBSubnetGroup
    , dbcDatabaseName
    , dbcDbClusterResourceId
    , dbcDeletionProtection
    , dbcDomainMemberships
    , dbcEarliestBacktrackTime
    , dbcEarliestRestorableTime
    , dbcEnabledCloudwatchLogsExports
    , dbcEndpoint
    , dbcEngine
    , dbcEngineMode
    , dbcEngineVersion
    , dbcGlobalWriteForwardingRequested
    , dbcGlobalWriteForwardingStatus
    , dbcHostedZoneId
    , dbcHttpEndpointEnabled
    , dbcIAMDatabaseAuthenticationEnabled
    , dbcKmsKeyId
    , dbcLatestRestorableTime
    , dbcMasterUsername
    , dbcMultiAZ
    , dbcPercentProgress
    , dbcPort
    , dbcPreferredBackupWindow
    , dbcPreferredMaintenanceWindow
    , dbcReadReplicaIdentifiers
    , dbcReaderEndpoint
    , dbcReplicationSourceIdentifier
    , dbcScalingConfigurationInfo
    , dbcStatus
    , dbcStorageEncrypted
    , dbcTagList
    , dbcVpcSecurityGroups

    -- * DBClusterSnapshotAttribute
    , DBClusterSnapshotAttribute (..)
    , mkDBClusterSnapshotAttribute
    , dbcsaAttributeName
    , dbcsaAttributeValues

    -- * OptionGroup
    , OptionGroup (..)
    , mkOptionGroup
    , ogAllowsVpcAndNonVpcInstanceMemberships
    , ogEngineName
    , ogMajorEngineVersion
    , ogOptionGroupArn
    , ogOptionGroupDescription
    , ogOptionGroupName
    , ogOptions
    , ogVpcId

    -- * DBClusterOptionGroupStatus
    , DBClusterOptionGroupStatus (..)
    , mkDBClusterOptionGroupStatus
    , dbcogsDBClusterOptionGroupName
    , dbcogsStatus

    -- * DBParameterGroupStatus
    , DBParameterGroupStatus (..)
    , mkDBParameterGroupStatus
    , dbpgsDBParameterGroupName
    , dbpgsParameterApplyStatus

    -- * Event
    , Event (..)
    , mkEvent
    , eDate
    , eEventCategories
    , eMessage
    , eSourceArn
    , eSourceIdentifier
    , eSourceType

    -- * DBSecurityGroup
    , DBSecurityGroup (..)
    , mkDBSecurityGroup
    , dbsgDBSecurityGroupArn
    , dbsgDBSecurityGroupDescription
    , dbsgDBSecurityGroupName
    , dbsgEC2SecurityGroups
    , dbsgIPRanges
    , dbsgOwnerId
    , dbsgVpcId

    -- * ValidDBInstanceModificationsMessage
    , ValidDBInstanceModificationsMessage (..)
    , mkValidDBInstanceModificationsMessage
    , vdbimmStorage
    , vdbimmValidProcessorFeatures

    -- * ValidStorageOptions
    , ValidStorageOptions (..)
    , mkValidStorageOptions
    , vsoIopsToStorageRatio
    , vsoProvisionedIops
    , vsoStorageSize
    , vsoStorageType
    , vsoSupportsStorageAutoscaling

    -- * DomainMembership
    , DomainMembership (..)
    , mkDomainMembership
    , dmDomain
    , dmFQDN
    , dmIAMRoleName
    , dmStatus

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * VpnDetails
    , VpnDetails (..)
    , mkVpnDetails
    , vdVpnGatewayIp
    , vdVpnId
    , vdVpnName
    , vdVpnPSK
    , vdVpnState
    , vdVpnTunnelOriginatorIP

    -- * DBEngineVersion
    , DBEngineVersion (..)
    , mkDBEngineVersion
    , dbevDBEngineDescription
    , dbevDBEngineVersionDescription
    , dbevDBParameterGroupFamily
    , dbevDefaultCharacterSet
    , dbevEngine
    , dbevEngineVersion
    , dbevExportableLogTypes
    , dbevStatus
    , dbevSupportedCharacterSets
    , dbevSupportedEngineModes
    , dbevSupportedFeatureNames
    , dbevSupportedNcharCharacterSets
    , dbevSupportedTimezones
    , dbevSupportsGlobalDatabases
    , dbevSupportsLogExportsToCloudwatchLogs
    , dbevSupportsParallelQuery
    , dbevSupportsReadReplica
    , dbevValidUpgradeTarget

    -- * DBProxy
    , DBProxy (..)
    , mkDBProxy
    , dbpAuth
    , dbpCreatedDate
    , dbpDBProxyArn
    , dbpDBProxyName
    , dbpDebugLogging
    , dbpEndpoint
    , dbpEngineFamily
    , dbpIdleClientTimeout
    , dbpRequireTLS
    , dbpRoleArn
    , dbpStatus
    , dbpUpdatedDate
    , dbpVpcSecurityGroupIds
    , dbpVpcSubnetIds

    -- * DBClusterParameterGroup
    , DBClusterParameterGroup (..)
    , mkDBClusterParameterGroup
    , dbcpgDBClusterParameterGroupArn
    , dbcpgDBClusterParameterGroupName
    , dbcpgDBParameterGroupFamily
    , dbcpgDescription

    -- * DoubleRange
    , DoubleRange (..)
    , mkDoubleRange
    , drFrom
    , drTo

    -- * DBSnapshot
    , DBSnapshot (..)
    , mkDBSnapshot
    , dbsAllocatedStorage
    , dbsAvailabilityZone
    , dbsDBInstanceIdentifier
    , dbsDBSnapshotArn
    , dbsDBSnapshotIdentifier
    , dbsDbiResourceId
    , dbsEncrypted
    , dbsEngine
    , dbsEngineVersion
    , dbsIAMDatabaseAuthenticationEnabled
    , dbsInstanceCreateTime
    , dbsIops
    , dbsKmsKeyId
    , dbsLicenseModel
    , dbsMasterUsername
    , dbsOptionGroupName
    , dbsPercentProgress
    , dbsPort
    , dbsProcessorFeatures
    , dbsSnapshotCreateTime
    , dbsSnapshotType
    , dbsSourceDBSnapshotIdentifier
    , dbsSourceRegion
    , dbsStatus
    , dbsStorageType
    , dbsTagList
    , dbsTdeCredentialArn
    , dbsTimezone
    , dbsVpcId

    -- * DBSecurityGroupMembership
    , DBSecurityGroupMembership (..)
    , mkDBSecurityGroupMembership
    , dbsgmDBSecurityGroupName
    , dbsgmStatus

    -- * TargetHealth
    , TargetHealth (..)
    , mkTargetHealth
    , thDescription
    , thReason
    , thState

    -- * SourceRegion
    , SourceRegion (..)
    , mkSourceRegion
    , srEndpoint
    , srRegionName
    , srStatus

    -- * EC2SecurityGroup
    , EC2SecurityGroup (..)
    , mkEC2SecurityGroup
    , ecsgEC2SecurityGroupId
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId
    , ecsgStatus

    -- * UserAuthConfigInfo
    , UserAuthConfigInfo (..)
    , mkUserAuthConfigInfo
    , uaciAuthScheme
    , uaciDescription
    , uaciIAMAuth
    , uaciSecretArn
    , uaciUserName

    -- * SourceType
    , SourceType (..)

    -- * ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions (..)
    , mkResourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- * DBParameterGroup
    , DBParameterGroup (..)
    , mkDBParameterGroup
    , dbpgDBParameterGroupArn
    , dbpgDBParameterGroupFamily
    , dbpgDBParameterGroupName
    , dbpgDescription

    -- * EngineFamily
    , EngineFamily (..)

    -- * DBClusterBacktrack
    , DBClusterBacktrack (..)
    , mkDBClusterBacktrack
    , dbcbBacktrackIdentifier
    , dbcbBacktrackRequestCreationTime
    , dbcbBacktrackTo
    , dbcbBacktrackedFrom
    , dbcbDBClusterIdentifier
    , dbcbStatus

    -- * ReservedDBInstancesOffering
    , ReservedDBInstancesOffering (..)
    , mkReservedDBInstancesOffering
    , rdbioCurrencyCode
    , rdbioDBInstanceClass
    , rdbioDuration
    , rdbioFixedPrice
    , rdbioMultiAZ
    , rdbioOfferingType
    , rdbioProductDescription
    , rdbioRecurringCharges
    , rdbioReservedDBInstancesOfferingId
    , rdbioUsagePrice

    -- * DBClusterSnapshot
    , DBClusterSnapshot (..)
    , mkDBClusterSnapshot
    , dbcsAllocatedStorage
    , dbcsAvailabilityZones
    , dbcsClusterCreateTime
    , dbcsDBClusterIdentifier
    , dbcsDBClusterSnapshotArn
    , dbcsDBClusterSnapshotIdentifier
    , dbcsEngine
    , dbcsEngineVersion
    , dbcsIAMDatabaseAuthenticationEnabled
    , dbcsKmsKeyId
    , dbcsLicenseModel
    , dbcsMasterUsername
    , dbcsPercentProgress
    , dbcsPort
    , dbcsSnapshotCreateTime
    , dbcsSnapshotType
    , dbcsSourceDBClusterSnapshotArn
    , dbcsStatus
    , dbcsStorageEncrypted
    , dbcsTagList
    , dbcsVpcId

    -- * ApplyMethod
    , ApplyMethod (..)

    -- * ReplicaMode
    , ReplicaMode (..)

    -- * UserAuthConfig
    , UserAuthConfig (..)
    , mkUserAuthConfig
    , uacAuthScheme
    , uacDescription
    , uacIAMAuth
    , uacSecretArn
    , uacUserName

    -- * CharacterSet
    , CharacterSet (..)
    , mkCharacterSet
    , csCharacterSetDescription
    , csCharacterSetName

    -- * Subnet
    , Subnet (..)
    , mkSubnet
    , sSubnetAvailabilityZone
    , sSubnetIdentifier
    , sSubnetOutpost
    , sSubnetStatus

    -- * ReservedDBInstance
    , ReservedDBInstance (..)
    , mkReservedDBInstance
    , rdbiCurrencyCode
    , rdbiDBInstanceClass
    , rdbiDBInstanceCount
    , rdbiDuration
    , rdbiFixedPrice
    , rdbiLeaseId
    , rdbiMultiAZ
    , rdbiOfferingType
    , rdbiProductDescription
    , rdbiRecurringCharges
    , rdbiReservedDBInstanceArn
    , rdbiReservedDBInstanceId
    , rdbiReservedDBInstancesOfferingId
    , rdbiStartTime
    , rdbiState
    , rdbiUsagePrice

    -- * CloudwatchLogsExportConfiguration
    , CloudwatchLogsExportConfiguration (..)
    , mkCloudwatchLogsExportConfiguration
    , clecDisableLogTypes
    , clecEnableLogTypes

    -- * AvailableProcessorFeature
    , AvailableProcessorFeature (..)
    , mkAvailableProcessorFeature
    , apfAllowedValues
    , apfDefaultValue
    , apfName

    -- * DBInstanceRole
    , DBInstanceRole (..)
    , mkDBInstanceRole
    , dbirFeatureName
    , dbirRoleArn
    , dbirStatus

    -- * DBClusterSnapshotAttributesResult
    , DBClusterSnapshotAttributesResult (..)
    , mkDBClusterSnapshotAttributesResult
    , dbcsarDBClusterSnapshotAttributes
    , dbcsarDBClusterSnapshotIdentifier

    -- * MinimumEngineVersionPerAllowedValue
    , MinimumEngineVersionPerAllowedValue (..)
    , mkMinimumEngineVersionPerAllowedValue
    , mevpavAllowedValue
    , mevpavMinimumEngineVersion

    -- * EngineDefaults
    , EngineDefaults (..)
    , mkEngineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- * DBClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage (..)
    , mkDBClusterParameterGroupNameMessage
    , dbcpgnmDBClusterParameterGroupName

    -- * InstallationMediaFailureCause
    , InstallationMediaFailureCause (..)
    , mkInstallationMediaFailureCause
    , imfcMessage

    -- * GlobalCluster
    , GlobalCluster (..)
    , mkGlobalCluster
    , gcDatabaseName
    , gcDeletionProtection
    , gcEngine
    , gcEngineVersion
    , gcGlobalClusterArn
    , gcGlobalClusterIdentifier
    , gcGlobalClusterMembers
    , gcGlobalClusterResourceId
    , gcStatus
    , gcStorageEncrypted

    -- * TargetType
    , TargetType (..)

    -- * DBParameterGroupNameMessage
    , DBParameterGroupNameMessage (..)
    , mkDBParameterGroupNameMessage
    , dbpgnmDBParameterGroupName

    -- * CustomAvailabilityZone
    , CustomAvailabilityZone (..)
    , mkCustomAvailabilityZone
    , cazCustomAvailabilityZoneId
    , cazCustomAvailabilityZoneName
    , cazCustomAvailabilityZoneStatus
    , cazVpnDetails

    -- * DBSnapshotAttributesResult
    , DBSnapshotAttributesResult (..)
    , mkDBSnapshotAttributesResult
    , dbsarDBSnapshotAttributes
    , dbsarDBSnapshotIdentifier

    -- * ConnectionPoolConfigurationInfo
    , ConnectionPoolConfigurationInfo (..)
    , mkConnectionPoolConfigurationInfo
    , cpciConnectionBorrowTimeout
    , cpciInitQuery
    , cpciMaxConnectionsPercent
    , cpciMaxIdleConnectionsPercent
    , cpciSessionPinningFilters

    -- * DBClusterMember
    , DBClusterMember (..)
    , mkDBClusterMember
    , dbcmDBClusterParameterGroupStatus
    , dbcmDBInstanceIdentifier
    , dbcmIsClusterWriter
    , dbcmPromotionTier

    -- * OptionGroupOption
    , OptionGroupOption (..)
    , mkOptionGroupOption
    , ogoDefaultPort
    , ogoDescription
    , ogoEngineName
    , ogoMajorEngineVersion
    , ogoMinimumRequiredMinorEngineVersion
    , ogoName
    , ogoOptionGroupOptionSettings
    , ogoOptionGroupOptionVersions
    , ogoOptionsConflictsWith
    , ogoOptionsDependedOn
    , ogoPermanent
    , ogoPersistent
    , ogoPortRequired
    , ogoRequiresAutoMinorEngineVersionUpgrade
    , ogoSupportsOptionVersionDowngrade
    , ogoVpcOnly

    -- * Range
    , Range (..)
    , mkRange
    , rFrom
    , rStep
    , rTo

    -- * DBInstance
    , DBInstance (..)
    , mkDBInstance
    , dbiAllocatedStorage
    , dbiAssociatedRoles
    , dbiAutoMinorVersionUpgrade
    , dbiAvailabilityZone
    , dbiBackupRetentionPeriod
    , dbiCACertificateIdentifier
    , dbiCharacterSetName
    , dbiCopyTagsToSnapshot
    , dbiDBClusterIdentifier
    , dbiDBInstanceArn
    , dbiDBInstanceClass
    , dbiDBInstanceIdentifier
    , dbiDBInstanceStatus
    , dbiDBName
    , dbiDBParameterGroups
    , dbiDBSecurityGroups
    , dbiDBSubnetGroup
    , dbiDbInstancePort
    , dbiDbiResourceId
    , dbiDeletionProtection
    , dbiDomainMemberships
    , dbiEnabledCloudwatchLogsExports
    , dbiEndpoint
    , dbiEngine
    , dbiEngineVersion
    , dbiEnhancedMonitoringResourceArn
    , dbiIAMDatabaseAuthenticationEnabled
    , dbiInstanceCreateTime
    , dbiIops
    , dbiKmsKeyId
    , dbiLatestRestorableTime
    , dbiLicenseModel
    , dbiListenerEndpoint
    , dbiMasterUsername
    , dbiMaxAllocatedStorage
    , dbiMonitoringInterval
    , dbiMonitoringRoleArn
    , dbiMultiAZ
    , dbiNcharCharacterSetName
    , dbiOptionGroupMemberships
    , dbiPendingModifiedValues
    , dbiPerformanceInsightsEnabled
    , dbiPerformanceInsightsKMSKeyId
    , dbiPerformanceInsightsRetentionPeriod
    , dbiPreferredBackupWindow
    , dbiPreferredMaintenanceWindow
    , dbiProcessorFeatures
    , dbiPromotionTier
    , dbiPubliclyAccessible
    , dbiReadReplicaDBClusterIdentifiers
    , dbiReadReplicaDBInstanceIdentifiers
    , dbiReadReplicaSourceDBInstanceIdentifier
    , dbiReplicaMode
    , dbiSecondaryAvailabilityZone
    , dbiStatusInfos
    , dbiStorageEncrypted
    , dbiStorageType
    , dbiTagList
    , dbiTdeCredentialArn
    , dbiTimezone
    , dbiVpcSecurityGroups

    -- * PendingCloudwatchLogsExports
    , PendingCloudwatchLogsExports (..)
    , mkPendingCloudwatchLogsExports
    , pcleLogTypesToDisable
    , pcleLogTypesToEnable

    -- * AccountQuota
    , AccountQuota (..)
    , mkAccountQuota
    , aqAccountQuotaName
    , aqMax
    , aqUsed

    -- * AvailabilityZone
    , AvailabilityZone (..)
    , mkAvailabilityZone
    , azName

    -- * DBClusterEndpoint
    , DBClusterEndpoint (..)
    , mkDBClusterEndpoint
    , dbceCustomEndpointType
    , dbceDBClusterEndpointArn
    , dbceDBClusterEndpointIdentifier
    , dbceDBClusterEndpointResourceIdentifier
    , dbceDBClusterIdentifier
    , dbceEndpoint
    , dbceEndpointType
    , dbceExcludedMembers
    , dbceStaticMembers
    , dbceStatus

    -- * EventSubscription
    , EventSubscription (..)
    , mkEventSubscription
    , esCustSubscriptionId
    , esCustomerAwsId
    , esEnabled
    , esEventCategoriesList
    , esEventSubscriptionArn
    , esSnsTopicArn
    , esSourceIdsList
    , esSourceType
    , esStatus
    , esSubscriptionCreationTime

    -- * DBInstanceAutomatedBackup
    , DBInstanceAutomatedBackup (..)
    , mkDBInstanceAutomatedBackup
    , dbiabAllocatedStorage
    , dbiabAvailabilityZone
    , dbiabDBInstanceArn
    , dbiabDBInstanceIdentifier
    , dbiabDbiResourceId
    , dbiabEncrypted
    , dbiabEngine
    , dbiabEngineVersion
    , dbiabIAMDatabaseAuthenticationEnabled
    , dbiabInstanceCreateTime
    , dbiabIops
    , dbiabKmsKeyId
    , dbiabLicenseModel
    , dbiabMasterUsername
    , dbiabOptionGroupName
    , dbiabPort
    , dbiabRegion
    , dbiabRestoreWindow
    , dbiabStatus
    , dbiabStorageType
    , dbiabTdeCredentialArn
    , dbiabTimezone
    , dbiabVpcId

    -- * ConnectionPoolConfiguration
    , ConnectionPoolConfiguration (..)
    , mkConnectionPoolConfiguration
    , cpcConnectionBorrowTimeout
    , cpcInitQuery
    , cpcMaxConnectionsPercent
    , cpcMaxIdleConnectionsPercent
    , cpcSessionPinningFilters

    -- * ProcessorFeature
    , ProcessorFeature (..)
    , mkProcessorFeature
    , pfName
    , pfValue

    -- * DBSubnetGroup
    , DBSubnetGroup (..)
    , mkDBSubnetGroup
    , dDBSubnetGroupArn
    , dDBSubnetGroupDescription
    , dDBSubnetGroupName
    , dSubnetGroupStatus
    , dSubnets
    , dVpcId

    -- * ActivityStreamMode
    , ActivityStreamMode (..)

    -- * WriteForwardingStatus
    , WriteForwardingStatus (..)

    -- * Certificate
    , Certificate (..)
    , mkCertificate
    , cCertificateArn
    , cCertificateIdentifier
    , cCertificateType
    , cCustomerOverride
    , cCustomerOverrideValidTill
    , cThumbprint
    , cValidFrom
    , cValidTill

    -- * AuthScheme
    , AuthScheme (..)

    -- * DBInstanceStatusInfo
    , DBInstanceStatusInfo (..)
    , mkDBInstanceStatusInfo
    , dbisiMessage
    , dbisiNormal
    , dbisiStatus
    , dbisiStatusType

    -- * OptionSetting
    , OptionSetting (..)
    , mkOptionSetting
    , osAllowedValues
    , osApplyType
    , osDataType
    , osDefaultValue
    , osDescription
    , osIsCollection
    , osIsModifiable
    , osName
    , osValue

    -- * ActivityStreamStatus
    , ActivityStreamStatus (..)

    -- * ScalingConfiguration
    , ScalingConfiguration (..)
    , mkScalingConfiguration
    , scAutoPause
    , scMaxCapacity
    , scMinCapacity
    , scSecondsUntilAutoPause
    , scTimeoutAction

    -- * DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails (..)
    , mkDescribeDBLogFilesDetails
    , ddblfdLastWritten
    , ddblfdLogFileName
    , ddblfdSize

    -- * OrderableDBInstanceOption
    , OrderableDBInstanceOption (..)
    , mkOrderableDBInstanceOption
    , odbioAvailabilityZoneGroup
    , odbioAvailabilityZones
    , odbioAvailableProcessorFeatures
    , odbioDBInstanceClass
    , odbioEngine
    , odbioEngineVersion
    , odbioLicenseModel
    , odbioMaxIopsPerDbInstance
    , odbioMaxIopsPerGib
    , odbioMaxStorageSize
    , odbioMinIopsPerDbInstance
    , odbioMinIopsPerGib
    , odbioMinStorageSize
    , odbioMultiAZCapable
    , odbioOutpostCapable
    , odbioReadReplicaCapable
    , odbioStorageType
    , odbioSupportedEngineModes
    , odbioSupportsEnhancedMonitoring
    , odbioSupportsGlobalDatabases
    , odbioSupportsIAMDatabaseAuthentication
    , odbioSupportsIops
    , odbioSupportsKerberosAuthentication
    , odbioSupportsPerformanceInsights
    , odbioSupportsStorageAutoscaling
    , odbioSupportsStorageEncryption
    , odbioVpc

    -- * DBClusterRole
    , DBClusterRole (..)
    , mkDBClusterRole
    , dbcrFeatureName
    , dbcrRoleArn
    , dbcrStatus

    -- * InstallationMedia
    , InstallationMedia (..)
    , mkInstallationMedia
    , imCustomAvailabilityZoneId
    , imEngine
    , imEngineInstallationMediaPath
    , imEngineVersion
    , imFailureCause
    , imInstallationMediaId
    , imOSInstallationMediaPath
    , imStatus

    -- * Filter
    , Filter (..)
    , mkFilter
    , fName
    , fValues

    -- * RecurringCharge
    , RecurringCharge (..)
    , mkRecurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * Timezone
    , Timezone (..)
    , mkTimezone
    , tTimezoneName

    -- * DBProxyTarget
    , DBProxyTarget (..)
    , mkDBProxyTarget
    , dbptEndpoint
    , dbptPort
    , dbptRdsResourceId
    , dbptTargetArn
    , dbptTargetHealth
    , dbptTrackedClusterId
    , dbptType

    -- * Endpoint
    , Endpoint (..)
    , mkEndpoint
    , eAddress
    , eHostedZoneId
    , ePort

    -- * ScalingConfigurationInfo
    , ScalingConfigurationInfo (..)
    , mkScalingConfigurationInfo
    , sciAutoPause
    , sciMaxCapacity
    , sciMinCapacity
    , sciSecondsUntilAutoPause
    , sciTimeoutAction

    -- * OptionConfiguration
    , OptionConfiguration (..)
    , mkOptionConfiguration
    , ocOptionName
    , ocDBSecurityGroupMemberships
    , ocOptionSettings
    , ocOptionVersion
    , ocPort
    , ocVpcSecurityGroupMemberships

    -- * Option
    , Option (..)
    , mkOption
    , oDBSecurityGroupMemberships
    , oOptionDescription
    , oOptionName
    , oOptionSettings
    , oOptionVersion
    , oPermanent
    , oPersistent
    , oPort
    , oVpcSecurityGroupMemberships

    -- * IPRange
    , IPRange (..)
    , mkIPRange
    , iprCIDRIP
    , iprStatus

    -- * IAMAuthMode
    , IAMAuthMode (..)

    -- * OptionGroupMembership
    , OptionGroupMembership (..)
    , mkOptionGroupMembership
    , ogmOptionGroupName
    , ogmStatus

    -- * EventCategoriesMap
    , EventCategoriesMap (..)
    , mkEventCategoriesMap
    , ecmEventCategories
    , ecmSourceType

    -- * DBSnapshotAttribute
    , DBSnapshotAttribute (..)
    , mkDBSnapshotAttribute
    , dbsaAttributeName
    , dbsaAttributeValues

    -- * DBProxyStatus
    , DBProxyStatus (..)

    -- * TargetState
    , TargetState (..)

    -- * PendingModifiedValues
    , PendingModifiedValues (..)
    , mkPendingModifiedValues
    , pmvAllocatedStorage
    , pmvBackupRetentionPeriod
    , pmvCACertificateIdentifier
    , pmvDBInstanceClass
    , pmvDBInstanceIdentifier
    , pmvDBSubnetGroupName
    , pmvEngineVersion
    , pmvIops
    , pmvLicenseModel
    , pmvMasterUserPassword
    , pmvMultiAZ
    , pmvPendingCloudwatchLogsExports
    , pmvPort
    , pmvProcessorFeatures
    , pmvStorageType

    -- * VpcSecurityGroupMembership
    , VpcSecurityGroupMembership (..)
    , mkVpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVpcSecurityGroupId

    -- * Outpost
    , Outpost (..)
    , mkOutpost
    , oArn

    -- * UpgradeTarget
    , UpgradeTarget (..)
    , mkUpgradeTarget
    , utAutoUpgrade
    , utDescription
    , utEngine
    , utEngineVersion
    , utIsMajorVersionUpgrade

    -- * Parameter
    , Parameter (..)
    , mkParameter
    , pAllowedValues
    , pApplyMethod
    , pApplyType
    , pDataType
    , pDescription
    , pIsModifiable
    , pMinimumEngineVersion
    , pParameterName
    , pParameterValue
    , pSource
    , pSupportedEngineModes

    -- * OptionVersion
    , OptionVersion (..)
    , mkOptionVersion
    , ovIsDefault
    , ovVersion

    -- * OptionGroupOptionSetting
    , OptionGroupOptionSetting (..)
    , mkOptionGroupOptionSetting
    , ogosAllowedValues
    , ogosApplyType
    , ogosDefaultValue
    , ogosIsModifiable
    , ogosIsRequired
    , ogosMinimumEngineVersionPerAllowedValue
    , ogosSettingDescription
    , ogosSettingName

    -- * ExportTask
    , ExportTask (..)
    , mkExportTask
    , etExportOnly
    , etExportTaskIdentifier
    , etFailureCause
    , etIamRoleArn
    , etKmsKeyId
    , etPercentProgress
    , etS3Bucket
    , etS3Prefix
    , etSnapshotTime
    , etSourceArn
    , etStatus
    , etTaskEndTime
    , etTaskStartTime
    , etTotalExtractedDataInGB
    , etWarningMessage

    -- * TargetHealthReason
    , TargetHealthReason (..)

    -- * GlobalClusterMember
    , GlobalClusterMember (..)
    , mkGlobalClusterMember
    , gcmDBClusterArn
    , gcmGlobalWriteForwardingStatus
    , gcmIsWriter
    , gcmReaders

    -- * VpnPSK
    , VpnPSK (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
  
import Network.AWS.RDS.Types.RestoreWindow
  
  
import Network.AWS.RDS.Types.DBProxyTargetGroup
  
  
  
import Network.AWS.RDS.Types.PendingMaintenanceAction
  
  
  
import Network.AWS.RDS.Types.DBCluster
  
import Network.AWS.RDS.Types.DBClusterSnapshotAttribute
  
  
  
import Network.AWS.RDS.Types.OptionGroup
  
import Network.AWS.RDS.Types.DBClusterOptionGroupStatus
  
import Network.AWS.RDS.Types.DBParameterGroupStatus
  
import Network.AWS.RDS.Types.Event
  
  
import Network.AWS.RDS.Types.DBSecurityGroup
  
  
  
  
  
  
  
import Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
  
  
import Network.AWS.RDS.Types.ValidStorageOptions
  
import Network.AWS.RDS.Types.DomainMembership
  
  
import Network.AWS.RDS.Types.Tag
  
  
import Network.AWS.RDS.Types.VpnDetails
  
import Network.AWS.RDS.Types.DBEngineVersion
  
  
import Network.AWS.RDS.Types.DBProxy
  
import Network.AWS.RDS.Types.DBClusterParameterGroup
  
import Network.AWS.RDS.Types.DoubleRange
  
import Network.AWS.RDS.Types.DBSnapshot
  
  
  
  
import Network.AWS.RDS.Types.DBSecurityGroupMembership
  
import Network.AWS.RDS.Types.TargetHealth
  
import Network.AWS.RDS.Types.SourceRegion
  
  
  
import Network.AWS.RDS.Types.EC2SecurityGroup
  
import Network.AWS.RDS.Types.UserAuthConfigInfo
  
import Network.AWS.RDS.Types.SourceType
  
  
  
  
import Network.AWS.RDS.Types.ResourcePendingMaintenanceActions
  
  
  
import Network.AWS.RDS.Types.DBParameterGroup
  
import Network.AWS.RDS.Types.EngineFamily
  
  
import Network.AWS.RDS.Types.DBClusterBacktrack
  
  
  
  
  
  
import Network.AWS.RDS.Types.ReservedDBInstancesOffering
  
import Network.AWS.RDS.Types.DBClusterSnapshot
  
  
  
  
  
import Network.AWS.RDS.Types.ApplyMethod
  
import Network.AWS.RDS.Types.ReplicaMode
  
  
import Network.AWS.RDS.Types.UserAuthConfig
  
  
import Network.AWS.RDS.Types.CharacterSet
  
import Network.AWS.RDS.Types.Subnet
  
  
import Network.AWS.RDS.Types.ReservedDBInstance
  
  
  
  
  
  
import Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration
  
  
  
import Network.AWS.RDS.Types.AvailableProcessorFeature
  
  
import Network.AWS.RDS.Types.DBInstanceRole
  
import Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
  
  
import Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
  
  
  
import Network.AWS.RDS.Types.EngineDefaults
  
  
  
import Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage
  
  
import Network.AWS.RDS.Types.InstallationMediaFailureCause
  
  
  
  
import Network.AWS.RDS.Types.GlobalCluster
  
  
import Network.AWS.RDS.Types.TargetType
  
  
  
  
import Network.AWS.RDS.Types.DBParameterGroupNameMessage
  
  
import Network.AWS.RDS.Types.CustomAvailabilityZone
  
  
import Network.AWS.RDS.Types.DBSnapshotAttributesResult
  
import Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo
  
  
import Network.AWS.RDS.Types.DBClusterMember
  
  
  
  
import Network.AWS.RDS.Types.OptionGroupOption
  
import Network.AWS.RDS.Types.Range
  
import Network.AWS.RDS.Types.DBInstance
  
  
  
  
import Network.AWS.RDS.Types.PendingCloudwatchLogsExports
  
import Network.AWS.RDS.Types.AccountQuota
  
import Network.AWS.RDS.Types.AvailabilityZone
  
import Network.AWS.RDS.Types.DBClusterEndpoint
  
import Network.AWS.RDS.Types.EventSubscription
  
import Network.AWS.RDS.Types.DBInstanceAutomatedBackup
  
  
import Network.AWS.RDS.Types.ConnectionPoolConfiguration
  
import Network.AWS.RDS.Types.ProcessorFeature
  
  
import Network.AWS.RDS.Types.DBSubnetGroup
  
import Network.AWS.RDS.Types.ActivityStreamMode
  
  
import Network.AWS.RDS.Types.WriteForwardingStatus
  
import Network.AWS.RDS.Types.Certificate
  
  
  
import Network.AWS.RDS.Types.AuthScheme
  
import Network.AWS.RDS.Types.DBInstanceStatusInfo
  
  
  
import Network.AWS.RDS.Types.OptionSetting
  
  
import Network.AWS.RDS.Types.ActivityStreamStatus
  
  
  
import Network.AWS.RDS.Types.ScalingConfiguration
  
  
  
import Network.AWS.RDS.Types.DescribeDBLogFilesDetails
  
  
import Network.AWS.RDS.Types.OrderableDBInstanceOption
  
import Network.AWS.RDS.Types.DBClusterRole
  
  
  
import Network.AWS.RDS.Types.InstallationMedia
  
import Network.AWS.RDS.Types.Filter
  
  
  
  
  
import Network.AWS.RDS.Types.RecurringCharge
  
  
  
import Network.AWS.RDS.Types.Timezone
  
  
import Network.AWS.RDS.Types.DBProxyTarget
  
import Network.AWS.RDS.Types.Endpoint
  
import Network.AWS.RDS.Types.ScalingConfigurationInfo
  
  
  
import Network.AWS.RDS.Types.OptionConfiguration
  
  
  
  
import Network.AWS.RDS.Types.Option
  
  
import Network.AWS.RDS.Types.IPRange
  
  
  
import Network.AWS.RDS.Types.IAMAuthMode
  
import Network.AWS.RDS.Types.OptionGroupMembership
  
  
import Network.AWS.RDS.Types.EventCategoriesMap
  
import Network.AWS.RDS.Types.DBSnapshotAttribute
  
import Network.AWS.RDS.Types.DBProxyStatus
  
  
  
import Network.AWS.RDS.Types.TargetState
  
  
import Network.AWS.RDS.Types.PendingModifiedValues
  
  
  
import Network.AWS.RDS.Types.VpcSecurityGroupMembership
  
import Network.AWS.RDS.Types.Outpost
  
  
  
import Network.AWS.RDS.Types.UpgradeTarget
  
import Network.AWS.RDS.Types.Parameter
  
  
import Network.AWS.RDS.Types.OptionVersion
  
import Network.AWS.RDS.Types.OptionGroupOptionSetting
  
import Network.AWS.RDS.Types.ExportTask
  
import Network.AWS.RDS.Types.TargetHealthReason
  
import Network.AWS.RDS.Types.GlobalClusterMember
  
  
import Network.AWS.RDS.Types.VpnPSK
  

-- | API version @2014-10-31@ of the Amazon Relational Database Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "RDS", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "rds", Core._svcVersion = "2014-10-31",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "RDS",
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

-- | @SourceDBInstanceIdentifier@ refers to a DB instance with @BackupRetentionPeriod@ equal to 0. 
_PointInTimeRestoreNotEnabledFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PointInTimeRestoreNotEnabledFault
  = Core._MatchServiceError mkServiceConfig
      "PointInTimeRestoreNotEnabled"
      Core.. Core.hasStatues 400
{-# INLINEABLE _PointInTimeRestoreNotEnabledFault #-}
{-# DEPRECATED _PointInTimeRestoreNotEnabledFault "Use generic-lens or generic-optics instead"  #-}

-- | The DB parameter group is in use or is in an invalid state. If you are attempting to delete the parameter group, you can't delete it when the parameter group is in this state.
_InvalidDBParameterGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBParameterGroupStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBParameterGroupState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBParameterGroupStateFault #-}
{-# DEPRECATED _InvalidDBParameterGroupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | Request would exceed the user's DB Instance quota.
_ReservedDBInstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "ReservedDBInstanceQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ReservedDBInstanceQuotaExceededFault #-}
{-# DEPRECATED _ReservedDBInstanceQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested source could not be found.
_SourceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SourceNotFoundFault
  = Core._MatchServiceError mkServiceConfig "SourceNotFound" Core..
      Core.hasStatues 404
{-# INLINEABLE _SourceNotFoundFault #-}
{-# DEPRECATED _SourceNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | @CertificateIdentifier@ doesn't refer to an existing certificate. 
_CertificateNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundFault
  = Core._MatchServiceError mkServiceConfig "CertificateNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _CertificateNotFoundFault #-}
{-# DEPRECATED _CertificateNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The DB security group authorization quota has been reached.
_AuthorizationQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "AuthorizationQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _AuthorizationQuotaExceededFault #-}
{-# DEPRECATED _AuthorizationQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The user already has a DB cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterSnapshotAlreadyExistsFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBClusterSnapshotAlreadyExistsFault #-}
{-# DEPRECATED _DBClusterSnapshotAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "DBParameterGroupAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBParameterGroupAlreadyExistsFault #-}
{-# DEPRECATED _DBParameterGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | You can't associate any more AWS Identity and Access Management (IAM) roles with the DB instance because the quota has been reached.
_DBInstanceRoleQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "DBInstanceRoleQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBInstanceRoleQuotaExceededFault #-}
{-# DEPRECATED _DBInstanceRoleQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified @RoleArn@ or @FeatureName@ value is already associated with the DB instance.
_DBInstanceRoleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "DBInstanceRoleAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBInstanceRoleAlreadyExistsFault #-}
{-# DEPRECATED _DBInstanceRoleAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The request would result in the user exceeding the allowed number of DB parameter groups.
_DBParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "DBParameterGroupQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBParameterGroupQuotaExceededFault #-}
{-# DEPRECATED _DBParameterGroupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | Prism for 'BackupPolicyNotFoundFault' errors.
_BackupPolicyNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BackupPolicyNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "BackupPolicyNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _BackupPolicyNotFoundFault #-}
{-# DEPRECATED _BackupPolicyNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The DB cluster doesn't have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientDBClusterCapacityFault
  = Core._MatchServiceError mkServiceConfig
      "InsufficientDBClusterCapacityFault"
      Core.. Core.hasStatues 403
{-# INLINEABLE _InsufficientDBClusterCapacityFault #-}
{-# DEPRECATED _InsufficientDBClusterCapacityFault "Use generic-lens or generic-optics instead"  #-}

-- | User already has a reservation with the given identifier.
_ReservedDBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "ReservedDBInstanceAlreadyExists"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ReservedDBInstanceAlreadyExistsFault #-}
{-# DEPRECATED _ReservedDBInstanceAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIopsNotAvailableInAZFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ProvisionedIopsNotAvailableInAZFault
  = Core._MatchServiceError mkServiceConfig
      "ProvisionedIopsNotAvailableInAZFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ProvisionedIopsNotAvailableInAZFault #-}
{-# DEPRECATED _ProvisionedIopsNotAvailableInAZFault "Use generic-lens or generic-optics instead"  #-}

-- | The proxy is already associated with the specified RDS DB instance or Aurora DB cluster.
_DBProxyTargetAlreadyRegisteredFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetAlreadyRegisteredFault
  = Core._MatchServiceError mkServiceConfig
      "DBProxyTargetAlreadyRegisteredFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBProxyTargetAlreadyRegisteredFault #-}
{-# DEPRECATED _DBProxyTargetAlreadyRegisteredFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified CIDR IP range or Amazon EC2 security group is already authorized for the specified DB security group.
_AuthorizationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "AuthorizationAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _AuthorizationAlreadyExistsFault #-}
{-# DEPRECATED _AuthorizationAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The supplied category does not exist.
_SubscriptionCategoryNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionCategoryNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "SubscriptionCategoryNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _SubscriptionCategoryNotFoundFault #-}
{-# DEPRECATED _SubscriptionCategoryNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified RDS DB instance or Aurora DB cluster isn't available for a proxy owned by your AWS account in the specified AWS Region.
_DBProxyTargetNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "DBProxyTargetNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBProxyTargetNotFoundFault #-}
{-# DEPRECATED _DBProxyTargetNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionNotFoundFault
  = Core._MatchServiceError mkServiceConfig "SubscriptionNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _SubscriptionNotFoundFault #-}
{-# DEPRECATED _SubscriptionNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested subnet is invalid, or multiple subnets were requested that are not all in a common VPC.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet
  = Core._MatchServiceError mkServiceConfig "InvalidSubnet" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidSubnet #-}
{-# DEPRECATED _InvalidSubnet "Use generic-lens or generic-optics instead"  #-}

-- | You have exceeded the maximum number of accounts that you can share a manual DB snapshot with.
_SharedSnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SharedSnapshotQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "SharedSnapshotQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SharedSnapshotQuotaExceededFault #-}
{-# DEPRECATED _SharedSnapshotQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The request would result in the user exceeding the allowed number of subnets in a DB subnet groups.
_DBSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "DBSubnetQuotaExceededFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBSubnetQuotaExceededFault #-}
{-# DEPRECATED _DBSubnetQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | 
_GlobalClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalClusterAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "GlobalClusterAlreadyExistsFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _GlobalClusterAlreadyExistsFault #-}
{-# DEPRECATED _GlobalClusterAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified option group could not be found.
_OptionGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OptionGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "OptionGroupNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _OptionGroupNotFoundFault #-}
{-# DEPRECATED _OptionGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBClusterIdentifier@ doesn't refer to an existing DB cluster. 
_DBClusterNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterNotFoundFault
  = Core._MatchServiceError mkServiceConfig "DBClusterNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBClusterNotFoundFault #-}
{-# DEPRECATED _DBClusterNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | @LogFileName@ doesn't refer to an existing DB log file.
_DBLogFileNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBLogFileNotFoundFault
  = Core._MatchServiceError mkServiceConfig "DBLogFileNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBLogFileNotFoundFault #-}
{-# DEPRECATED _DBLogFileNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified target group isn't available for a proxy owned by your AWS account in the specified AWS Region.
_DBProxyTargetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyTargetGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "DBProxyTargetGroupNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBProxyTargetGroupNotFoundFault #-}
{-# DEPRECATED _DBProxyTargetGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified Amazon S3 bucket name can't be found or Amazon RDS isn't authorized to access the specified Amazon S3 bucket. Verify the __SourceS3BucketName__ and __S3IngestionRoleArn__ values and try again.
_InvalidS3BucketFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidS3BucketFault
  = Core._MatchServiceError mkServiceConfig "InvalidS3BucketFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidS3BucketFault #-}
{-# DEPRECATED _InvalidS3BucketFault "Use generic-lens or generic-optics instead"  #-}

-- | Your AWS account already has the maximum number of proxies in the specified AWS Region.
_DBProxyQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "DBProxyQuotaExceededFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBProxyQuotaExceededFault #-}
{-# DEPRECATED _DBProxyQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The IAM role is missing for exporting to an Amazon S3 bucket.
_IamRoleNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IamRoleNotFoundFault
  = Core._MatchServiceError mkServiceConfig "IamRoleNotFound" Core..
      Core.hasStatues 404
{-# INLINEABLE _IamRoleNotFoundFault #-}
{-# DEPRECATED _IamRoleNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The user already has a DB cluster with the given identifier.
_DBClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterAlreadyExistsFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBClusterAlreadyExistsFault #-}
{-# DEPRECATED _DBClusterAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | Storage of the @StorageType@ specified can't be associated with the DB instance. 
_StorageTypeNotSupportedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StorageTypeNotSupportedFault
  = Core._MatchServiceError mkServiceConfig "StorageTypeNotSupported"
      Core.. Core.hasStatues 400
{-# INLINEABLE _StorageTypeNotSupportedFault #-}
{-# DEPRECATED _StorageTypeNotSupportedFault "Use generic-lens or generic-optics instead"  #-}

-- | The request would result in the user exceeding the allowed number of DB security groups.
_DBSecurityGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "QuotaExceeded.DBSecurityGroup"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBSecurityGroupQuotaExceededFault #-}
{-# DEPRECATED _DBSecurityGroupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The option group you are trying to create already exists.
_OptionGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OptionGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "OptionGroupAlreadyExistsFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _OptionGroupAlreadyExistsFault #-}
{-# DEPRECATED _OptionGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The export task doesn't exist.
_ExportTaskNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExportTaskNotFoundFault
  = Core._MatchServiceError mkServiceConfig "ExportTaskNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ExportTaskNotFoundFault #-}
{-# DEPRECATED _ExportTaskNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation can't be performed because there aren't enough available IP addresses in the proxy's subnets. Add more CIDR blocks to the VPC or remove IP address that aren't required from the subnets.
_InsufficientAvailableIPsInSubnetFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientAvailableIPsInSubnetFault
  = Core._MatchServiceError mkServiceConfig
      "InsufficientAvailableIPsInSubnetFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InsufficientAvailableIPsInSubnetFault #-}
{-# DEPRECATED _InsufficientAvailableIPsInSubnetFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified proxy name doesn't correspond to a proxy owned by your AWS account in the specified AWS Region.
_DBProxyNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyNotFoundFault
  = Core._MatchServiceError mkServiceConfig "DBProxyNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBProxyNotFoundFault #-}
{-# DEPRECATED _DBProxyNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The quota of 20 option groups was exceeded for this AWS account.
_OptionGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OptionGroupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "OptionGroupQuotaExceededFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _OptionGroupQuotaExceededFault #-}
{-# DEPRECATED _OptionGroupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | A DB security group with the name specified in @DBSecurityGroupName@ already exists. 
_DBSecurityGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "DBSecurityGroupAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBSecurityGroupAlreadyExistsFault #-}
{-# DEPRECATED _DBSecurityGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The SNS topic ARN does not exist.
_SNSTopicArnNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSTopicArnNotFoundFault
  = Core._MatchServiceError mkServiceConfig "SNSTopicArnNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _SNSTopicArnNotFoundFault #-}
{-# DEPRECATED _SNSTopicArnNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation can't be performed on the endpoint while the endpoint is in this state.
_InvalidDBClusterEndpointStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterEndpointStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBClusterEndpointStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBClusterEndpointStateFault #-}
{-# DEPRECATED _InvalidDBClusterEndpointStateFault "Use generic-lens or generic-optics instead"  #-}

-- | This error can occur if someone else is modifying a subscription. You should retry the action.
_InvalidEventSubscriptionStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEventSubscriptionStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidEventSubscriptionState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidEventSubscriptionStateFault #-}
{-# DEPRECATED _InvalidEventSubscriptionStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The automated backup is in an invalid state. For example, this automated backup is associated with an active instance. 
_InvalidDBInstanceAutomatedBackupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceAutomatedBackupStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBInstanceAutomatedBackupState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBInstanceAutomatedBackupStateFault #-}
{-# DEPRECATED _InvalidDBInstanceAutomatedBackupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | An error occurred accessing an AWS KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault
  = Core._MatchServiceError mkServiceConfig
      "KMSKeyNotAccessibleFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _KMSKeyNotAccessibleFault #-}
{-# DEPRECATED _KMSKeyNotAccessibleFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBSnapshotIdentifier@ doesn't refer to an existing DB snapshot. 
_DBSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSnapshotNotFoundFault
  = Core._MatchServiceError mkServiceConfig "DBSnapshotNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBSnapshotNotFoundFault #-}
{-# DEPRECATED _DBSnapshotNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBClusterParameterGroupName@ doesn't refer to an existing DB cluster parameter group. 
_DBClusterParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterParameterGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterParameterGroupNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBClusterParameterGroupNotFoundFault #-}
{-# DEPRECATED _DBClusterParameterGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The user attempted to create a new DB cluster and the user has already reached the maximum allowed DB cluster quota.
_DBClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterQuotaExceededFault"
      Core.. Core.hasStatues 403
{-# INLINEABLE _DBClusterQuotaExceededFault #-}
{-# DEPRECATED _DBClusterQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The export is invalid for exporting to an Amazon S3 bucket.
_InvalidExportOnlyFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExportOnlyFault
  = Core._MatchServiceError mkServiceConfig "InvalidExportOnly"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidExportOnlyFault #-}
{-# DEPRECATED _InvalidExportOnlyFault "Use generic-lens or generic-optics instead"  #-}

-- | The request would result in the user exceeding the allowed number of DB snapshots.
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig "SnapshotQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SnapshotQuotaExceededFault #-}
{-# DEPRECATED _SnapshotQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | @Capacity@ isn't a valid Aurora Serverless DB cluster capacity. Valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
_InvalidDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterCapacityFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBClusterCapacityFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBClusterCapacityFault #-}
{-# DEPRECATED _InvalidDBClusterCapacityFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBSubnetGroupName@ is already used by an existing DB subnet group. 
_DBSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "DBSubnetGroupAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBSubnetGroupAlreadyExistsFault #-}
{-# DEPRECATED _DBSubnetGroupAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | You do not have permission to publish to the SNS topic ARN.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault
  = Core._MatchServiceError mkServiceConfig "SNSNoAuthorization"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SNSNoAuthorizationFault #-}
{-# DEPRECATED _SNSNoAuthorizationFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBSecurityGroupName@ doesn't refer to an existing DB security group. 
_DBSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig "DBSecurityGroupNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBSecurityGroupNotFoundFault #-}
{-# DEPRECATED _DBSecurityGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | A DB security group isn't allowed for this action.
_DBSecurityGroupNotSupportedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotSupportedFault
  = Core._MatchServiceError mkServiceConfig
      "DBSecurityGroupNotSupported"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBSecurityGroupNotSupportedFault #-}
{-# DEPRECATED _DBSecurityGroupNotSupportedFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation can't be performed while the proxy is in this state.
_InvalidDBProxyStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBProxyStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBProxyStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBProxyStateFault #-}
{-# DEPRECATED _InvalidDBProxyStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The request would result in the user exceeding the allowed number of DB instances.
_InstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig "InstanceQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InstanceQuotaExceededFault #-}
{-# DEPRECATED _InstanceQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | @BacktrackIdentifier@ doesn't refer to an existing backtrack. 
_DBClusterBacktrackNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterBacktrackNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterBacktrackNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBClusterBacktrackNotFoundFault #-}
{-# DEPRECATED _DBClusterBacktrackNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | @Domain@ doesn't refer to an existing Active Directory domain. 
_DomainNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DomainNotFoundFault
  = Core._MatchServiceError mkServiceConfig "DomainNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DomainNotFoundFault #-}
{-# DEPRECATED _DomainNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBParameterGroupName@ doesn't refer to an existing DB parameter group. 
_DBParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "DBParameterGroupNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBParameterGroupNotFoundFault #-}
{-# DEPRECATED _DBParameterGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The DBSubnetGroup doesn't belong to the same VPC as that of an existing cross-region read replica of the same source instance.
_InvalidDBSubnetGroupFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBSubnetGroupFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBSubnetGroupFault #-}
{-# DEPRECATED _InvalidDBSubnetGroupFault "Use generic-lens or generic-optics instead"  #-}

-- | Specified offering does not exist.
_ReservedDBInstancesOfferingNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstancesOfferingNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "ReservedDBInstancesOfferingNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ReservedDBInstancesOfferingNotFoundFault #-}
{-# DEPRECATED _ReservedDBInstancesOfferingNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The DB subnet isn't in the /available/ state. 
_InvalidDBSubnetStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBSubnetStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBSubnetStateFault #-}
{-# DEPRECATED _InvalidDBSubnetStateFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBClusterSnapshotIdentifier@ doesn't refer to an existing DB cluster snapshot. 
_DBClusterSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterSnapshotNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBClusterSnapshotNotFoundFault #-}
{-# DEPRECATED _DBClusterSnapshotNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | SNS has responded that there is a problem with the SND topic specified.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault
  = Core._MatchServiceError mkServiceConfig "SNSInvalidTopic" Core..
      Core.hasStatues 400
{-# INLINEABLE _SNSInvalidTopicFault #-}
{-# DEPRECATED _SNSInvalidTopicFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified DB instance class isn't available in the specified Availability Zone.
_InsufficientDBInstanceCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientDBInstanceCapacityFault
  = Core._MatchServiceError mkServiceConfig
      "InsufficientDBInstanceCapacity"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InsufficientDBInstanceCapacityFault #-}
{-# DEPRECATED _InsufficientDBInstanceCapacityFault "Use generic-lens or generic-optics instead"  #-}

-- | The supplied value isn't a valid DB cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterSnapshotStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBClusterSnapshotStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBClusterSnapshotStateFault #-}
{-# DEPRECATED _InvalidDBClusterSnapshotStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified installation medium has already been imported.
_InstallationMediaAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstallationMediaAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "InstallationMediaAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InstallationMediaAlreadyExistsFault #-}
{-# DEPRECATED _InstallationMediaAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The supplied subscription name already exists.
_SubscriptionAlreadyExistFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubscriptionAlreadyExistFault
  = Core._MatchServiceError mkServiceConfig
      "SubscriptionAlreadyExist"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SubscriptionAlreadyExistFault #-}
{-# DEPRECATED _SubscriptionAlreadyExistFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified IAM role Amazon Resource Name (ARN) is already associated with the specified DB cluster.
_DBClusterRoleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterRoleAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBClusterRoleAlreadyExistsFault #-}
{-# DEPRECATED _DBClusterRoleAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The IAM role requires additional permissions to export to an Amazon S3 bucket.
_IamRoleMissingPermissionsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IamRoleMissingPermissionsFault
  = Core._MatchServiceError mkServiceConfig
      "IamRoleMissingPermissions"
      Core.. Core.hasStatues 400
{-# INLINEABLE _IamRoleMissingPermissionsFault #-}
{-# DEPRECATED _IamRoleMissingPermissionsFault "Use generic-lens or generic-optics instead"  #-}

-- | You have exceeded the maximum number of IAM roles that can be associated with the specified DB cluster.
_DBClusterRoleQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterRoleQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBClusterRoleQuotaExceededFault #-}
{-# DEPRECATED _DBClusterRoleQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The DB subnet group doesn't cover all Availability Zones after it's created because of users' change.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidVPCNetworkStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidVPCNetworkStateFault #-}
{-# DEPRECATED _InvalidVPCNetworkStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified @RoleArn@ value doesn't match the specified feature for the DB instance.
_DBInstanceRoleNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceRoleNotFoundFault
  = Core._MatchServiceError mkServiceConfig "DBInstanceRoleNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBInstanceRoleNotFoundFault #-}
{-# DEPRECATED _DBInstanceRoleNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified CIDR IP range or Amazon EC2 security group might not be authorized for the specified DB security group.
--
-- Or, RDS might not be authorized to perform necessary actions using IAM on your behalf.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault
  = Core._MatchServiceError mkServiceConfig "AuthorizationNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _AuthorizationNotFoundFault #-}
{-# DEPRECATED _AuthorizationNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified reserved DB Instance not found.
_ReservedDBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedDBInstanceNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "ReservedDBInstanceNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ReservedDBInstanceNotFoundFault #-}
{-# DEPRECATED _ReservedDBInstanceNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The request would result in the user exceeding the allowed number of DB subnet groups.
_DBSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "DBSubnetGroupQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBSubnetGroupQuotaExceededFault #-}
{-# DEPRECATED _DBSubnetGroupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | @CustomAvailabilityZoneId@ doesn't refer to an existing custom Availability Zone identifier.
_CustomAvailabilityZoneNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "CustomAvailabilityZoneNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _CustomAvailabilityZoneNotFoundFault #-}
{-# DEPRECATED _CustomAvailabilityZoneNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | 
_InvalidGlobalClusterStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGlobalClusterStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidGlobalClusterStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidGlobalClusterStateFault #-}
{-# DEPRECATED _InvalidGlobalClusterStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The DBSubnetGroup shouldn't be specified while creating read replicas that lie in the same region as the source instance.
_DBSubnetGroupNotAllowedFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotAllowedFault
  = Core._MatchServiceError mkServiceConfig
      "DBSubnetGroupNotAllowedFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBSubnetGroupNotAllowedFault #-}
{-# DEPRECATED _DBSubnetGroupNotAllowedFault "Use generic-lens or generic-optics instead"  #-}

-- | You can't cancel an export task that has completed.
_InvalidExportTaskStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExportTaskStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidExportTaskStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidExportTaskStateFault #-}
{-# DEPRECATED _InvalidExportTaskStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The state of the export snapshot is invalid for exporting to an Amazon S3 bucket.
_InvalidExportSourceStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExportSourceStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidExportSourceState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidExportSourceStateFault #-}
{-# DEPRECATED _InvalidExportSourceStateFault "Use generic-lens or generic-optics instead"  #-}

-- | You can't start an export task that's already running.
_ExportTaskAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExportTaskAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig "ExportTaskAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ExportTaskAlreadyExistsFault #-}
{-# DEPRECATED _ExportTaskAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EventSubscriptionQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "EventSubscriptionQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _EventSubscriptionQuotaExceededFault #-}
{-# DEPRECATED _EventSubscriptionQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | There is insufficient storage available for the current action. You might be able to resolve this error by updating your subnet group to use different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientStorageClusterCapacityFault
  = Core._MatchServiceError mkServiceConfig
      "InsufficientStorageClusterCapacity"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InsufficientStorageClusterCapacityFault #-}
{-# DEPRECATED _InsufficientStorageClusterCapacityFault "Use generic-lens or generic-optics instead"  #-}

-- | The cluster already has the maximum number of custom endpoints.
_DBClusterEndpointQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterEndpointQuotaExceededFault"
      Core.. Core.hasStatues 403
{-# INLINEABLE _DBClusterEndpointQuotaExceededFault #-}
{-# DEPRECATED _DBClusterEndpointQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The option group isn't in the /available/ state. 
_InvalidOptionGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOptionGroupStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidOptionGroupStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidOptionGroupStateFault #-}
{-# DEPRECATED _InvalidOptionGroupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The quota for retained automated backups was exceeded. This prevents you from retaining any additional automated backups. The retained automated backups quota is the same as your DB Instance quota.
_DBInstanceAutomatedBackupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceAutomatedBackupQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "DBInstanceAutomatedBackupQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBInstanceAutomatedBackupQuotaExceededFault #-}
{-# DEPRECATED _DBInstanceAutomatedBackupQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | @CustomAvailabilityZoneName@ is already used by an existing custom Availability Zone.
_CustomAvailabilityZoneAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "CustomAvailabilityZoneAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CustomAvailabilityZoneAlreadyExistsFault #-}
{-# DEPRECATED _CustomAvailabilityZoneAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | The requested operation can't be performed while the cluster is in this state.
_InvalidDBClusterStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBClusterStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBClusterStateFault #-}
{-# DEPRECATED _InvalidDBClusterStateFault "Use generic-lens or generic-optics instead"  #-}

-- | 
_GlobalClusterNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalClusterNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "GlobalClusterNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _GlobalClusterNotFoundFault #-}
{-# DEPRECATED _GlobalClusterNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The user already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig "DBInstanceAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBInstanceAlreadyExistsFault #-}
{-# DEPRECATED _DBInstanceAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | Cannot restore from VPC backup to non-VPC DB instance.
_InvalidRestoreFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreFault
  = Core._MatchServiceError mkServiceConfig "InvalidRestoreFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRestoreFault #-}
{-# DEPRECATED _InvalidRestoreFault "Use generic-lens or generic-optics instead"  #-}

-- | The state of the DB security group doesn't allow deletion.
_InvalidDBSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSecurityGroupStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBSecurityGroupState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBSecurityGroupStateFault #-}
{-# DEPRECATED _InvalidDBSecurityGroupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault
  = Core._MatchServiceError mkServiceConfig "ResourceNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundFault #-}
{-# DEPRECATED _ResourceNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBSubnetGroupName@ doesn't refer to an existing DB subnet group. 
_DBSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "DBSubnetGroupNotFoundFault"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBSubnetGroupNotFoundFault #-}
{-# DEPRECATED _DBSubnetGroupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The DB upgrade failed because a resource the DB depends on can't be modified.
_DBUpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBUpgradeDependencyFailureFault
  = Core._MatchServiceError mkServiceConfig
      "DBUpgradeDependencyFailure"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBUpgradeDependencyFailureFault #-}
{-# DEPRECATED _DBUpgradeDependencyFailureFault "Use generic-lens or generic-optics instead"  #-}

-- | You have exceeded the maximum number of custom Availability Zones.
_CustomAvailabilityZoneQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CustomAvailabilityZoneQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "CustomAvailabilityZoneQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CustomAvailabilityZoneQuotaExceededFault #-}
{-# DEPRECATED _CustomAvailabilityZoneQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The DB instance isn't in a valid state.
_InvalidDBInstanceStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceStateFault
  = Core._MatchServiceError mkServiceConfig "InvalidDBInstanceState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBInstanceStateFault #-}
{-# DEPRECATED _InvalidDBInstanceStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified custom endpoint can't be created because it already exists.
_DBClusterEndpointAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterEndpointAlreadyExistsFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBClusterEndpointAlreadyExistsFault #-}
{-# DEPRECATED _DBClusterEndpointAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBSnapshotIdentifier@ is already used by an existing snapshot. 
_DBSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSnapshotAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig "DBSnapshotAlreadyExists"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBSnapshotAlreadyExistsFault #-}
{-# DEPRECATED _DBSnapshotAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | @DBInstanceIdentifier@ doesn't refer to an existing DB instance. 
_DBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceNotFoundFault
  = Core._MatchServiceError mkServiceConfig "DBInstanceNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBInstanceNotFoundFault #-}
{-# DEPRECATED _DBInstanceNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The request would result in the user exceeding the allowed amount of storage available across all DB instances.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig "StorageQuotaExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _StorageQuotaExceededFault #-}
{-# DEPRECATED _StorageQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified proxy name must be unique for all proxies owned by your AWS account in the specified AWS Region.
_DBProxyAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBProxyAlreadyExistsFault
  = Core._MatchServiceError mkServiceConfig
      "DBProxyTargetExistsFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBProxyAlreadyExistsFault #-}
{-# DEPRECATED _DBProxyAlreadyExistsFault "Use generic-lens or generic-optics instead"  #-}

-- | No automated backup for this DB instance was found.
_DBInstanceAutomatedBackupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBInstanceAutomatedBackupNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "DBInstanceAutomatedBackupNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBInstanceAutomatedBackupNotFoundFault #-}
{-# DEPRECATED _DBInstanceAutomatedBackupNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | The state of the DB snapshot doesn't allow deletion.
_InvalidDBSnapshotStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSnapshotStateFault
  = Core._MatchServiceError mkServiceConfig "InvalidDBSnapshotState"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBSnapshotStateFault #-}
{-# DEPRECATED _InvalidDBSnapshotStateFault "Use generic-lens or generic-optics instead"  #-}

-- | The DB subnet group cannot be deleted because it's in use.
_InvalidDBSubnetGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupStateFault
  = Core._MatchServiceError mkServiceConfig
      "InvalidDBSubnetGroupStateFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDBSubnetGroupStateFault #-}
{-# DEPRECATED _InvalidDBSubnetGroupStateFault "Use generic-lens or generic-optics instead"  #-}

-- | 
_GlobalClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GlobalClusterQuotaExceededFault
  = Core._MatchServiceError mkServiceConfig
      "GlobalClusterQuotaExceededFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _GlobalClusterQuotaExceededFault #-}
{-# DEPRECATED _GlobalClusterQuotaExceededFault "Use generic-lens or generic-optics instead"  #-}

-- | The specified custom endpoint doesn't exist.
_DBClusterEndpointNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "DBClusterEndpointNotFoundFault"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBClusterEndpointNotFoundFault #-}
{-# DEPRECATED _DBClusterEndpointNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | @InstallationMediaID@ doesn't refer to an existing installation medium.
_InstallationMediaNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstallationMediaNotFoundFault
  = Core._MatchServiceError mkServiceConfig
      "InstallationMediaNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _InstallationMediaNotFoundFault #-}
{-# DEPRECATED _InstallationMediaNotFoundFault "Use generic-lens or generic-optics instead"  #-}

-- | Subnets in the DB subnet group should cover at least two Availability Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs
  = Core._MatchServiceError mkServiceConfig
      "DBSubnetGroupDoesNotCoverEnoughAZs"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DBSubnetGroupDoesNotCoverEnoughAZs #-}
{-# DEPRECATED _DBSubnetGroupDoesNotCoverEnoughAZs "Use generic-lens or generic-optics instead"  #-}

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse
  = Core._MatchServiceError mkServiceConfig "SubnetAlreadyInUse"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SubnetAlreadyInUse #-}
{-# DEPRECATED _SubnetAlreadyInUse "Use generic-lens or generic-optics instead"  #-}

-- | The specified IAM role Amazon Resource Name (ARN) isn't associated with the specified DB cluster.
_DBClusterRoleNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleNotFoundFault
  = Core._MatchServiceError mkServiceConfig "DBClusterRoleNotFound"
      Core.. Core.hasStatues 404
{-# INLINEABLE _DBClusterRoleNotFoundFault #-}
{-# DEPRECATED _DBClusterRoleNotFoundFault "Use generic-lens or generic-optics instead"  #-}
