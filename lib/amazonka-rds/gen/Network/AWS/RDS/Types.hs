-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types
  ( -- * Service configuration
    rdsService,

    -- * Errors

    -- * ActivityStreamMode
    ActivityStreamMode (..),

    -- * ActivityStreamStatus
    ActivityStreamStatus (..),

    -- * ApplyMethod
    ApplyMethod (..),

    -- * AuthScheme
    AuthScheme (..),

    -- * DBProxyStatus
    DBProxyStatus (..),

    -- * EngineFamily
    EngineFamily (..),

    -- * IAMAuthMode
    IAMAuthMode (..),

    -- * ReplicaMode
    ReplicaMode (..),

    -- * SourceType
    SourceType (..),

    -- * TargetHealthReason
    TargetHealthReason (..),

    -- * TargetState
    TargetState (..),

    -- * TargetType
    TargetType (..),

    -- * WriteForwardingStatus
    WriteForwardingStatus (..),

    -- * AccountQuota
    AccountQuota (..),
    mkAccountQuota,
    aqMax,
    aqUsed,
    aqAccountQuotaName,

    -- * AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,

    -- * AvailableProcessorFeature
    AvailableProcessorFeature (..),
    mkAvailableProcessorFeature,
    apfName,
    apfDefaultValue,
    apfAllowedValues,

    -- * Certificate
    Certificate (..),
    mkCertificate,
    cCertificateType,
    cCustomerOverride,
    cCertificateARN,
    cCustomerOverrideValidTill,
    cValidTill,
    cCertificateIdentifier,
    cThumbprint,
    cValidFrom,

    -- * CharacterSet
    CharacterSet (..),
    mkCharacterSet,
    csCharacterSetName,
    csCharacterSetDescription,

    -- * CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (..),
    mkCloudwatchLogsExportConfiguration,
    clecDisableLogTypes,
    clecEnableLogTypes,

    -- * ConnectionPoolConfiguration
    ConnectionPoolConfiguration (..),
    mkConnectionPoolConfiguration,
    cpcMaxIdleConnectionsPercent,
    cpcSessionPinningFilters,
    cpcMaxConnectionsPercent,
    cpcConnectionBorrowTimeout,
    cpcInitQuery,

    -- * ConnectionPoolConfigurationInfo
    ConnectionPoolConfigurationInfo (..),
    mkConnectionPoolConfigurationInfo,
    cpciMaxIdleConnectionsPercent,
    cpciSessionPinningFilters,
    cpciMaxConnectionsPercent,
    cpciConnectionBorrowTimeout,
    cpciInitQuery,

    -- * CustomAvailabilityZone
    CustomAvailabilityZone (..),
    mkCustomAvailabilityZone,
    cazVPNDetails,
    cazCustomAvailabilityZoneName,
    cazCustomAvailabilityZoneId,
    cazCustomAvailabilityZoneStatus,

    -- * DBCluster
    DBCluster (..),
    mkDBCluster,
    dcBacktrackConsumedChangeRecords,
    dcEngineVersion,
    dcStatus,
    dcDeletionProtection,
    dcStorageEncrypted,
    dcDBClusterIdentifier,
    dcDBClusterMembers,
    dcReadReplicaIdentifiers,
    dcReplicationSourceIdentifier,
    dcActivityStreamKinesisStreamName,
    dcHostedZoneId,
    dcDBClusterParameterGroup,
    dcMasterUsername,
    dcIAMDatabaseAuthenticationEnabled,
    dcGlobalWriteForwardingRequested,
    dcEarliestBacktrackTime,
    dcBacktrackWindow,
    dcTagList,
    dcDBClusterResourceId,
    dcEarliestRestorableTime,
    dcCustomEndpoints,
    dcEngine,
    dcHTTPEndpointEnabled,
    dcDBClusterARN,
    dcCloneGroupId,
    dcLatestRestorableTime,
    dcCrossAccountClone,
    dcCapacity,
    dcPreferredMaintenanceWindow,
    dcAvailabilityZones,
    dcCharacterSetName,
    dcKMSKeyId,
    dcPreferredBackupWindow,
    dcAssociatedRoles,
    dcVPCSecurityGroups,
    dcBackupRetentionPeriod,
    dcDBSubnetGroup,
    dcActivityStreamMode,
    dcDatabaseName,
    dcMultiAZ,
    dcEngineMode,
    dcEnabledCloudwatchLogsExports,
    dcActivityStreamStatus,
    dcAllocatedStorage,
    dcCopyTagsToSnapshot,
    dcClusterCreateTime,
    dcEndpoint,
    dcScalingConfigurationInfo,
    dcActivityStreamKMSKeyId,
    dcPercentProgress,
    dcReaderEndpoint,
    dcGlobalWriteForwardingStatus,
    dcPort,
    dcDomainMemberships,
    dcDBClusterOptionGroupMemberships,

    -- * DBClusterBacktrack
    DBClusterBacktrack (..),
    mkDBClusterBacktrack,
    dcbStatus,
    dcbBacktrackIdentifier,
    dcbBacktrackTo,
    dcbDBClusterIdentifier,
    dcbBacktrackedFrom,
    dcbBacktrackRequestCreationTime,

    -- * DBClusterEndpoint
    DBClusterEndpoint (..),
    mkDBClusterEndpoint,
    dceStatus,
    dceDBClusterIdentifier,
    dceDBClusterEndpointARN,
    dceCustomEndpointType,
    dceStaticMembers,
    dceEndpointType,
    dceDBClusterEndpointIdentifier,
    dceEndpoint,
    dceDBClusterEndpointResourceIdentifier,
    dceExcludedMembers,

    -- * DBClusterMember
    DBClusterMember (..),
    mkDBClusterMember,
    dcmPromotionTier,
    dcmDBInstanceIdentifier,
    dcmIsClusterWriter,
    dcmDBClusterParameterGroupStatus,

    -- * DBClusterOptionGroupStatus
    DBClusterOptionGroupStatus (..),
    mkDBClusterOptionGroupStatus,
    dcogsStatus,
    dcogsDBClusterOptionGroupName,

    -- * DBClusterParameterGroup
    DBClusterParameterGroup (..),
    mkDBClusterParameterGroup,
    dcpgDBClusterParameterGroupARN,
    dcpgDBParameterGroupFamily,
    dcpgDBClusterParameterGroupName,
    dcpgDescription,

    -- * DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage (..),
    mkDBClusterParameterGroupNameMessage,
    dcpgnmDBClusterParameterGroupName,

    -- * DBClusterRole
    DBClusterRole (..),
    mkDBClusterRole,
    dcrStatus,
    dcrFeatureName,
    dcrRoleARN,

    -- * DBClusterSnapshot
    DBClusterSnapshot (..),
    mkDBClusterSnapshot,
    dcsEngineVersion,
    dcsStatus,
    dcsStorageEncrypted,
    dcsDBClusterIdentifier,
    dcsMasterUsername,
    dcsIAMDatabaseAuthenticationEnabled,
    dcsDBClusterSnapshotARN,
    dcsVPCId,
    dcsTagList,
    dcsDBClusterSnapshotIdentifier,
    dcsEngine,
    dcsLicenseModel,
    dcsAvailabilityZones,
    dcsSnapshotType,
    dcsKMSKeyId,
    dcsSnapshotCreateTime,
    dcsAllocatedStorage,
    dcsSourceDBClusterSnapshotARN,
    dcsClusterCreateTime,
    dcsPercentProgress,
    dcsPort,

    -- * DBClusterSnapshotAttribute
    DBClusterSnapshotAttribute (..),
    mkDBClusterSnapshotAttribute,
    dcsaAttributeValues,
    dcsaAttributeName,

    -- * DBClusterSnapshotAttributesResult
    DBClusterSnapshotAttributesResult (..),
    mkDBClusterSnapshotAttributesResult,
    dcsarDBClusterSnapshotIdentifier,
    dcsarDBClusterSnapshotAttributes,

    -- * DBEngineVersion
    DBEngineVersion (..),
    mkDBEngineVersion,
    devEngineVersion,
    devStatus,
    devDBEngineVersionDescription,
    devSupportedEngineModes,
    devDefaultCharacterSet,
    devEngine,
    devDBParameterGroupFamily,
    devSupportedCharacterSets,
    devDBEngineDescription,
    devSupportsGlobalDatabases,
    devValidUpgradeTarget,
    devSupportsParallelQuery,
    devSupportedNcharCharacterSets,
    devSupportsLogExportsToCloudwatchLogs,
    devSupportsReadReplica,
    devSupportedFeatureNames,
    devSupportedTimezones,
    devExportableLogTypes,

    -- * DBInstance
    DBInstance (..),
    mkDBInstance,
    diEngineVersion,
    diDBSecurityGroups,
    diDeletionProtection,
    diStorageEncrypted,
    diDBClusterIdentifier,
    diPubliclyAccessible,
    diAutoMinorVersionUpgrade,
    diDBInstanceARN,
    diMasterUsername,
    diReadReplicaDBInstanceIdentifiers,
    diIAMDatabaseAuthenticationEnabled,
    diMonitoringRoleARN,
    diIOPS,
    diInstanceCreateTime,
    diTagList,
    diReadReplicaSourceDBInstanceIdentifier,
    diReplicaMode,
    diMonitoringInterval,
    diEngine,
    diProcessorFeatures,
    diLatestRestorableTime,
    diDBInstanceClass,
    diPromotionTier,
    diLicenseModel,
    diPreferredMaintenanceWindow,
    diPerformanceInsightsRetentionPeriod,
    diCACertificateIdentifier,
    diDBInstanceIdentifier,
    diCharacterSetName,
    diMaxAllocatedStorage,
    diKMSKeyId,
    diPreferredBackupWindow,
    diAssociatedRoles,
    diAvailabilityZone,
    diVPCSecurityGroups,
    diBackupRetentionPeriod,
    diNcharCharacterSetName,
    diPerformanceInsightsKMSKeyId,
    diDBSubnetGroup,
    diMultiAZ,
    diListenerEndpoint,
    diOptionGroupMemberships,
    diEnabledCloudwatchLogsExports,
    diEnhancedMonitoringResourceARN,
    diSecondaryAvailabilityZone,
    diPerformanceInsightsEnabled,
    diAllocatedStorage,
    diDBiResourceId,
    diDBParameterGroups,
    diCopyTagsToSnapshot,
    diTimezone,
    diTDECredentialARN,
    diEndpoint,
    diDBInstanceStatus,
    diDBInstancePort,
    diPendingModifiedValues,
    diReadReplicaDBClusterIdentifiers,
    diStorageType,
    diStatusInfos,
    diDomainMemberships,
    diDBName,

    -- * DBInstanceAutomatedBackup
    DBInstanceAutomatedBackup (..),
    mkDBInstanceAutomatedBackup,
    diabRestoreWindow,
    diabEngineVersion,
    diabStatus,
    diabDBInstanceARN,
    diabMasterUsername,
    diabIAMDatabaseAuthenticationEnabled,
    diabIOPS,
    diabVPCId,
    diabInstanceCreateTime,
    diabEngine,
    diabEncrypted,
    diabLicenseModel,
    diabDBInstanceIdentifier,
    diabKMSKeyId,
    diabAvailabilityZone,
    diabRegion,
    diabAllocatedStorage,
    diabDBiResourceId,
    diabOptionGroupName,
    diabTimezone,
    diabTDECredentialARN,
    diabPort,
    diabStorageType,

    -- * DBInstanceRole
    DBInstanceRole (..),
    mkDBInstanceRole,
    dirStatus,
    dirFeatureName,
    dirRoleARN,

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    mkDBInstanceStatusInfo,
    disiStatus,
    disiNormal,
    disiStatusType,
    disiMessage,

    -- * DBParameterGroup
    DBParameterGroup (..),
    mkDBParameterGroup,
    dpgDBParameterGroupARN,
    dpgDBParameterGroupFamily,
    dpgDBParameterGroupName,
    dpgDescription,

    -- * DBParameterGroupNameMessage
    DBParameterGroupNameMessage (..),
    mkDBParameterGroupNameMessage,
    dpgnmDBParameterGroupName,

    -- * DBParameterGroupStatus
    DBParameterGroupStatus (..),
    mkDBParameterGroupStatus,
    dpgsDBParameterGroupName,
    dpgsParameterApplyStatus,

    -- * DBProxy
    DBProxy (..),
    mkDBProxy,
    dpStatus,
    dpDBProxyARN,
    dpDebugLogging,
    dpVPCSubnetIds,
    dpEngineFamily,
    dpAuth,
    dpRequireTLS,
    dpIdleClientTimeout,
    dpUpdatedDate,
    dpCreatedDate,
    dpVPCSecurityGroupIds,
    dpDBProxyName,
    dpEndpoint,
    dpRoleARN,

    -- * DBProxyTarget
    DBProxyTarget (..),
    mkDBProxyTarget,
    dptTargetARN,
    dptTargetHealth,
    dptTrackedClusterId,
    dptRDSResourceId,
    dptType,
    dptEndpoint,
    dptPort,

    -- * DBProxyTargetGroup
    DBProxyTargetGroup (..),
    mkDBProxyTargetGroup,
    dptgStatus,
    dptgConnectionPoolConfig,
    dptgTargetGroupARN,
    dptgUpdatedDate,
    dptgCreatedDate,
    dptgDBProxyName,
    dptgTargetGroupName,
    dptgIsDefault,

    -- * DBSecurityGroup
    DBSecurityGroup (..),
    mkDBSecurityGroup,
    dbsgVPCId,
    dbsgOwnerId,
    dbsgDBSecurityGroupARN,
    dbsgIPRanges,
    dbsgDBSecurityGroupName,
    dbsgEC2SecurityGroups,
    dbsgDBSecurityGroupDescription,

    -- * DBSecurityGroupMembership
    DBSecurityGroupMembership (..),
    mkDBSecurityGroupMembership,
    dsgmStatus,
    dsgmDBSecurityGroupName,

    -- * DBSnapshot
    DBSnapshot (..),
    mkDBSnapshot,
    dsEngineVersion,
    dsStatus,
    dsDBSnapshotARN,
    dsMasterUsername,
    dsSourceRegion,
    dsIAMDatabaseAuthenticationEnabled,
    dsIOPS,
    dsVPCId,
    dsInstanceCreateTime,
    dsTagList,
    dsEngine,
    dsEncrypted,
    dsDBSnapshotIdentifier,
    dsProcessorFeatures,
    dsLicenseModel,
    dsSourceDBSnapshotIdentifier,
    dsSnapshotType,
    dsDBInstanceIdentifier,
    dsKMSKeyId,
    dsAvailabilityZone,
    dsSnapshotCreateTime,
    dsAllocatedStorage,
    dsDBiResourceId,
    dsOptionGroupName,
    dsTimezone,
    dsTDECredentialARN,
    dsPercentProgress,
    dsPort,
    dsStorageType,

    -- * DBSnapshotAttribute
    DBSnapshotAttribute (..),
    mkDBSnapshotAttribute,
    dsaAttributeValues,
    dsaAttributeName,

    -- * DBSnapshotAttributesResult
    DBSnapshotAttributesResult (..),
    mkDBSnapshotAttributesResult,
    dsarDBSnapshotIdentifier,
    dsarDBSnapshotAttributes,

    -- * DBSubnetGroup
    DBSubnetGroup (..),
    mkDBSubnetGroup,
    dsgDBSubnetGroupName,
    dsgVPCId,
    dsgSubnets,
    dsgDBSubnetGroupDescription,
    dsgDBSubnetGroupARN,
    dsgSubnetGroupStatus,

    -- * DescribeDBLogFilesDetails
    DescribeDBLogFilesDetails (..),
    mkDescribeDBLogFilesDetails,
    ddlfdLastWritten,
    ddlfdSize,
    ddlfdLogFileName,

    -- * DomainMembership
    DomainMembership (..),
    mkDomainMembership,
    dmStatus,
    dmFQDN,
    dmDomain,
    dmIAMRoleName,

    -- * DoubleRange
    DoubleRange (..),
    mkDoubleRange,
    drTo,
    drFrom,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    mkEC2SecurityGroup,
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,
    esgEC2SecurityGroupId,

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eHostedZoneId,
    eAddress,
    ePort,

    -- * EngineDefaults
    EngineDefaults (..),
    mkEngineDefaults,
    edDBParameterGroupFamily,
    edMarker,
    edParameters,

    -- * Event
    Event (..),
    mkEvent,
    eSourceType,
    eSourceARN,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,

    -- * EventCategoriesMap
    EventCategoriesMap (..),
    mkEventCategoriesMap,
    ecmSourceType,
    ecmEventCategories,

    -- * EventSubscription
    EventSubscription (..),
    mkEventSubscription,
    esStatus,
    esCustomerAWSId,
    esCustSubscriptionId,
    esSNSTopicARN,
    esEventSubscriptionARN,
    esEnabled,
    esSourceType,
    esSubscriptionCreationTime,
    esEventCategoriesList,
    esSourceIdsList,

    -- * ExportTask
    ExportTask (..),
    mkExportTask,
    etTotalExtractedDataInGB,
    etStatus,
    etIAMRoleARN,
    etSourceARN,
    etExportOnly,
    etTaskStartTime,
    etWarningMessage,
    etSnapshotTime,
    etKMSKeyId,
    etTaskEndTime,
    etExportTaskIdentifier,
    etS3Prefix,
    etPercentProgress,
    etS3Bucket,
    etFailureCause,

    -- * Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,

    -- * GlobalCluster
    GlobalCluster (..),
    mkGlobalCluster,
    gcEngineVersion,
    gcStatus,
    gcDeletionProtection,
    gcStorageEncrypted,
    gcGlobalClusterIdentifier,
    gcEngine,
    gcGlobalClusterARN,
    gcDatabaseName,
    gcGlobalClusterMembers,
    gcGlobalClusterResourceId,

    -- * GlobalClusterMember
    GlobalClusterMember (..),
    mkGlobalClusterMember,
    gcmReaders,
    gcmDBClusterARN,
    gcmIsWriter,
    gcmGlobalWriteForwardingStatus,

    -- * IPRange
    IPRange (..),
    mkIPRange,
    irStatus,
    irCIdRIP,

    -- * InstallationMedia
    InstallationMedia (..),
    mkInstallationMedia,
    imEngineVersion,
    imStatus,
    imInstallationMediaId,
    imEngineInstallationMediaPath,
    imEngine,
    imOSInstallationMediaPath,
    imCustomAvailabilityZoneId,
    imFailureCause,

    -- * InstallationMediaFailureCause
    InstallationMediaFailureCause (..),
    mkInstallationMediaFailureCause,
    imfcMessage,

    -- * MinimumEngineVersionPerAllowedValue
    MinimumEngineVersionPerAllowedValue (..),
    mkMinimumEngineVersionPerAllowedValue,
    mevpavMinimumEngineVersion,
    mevpavAllowedValue,

    -- * Option
    Option (..),
    mkOption,
    oOptionName,
    oPermanent,
    oPersistent,
    oOptionDescription,
    oOptionSettings,
    oVPCSecurityGroupMemberships,
    oDBSecurityGroupMemberships,
    oOptionVersion,
    oPort,

    -- * OptionConfiguration
    OptionConfiguration (..),
    mkOptionConfiguration,
    ocOptionSettings,
    ocVPCSecurityGroupMemberships,
    ocDBSecurityGroupMemberships,
    ocOptionVersion,
    ocPort,
    ocOptionName,

    -- * OptionGroup
    OptionGroup (..),
    mkOptionGroup,
    ogOptionGroupDescription,
    ogVPCId,
    ogAllowsVPCAndNonVPCInstanceMemberships,
    ogEngineName,
    ogOptionGroupARN,
    ogMajorEngineVersion,
    ogOptions,
    ogOptionGroupName,

    -- * OptionGroupMembership
    OptionGroupMembership (..),
    mkOptionGroupMembership,
    ogmStatus,
    ogmOptionGroupName,

    -- * OptionGroupOption
    OptionGroupOption (..),
    mkOptionGroupOption,
    ogoMinimumRequiredMinorEngineVersion,
    ogoOptionsConflictsWith,
    ogoPermanent,
    ogoPersistent,
    ogoOptionGroupOptionVersions,
    ogoEngineName,
    ogoMajorEngineVersion,
    ogoName,
    ogoSupportsOptionVersionDowngrade,
    ogoDefaultPort,
    ogoOptionGroupOptionSettings,
    ogoRequiresAutoMinorEngineVersionUpgrade,
    ogoPortRequired,
    ogoDescription,
    ogoOptionsDependedOn,
    ogoVPCOnly,

    -- * OptionGroupOptionSetting
    OptionGroupOptionSetting (..),
    mkOptionGroupOptionSetting,
    ogosApplyType,
    ogosMinimumEngineVersionPerAllowedValue,
    ogosSettingName,
    ogosDefaultValue,
    ogosIsModifiable,
    ogosSettingDescription,
    ogosAllowedValues,
    ogosIsRequired,

    -- * OptionSetting
    OptionSetting (..),
    mkOptionSetting,
    osIsCollection,
    osApplyType,
    osValue,
    osName,
    osDefaultValue,
    osIsModifiable,
    osDataType,
    osAllowedValues,
    osDescription,

    -- * OptionVersion
    OptionVersion (..),
    mkOptionVersion,
    ovVersion,
    ovIsDefault,

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    mkOrderableDBInstanceOption,
    odioEngineVersion,
    odioMinIOPSPerGib,
    odioSupportsIAMDatabaseAuthentication,
    odioMinIOPSPerDBInstance,
    odioMultiAZCapable,
    odioMaxStorageSize,
    odioSupportedEngineModes,
    odioAvailabilityZoneGroup,
    odioAvailableProcessorFeatures,
    odioEngine,
    odioMinStorageSize,
    odioOutpostCapable,
    odioSupportsIOPS,
    odioSupportsKerberosAuthentication,
    odioSupportsPerformanceInsights,
    odioDBInstanceClass,
    odioSupportsGlobalDatabases,
    odioLicenseModel,
    odioAvailabilityZones,
    odioSupportsStorageAutoscaling,
    odioSupportsStorageEncryption,
    odioReadReplicaCapable,
    odioMaxIOPSPerGib,
    odioVPC,
    odioSupportsEnhancedMonitoring,
    odioMaxIOPSPerDBInstance,
    odioStorageType,

    -- * Outpost
    Outpost (..),
    mkOutpost,
    oARN,

    -- * Parameter
    Parameter (..),
    mkParameter,
    pApplyType,
    pParameterValue,
    pSupportedEngineModes,
    pApplyMethod,
    pMinimumEngineVersion,
    pSource,
    pIsModifiable,
    pDataType,
    pAllowedValues,
    pParameterName,
    pDescription,

    -- * PendingCloudwatchLogsExports
    PendingCloudwatchLogsExports (..),
    mkPendingCloudwatchLogsExports,
    pcleLogTypesToEnable,
    pcleLogTypesToDisable,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    mkPendingMaintenanceAction,
    pmaAutoAppliedAfterDate,
    pmaAction,
    pmaOptInStatus,
    pmaDescription,
    pmaForcedApplyDate,
    pmaCurrentApplyDate,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    mkPendingModifiedValues,
    pmvEngineVersion,
    pmvMasterUserPassword,
    pmvDBSubnetGroupName,
    pmvIOPS,
    pmvProcessorFeatures,
    pmvDBInstanceClass,
    pmvLicenseModel,
    pmvCACertificateIdentifier,
    pmvDBInstanceIdentifier,
    pmvPendingCloudwatchLogsExports,
    pmvBackupRetentionPeriod,
    pmvMultiAZ,
    pmvAllocatedStorage,
    pmvPort,
    pmvStorageType,

    -- * ProcessorFeature
    ProcessorFeature (..),
    mkProcessorFeature,
    pfValue,
    pfName,

    -- * Range
    Range (..),
    mkRange,
    rTo,
    rFrom,
    rStep,

    -- * RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- * ReservedDBInstance
    ReservedDBInstance (..),
    mkReservedDBInstance,
    rdiDBInstanceCount,
    rdiState,
    rdiCurrencyCode,
    rdiStartTime,
    rdiProductDescription,
    rdiLeaseId,
    rdiReservedDBInstanceId,
    rdiReservedDBInstanceARN,
    rdiDBInstanceClass,
    rdiMultiAZ,
    rdiReservedDBInstancesOfferingId,
    rdiRecurringCharges,
    rdiOfferingType,
    rdiUsagePrice,
    rdiFixedPrice,
    rdiDuration,

    -- * ReservedDBInstancesOffering
    ReservedDBInstancesOffering (..),
    mkReservedDBInstancesOffering,
    rdioCurrencyCode,
    rdioProductDescription,
    rdioDBInstanceClass,
    rdioMultiAZ,
    rdioReservedDBInstancesOfferingId,
    rdioRecurringCharges,
    rdioOfferingType,
    rdioUsagePrice,
    rdioFixedPrice,
    rdioDuration,

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    mkResourcePendingMaintenanceActions,
    rpmaPendingMaintenanceActionDetails,
    rpmaResourceIdentifier,

    -- * RestoreWindow
    RestoreWindow (..),
    mkRestoreWindow,
    rwLatestTime,
    rwEarliestTime,

    -- * ScalingConfiguration
    ScalingConfiguration (..),
    mkScalingConfiguration,
    scSecondsUntilAutoPause,
    scTimeoutAction,
    scAutoPause,
    scMaxCapacity,
    scMinCapacity,

    -- * ScalingConfigurationInfo
    ScalingConfigurationInfo (..),
    mkScalingConfigurationInfo,
    sciSecondsUntilAutoPause,
    sciTimeoutAction,
    sciAutoPause,
    sciMaxCapacity,
    sciMinCapacity,

    -- * SourceRegion
    SourceRegion (..),
    mkSourceRegion,
    srStatus,
    srRegionName,
    srEndpoint,

    -- * Subnet
    Subnet (..),
    mkSubnet,
    sSubnetStatus,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,
    sSubnetOutpost,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TargetHealth
    TargetHealth (..),
    mkTargetHealth,
    thState,
    thReason,
    thDescription,

    -- * Timezone
    Timezone (..),
    mkTimezone,
    tTimezoneName,

    -- * UpgradeTarget
    UpgradeTarget (..),
    mkUpgradeTarget,
    utEngineVersion,
    utIsMajorVersionUpgrade,
    utEngine,
    utAutoUpgrade,
    utDescription,

    -- * UserAuthConfig
    UserAuthConfig (..),
    mkUserAuthConfig,
    uacIAMAuth,
    uacUserName,
    uacAuthScheme,
    uacSecretARN,
    uacDescription,

    -- * UserAuthConfigInfo
    UserAuthConfigInfo (..),
    mkUserAuthConfigInfo,
    uaciIAMAuth,
    uaciUserName,
    uaciAuthScheme,
    uaciSecretARN,
    uaciDescription,

    -- * VPCSecurityGroupMembership
    VPCSecurityGroupMembership (..),
    mkVPCSecurityGroupMembership,
    vsgmStatus,
    vsgmVPCSecurityGroupId,

    -- * VPNDetails
    VPNDetails (..),
    mkVPNDetails,
    vdVPNName,
    vdVPNTunnelOriginatorIP,
    vdVPNId,
    vdVPNState,
    vdVPNPSK,
    vdVPNGatewayIP,

    -- * ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (..),
    mkValidDBInstanceModificationsMessage,
    vdimmValidProcessorFeatures,
    vdimmStorage,

    -- * ValidStorageOptions
    ValidStorageOptions (..),
    mkValidStorageOptions,
    vsoStorageSize,
    vsoProvisionedIOPS,
    vsoIOPSToStorageRatio,
    vsoSupportsStorageAutoscaling,
    vsoStorageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.AccountQuota
import Network.AWS.RDS.Types.ActivityStreamMode
import Network.AWS.RDS.Types.ActivityStreamStatus
import Network.AWS.RDS.Types.ApplyMethod
import Network.AWS.RDS.Types.AuthScheme
import Network.AWS.RDS.Types.AvailabilityZone
import Network.AWS.RDS.Types.AvailableProcessorFeature
import Network.AWS.RDS.Types.Certificate
import Network.AWS.RDS.Types.CharacterSet
import Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration
import Network.AWS.RDS.Types.ConnectionPoolConfiguration
import Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo
import Network.AWS.RDS.Types.CustomAvailabilityZone
import Network.AWS.RDS.Types.DBCluster
import Network.AWS.RDS.Types.DBClusterBacktrack
import Network.AWS.RDS.Types.DBClusterEndpoint
import Network.AWS.RDS.Types.DBClusterMember
import Network.AWS.RDS.Types.DBClusterOptionGroupStatus
import Network.AWS.RDS.Types.DBClusterParameterGroup
import Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage
import Network.AWS.RDS.Types.DBClusterRole
import Network.AWS.RDS.Types.DBClusterSnapshot
import Network.AWS.RDS.Types.DBClusterSnapshotAttribute
import Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
import Network.AWS.RDS.Types.DBEngineVersion
import Network.AWS.RDS.Types.DBInstance
import Network.AWS.RDS.Types.DBInstanceAutomatedBackup
import Network.AWS.RDS.Types.DBInstanceRole
import Network.AWS.RDS.Types.DBInstanceStatusInfo
import Network.AWS.RDS.Types.DBParameterGroup
import Network.AWS.RDS.Types.DBParameterGroupNameMessage
import Network.AWS.RDS.Types.DBParameterGroupStatus
import Network.AWS.RDS.Types.DBProxy
import Network.AWS.RDS.Types.DBProxyStatus
import Network.AWS.RDS.Types.DBProxyTarget
import Network.AWS.RDS.Types.DBProxyTargetGroup
import Network.AWS.RDS.Types.DBSecurityGroup
import Network.AWS.RDS.Types.DBSecurityGroupMembership
import Network.AWS.RDS.Types.DBSnapshot
import Network.AWS.RDS.Types.DBSnapshotAttribute
import Network.AWS.RDS.Types.DBSnapshotAttributesResult
import Network.AWS.RDS.Types.DBSubnetGroup
import Network.AWS.RDS.Types.DescribeDBLogFilesDetails
import Network.AWS.RDS.Types.DomainMembership
import Network.AWS.RDS.Types.DoubleRange
import Network.AWS.RDS.Types.EC2SecurityGroup
import Network.AWS.RDS.Types.Endpoint
import Network.AWS.RDS.Types.EngineDefaults
import Network.AWS.RDS.Types.EngineFamily
import Network.AWS.RDS.Types.Event
import Network.AWS.RDS.Types.EventCategoriesMap
import Network.AWS.RDS.Types.EventSubscription
import Network.AWS.RDS.Types.ExportTask
import Network.AWS.RDS.Types.Filter
import Network.AWS.RDS.Types.GlobalCluster
import Network.AWS.RDS.Types.GlobalClusterMember
import Network.AWS.RDS.Types.IAMAuthMode
import Network.AWS.RDS.Types.IPRange
import Network.AWS.RDS.Types.InstallationMedia
import Network.AWS.RDS.Types.InstallationMediaFailureCause
import Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
import Network.AWS.RDS.Types.Option
import Network.AWS.RDS.Types.OptionConfiguration
import Network.AWS.RDS.Types.OptionGroup
import Network.AWS.RDS.Types.OptionGroupMembership
import Network.AWS.RDS.Types.OptionGroupOption
import Network.AWS.RDS.Types.OptionGroupOptionSetting
import Network.AWS.RDS.Types.OptionSetting
import Network.AWS.RDS.Types.OptionVersion
import Network.AWS.RDS.Types.OrderableDBInstanceOption
import Network.AWS.RDS.Types.Outpost
import Network.AWS.RDS.Types.Parameter
import Network.AWS.RDS.Types.PendingCloudwatchLogsExports
import Network.AWS.RDS.Types.PendingMaintenanceAction
import Network.AWS.RDS.Types.PendingModifiedValues
import Network.AWS.RDS.Types.ProcessorFeature
import Network.AWS.RDS.Types.Range
import Network.AWS.RDS.Types.RecurringCharge
import Network.AWS.RDS.Types.ReplicaMode
import Network.AWS.RDS.Types.ReservedDBInstance
import Network.AWS.RDS.Types.ReservedDBInstancesOffering
import Network.AWS.RDS.Types.ResourcePendingMaintenanceActions
import Network.AWS.RDS.Types.RestoreWindow
import Network.AWS.RDS.Types.ScalingConfiguration
import Network.AWS.RDS.Types.ScalingConfigurationInfo
import Network.AWS.RDS.Types.SourceRegion
import Network.AWS.RDS.Types.SourceType
import Network.AWS.RDS.Types.Subnet
import Network.AWS.RDS.Types.Tag
import Network.AWS.RDS.Types.TargetHealth
import Network.AWS.RDS.Types.TargetHealthReason
import Network.AWS.RDS.Types.TargetState
import Network.AWS.RDS.Types.TargetType
import Network.AWS.RDS.Types.Timezone
import Network.AWS.RDS.Types.UpgradeTarget
import Network.AWS.RDS.Types.UserAuthConfig
import Network.AWS.RDS.Types.UserAuthConfigInfo
import Network.AWS.RDS.Types.VPCSecurityGroupMembership
import Network.AWS.RDS.Types.VPNDetails
import Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
import Network.AWS.RDS.Types.ValidStorageOptions
import Network.AWS.RDS.Types.WriteForwardingStatus
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-10-31@ of the Amazon Relational Database Service SDK configuration.
rdsService :: Lude.Service
rdsService =
  Lude.Service
    { Lude._svcAbbrev = "RDS",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "rds",
      Lude._svcVersion = "2014-10-31",
      Lude._svcEndpoint = Lude.defaultEndpoint rdsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "RDS",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
