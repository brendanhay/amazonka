{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types
  ( -- * Service Configuration
    rds,

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
    AccountQuota,
    accountQuota,
    aqMax,
    aqUsed,
    aqAccountQuotaName,

    -- * AvailabilityZone
    AvailabilityZone,
    availabilityZone,
    azName,

    -- * AvailableProcessorFeature
    AvailableProcessorFeature,
    availableProcessorFeature,
    apfName,
    apfDefaultValue,
    apfAllowedValues,

    -- * Certificate
    Certificate,
    certificate,
    cCertificateType,
    cCustomerOverride,
    cCertificateARN,
    cCustomerOverrideValidTill,
    cValidTill,
    cCertificateIdentifier,
    cThumbprint,
    cValidFrom,

    -- * CharacterSet
    CharacterSet,
    characterSet,
    csCharacterSetName,
    csCharacterSetDescription,

    -- * CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration,
    cloudwatchLogsExportConfiguration,
    clecDisableLogTypes,
    clecEnableLogTypes,

    -- * ConnectionPoolConfiguration
    ConnectionPoolConfiguration,
    connectionPoolConfiguration,
    cpcMaxIdleConnectionsPercent,
    cpcSessionPinningFilters,
    cpcMaxConnectionsPercent,
    cpcConnectionBorrowTimeout,
    cpcInitQuery,

    -- * ConnectionPoolConfigurationInfo
    ConnectionPoolConfigurationInfo,
    connectionPoolConfigurationInfo,
    cpciMaxIdleConnectionsPercent,
    cpciSessionPinningFilters,
    cpciMaxConnectionsPercent,
    cpciConnectionBorrowTimeout,
    cpciInitQuery,

    -- * CustomAvailabilityZone
    CustomAvailabilityZone,
    customAvailabilityZone,
    cazVPNDetails,
    cazCustomAvailabilityZoneName,
    cazCustomAvailabilityZoneId,
    cazCustomAvailabilityZoneStatus,

    -- * DBCluster
    DBCluster,
    dbCluster,
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
    DBClusterBacktrack,
    dbClusterBacktrack,
    dcbStatus,
    dcbBacktrackIdentifier,
    dcbBacktrackTo,
    dcbDBClusterIdentifier,
    dcbBacktrackedFrom,
    dcbBacktrackRequestCreationTime,

    -- * DBClusterEndpoint
    DBClusterEndpoint,
    dbClusterEndpoint,
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
    DBClusterMember,
    dbClusterMember,
    dcmPromotionTier,
    dcmDBInstanceIdentifier,
    dcmIsClusterWriter,
    dcmDBClusterParameterGroupStatus,

    -- * DBClusterOptionGroupStatus
    DBClusterOptionGroupStatus,
    dbClusterOptionGroupStatus,
    dcogsStatus,
    dcogsDBClusterOptionGroupName,

    -- * DBClusterParameterGroup
    DBClusterParameterGroup,
    dbClusterParameterGroup,
    dcpgDBClusterParameterGroupARN,
    dcpgDBParameterGroupFamily,
    dcpgDBClusterParameterGroupName,
    dcpgDescription,

    -- * DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage,
    dbClusterParameterGroupNameMessage,
    dcpgnmDBClusterParameterGroupName,

    -- * DBClusterRole
    DBClusterRole,
    dbClusterRole,
    dcrStatus,
    dcrFeatureName,
    dcrRoleARN,

    -- * DBClusterSnapshot
    DBClusterSnapshot,
    dbClusterSnapshot,
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
    DBClusterSnapshotAttribute,
    dbClusterSnapshotAttribute,
    dcsaAttributeValues,
    dcsaAttributeName,

    -- * DBClusterSnapshotAttributesResult
    DBClusterSnapshotAttributesResult,
    dbClusterSnapshotAttributesResult,
    dcsarDBClusterSnapshotIdentifier,
    dcsarDBClusterSnapshotAttributes,

    -- * DBEngineVersion
    DBEngineVersion,
    dbEngineVersion,
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
    DBInstance,
    dbInstance,
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
    DBInstanceAutomatedBackup,
    dbInstanceAutomatedBackup,
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
    DBInstanceRole,
    dbInstanceRole,
    dirStatus,
    dirFeatureName,
    dirRoleARN,

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo,
    dbInstanceStatusInfo,
    disiStatus,
    disiNormal,
    disiStatusType,
    disiMessage,

    -- * DBParameterGroup
    DBParameterGroup,
    dbParameterGroup,
    dpgDBParameterGroupARN,
    dpgDBParameterGroupFamily,
    dpgDBParameterGroupName,
    dpgDescription,

    -- * DBParameterGroupNameMessage
    DBParameterGroupNameMessage,
    dbParameterGroupNameMessage,
    dpgnmDBParameterGroupName,

    -- * DBParameterGroupStatus
    DBParameterGroupStatus,
    dbParameterGroupStatus,
    dpgsDBParameterGroupName,
    dpgsParameterApplyStatus,

    -- * DBProxy
    DBProxy,
    dbProxy,
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
    DBProxyTarget,
    dbProxyTarget,
    dptTargetARN,
    dptTargetHealth,
    dptTrackedClusterId,
    dptRDSResourceId,
    dptType,
    dptEndpoint,
    dptPort,

    -- * DBProxyTargetGroup
    DBProxyTargetGroup,
    dbProxyTargetGroup,
    dptgStatus,
    dptgConnectionPoolConfig,
    dptgTargetGroupARN,
    dptgUpdatedDate,
    dptgCreatedDate,
    dptgDBProxyName,
    dptgTargetGroupName,
    dptgIsDefault,

    -- * DBSecurityGroup
    DBSecurityGroup,
    dbSecurityGroup,
    dbsgVPCId,
    dbsgOwnerId,
    dbsgDBSecurityGroupARN,
    dbsgIPRanges,
    dbsgDBSecurityGroupName,
    dbsgEC2SecurityGroups,
    dbsgDBSecurityGroupDescription,

    -- * DBSecurityGroupMembership
    DBSecurityGroupMembership,
    dbSecurityGroupMembership,
    dsgmStatus,
    dsgmDBSecurityGroupName,

    -- * DBSnapshot
    DBSnapshot,
    dbSnapshot,
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
    DBSnapshotAttribute,
    dbSnapshotAttribute,
    dsaAttributeValues,
    dsaAttributeName,

    -- * DBSnapshotAttributesResult
    DBSnapshotAttributesResult,
    dbSnapshotAttributesResult,
    dsarDBSnapshotIdentifier,
    dsarDBSnapshotAttributes,

    -- * DBSubnetGroup
    DBSubnetGroup,
    dbSubnetGroup,
    dsgDBSubnetGroupName,
    dsgVPCId,
    dsgSubnets,
    dsgDBSubnetGroupDescription,
    dsgDBSubnetGroupARN,
    dsgSubnetGroupStatus,

    -- * DescribeDBLogFilesDetails
    DescribeDBLogFilesDetails,
    describeDBLogFilesDetails,
    ddlfdLastWritten,
    ddlfdSize,
    ddlfdLogFileName,

    -- * DomainMembership
    DomainMembership,
    domainMembership,
    dmStatus,
    dmFQDN,
    dmDomain,
    dmIAMRoleName,

    -- * DoubleRange
    DoubleRange,
    doubleRange,
    drTo,
    drFrom,

    -- * EC2SecurityGroup
    EC2SecurityGroup,
    ec2SecurityGroup,
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,
    esgEC2SecurityGroupId,

    -- * Endpoint
    Endpoint,
    endpoint,
    eHostedZoneId,
    eAddress,
    ePort,

    -- * EngineDefaults
    EngineDefaults,
    engineDefaults,
    edDBParameterGroupFamily,
    edMarker,
    edParameters,

    -- * Event
    Event,
    event,
    eSourceType,
    eSourceARN,
    eSourceIdentifier,
    eDate,
    eEventCategories,
    eMessage,

    -- * EventCategoriesMap
    EventCategoriesMap,
    eventCategoriesMap,
    ecmSourceType,
    ecmEventCategories,

    -- * EventSubscription
    EventSubscription,
    eventSubscription,
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
    ExportTask,
    exportTask,
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
    Filter,
    filter',
    fName,
    fValues,

    -- * GlobalCluster
    GlobalCluster,
    globalCluster,
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
    GlobalClusterMember,
    globalClusterMember,
    gcmReaders,
    gcmDBClusterARN,
    gcmIsWriter,
    gcmGlobalWriteForwardingStatus,

    -- * IPRange
    IPRange,
    ipRange,
    irStatus,
    irCIdRIP,

    -- * InstallationMedia
    InstallationMedia,
    installationMedia,
    imEngineVersion,
    imStatus,
    imInstallationMediaId,
    imEngineInstallationMediaPath,
    imEngine,
    imOSInstallationMediaPath,
    imCustomAvailabilityZoneId,
    imFailureCause,

    -- * InstallationMediaFailureCause
    InstallationMediaFailureCause,
    installationMediaFailureCause,
    imfcMessage,

    -- * MinimumEngineVersionPerAllowedValue
    MinimumEngineVersionPerAllowedValue,
    minimumEngineVersionPerAllowedValue,
    mevpavMinimumEngineVersion,
    mevpavAllowedValue,

    -- * Option
    Option,
    option,
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
    OptionConfiguration,
    optionConfiguration,
    ocOptionSettings,
    ocVPCSecurityGroupMemberships,
    ocDBSecurityGroupMemberships,
    ocOptionVersion,
    ocPort,
    ocOptionName,

    -- * OptionGroup
    OptionGroup,
    optionGroup,
    ogOptionGroupDescription,
    ogVPCId,
    ogAllowsVPCAndNonVPCInstanceMemberships,
    ogEngineName,
    ogOptionGroupARN,
    ogMajorEngineVersion,
    ogOptions,
    ogOptionGroupName,

    -- * OptionGroupMembership
    OptionGroupMembership,
    optionGroupMembership,
    ogmStatus,
    ogmOptionGroupName,

    -- * OptionGroupOption
    OptionGroupOption,
    optionGroupOption,
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
    OptionGroupOptionSetting,
    optionGroupOptionSetting,
    ogosApplyType,
    ogosMinimumEngineVersionPerAllowedValue,
    ogosSettingName,
    ogosDefaultValue,
    ogosIsModifiable,
    ogosSettingDescription,
    ogosAllowedValues,
    ogosIsRequired,

    -- * OptionSetting
    OptionSetting,
    optionSetting,
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
    OptionVersion,
    optionVersion,
    ovVersion,
    ovIsDefault,

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption,
    orderableDBInstanceOption,
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
    Outpost,
    outpost,
    oARN,

    -- * Parameter
    Parameter,
    parameter,
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
    PendingCloudwatchLogsExports,
    pendingCloudwatchLogsExports,
    pcleLogTypesToEnable,
    pcleLogTypesToDisable,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction,
    pendingMaintenanceAction,
    pmaAutoAppliedAfterDate,
    pmaAction,
    pmaOptInStatus,
    pmaDescription,
    pmaForcedApplyDate,
    pmaCurrentApplyDate,

    -- * PendingModifiedValues
    PendingModifiedValues,
    pendingModifiedValues,
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
    ProcessorFeature,
    processorFeature,
    pfValue,
    pfName,

    -- * Range
    Range,
    range,
    rTo,
    rFrom,
    rStep,

    -- * RecurringCharge
    RecurringCharge,
    recurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- * ReservedDBInstance
    ReservedDBInstance,
    reservedDBInstance,
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
    ReservedDBInstancesOffering,
    reservedDBInstancesOffering,
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
    ResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions,
    rpmaPendingMaintenanceActionDetails,
    rpmaResourceIdentifier,

    -- * RestoreWindow
    RestoreWindow,
    restoreWindow,
    rwLatestTime,
    rwEarliestTime,

    -- * ScalingConfiguration
    ScalingConfiguration,
    scalingConfiguration,
    scSecondsUntilAutoPause,
    scTimeoutAction,
    scAutoPause,
    scMaxCapacity,
    scMinCapacity,

    -- * ScalingConfigurationInfo
    ScalingConfigurationInfo,
    scalingConfigurationInfo,
    sciSecondsUntilAutoPause,
    sciTimeoutAction,
    sciAutoPause,
    sciMaxCapacity,
    sciMinCapacity,

    -- * SourceRegion
    SourceRegion,
    sourceRegion,
    srStatus,
    srRegionName,
    srEndpoint,

    -- * Subnet
    Subnet,
    subnet,
    sSubnetStatus,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,
    sSubnetOutpost,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TargetHealth
    TargetHealth,
    targetHealth,
    thState,
    thReason,
    thDescription,

    -- * Timezone
    Timezone,
    timezone,
    tTimezoneName,

    -- * UpgradeTarget
    UpgradeTarget,
    upgradeTarget,
    utEngineVersion,
    utIsMajorVersionUpgrade,
    utEngine,
    utAutoUpgrade,
    utDescription,

    -- * UserAuthConfig
    UserAuthConfig,
    userAuthConfig,
    uacIAMAuth,
    uacUserName,
    uacAuthScheme,
    uacSecretARN,
    uacDescription,

    -- * UserAuthConfigInfo
    UserAuthConfigInfo,
    userAuthConfigInfo,
    uaciIAMAuth,
    uaciUserName,
    uaciAuthScheme,
    uaciSecretARN,
    uaciDescription,

    -- * VPCSecurityGroupMembership
    VPCSecurityGroupMembership,
    vpcSecurityGroupMembership,
    vsgmStatus,
    vsgmVPCSecurityGroupId,

    -- * VPNDetails
    VPNDetails,
    vpnDetails,
    vdVPNName,
    vdVPNTunnelOriginatorIP,
    vdVPNId,
    vdVPNState,
    vdVPNPSK,
    vdVPNGatewayIP,

    -- * ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage,
    validDBInstanceModificationsMessage,
    vdimmValidProcessorFeatures,
    vdimmStorage,

    -- * ValidStorageOptions
    ValidStorageOptions,
    validStorageOptions,
    vsoStorageSize,
    vsoProvisionedIOPS,
    vsoIOPSToStorageRatio,
    vsoSupportsStorageAutoscaling,
    vsoStorageType,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
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
import Network.AWS.Sign.V4

-- | API version @2014-10-31@ of the Amazon Relational Database Service SDK configuration.
rds :: Service
rds =
  Service
    { _svcAbbrev = "RDS",
      _svcSigner = v4,
      _svcPrefix = "rds",
      _svcVersion = "2014-10-31",
      _svcEndpoint = defaultEndpoint rds,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "RDS",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
