{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Relational Database Service
--
-- Amazon Relational Database Service (Amazon RDS) is a web service that
-- makes it easier to set up, operate, and scale a relational database in
-- the cloud. It provides cost-efficient, resizeable capacity for an
-- industry-standard relational database and manages common database
-- administration tasks, freeing up developers to focus on what makes their
-- applications and businesses unique.
--
-- Amazon RDS gives you access to the capabilities of a MySQL, PostgreSQL,
-- Microsoft SQL Server, Oracle, or Aurora database server. This means the
-- code, applications, and tools you already use today with your existing
-- databases work with Amazon RDS without modification. Amazon RDS
-- automatically backs up your database and maintains the database software
-- that powers your DB instance. Amazon RDS is flexible: you can scale your
-- database instance\'s compute resources and storage capacity to meet your
-- application\'s demand. As with all Amazon Web Services, there are no
-- up-front investments, and you pay only for the resources you use.
--
-- This is an interface reference for Amazon RDS. It contains documentation
-- for a programming or command line interface you can use to manage Amazon
-- RDS. Note that Amazon RDS is asynchronous, which means that some
-- interfaces might require techniques such as polling or callback
-- functions to determine when a command has been applied. In this
-- reference, the parameter descriptions indicate whether a command is
-- applied immediately, on the next instance reboot, or during the
-- maintenance window. For a summary of the Amazon RDS interfaces, go to
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Welcome.html#Welcome.Interfaces Available RDS Interfaces>.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.RDS
    (
    -- * Service
      RDS

    -- * Errors
    -- $errors

    -- ** CertificateNotFoundFault
    , _CertificateNotFoundFault

    -- ** ReservedDBInstanceQuotaExceededFault
    , _ReservedDBInstanceQuotaExceededFault

    -- ** DBClusterSnapshotAlreadyExistsFault
    , _DBClusterSnapshotAlreadyExistsFault

    -- ** AuthorizationQuotaExceededFault
    , _AuthorizationQuotaExceededFault

    -- ** SourceNotFoundFault
    , _SourceNotFoundFault

    -- ** InvalidDBParameterGroupStateFault
    , _InvalidDBParameterGroupStateFault

    -- ** DBParameterGroupAlreadyExistsFault
    , _DBParameterGroupAlreadyExistsFault

    -- ** PointInTimeRestoreNotEnabledFault
    , _PointInTimeRestoreNotEnabledFault

    -- ** DBParameterGroupQuotaExceededFault
    , _DBParameterGroupQuotaExceededFault

    -- ** ProvisionedIOPSNotAvailableInAZFault
    , _ProvisionedIOPSNotAvailableInAZFault

    -- ** AuthorizationAlreadyExistsFault
    , _AuthorizationAlreadyExistsFault

    -- ** InsufficientDBClusterCapacityFault
    , _InsufficientDBClusterCapacityFault

    -- ** ReservedDBInstanceAlreadyExistsFault
    , _ReservedDBInstanceAlreadyExistsFault

    -- ** SubscriptionCategoryNotFoundFault
    , _SubscriptionCategoryNotFoundFault

    -- ** DBSubnetQuotaExceededFault
    , _DBSubnetQuotaExceededFault

    -- ** SubscriptionNotFoundFault
    , _SubscriptionNotFoundFault

    -- ** InvalidSubnet
    , _InvalidSubnet

    -- ** DBClusterNotFoundFault
    , _DBClusterNotFoundFault

    -- ** OptionGroupNotFoundFault
    , _OptionGroupNotFoundFault

    -- ** DBLogFileNotFoundFault
    , _DBLogFileNotFoundFault

    -- ** OptionGroupAlreadyExistsFault
    , _OptionGroupAlreadyExistsFault

    -- ** DBClusterAlreadyExistsFault
    , _DBClusterAlreadyExistsFault

    -- ** StorageTypeNotSupportedFault
    , _StorageTypeNotSupportedFault

    -- ** DBSecurityGroupQuotaExceededFault
    , _DBSecurityGroupQuotaExceededFault

    -- ** DBSnapshotNotFoundFault
    , _DBSnapshotNotFoundFault

    -- ** InvalidEventSubscriptionStateFault
    , _InvalidEventSubscriptionStateFault

    -- ** KMSKeyNotAccessibleFault
    , _KMSKeyNotAccessibleFault

    -- ** OptionGroupQuotaExceededFault
    , _OptionGroupQuotaExceededFault

    -- ** DBSecurityGroupAlreadyExistsFault
    , _DBSecurityGroupAlreadyExistsFault

    -- ** SnapshotQuotaExceededFault
    , _SnapshotQuotaExceededFault

    -- ** SNSTopicARNNotFoundFault
    , _SNSTopicARNNotFoundFault

    -- ** DBClusterQuotaExceededFault
    , _DBClusterQuotaExceededFault

    -- ** DBClusterParameterGroupNotFoundFault
    , _DBClusterParameterGroupNotFoundFault

    -- ** DBSubnetGroupAlreadyExistsFault
    , _DBSubnetGroupAlreadyExistsFault

    -- ** InstanceQuotaExceededFault
    , _InstanceQuotaExceededFault

    -- ** SNSNoAuthorizationFault
    , _SNSNoAuthorizationFault

    -- ** DBSecurityGroupNotFoundFault
    , _DBSecurityGroupNotFoundFault

    -- ** DBSecurityGroupNotSupportedFault
    , _DBSecurityGroupNotSupportedFault

    -- ** DomainNotFoundFault
    , _DomainNotFoundFault

    -- ** DBClusterSnapshotNotFoundFault
    , _DBClusterSnapshotNotFoundFault

    -- ** ReservedDBInstancesOfferingNotFoundFault
    , _ReservedDBInstancesOfferingNotFoundFault

    -- ** InvalidDBSubnetGroupFault
    , _InvalidDBSubnetGroupFault

    -- ** InvalidDBSubnetStateFault
    , _InvalidDBSubnetStateFault

    -- ** DBParameterGroupNotFoundFault
    , _DBParameterGroupNotFoundFault

    -- ** SNSInvalidTopicFault
    , _SNSInvalidTopicFault

    -- ** SubscriptionAlreadyExistFault
    , _SubscriptionAlreadyExistFault

    -- ** InvalidDBClusterSnapshotStateFault
    , _InvalidDBClusterSnapshotStateFault

    -- ** InsufficientDBInstanceCapacityFault
    , _InsufficientDBInstanceCapacityFault

    -- ** InvalidVPCNetworkStateFault
    , _InvalidVPCNetworkStateFault

    -- ** AuthorizationNotFoundFault
    , _AuthorizationNotFoundFault

    -- ** ReservedDBInstanceNotFoundFault
    , _ReservedDBInstanceNotFoundFault

    -- ** DBSubnetGroupQuotaExceededFault
    , _DBSubnetGroupQuotaExceededFault

    -- ** DBSubnetGroupNotAllowedFault
    , _DBSubnetGroupNotAllowedFault

    -- ** EventSubscriptionQuotaExceededFault
    , _EventSubscriptionQuotaExceededFault

    -- ** InvalidOptionGroupStateFault
    , _InvalidOptionGroupStateFault

    -- ** InvalidDBClusterStateFault
    , _InvalidDBClusterStateFault

    -- ** InsufficientStorageClusterCapacityFault
    , _InsufficientStorageClusterCapacityFault

    -- ** DBInstanceAlreadyExistsFault
    , _DBInstanceAlreadyExistsFault

    -- ** ResourceNotFoundFault
    , _ResourceNotFoundFault

    -- ** DBUpgradeDependencyFailureFault
    , _DBUpgradeDependencyFailureFault

    -- ** InvalidDBSecurityGroupStateFault
    , _InvalidDBSecurityGroupStateFault

    -- ** InsufficientDomainCapacityFault
    , _InsufficientDomainCapacityFault

    -- ** DBSubnetGroupNotFoundFault
    , _DBSubnetGroupNotFoundFault

    -- ** InvalidRestoreFault
    , _InvalidRestoreFault

    -- ** InvalidDBInstanceStateFault
    , _InvalidDBInstanceStateFault

    -- ** InvalidDBSnapshotStateFault
    , _InvalidDBSnapshotStateFault

    -- ** InvalidDBSubnetGroupStateFault
    , _InvalidDBSubnetGroupStateFault

    -- ** StorageQuotaExceededFault
    , _StorageQuotaExceededFault

    -- ** DBSnapshotAlreadyExistsFault
    , _DBSnapshotAlreadyExistsFault

    -- ** DBInstanceNotFoundFault
    , _DBInstanceNotFoundFault

    -- ** SubnetAlreadyInUse
    , _SubnetAlreadyInUse

    -- ** DBSubnetGroupDoesNotCoverEnoughAZs
    , _DBSubnetGroupDoesNotCoverEnoughAZs

    -- * Waiters
    -- $waiters

    -- ** DBInstanceAvailable
    , dbInstanceAvailable

    -- ** DBSnapshotCompleted
    , dbSnapshotCompleted

    -- ** DBInstanceDeleted
    , dbInstanceDeleted

    -- * Operations
    -- $operations

    -- ** DescribeDBEngineVersions (Paginated)
    , module Network.AWS.RDS.DescribeDBEngineVersions

    -- ** DescribeDBClusterParameterGroups
    , module Network.AWS.RDS.DescribeDBClusterParameterGroups

    -- ** PromoteReadReplica
    , module Network.AWS.RDS.PromoteReadReplica

    -- ** ModifyEventSubscription
    , module Network.AWS.RDS.ModifyEventSubscription

    -- ** CopyDBSnapshot
    , module Network.AWS.RDS.CopyDBSnapshot

    -- ** AddSourceIdentifierToSubscription
    , module Network.AWS.RDS.AddSourceIdentifierToSubscription

    -- ** ModifyDBInstance
    , module Network.AWS.RDS.ModifyDBInstance

    -- ** ResetDBClusterParameterGroup
    , module Network.AWS.RDS.ResetDBClusterParameterGroup

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.RDS.DescribeEvents

    -- ** DescribeEngineDefaultParameters (Paginated)
    , module Network.AWS.RDS.DescribeEngineDefaultParameters

    -- ** DescribeDBClusters
    , module Network.AWS.RDS.DescribeDBClusters

    -- ** ModifyDBSubnetGroup
    , module Network.AWS.RDS.ModifyDBSubnetGroup

    -- ** DescribeDBLogFiles (Paginated)
    , module Network.AWS.RDS.DescribeDBLogFiles

    -- ** ListTagsForResource
    , module Network.AWS.RDS.ListTagsForResource

    -- ** DescribeOptionGroups (Paginated)
    , module Network.AWS.RDS.DescribeOptionGroups

    -- ** DeleteDBCluster
    , module Network.AWS.RDS.DeleteDBCluster

    -- ** RemoveSourceIdentifierFromSubscription
    , module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription

    -- ** CopyDBParameterGroup
    , module Network.AWS.RDS.CopyDBParameterGroup

    -- ** DescribeReservedDBInstances (Paginated)
    , module Network.AWS.RDS.DescribeReservedDBInstances

    -- ** DeleteOptionGroup
    , module Network.AWS.RDS.DeleteOptionGroup

    -- ** DescribeEngineDefaultClusterParameters
    , module Network.AWS.RDS.DescribeEngineDefaultClusterParameters

    -- ** CreateEventSubscription
    , module Network.AWS.RDS.CreateEventSubscription

    -- ** RemoveTagsFromResource
    , module Network.AWS.RDS.RemoveTagsFromResource

    -- ** CreateDBInstance
    , module Network.AWS.RDS.CreateDBInstance

    -- ** RestoreDBInstanceFromDBSnapshot
    , module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot

    -- ** AuthorizeDBSecurityGroupIngress
    , module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress

    -- ** DeleteDBClusterParameterGroup
    , module Network.AWS.RDS.DeleteDBClusterParameterGroup

    -- ** PurchaseReservedDBInstancesOffering
    , module Network.AWS.RDS.PurchaseReservedDBInstancesOffering

    -- ** DescribeCertificates
    , module Network.AWS.RDS.DescribeCertificates

    -- ** RestoreDBClusterFromSnapshot
    , module Network.AWS.RDS.RestoreDBClusterFromSnapshot

    -- ** CreateDBSnapshot
    , module Network.AWS.RDS.CreateDBSnapshot

    -- ** DeleteEventSubscription
    , module Network.AWS.RDS.DeleteEventSubscription

    -- ** DescribeDBParameterGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBParameterGroups

    -- ** DescribeOrderableDBInstanceOptions (Paginated)
    , module Network.AWS.RDS.DescribeOrderableDBInstanceOptions

    -- ** CreateDBClusterParameterGroup
    , module Network.AWS.RDS.CreateDBClusterParameterGroup

    -- ** DescribeEventSubscriptions (Paginated)
    , module Network.AWS.RDS.DescribeEventSubscriptions

    -- ** AddTagsToResource
    , module Network.AWS.RDS.AddTagsToResource

    -- ** DescribeOptionGroupOptions (Paginated)
    , module Network.AWS.RDS.DescribeOptionGroupOptions

    -- ** DescribeDBParameters (Paginated)
    , module Network.AWS.RDS.DescribeDBParameters

    -- ** DeleteDBClusterSnapshot
    , module Network.AWS.RDS.DeleteDBClusterSnapshot

    -- ** DescribeDBSnapshots (Paginated)
    , module Network.AWS.RDS.DescribeDBSnapshots

    -- ** DescribeDBSubnetGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBSubnetGroups

    -- ** CreateDBParameterGroup
    , module Network.AWS.RDS.CreateDBParameterGroup

    -- ** CreateDBClusterSnapshot
    , module Network.AWS.RDS.CreateDBClusterSnapshot

    -- ** ModifyOptionGroup
    , module Network.AWS.RDS.ModifyOptionGroup

    -- ** ModifyDBCluster
    , module Network.AWS.RDS.ModifyDBCluster

    -- ** DescribeEventCategories
    , module Network.AWS.RDS.DescribeEventCategories

    -- ** ModifyDBClusterParameterGroup
    , module Network.AWS.RDS.ModifyDBClusterParameterGroup

    -- ** DescribePendingMaintenanceActions
    , module Network.AWS.RDS.DescribePendingMaintenanceActions

    -- ** RestoreDBInstanceToPointInTime
    , module Network.AWS.RDS.RestoreDBInstanceToPointInTime

    -- ** ResetDBParameterGroup
    , module Network.AWS.RDS.ResetDBParameterGroup

    -- ** CopyDBClusterSnapshot
    , module Network.AWS.RDS.CopyDBClusterSnapshot

    -- ** ModifyDBParameterGroup
    , module Network.AWS.RDS.ModifyDBParameterGroup

    -- ** FailoverDBCluster
    , module Network.AWS.RDS.FailoverDBCluster

    -- ** CreateDBCluster
    , module Network.AWS.RDS.CreateDBCluster

    -- ** CreateOptionGroup
    , module Network.AWS.RDS.CreateOptionGroup

    -- ** ApplyPendingMaintenanceAction
    , module Network.AWS.RDS.ApplyPendingMaintenanceAction

    -- ** RevokeDBSecurityGroupIngress
    , module Network.AWS.RDS.RevokeDBSecurityGroupIngress

    -- ** DeleteDBSnapshot
    , module Network.AWS.RDS.DeleteDBSnapshot

    -- ** DescribeDBClusterParameters
    , module Network.AWS.RDS.DescribeDBClusterParameters

    -- ** CreateDBSecurityGroup
    , module Network.AWS.RDS.CreateDBSecurityGroup

    -- ** DeleteDBSubnetGroup
    , module Network.AWS.RDS.DeleteDBSubnetGroup

    -- ** DescribeAccountAttributes
    , module Network.AWS.RDS.DescribeAccountAttributes

    -- ** DeleteDBSecurityGroup
    , module Network.AWS.RDS.DeleteDBSecurityGroup

    -- ** RebootDBInstance
    , module Network.AWS.RDS.RebootDBInstance

    -- ** DescribeDBClusterSnapshots
    , module Network.AWS.RDS.DescribeDBClusterSnapshots

    -- ** CreateDBSubnetGroup
    , module Network.AWS.RDS.CreateDBSubnetGroup

    -- ** DescribeReservedDBInstancesOfferings (Paginated)
    , module Network.AWS.RDS.DescribeReservedDBInstancesOfferings

    -- ** DeleteDBInstance
    , module Network.AWS.RDS.DeleteDBInstance

    -- ** DescribeDBInstances (Paginated)
    , module Network.AWS.RDS.DescribeDBInstances

    -- ** CopyOptionGroup
    , module Network.AWS.RDS.CopyOptionGroup

    -- ** DownloadDBLogFilePortion (Paginated)
    , module Network.AWS.RDS.DownloadDBLogFilePortion

    -- ** CreateDBInstanceReadReplica
    , module Network.AWS.RDS.CreateDBInstanceReadReplica

    -- ** RestoreDBClusterToPointInTime
    , module Network.AWS.RDS.RestoreDBClusterToPointInTime

    -- ** DeleteDBParameterGroup
    , module Network.AWS.RDS.DeleteDBParameterGroup

    -- ** DescribeDBSecurityGroups (Paginated)
    , module Network.AWS.RDS.DescribeDBSecurityGroups

    -- * Types

    -- ** ApplyMethod
    , ApplyMethod (..)

    -- ** SourceType
    , SourceType (..)

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
    , cCertificateType
    , cValidTill
    , cCertificateIdentifier
    , cThumbprint
    , cValidFrom

    -- ** CharacterSet
    , CharacterSet
    , characterSet
    , csCharacterSetName
    , csCharacterSetDescription

    -- ** DBCluster
    , DBCluster
    , dbCluster
    , dcEngineVersion
    , dcStatus
    , dcDBClusterIdentifier
    , dcDBClusterMembers
    , dcDBClusterParameterGroup
    , dcMasterUsername
    , dcEarliestRestorableTime
    , dcEngine
    , dcLatestRestorableTime
    , dcPreferredMaintenanceWindow
    , dcCharacterSetName
    , dcAvailabilityZones
    , dcPreferredBackupWindow
    , dcVPCSecurityGroups
    , dcBackupRetentionPeriod
    , dcDatabaseName
    , dcDBSubnetGroup
    , dcAllocatedStorage
    , dcEndpoint
    , dcPercentProgress
    , dcPort
    , dcDBClusterOptionGroupMemberships

    -- ** DBClusterMember
    , DBClusterMember
    , dbClusterMember
    , dcmDBInstanceIdentifier
    , dcmIsClusterWriter
    , dcmDBClusterParameterGroupStatus

    -- ** DBClusterOptionGroupStatus
    , DBClusterOptionGroupStatus
    , dbClusterOptionGroupStatus
    , dcogsStatus
    , dcogsDBClusterOptionGroupName

    -- ** DBClusterParameterGroup
    , DBClusterParameterGroup
    , dbClusterParameterGroup
    , dcpgDBParameterGroupFamily
    , dcpgDBClusterParameterGroupName
    , dcpgDescription

    -- ** DBClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage
    , dbClusterParameterGroupNameMessage
    , dcpgnmDBClusterParameterGroupName

    -- ** DBClusterSnapshot
    , DBClusterSnapshot
    , dbClusterSnapshot
    , dcsEngineVersion
    , dcsStatus
    , dcsDBClusterIdentifier
    , dcsMasterUsername
    , dcsVPCId
    , dcsDBClusterSnapshotIdentifier
    , dcsEngine
    , dcsLicenseModel
    , dcsSnapshotType
    , dcsAvailabilityZones
    , dcsSnapshotCreateTime
    , dcsAllocatedStorage
    , dcsClusterCreateTime
    , dcsPercentProgress
    , dcsPort

    -- ** DBEngineVersion
    , DBEngineVersion
    , dbEngineVersion
    , devDBEngineVersionDescription
    , devEngineVersion
    , devDefaultCharacterSet
    , devSupportedCharacterSets
    , devEngine
    , devDBParameterGroupFamily
    , devDBEngineDescription

    -- ** DBInstance
    , DBInstance
    , dbInstance
    , diDBSecurityGroups
    , diEngineVersion
    , diStorageEncrypted
    , diDBClusterIdentifier
    , diAutoMinorVersionUpgrade
    , diMasterUsername
    , diPubliclyAccessible
    , diReadReplicaDBInstanceIdentifiers
    , diIOPS
    , diInstanceCreateTime
    , diReadReplicaSourceDBInstanceIdentifier
    , diEngine
    , diLatestRestorableTime
    , diDBInstanceClass
    , diLicenseModel
    , diPreferredMaintenanceWindow
    , diCharacterSetName
    , diDBInstanceIdentifier
    , diCACertificateIdentifier
    , diPreferredBackupWindow
    , diAvailabilityZone
    , diVPCSecurityGroups
    , diBackupRetentionPeriod
    , diKMSKeyId
    , diDBSubnetGroup
    , diMultiAZ
    , diSecondaryAvailabilityZone
    , diOptionGroupMemberships
    , diDBiResourceId
    , diAllocatedStorage
    , diEndpoint
    , diDBParameterGroups
    , diTDECredentialARN
    , diCopyTagsToSnapshot
    , diDBInstanceStatus
    , diDBInstancePort
    , diPendingModifiedValues
    , diStatusInfos
    , diDBName
    , diDomainMemberships
    , diStorageType

    -- ** DBInstanceStatusInfo
    , DBInstanceStatusInfo
    , dbInstanceStatusInfo
    , disiStatus
    , disiNormal
    , disiStatusType
    , disiMessage

    -- ** DBParameterGroup
    , DBParameterGroup
    , dbParameterGroup
    , dpgDBParameterGroupFamily
    , dpgDBParameterGroupName
    , dpgDescription

    -- ** DBParameterGroupNameMessage
    , DBParameterGroupNameMessage
    , dbParameterGroupNameMessage
    , dpgnmDBParameterGroupName

    -- ** DBParameterGroupStatus
    , DBParameterGroupStatus
    , dbParameterGroupStatus
    , dpgsDBParameterGroupName
    , dpgsParameterApplyStatus

    -- ** DBSecurityGroup
    , DBSecurityGroup
    , dbSecurityGroup
    , dsgVPCId
    , dsgOwnerId
    , dsgIPRanges
    , dsgDBSecurityGroupName
    , dsgEC2SecurityGroups
    , dsgDBSecurityGroupDescription

    -- ** DBSecurityGroupMembership
    , DBSecurityGroupMembership
    , dbSecurityGroupMembership
    , dsgmStatus
    , dsgmDBSecurityGroupName

    -- ** DBSnapshot
    , DBSnapshot
    , dbSnapshot
    , dsEngineVersion
    , dsStatus
    , dsMasterUsername
    , dsSourceRegion
    , dsIOPS
    , dsInstanceCreateTime
    , dsVPCId
    , dsEngine
    , dsEncrypted
    , dsDBSnapshotIdentifier
    , dsLicenseModel
    , dsSnapshotType
    , dsDBInstanceIdentifier
    , dsSourceDBSnapshotIdentifier
    , dsAvailabilityZone
    , dsKMSKeyId
    , dsSnapshotCreateTime
    , dsAllocatedStorage
    , dsTDECredentialARN
    , dsOptionGroupName
    , dsPercentProgress
    , dsPort
    , dsStorageType

    -- ** DBSubnetGroup
    , DBSubnetGroup
    , dbSubnetGroup
    , dbsgDBSubnetGroupName
    , dbsgVPCId
    , dbsgSubnets
    , dbsgDBSubnetGroupDescription
    , dbsgSubnetGroupStatus

    -- ** DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails
    , describeDBLogFilesDetails
    , ddlfdLastWritten
    , ddlfdSize
    , ddlfdLogFileName

    -- ** DomainMembership
    , DomainMembership
    , domainMembership
    , dmStatus
    , dmDomain
    , dmConnectivity

    -- ** EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , esgStatus
    , esgEC2SecurityGroupOwnerId
    , esgEC2SecurityGroupName
    , esgEC2SecurityGroupId

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- ** EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edParameters
    , edMarker

    -- ** Event
    , Event
    , event
    , eSourceType
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage

    -- ** EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEventCategories

    -- ** EventSubscription
    , EventSubscription
    , eventSubscription
    , esCustomerAWSId
    , esStatus
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

    -- ** IPRange
    , IPRange
    , ipRange
    , irStatus
    , irCIdRIP

    -- ** Option
    , Option
    , option
    , oOptionName
    , oPermanent
    , oPersistent
    , oOptionDescription
    , oOptionSettings
    , oVPCSecurityGroupMemberships
    , oDBSecurityGroupMemberships
    , oPort

    -- ** OptionConfiguration
    , OptionConfiguration
    , optionConfiguration
    , ocOptionSettings
    , ocVPCSecurityGroupMemberships
    , ocDBSecurityGroupMemberships
    , ocPort
    , ocOptionName

    -- ** OptionGroup
    , OptionGroup
    , optionGroup
    , ogOptionGroupDescription
    , ogVPCId
    , ogAllowsVPCAndNonVPCInstanceMemberships
    , ogEngineName
    , ogMajorEngineVersion
    , ogOptions
    , ogOptionGroupName

    -- ** OptionGroupMembership
    , OptionGroupMembership
    , optionGroupMembership
    , ogmStatus
    , ogmOptionGroupName

    -- ** OptionGroupOption
    , OptionGroupOption
    , optionGroupOption
    , ogoMinimumRequiredMinorEngineVersion
    , ogoPermanent
    , ogoPersistent
    , ogoEngineName
    , ogoName
    , ogoMajorEngineVersion
    , ogoDefaultPort
    , ogoOptionGroupOptionSettings
    , ogoPortRequired
    , ogoOptionsDependedOn
    , ogoDescription

    -- ** OptionGroupOptionSetting
    , OptionGroupOptionSetting
    , optionGroupOptionSetting
    , ogosApplyType
    , ogosSettingName
    , ogosDefaultValue
    , ogosIsModifiable
    , ogosAllowedValues
    , ogosSettingDescription

    -- ** OptionSetting
    , OptionSetting
    , optionSetting
    , osIsCollection
    , osApplyType
    , osValue
    , osName
    , osDefaultValue
    , osIsModifiable
    , osAllowedValues
    , osDataType
    , osDescription

    -- ** OrderableDBInstanceOption
    , OrderableDBInstanceOption
    , orderableDBInstanceOption
    , odioEngineVersion
    , odioMultiAZCapable
    , odioEngine
    , odioSupportsIOPS
    , odioDBInstanceClass
    , odioLicenseModel
    , odioAvailabilityZones
    , odioReadReplicaCapable
    , odioSupportsStorageEncryption
    , odioVPC
    , odioStorageType

    -- ** Parameter
    , Parameter
    , parameter
    , pApplyType
    , pParameterValue
    , pApplyMethod
    , pMinimumEngineVersion
    , pSource
    , pIsModifiable
    , pAllowedValues
    , pDataType
    , pParameterName
    , pDescription

    -- ** PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAutoAppliedAfterDate
    , pmaAction
    , pmaOptInStatus
    , pmaDescription
    , pmaCurrentApplyDate
    , pmaForcedApplyDate

    -- ** PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
    , pmvMasterUserPassword
    , pmvIOPS
    , pmvDBInstanceClass
    , pmvDBInstanceIdentifier
    , pmvCACertificateIdentifier
    , pmvBackupRetentionPeriod
    , pmvMultiAZ
    , pmvAllocatedStorage
    , pmvPort
    , pmvStorageType

    -- ** RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- ** ReservedDBInstance
    , ReservedDBInstance
    , reservedDBInstance
    , rdiDBInstanceCount
    , rdiState
    , rdiCurrencyCode
    , rdiProductDescription
    , rdiStartTime
    , rdiReservedDBInstanceId
    , rdiDBInstanceClass
    , rdiMultiAZ
    , rdiReservedDBInstancesOfferingId
    , rdiOfferingType
    , rdiUsagePrice
    , rdiRecurringCharges
    , rdiFixedPrice
    , rdiDuration

    -- ** ReservedDBInstancesOffering
    , ReservedDBInstancesOffering
    , reservedDBInstancesOffering
    , rdioCurrencyCode
    , rdioProductDescription
    , rdioDBInstanceClass
    , rdioMultiAZ
    , rdioReservedDBInstancesOfferingId
    , rdioOfferingType
    , rdioUsagePrice
    , rdioRecurringCharges
    , rdioFixedPrice
    , rdioDuration

    -- ** ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions
    , resourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- ** Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

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

import           Network.AWS.RDS.AddSourceIdentifierToSubscription
import           Network.AWS.RDS.AddTagsToResource
import           Network.AWS.RDS.ApplyPendingMaintenanceAction
import           Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
import           Network.AWS.RDS.CopyDBClusterSnapshot
import           Network.AWS.RDS.CopyDBParameterGroup
import           Network.AWS.RDS.CopyDBSnapshot
import           Network.AWS.RDS.CopyOptionGroup
import           Network.AWS.RDS.CreateDBCluster
import           Network.AWS.RDS.CreateDBClusterParameterGroup
import           Network.AWS.RDS.CreateDBClusterSnapshot
import           Network.AWS.RDS.CreateDBInstance
import           Network.AWS.RDS.CreateDBInstanceReadReplica
import           Network.AWS.RDS.CreateDBParameterGroup
import           Network.AWS.RDS.CreateDBSecurityGroup
import           Network.AWS.RDS.CreateDBSnapshot
import           Network.AWS.RDS.CreateDBSubnetGroup
import           Network.AWS.RDS.CreateEventSubscription
import           Network.AWS.RDS.CreateOptionGroup
import           Network.AWS.RDS.DeleteDBCluster
import           Network.AWS.RDS.DeleteDBClusterParameterGroup
import           Network.AWS.RDS.DeleteDBClusterSnapshot
import           Network.AWS.RDS.DeleteDBInstance
import           Network.AWS.RDS.DeleteDBParameterGroup
import           Network.AWS.RDS.DeleteDBSecurityGroup
import           Network.AWS.RDS.DeleteDBSnapshot
import           Network.AWS.RDS.DeleteDBSubnetGroup
import           Network.AWS.RDS.DeleteEventSubscription
import           Network.AWS.RDS.DeleteOptionGroup
import           Network.AWS.RDS.DescribeAccountAttributes
import           Network.AWS.RDS.DescribeCertificates
import           Network.AWS.RDS.DescribeDBClusterParameterGroups
import           Network.AWS.RDS.DescribeDBClusterParameters
import           Network.AWS.RDS.DescribeDBClusters
import           Network.AWS.RDS.DescribeDBClusterSnapshots
import           Network.AWS.RDS.DescribeDBEngineVersions
import           Network.AWS.RDS.DescribeDBInstances
import           Network.AWS.RDS.DescribeDBLogFiles
import           Network.AWS.RDS.DescribeDBParameterGroups
import           Network.AWS.RDS.DescribeDBParameters
import           Network.AWS.RDS.DescribeDBSecurityGroups
import           Network.AWS.RDS.DescribeDBSnapshots
import           Network.AWS.RDS.DescribeDBSubnetGroups
import           Network.AWS.RDS.DescribeEngineDefaultClusterParameters
import           Network.AWS.RDS.DescribeEngineDefaultParameters
import           Network.AWS.RDS.DescribeEventCategories
import           Network.AWS.RDS.DescribeEvents
import           Network.AWS.RDS.DescribeEventSubscriptions
import           Network.AWS.RDS.DescribeOptionGroupOptions
import           Network.AWS.RDS.DescribeOptionGroups
import           Network.AWS.RDS.DescribeOrderableDBInstanceOptions
import           Network.AWS.RDS.DescribePendingMaintenanceActions
import           Network.AWS.RDS.DescribeReservedDBInstances
import           Network.AWS.RDS.DescribeReservedDBInstancesOfferings
import           Network.AWS.RDS.DownloadDBLogFilePortion
import           Network.AWS.RDS.FailoverDBCluster
import           Network.AWS.RDS.ListTagsForResource
import           Network.AWS.RDS.ModifyDBCluster
import           Network.AWS.RDS.ModifyDBClusterParameterGroup
import           Network.AWS.RDS.ModifyDBInstance
import           Network.AWS.RDS.ModifyDBParameterGroup
import           Network.AWS.RDS.ModifyDBSubnetGroup
import           Network.AWS.RDS.ModifyEventSubscription
import           Network.AWS.RDS.ModifyOptionGroup
import           Network.AWS.RDS.PromoteReadReplica
import           Network.AWS.RDS.PurchaseReservedDBInstancesOffering
import           Network.AWS.RDS.RebootDBInstance
import           Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
import           Network.AWS.RDS.RemoveTagsFromResource
import           Network.AWS.RDS.ResetDBClusterParameterGroup
import           Network.AWS.RDS.ResetDBParameterGroup
import           Network.AWS.RDS.RestoreDBClusterFromSnapshot
import           Network.AWS.RDS.RestoreDBClusterToPointInTime
import           Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
import           Network.AWS.RDS.RestoreDBInstanceToPointInTime
import           Network.AWS.RDS.RevokeDBSecurityGroupIngress
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'RDS'.
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
