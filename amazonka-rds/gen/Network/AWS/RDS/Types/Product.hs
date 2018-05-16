{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.Sum

-- | Describes a quota for an AWS account, for example, the number of DB instances allowed.
--
--
--
-- /See:/ 'accountQuota' smart constructor.
data AccountQuota = AccountQuota'
  { _aqMax              :: !(Maybe Integer)
  , _aqUsed             :: !(Maybe Integer)
  , _aqAccountQuotaName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqMax' - The maximum allowed value for the quota.
--
-- * 'aqUsed' - The amount currently used toward the quota maximum.
--
-- * 'aqAccountQuotaName' - The name of the Amazon RDS quota for this AWS account.
accountQuota
    :: AccountQuota
accountQuota =
  AccountQuota'
    {_aqMax = Nothing, _aqUsed = Nothing, _aqAccountQuotaName = Nothing}


-- | The maximum allowed value for the quota.
aqMax :: Lens' AccountQuota (Maybe Integer)
aqMax = lens _aqMax (\ s a -> s{_aqMax = a})

-- | The amount currently used toward the quota maximum.
aqUsed :: Lens' AccountQuota (Maybe Integer)
aqUsed = lens _aqUsed (\ s a -> s{_aqUsed = a})

-- | The name of the Amazon RDS quota for this AWS account.
aqAccountQuotaName :: Lens' AccountQuota (Maybe Text)
aqAccountQuotaName = lens _aqAccountQuotaName (\ s a -> s{_aqAccountQuotaName = a})

instance FromXML AccountQuota where
        parseXML x
          = AccountQuota' <$>
              (x .@? "Max") <*> (x .@? "Used") <*>
                (x .@? "AccountQuotaName")

instance Hashable AccountQuota where

instance NFData AccountQuota where

-- | Contains Availability Zone information.
--
--
-- This data type is used as an element in the following data type:
--
--     * 'OrderableDBInstanceOption'
--
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
  { _azName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName' - The name of the availability zone.
availabilityZone
    :: AvailabilityZone
availabilityZone = AvailabilityZone' {_azName = Nothing}


-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a})

instance FromXML AvailabilityZone where
        parseXML x = AvailabilityZone' <$> (x .@? "Name")

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where

-- | A CA certificate for an AWS account.
--
--
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
  { _cCertificateType       :: !(Maybe Text)
  , _cCertificateARN        :: !(Maybe Text)
  , _cValidTill             :: !(Maybe ISO8601)
  , _cCertificateIdentifier :: !(Maybe Text)
  , _cThumbprint            :: !(Maybe Text)
  , _cValidFrom             :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateType' - The type of the certificate.
--
-- * 'cCertificateARN' - The Amazon Resource Name (ARN) for the certificate.
--
-- * 'cValidTill' - The final date that the certificate continues to be valid.
--
-- * 'cCertificateIdentifier' - The unique key that identifies a certificate.
--
-- * 'cThumbprint' - The thumbprint of the certificate.
--
-- * 'cValidFrom' - The starting date from which the certificate is valid.
certificate
    :: Certificate
certificate =
  Certificate'
    { _cCertificateType = Nothing
    , _cCertificateARN = Nothing
    , _cValidTill = Nothing
    , _cCertificateIdentifier = Nothing
    , _cThumbprint = Nothing
    , _cValidFrom = Nothing
    }


-- | The type of the certificate.
cCertificateType :: Lens' Certificate (Maybe Text)
cCertificateType = lens _cCertificateType (\ s a -> s{_cCertificateType = a})

-- | The Amazon Resource Name (ARN) for the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a})

-- | The final date that the certificate continues to be valid.
cValidTill :: Lens' Certificate (Maybe UTCTime)
cValidTill = lens _cValidTill (\ s a -> s{_cValidTill = a}) . mapping _Time

-- | The unique key that identifies a certificate.
cCertificateIdentifier :: Lens' Certificate (Maybe Text)
cCertificateIdentifier = lens _cCertificateIdentifier (\ s a -> s{_cCertificateIdentifier = a})

-- | The thumbprint of the certificate.
cThumbprint :: Lens' Certificate (Maybe Text)
cThumbprint = lens _cThumbprint (\ s a -> s{_cThumbprint = a})

-- | The starting date from which the certificate is valid.
cValidFrom :: Lens' Certificate (Maybe UTCTime)
cValidFrom = lens _cValidFrom (\ s a -> s{_cValidFrom = a}) . mapping _Time

instance FromXML Certificate where
        parseXML x
          = Certificate' <$>
              (x .@? "CertificateType") <*>
                (x .@? "CertificateArn")
                <*> (x .@? "ValidTill")
                <*> (x .@? "CertificateIdentifier")
                <*> (x .@? "Thumbprint")
                <*> (x .@? "ValidFrom")

instance Hashable Certificate where

instance NFData Certificate where

-- | This data type is used as a response element in the action 'DescribeDBEngineVersions' .
--
--
--
-- /See:/ 'characterSet' smart constructor.
data CharacterSet = CharacterSet'
  { _csCharacterSetName        :: !(Maybe Text)
  , _csCharacterSetDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CharacterSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCharacterSetName' - The name of the character set.
--
-- * 'csCharacterSetDescription' - The description of the character set.
characterSet
    :: CharacterSet
characterSet =
  CharacterSet'
    {_csCharacterSetName = Nothing, _csCharacterSetDescription = Nothing}


-- | The name of the character set.
csCharacterSetName :: Lens' CharacterSet (Maybe Text)
csCharacterSetName = lens _csCharacterSetName (\ s a -> s{_csCharacterSetName = a})

-- | The description of the character set.
csCharacterSetDescription :: Lens' CharacterSet (Maybe Text)
csCharacterSetDescription = lens _csCharacterSetDescription (\ s a -> s{_csCharacterSetDescription = a})

instance FromXML CharacterSet where
        parseXML x
          = CharacterSet' <$>
              (x .@? "CharacterSetName") <*>
                (x .@? "CharacterSetDescription")

instance Hashable CharacterSet where

instance NFData CharacterSet where

-- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB instance or DB cluster.
--
--
--
-- /See:/ 'cloudwatchLogsExportConfiguration' smart constructor.
data CloudwatchLogsExportConfiguration = CloudwatchLogsExportConfiguration'
  { _clecDisableLogTypes :: !(Maybe [Text])
  , _clecEnableLogTypes  :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudwatchLogsExportConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clecDisableLogTypes' - The list of log types to disable.
--
-- * 'clecEnableLogTypes' - The list of log types to enable.
cloudwatchLogsExportConfiguration
    :: CloudwatchLogsExportConfiguration
cloudwatchLogsExportConfiguration =
  CloudwatchLogsExportConfiguration'
    {_clecDisableLogTypes = Nothing, _clecEnableLogTypes = Nothing}


-- | The list of log types to disable.
clecDisableLogTypes :: Lens' CloudwatchLogsExportConfiguration [Text]
clecDisableLogTypes = lens _clecDisableLogTypes (\ s a -> s{_clecDisableLogTypes = a}) . _Default . _Coerce

-- | The list of log types to enable.
clecEnableLogTypes :: Lens' CloudwatchLogsExportConfiguration [Text]
clecEnableLogTypes = lens _clecEnableLogTypes (\ s a -> s{_clecEnableLogTypes = a}) . _Default . _Coerce

instance Hashable CloudwatchLogsExportConfiguration
         where

instance NFData CloudwatchLogsExportConfiguration
         where

instance ToQuery CloudwatchLogsExportConfiguration
         where
        toQuery CloudwatchLogsExportConfiguration'{..}
          = mconcat
              ["DisableLogTypes" =:
                 toQuery
                   (toQueryList "member" <$> _clecDisableLogTypes),
               "EnableLogTypes" =:
                 toQuery
                   (toQueryList "member" <$> _clecEnableLogTypes)]

-- | Contains the details of an Amazon RDS DB cluster.
--
--
-- This data type is used as a response element in the 'DescribeDBClusters' action.
--
--
-- /See:/ 'dbCluster' smart constructor.
data DBCluster = DBCluster'
  { _dcBacktrackConsumedChangeRecords   :: !(Maybe Integer)
  , _dcEngineVersion                    :: !(Maybe Text)
  , _dcStatus                           :: !(Maybe Text)
  , _dcStorageEncrypted                 :: !(Maybe Bool)
  , _dcDBClusterIdentifier              :: !(Maybe Text)
  , _dcDBClusterMembers                 :: !(Maybe [DBClusterMember])
  , _dcReadReplicaIdentifiers           :: !(Maybe [Text])
  , _dcReplicationSourceIdentifier      :: !(Maybe Text)
  , _dcHostedZoneId                     :: !(Maybe Text)
  , _dcDBClusterParameterGroup          :: !(Maybe Text)
  , _dcMasterUsername                   :: !(Maybe Text)
  , _dcIAMDatabaseAuthenticationEnabled :: !(Maybe Bool)
  , _dcEarliestBacktrackTime            :: !(Maybe ISO8601)
  , _dcBacktrackWindow                  :: !(Maybe Integer)
  , _dcDBClusterResourceId              :: !(Maybe Text)
  , _dcEarliestRestorableTime           :: !(Maybe ISO8601)
  , _dcEngine                           :: !(Maybe Text)
  , _dcDBClusterARN                     :: !(Maybe Text)
  , _dcCloneGroupId                     :: !(Maybe Text)
  , _dcLatestRestorableTime             :: !(Maybe ISO8601)
  , _dcPreferredMaintenanceWindow       :: !(Maybe Text)
  , _dcAvailabilityZones                :: !(Maybe [Text])
  , _dcCharacterSetName                 :: !(Maybe Text)
  , _dcKMSKeyId                         :: !(Maybe Text)
  , _dcPreferredBackupWindow            :: !(Maybe Text)
  , _dcAssociatedRoles                  :: !(Maybe [DBClusterRole])
  , _dcVPCSecurityGroups                :: !(Maybe [VPCSecurityGroupMembership])
  , _dcBackupRetentionPeriod            :: !(Maybe Int)
  , _dcDBSubnetGroup                    :: !(Maybe Text)
  , _dcDatabaseName                     :: !(Maybe Text)
  , _dcMultiAZ                          :: !(Maybe Bool)
  , _dcAllocatedStorage                 :: !(Maybe Int)
  , _dcClusterCreateTime                :: !(Maybe ISO8601)
  , _dcEndpoint                         :: !(Maybe Text)
  , _dcPercentProgress                  :: !(Maybe Text)
  , _dcReaderEndpoint                   :: !(Maybe Text)
  , _dcPort                             :: !(Maybe Int)
  , _dcDBClusterOptionGroupMemberships  :: !(Maybe [DBClusterOptionGroupStatus])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcBacktrackConsumedChangeRecords' - The number of change records stored for Backtrack.
--
-- * 'dcEngineVersion' - Indicates the database engine version.
--
-- * 'dcStatus' - Specifies the current state of this DB cluster.
--
-- * 'dcStorageEncrypted' - Specifies whether the DB cluster is encrypted.
--
-- * 'dcDBClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- * 'dcDBClusterMembers' - Provides the list of instances that make up the DB cluster.
--
-- * 'dcReadReplicaIdentifiers' - Contains one or more identifiers of the Read Replicas associated with this DB cluster.
--
-- * 'dcReplicationSourceIdentifier' - Contains the identifier of the source DB cluster if this DB cluster is a Read Replica.
--
-- * 'dcHostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- * 'dcDBClusterParameterGroup' - Specifies the name of the DB cluster parameter group for the DB cluster.
--
-- * 'dcMasterUsername' - Contains the master username for the DB cluster.
--
-- * 'dcIAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- * 'dcEarliestBacktrackTime' - The earliest time to which a DB cluster can be backtracked.
--
-- * 'dcBacktrackWindow' - The target backtrack window, in seconds. If this value is set to 0, backtracking is disabled for the DB cluster. Otherwise, backtracking is enabled.
--
-- * 'dcDBClusterResourceId' - The AWS Region-unique, immutable identifier for the DB cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
--
-- * 'dcEarliestRestorableTime' - The earliest time to which a database can be restored with point-in-time restore.
--
-- * 'dcEngine' - Provides the name of the database engine to be used for this DB cluster.
--
-- * 'dcDBClusterARN' - The Amazon Resource Name (ARN) for the DB cluster.
--
-- * 'dcCloneGroupId' - Identifies the clone group to which the DB cluster is associated.
--
-- * 'dcLatestRestorableTime' - Specifies the latest time to which a database can be restored with point-in-time restore.
--
-- * 'dcPreferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- * 'dcAvailabilityZones' - Provides the list of EC2 Availability Zones that instances in the DB cluster can be created in.
--
-- * 'dcCharacterSetName' - If present, specifies the name of the character set that this cluster is associated with.
--
-- * 'dcKMSKeyId' - If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster.
--
-- * 'dcPreferredBackupWindow' - Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
--
-- * 'dcAssociatedRoles' - Provides a list of the AWS Identity and Access Management (IAM) roles that are associated with the DB cluster. IAM roles that are associated with a DB cluster grant permission for the DB cluster to access other AWS services on your behalf.
--
-- * 'dcVPCSecurityGroups' - Provides a list of VPC security groups that the DB cluster belongs to.
--
-- * 'dcBackupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are retained.
--
-- * 'dcDBSubnetGroup' - Specifies information on the subnet group associated with the DB cluster, including the name, description, and subnets in the subnet group.
--
-- * 'dcDatabaseName' - Contains the name of the initial database of this DB cluster that was provided at create time, if one was specified when the DB cluster was created. This same name is returned for the life of the DB cluster.
--
-- * 'dcMultiAZ' - Specifies whether the DB cluster has instances in multiple Availability Zones.
--
-- * 'dcAllocatedStorage' - For all database engines except Amazon Aurora, @AllocatedStorage@ specifies the allocated storage size in gibibytes (GiB). For Aurora, @AllocatedStorage@ always returns 1, because Aurora DB cluster storage size is not fixed, but instead automatically adjusts as needed.
--
-- * 'dcClusterCreateTime' - Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- * 'dcEndpoint' - Specifies the connection endpoint for the primary instance of the DB cluster.
--
-- * 'dcPercentProgress' - Specifies the progress of the operation as a percentage.
--
-- * 'dcReaderEndpoint' - The reader endpoint for the DB cluster. The reader endpoint for a DB cluster load-balances connections across the Aurora Replicas that are available in a DB cluster. As clients request new connections to the reader endpoint, Aurora distributes the connection requests among the Aurora Replicas in the DB cluster. This functionality can help balance your read workload across multiple Aurora Replicas in your DB cluster.  If a failover occurs, and the Aurora Replica that you are connected to is promoted to be the primary instance, your connection is dropped. To continue sending your read workload to other Aurora Replicas in the cluster, you can then reconnect to the reader endpoint.
--
-- * 'dcPort' - Specifies the port that the database engine is listening on.
--
-- * 'dcDBClusterOptionGroupMemberships' - Provides the list of option group memberships for this DB cluster.
dbCluster
    :: DBCluster
dbCluster =
  DBCluster'
    { _dcBacktrackConsumedChangeRecords = Nothing
    , _dcEngineVersion = Nothing
    , _dcStatus = Nothing
    , _dcStorageEncrypted = Nothing
    , _dcDBClusterIdentifier = Nothing
    , _dcDBClusterMembers = Nothing
    , _dcReadReplicaIdentifiers = Nothing
    , _dcReplicationSourceIdentifier = Nothing
    , _dcHostedZoneId = Nothing
    , _dcDBClusterParameterGroup = Nothing
    , _dcMasterUsername = Nothing
    , _dcIAMDatabaseAuthenticationEnabled = Nothing
    , _dcEarliestBacktrackTime = Nothing
    , _dcBacktrackWindow = Nothing
    , _dcDBClusterResourceId = Nothing
    , _dcEarliestRestorableTime = Nothing
    , _dcEngine = Nothing
    , _dcDBClusterARN = Nothing
    , _dcCloneGroupId = Nothing
    , _dcLatestRestorableTime = Nothing
    , _dcPreferredMaintenanceWindow = Nothing
    , _dcAvailabilityZones = Nothing
    , _dcCharacterSetName = Nothing
    , _dcKMSKeyId = Nothing
    , _dcPreferredBackupWindow = Nothing
    , _dcAssociatedRoles = Nothing
    , _dcVPCSecurityGroups = Nothing
    , _dcBackupRetentionPeriod = Nothing
    , _dcDBSubnetGroup = Nothing
    , _dcDatabaseName = Nothing
    , _dcMultiAZ = Nothing
    , _dcAllocatedStorage = Nothing
    , _dcClusterCreateTime = Nothing
    , _dcEndpoint = Nothing
    , _dcPercentProgress = Nothing
    , _dcReaderEndpoint = Nothing
    , _dcPort = Nothing
    , _dcDBClusterOptionGroupMemberships = Nothing
    }


-- | The number of change records stored for Backtrack.
dcBacktrackConsumedChangeRecords :: Lens' DBCluster (Maybe Integer)
dcBacktrackConsumedChangeRecords = lens _dcBacktrackConsumedChangeRecords (\ s a -> s{_dcBacktrackConsumedChangeRecords = a})

-- | Indicates the database engine version.
dcEngineVersion :: Lens' DBCluster (Maybe Text)
dcEngineVersion = lens _dcEngineVersion (\ s a -> s{_dcEngineVersion = a})

-- | Specifies the current state of this DB cluster.
dcStatus :: Lens' DBCluster (Maybe Text)
dcStatus = lens _dcStatus (\ s a -> s{_dcStatus = a})

-- | Specifies whether the DB cluster is encrypted.
dcStorageEncrypted :: Lens' DBCluster (Maybe Bool)
dcStorageEncrypted = lens _dcStorageEncrypted (\ s a -> s{_dcStorageEncrypted = a})

-- | Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
dcDBClusterIdentifier :: Lens' DBCluster (Maybe Text)
dcDBClusterIdentifier = lens _dcDBClusterIdentifier (\ s a -> s{_dcDBClusterIdentifier = a})

-- | Provides the list of instances that make up the DB cluster.
dcDBClusterMembers :: Lens' DBCluster [DBClusterMember]
dcDBClusterMembers = lens _dcDBClusterMembers (\ s a -> s{_dcDBClusterMembers = a}) . _Default . _Coerce

-- | Contains one or more identifiers of the Read Replicas associated with this DB cluster.
dcReadReplicaIdentifiers :: Lens' DBCluster [Text]
dcReadReplicaIdentifiers = lens _dcReadReplicaIdentifiers (\ s a -> s{_dcReadReplicaIdentifiers = a}) . _Default . _Coerce

-- | Contains the identifier of the source DB cluster if this DB cluster is a Read Replica.
dcReplicationSourceIdentifier :: Lens' DBCluster (Maybe Text)
dcReplicationSourceIdentifier = lens _dcReplicationSourceIdentifier (\ s a -> s{_dcReplicationSourceIdentifier = a})

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
dcHostedZoneId :: Lens' DBCluster (Maybe Text)
dcHostedZoneId = lens _dcHostedZoneId (\ s a -> s{_dcHostedZoneId = a})

-- | Specifies the name of the DB cluster parameter group for the DB cluster.
dcDBClusterParameterGroup :: Lens' DBCluster (Maybe Text)
dcDBClusterParameterGroup = lens _dcDBClusterParameterGroup (\ s a -> s{_dcDBClusterParameterGroup = a})

-- | Contains the master username for the DB cluster.
dcMasterUsername :: Lens' DBCluster (Maybe Text)
dcMasterUsername = lens _dcMasterUsername (\ s a -> s{_dcMasterUsername = a})

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
dcIAMDatabaseAuthenticationEnabled :: Lens' DBCluster (Maybe Bool)
dcIAMDatabaseAuthenticationEnabled = lens _dcIAMDatabaseAuthenticationEnabled (\ s a -> s{_dcIAMDatabaseAuthenticationEnabled = a})

-- | The earliest time to which a DB cluster can be backtracked.
dcEarliestBacktrackTime :: Lens' DBCluster (Maybe UTCTime)
dcEarliestBacktrackTime = lens _dcEarliestBacktrackTime (\ s a -> s{_dcEarliestBacktrackTime = a}) . mapping _Time

-- | The target backtrack window, in seconds. If this value is set to 0, backtracking is disabled for the DB cluster. Otherwise, backtracking is enabled.
dcBacktrackWindow :: Lens' DBCluster (Maybe Integer)
dcBacktrackWindow = lens _dcBacktrackWindow (\ s a -> s{_dcBacktrackWindow = a})

-- | The AWS Region-unique, immutable identifier for the DB cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
dcDBClusterResourceId :: Lens' DBCluster (Maybe Text)
dcDBClusterResourceId = lens _dcDBClusterResourceId (\ s a -> s{_dcDBClusterResourceId = a})

-- | The earliest time to which a database can be restored with point-in-time restore.
dcEarliestRestorableTime :: Lens' DBCluster (Maybe UTCTime)
dcEarliestRestorableTime = lens _dcEarliestRestorableTime (\ s a -> s{_dcEarliestRestorableTime = a}) . mapping _Time

-- | Provides the name of the database engine to be used for this DB cluster.
dcEngine :: Lens' DBCluster (Maybe Text)
dcEngine = lens _dcEngine (\ s a -> s{_dcEngine = a})

-- | The Amazon Resource Name (ARN) for the DB cluster.
dcDBClusterARN :: Lens' DBCluster (Maybe Text)
dcDBClusterARN = lens _dcDBClusterARN (\ s a -> s{_dcDBClusterARN = a})

-- | Identifies the clone group to which the DB cluster is associated.
dcCloneGroupId :: Lens' DBCluster (Maybe Text)
dcCloneGroupId = lens _dcCloneGroupId (\ s a -> s{_dcCloneGroupId = a})

-- | Specifies the latest time to which a database can be restored with point-in-time restore.
dcLatestRestorableTime :: Lens' DBCluster (Maybe UTCTime)
dcLatestRestorableTime = lens _dcLatestRestorableTime (\ s a -> s{_dcLatestRestorableTime = a}) . mapping _Time

-- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
dcPreferredMaintenanceWindow :: Lens' DBCluster (Maybe Text)
dcPreferredMaintenanceWindow = lens _dcPreferredMaintenanceWindow (\ s a -> s{_dcPreferredMaintenanceWindow = a})

-- | Provides the list of EC2 Availability Zones that instances in the DB cluster can be created in.
dcAvailabilityZones :: Lens' DBCluster [Text]
dcAvailabilityZones = lens _dcAvailabilityZones (\ s a -> s{_dcAvailabilityZones = a}) . _Default . _Coerce

-- | If present, specifies the name of the character set that this cluster is associated with.
dcCharacterSetName :: Lens' DBCluster (Maybe Text)
dcCharacterSetName = lens _dcCharacterSetName (\ s a -> s{_dcCharacterSetName = a})

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster.
dcKMSKeyId :: Lens' DBCluster (Maybe Text)
dcKMSKeyId = lens _dcKMSKeyId (\ s a -> s{_dcKMSKeyId = a})

-- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
dcPreferredBackupWindow :: Lens' DBCluster (Maybe Text)
dcPreferredBackupWindow = lens _dcPreferredBackupWindow (\ s a -> s{_dcPreferredBackupWindow = a})

-- | Provides a list of the AWS Identity and Access Management (IAM) roles that are associated with the DB cluster. IAM roles that are associated with a DB cluster grant permission for the DB cluster to access other AWS services on your behalf.
dcAssociatedRoles :: Lens' DBCluster [DBClusterRole]
dcAssociatedRoles = lens _dcAssociatedRoles (\ s a -> s{_dcAssociatedRoles = a}) . _Default . _Coerce

-- | Provides a list of VPC security groups that the DB cluster belongs to.
dcVPCSecurityGroups :: Lens' DBCluster [VPCSecurityGroupMembership]
dcVPCSecurityGroups = lens _dcVPCSecurityGroups (\ s a -> s{_dcVPCSecurityGroups = a}) . _Default . _Coerce

-- | Specifies the number of days for which automatic DB snapshots are retained.
dcBackupRetentionPeriod :: Lens' DBCluster (Maybe Int)
dcBackupRetentionPeriod = lens _dcBackupRetentionPeriod (\ s a -> s{_dcBackupRetentionPeriod = a})

-- | Specifies information on the subnet group associated with the DB cluster, including the name, description, and subnets in the subnet group.
dcDBSubnetGroup :: Lens' DBCluster (Maybe Text)
dcDBSubnetGroup = lens _dcDBSubnetGroup (\ s a -> s{_dcDBSubnetGroup = a})

-- | Contains the name of the initial database of this DB cluster that was provided at create time, if one was specified when the DB cluster was created. This same name is returned for the life of the DB cluster.
dcDatabaseName :: Lens' DBCluster (Maybe Text)
dcDatabaseName = lens _dcDatabaseName (\ s a -> s{_dcDatabaseName = a})

-- | Specifies whether the DB cluster has instances in multiple Availability Zones.
dcMultiAZ :: Lens' DBCluster (Maybe Bool)
dcMultiAZ = lens _dcMultiAZ (\ s a -> s{_dcMultiAZ = a})

-- | For all database engines except Amazon Aurora, @AllocatedStorage@ specifies the allocated storage size in gibibytes (GiB). For Aurora, @AllocatedStorage@ always returns 1, because Aurora DB cluster storage size is not fixed, but instead automatically adjusts as needed.
dcAllocatedStorage :: Lens' DBCluster (Maybe Int)
dcAllocatedStorage = lens _dcAllocatedStorage (\ s a -> s{_dcAllocatedStorage = a})

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
dcClusterCreateTime :: Lens' DBCluster (Maybe UTCTime)
dcClusterCreateTime = lens _dcClusterCreateTime (\ s a -> s{_dcClusterCreateTime = a}) . mapping _Time

-- | Specifies the connection endpoint for the primary instance of the DB cluster.
dcEndpoint :: Lens' DBCluster (Maybe Text)
dcEndpoint = lens _dcEndpoint (\ s a -> s{_dcEndpoint = a})

-- | Specifies the progress of the operation as a percentage.
dcPercentProgress :: Lens' DBCluster (Maybe Text)
dcPercentProgress = lens _dcPercentProgress (\ s a -> s{_dcPercentProgress = a})

-- | The reader endpoint for the DB cluster. The reader endpoint for a DB cluster load-balances connections across the Aurora Replicas that are available in a DB cluster. As clients request new connections to the reader endpoint, Aurora distributes the connection requests among the Aurora Replicas in the DB cluster. This functionality can help balance your read workload across multiple Aurora Replicas in your DB cluster.  If a failover occurs, and the Aurora Replica that you are connected to is promoted to be the primary instance, your connection is dropped. To continue sending your read workload to other Aurora Replicas in the cluster, you can then reconnect to the reader endpoint.
dcReaderEndpoint :: Lens' DBCluster (Maybe Text)
dcReaderEndpoint = lens _dcReaderEndpoint (\ s a -> s{_dcReaderEndpoint = a})

-- | Specifies the port that the database engine is listening on.
dcPort :: Lens' DBCluster (Maybe Int)
dcPort = lens _dcPort (\ s a -> s{_dcPort = a})

-- | Provides the list of option group memberships for this DB cluster.
dcDBClusterOptionGroupMemberships :: Lens' DBCluster [DBClusterOptionGroupStatus]
dcDBClusterOptionGroupMemberships = lens _dcDBClusterOptionGroupMemberships (\ s a -> s{_dcDBClusterOptionGroupMemberships = a}) . _Default . _Coerce

instance FromXML DBCluster where
        parseXML x
          = DBCluster' <$>
              (x .@? "BacktrackConsumedChangeRecords") <*>
                (x .@? "EngineVersion")
                <*> (x .@? "Status")
                <*> (x .@? "StorageEncrypted")
                <*> (x .@? "DBClusterIdentifier")
                <*>
                (x .@? "DBClusterMembers" .!@ mempty >>=
                   may (parseXMLList "DBClusterMember"))
                <*>
                (x .@? "ReadReplicaIdentifiers" .!@ mempty >>=
                   may (parseXMLList "ReadReplicaIdentifier"))
                <*> (x .@? "ReplicationSourceIdentifier")
                <*> (x .@? "HostedZoneId")
                <*> (x .@? "DBClusterParameterGroup")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "IAMDatabaseAuthenticationEnabled")
                <*> (x .@? "EarliestBacktrackTime")
                <*> (x .@? "BacktrackWindow")
                <*> (x .@? "DbClusterResourceId")
                <*> (x .@? "EarliestRestorableTime")
                <*> (x .@? "Engine")
                <*> (x .@? "DBClusterArn")
                <*> (x .@? "CloneGroupId")
                <*> (x .@? "LatestRestorableTime")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "CharacterSetName")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "PreferredBackupWindow")
                <*>
                (x .@? "AssociatedRoles" .!@ mempty >>=
                   may (parseXMLList "DBClusterRole"))
                <*>
                (x .@? "VpcSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroupMembership"))
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "DBSubnetGroup")
                <*> (x .@? "DatabaseName")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "ClusterCreateTime")
                <*> (x .@? "Endpoint")
                <*> (x .@? "PercentProgress")
                <*> (x .@? "ReaderEndpoint")
                <*> (x .@? "Port")
                <*>
                (x .@? "DBClusterOptionGroupMemberships" .!@ mempty
                   >>= may (parseXMLList "DBClusterOptionGroup"))

instance Hashable DBCluster where

instance NFData DBCluster where

-- | This data type is used as a response element in the 'DescribeDBClusterBacktracks' action.
--
--
--
-- /See:/ 'dbClusterBacktrack' smart constructor.
data DBClusterBacktrack = DBClusterBacktrack'
  { _dcbStatus                       :: !(Maybe Text)
  , _dcbBacktrackIdentifier          :: !(Maybe Text)
  , _dcbBacktrackTo                  :: !(Maybe ISO8601)
  , _dcbDBClusterIdentifier          :: !(Maybe Text)
  , _dcbBacktrackedFrom              :: !(Maybe ISO8601)
  , _dcbBacktrackRequestCreationTime :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterBacktrack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbStatus' - The status of the backtrack. This property returns one of the following values:     * @applying@ - The backtrack is currently being applied to or rolled back from the DB cluster.     * @completed@ - The backtrack has successfully been applied to or rolled back from the DB cluster.     * @failed@ - An error occurred while the backtrack was applied to or rolled back from the DB cluster.     * @pending@ - The backtrack is currently pending application to or rollback from the DB cluster.
--
-- * 'dcbBacktrackIdentifier' - Contains the backtrack identifier.
--
-- * 'dcbBacktrackTo' - The timestamp of the time to which the DB cluster was backtracked.
--
-- * 'dcbDBClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- * 'dcbBacktrackedFrom' - The timestamp of the time from which the DB cluster was backtracked.
--
-- * 'dcbBacktrackRequestCreationTime' - The timestamp of the time at which the backtrack was requested.
dbClusterBacktrack
    :: DBClusterBacktrack
dbClusterBacktrack =
  DBClusterBacktrack'
    { _dcbStatus = Nothing
    , _dcbBacktrackIdentifier = Nothing
    , _dcbBacktrackTo = Nothing
    , _dcbDBClusterIdentifier = Nothing
    , _dcbBacktrackedFrom = Nothing
    , _dcbBacktrackRequestCreationTime = Nothing
    }


-- | The status of the backtrack. This property returns one of the following values:     * @applying@ - The backtrack is currently being applied to or rolled back from the DB cluster.     * @completed@ - The backtrack has successfully been applied to or rolled back from the DB cluster.     * @failed@ - An error occurred while the backtrack was applied to or rolled back from the DB cluster.     * @pending@ - The backtrack is currently pending application to or rollback from the DB cluster.
dcbStatus :: Lens' DBClusterBacktrack (Maybe Text)
dcbStatus = lens _dcbStatus (\ s a -> s{_dcbStatus = a})

-- | Contains the backtrack identifier.
dcbBacktrackIdentifier :: Lens' DBClusterBacktrack (Maybe Text)
dcbBacktrackIdentifier = lens _dcbBacktrackIdentifier (\ s a -> s{_dcbBacktrackIdentifier = a})

-- | The timestamp of the time to which the DB cluster was backtracked.
dcbBacktrackTo :: Lens' DBClusterBacktrack (Maybe UTCTime)
dcbBacktrackTo = lens _dcbBacktrackTo (\ s a -> s{_dcbBacktrackTo = a}) . mapping _Time

-- | Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
dcbDBClusterIdentifier :: Lens' DBClusterBacktrack (Maybe Text)
dcbDBClusterIdentifier = lens _dcbDBClusterIdentifier (\ s a -> s{_dcbDBClusterIdentifier = a})

-- | The timestamp of the time from which the DB cluster was backtracked.
dcbBacktrackedFrom :: Lens' DBClusterBacktrack (Maybe UTCTime)
dcbBacktrackedFrom = lens _dcbBacktrackedFrom (\ s a -> s{_dcbBacktrackedFrom = a}) . mapping _Time

-- | The timestamp of the time at which the backtrack was requested.
dcbBacktrackRequestCreationTime :: Lens' DBClusterBacktrack (Maybe UTCTime)
dcbBacktrackRequestCreationTime = lens _dcbBacktrackRequestCreationTime (\ s a -> s{_dcbBacktrackRequestCreationTime = a}) . mapping _Time

instance FromXML DBClusterBacktrack where
        parseXML x
          = DBClusterBacktrack' <$>
              (x .@? "Status") <*> (x .@? "BacktrackIdentifier")
                <*> (x .@? "BacktrackTo")
                <*> (x .@? "DBClusterIdentifier")
                <*> (x .@? "BacktrackedFrom")
                <*> (x .@? "BacktrackRequestCreationTime")

instance Hashable DBClusterBacktrack where

instance NFData DBClusterBacktrack where

-- | Contains information about an instance that is part of a DB cluster.
--
--
--
-- /See:/ 'dbClusterMember' smart constructor.
data DBClusterMember = DBClusterMember'
  { _dcmPromotionTier                 :: !(Maybe Int)
  , _dcmDBInstanceIdentifier          :: !(Maybe Text)
  , _dcmIsClusterWriter               :: !(Maybe Bool)
  , _dcmDBClusterParameterGroupStatus :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcmPromotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Managing.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> .
--
-- * 'dcmDBInstanceIdentifier' - Specifies the instance identifier for this member of the DB cluster.
--
-- * 'dcmIsClusterWriter' - Value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
--
-- * 'dcmDBClusterParameterGroupStatus' - Specifies the status of the DB cluster parameter group for this member of the DB cluster.
dbClusterMember
    :: DBClusterMember
dbClusterMember =
  DBClusterMember'
    { _dcmPromotionTier = Nothing
    , _dcmDBInstanceIdentifier = Nothing
    , _dcmIsClusterWriter = Nothing
    , _dcmDBClusterParameterGroupStatus = Nothing
    }


-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Managing.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> .
dcmPromotionTier :: Lens' DBClusterMember (Maybe Int)
dcmPromotionTier = lens _dcmPromotionTier (\ s a -> s{_dcmPromotionTier = a})

-- | Specifies the instance identifier for this member of the DB cluster.
dcmDBInstanceIdentifier :: Lens' DBClusterMember (Maybe Text)
dcmDBInstanceIdentifier = lens _dcmDBInstanceIdentifier (\ s a -> s{_dcmDBInstanceIdentifier = a})

-- | Value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
dcmIsClusterWriter :: Lens' DBClusterMember (Maybe Bool)
dcmIsClusterWriter = lens _dcmIsClusterWriter (\ s a -> s{_dcmIsClusterWriter = a})

-- | Specifies the status of the DB cluster parameter group for this member of the DB cluster.
dcmDBClusterParameterGroupStatus :: Lens' DBClusterMember (Maybe Text)
dcmDBClusterParameterGroupStatus = lens _dcmDBClusterParameterGroupStatus (\ s a -> s{_dcmDBClusterParameterGroupStatus = a})

instance FromXML DBClusterMember where
        parseXML x
          = DBClusterMember' <$>
              (x .@? "PromotionTier") <*>
                (x .@? "DBInstanceIdentifier")
                <*> (x .@? "IsClusterWriter")
                <*> (x .@? "DBClusterParameterGroupStatus")

instance Hashable DBClusterMember where

instance NFData DBClusterMember where

-- | Contains status information for a DB cluster option group.
--
--
--
-- /See:/ 'dbClusterOptionGroupStatus' smart constructor.
data DBClusterOptionGroupStatus = DBClusterOptionGroupStatus'
  { _dcogsStatus                   :: !(Maybe Text)
  , _dcogsDBClusterOptionGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterOptionGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcogsStatus' - Specifies the status of the DB cluster option group.
--
-- * 'dcogsDBClusterOptionGroupName' - Specifies the name of the DB cluster option group.
dbClusterOptionGroupStatus
    :: DBClusterOptionGroupStatus
dbClusterOptionGroupStatus =
  DBClusterOptionGroupStatus'
    {_dcogsStatus = Nothing, _dcogsDBClusterOptionGroupName = Nothing}


-- | Specifies the status of the DB cluster option group.
dcogsStatus :: Lens' DBClusterOptionGroupStatus (Maybe Text)
dcogsStatus = lens _dcogsStatus (\ s a -> s{_dcogsStatus = a})

-- | Specifies the name of the DB cluster option group.
dcogsDBClusterOptionGroupName :: Lens' DBClusterOptionGroupStatus (Maybe Text)
dcogsDBClusterOptionGroupName = lens _dcogsDBClusterOptionGroupName (\ s a -> s{_dcogsDBClusterOptionGroupName = a})

instance FromXML DBClusterOptionGroupStatus where
        parseXML x
          = DBClusterOptionGroupStatus' <$>
              (x .@? "Status") <*>
                (x .@? "DBClusterOptionGroupName")

instance Hashable DBClusterOptionGroupStatus where

instance NFData DBClusterOptionGroupStatus where

-- | Contains the details of an Amazon RDS DB cluster parameter group.
--
--
-- This data type is used as a response element in the 'DescribeDBClusterParameterGroups' action.
--
--
-- /See:/ 'dbClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
  { _dcpgDBClusterParameterGroupARN  :: !(Maybe Text)
  , _dcpgDBParameterGroupFamily      :: !(Maybe Text)
  , _dcpgDBClusterParameterGroupName :: !(Maybe Text)
  , _dcpgDescription                 :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgDBClusterParameterGroupARN' - The Amazon Resource Name (ARN) for the DB cluster parameter group.
--
-- * 'dcpgDBParameterGroupFamily' - Provides the name of the DB parameter group family that this DB cluster parameter group is compatible with.
--
-- * 'dcpgDBClusterParameterGroupName' - Provides the name of the DB cluster parameter group.
--
-- * 'dcpgDescription' - Provides the customer-specified description for this DB cluster parameter group.
dbClusterParameterGroup
    :: DBClusterParameterGroup
dbClusterParameterGroup =
  DBClusterParameterGroup'
    { _dcpgDBClusterParameterGroupARN = Nothing
    , _dcpgDBParameterGroupFamily = Nothing
    , _dcpgDBClusterParameterGroupName = Nothing
    , _dcpgDescription = Nothing
    }


-- | The Amazon Resource Name (ARN) for the DB cluster parameter group.
dcpgDBClusterParameterGroupARN :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBClusterParameterGroupARN = lens _dcpgDBClusterParameterGroupARN (\ s a -> s{_dcpgDBClusterParameterGroupARN = a})

-- | Provides the name of the DB parameter group family that this DB cluster parameter group is compatible with.
dcpgDBParameterGroupFamily :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBParameterGroupFamily = lens _dcpgDBParameterGroupFamily (\ s a -> s{_dcpgDBParameterGroupFamily = a})

-- | Provides the name of the DB cluster parameter group.
dcpgDBClusterParameterGroupName :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBClusterParameterGroupName = lens _dcpgDBClusterParameterGroupName (\ s a -> s{_dcpgDBClusterParameterGroupName = a})

-- | Provides the customer-specified description for this DB cluster parameter group.
dcpgDescription :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDescription = lens _dcpgDescription (\ s a -> s{_dcpgDescription = a})

instance FromXML DBClusterParameterGroup where
        parseXML x
          = DBClusterParameterGroup' <$>
              (x .@? "DBClusterParameterGroupArn") <*>
                (x .@? "DBParameterGroupFamily")
                <*> (x .@? "DBClusterParameterGroupName")
                <*> (x .@? "Description")

instance Hashable DBClusterParameterGroup where

instance NFData DBClusterParameterGroup where

-- |
--
--
--
-- /See:/ 'dbClusterParameterGroupNameMessage' smart constructor.
newtype DBClusterParameterGroupNameMessage = DBClusterParameterGroupNameMessage'
  { _dcpgnmDBClusterParameterGroupName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgnmDBClusterParameterGroupName' - The name of the DB cluster parameter group. Constraints:     * Must be 1 to 255 letters or numbers.     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
dbClusterParameterGroupNameMessage
    :: DBClusterParameterGroupNameMessage
dbClusterParameterGroupNameMessage =
  DBClusterParameterGroupNameMessage'
    {_dcpgnmDBClusterParameterGroupName = Nothing}


-- | The name of the DB cluster parameter group. Constraints:     * Must be 1 to 255 letters or numbers.     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
dcpgnmDBClusterParameterGroupName :: Lens' DBClusterParameterGroupNameMessage (Maybe Text)
dcpgnmDBClusterParameterGroupName = lens _dcpgnmDBClusterParameterGroupName (\ s a -> s{_dcpgnmDBClusterParameterGroupName = a})

instance FromXML DBClusterParameterGroupNameMessage
         where
        parseXML x
          = DBClusterParameterGroupNameMessage' <$>
              (x .@? "DBClusterParameterGroupName")

instance Hashable DBClusterParameterGroupNameMessage
         where

instance NFData DBClusterParameterGroupNameMessage
         where

-- | Describes an AWS Identity and Access Management (IAM) role that is associated with a DB cluster.
--
--
--
-- /See:/ 'dbClusterRole' smart constructor.
data DBClusterRole = DBClusterRole'
  { _dcrStatus  :: !(Maybe Text)
  , _dcrRoleARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrStatus' - Describes the state of association between the IAM role and the DB cluster. The Status property returns one of the following values:     * @ACTIVE@ - the IAM role ARN is associated with the DB cluster and can be used to access other AWS services on your behalf.     * @PENDING@ - the IAM role ARN is being associated with the DB cluster.     * @INVALID@ - the IAM role ARN is associated with the DB cluster, but the DB cluster is unable to assume the IAM role in order to access other AWS services on your behalf.
--
-- * 'dcrRoleARN' - The Amazon Resource Name (ARN) of the IAM role that is associated with the DB cluster.
dbClusterRole
    :: DBClusterRole
dbClusterRole = DBClusterRole' {_dcrStatus = Nothing, _dcrRoleARN = Nothing}


-- | Describes the state of association between the IAM role and the DB cluster. The Status property returns one of the following values:     * @ACTIVE@ - the IAM role ARN is associated with the DB cluster and can be used to access other AWS services on your behalf.     * @PENDING@ - the IAM role ARN is being associated with the DB cluster.     * @INVALID@ - the IAM role ARN is associated with the DB cluster, but the DB cluster is unable to assume the IAM role in order to access other AWS services on your behalf.
dcrStatus :: Lens' DBClusterRole (Maybe Text)
dcrStatus = lens _dcrStatus (\ s a -> s{_dcrStatus = a})

-- | The Amazon Resource Name (ARN) of the IAM role that is associated with the DB cluster.
dcrRoleARN :: Lens' DBClusterRole (Maybe Text)
dcrRoleARN = lens _dcrRoleARN (\ s a -> s{_dcrRoleARN = a})

instance FromXML DBClusterRole where
        parseXML x
          = DBClusterRole' <$>
              (x .@? "Status") <*> (x .@? "RoleArn")

instance Hashable DBClusterRole where

instance NFData DBClusterRole where

-- | Contains the details for an Amazon RDS DB cluster snapshot
--
--
-- This data type is used as a response element in the 'DescribeDBClusterSnapshots' action.
--
--
-- /See:/ 'dbClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
  { _dcsEngineVersion                    :: !(Maybe Text)
  , _dcsStatus                           :: !(Maybe Text)
  , _dcsStorageEncrypted                 :: !(Maybe Bool)
  , _dcsDBClusterIdentifier              :: !(Maybe Text)
  , _dcsMasterUsername                   :: !(Maybe Text)
  , _dcsIAMDatabaseAuthenticationEnabled :: !(Maybe Bool)
  , _dcsDBClusterSnapshotARN             :: !(Maybe Text)
  , _dcsVPCId                            :: !(Maybe Text)
  , _dcsDBClusterSnapshotIdentifier      :: !(Maybe Text)
  , _dcsEngine                           :: !(Maybe Text)
  , _dcsLicenseModel                     :: !(Maybe Text)
  , _dcsAvailabilityZones                :: !(Maybe [Text])
  , _dcsSnapshotType                     :: !(Maybe Text)
  , _dcsKMSKeyId                         :: !(Maybe Text)
  , _dcsSnapshotCreateTime               :: !(Maybe ISO8601)
  , _dcsAllocatedStorage                 :: !(Maybe Int)
  , _dcsSourceDBClusterSnapshotARN       :: !(Maybe Text)
  , _dcsClusterCreateTime                :: !(Maybe ISO8601)
  , _dcsPercentProgress                  :: !(Maybe Int)
  , _dcsPort                             :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsEngineVersion' - Provides the version of the database engine for this DB cluster snapshot.
--
-- * 'dcsStatus' - Specifies the status of this DB cluster snapshot.
--
-- * 'dcsStorageEncrypted' - Specifies whether the DB cluster snapshot is encrypted.
--
-- * 'dcsDBClusterIdentifier' - Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
--
-- * 'dcsMasterUsername' - Provides the master username for the DB cluster snapshot.
--
-- * 'dcsIAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- * 'dcsDBClusterSnapshotARN' - The Amazon Resource Name (ARN) for the DB cluster snapshot.
--
-- * 'dcsVPCId' - Provides the VPC ID associated with the DB cluster snapshot.
--
-- * 'dcsDBClusterSnapshotIdentifier' - Specifies the identifier for the DB cluster snapshot.
--
-- * 'dcsEngine' - Specifies the name of the database engine.
--
-- * 'dcsLicenseModel' - Provides the license model information for this DB cluster snapshot.
--
-- * 'dcsAvailabilityZones' - Provides the list of EC2 Availability Zones that instances in the DB cluster snapshot can be restored in.
--
-- * 'dcsSnapshotType' - Provides the type of the DB cluster snapshot.
--
-- * 'dcsKMSKeyId' - If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
--
-- * 'dcsSnapshotCreateTime' - Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
--
-- * 'dcsAllocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- * 'dcsSourceDBClusterSnapshotARN' - If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
--
-- * 'dcsClusterCreateTime' - Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- * 'dcsPercentProgress' - Specifies the percentage of the estimated data that has been transferred.
--
-- * 'dcsPort' - Specifies the port that the DB cluster was listening on at the time of the snapshot.
dbClusterSnapshot
    :: DBClusterSnapshot
dbClusterSnapshot =
  DBClusterSnapshot'
    { _dcsEngineVersion = Nothing
    , _dcsStatus = Nothing
    , _dcsStorageEncrypted = Nothing
    , _dcsDBClusterIdentifier = Nothing
    , _dcsMasterUsername = Nothing
    , _dcsIAMDatabaseAuthenticationEnabled = Nothing
    , _dcsDBClusterSnapshotARN = Nothing
    , _dcsVPCId = Nothing
    , _dcsDBClusterSnapshotIdentifier = Nothing
    , _dcsEngine = Nothing
    , _dcsLicenseModel = Nothing
    , _dcsAvailabilityZones = Nothing
    , _dcsSnapshotType = Nothing
    , _dcsKMSKeyId = Nothing
    , _dcsSnapshotCreateTime = Nothing
    , _dcsAllocatedStorage = Nothing
    , _dcsSourceDBClusterSnapshotARN = Nothing
    , _dcsClusterCreateTime = Nothing
    , _dcsPercentProgress = Nothing
    , _dcsPort = Nothing
    }


-- | Provides the version of the database engine for this DB cluster snapshot.
dcsEngineVersion :: Lens' DBClusterSnapshot (Maybe Text)
dcsEngineVersion = lens _dcsEngineVersion (\ s a -> s{_dcsEngineVersion = a})

-- | Specifies the status of this DB cluster snapshot.
dcsStatus :: Lens' DBClusterSnapshot (Maybe Text)
dcsStatus = lens _dcsStatus (\ s a -> s{_dcsStatus = a})

-- | Specifies whether the DB cluster snapshot is encrypted.
dcsStorageEncrypted :: Lens' DBClusterSnapshot (Maybe Bool)
dcsStorageEncrypted = lens _dcsStorageEncrypted (\ s a -> s{_dcsStorageEncrypted = a})

-- | Specifies the DB cluster identifier of the DB cluster that this DB cluster snapshot was created from.
dcsDBClusterIdentifier :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterIdentifier = lens _dcsDBClusterIdentifier (\ s a -> s{_dcsDBClusterIdentifier = a})

-- | Provides the master username for the DB cluster snapshot.
dcsMasterUsername :: Lens' DBClusterSnapshot (Maybe Text)
dcsMasterUsername = lens _dcsMasterUsername (\ s a -> s{_dcsMasterUsername = a})

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
dcsIAMDatabaseAuthenticationEnabled :: Lens' DBClusterSnapshot (Maybe Bool)
dcsIAMDatabaseAuthenticationEnabled = lens _dcsIAMDatabaseAuthenticationEnabled (\ s a -> s{_dcsIAMDatabaseAuthenticationEnabled = a})

-- | The Amazon Resource Name (ARN) for the DB cluster snapshot.
dcsDBClusterSnapshotARN :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterSnapshotARN = lens _dcsDBClusterSnapshotARN (\ s a -> s{_dcsDBClusterSnapshotARN = a})

-- | Provides the VPC ID associated with the DB cluster snapshot.
dcsVPCId :: Lens' DBClusterSnapshot (Maybe Text)
dcsVPCId = lens _dcsVPCId (\ s a -> s{_dcsVPCId = a})

-- | Specifies the identifier for the DB cluster snapshot.
dcsDBClusterSnapshotIdentifier :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterSnapshotIdentifier = lens _dcsDBClusterSnapshotIdentifier (\ s a -> s{_dcsDBClusterSnapshotIdentifier = a})

-- | Specifies the name of the database engine.
dcsEngine :: Lens' DBClusterSnapshot (Maybe Text)
dcsEngine = lens _dcsEngine (\ s a -> s{_dcsEngine = a})

-- | Provides the license model information for this DB cluster snapshot.
dcsLicenseModel :: Lens' DBClusterSnapshot (Maybe Text)
dcsLicenseModel = lens _dcsLicenseModel (\ s a -> s{_dcsLicenseModel = a})

-- | Provides the list of EC2 Availability Zones that instances in the DB cluster snapshot can be restored in.
dcsAvailabilityZones :: Lens' DBClusterSnapshot [Text]
dcsAvailabilityZones = lens _dcsAvailabilityZones (\ s a -> s{_dcsAvailabilityZones = a}) . _Default . _Coerce

-- | Provides the type of the DB cluster snapshot.
dcsSnapshotType :: Lens' DBClusterSnapshot (Maybe Text)
dcsSnapshotType = lens _dcsSnapshotType (\ s a -> s{_dcsSnapshotType = a})

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB cluster snapshot.
dcsKMSKeyId :: Lens' DBClusterSnapshot (Maybe Text)
dcsKMSKeyId = lens _dcsKMSKeyId (\ s a -> s{_dcsKMSKeyId = a})

-- | Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
dcsSnapshotCreateTime :: Lens' DBClusterSnapshot (Maybe UTCTime)
dcsSnapshotCreateTime = lens _dcsSnapshotCreateTime (\ s a -> s{_dcsSnapshotCreateTime = a}) . mapping _Time

-- | Specifies the allocated storage size in gibibytes (GiB).
dcsAllocatedStorage :: Lens' DBClusterSnapshot (Maybe Int)
dcsAllocatedStorage = lens _dcsAllocatedStorage (\ s a -> s{_dcsAllocatedStorage = a})

-- | If the DB cluster snapshot was copied from a source DB cluster snapshot, the Amazon Resource Name (ARN) for the source DB cluster snapshot, otherwise, a null value.
dcsSourceDBClusterSnapshotARN :: Lens' DBClusterSnapshot (Maybe Text)
dcsSourceDBClusterSnapshotARN = lens _dcsSourceDBClusterSnapshotARN (\ s a -> s{_dcsSourceDBClusterSnapshotARN = a})

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
dcsClusterCreateTime :: Lens' DBClusterSnapshot (Maybe UTCTime)
dcsClusterCreateTime = lens _dcsClusterCreateTime (\ s a -> s{_dcsClusterCreateTime = a}) . mapping _Time

-- | Specifies the percentage of the estimated data that has been transferred.
dcsPercentProgress :: Lens' DBClusterSnapshot (Maybe Int)
dcsPercentProgress = lens _dcsPercentProgress (\ s a -> s{_dcsPercentProgress = a})

-- | Specifies the port that the DB cluster was listening on at the time of the snapshot.
dcsPort :: Lens' DBClusterSnapshot (Maybe Int)
dcsPort = lens _dcsPort (\ s a -> s{_dcsPort = a})

instance FromXML DBClusterSnapshot where
        parseXML x
          = DBClusterSnapshot' <$>
              (x .@? "EngineVersion") <*> (x .@? "Status") <*>
                (x .@? "StorageEncrypted")
                <*> (x .@? "DBClusterIdentifier")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "IAMDatabaseAuthenticationEnabled")
                <*> (x .@? "DBClusterSnapshotArn")
                <*> (x .@? "VpcId")
                <*> (x .@? "DBClusterSnapshotIdentifier")
                <*> (x .@? "Engine")
                <*> (x .@? "LicenseModel")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "SnapshotType")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "SnapshotCreateTime")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "SourceDBClusterSnapshotArn")
                <*> (x .@? "ClusterCreateTime")
                <*> (x .@? "PercentProgress")
                <*> (x .@? "Port")

instance Hashable DBClusterSnapshot where

instance NFData DBClusterSnapshot where

-- | Contains the name and values of a manual DB cluster snapshot attribute.
--
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS accounts to restore a manual DB cluster snapshot. For more information, see the 'ModifyDBClusterSnapshotAttribute' API action.
--
--
-- /See:/ 'dbClusterSnapshotAttribute' smart constructor.
data DBClusterSnapshotAttribute = DBClusterSnapshotAttribute'
  { _dcsaAttributeValues :: !(Maybe [Text])
  , _dcsaAttributeName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterSnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsaAttributeValues' - The value(s) for the manual DB cluster snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
--
-- * 'dcsaAttributeName' - The name of the manual DB cluster snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the 'ModifyDBClusterSnapshotAttribute' API action.
dbClusterSnapshotAttribute
    :: DBClusterSnapshotAttribute
dbClusterSnapshotAttribute =
  DBClusterSnapshotAttribute'
    {_dcsaAttributeValues = Nothing, _dcsaAttributeName = Nothing}


-- | The value(s) for the manual DB cluster snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
dcsaAttributeValues :: Lens' DBClusterSnapshotAttribute [Text]
dcsaAttributeValues = lens _dcsaAttributeValues (\ s a -> s{_dcsaAttributeValues = a}) . _Default . _Coerce

-- | The name of the manual DB cluster snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the 'ModifyDBClusterSnapshotAttribute' API action.
dcsaAttributeName :: Lens' DBClusterSnapshotAttribute (Maybe Text)
dcsaAttributeName = lens _dcsaAttributeName (\ s a -> s{_dcsaAttributeName = a})

instance FromXML DBClusterSnapshotAttribute where
        parseXML x
          = DBClusterSnapshotAttribute' <$>
              (x .@? "AttributeValues" .!@ mempty >>=
                 may (parseXMLList "AttributeValue"))
                <*> (x .@? "AttributeName")

instance Hashable DBClusterSnapshotAttribute where

instance NFData DBClusterSnapshotAttribute where

-- | Contains the results of a successful call to the 'DescribeDBClusterSnapshotAttributes' API action.
--
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS accounts to copy or restore a manual DB cluster snapshot. For more information, see the 'ModifyDBClusterSnapshotAttribute' API action.
--
--
-- /See:/ 'dbClusterSnapshotAttributesResult' smart constructor.
data DBClusterSnapshotAttributesResult = DBClusterSnapshotAttributesResult'
  { _dcsarDBClusterSnapshotIdentifier :: !(Maybe Text)
  , _dcsarDBClusterSnapshotAttributes :: !(Maybe [DBClusterSnapshotAttribute])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBClusterSnapshotAttributesResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsarDBClusterSnapshotIdentifier' - The identifier of the manual DB cluster snapshot that the attributes apply to.
--
-- * 'dcsarDBClusterSnapshotAttributes' - The list of attributes and values for the manual DB cluster snapshot.
dbClusterSnapshotAttributesResult
    :: DBClusterSnapshotAttributesResult
dbClusterSnapshotAttributesResult =
  DBClusterSnapshotAttributesResult'
    { _dcsarDBClusterSnapshotIdentifier = Nothing
    , _dcsarDBClusterSnapshotAttributes = Nothing
    }


-- | The identifier of the manual DB cluster snapshot that the attributes apply to.
dcsarDBClusterSnapshotIdentifier :: Lens' DBClusterSnapshotAttributesResult (Maybe Text)
dcsarDBClusterSnapshotIdentifier = lens _dcsarDBClusterSnapshotIdentifier (\ s a -> s{_dcsarDBClusterSnapshotIdentifier = a})

-- | The list of attributes and values for the manual DB cluster snapshot.
dcsarDBClusterSnapshotAttributes :: Lens' DBClusterSnapshotAttributesResult [DBClusterSnapshotAttribute]
dcsarDBClusterSnapshotAttributes = lens _dcsarDBClusterSnapshotAttributes (\ s a -> s{_dcsarDBClusterSnapshotAttributes = a}) . _Default . _Coerce

instance FromXML DBClusterSnapshotAttributesResult
         where
        parseXML x
          = DBClusterSnapshotAttributesResult' <$>
              (x .@? "DBClusterSnapshotIdentifier") <*>
                (x .@? "DBClusterSnapshotAttributes" .!@ mempty >>=
                   may (parseXMLList "DBClusterSnapshotAttribute"))

instance Hashable DBClusterSnapshotAttributesResult
         where

instance NFData DBClusterSnapshotAttributesResult
         where

-- | This data type is used as a response element in the action 'DescribeDBEngineVersions' .
--
--
--
-- /See:/ 'dbEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { _devEngineVersion                      :: !(Maybe Text)
  , _devDBEngineVersionDescription         :: !(Maybe Text)
  , _devDefaultCharacterSet                :: !(Maybe CharacterSet)
  , _devEngine                             :: !(Maybe Text)
  , _devDBParameterGroupFamily             :: !(Maybe Text)
  , _devSupportedCharacterSets             :: !(Maybe [CharacterSet])
  , _devDBEngineDescription                :: !(Maybe Text)
  , _devValidUpgradeTarget                 :: !(Maybe [UpgradeTarget])
  , _devSupportsLogExportsToCloudwatchLogs :: !(Maybe Bool)
  , _devSupportsReadReplica                :: !(Maybe Bool)
  , _devSupportedTimezones                 :: !(Maybe [Timezone])
  , _devExportableLogTypes                 :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBEngineVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'devEngineVersion' - The version number of the database engine.
--
-- * 'devDBEngineVersionDescription' - The description of the database engine version.
--
-- * 'devDefaultCharacterSet' - The default character set for new instances of this engine version, if the @CharacterSetName@ parameter of the CreateDBInstance API is not specified.
--
-- * 'devEngine' - The name of the database engine.
--
-- * 'devDBParameterGroupFamily' - The name of the DB parameter group family for the database engine.
--
-- * 'devSupportedCharacterSets' - A list of the character sets supported by this engine for the @CharacterSetName@ parameter of the @CreateDBInstance@ action.
--
-- * 'devDBEngineDescription' - The description of the database engine.
--
-- * 'devValidUpgradeTarget' - A list of engine versions that this database engine version can be upgraded to.
--
-- * 'devSupportsLogExportsToCloudwatchLogs' - A value that indicates whether the engine version supports exporting the log types specified by ExportableLogTypes to CloudWatch Logs.
--
-- * 'devSupportsReadReplica' - Indicates whether the database engine version supports read replicas.
--
-- * 'devSupportedTimezones' - A list of the time zones supported by this engine for the @Timezone@ parameter of the @CreateDBInstance@ action.
--
-- * 'devExportableLogTypes' - The types of logs that the database engine has available for export to CloudWatch Logs.
dbEngineVersion
    :: DBEngineVersion
dbEngineVersion =
  DBEngineVersion'
    { _devEngineVersion = Nothing
    , _devDBEngineVersionDescription = Nothing
    , _devDefaultCharacterSet = Nothing
    , _devEngine = Nothing
    , _devDBParameterGroupFamily = Nothing
    , _devSupportedCharacterSets = Nothing
    , _devDBEngineDescription = Nothing
    , _devValidUpgradeTarget = Nothing
    , _devSupportsLogExportsToCloudwatchLogs = Nothing
    , _devSupportsReadReplica = Nothing
    , _devSupportedTimezones = Nothing
    , _devExportableLogTypes = Nothing
    }


-- | The version number of the database engine.
devEngineVersion :: Lens' DBEngineVersion (Maybe Text)
devEngineVersion = lens _devEngineVersion (\ s a -> s{_devEngineVersion = a})

-- | The description of the database engine version.
devDBEngineVersionDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineVersionDescription = lens _devDBEngineVersionDescription (\ s a -> s{_devDBEngineVersionDescription = a})

-- | The default character set for new instances of this engine version, if the @CharacterSetName@ parameter of the CreateDBInstance API is not specified.
devDefaultCharacterSet :: Lens' DBEngineVersion (Maybe CharacterSet)
devDefaultCharacterSet = lens _devDefaultCharacterSet (\ s a -> s{_devDefaultCharacterSet = a})

-- | The name of the database engine.
devEngine :: Lens' DBEngineVersion (Maybe Text)
devEngine = lens _devEngine (\ s a -> s{_devEngine = a})

-- | The name of the DB parameter group family for the database engine.
devDBParameterGroupFamily :: Lens' DBEngineVersion (Maybe Text)
devDBParameterGroupFamily = lens _devDBParameterGroupFamily (\ s a -> s{_devDBParameterGroupFamily = a})

-- | A list of the character sets supported by this engine for the @CharacterSetName@ parameter of the @CreateDBInstance@ action.
devSupportedCharacterSets :: Lens' DBEngineVersion [CharacterSet]
devSupportedCharacterSets = lens _devSupportedCharacterSets (\ s a -> s{_devSupportedCharacterSets = a}) . _Default . _Coerce

-- | The description of the database engine.
devDBEngineDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineDescription = lens _devDBEngineDescription (\ s a -> s{_devDBEngineDescription = a})

-- | A list of engine versions that this database engine version can be upgraded to.
devValidUpgradeTarget :: Lens' DBEngineVersion [UpgradeTarget]
devValidUpgradeTarget = lens _devValidUpgradeTarget (\ s a -> s{_devValidUpgradeTarget = a}) . _Default . _Coerce

-- | A value that indicates whether the engine version supports exporting the log types specified by ExportableLogTypes to CloudWatch Logs.
devSupportsLogExportsToCloudwatchLogs :: Lens' DBEngineVersion (Maybe Bool)
devSupportsLogExportsToCloudwatchLogs = lens _devSupportsLogExportsToCloudwatchLogs (\ s a -> s{_devSupportsLogExportsToCloudwatchLogs = a})

-- | Indicates whether the database engine version supports read replicas.
devSupportsReadReplica :: Lens' DBEngineVersion (Maybe Bool)
devSupportsReadReplica = lens _devSupportsReadReplica (\ s a -> s{_devSupportsReadReplica = a})

-- | A list of the time zones supported by this engine for the @Timezone@ parameter of the @CreateDBInstance@ action.
devSupportedTimezones :: Lens' DBEngineVersion [Timezone]
devSupportedTimezones = lens _devSupportedTimezones (\ s a -> s{_devSupportedTimezones = a}) . _Default . _Coerce

-- | The types of logs that the database engine has available for export to CloudWatch Logs.
devExportableLogTypes :: Lens' DBEngineVersion [Text]
devExportableLogTypes = lens _devExportableLogTypes (\ s a -> s{_devExportableLogTypes = a}) . _Default . _Coerce

instance FromXML DBEngineVersion where
        parseXML x
          = DBEngineVersion' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "DBEngineVersionDescription")
                <*> (x .@? "DefaultCharacterSet")
                <*> (x .@? "Engine")
                <*> (x .@? "DBParameterGroupFamily")
                <*>
                (x .@? "SupportedCharacterSets" .!@ mempty >>=
                   may (parseXMLList "CharacterSet"))
                <*> (x .@? "DBEngineDescription")
                <*>
                (x .@? "ValidUpgradeTarget" .!@ mempty >>=
                   may (parseXMLList "UpgradeTarget"))
                <*> (x .@? "SupportsLogExportsToCloudwatchLogs")
                <*> (x .@? "SupportsReadReplica")
                <*>
                (x .@? "SupportedTimezones" .!@ mempty >>=
                   may (parseXMLList "Timezone"))
                <*>
                (x .@? "ExportableLogTypes" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable DBEngineVersion where

instance NFData DBEngineVersion where

-- | Contains the details of an Amazon RDS DB instance.
--
--
-- This data type is used as a response element in the 'DescribeDBInstances' action.
--
--
-- /See:/ 'dbInstance' smart constructor.
data DBInstance = DBInstance'
  { _diEngineVersion :: !(Maybe Text)
  , _diDBSecurityGroups :: !(Maybe [DBSecurityGroupMembership])
  , _diStorageEncrypted :: !(Maybe Bool)
  , _diDBClusterIdentifier :: !(Maybe Text)
  , _diPubliclyAccessible :: !(Maybe Bool)
  , _diAutoMinorVersionUpgrade :: !(Maybe Bool)
  , _diDBInstanceARN :: !(Maybe Text)
  , _diMasterUsername :: !(Maybe Text)
  , _diReadReplicaDBInstanceIdentifiers :: !(Maybe [Text])
  , _diIAMDatabaseAuthenticationEnabled :: !(Maybe Bool)
  , _diMonitoringRoleARN :: !(Maybe Text)
  , _diIOPS :: !(Maybe Int)
  , _diInstanceCreateTime :: !(Maybe ISO8601)
  , _diReadReplicaSourceDBInstanceIdentifier :: !(Maybe Text)
  , _diMonitoringInterval :: !(Maybe Int)
  , _diEngine :: !(Maybe Text)
  , _diLatestRestorableTime :: !(Maybe ISO8601)
  , _diDBInstanceClass :: !(Maybe Text)
  , _diPromotionTier :: !(Maybe Int)
  , _diLicenseModel :: !(Maybe Text)
  , _diPreferredMaintenanceWindow :: !(Maybe Text)
  , _diCACertificateIdentifier :: !(Maybe Text)
  , _diDBInstanceIdentifier :: !(Maybe Text)
  , _diCharacterSetName :: !(Maybe Text)
  , _diKMSKeyId :: !(Maybe Text)
  , _diPreferredBackupWindow :: !(Maybe Text)
  , _diAvailabilityZone :: !(Maybe Text)
  , _diVPCSecurityGroups :: !(Maybe [VPCSecurityGroupMembership])
  , _diBackupRetentionPeriod :: !(Maybe Int)
  , _diPerformanceInsightsKMSKeyId :: !(Maybe Text)
  , _diDBSubnetGroup :: !(Maybe DBSubnetGroup)
  , _diMultiAZ :: !(Maybe Bool)
  , _diOptionGroupMemberships :: !(Maybe [OptionGroupMembership])
  , _diEnabledCloudwatchLogsExports :: !(Maybe [Text])
  , _diEnhancedMonitoringResourceARN :: !(Maybe Text)
  , _diSecondaryAvailabilityZone :: !(Maybe Text)
  , _diPerformanceInsightsEnabled :: !(Maybe Bool)
  , _diAllocatedStorage :: !(Maybe Int)
  , _diDBiResourceId :: !(Maybe Text)
  , _diDBParameterGroups :: !(Maybe [DBParameterGroupStatus])
  , _diCopyTagsToSnapshot :: !(Maybe Bool)
  , _diTimezone :: !(Maybe Text)
  , _diTDECredentialARN :: !(Maybe Text)
  , _diEndpoint :: !(Maybe Endpoint)
  , _diDBInstanceStatus :: !(Maybe Text)
  , _diDBInstancePort :: !(Maybe Int)
  , _diPendingModifiedValues :: !(Maybe PendingModifiedValues)
  , _diReadReplicaDBClusterIdentifiers :: !(Maybe [Text])
  , _diStorageType :: !(Maybe Text)
  , _diStatusInfos :: !(Maybe [DBInstanceStatusInfo])
  , _diDomainMemberships :: !(Maybe [DomainMembership])
  , _diDBName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diEngineVersion' - Indicates the database engine version.
--
-- * 'diDBSecurityGroups' - Provides List of DB security group elements containing only @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
--
-- * 'diStorageEncrypted' - Specifies whether the DB instance is encrypted.
--
-- * 'diDBClusterIdentifier' - If the DB instance is a member of a DB cluster, contains the name of the DB cluster that the DB instance is a member of.
--
-- * 'diPubliclyAccessible' - Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is private.
--
-- * 'diAutoMinorVersionUpgrade' - Indicates that minor version patches are applied automatically.
--
-- * 'diDBInstanceARN' - The Amazon Resource Name (ARN) for the DB instance.
--
-- * 'diMasterUsername' - Contains the master username for the DB instance.
--
-- * 'diReadReplicaDBInstanceIdentifiers' - Contains one or more identifiers of the Read Replicas associated with this DB instance.
--
-- * 'diIAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false. IAM database authentication can be enabled for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher     * Aurora 5.6 or higher. To enable IAM database authentication for Aurora, see DBCluster Type.
--
-- * 'diMonitoringRoleARN' - The ARN for the IAM role that permits RDS to send Enhanced Monitoring metrics to Amazon CloudWatch Logs.
--
-- * 'diIOPS' - Specifies the Provisioned IOPS (I/O operations per second) value.
--
-- * 'diInstanceCreateTime' - Provides the date and time the DB instance was created.
--
-- * 'diReadReplicaSourceDBInstanceIdentifier' - Contains the identifier of the source DB instance if this DB instance is a Read Replica.
--
-- * 'diMonitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance.
--
-- * 'diEngine' - Provides the name of the database engine to be used for this DB instance.
--
-- * 'diLatestRestorableTime' - Specifies the latest time to which a database can be restored with point-in-time restore.
--
-- * 'diDBInstanceClass' - Contains the name of the compute and memory capacity class of the DB instance.
--
-- * 'diPromotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Managing.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> .
--
-- * 'diLicenseModel' - License model information for this DB instance.
--
-- * 'diPreferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- * 'diCACertificateIdentifier' - The identifier of the CA certificate for this DB instance.
--
-- * 'diDBInstanceIdentifier' - Contains a user-supplied database identifier. This identifier is the unique key that identifies a DB instance.
--
-- * 'diCharacterSetName' - If present, specifies the name of the character set that this instance is associated with.
--
-- * 'diKMSKeyId' - If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB instance.
--
-- * 'diPreferredBackupWindow' - Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
--
-- * 'diAvailabilityZone' - Specifies the name of the Availability Zone the DB instance is located in.
--
-- * 'diVPCSecurityGroups' - Provides a list of VPC security group elements that the DB instance belongs to.
--
-- * 'diBackupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are retained.
--
-- * 'diPerformanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- * 'diDBSubnetGroup' - Specifies information on the subnet group associated with the DB instance, including the name, description, and subnets in the subnet group.
--
-- * 'diMultiAZ' - Specifies if the DB instance is a Multi-AZ deployment.
--
-- * 'diOptionGroupMemberships' - Provides the list of option group memberships for this DB instance.
--
-- * 'diEnabledCloudwatchLogsExports' - A list of log types that this DB instance is configured to export to CloudWatch Logs.
--
-- * 'diEnhancedMonitoringResourceARN' - The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream that receives the Enhanced Monitoring metrics data for the DB instance.
--
-- * 'diSecondaryAvailabilityZone' - If present, specifies the name of the secondary Availability Zone for a DB instance with multi-AZ support.
--
-- * 'diPerformanceInsightsEnabled' - True if Performance Insights is enabled for the DB instance, and otherwise false.
--
-- * 'diAllocatedStorage' - Specifies the allocated storage size specified in gibibytes.
--
-- * 'diDBiResourceId' - The AWS Region-unique, immutable identifier for the DB instance. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB instance is accessed.
--
-- * 'diDBParameterGroups' - Provides the list of DB parameter groups applied to this DB instance.
--
-- * 'diCopyTagsToSnapshot' - Specifies whether tags are copied from the DB instance to snapshots of the DB instance.
--
-- * 'diTimezone' - The time zone of the DB instance. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- * 'diTDECredentialARN' - The ARN from the key store with which the instance is associated for TDE encryption.
--
-- * 'diEndpoint' - Specifies the connection endpoint.
--
-- * 'diDBInstanceStatus' - Specifies the current state of this database.
--
-- * 'diDBInstancePort' - Specifies the port that the DB instance listens on. If the DB instance is part of a DB cluster, this can be a different port than the DB cluster port.
--
-- * 'diPendingModifiedValues' - Specifies that changes to the DB instance are pending. This element is only included when changes are pending. Specific changes are identified by subelements.
--
-- * 'diReadReplicaDBClusterIdentifiers' - Contains one or more identifiers of Aurora DB clusters that are Read Replicas of this DB instance.
--
-- * 'diStorageType' - Specifies the storage type associated with DB instance.
--
-- * 'diStatusInfos' - The status of a Read Replica. If the instance is not a Read Replica, this is blank.
--
-- * 'diDomainMemberships' - The Active Directory Domain membership records associated with the DB instance.
--
-- * 'diDBName' - The meaning of this parameter differs according to the database engine you use. For example, this value returns MySQL, MariaDB, or PostgreSQL information when returning values from CreateDBInstanceReadReplica since Read Replicas are only supported for these engines. __MySQL, MariaDB, SQL Server, PostgreSQL__  Contains the name of the initial database of this instance that was provided at create time, if one was specified when the DB instance was created. This same name is returned for the life of the DB instance. Type: String __Oracle__  Contains the Oracle System ID (SID) of the created DB instance. Not shown when the returned parameters do not apply to an Oracle DB instance.
dbInstance
    :: DBInstance
dbInstance =
  DBInstance'
    { _diEngineVersion = Nothing
    , _diDBSecurityGroups = Nothing
    , _diStorageEncrypted = Nothing
    , _diDBClusterIdentifier = Nothing
    , _diPubliclyAccessible = Nothing
    , _diAutoMinorVersionUpgrade = Nothing
    , _diDBInstanceARN = Nothing
    , _diMasterUsername = Nothing
    , _diReadReplicaDBInstanceIdentifiers = Nothing
    , _diIAMDatabaseAuthenticationEnabled = Nothing
    , _diMonitoringRoleARN = Nothing
    , _diIOPS = Nothing
    , _diInstanceCreateTime = Nothing
    , _diReadReplicaSourceDBInstanceIdentifier = Nothing
    , _diMonitoringInterval = Nothing
    , _diEngine = Nothing
    , _diLatestRestorableTime = Nothing
    , _diDBInstanceClass = Nothing
    , _diPromotionTier = Nothing
    , _diLicenseModel = Nothing
    , _diPreferredMaintenanceWindow = Nothing
    , _diCACertificateIdentifier = Nothing
    , _diDBInstanceIdentifier = Nothing
    , _diCharacterSetName = Nothing
    , _diKMSKeyId = Nothing
    , _diPreferredBackupWindow = Nothing
    , _diAvailabilityZone = Nothing
    , _diVPCSecurityGroups = Nothing
    , _diBackupRetentionPeriod = Nothing
    , _diPerformanceInsightsKMSKeyId = Nothing
    , _diDBSubnetGroup = Nothing
    , _diMultiAZ = Nothing
    , _diOptionGroupMemberships = Nothing
    , _diEnabledCloudwatchLogsExports = Nothing
    , _diEnhancedMonitoringResourceARN = Nothing
    , _diSecondaryAvailabilityZone = Nothing
    , _diPerformanceInsightsEnabled = Nothing
    , _diAllocatedStorage = Nothing
    , _diDBiResourceId = Nothing
    , _diDBParameterGroups = Nothing
    , _diCopyTagsToSnapshot = Nothing
    , _diTimezone = Nothing
    , _diTDECredentialARN = Nothing
    , _diEndpoint = Nothing
    , _diDBInstanceStatus = Nothing
    , _diDBInstancePort = Nothing
    , _diPendingModifiedValues = Nothing
    , _diReadReplicaDBClusterIdentifiers = Nothing
    , _diStorageType = Nothing
    , _diStatusInfos = Nothing
    , _diDomainMemberships = Nothing
    , _diDBName = Nothing
    }


-- | Indicates the database engine version.
diEngineVersion :: Lens' DBInstance (Maybe Text)
diEngineVersion = lens _diEngineVersion (\ s a -> s{_diEngineVersion = a})

-- | Provides List of DB security group elements containing only @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
diDBSecurityGroups :: Lens' DBInstance [DBSecurityGroupMembership]
diDBSecurityGroups = lens _diDBSecurityGroups (\ s a -> s{_diDBSecurityGroups = a}) . _Default . _Coerce

-- | Specifies whether the DB instance is encrypted.
diStorageEncrypted :: Lens' DBInstance (Maybe Bool)
diStorageEncrypted = lens _diStorageEncrypted (\ s a -> s{_diStorageEncrypted = a})

-- | If the DB instance is a member of a DB cluster, contains the name of the DB cluster that the DB instance is a member of.
diDBClusterIdentifier :: Lens' DBInstance (Maybe Text)
diDBClusterIdentifier = lens _diDBClusterIdentifier (\ s a -> s{_diDBClusterIdentifier = a})

-- | Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is private.
diPubliclyAccessible :: Lens' DBInstance (Maybe Bool)
diPubliclyAccessible = lens _diPubliclyAccessible (\ s a -> s{_diPubliclyAccessible = a})

-- | Indicates that minor version patches are applied automatically.
diAutoMinorVersionUpgrade :: Lens' DBInstance (Maybe Bool)
diAutoMinorVersionUpgrade = lens _diAutoMinorVersionUpgrade (\ s a -> s{_diAutoMinorVersionUpgrade = a})

-- | The Amazon Resource Name (ARN) for the DB instance.
diDBInstanceARN :: Lens' DBInstance (Maybe Text)
diDBInstanceARN = lens _diDBInstanceARN (\ s a -> s{_diDBInstanceARN = a})

-- | Contains the master username for the DB instance.
diMasterUsername :: Lens' DBInstance (Maybe Text)
diMasterUsername = lens _diMasterUsername (\ s a -> s{_diMasterUsername = a})

-- | Contains one or more identifiers of the Read Replicas associated with this DB instance.
diReadReplicaDBInstanceIdentifiers :: Lens' DBInstance [Text]
diReadReplicaDBInstanceIdentifiers = lens _diReadReplicaDBInstanceIdentifiers (\ s a -> s{_diReadReplicaDBInstanceIdentifiers = a}) . _Default . _Coerce

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false. IAM database authentication can be enabled for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher     * Aurora 5.6 or higher. To enable IAM database authentication for Aurora, see DBCluster Type.
diIAMDatabaseAuthenticationEnabled :: Lens' DBInstance (Maybe Bool)
diIAMDatabaseAuthenticationEnabled = lens _diIAMDatabaseAuthenticationEnabled (\ s a -> s{_diIAMDatabaseAuthenticationEnabled = a})

-- | The ARN for the IAM role that permits RDS to send Enhanced Monitoring metrics to Amazon CloudWatch Logs.
diMonitoringRoleARN :: Lens' DBInstance (Maybe Text)
diMonitoringRoleARN = lens _diMonitoringRoleARN (\ s a -> s{_diMonitoringRoleARN = a})

-- | Specifies the Provisioned IOPS (I/O operations per second) value.
diIOPS :: Lens' DBInstance (Maybe Int)
diIOPS = lens _diIOPS (\ s a -> s{_diIOPS = a})

-- | Provides the date and time the DB instance was created.
diInstanceCreateTime :: Lens' DBInstance (Maybe UTCTime)
diInstanceCreateTime = lens _diInstanceCreateTime (\ s a -> s{_diInstanceCreateTime = a}) . mapping _Time

-- | Contains the identifier of the source DB instance if this DB instance is a Read Replica.
diReadReplicaSourceDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
diReadReplicaSourceDBInstanceIdentifier = lens _diReadReplicaSourceDBInstanceIdentifier (\ s a -> s{_diReadReplicaSourceDBInstanceIdentifier = a})

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance.
diMonitoringInterval :: Lens' DBInstance (Maybe Int)
diMonitoringInterval = lens _diMonitoringInterval (\ s a -> s{_diMonitoringInterval = a})

-- | Provides the name of the database engine to be used for this DB instance.
diEngine :: Lens' DBInstance (Maybe Text)
diEngine = lens _diEngine (\ s a -> s{_diEngine = a})

-- | Specifies the latest time to which a database can be restored with point-in-time restore.
diLatestRestorableTime :: Lens' DBInstance (Maybe UTCTime)
diLatestRestorableTime = lens _diLatestRestorableTime (\ s a -> s{_diLatestRestorableTime = a}) . mapping _Time

-- | Contains the name of the compute and memory capacity class of the DB instance.
diDBInstanceClass :: Lens' DBInstance (Maybe Text)
diDBInstanceClass = lens _diDBInstanceClass (\ s a -> s{_diDBInstanceClass = a})

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Managing.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> .
diPromotionTier :: Lens' DBInstance (Maybe Int)
diPromotionTier = lens _diPromotionTier (\ s a -> s{_diPromotionTier = a})

-- | License model information for this DB instance.
diLicenseModel :: Lens' DBInstance (Maybe Text)
diLicenseModel = lens _diLicenseModel (\ s a -> s{_diLicenseModel = a})

-- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
diPreferredMaintenanceWindow :: Lens' DBInstance (Maybe Text)
diPreferredMaintenanceWindow = lens _diPreferredMaintenanceWindow (\ s a -> s{_diPreferredMaintenanceWindow = a})

-- | The identifier of the CA certificate for this DB instance.
diCACertificateIdentifier :: Lens' DBInstance (Maybe Text)
diCACertificateIdentifier = lens _diCACertificateIdentifier (\ s a -> s{_diCACertificateIdentifier = a})

-- | Contains a user-supplied database identifier. This identifier is the unique key that identifies a DB instance.
diDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
diDBInstanceIdentifier = lens _diDBInstanceIdentifier (\ s a -> s{_diDBInstanceIdentifier = a})

-- | If present, specifies the name of the character set that this instance is associated with.
diCharacterSetName :: Lens' DBInstance (Maybe Text)
diCharacterSetName = lens _diCharacterSetName (\ s a -> s{_diCharacterSetName = a})

-- | If @StorageEncrypted@ is true, the AWS KMS key identifier for the encrypted DB instance.
diKMSKeyId :: Lens' DBInstance (Maybe Text)
diKMSKeyId = lens _diKMSKeyId (\ s a -> s{_diKMSKeyId = a})

-- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
diPreferredBackupWindow :: Lens' DBInstance (Maybe Text)
diPreferredBackupWindow = lens _diPreferredBackupWindow (\ s a -> s{_diPreferredBackupWindow = a})

-- | Specifies the name of the Availability Zone the DB instance is located in.
diAvailabilityZone :: Lens' DBInstance (Maybe Text)
diAvailabilityZone = lens _diAvailabilityZone (\ s a -> s{_diAvailabilityZone = a})

-- | Provides a list of VPC security group elements that the DB instance belongs to.
diVPCSecurityGroups :: Lens' DBInstance [VPCSecurityGroupMembership]
diVPCSecurityGroups = lens _diVPCSecurityGroups (\ s a -> s{_diVPCSecurityGroups = a}) . _Default . _Coerce

-- | Specifies the number of days for which automatic DB snapshots are retained.
diBackupRetentionPeriod :: Lens' DBInstance (Maybe Int)
diBackupRetentionPeriod = lens _diBackupRetentionPeriod (\ s a -> s{_diBackupRetentionPeriod = a})

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
diPerformanceInsightsKMSKeyId :: Lens' DBInstance (Maybe Text)
diPerformanceInsightsKMSKeyId = lens _diPerformanceInsightsKMSKeyId (\ s a -> s{_diPerformanceInsightsKMSKeyId = a})

-- | Specifies information on the subnet group associated with the DB instance, including the name, description, and subnets in the subnet group.
diDBSubnetGroup :: Lens' DBInstance (Maybe DBSubnetGroup)
diDBSubnetGroup = lens _diDBSubnetGroup (\ s a -> s{_diDBSubnetGroup = a})

-- | Specifies if the DB instance is a Multi-AZ deployment.
diMultiAZ :: Lens' DBInstance (Maybe Bool)
diMultiAZ = lens _diMultiAZ (\ s a -> s{_diMultiAZ = a})

-- | Provides the list of option group memberships for this DB instance.
diOptionGroupMemberships :: Lens' DBInstance [OptionGroupMembership]
diOptionGroupMemberships = lens _diOptionGroupMemberships (\ s a -> s{_diOptionGroupMemberships = a}) . _Default . _Coerce

-- | A list of log types that this DB instance is configured to export to CloudWatch Logs.
diEnabledCloudwatchLogsExports :: Lens' DBInstance [Text]
diEnabledCloudwatchLogsExports = lens _diEnabledCloudwatchLogsExports (\ s a -> s{_diEnabledCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream that receives the Enhanced Monitoring metrics data for the DB instance.
diEnhancedMonitoringResourceARN :: Lens' DBInstance (Maybe Text)
diEnhancedMonitoringResourceARN = lens _diEnhancedMonitoringResourceARN (\ s a -> s{_diEnhancedMonitoringResourceARN = a})

-- | If present, specifies the name of the secondary Availability Zone for a DB instance with multi-AZ support.
diSecondaryAvailabilityZone :: Lens' DBInstance (Maybe Text)
diSecondaryAvailabilityZone = lens _diSecondaryAvailabilityZone (\ s a -> s{_diSecondaryAvailabilityZone = a})

-- | True if Performance Insights is enabled for the DB instance, and otherwise false.
diPerformanceInsightsEnabled :: Lens' DBInstance (Maybe Bool)
diPerformanceInsightsEnabled = lens _diPerformanceInsightsEnabled (\ s a -> s{_diPerformanceInsightsEnabled = a})

-- | Specifies the allocated storage size specified in gibibytes.
diAllocatedStorage :: Lens' DBInstance (Maybe Int)
diAllocatedStorage = lens _diAllocatedStorage (\ s a -> s{_diAllocatedStorage = a})

-- | The AWS Region-unique, immutable identifier for the DB instance. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB instance is accessed.
diDBiResourceId :: Lens' DBInstance (Maybe Text)
diDBiResourceId = lens _diDBiResourceId (\ s a -> s{_diDBiResourceId = a})

-- | Provides the list of DB parameter groups applied to this DB instance.
diDBParameterGroups :: Lens' DBInstance [DBParameterGroupStatus]
diDBParameterGroups = lens _diDBParameterGroups (\ s a -> s{_diDBParameterGroups = a}) . _Default . _Coerce

-- | Specifies whether tags are copied from the DB instance to snapshots of the DB instance.
diCopyTagsToSnapshot :: Lens' DBInstance (Maybe Bool)
diCopyTagsToSnapshot = lens _diCopyTagsToSnapshot (\ s a -> s{_diCopyTagsToSnapshot = a})

-- | The time zone of the DB instance. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for Microsoft SQL Server DB instances that were created with a time zone specified.
diTimezone :: Lens' DBInstance (Maybe Text)
diTimezone = lens _diTimezone (\ s a -> s{_diTimezone = a})

-- | The ARN from the key store with which the instance is associated for TDE encryption.
diTDECredentialARN :: Lens' DBInstance (Maybe Text)
diTDECredentialARN = lens _diTDECredentialARN (\ s a -> s{_diTDECredentialARN = a})

-- | Specifies the connection endpoint.
diEndpoint :: Lens' DBInstance (Maybe Endpoint)
diEndpoint = lens _diEndpoint (\ s a -> s{_diEndpoint = a})

-- | Specifies the current state of this database.
diDBInstanceStatus :: Lens' DBInstance (Maybe Text)
diDBInstanceStatus = lens _diDBInstanceStatus (\ s a -> s{_diDBInstanceStatus = a})

-- | Specifies the port that the DB instance listens on. If the DB instance is part of a DB cluster, this can be a different port than the DB cluster port.
diDBInstancePort :: Lens' DBInstance (Maybe Int)
diDBInstancePort = lens _diDBInstancePort (\ s a -> s{_diDBInstancePort = a})

-- | Specifies that changes to the DB instance are pending. This element is only included when changes are pending. Specific changes are identified by subelements.
diPendingModifiedValues :: Lens' DBInstance (Maybe PendingModifiedValues)
diPendingModifiedValues = lens _diPendingModifiedValues (\ s a -> s{_diPendingModifiedValues = a})

-- | Contains one or more identifiers of Aurora DB clusters that are Read Replicas of this DB instance.
diReadReplicaDBClusterIdentifiers :: Lens' DBInstance [Text]
diReadReplicaDBClusterIdentifiers = lens _diReadReplicaDBClusterIdentifiers (\ s a -> s{_diReadReplicaDBClusterIdentifiers = a}) . _Default . _Coerce

-- | Specifies the storage type associated with DB instance.
diStorageType :: Lens' DBInstance (Maybe Text)
diStorageType = lens _diStorageType (\ s a -> s{_diStorageType = a})

-- | The status of a Read Replica. If the instance is not a Read Replica, this is blank.
diStatusInfos :: Lens' DBInstance [DBInstanceStatusInfo]
diStatusInfos = lens _diStatusInfos (\ s a -> s{_diStatusInfos = a}) . _Default . _Coerce

-- | The Active Directory Domain membership records associated with the DB instance.
diDomainMemberships :: Lens' DBInstance [DomainMembership]
diDomainMemberships = lens _diDomainMemberships (\ s a -> s{_diDomainMemberships = a}) . _Default . _Coerce

-- | The meaning of this parameter differs according to the database engine you use. For example, this value returns MySQL, MariaDB, or PostgreSQL information when returning values from CreateDBInstanceReadReplica since Read Replicas are only supported for these engines. __MySQL, MariaDB, SQL Server, PostgreSQL__  Contains the name of the initial database of this instance that was provided at create time, if one was specified when the DB instance was created. This same name is returned for the life of the DB instance. Type: String __Oracle__  Contains the Oracle System ID (SID) of the created DB instance. Not shown when the returned parameters do not apply to an Oracle DB instance.
diDBName :: Lens' DBInstance (Maybe Text)
diDBName = lens _diDBName (\ s a -> s{_diDBName = a})

instance FromXML DBInstance where
        parseXML x
          = DBInstance' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "DBSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "DBSecurityGroup"))
                <*> (x .@? "StorageEncrypted")
                <*> (x .@? "DBClusterIdentifier")
                <*> (x .@? "PubliclyAccessible")
                <*> (x .@? "AutoMinorVersionUpgrade")
                <*> (x .@? "DBInstanceArn")
                <*> (x .@? "MasterUsername")
                <*>
                (x .@? "ReadReplicaDBInstanceIdentifiers" .!@ mempty
                   >>=
                   may (parseXMLList "ReadReplicaDBInstanceIdentifier"))
                <*> (x .@? "IAMDatabaseAuthenticationEnabled")
                <*> (x .@? "MonitoringRoleArn")
                <*> (x .@? "Iops")
                <*> (x .@? "InstanceCreateTime")
                <*> (x .@? "ReadReplicaSourceDBInstanceIdentifier")
                <*> (x .@? "MonitoringInterval")
                <*> (x .@? "Engine")
                <*> (x .@? "LatestRestorableTime")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "PromotionTier")
                <*> (x .@? "LicenseModel")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "CACertificateIdentifier")
                <*> (x .@? "DBInstanceIdentifier")
                <*> (x .@? "CharacterSetName")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "PreferredBackupWindow")
                <*> (x .@? "AvailabilityZone")
                <*>
                (x .@? "VpcSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroupMembership"))
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "PerformanceInsightsKMSKeyId")
                <*> (x .@? "DBSubnetGroup")
                <*> (x .@? "MultiAZ")
                <*>
                (x .@? "OptionGroupMemberships" .!@ mempty >>=
                   may (parseXMLList "OptionGroupMembership"))
                <*>
                (x .@? "EnabledCloudwatchLogsExports" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "EnhancedMonitoringResourceArn")
                <*> (x .@? "SecondaryAvailabilityZone")
                <*> (x .@? "PerformanceInsightsEnabled")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "DbiResourceId")
                <*>
                (x .@? "DBParameterGroups" .!@ mempty >>=
                   may (parseXMLList "DBParameterGroup"))
                <*> (x .@? "CopyTagsToSnapshot")
                <*> (x .@? "Timezone")
                <*> (x .@? "TdeCredentialArn")
                <*> (x .@? "Endpoint")
                <*> (x .@? "DBInstanceStatus")
                <*> (x .@? "DbInstancePort")
                <*> (x .@? "PendingModifiedValues")
                <*>
                (x .@? "ReadReplicaDBClusterIdentifiers" .!@ mempty
                   >>=
                   may (parseXMLList "ReadReplicaDBClusterIdentifier"))
                <*> (x .@? "StorageType")
                <*>
                (x .@? "StatusInfos" .!@ mempty >>=
                   may (parseXMLList "DBInstanceStatusInfo"))
                <*>
                (x .@? "DomainMemberships" .!@ mempty >>=
                   may (parseXMLList "DomainMembership"))
                <*> (x .@? "DBName")

instance Hashable DBInstance where

instance NFData DBInstance where

-- | Provides a list of status information for a DB instance.
--
--
--
-- /See:/ 'dbInstanceStatusInfo' smart constructor.
data DBInstanceStatusInfo = DBInstanceStatusInfo'
  { _disiStatus     :: !(Maybe Text)
  , _disiNormal     :: !(Maybe Bool)
  , _disiStatusType :: !(Maybe Text)
  , _disiMessage    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBInstanceStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disiStatus' - Status of the DB instance. For a StatusType of read replica, the values can be replicating, error, stopped, or terminated.
--
-- * 'disiNormal' - Boolean value that is true if the instance is operating normally, or false if the instance is in an error state.
--
-- * 'disiStatusType' - This value is currently "read replication."
--
-- * 'disiMessage' - Details of the error if there is an error for the instance. If the instance is not in an error state, this value is blank.
dbInstanceStatusInfo
    :: DBInstanceStatusInfo
dbInstanceStatusInfo =
  DBInstanceStatusInfo'
    { _disiStatus = Nothing
    , _disiNormal = Nothing
    , _disiStatusType = Nothing
    , _disiMessage = Nothing
    }


-- | Status of the DB instance. For a StatusType of read replica, the values can be replicating, error, stopped, or terminated.
disiStatus :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatus = lens _disiStatus (\ s a -> s{_disiStatus = a})

-- | Boolean value that is true if the instance is operating normally, or false if the instance is in an error state.
disiNormal :: Lens' DBInstanceStatusInfo (Maybe Bool)
disiNormal = lens _disiNormal (\ s a -> s{_disiNormal = a})

-- | This value is currently "read replication."
disiStatusType :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatusType = lens _disiStatusType (\ s a -> s{_disiStatusType = a})

-- | Details of the error if there is an error for the instance. If the instance is not in an error state, this value is blank.
disiMessage :: Lens' DBInstanceStatusInfo (Maybe Text)
disiMessage = lens _disiMessage (\ s a -> s{_disiMessage = a})

instance FromXML DBInstanceStatusInfo where
        parseXML x
          = DBInstanceStatusInfo' <$>
              (x .@? "Status") <*> (x .@? "Normal") <*>
                (x .@? "StatusType")
                <*> (x .@? "Message")

instance Hashable DBInstanceStatusInfo where

instance NFData DBInstanceStatusInfo where

-- | Contains the details of an Amazon RDS DB parameter group.
--
--
-- This data type is used as a response element in the 'DescribeDBParameterGroups' action.
--
--
-- /See:/ 'dbParameterGroup' smart constructor.
data DBParameterGroup = DBParameterGroup'
  { _dpgDBParameterGroupARN    :: !(Maybe Text)
  , _dpgDBParameterGroupFamily :: !(Maybe Text)
  , _dpgDBParameterGroupName   :: !(Maybe Text)
  , _dpgDescription            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgDBParameterGroupARN' - The Amazon Resource Name (ARN) for the DB parameter group.
--
-- * 'dpgDBParameterGroupFamily' - Provides the name of the DB parameter group family that this DB parameter group is compatible with.
--
-- * 'dpgDBParameterGroupName' - Provides the name of the DB parameter group.
--
-- * 'dpgDescription' - Provides the customer-specified description for this DB parameter group.
dbParameterGroup
    :: DBParameterGroup
dbParameterGroup =
  DBParameterGroup'
    { _dpgDBParameterGroupARN = Nothing
    , _dpgDBParameterGroupFamily = Nothing
    , _dpgDBParameterGroupName = Nothing
    , _dpgDescription = Nothing
    }


-- | The Amazon Resource Name (ARN) for the DB parameter group.
dpgDBParameterGroupARN :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupARN = lens _dpgDBParameterGroupARN (\ s a -> s{_dpgDBParameterGroupARN = a})

-- | Provides the name of the DB parameter group family that this DB parameter group is compatible with.
dpgDBParameterGroupFamily :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupFamily = lens _dpgDBParameterGroupFamily (\ s a -> s{_dpgDBParameterGroupFamily = a})

-- | Provides the name of the DB parameter group.
dpgDBParameterGroupName :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupName = lens _dpgDBParameterGroupName (\ s a -> s{_dpgDBParameterGroupName = a})

-- | Provides the customer-specified description for this DB parameter group.
dpgDescription :: Lens' DBParameterGroup (Maybe Text)
dpgDescription = lens _dpgDescription (\ s a -> s{_dpgDescription = a})

instance FromXML DBParameterGroup where
        parseXML x
          = DBParameterGroup' <$>
              (x .@? "DBParameterGroupArn") <*>
                (x .@? "DBParameterGroupFamily")
                <*> (x .@? "DBParameterGroupName")
                <*> (x .@? "Description")

instance Hashable DBParameterGroup where

instance NFData DBParameterGroup where

-- | Contains the result of a successful invocation of the 'ModifyDBParameterGroup' or 'ResetDBParameterGroup' action.
--
--
--
-- /See:/ 'dbParameterGroupNameMessage' smart constructor.
newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage'
  { _dpgnmDBParameterGroupName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgnmDBParameterGroupName' - Provides the name of the DB parameter group.
dbParameterGroupNameMessage
    :: DBParameterGroupNameMessage
dbParameterGroupNameMessage =
  DBParameterGroupNameMessage' {_dpgnmDBParameterGroupName = Nothing}


-- | Provides the name of the DB parameter group.
dpgnmDBParameterGroupName :: Lens' DBParameterGroupNameMessage (Maybe Text)
dpgnmDBParameterGroupName = lens _dpgnmDBParameterGroupName (\ s a -> s{_dpgnmDBParameterGroupName = a})

instance FromXML DBParameterGroupNameMessage where
        parseXML x
          = DBParameterGroupNameMessage' <$>
              (x .@? "DBParameterGroupName")

instance Hashable DBParameterGroupNameMessage where

instance NFData DBParameterGroupNameMessage where

-- | The status of the DB parameter group.
--
--
-- This data type is used as a response element in the following actions:
--
--     * 'CreateDBInstance'
--
--     * 'CreateDBInstanceReadReplica'
--
--     * 'DeleteDBInstance'
--
--     * 'ModifyDBInstance'
--
--     * 'RebootDBInstance'
--
--     * 'RestoreDBInstanceFromDBSnapshot'
--
--
--
--
-- /See:/ 'dbParameterGroupStatus' smart constructor.
data DBParameterGroupStatus = DBParameterGroupStatus'
  { _dpgsDBParameterGroupName :: !(Maybe Text)
  , _dpgsParameterApplyStatus :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBParameterGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgsDBParameterGroupName' - The name of the DP parameter group.
--
-- * 'dpgsParameterApplyStatus' - The status of parameter updates.
dbParameterGroupStatus
    :: DBParameterGroupStatus
dbParameterGroupStatus =
  DBParameterGroupStatus'
    {_dpgsDBParameterGroupName = Nothing, _dpgsParameterApplyStatus = Nothing}


-- | The name of the DP parameter group.
dpgsDBParameterGroupName :: Lens' DBParameterGroupStatus (Maybe Text)
dpgsDBParameterGroupName = lens _dpgsDBParameterGroupName (\ s a -> s{_dpgsDBParameterGroupName = a})

-- | The status of parameter updates.
dpgsParameterApplyStatus :: Lens' DBParameterGroupStatus (Maybe Text)
dpgsParameterApplyStatus = lens _dpgsParameterApplyStatus (\ s a -> s{_dpgsParameterApplyStatus = a})

instance FromXML DBParameterGroupStatus where
        parseXML x
          = DBParameterGroupStatus' <$>
              (x .@? "DBParameterGroupName") <*>
                (x .@? "ParameterApplyStatus")

instance Hashable DBParameterGroupStatus where

instance NFData DBParameterGroupStatus where

-- | Contains the details for an Amazon RDS DB security group.
--
--
-- This data type is used as a response element in the 'DescribeDBSecurityGroups' action.
--
--
-- /See:/ 'dbSecurityGroup' smart constructor.
data DBSecurityGroup = DBSecurityGroup'
  { _dbsgVPCId                      :: !(Maybe Text)
  , _dbsgOwnerId                    :: !(Maybe Text)
  , _dbsgDBSecurityGroupARN         :: !(Maybe Text)
  , _dbsgIPRanges                   :: !(Maybe [IPRange])
  , _dbsgDBSecurityGroupName        :: !(Maybe Text)
  , _dbsgEC2SecurityGroups          :: !(Maybe [EC2SecurityGroup])
  , _dbsgDBSecurityGroupDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbsgVPCId' - Provides the VpcId of the DB security group.
--
-- * 'dbsgOwnerId' - Provides the AWS ID of the owner of a specific DB security group.
--
-- * 'dbsgDBSecurityGroupARN' - The Amazon Resource Name (ARN) for the DB security group.
--
-- * 'dbsgIPRanges' - Contains a list of 'IPRange' elements.
--
-- * 'dbsgDBSecurityGroupName' - Specifies the name of the DB security group.
--
-- * 'dbsgEC2SecurityGroups' - Contains a list of 'EC2SecurityGroup' elements.
--
-- * 'dbsgDBSecurityGroupDescription' - Provides the description of the DB security group.
dbSecurityGroup
    :: DBSecurityGroup
dbSecurityGroup =
  DBSecurityGroup'
    { _dbsgVPCId = Nothing
    , _dbsgOwnerId = Nothing
    , _dbsgDBSecurityGroupARN = Nothing
    , _dbsgIPRanges = Nothing
    , _dbsgDBSecurityGroupName = Nothing
    , _dbsgEC2SecurityGroups = Nothing
    , _dbsgDBSecurityGroupDescription = Nothing
    }


-- | Provides the VpcId of the DB security group.
dbsgVPCId :: Lens' DBSecurityGroup (Maybe Text)
dbsgVPCId = lens _dbsgVPCId (\ s a -> s{_dbsgVPCId = a})

-- | Provides the AWS ID of the owner of a specific DB security group.
dbsgOwnerId :: Lens' DBSecurityGroup (Maybe Text)
dbsgOwnerId = lens _dbsgOwnerId (\ s a -> s{_dbsgOwnerId = a})

-- | The Amazon Resource Name (ARN) for the DB security group.
dbsgDBSecurityGroupARN :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupARN = lens _dbsgDBSecurityGroupARN (\ s a -> s{_dbsgDBSecurityGroupARN = a})

-- | Contains a list of 'IPRange' elements.
dbsgIPRanges :: Lens' DBSecurityGroup [IPRange]
dbsgIPRanges = lens _dbsgIPRanges (\ s a -> s{_dbsgIPRanges = a}) . _Default . _Coerce

-- | Specifies the name of the DB security group.
dbsgDBSecurityGroupName :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupName = lens _dbsgDBSecurityGroupName (\ s a -> s{_dbsgDBSecurityGroupName = a})

-- | Contains a list of 'EC2SecurityGroup' elements.
dbsgEC2SecurityGroups :: Lens' DBSecurityGroup [EC2SecurityGroup]
dbsgEC2SecurityGroups = lens _dbsgEC2SecurityGroups (\ s a -> s{_dbsgEC2SecurityGroups = a}) . _Default . _Coerce

-- | Provides the description of the DB security group.
dbsgDBSecurityGroupDescription :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupDescription = lens _dbsgDBSecurityGroupDescription (\ s a -> s{_dbsgDBSecurityGroupDescription = a})

instance FromXML DBSecurityGroup where
        parseXML x
          = DBSecurityGroup' <$>
              (x .@? "VpcId") <*> (x .@? "OwnerId") <*>
                (x .@? "DBSecurityGroupArn")
                <*>
                (x .@? "IPRanges" .!@ mempty >>=
                   may (parseXMLList "IPRange"))
                <*> (x .@? "DBSecurityGroupName")
                <*>
                (x .@? "EC2SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "EC2SecurityGroup"))
                <*> (x .@? "DBSecurityGroupDescription")

instance Hashable DBSecurityGroup where

instance NFData DBSecurityGroup where

-- | This data type is used as a response element in the following actions:
--
--
--     * 'ModifyDBInstance'
--
--     * 'RebootDBInstance'
--
--     * 'RestoreDBInstanceFromDBSnapshot'
--
--     * 'RestoreDBInstanceToPointInTime'
--
--
--
--
-- /See:/ 'dbSecurityGroupMembership' smart constructor.
data DBSecurityGroupMembership = DBSecurityGroupMembership'
  { _dsgmStatus              :: !(Maybe Text)
  , _dsgmDBSecurityGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgmStatus' - The status of the DB security group.
--
-- * 'dsgmDBSecurityGroupName' - The name of the DB security group.
dbSecurityGroupMembership
    :: DBSecurityGroupMembership
dbSecurityGroupMembership =
  DBSecurityGroupMembership'
    {_dsgmStatus = Nothing, _dsgmDBSecurityGroupName = Nothing}


-- | The status of the DB security group.
dsgmStatus :: Lens' DBSecurityGroupMembership (Maybe Text)
dsgmStatus = lens _dsgmStatus (\ s a -> s{_dsgmStatus = a})

-- | The name of the DB security group.
dsgmDBSecurityGroupName :: Lens' DBSecurityGroupMembership (Maybe Text)
dsgmDBSecurityGroupName = lens _dsgmDBSecurityGroupName (\ s a -> s{_dsgmDBSecurityGroupName = a})

instance FromXML DBSecurityGroupMembership where
        parseXML x
          = DBSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "DBSecurityGroupName")

instance Hashable DBSecurityGroupMembership where

instance NFData DBSecurityGroupMembership where

-- | Contains the details of an Amazon RDS DB snapshot.
--
--
-- This data type is used as a response element in the 'DescribeDBSnapshots' action.
--
--
-- /See:/ 'dbSnapshot' smart constructor.
data DBSnapshot = DBSnapshot'
  { _dsEngineVersion                    :: !(Maybe Text)
  , _dsStatus                           :: !(Maybe Text)
  , _dsDBSnapshotARN                    :: !(Maybe Text)
  , _dsMasterUsername                   :: !(Maybe Text)
  , _dsSourceRegion                     :: !(Maybe Text)
  , _dsIAMDatabaseAuthenticationEnabled :: !(Maybe Bool)
  , _dsIOPS                             :: !(Maybe Int)
  , _dsVPCId                            :: !(Maybe Text)
  , _dsInstanceCreateTime               :: !(Maybe ISO8601)
  , _dsEngine                           :: !(Maybe Text)
  , _dsEncrypted                        :: !(Maybe Bool)
  , _dsDBSnapshotIdentifier             :: !(Maybe Text)
  , _dsLicenseModel                     :: !(Maybe Text)
  , _dsSourceDBSnapshotIdentifier       :: !(Maybe Text)
  , _dsSnapshotType                     :: !(Maybe Text)
  , _dsDBInstanceIdentifier             :: !(Maybe Text)
  , _dsKMSKeyId                         :: !(Maybe Text)
  , _dsAvailabilityZone                 :: !(Maybe Text)
  , _dsSnapshotCreateTime               :: !(Maybe ISO8601)
  , _dsAllocatedStorage                 :: !(Maybe Int)
  , _dsOptionGroupName                  :: !(Maybe Text)
  , _dsTimezone                         :: !(Maybe Text)
  , _dsTDECredentialARN                 :: !(Maybe Text)
  , _dsPercentProgress                  :: !(Maybe Int)
  , _dsPort                             :: !(Maybe Int)
  , _dsStorageType                      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsEngineVersion' - Specifies the version of the database engine.
--
-- * 'dsStatus' - Specifies the status of this DB snapshot.
--
-- * 'dsDBSnapshotARN' - The Amazon Resource Name (ARN) for the DB snapshot.
--
-- * 'dsMasterUsername' - Provides the master username for the DB snapshot.
--
-- * 'dsSourceRegion' - The AWS Region that the DB snapshot was created in or copied from.
--
-- * 'dsIAMDatabaseAuthenticationEnabled' - True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
--
-- * 'dsIOPS' - Specifies the Provisioned IOPS (I/O operations per second) value of the DB instance at the time of the snapshot.
--
-- * 'dsVPCId' - Provides the VPC ID associated with the DB snapshot.
--
-- * 'dsInstanceCreateTime' - Specifies the time when the snapshot was taken, in Universal Coordinated Time (UTC).
--
-- * 'dsEngine' - Specifies the name of the database engine.
--
-- * 'dsEncrypted' - Specifies whether the DB snapshot is encrypted.
--
-- * 'dsDBSnapshotIdentifier' - Specifies the identifier for the DB snapshot.
--
-- * 'dsLicenseModel' - License model information for the restored DB instance.
--
-- * 'dsSourceDBSnapshotIdentifier' - The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was copied from. It only has value in case of cross-customer or cross-region copy.
--
-- * 'dsSnapshotType' - Provides the type of the DB snapshot.
--
-- * 'dsDBInstanceIdentifier' - Specifies the DB instance identifier of the DB instance this DB snapshot was created from.
--
-- * 'dsKMSKeyId' - If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB snapshot.
--
-- * 'dsAvailabilityZone' - Specifies the name of the Availability Zone the DB instance was located in at the time of the DB snapshot.
--
-- * 'dsSnapshotCreateTime' - Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
--
-- * 'dsAllocatedStorage' - Specifies the allocated storage size in gibibytes (GiB).
--
-- * 'dsOptionGroupName' - Provides the option group name for the DB snapshot.
--
-- * 'dsTimezone' - The time zone of the DB snapshot. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for snapshots taken from Microsoft SQL Server DB instances that were created with a time zone specified.
--
-- * 'dsTDECredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
--
-- * 'dsPercentProgress' - The percentage of the estimated data that has been transferred.
--
-- * 'dsPort' - Specifies the port that the database engine was listening on at the time of the snapshot.
--
-- * 'dsStorageType' - Specifies the storage type associated with DB snapshot.
dbSnapshot
    :: DBSnapshot
dbSnapshot =
  DBSnapshot'
    { _dsEngineVersion = Nothing
    , _dsStatus = Nothing
    , _dsDBSnapshotARN = Nothing
    , _dsMasterUsername = Nothing
    , _dsSourceRegion = Nothing
    , _dsIAMDatabaseAuthenticationEnabled = Nothing
    , _dsIOPS = Nothing
    , _dsVPCId = Nothing
    , _dsInstanceCreateTime = Nothing
    , _dsEngine = Nothing
    , _dsEncrypted = Nothing
    , _dsDBSnapshotIdentifier = Nothing
    , _dsLicenseModel = Nothing
    , _dsSourceDBSnapshotIdentifier = Nothing
    , _dsSnapshotType = Nothing
    , _dsDBInstanceIdentifier = Nothing
    , _dsKMSKeyId = Nothing
    , _dsAvailabilityZone = Nothing
    , _dsSnapshotCreateTime = Nothing
    , _dsAllocatedStorage = Nothing
    , _dsOptionGroupName = Nothing
    , _dsTimezone = Nothing
    , _dsTDECredentialARN = Nothing
    , _dsPercentProgress = Nothing
    , _dsPort = Nothing
    , _dsStorageType = Nothing
    }


-- | Specifies the version of the database engine.
dsEngineVersion :: Lens' DBSnapshot (Maybe Text)
dsEngineVersion = lens _dsEngineVersion (\ s a -> s{_dsEngineVersion = a})

-- | Specifies the status of this DB snapshot.
dsStatus :: Lens' DBSnapshot (Maybe Text)
dsStatus = lens _dsStatus (\ s a -> s{_dsStatus = a})

-- | The Amazon Resource Name (ARN) for the DB snapshot.
dsDBSnapshotARN :: Lens' DBSnapshot (Maybe Text)
dsDBSnapshotARN = lens _dsDBSnapshotARN (\ s a -> s{_dsDBSnapshotARN = a})

-- | Provides the master username for the DB snapshot.
dsMasterUsername :: Lens' DBSnapshot (Maybe Text)
dsMasterUsername = lens _dsMasterUsername (\ s a -> s{_dsMasterUsername = a})

-- | The AWS Region that the DB snapshot was created in or copied from.
dsSourceRegion :: Lens' DBSnapshot (Maybe Text)
dsSourceRegion = lens _dsSourceRegion (\ s a -> s{_dsSourceRegion = a})

-- | True if mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled, and otherwise false.
dsIAMDatabaseAuthenticationEnabled :: Lens' DBSnapshot (Maybe Bool)
dsIAMDatabaseAuthenticationEnabled = lens _dsIAMDatabaseAuthenticationEnabled (\ s a -> s{_dsIAMDatabaseAuthenticationEnabled = a})

-- | Specifies the Provisioned IOPS (I/O operations per second) value of the DB instance at the time of the snapshot.
dsIOPS :: Lens' DBSnapshot (Maybe Int)
dsIOPS = lens _dsIOPS (\ s a -> s{_dsIOPS = a})

-- | Provides the VPC ID associated with the DB snapshot.
dsVPCId :: Lens' DBSnapshot (Maybe Text)
dsVPCId = lens _dsVPCId (\ s a -> s{_dsVPCId = a})

-- | Specifies the time when the snapshot was taken, in Universal Coordinated Time (UTC).
dsInstanceCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dsInstanceCreateTime = lens _dsInstanceCreateTime (\ s a -> s{_dsInstanceCreateTime = a}) . mapping _Time

-- | Specifies the name of the database engine.
dsEngine :: Lens' DBSnapshot (Maybe Text)
dsEngine = lens _dsEngine (\ s a -> s{_dsEngine = a})

-- | Specifies whether the DB snapshot is encrypted.
dsEncrypted :: Lens' DBSnapshot (Maybe Bool)
dsEncrypted = lens _dsEncrypted (\ s a -> s{_dsEncrypted = a})

-- | Specifies the identifier for the DB snapshot.
dsDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dsDBSnapshotIdentifier = lens _dsDBSnapshotIdentifier (\ s a -> s{_dsDBSnapshotIdentifier = a})

-- | License model information for the restored DB instance.
dsLicenseModel :: Lens' DBSnapshot (Maybe Text)
dsLicenseModel = lens _dsLicenseModel (\ s a -> s{_dsLicenseModel = a})

-- | The DB snapshot Amazon Resource Name (ARN) that the DB snapshot was copied from. It only has value in case of cross-customer or cross-region copy.
dsSourceDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dsSourceDBSnapshotIdentifier = lens _dsSourceDBSnapshotIdentifier (\ s a -> s{_dsSourceDBSnapshotIdentifier = a})

-- | Provides the type of the DB snapshot.
dsSnapshotType :: Lens' DBSnapshot (Maybe Text)
dsSnapshotType = lens _dsSnapshotType (\ s a -> s{_dsSnapshotType = a})

-- | Specifies the DB instance identifier of the DB instance this DB snapshot was created from.
dsDBInstanceIdentifier :: Lens' DBSnapshot (Maybe Text)
dsDBInstanceIdentifier = lens _dsDBInstanceIdentifier (\ s a -> s{_dsDBInstanceIdentifier = a})

-- | If @Encrypted@ is true, the AWS KMS key identifier for the encrypted DB snapshot.
dsKMSKeyId :: Lens' DBSnapshot (Maybe Text)
dsKMSKeyId = lens _dsKMSKeyId (\ s a -> s{_dsKMSKeyId = a})

-- | Specifies the name of the Availability Zone the DB instance was located in at the time of the DB snapshot.
dsAvailabilityZone :: Lens' DBSnapshot (Maybe Text)
dsAvailabilityZone = lens _dsAvailabilityZone (\ s a -> s{_dsAvailabilityZone = a})

-- | Provides the time when the snapshot was taken, in Universal Coordinated Time (UTC).
dsSnapshotCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dsSnapshotCreateTime = lens _dsSnapshotCreateTime (\ s a -> s{_dsSnapshotCreateTime = a}) . mapping _Time

-- | Specifies the allocated storage size in gibibytes (GiB).
dsAllocatedStorage :: Lens' DBSnapshot (Maybe Int)
dsAllocatedStorage = lens _dsAllocatedStorage (\ s a -> s{_dsAllocatedStorage = a})

-- | Provides the option group name for the DB snapshot.
dsOptionGroupName :: Lens' DBSnapshot (Maybe Text)
dsOptionGroupName = lens _dsOptionGroupName (\ s a -> s{_dsOptionGroupName = a})

-- | The time zone of the DB snapshot. In most cases, the @Timezone@ element is empty. @Timezone@ content appears only for snapshots taken from Microsoft SQL Server DB instances that were created with a time zone specified.
dsTimezone :: Lens' DBSnapshot (Maybe Text)
dsTimezone = lens _dsTimezone (\ s a -> s{_dsTimezone = a})

-- | The ARN from the key store with which to associate the instance for TDE encryption.
dsTDECredentialARN :: Lens' DBSnapshot (Maybe Text)
dsTDECredentialARN = lens _dsTDECredentialARN (\ s a -> s{_dsTDECredentialARN = a})

-- | The percentage of the estimated data that has been transferred.
dsPercentProgress :: Lens' DBSnapshot (Maybe Int)
dsPercentProgress = lens _dsPercentProgress (\ s a -> s{_dsPercentProgress = a})

-- | Specifies the port that the database engine was listening on at the time of the snapshot.
dsPort :: Lens' DBSnapshot (Maybe Int)
dsPort = lens _dsPort (\ s a -> s{_dsPort = a})

-- | Specifies the storage type associated with DB snapshot.
dsStorageType :: Lens' DBSnapshot (Maybe Text)
dsStorageType = lens _dsStorageType (\ s a -> s{_dsStorageType = a})

instance FromXML DBSnapshot where
        parseXML x
          = DBSnapshot' <$>
              (x .@? "EngineVersion") <*> (x .@? "Status") <*>
                (x .@? "DBSnapshotArn")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "SourceRegion")
                <*> (x .@? "IAMDatabaseAuthenticationEnabled")
                <*> (x .@? "Iops")
                <*> (x .@? "VpcId")
                <*> (x .@? "InstanceCreateTime")
                <*> (x .@? "Engine")
                <*> (x .@? "Encrypted")
                <*> (x .@? "DBSnapshotIdentifier")
                <*> (x .@? "LicenseModel")
                <*> (x .@? "SourceDBSnapshotIdentifier")
                <*> (x .@? "SnapshotType")
                <*> (x .@? "DBInstanceIdentifier")
                <*> (x .@? "KmsKeyId")
                <*> (x .@? "AvailabilityZone")
                <*> (x .@? "SnapshotCreateTime")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "OptionGroupName")
                <*> (x .@? "Timezone")
                <*> (x .@? "TdeCredentialArn")
                <*> (x .@? "PercentProgress")
                <*> (x .@? "Port")
                <*> (x .@? "StorageType")

instance Hashable DBSnapshot where

instance NFData DBSnapshot where

-- | Contains the name and values of a manual DB snapshot attribute
--
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts to restore a manual DB snapshot. For more information, see the 'ModifyDBSnapshotAttribute' API.
--
--
-- /See:/ 'dbSnapshotAttribute' smart constructor.
data DBSnapshotAttribute = DBSnapshotAttribute'
  { _dsaAttributeValues :: !(Maybe [Text])
  , _dsaAttributeName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBSnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaAttributeValues' - The value or values for the manual DB snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB snapshot. If a value of @all@ is in the list, then the manual DB snapshot is public and available for any AWS account to copy or restore.
--
-- * 'dsaAttributeName' - The name of the manual DB snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the 'ModifyDBSnapshotAttribute' API action.
dbSnapshotAttribute
    :: DBSnapshotAttribute
dbSnapshotAttribute =
  DBSnapshotAttribute'
    {_dsaAttributeValues = Nothing, _dsaAttributeName = Nothing}


-- | The value or values for the manual DB snapshot attribute. If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB snapshot. If a value of @all@ is in the list, then the manual DB snapshot is public and available for any AWS account to copy or restore.
dsaAttributeValues :: Lens' DBSnapshotAttribute [Text]
dsaAttributeValues = lens _dsaAttributeValues (\ s a -> s{_dsaAttributeValues = a}) . _Default . _Coerce

-- | The name of the manual DB snapshot attribute. The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the 'ModifyDBSnapshotAttribute' API action.
dsaAttributeName :: Lens' DBSnapshotAttribute (Maybe Text)
dsaAttributeName = lens _dsaAttributeName (\ s a -> s{_dsaAttributeName = a})

instance FromXML DBSnapshotAttribute where
        parseXML x
          = DBSnapshotAttribute' <$>
              (x .@? "AttributeValues" .!@ mempty >>=
                 may (parseXMLList "AttributeValue"))
                <*> (x .@? "AttributeName")

instance Hashable DBSnapshotAttribute where

instance NFData DBSnapshotAttribute where

-- | Contains the results of a successful call to the 'DescribeDBSnapshotAttributes' API action.
--
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts to copy or restore a manual DB snapshot. For more information, see the 'ModifyDBSnapshotAttribute' API action.
--
--
-- /See:/ 'dbSnapshotAttributesResult' smart constructor.
data DBSnapshotAttributesResult = DBSnapshotAttributesResult'
  { _dsarDBSnapshotIdentifier :: !(Maybe Text)
  , _dsarDBSnapshotAttributes :: !(Maybe [DBSnapshotAttribute])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBSnapshotAttributesResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarDBSnapshotIdentifier' - The identifier of the manual DB snapshot that the attributes apply to.
--
-- * 'dsarDBSnapshotAttributes' - The list of attributes and values for the manual DB snapshot.
dbSnapshotAttributesResult
    :: DBSnapshotAttributesResult
dbSnapshotAttributesResult =
  DBSnapshotAttributesResult'
    {_dsarDBSnapshotIdentifier = Nothing, _dsarDBSnapshotAttributes = Nothing}


-- | The identifier of the manual DB snapshot that the attributes apply to.
dsarDBSnapshotIdentifier :: Lens' DBSnapshotAttributesResult (Maybe Text)
dsarDBSnapshotIdentifier = lens _dsarDBSnapshotIdentifier (\ s a -> s{_dsarDBSnapshotIdentifier = a})

-- | The list of attributes and values for the manual DB snapshot.
dsarDBSnapshotAttributes :: Lens' DBSnapshotAttributesResult [DBSnapshotAttribute]
dsarDBSnapshotAttributes = lens _dsarDBSnapshotAttributes (\ s a -> s{_dsarDBSnapshotAttributes = a}) . _Default . _Coerce

instance FromXML DBSnapshotAttributesResult where
        parseXML x
          = DBSnapshotAttributesResult' <$>
              (x .@? "DBSnapshotIdentifier") <*>
                (x .@? "DBSnapshotAttributes" .!@ mempty >>=
                   may (parseXMLList "DBSnapshotAttribute"))

instance Hashable DBSnapshotAttributesResult where

instance NFData DBSnapshotAttributesResult where

-- | Contains the details of an Amazon RDS DB subnet group.
--
--
-- This data type is used as a response element in the 'DescribeDBSubnetGroups' action.
--
--
-- /See:/ 'dbSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
  { _dsgDBSubnetGroupName        :: !(Maybe Text)
  , _dsgVPCId                    :: !(Maybe Text)
  , _dsgSubnets                  :: !(Maybe [Subnet])
  , _dsgDBSubnetGroupDescription :: !(Maybe Text)
  , _dsgDBSubnetGroupARN         :: !(Maybe Text)
  , _dsgSubnetGroupStatus        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DBSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgDBSubnetGroupName' - The name of the DB subnet group.
--
-- * 'dsgVPCId' - Provides the VpcId of the DB subnet group.
--
-- * 'dsgSubnets' - Contains a list of 'Subnet' elements.
--
-- * 'dsgDBSubnetGroupDescription' - Provides the description of the DB subnet group.
--
-- * 'dsgDBSubnetGroupARN' - The Amazon Resource Name (ARN) for the DB subnet group.
--
-- * 'dsgSubnetGroupStatus' - Provides the status of the DB subnet group.
dbSubnetGroup
    :: DBSubnetGroup
dbSubnetGroup =
  DBSubnetGroup'
    { _dsgDBSubnetGroupName = Nothing
    , _dsgVPCId = Nothing
    , _dsgSubnets = Nothing
    , _dsgDBSubnetGroupDescription = Nothing
    , _dsgDBSubnetGroupARN = Nothing
    , _dsgSubnetGroupStatus = Nothing
    }


-- | The name of the DB subnet group.
dsgDBSubnetGroupName :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupName = lens _dsgDBSubnetGroupName (\ s a -> s{_dsgDBSubnetGroupName = a})

-- | Provides the VpcId of the DB subnet group.
dsgVPCId :: Lens' DBSubnetGroup (Maybe Text)
dsgVPCId = lens _dsgVPCId (\ s a -> s{_dsgVPCId = a})

-- | Contains a list of 'Subnet' elements.
dsgSubnets :: Lens' DBSubnetGroup [Subnet]
dsgSubnets = lens _dsgSubnets (\ s a -> s{_dsgSubnets = a}) . _Default . _Coerce

-- | Provides the description of the DB subnet group.
dsgDBSubnetGroupDescription :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupDescription = lens _dsgDBSubnetGroupDescription (\ s a -> s{_dsgDBSubnetGroupDescription = a})

-- | The Amazon Resource Name (ARN) for the DB subnet group.
dsgDBSubnetGroupARN :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupARN = lens _dsgDBSubnetGroupARN (\ s a -> s{_dsgDBSubnetGroupARN = a})

-- | Provides the status of the DB subnet group.
dsgSubnetGroupStatus :: Lens' DBSubnetGroup (Maybe Text)
dsgSubnetGroupStatus = lens _dsgSubnetGroupStatus (\ s a -> s{_dsgSubnetGroupStatus = a})

instance FromXML DBSubnetGroup where
        parseXML x
          = DBSubnetGroup' <$>
              (x .@? "DBSubnetGroupName") <*> (x .@? "VpcId") <*>
                (x .@? "Subnets" .!@ mempty >>=
                   may (parseXMLList "Subnet"))
                <*> (x .@? "DBSubnetGroupDescription")
                <*> (x .@? "DBSubnetGroupArn")
                <*> (x .@? "SubnetGroupStatus")

instance Hashable DBSubnetGroup where

instance NFData DBSubnetGroup where

-- | This data type is used as a response element to 'DescribeDBLogFiles' .
--
--
--
-- /See:/ 'describeDBLogFilesDetails' smart constructor.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails'
  { _ddlfdLastWritten :: !(Maybe Integer)
  , _ddlfdSize        :: !(Maybe Integer)
  , _ddlfdLogFileName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBLogFilesDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddlfdLastWritten' - A POSIX timestamp when the last log entry was written.
--
-- * 'ddlfdSize' - The size, in bytes, of the log file for the specified DB instance.
--
-- * 'ddlfdLogFileName' - The name of the log file for the specified DB instance.
describeDBLogFilesDetails
    :: DescribeDBLogFilesDetails
describeDBLogFilesDetails =
  DescribeDBLogFilesDetails'
    { _ddlfdLastWritten = Nothing
    , _ddlfdSize = Nothing
    , _ddlfdLogFileName = Nothing
    }


-- | A POSIX timestamp when the last log entry was written.
ddlfdLastWritten :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddlfdLastWritten = lens _ddlfdLastWritten (\ s a -> s{_ddlfdLastWritten = a})

-- | The size, in bytes, of the log file for the specified DB instance.
ddlfdSize :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddlfdSize = lens _ddlfdSize (\ s a -> s{_ddlfdSize = a})

-- | The name of the log file for the specified DB instance.
ddlfdLogFileName :: Lens' DescribeDBLogFilesDetails (Maybe Text)
ddlfdLogFileName = lens _ddlfdLogFileName (\ s a -> s{_ddlfdLogFileName = a})

instance FromXML DescribeDBLogFilesDetails where
        parseXML x
          = DescribeDBLogFilesDetails' <$>
              (x .@? "LastWritten") <*> (x .@? "Size") <*>
                (x .@? "LogFileName")

instance Hashable DescribeDBLogFilesDetails where

instance NFData DescribeDBLogFilesDetails where

-- | An Active Directory Domain membership record associated with the DB instance.
--
--
--
-- /See:/ 'domainMembership' smart constructor.
data DomainMembership = DomainMembership'
  { _dmStatus      :: !(Maybe Text)
  , _dmFQDN        :: !(Maybe Text)
  , _dmDomain      :: !(Maybe Text)
  , _dmIAMRoleName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmStatus' - The status of the DB instance's Active Directory Domain membership, such as joined, pending-join, failed etc).
--
-- * 'dmFQDN' - The fully qualified domain name of the Active Directory Domain.
--
-- * 'dmDomain' - The identifier of the Active Directory Domain.
--
-- * 'dmIAMRoleName' - The name of the IAM role to be used when making API calls to the Directory Service.
domainMembership
    :: DomainMembership
domainMembership =
  DomainMembership'
    { _dmStatus = Nothing
    , _dmFQDN = Nothing
    , _dmDomain = Nothing
    , _dmIAMRoleName = Nothing
    }


-- | The status of the DB instance's Active Directory Domain membership, such as joined, pending-join, failed etc).
dmStatus :: Lens' DomainMembership (Maybe Text)
dmStatus = lens _dmStatus (\ s a -> s{_dmStatus = a})

-- | The fully qualified domain name of the Active Directory Domain.
dmFQDN :: Lens' DomainMembership (Maybe Text)
dmFQDN = lens _dmFQDN (\ s a -> s{_dmFQDN = a})

-- | The identifier of the Active Directory Domain.
dmDomain :: Lens' DomainMembership (Maybe Text)
dmDomain = lens _dmDomain (\ s a -> s{_dmDomain = a})

-- | The name of the IAM role to be used when making API calls to the Directory Service.
dmIAMRoleName :: Lens' DomainMembership (Maybe Text)
dmIAMRoleName = lens _dmIAMRoleName (\ s a -> s{_dmIAMRoleName = a})

instance FromXML DomainMembership where
        parseXML x
          = DomainMembership' <$>
              (x .@? "Status") <*> (x .@? "FQDN") <*>
                (x .@? "Domain")
                <*> (x .@? "IAMRoleName")

instance Hashable DomainMembership where

instance NFData DomainMembership where

-- | A range of double values.
--
--
--
-- /See:/ 'doubleRange' smart constructor.
data DoubleRange = DoubleRange'
  { _drTo   :: !(Maybe Double)
  , _drFrom :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DoubleRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drTo' - The maximum value in the range.
--
-- * 'drFrom' - The minimum value in the range.
doubleRange
    :: DoubleRange
doubleRange = DoubleRange' {_drTo = Nothing, _drFrom = Nothing}


-- | The maximum value in the range.
drTo :: Lens' DoubleRange (Maybe Double)
drTo = lens _drTo (\ s a -> s{_drTo = a})

-- | The minimum value in the range.
drFrom :: Lens' DoubleRange (Maybe Double)
drFrom = lens _drFrom (\ s a -> s{_drFrom = a})

instance FromXML DoubleRange where
        parseXML x
          = DoubleRange' <$> (x .@? "To") <*> (x .@? "From")

instance Hashable DoubleRange where

instance NFData DoubleRange where

-- | This data type is used as a response element in the following actions:
--
--
--     * 'AuthorizeDBSecurityGroupIngress'
--
--     * 'DescribeDBSecurityGroups'
--
--     * 'RevokeDBSecurityGroupIngress'
--
--
--
--
-- /See:/ 'ec2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
  { _esgStatus                  :: !(Maybe Text)
  , _esgEC2SecurityGroupOwnerId :: !(Maybe Text)
  , _esgEC2SecurityGroupName    :: !(Maybe Text)
  , _esgEC2SecurityGroupId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EC2SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esgStatus' - Provides the status of the EC2 security group. Status can be "authorizing", "authorized", "revoking", and "revoked".
--
-- * 'esgEC2SecurityGroupOwnerId' - Specifies the AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
--
-- * 'esgEC2SecurityGroupName' - Specifies the name of the EC2 security group.
--
-- * 'esgEC2SecurityGroupId' - Specifies the id of the EC2 security group.
ec2SecurityGroup
    :: EC2SecurityGroup
ec2SecurityGroup =
  EC2SecurityGroup'
    { _esgStatus = Nothing
    , _esgEC2SecurityGroupOwnerId = Nothing
    , _esgEC2SecurityGroupName = Nothing
    , _esgEC2SecurityGroupId = Nothing
    }


-- | Provides the status of the EC2 security group. Status can be "authorizing", "authorized", "revoking", and "revoked".
esgStatus :: Lens' EC2SecurityGroup (Maybe Text)
esgStatus = lens _esgStatus (\ s a -> s{_esgStatus = a})

-- | Specifies the AWS ID of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ field.
esgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupOwnerId = lens _esgEC2SecurityGroupOwnerId (\ s a -> s{_esgEC2SecurityGroupOwnerId = a})

-- | Specifies the name of the EC2 security group.
esgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupName = lens _esgEC2SecurityGroupName (\ s a -> s{_esgEC2SecurityGroupName = a})

-- | Specifies the id of the EC2 security group.
esgEC2SecurityGroupId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupId = lens _esgEC2SecurityGroupId (\ s a -> s{_esgEC2SecurityGroupId = a})

instance FromXML EC2SecurityGroup where
        parseXML x
          = EC2SecurityGroup' <$>
              (x .@? "Status") <*>
                (x .@? "EC2SecurityGroupOwnerId")
                <*> (x .@? "EC2SecurityGroupName")
                <*> (x .@? "EC2SecurityGroupId")

instance Hashable EC2SecurityGroup where

instance NFData EC2SecurityGroup where

-- | This data type is used as a response element in the following actions:
--
--
--     * 'CreateDBInstance'
--
--     * 'DescribeDBInstances'
--
--     * 'DeleteDBInstance'
--
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eHostedZoneId :: !(Maybe Text)
  , _eAddress      :: !(Maybe Text)
  , _ePort         :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eHostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- * 'eAddress' - Specifies the DNS address of the DB instance.
--
-- * 'ePort' - Specifies the port that the database engine is listening on.
endpoint
    :: Endpoint
endpoint =
  Endpoint' {_eHostedZoneId = Nothing, _eAddress = Nothing, _ePort = Nothing}


-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
eHostedZoneId :: Lens' Endpoint (Maybe Text)
eHostedZoneId = lens _eHostedZoneId (\ s a -> s{_eHostedZoneId = a})

-- | Specifies the DNS address of the DB instance.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\ s a -> s{_eAddress = a})

-- | Specifies the port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a})

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$>
              (x .@? "HostedZoneId") <*> (x .@? "Address") <*>
                (x .@? "Port")

instance Hashable Endpoint where

instance NFData Endpoint where

-- | Contains the result of a successful invocation of the 'DescribeEngineDefaultParameters' action.
--
--
--
-- /See:/ 'engineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { _edDBParameterGroupFamily :: !(Maybe Text)
  , _edMarker                 :: !(Maybe Text)
  , _edParameters             :: !(Maybe [Parameter])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EngineDefaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edDBParameterGroupFamily' - Specifies the name of the DB parameter group family that the engine default parameters apply to.
--
-- * 'edMarker' - An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'edParameters' - Contains a list of engine default parameters.
engineDefaults
    :: EngineDefaults
engineDefaults =
  EngineDefaults'
    { _edDBParameterGroupFamily = Nothing
    , _edMarker = Nothing
    , _edParameters = Nothing
    }


-- | Specifies the name of the DB parameter group family that the engine default parameters apply to.
edDBParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edDBParameterGroupFamily = lens _edDBParameterGroupFamily (\ s a -> s{_edDBParameterGroupFamily = a})

-- | An optional pagination token provided by a previous EngineDefaults request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\ s a -> s{_edMarker = a})

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\ s a -> s{_edParameters = a}) . _Default . _Coerce

instance FromXML EngineDefaults where
        parseXML x
          = EngineDefaults' <$>
              (x .@? "DBParameterGroupFamily") <*> (x .@? "Marker")
                <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "Parameter"))

instance Hashable EngineDefaults where

instance NFData EngineDefaults where

-- | This data type is used as a response element in the 'DescribeEvents' action.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eSourceType       :: !(Maybe SourceType)
  , _eSourceARN        :: !(Maybe Text)
  , _eSourceIdentifier :: !(Maybe Text)
  , _eDate             :: !(Maybe ISO8601)
  , _eEventCategories  :: !(Maybe [Text])
  , _eMessage          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceType' - Specifies the source type for this event.
--
-- * 'eSourceARN' - The Amazon Resource Name (ARN) for the event.
--
-- * 'eSourceIdentifier' - Provides the identifier for the source of the event.
--
-- * 'eDate' - Specifies the date and time of the event.
--
-- * 'eEventCategories' - Specifies the category for the event.
--
-- * 'eMessage' - Provides the text of this event.
event
    :: Event
event =
  Event'
    { _eSourceType = Nothing
    , _eSourceARN = Nothing
    , _eSourceIdentifier = Nothing
    , _eDate = Nothing
    , _eEventCategories = Nothing
    , _eMessage = Nothing
    }


-- | Specifies the source type for this event.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\ s a -> s{_eSourceType = a})

-- | The Amazon Resource Name (ARN) for the event.
eSourceARN :: Lens' Event (Maybe Text)
eSourceARN = lens _eSourceARN (\ s a -> s{_eSourceARN = a})

-- | Provides the identifier for the source of the event.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier = lens _eSourceIdentifier (\ s a -> s{_eSourceIdentifier = a})

-- | Specifies the date and time of the event.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\ s a -> s{_eDate = a}) . mapping _Time

-- | Specifies the category for the event.
eEventCategories :: Lens' Event [Text]
eEventCategories = lens _eEventCategories (\ s a -> s{_eEventCategories = a}) . _Default . _Coerce

-- | Provides the text of this event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a})

instance FromXML Event where
        parseXML x
          = Event' <$>
              (x .@? "SourceType") <*> (x .@? "SourceArn") <*>
                (x .@? "SourceIdentifier")
                <*> (x .@? "Date")
                <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*> (x .@? "Message")

instance Hashable Event where

instance NFData Event where

-- | Contains the results of a successful invocation of the 'DescribeEventCategories' action.
--
--
--
-- /See:/ 'eventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
  { _ecmSourceType      :: !(Maybe Text)
  , _ecmEventCategories :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventCategoriesMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecmSourceType' - The source type that the returned categories belong to
--
-- * 'ecmEventCategories' - The event categories for the specified source type
eventCategoriesMap
    :: EventCategoriesMap
eventCategoriesMap =
  EventCategoriesMap' {_ecmSourceType = Nothing, _ecmEventCategories = Nothing}


-- | The source type that the returned categories belong to
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\ s a -> s{_ecmSourceType = a})

-- | The event categories for the specified source type
ecmEventCategories :: Lens' EventCategoriesMap [Text]
ecmEventCategories = lens _ecmEventCategories (\ s a -> s{_ecmEventCategories = a}) . _Default . _Coerce

instance FromXML EventCategoriesMap where
        parseXML x
          = EventCategoriesMap' <$>
              (x .@? "SourceType") <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))

instance Hashable EventCategoriesMap where

instance NFData EventCategoriesMap where

-- | Contains the results of a successful invocation of the 'DescribeEventSubscriptions' action.
--
--
--
-- /See:/ 'eventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { _esStatus                   :: !(Maybe Text)
  , _esCustomerAWSId            :: !(Maybe Text)
  , _esCustSubscriptionId       :: !(Maybe Text)
  , _esSNSTopicARN              :: !(Maybe Text)
  , _esEventSubscriptionARN     :: !(Maybe Text)
  , _esEnabled                  :: !(Maybe Bool)
  , _esSourceType               :: !(Maybe Text)
  , _esSubscriptionCreationTime :: !(Maybe Text)
  , _esEventCategoriesList      :: !(Maybe [Text])
  , _esSourceIdsList            :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esStatus' - The status of the RDS event notification subscription. Constraints: Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist The status "no-permission" indicates that RDS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
--
-- * 'esCustomerAWSId' - The AWS customer account associated with the RDS event notification subscription.
--
-- * 'esCustSubscriptionId' - The RDS event notification subscription Id.
--
-- * 'esSNSTopicARN' - The topic ARN of the RDS event notification subscription.
--
-- * 'esEventSubscriptionARN' - The Amazon Resource Name (ARN) for the event subscription.
--
-- * 'esEnabled' - A Boolean value indicating if the subscription is enabled. True indicates the subscription is enabled.
--
-- * 'esSourceType' - The source type for the RDS event notification subscription.
--
-- * 'esSubscriptionCreationTime' - The time the RDS event notification subscription was created.
--
-- * 'esEventCategoriesList' - A list of event categories for the RDS event notification subscription.
--
-- * 'esSourceIdsList' - A list of source IDs for the RDS event notification subscription.
eventSubscription
    :: EventSubscription
eventSubscription =
  EventSubscription'
    { _esStatus = Nothing
    , _esCustomerAWSId = Nothing
    , _esCustSubscriptionId = Nothing
    , _esSNSTopicARN = Nothing
    , _esEventSubscriptionARN = Nothing
    , _esEnabled = Nothing
    , _esSourceType = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esEventCategoriesList = Nothing
    , _esSourceIdsList = Nothing
    }


-- | The status of the RDS event notification subscription. Constraints: Can be one of the following: creating | modifying | deleting | active | no-permission | topic-not-exist The status "no-permission" indicates that RDS no longer has permission to post to the SNS topic. The status "topic-not-exist" indicates that the topic was deleted after the subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\ s a -> s{_esStatus = a})

-- | The AWS customer account associated with the RDS event notification subscription.
esCustomerAWSId :: Lens' EventSubscription (Maybe Text)
esCustomerAWSId = lens _esCustomerAWSId (\ s a -> s{_esCustomerAWSId = a})

-- | The RDS event notification subscription Id.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId = lens _esCustSubscriptionId (\ s a -> s{_esCustSubscriptionId = a})

-- | The topic ARN of the RDS event notification subscription.
esSNSTopicARN :: Lens' EventSubscription (Maybe Text)
esSNSTopicARN = lens _esSNSTopicARN (\ s a -> s{_esSNSTopicARN = a})

-- | The Amazon Resource Name (ARN) for the event subscription.
esEventSubscriptionARN :: Lens' EventSubscription (Maybe Text)
esEventSubscriptionARN = lens _esEventSubscriptionARN (\ s a -> s{_esEventSubscriptionARN = a})

-- | A Boolean value indicating if the subscription is enabled. True indicates the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\ s a -> s{_esEnabled = a})

-- | The source type for the RDS event notification subscription.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\ s a -> s{_esSourceType = a})

-- | The time the RDS event notification subscription was created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe Text)
esSubscriptionCreationTime = lens _esSubscriptionCreationTime (\ s a -> s{_esSubscriptionCreationTime = a})

-- | A list of event categories for the RDS event notification subscription.
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList = lens _esEventCategoriesList (\ s a -> s{_esEventCategoriesList = a}) . _Default . _Coerce

-- | A list of source IDs for the RDS event notification subscription.
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\ s a -> s{_esSourceIdsList = a}) . _Default . _Coerce

instance FromXML EventSubscription where
        parseXML x
          = EventSubscription' <$>
              (x .@? "Status") <*> (x .@? "CustomerAwsId") <*>
                (x .@? "CustSubscriptionId")
                <*> (x .@? "SnsTopicArn")
                <*> (x .@? "EventSubscriptionArn")
                <*> (x .@? "Enabled")
                <*> (x .@? "SourceType")
                <*> (x .@? "SubscriptionCreationTime")
                <*>
                (x .@? "EventCategoriesList" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*>
                (x .@? "SourceIdsList" .!@ mempty >>=
                   may (parseXMLList "SourceId"))

instance Hashable EventSubscription where

instance NFData EventSubscription where

-- | A filter name and value pair that is used to return a more specific list of results from a describe operation. Filters can be used to match a set of resources by specific criteria, such as IDs. The filters supported by a describe operation are documented with the describe operation.
--
--
-- The following actions can be filtered:
--
--     * 'DescribeDBClusterBacktracks'
--
--     * 'DescribeDBClusters'
--
--     * 'DescribeDBInstances'
--
--     * 'DescribePendingMaintenanceActions'
--
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fName   :: !Text
  , _fValues :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName' - The name of the filter. Filter names are case-sensitive.
--
-- * 'fValues' - One or more filter values. Filter values are case-sensitive.
filter'
    :: Text -- ^ 'fName'
    -> Filter
filter' pName_ = Filter' {_fName = pName_, _fValues = mempty}


-- | The name of the filter. Filter names are case-sensitive.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a})

-- | One or more filter values. Filter values are case-sensitive.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce

instance Hashable Filter where

instance NFData Filter where

instance ToQuery Filter where
        toQuery Filter'{..}
          = mconcat
              ["Name" =: _fName,
               "Values" =: toQueryList "Value" _fValues]

-- | This data type is used as a response element in the 'DescribeDBSecurityGroups' action.
--
--
--
-- /See:/ 'ipRange' smart constructor.
data IPRange = IPRange'
  { _irStatus :: !(Maybe Text)
  , _irCIdRIP :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IPRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irStatus' - Specifies the status of the IP range. Status can be "authorizing", "authorized", "revoking", and "revoked".
--
-- * 'irCIdRIP' - Specifies the IP range.
ipRange
    :: IPRange
ipRange = IPRange' {_irStatus = Nothing, _irCIdRIP = Nothing}


-- | Specifies the status of the IP range. Status can be "authorizing", "authorized", "revoking", and "revoked".
irStatus :: Lens' IPRange (Maybe Text)
irStatus = lens _irStatus (\ s a -> s{_irStatus = a})

-- | Specifies the IP range.
irCIdRIP :: Lens' IPRange (Maybe Text)
irCIdRIP = lens _irCIdRIP (\ s a -> s{_irCIdRIP = a})

instance FromXML IPRange where
        parseXML x
          = IPRange' <$> (x .@? "Status") <*> (x .@? "CIDRIP")

instance Hashable IPRange where

instance NFData IPRange where

-- | Option details.
--
--
--
-- /See:/ 'option' smart constructor.
data Option = Option'
  { _oOptionName                  :: !(Maybe Text)
  , _oPermanent                   :: !(Maybe Bool)
  , _oPersistent                  :: !(Maybe Bool)
  , _oOptionDescription           :: !(Maybe Text)
  , _oOptionSettings              :: !(Maybe [OptionSetting])
  , _oVPCSecurityGroupMemberships :: !(Maybe [VPCSecurityGroupMembership])
  , _oDBSecurityGroupMemberships  :: !(Maybe [DBSecurityGroupMembership])
  , _oOptionVersion               :: !(Maybe Text)
  , _oPort                        :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Option' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oOptionName' - The name of the option.
--
-- * 'oPermanent' - Indicate if this option is permanent.
--
-- * 'oPersistent' - Indicate if this option is persistent.
--
-- * 'oOptionDescription' - The description of the option.
--
-- * 'oOptionSettings' - The option settings for this option.
--
-- * 'oVPCSecurityGroupMemberships' - If the option requires access to a port, then this VPC security group allows access to the port.
--
-- * 'oDBSecurityGroupMemberships' - If the option requires access to a port, then this DB security group allows access to the port.
--
-- * 'oOptionVersion' - The version of the option.
--
-- * 'oPort' - If required, the port configured for this option to use.
option
    :: Option
option =
  Option'
    { _oOptionName = Nothing
    , _oPermanent = Nothing
    , _oPersistent = Nothing
    , _oOptionDescription = Nothing
    , _oOptionSettings = Nothing
    , _oVPCSecurityGroupMemberships = Nothing
    , _oDBSecurityGroupMemberships = Nothing
    , _oOptionVersion = Nothing
    , _oPort = Nothing
    }


-- | The name of the option.
oOptionName :: Lens' Option (Maybe Text)
oOptionName = lens _oOptionName (\ s a -> s{_oOptionName = a})

-- | Indicate if this option is permanent.
oPermanent :: Lens' Option (Maybe Bool)
oPermanent = lens _oPermanent (\ s a -> s{_oPermanent = a})

-- | Indicate if this option is persistent.
oPersistent :: Lens' Option (Maybe Bool)
oPersistent = lens _oPersistent (\ s a -> s{_oPersistent = a})

-- | The description of the option.
oOptionDescription :: Lens' Option (Maybe Text)
oOptionDescription = lens _oOptionDescription (\ s a -> s{_oOptionDescription = a})

-- | The option settings for this option.
oOptionSettings :: Lens' Option [OptionSetting]
oOptionSettings = lens _oOptionSettings (\ s a -> s{_oOptionSettings = a}) . _Default . _Coerce

-- | If the option requires access to a port, then this VPC security group allows access to the port.
oVPCSecurityGroupMemberships :: Lens' Option [VPCSecurityGroupMembership]
oVPCSecurityGroupMemberships = lens _oVPCSecurityGroupMemberships (\ s a -> s{_oVPCSecurityGroupMemberships = a}) . _Default . _Coerce

-- | If the option requires access to a port, then this DB security group allows access to the port.
oDBSecurityGroupMemberships :: Lens' Option [DBSecurityGroupMembership]
oDBSecurityGroupMemberships = lens _oDBSecurityGroupMemberships (\ s a -> s{_oDBSecurityGroupMemberships = a}) . _Default . _Coerce

-- | The version of the option.
oOptionVersion :: Lens' Option (Maybe Text)
oOptionVersion = lens _oOptionVersion (\ s a -> s{_oOptionVersion = a})

-- | If required, the port configured for this option to use.
oPort :: Lens' Option (Maybe Int)
oPort = lens _oPort (\ s a -> s{_oPort = a})

instance FromXML Option where
        parseXML x
          = Option' <$>
              (x .@? "OptionName") <*> (x .@? "Permanent") <*>
                (x .@? "Persistent")
                <*> (x .@? "OptionDescription")
                <*>
                (x .@? "OptionSettings" .!@ mempty >>=
                   may (parseXMLList "OptionSetting"))
                <*>
                (x .@? "VpcSecurityGroupMemberships" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroupMembership"))
                <*>
                (x .@? "DBSecurityGroupMemberships" .!@ mempty >>=
                   may (parseXMLList "DBSecurityGroup"))
                <*> (x .@? "OptionVersion")
                <*> (x .@? "Port")

instance Hashable Option where

instance NFData Option where

-- | A list of all available options
--
--
--
-- /See:/ 'optionConfiguration' smart constructor.
data OptionConfiguration = OptionConfiguration'
  { _ocOptionSettings              :: !(Maybe [OptionSetting])
  , _ocVPCSecurityGroupMemberships :: !(Maybe [Text])
  , _ocDBSecurityGroupMemberships  :: !(Maybe [Text])
  , _ocOptionVersion               :: !(Maybe Text)
  , _ocPort                        :: !(Maybe Int)
  , _ocOptionName                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocOptionSettings' - The option settings to include in an option group.
--
-- * 'ocVPCSecurityGroupMemberships' - A list of VpcSecurityGroupMemebrship name strings used for this option.
--
-- * 'ocDBSecurityGroupMemberships' - A list of DBSecurityGroupMemebrship name strings used for this option.
--
-- * 'ocOptionVersion' - The version for the option.
--
-- * 'ocPort' - The optional port for the option.
--
-- * 'ocOptionName' - The configuration of options to include in a group.
optionConfiguration
    :: Text -- ^ 'ocOptionName'
    -> OptionConfiguration
optionConfiguration pOptionName_ =
  OptionConfiguration'
    { _ocOptionSettings = Nothing
    , _ocVPCSecurityGroupMemberships = Nothing
    , _ocDBSecurityGroupMemberships = Nothing
    , _ocOptionVersion = Nothing
    , _ocPort = Nothing
    , _ocOptionName = pOptionName_
    }


-- | The option settings to include in an option group.
ocOptionSettings :: Lens' OptionConfiguration [OptionSetting]
ocOptionSettings = lens _ocOptionSettings (\ s a -> s{_ocOptionSettings = a}) . _Default . _Coerce

-- | A list of VpcSecurityGroupMemebrship name strings used for this option.
ocVPCSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocVPCSecurityGroupMemberships = lens _ocVPCSecurityGroupMemberships (\ s a -> s{_ocVPCSecurityGroupMemberships = a}) . _Default . _Coerce

-- | A list of DBSecurityGroupMemebrship name strings used for this option.
ocDBSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocDBSecurityGroupMemberships = lens _ocDBSecurityGroupMemberships (\ s a -> s{_ocDBSecurityGroupMemberships = a}) . _Default . _Coerce

-- | The version for the option.
ocOptionVersion :: Lens' OptionConfiguration (Maybe Text)
ocOptionVersion = lens _ocOptionVersion (\ s a -> s{_ocOptionVersion = a})

-- | The optional port for the option.
ocPort :: Lens' OptionConfiguration (Maybe Int)
ocPort = lens _ocPort (\ s a -> s{_ocPort = a})

-- | The configuration of options to include in a group.
ocOptionName :: Lens' OptionConfiguration Text
ocOptionName = lens _ocOptionName (\ s a -> s{_ocOptionName = a})

instance Hashable OptionConfiguration where

instance NFData OptionConfiguration where

instance ToQuery OptionConfiguration where
        toQuery OptionConfiguration'{..}
          = mconcat
              ["OptionSettings" =:
                 toQuery
                   (toQueryList "OptionSetting" <$> _ocOptionSettings),
               "VpcSecurityGroupMemberships" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _ocVPCSecurityGroupMemberships),
               "DBSecurityGroupMemberships" =:
                 toQuery
                   (toQueryList "DBSecurityGroupName" <$>
                      _ocDBSecurityGroupMemberships),
               "OptionVersion" =: _ocOptionVersion,
               "Port" =: _ocPort, "OptionName" =: _ocOptionName]

-- |
--
--
--
-- /See:/ 'optionGroup' smart constructor.
data OptionGroup = OptionGroup'
  { _ogOptionGroupDescription                :: !(Maybe Text)
  , _ogVPCId                                 :: !(Maybe Text)
  , _ogAllowsVPCAndNonVPCInstanceMemberships :: !(Maybe Bool)
  , _ogEngineName                            :: !(Maybe Text)
  , _ogOptionGroupARN                        :: !(Maybe Text)
  , _ogMajorEngineVersion                    :: !(Maybe Text)
  , _ogOptions                               :: !(Maybe [Option])
  , _ogOptionGroupName                       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogOptionGroupDescription' - Provides a description of the option group.
--
-- * 'ogVPCId' - If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field.
--
-- * 'ogAllowsVPCAndNonVPCInstanceMemberships' - Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances.
--
-- * 'ogEngineName' - Indicates the name of the engine that this option group can be applied to.
--
-- * 'ogOptionGroupARN' - The Amazon Resource Name (ARN) for the option group.
--
-- * 'ogMajorEngineVersion' - Indicates the major engine version associated with this option group.
--
-- * 'ogOptions' - Indicates what options are available in the option group.
--
-- * 'ogOptionGroupName' - Specifies the name of the option group.
optionGroup
    :: OptionGroup
optionGroup =
  OptionGroup'
    { _ogOptionGroupDescription = Nothing
    , _ogVPCId = Nothing
    , _ogAllowsVPCAndNonVPCInstanceMemberships = Nothing
    , _ogEngineName = Nothing
    , _ogOptionGroupARN = Nothing
    , _ogMajorEngineVersion = Nothing
    , _ogOptions = Nothing
    , _ogOptionGroupName = Nothing
    }


-- | Provides a description of the option group.
ogOptionGroupDescription :: Lens' OptionGroup (Maybe Text)
ogOptionGroupDescription = lens _ogOptionGroupDescription (\ s a -> s{_ogOptionGroupDescription = a})

-- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field.
ogVPCId :: Lens' OptionGroup (Maybe Text)
ogVPCId = lens _ogVPCId (\ s a -> s{_ogVPCId = a})

-- | Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances.
ogAllowsVPCAndNonVPCInstanceMemberships :: Lens' OptionGroup (Maybe Bool)
ogAllowsVPCAndNonVPCInstanceMemberships = lens _ogAllowsVPCAndNonVPCInstanceMemberships (\ s a -> s{_ogAllowsVPCAndNonVPCInstanceMemberships = a})

-- | Indicates the name of the engine that this option group can be applied to.
ogEngineName :: Lens' OptionGroup (Maybe Text)
ogEngineName = lens _ogEngineName (\ s a -> s{_ogEngineName = a})

-- | The Amazon Resource Name (ARN) for the option group.
ogOptionGroupARN :: Lens' OptionGroup (Maybe Text)
ogOptionGroupARN = lens _ogOptionGroupARN (\ s a -> s{_ogOptionGroupARN = a})

-- | Indicates the major engine version associated with this option group.
ogMajorEngineVersion :: Lens' OptionGroup (Maybe Text)
ogMajorEngineVersion = lens _ogMajorEngineVersion (\ s a -> s{_ogMajorEngineVersion = a})

-- | Indicates what options are available in the option group.
ogOptions :: Lens' OptionGroup [Option]
ogOptions = lens _ogOptions (\ s a -> s{_ogOptions = a}) . _Default . _Coerce

-- | Specifies the name of the option group.
ogOptionGroupName :: Lens' OptionGroup (Maybe Text)
ogOptionGroupName = lens _ogOptionGroupName (\ s a -> s{_ogOptionGroupName = a})

instance FromXML OptionGroup where
        parseXML x
          = OptionGroup' <$>
              (x .@? "OptionGroupDescription") <*> (x .@? "VpcId")
                <*> (x .@? "AllowsVpcAndNonVpcInstanceMemberships")
                <*> (x .@? "EngineName")
                <*> (x .@? "OptionGroupArn")
                <*> (x .@? "MajorEngineVersion")
                <*>
                (x .@? "Options" .!@ mempty >>=
                   may (parseXMLList "Option"))
                <*> (x .@? "OptionGroupName")

instance Hashable OptionGroup where

instance NFData OptionGroup where

-- | Provides information on the option groups the DB instance is a member of.
--
--
--
-- /See:/ 'optionGroupMembership' smart constructor.
data OptionGroupMembership = OptionGroupMembership'
  { _ogmStatus          :: !(Maybe Text)
  , _ogmOptionGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogmStatus' - The status of the DB instance's option group membership. Valid values are: @in-sync@ , @pending-apply@ , @pending-removal@ , @pending-maintenance-apply@ , @pending-maintenance-removal@ , @applying@ , @removing@ , and @failed@ .
--
-- * 'ogmOptionGroupName' - The name of the option group that the instance belongs to.
optionGroupMembership
    :: OptionGroupMembership
optionGroupMembership =
  OptionGroupMembership' {_ogmStatus = Nothing, _ogmOptionGroupName = Nothing}


-- | The status of the DB instance's option group membership. Valid values are: @in-sync@ , @pending-apply@ , @pending-removal@ , @pending-maintenance-apply@ , @pending-maintenance-removal@ , @applying@ , @removing@ , and @failed@ .
ogmStatus :: Lens' OptionGroupMembership (Maybe Text)
ogmStatus = lens _ogmStatus (\ s a -> s{_ogmStatus = a})

-- | The name of the option group that the instance belongs to.
ogmOptionGroupName :: Lens' OptionGroupMembership (Maybe Text)
ogmOptionGroupName = lens _ogmOptionGroupName (\ s a -> s{_ogmOptionGroupName = a})

instance FromXML OptionGroupMembership where
        parseXML x
          = OptionGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "OptionGroupName")

instance Hashable OptionGroupMembership where

instance NFData OptionGroupMembership where

-- | Available option.
--
--
--
-- /See:/ 'optionGroupOption' smart constructor.
data OptionGroupOption = OptionGroupOption'
  { _ogoMinimumRequiredMinorEngineVersion :: !(Maybe Text)
  , _ogoOptionsConflictsWith :: !(Maybe [Text])
  , _ogoPermanent :: !(Maybe Bool)
  , _ogoPersistent :: !(Maybe Bool)
  , _ogoOptionGroupOptionVersions :: !(Maybe [OptionVersion])
  , _ogoEngineName :: !(Maybe Text)
  , _ogoMajorEngineVersion :: !(Maybe Text)
  , _ogoName :: !(Maybe Text)
  , _ogoSupportsOptionVersionDowngrade :: !(Maybe Bool)
  , _ogoDefaultPort :: !(Maybe Int)
  , _ogoOptionGroupOptionSettings :: !(Maybe [OptionGroupOptionSetting])
  , _ogoRequiresAutoMinorEngineVersionUpgrade :: !(Maybe Bool)
  , _ogoPortRequired :: !(Maybe Bool)
  , _ogoDescription :: !(Maybe Text)
  , _ogoOptionsDependedOn :: !(Maybe [Text])
  , _ogoVPCOnly :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionGroupOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogoMinimumRequiredMinorEngineVersion' - The minimum required engine version for the option to be applied.
--
-- * 'ogoOptionsConflictsWith' - The options that conflict with this option.
--
-- * 'ogoPermanent' - Permanent options can never be removed from an option group. An option group containing a permanent option can't be removed from a DB instance.
--
-- * 'ogoPersistent' - Persistent options can't be removed from an option group while DB instances are associated with the option group. If you disassociate all DB instances from the option group, your can remove the persistent option from the option group.
--
-- * 'ogoOptionGroupOptionVersions' - The versions that are available for the option.
--
-- * 'ogoEngineName' - The name of the engine that this option can be applied to.
--
-- * 'ogoMajorEngineVersion' - Indicates the major engine version that the option is available for.
--
-- * 'ogoName' - The name of the option.
--
-- * 'ogoSupportsOptionVersionDowngrade' - If true, you can change the option to an earlier version of the option. This only applies to options that have different versions available.
--
-- * 'ogoDefaultPort' - If the option requires a port, specifies the default port for the option.
--
-- * 'ogoOptionGroupOptionSettings' - The option settings that are available (and the default value) for each option in an option group.
--
-- * 'ogoRequiresAutoMinorEngineVersionUpgrade' - If true, you must enable the Auto Minor Version Upgrade setting for your DB instance before you can use this option. You can enable Auto Minor Version Upgrade when you first create your DB instance, or by modifying your DB instance later.
--
-- * 'ogoPortRequired' - Specifies whether the option requires a port.
--
-- * 'ogoDescription' - The description of the option.
--
-- * 'ogoOptionsDependedOn' - The options that are prerequisites for this option.
--
-- * 'ogoVPCOnly' - If true, you can only use this option with a DB instance that is in a VPC.
optionGroupOption
    :: OptionGroupOption
optionGroupOption =
  OptionGroupOption'
    { _ogoMinimumRequiredMinorEngineVersion = Nothing
    , _ogoOptionsConflictsWith = Nothing
    , _ogoPermanent = Nothing
    , _ogoPersistent = Nothing
    , _ogoOptionGroupOptionVersions = Nothing
    , _ogoEngineName = Nothing
    , _ogoMajorEngineVersion = Nothing
    , _ogoName = Nothing
    , _ogoSupportsOptionVersionDowngrade = Nothing
    , _ogoDefaultPort = Nothing
    , _ogoOptionGroupOptionSettings = Nothing
    , _ogoRequiresAutoMinorEngineVersionUpgrade = Nothing
    , _ogoPortRequired = Nothing
    , _ogoDescription = Nothing
    , _ogoOptionsDependedOn = Nothing
    , _ogoVPCOnly = Nothing
    }


-- | The minimum required engine version for the option to be applied.
ogoMinimumRequiredMinorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMinimumRequiredMinorEngineVersion = lens _ogoMinimumRequiredMinorEngineVersion (\ s a -> s{_ogoMinimumRequiredMinorEngineVersion = a})

-- | The options that conflict with this option.
ogoOptionsConflictsWith :: Lens' OptionGroupOption [Text]
ogoOptionsConflictsWith = lens _ogoOptionsConflictsWith (\ s a -> s{_ogoOptionsConflictsWith = a}) . _Default . _Coerce

-- | Permanent options can never be removed from an option group. An option group containing a permanent option can't be removed from a DB instance.
ogoPermanent :: Lens' OptionGroupOption (Maybe Bool)
ogoPermanent = lens _ogoPermanent (\ s a -> s{_ogoPermanent = a})

-- | Persistent options can't be removed from an option group while DB instances are associated with the option group. If you disassociate all DB instances from the option group, your can remove the persistent option from the option group.
ogoPersistent :: Lens' OptionGroupOption (Maybe Bool)
ogoPersistent = lens _ogoPersistent (\ s a -> s{_ogoPersistent = a})

-- | The versions that are available for the option.
ogoOptionGroupOptionVersions :: Lens' OptionGroupOption [OptionVersion]
ogoOptionGroupOptionVersions = lens _ogoOptionGroupOptionVersions (\ s a -> s{_ogoOptionGroupOptionVersions = a}) . _Default . _Coerce

-- | The name of the engine that this option can be applied to.
ogoEngineName :: Lens' OptionGroupOption (Maybe Text)
ogoEngineName = lens _ogoEngineName (\ s a -> s{_ogoEngineName = a})

-- | Indicates the major engine version that the option is available for.
ogoMajorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMajorEngineVersion = lens _ogoMajorEngineVersion (\ s a -> s{_ogoMajorEngineVersion = a})

-- | The name of the option.
ogoName :: Lens' OptionGroupOption (Maybe Text)
ogoName = lens _ogoName (\ s a -> s{_ogoName = a})

-- | If true, you can change the option to an earlier version of the option. This only applies to options that have different versions available.
ogoSupportsOptionVersionDowngrade :: Lens' OptionGroupOption (Maybe Bool)
ogoSupportsOptionVersionDowngrade = lens _ogoSupportsOptionVersionDowngrade (\ s a -> s{_ogoSupportsOptionVersionDowngrade = a})

-- | If the option requires a port, specifies the default port for the option.
ogoDefaultPort :: Lens' OptionGroupOption (Maybe Int)
ogoDefaultPort = lens _ogoDefaultPort (\ s a -> s{_ogoDefaultPort = a})

-- | The option settings that are available (and the default value) for each option in an option group.
ogoOptionGroupOptionSettings :: Lens' OptionGroupOption [OptionGroupOptionSetting]
ogoOptionGroupOptionSettings = lens _ogoOptionGroupOptionSettings (\ s a -> s{_ogoOptionGroupOptionSettings = a}) . _Default . _Coerce

-- | If true, you must enable the Auto Minor Version Upgrade setting for your DB instance before you can use this option. You can enable Auto Minor Version Upgrade when you first create your DB instance, or by modifying your DB instance later.
ogoRequiresAutoMinorEngineVersionUpgrade :: Lens' OptionGroupOption (Maybe Bool)
ogoRequiresAutoMinorEngineVersionUpgrade = lens _ogoRequiresAutoMinorEngineVersionUpgrade (\ s a -> s{_ogoRequiresAutoMinorEngineVersionUpgrade = a})

-- | Specifies whether the option requires a port.
ogoPortRequired :: Lens' OptionGroupOption (Maybe Bool)
ogoPortRequired = lens _ogoPortRequired (\ s a -> s{_ogoPortRequired = a})

-- | The description of the option.
ogoDescription :: Lens' OptionGroupOption (Maybe Text)
ogoDescription = lens _ogoDescription (\ s a -> s{_ogoDescription = a})

-- | The options that are prerequisites for this option.
ogoOptionsDependedOn :: Lens' OptionGroupOption [Text]
ogoOptionsDependedOn = lens _ogoOptionsDependedOn (\ s a -> s{_ogoOptionsDependedOn = a}) . _Default . _Coerce

-- | If true, you can only use this option with a DB instance that is in a VPC.
ogoVPCOnly :: Lens' OptionGroupOption (Maybe Bool)
ogoVPCOnly = lens _ogoVPCOnly (\ s a -> s{_ogoVPCOnly = a})

instance FromXML OptionGroupOption where
        parseXML x
          = OptionGroupOption' <$>
              (x .@? "MinimumRequiredMinorEngineVersion") <*>
                (x .@? "OptionsConflictsWith" .!@ mempty >>=
                   may (parseXMLList "OptionConflictName"))
                <*> (x .@? "Permanent")
                <*> (x .@? "Persistent")
                <*>
                (x .@? "OptionGroupOptionVersions" .!@ mempty >>=
                   may (parseXMLList "OptionVersion"))
                <*> (x .@? "EngineName")
                <*> (x .@? "MajorEngineVersion")
                <*> (x .@? "Name")
                <*> (x .@? "SupportsOptionVersionDowngrade")
                <*> (x .@? "DefaultPort")
                <*>
                (x .@? "OptionGroupOptionSettings" .!@ mempty >>=
                   may (parseXMLList "OptionGroupOptionSetting"))
                <*> (x .@? "RequiresAutoMinorEngineVersionUpgrade")
                <*> (x .@? "PortRequired")
                <*> (x .@? "Description")
                <*>
                (x .@? "OptionsDependedOn" .!@ mempty >>=
                   may (parseXMLList "OptionName"))
                <*> (x .@? "VpcOnly")

instance Hashable OptionGroupOption where

instance NFData OptionGroupOption where

-- | Option group option settings are used to display settings available for each option with their default values and other information. These values are used with the DescribeOptionGroupOptions action.
--
--
--
-- /See:/ 'optionGroupOptionSetting' smart constructor.
data OptionGroupOptionSetting = OptionGroupOptionSetting'
  { _ogosApplyType          :: !(Maybe Text)
  , _ogosSettingName        :: !(Maybe Text)
  , _ogosDefaultValue       :: !(Maybe Text)
  , _ogosIsModifiable       :: !(Maybe Bool)
  , _ogosSettingDescription :: !(Maybe Text)
  , _ogosAllowedValues      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionGroupOptionSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogosApplyType' - The DB engine specific parameter type for the option group option.
--
-- * 'ogosSettingName' - The name of the option group option.
--
-- * 'ogosDefaultValue' - The default value for the option group option.
--
-- * 'ogosIsModifiable' - Boolean value where true indicates that this option group option can be changed from the default value.
--
-- * 'ogosSettingDescription' - The description of the option group option.
--
-- * 'ogosAllowedValues' - Indicates the acceptable values for the option group option.
optionGroupOptionSetting
    :: OptionGroupOptionSetting
optionGroupOptionSetting =
  OptionGroupOptionSetting'
    { _ogosApplyType = Nothing
    , _ogosSettingName = Nothing
    , _ogosDefaultValue = Nothing
    , _ogosIsModifiable = Nothing
    , _ogosSettingDescription = Nothing
    , _ogosAllowedValues = Nothing
    }


-- | The DB engine specific parameter type for the option group option.
ogosApplyType :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosApplyType = lens _ogosApplyType (\ s a -> s{_ogosApplyType = a})

-- | The name of the option group option.
ogosSettingName :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingName = lens _ogosSettingName (\ s a -> s{_ogosSettingName = a})

-- | The default value for the option group option.
ogosDefaultValue :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosDefaultValue = lens _ogosDefaultValue (\ s a -> s{_ogosDefaultValue = a})

-- | Boolean value where true indicates that this option group option can be changed from the default value.
ogosIsModifiable :: Lens' OptionGroupOptionSetting (Maybe Bool)
ogosIsModifiable = lens _ogosIsModifiable (\ s a -> s{_ogosIsModifiable = a})

-- | The description of the option group option.
ogosSettingDescription :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingDescription = lens _ogosSettingDescription (\ s a -> s{_ogosSettingDescription = a})

-- | Indicates the acceptable values for the option group option.
ogosAllowedValues :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosAllowedValues = lens _ogosAllowedValues (\ s a -> s{_ogosAllowedValues = a})

instance FromXML OptionGroupOptionSetting where
        parseXML x
          = OptionGroupOptionSetting' <$>
              (x .@? "ApplyType") <*> (x .@? "SettingName") <*>
                (x .@? "DefaultValue")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "SettingDescription")
                <*> (x .@? "AllowedValues")

instance Hashable OptionGroupOptionSetting where

instance NFData OptionGroupOptionSetting where

-- | Option settings are the actual settings being applied or configured for that option. It is used when you modify an option group or describe option groups. For example, the NATIVE_NETWORK_ENCRYPTION option has a setting called SQLNET.ENCRYPTION_SERVER that can have several different values.
--
--
--
-- /See:/ 'optionSetting' smart constructor.
data OptionSetting = OptionSetting'
  { _osIsCollection  :: !(Maybe Bool)
  , _osApplyType     :: !(Maybe Text)
  , _osValue         :: !(Maybe Text)
  , _osName          :: !(Maybe Text)
  , _osDefaultValue  :: !(Maybe Text)
  , _osIsModifiable  :: !(Maybe Bool)
  , _osDataType      :: !(Maybe Text)
  , _osAllowedValues :: !(Maybe Text)
  , _osDescription   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osIsCollection' - Indicates if the option setting is part of a collection.
--
-- * 'osApplyType' - The DB engine specific parameter type.
--
-- * 'osValue' - The current value of the option setting.
--
-- * 'osName' - The name of the option that has settings that you can set.
--
-- * 'osDefaultValue' - The default value of the option setting.
--
-- * 'osIsModifiable' - A Boolean value that, when true, indicates the option setting can be modified from the default.
--
-- * 'osDataType' - The data type of the option setting.
--
-- * 'osAllowedValues' - The allowed values of the option setting.
--
-- * 'osDescription' - The description of the option setting.
optionSetting
    :: OptionSetting
optionSetting =
  OptionSetting'
    { _osIsCollection = Nothing
    , _osApplyType = Nothing
    , _osValue = Nothing
    , _osName = Nothing
    , _osDefaultValue = Nothing
    , _osIsModifiable = Nothing
    , _osDataType = Nothing
    , _osAllowedValues = Nothing
    , _osDescription = Nothing
    }


-- | Indicates if the option setting is part of a collection.
osIsCollection :: Lens' OptionSetting (Maybe Bool)
osIsCollection = lens _osIsCollection (\ s a -> s{_osIsCollection = a})

-- | The DB engine specific parameter type.
osApplyType :: Lens' OptionSetting (Maybe Text)
osApplyType = lens _osApplyType (\ s a -> s{_osApplyType = a})

-- | The current value of the option setting.
osValue :: Lens' OptionSetting (Maybe Text)
osValue = lens _osValue (\ s a -> s{_osValue = a})

-- | The name of the option that has settings that you can set.
osName :: Lens' OptionSetting (Maybe Text)
osName = lens _osName (\ s a -> s{_osName = a})

-- | The default value of the option setting.
osDefaultValue :: Lens' OptionSetting (Maybe Text)
osDefaultValue = lens _osDefaultValue (\ s a -> s{_osDefaultValue = a})

-- | A Boolean value that, when true, indicates the option setting can be modified from the default.
osIsModifiable :: Lens' OptionSetting (Maybe Bool)
osIsModifiable = lens _osIsModifiable (\ s a -> s{_osIsModifiable = a})

-- | The data type of the option setting.
osDataType :: Lens' OptionSetting (Maybe Text)
osDataType = lens _osDataType (\ s a -> s{_osDataType = a})

-- | The allowed values of the option setting.
osAllowedValues :: Lens' OptionSetting (Maybe Text)
osAllowedValues = lens _osAllowedValues (\ s a -> s{_osAllowedValues = a})

-- | The description of the option setting.
osDescription :: Lens' OptionSetting (Maybe Text)
osDescription = lens _osDescription (\ s a -> s{_osDescription = a})

instance FromXML OptionSetting where
        parseXML x
          = OptionSetting' <$>
              (x .@? "IsCollection") <*> (x .@? "ApplyType") <*>
                (x .@? "Value")
                <*> (x .@? "Name")
                <*> (x .@? "DefaultValue")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "DataType")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "Description")

instance Hashable OptionSetting where

instance NFData OptionSetting where

instance ToQuery OptionSetting where
        toQuery OptionSetting'{..}
          = mconcat
              ["IsCollection" =: _osIsCollection,
               "ApplyType" =: _osApplyType, "Value" =: _osValue,
               "Name" =: _osName, "DefaultValue" =: _osDefaultValue,
               "IsModifiable" =: _osIsModifiable,
               "DataType" =: _osDataType,
               "AllowedValues" =: _osAllowedValues,
               "Description" =: _osDescription]

-- | The version for an option. Option group option versions are returned by the 'DescribeOptionGroupOptions' action.
--
--
--
-- /See:/ 'optionVersion' smart constructor.
data OptionVersion = OptionVersion'
  { _ovVersion   :: !(Maybe Text)
  , _ovIsDefault :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ovVersion' - The version of the option.
--
-- * 'ovIsDefault' - True if the version is the default version of the option, and otherwise false.
optionVersion
    :: OptionVersion
optionVersion = OptionVersion' {_ovVersion = Nothing, _ovIsDefault = Nothing}


-- | The version of the option.
ovVersion :: Lens' OptionVersion (Maybe Text)
ovVersion = lens _ovVersion (\ s a -> s{_ovVersion = a})

-- | True if the version is the default version of the option, and otherwise false.
ovIsDefault :: Lens' OptionVersion (Maybe Bool)
ovIsDefault = lens _ovIsDefault (\ s a -> s{_ovIsDefault = a})

instance FromXML OptionVersion where
        parseXML x
          = OptionVersion' <$>
              (x .@? "Version") <*> (x .@? "IsDefault")

instance Hashable OptionVersion where

instance NFData OptionVersion where

-- | Contains a list of available options for a DB instance.
--
--
-- This data type is used as a response element in the 'DescribeOrderableDBInstanceOptions' action.
--
--
-- /See:/ 'orderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
  { _odioEngineVersion                     :: !(Maybe Text)
  , _odioMinIOPSPerGib                     :: !(Maybe Double)
  , _odioSupportsIAMDatabaseAuthentication :: !(Maybe Bool)
  , _odioMinIOPSPerDBInstance              :: !(Maybe Int)
  , _odioMultiAZCapable                    :: !(Maybe Bool)
  , _odioMaxStorageSize                    :: !(Maybe Int)
  , _odioEngine                            :: !(Maybe Text)
  , _odioMinStorageSize                    :: !(Maybe Int)
  , _odioSupportsIOPS                      :: !(Maybe Bool)
  , _odioSupportsPerformanceInsights       :: !(Maybe Bool)
  , _odioDBInstanceClass                   :: !(Maybe Text)
  , _odioLicenseModel                      :: !(Maybe Text)
  , _odioAvailabilityZones                 :: !(Maybe [AvailabilityZone])
  , _odioSupportsStorageEncryption         :: !(Maybe Bool)
  , _odioReadReplicaCapable                :: !(Maybe Bool)
  , _odioMaxIOPSPerGib                     :: !(Maybe Double)
  , _odioVPC                               :: !(Maybe Bool)
  , _odioSupportsEnhancedMonitoring        :: !(Maybe Bool)
  , _odioMaxIOPSPerDBInstance              :: !(Maybe Int)
  , _odioStorageType                       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OrderableDBInstanceOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odioEngineVersion' - The engine version of a DB instance.
--
-- * 'odioMinIOPSPerGib' - Minimum provisioned IOPS per GiB for a DB instance.
--
-- * 'odioSupportsIAMDatabaseAuthentication' - Indicates whether a DB instance supports IAM database authentication.
--
-- * 'odioMinIOPSPerDBInstance' - Minimum total provisioned IOPS for a DB instance.
--
-- * 'odioMultiAZCapable' - Indicates whether a DB instance is Multi-AZ capable.
--
-- * 'odioMaxStorageSize' - Maximum storage size for a DB instance.
--
-- * 'odioEngine' - The engine type of a DB instance.
--
-- * 'odioMinStorageSize' - Minimum storage size for a DB instance.
--
-- * 'odioSupportsIOPS' - Indicates whether a DB instance supports provisioned IOPS.
--
-- * 'odioSupportsPerformanceInsights' - True if a DB instance supports Performance Insights, otherwise false.
--
-- * 'odioDBInstanceClass' - The DB instance class for a DB instance.
--
-- * 'odioLicenseModel' - The license model for a DB instance.
--
-- * 'odioAvailabilityZones' - A list of Availability Zones for a DB instance.
--
-- * 'odioSupportsStorageEncryption' - Indicates whether a DB instance supports encrypted storage.
--
-- * 'odioReadReplicaCapable' - Indicates whether a DB instance can have a Read Replica.
--
-- * 'odioMaxIOPSPerGib' - Maximum provisioned IOPS per GiB for a DB instance.
--
-- * 'odioVPC' - Indicates whether a DB instance is in a VPC.
--
-- * 'odioSupportsEnhancedMonitoring' - Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
--
-- * 'odioMaxIOPSPerDBInstance' - Maximum total provisioned IOPS for a DB instance.
--
-- * 'odioStorageType' - Indicates the storage type for a DB instance.
orderableDBInstanceOption
    :: OrderableDBInstanceOption
orderableDBInstanceOption =
  OrderableDBInstanceOption'
    { _odioEngineVersion = Nothing
    , _odioMinIOPSPerGib = Nothing
    , _odioSupportsIAMDatabaseAuthentication = Nothing
    , _odioMinIOPSPerDBInstance = Nothing
    , _odioMultiAZCapable = Nothing
    , _odioMaxStorageSize = Nothing
    , _odioEngine = Nothing
    , _odioMinStorageSize = Nothing
    , _odioSupportsIOPS = Nothing
    , _odioSupportsPerformanceInsights = Nothing
    , _odioDBInstanceClass = Nothing
    , _odioLicenseModel = Nothing
    , _odioAvailabilityZones = Nothing
    , _odioSupportsStorageEncryption = Nothing
    , _odioReadReplicaCapable = Nothing
    , _odioMaxIOPSPerGib = Nothing
    , _odioVPC = Nothing
    , _odioSupportsEnhancedMonitoring = Nothing
    , _odioMaxIOPSPerDBInstance = Nothing
    , _odioStorageType = Nothing
    }


-- | The engine version of a DB instance.
odioEngineVersion :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngineVersion = lens _odioEngineVersion (\ s a -> s{_odioEngineVersion = a})

-- | Minimum provisioned IOPS per GiB for a DB instance.
odioMinIOPSPerGib :: Lens' OrderableDBInstanceOption (Maybe Double)
odioMinIOPSPerGib = lens _odioMinIOPSPerGib (\ s a -> s{_odioMinIOPSPerGib = a})

-- | Indicates whether a DB instance supports IAM database authentication.
odioSupportsIAMDatabaseAuthentication :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsIAMDatabaseAuthentication = lens _odioSupportsIAMDatabaseAuthentication (\ s a -> s{_odioSupportsIAMDatabaseAuthentication = a})

-- | Minimum total provisioned IOPS for a DB instance.
odioMinIOPSPerDBInstance :: Lens' OrderableDBInstanceOption (Maybe Int)
odioMinIOPSPerDBInstance = lens _odioMinIOPSPerDBInstance (\ s a -> s{_odioMinIOPSPerDBInstance = a})

-- | Indicates whether a DB instance is Multi-AZ capable.
odioMultiAZCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioMultiAZCapable = lens _odioMultiAZCapable (\ s a -> s{_odioMultiAZCapable = a})

-- | Maximum storage size for a DB instance.
odioMaxStorageSize :: Lens' OrderableDBInstanceOption (Maybe Int)
odioMaxStorageSize = lens _odioMaxStorageSize (\ s a -> s{_odioMaxStorageSize = a})

-- | The engine type of a DB instance.
odioEngine :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngine = lens _odioEngine (\ s a -> s{_odioEngine = a})

-- | Minimum storage size for a DB instance.
odioMinStorageSize :: Lens' OrderableDBInstanceOption (Maybe Int)
odioMinStorageSize = lens _odioMinStorageSize (\ s a -> s{_odioMinStorageSize = a})

-- | Indicates whether a DB instance supports provisioned IOPS.
odioSupportsIOPS :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsIOPS = lens _odioSupportsIOPS (\ s a -> s{_odioSupportsIOPS = a})

-- | True if a DB instance supports Performance Insights, otherwise false.
odioSupportsPerformanceInsights :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsPerformanceInsights = lens _odioSupportsPerformanceInsights (\ s a -> s{_odioSupportsPerformanceInsights = a})

-- | The DB instance class for a DB instance.
odioDBInstanceClass :: Lens' OrderableDBInstanceOption (Maybe Text)
odioDBInstanceClass = lens _odioDBInstanceClass (\ s a -> s{_odioDBInstanceClass = a})

-- | The license model for a DB instance.
odioLicenseModel :: Lens' OrderableDBInstanceOption (Maybe Text)
odioLicenseModel = lens _odioLicenseModel (\ s a -> s{_odioLicenseModel = a})

-- | A list of Availability Zones for a DB instance.
odioAvailabilityZones :: Lens' OrderableDBInstanceOption [AvailabilityZone]
odioAvailabilityZones = lens _odioAvailabilityZones (\ s a -> s{_odioAvailabilityZones = a}) . _Default . _Coerce

-- | Indicates whether a DB instance supports encrypted storage.
odioSupportsStorageEncryption :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsStorageEncryption = lens _odioSupportsStorageEncryption (\ s a -> s{_odioSupportsStorageEncryption = a})

-- | Indicates whether a DB instance can have a Read Replica.
odioReadReplicaCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioReadReplicaCapable = lens _odioReadReplicaCapable (\ s a -> s{_odioReadReplicaCapable = a})

-- | Maximum provisioned IOPS per GiB for a DB instance.
odioMaxIOPSPerGib :: Lens' OrderableDBInstanceOption (Maybe Double)
odioMaxIOPSPerGib = lens _odioMaxIOPSPerGib (\ s a -> s{_odioMaxIOPSPerGib = a})

-- | Indicates whether a DB instance is in a VPC.
odioVPC :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioVPC = lens _odioVPC (\ s a -> s{_odioVPC = a})

-- | Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
odioSupportsEnhancedMonitoring :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsEnhancedMonitoring = lens _odioSupportsEnhancedMonitoring (\ s a -> s{_odioSupportsEnhancedMonitoring = a})

-- | Maximum total provisioned IOPS for a DB instance.
odioMaxIOPSPerDBInstance :: Lens' OrderableDBInstanceOption (Maybe Int)
odioMaxIOPSPerDBInstance = lens _odioMaxIOPSPerDBInstance (\ s a -> s{_odioMaxIOPSPerDBInstance = a})

-- | Indicates the storage type for a DB instance.
odioStorageType :: Lens' OrderableDBInstanceOption (Maybe Text)
odioStorageType = lens _odioStorageType (\ s a -> s{_odioStorageType = a})

instance FromXML OrderableDBInstanceOption where
        parseXML x
          = OrderableDBInstanceOption' <$>
              (x .@? "EngineVersion") <*> (x .@? "MinIopsPerGib")
                <*> (x .@? "SupportsIAMDatabaseAuthentication")
                <*> (x .@? "MinIopsPerDbInstance")
                <*> (x .@? "MultiAZCapable")
                <*> (x .@? "MaxStorageSize")
                <*> (x .@? "Engine")
                <*> (x .@? "MinStorageSize")
                <*> (x .@? "SupportsIops")
                <*> (x .@? "SupportsPerformanceInsights")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "LicenseModel")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "SupportsStorageEncryption")
                <*> (x .@? "ReadReplicaCapable")
                <*> (x .@? "MaxIopsPerGib")
                <*> (x .@? "Vpc")
                <*> (x .@? "SupportsEnhancedMonitoring")
                <*> (x .@? "MaxIopsPerDbInstance")
                <*> (x .@? "StorageType")

instance Hashable OrderableDBInstanceOption where

instance NFData OrderableDBInstanceOption where

-- | This data type is used as a request parameter in the 'ModifyDBParameterGroup' and 'ResetDBParameterGroup' actions.
--
--
-- This data type is used as a response element in the 'DescribeEngineDefaultParameters' and 'DescribeDBParameters' actions.
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pApplyType            :: !(Maybe Text)
  , _pParameterValue       :: !(Maybe Text)
  , _pApplyMethod          :: !(Maybe ApplyMethod)
  , _pMinimumEngineVersion :: !(Maybe Text)
  , _pSource               :: !(Maybe Text)
  , _pIsModifiable         :: !(Maybe Bool)
  , _pDataType             :: !(Maybe Text)
  , _pAllowedValues        :: !(Maybe Text)
  , _pParameterName        :: !(Maybe Text)
  , _pDescription          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pApplyType' - Specifies the engine specific parameters type.
--
-- * 'pParameterValue' - Specifies the value of the parameter.
--
-- * 'pApplyMethod' - Indicates when to apply parameter updates.
--
-- * 'pMinimumEngineVersion' - The earliest engine version to which the parameter can apply.
--
-- * 'pSource' - Indicates the source of the parameter value.
--
-- * 'pIsModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- * 'pDataType' - Specifies the valid data type for the parameter.
--
-- * 'pAllowedValues' - Specifies the valid range of values for the parameter.
--
-- * 'pParameterName' - Specifies the name of the parameter.
--
-- * 'pDescription' - Provides a description of the parameter.
parameter
    :: Parameter
parameter =
  Parameter'
    { _pApplyType = Nothing
    , _pParameterValue = Nothing
    , _pApplyMethod = Nothing
    , _pMinimumEngineVersion = Nothing
    , _pSource = Nothing
    , _pIsModifiable = Nothing
    , _pDataType = Nothing
    , _pAllowedValues = Nothing
    , _pParameterName = Nothing
    , _pDescription = Nothing
    }


-- | Specifies the engine specific parameters type.
pApplyType :: Lens' Parameter (Maybe Text)
pApplyType = lens _pApplyType (\ s a -> s{_pApplyType = a})

-- | Specifies the value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\ s a -> s{_pParameterValue = a})

-- | Indicates when to apply parameter updates.
pApplyMethod :: Lens' Parameter (Maybe ApplyMethod)
pApplyMethod = lens _pApplyMethod (\ s a -> s{_pApplyMethod = a})

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion = lens _pMinimumEngineVersion (\ s a -> s{_pMinimumEngineVersion = a})

-- | Indicates the source of the parameter value.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\ s a -> s{_pSource = a})

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\ s a -> s{_pIsModifiable = a})

-- | Specifies the valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\ s a -> s{_pDataType = a})

-- | Specifies the valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\ s a -> s{_pAllowedValues = a})

-- | Specifies the name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\ s a -> s{_pParameterName = a})

-- | Provides a description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ApplyType") <*> (x .@? "ParameterValue") <*>
                (x .@? "ApplyMethod")
                <*> (x .@? "MinimumEngineVersion")
                <*> (x .@? "Source")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "DataType")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "ParameterName")
                <*> (x .@? "Description")

instance Hashable Parameter where

instance NFData Parameter where

instance ToQuery Parameter where
        toQuery Parameter'{..}
          = mconcat
              ["ApplyType" =: _pApplyType,
               "ParameterValue" =: _pParameterValue,
               "ApplyMethod" =: _pApplyMethod,
               "MinimumEngineVersion" =: _pMinimumEngineVersion,
               "Source" =: _pSource,
               "IsModifiable" =: _pIsModifiable,
               "DataType" =: _pDataType,
               "AllowedValues" =: _pAllowedValues,
               "ParameterName" =: _pParameterName,
               "Description" =: _pDescription]

-- | A list of the log types whose configuration is still pending. In other words, these log types are in the process of being activated or deactivated.
--
--
--
-- /See:/ 'pendingCloudwatchLogsExports' smart constructor.
data PendingCloudwatchLogsExports = PendingCloudwatchLogsExports'
  { _pcleLogTypesToEnable  :: !(Maybe [Text])
  , _pcleLogTypesToDisable :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PendingCloudwatchLogsExports' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcleLogTypesToEnable' - Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
--
-- * 'pcleLogTypesToDisable' - Log types that are in the process of being enabled. After they are enabled, these log types are exported to CloudWatch Logs.
pendingCloudwatchLogsExports
    :: PendingCloudwatchLogsExports
pendingCloudwatchLogsExports =
  PendingCloudwatchLogsExports'
    {_pcleLogTypesToEnable = Nothing, _pcleLogTypesToDisable = Nothing}


-- | Log types that are in the process of being deactivated. After they are deactivated, these log types aren't exported to CloudWatch Logs.
pcleLogTypesToEnable :: Lens' PendingCloudwatchLogsExports [Text]
pcleLogTypesToEnable = lens _pcleLogTypesToEnable (\ s a -> s{_pcleLogTypesToEnable = a}) . _Default . _Coerce

-- | Log types that are in the process of being enabled. After they are enabled, these log types are exported to CloudWatch Logs.
pcleLogTypesToDisable :: Lens' PendingCloudwatchLogsExports [Text]
pcleLogTypesToDisable = lens _pcleLogTypesToDisable (\ s a -> s{_pcleLogTypesToDisable = a}) . _Default . _Coerce

instance FromXML PendingCloudwatchLogsExports where
        parseXML x
          = PendingCloudwatchLogsExports' <$>
              (x .@? "LogTypesToEnable" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*>
                (x .@? "LogTypesToDisable" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable PendingCloudwatchLogsExports where

instance NFData PendingCloudwatchLogsExports where

-- | Provides information about a pending maintenance action for a resource.
--
--
--
-- /See:/ 'pendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { _pmaAutoAppliedAfterDate :: !(Maybe ISO8601)
  , _pmaAction               :: !(Maybe Text)
  , _pmaOptInStatus          :: !(Maybe Text)
  , _pmaDescription          :: !(Maybe Text)
  , _pmaForcedApplyDate      :: !(Maybe ISO8601)
  , _pmaCurrentApplyDate     :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PendingMaintenanceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmaAutoAppliedAfterDate' - The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date. If this date is specified, any @next-maintenance@ opt-in requests are ignored.
--
-- * 'pmaAction' - The type of pending maintenance action that is available for the resource.
--
-- * 'pmaOptInStatus' - Indicates the type of opt-in request that has been received for the resource.
--
-- * 'pmaDescription' - A description providing more detail about the maintenance action.
--
-- * 'pmaForcedApplyDate' - The date when the maintenance action is automatically applied. The maintenance action is applied to the resource on this date regardless of the maintenance window for the resource. If this date is specified, any @immediate@ opt-in requests are ignored.
--
-- * 'pmaCurrentApplyDate' - The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the 'ApplyPendingMaintenanceAction' API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
pendingMaintenanceAction
    :: PendingMaintenanceAction
pendingMaintenanceAction =
  PendingMaintenanceAction'
    { _pmaAutoAppliedAfterDate = Nothing
    , _pmaAction = Nothing
    , _pmaOptInStatus = Nothing
    , _pmaDescription = Nothing
    , _pmaForcedApplyDate = Nothing
    , _pmaCurrentApplyDate = Nothing
    }


-- | The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date. If this date is specified, any @next-maintenance@ opt-in requests are ignored.
pmaAutoAppliedAfterDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaAutoAppliedAfterDate = lens _pmaAutoAppliedAfterDate (\ s a -> s{_pmaAutoAppliedAfterDate = a}) . mapping _Time

-- | The type of pending maintenance action that is available for the resource.
pmaAction :: Lens' PendingMaintenanceAction (Maybe Text)
pmaAction = lens _pmaAction (\ s a -> s{_pmaAction = a})

-- | Indicates the type of opt-in request that has been received for the resource.
pmaOptInStatus :: Lens' PendingMaintenanceAction (Maybe Text)
pmaOptInStatus = lens _pmaOptInStatus (\ s a -> s{_pmaOptInStatus = a})

-- | A description providing more detail about the maintenance action.
pmaDescription :: Lens' PendingMaintenanceAction (Maybe Text)
pmaDescription = lens _pmaDescription (\ s a -> s{_pmaDescription = a})

-- | The date when the maintenance action is automatically applied. The maintenance action is applied to the resource on this date regardless of the maintenance window for the resource. If this date is specified, any @immediate@ opt-in requests are ignored.
pmaForcedApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaForcedApplyDate = lens _pmaForcedApplyDate (\ s a -> s{_pmaForcedApplyDate = a}) . mapping _Time

-- | The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the 'ApplyPendingMaintenanceAction' API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
pmaCurrentApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaCurrentApplyDate = lens _pmaCurrentApplyDate (\ s a -> s{_pmaCurrentApplyDate = a}) . mapping _Time

instance FromXML PendingMaintenanceAction where
        parseXML x
          = PendingMaintenanceAction' <$>
              (x .@? "AutoAppliedAfterDate") <*> (x .@? "Action")
                <*> (x .@? "OptInStatus")
                <*> (x .@? "Description")
                <*> (x .@? "ForcedApplyDate")
                <*> (x .@? "CurrentApplyDate")

instance Hashable PendingMaintenanceAction where

instance NFData PendingMaintenanceAction where

-- | This data type is used as a response element in the 'ModifyDBInstance' action.
--
--
--
-- /See:/ 'pendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { _pmvEngineVersion                :: !(Maybe Text)
  , _pmvMasterUserPassword           :: !(Maybe Text)
  , _pmvDBSubnetGroupName            :: !(Maybe Text)
  , _pmvIOPS                         :: !(Maybe Int)
  , _pmvDBInstanceClass              :: !(Maybe Text)
  , _pmvLicenseModel                 :: !(Maybe Text)
  , _pmvCACertificateIdentifier      :: !(Maybe Text)
  , _pmvDBInstanceIdentifier         :: !(Maybe Text)
  , _pmvPendingCloudwatchLogsExports :: !(Maybe PendingCloudwatchLogsExports)
  , _pmvBackupRetentionPeriod        :: !(Maybe Int)
  , _pmvMultiAZ                      :: !(Maybe Bool)
  , _pmvAllocatedStorage             :: !(Maybe Int)
  , _pmvPort                         :: !(Maybe Int)
  , _pmvStorageType                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmvEngineVersion' - Indicates the database engine version.
--
-- * 'pmvMasterUserPassword' - Contains the pending or currently-in-progress change of the master credentials for the DB instance.
--
-- * 'pmvDBSubnetGroupName' - The new DB subnet group for the DB instance.
--
-- * 'pmvIOPS' - Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
--
-- * 'pmvDBInstanceClass' - Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied.
--
-- * 'pmvLicenseModel' - The license model for the DB instance. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- * 'pmvCACertificateIdentifier' - Specifies the identifier of the CA certificate for the DB instance.
--
-- * 'pmvDBInstanceIdentifier' - Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied.
--
-- * 'pmvPendingCloudwatchLogsExports' - Undocumented member.
--
-- * 'pmvBackupRetentionPeriod' - Specifies the pending number of days for which automated backups are retained.
--
-- * 'pmvMultiAZ' - Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
--
-- * 'pmvAllocatedStorage' - Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied.
--
-- * 'pmvPort' - Specifies the pending port for the DB instance.
--
-- * 'pmvStorageType' - Specifies the storage type to be associated with the DB instance.
pendingModifiedValues
    :: PendingModifiedValues
pendingModifiedValues =
  PendingModifiedValues'
    { _pmvEngineVersion = Nothing
    , _pmvMasterUserPassword = Nothing
    , _pmvDBSubnetGroupName = Nothing
    , _pmvIOPS = Nothing
    , _pmvDBInstanceClass = Nothing
    , _pmvLicenseModel = Nothing
    , _pmvCACertificateIdentifier = Nothing
    , _pmvDBInstanceIdentifier = Nothing
    , _pmvPendingCloudwatchLogsExports = Nothing
    , _pmvBackupRetentionPeriod = Nothing
    , _pmvMultiAZ = Nothing
    , _pmvAllocatedStorage = Nothing
    , _pmvPort = Nothing
    , _pmvStorageType = Nothing
    }


-- | Indicates the database engine version.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\ s a -> s{_pmvEngineVersion = a})

-- | Contains the pending or currently-in-progress change of the master credentials for the DB instance.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword = lens _pmvMasterUserPassword (\ s a -> s{_pmvMasterUserPassword = a})

-- | The new DB subnet group for the DB instance.
pmvDBSubnetGroupName :: Lens' PendingModifiedValues (Maybe Text)
pmvDBSubnetGroupName = lens _pmvDBSubnetGroupName (\ s a -> s{_pmvDBSubnetGroupName = a})

-- | Specifies the new Provisioned IOPS value for the DB instance that will be applied or is currently being applied.
pmvIOPS :: Lens' PendingModifiedValues (Maybe Int)
pmvIOPS = lens _pmvIOPS (\ s a -> s{_pmvIOPS = a})

-- | Contains the new @DBInstanceClass@ for the DB instance that will be applied or is currently being applied.
pmvDBInstanceClass :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceClass = lens _pmvDBInstanceClass (\ s a -> s{_pmvDBInstanceClass = a})

-- | The license model for the DB instance. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
pmvLicenseModel :: Lens' PendingModifiedValues (Maybe Text)
pmvLicenseModel = lens _pmvLicenseModel (\ s a -> s{_pmvLicenseModel = a})

-- | Specifies the identifier of the CA certificate for the DB instance.
pmvCACertificateIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvCACertificateIdentifier = lens _pmvCACertificateIdentifier (\ s a -> s{_pmvCACertificateIdentifier = a})

-- | Contains the new @DBInstanceIdentifier@ for the DB instance that will be applied or is currently being applied.
pmvDBInstanceIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceIdentifier = lens _pmvDBInstanceIdentifier (\ s a -> s{_pmvDBInstanceIdentifier = a})

-- | Undocumented member.
pmvPendingCloudwatchLogsExports :: Lens' PendingModifiedValues (Maybe PendingCloudwatchLogsExports)
pmvPendingCloudwatchLogsExports = lens _pmvPendingCloudwatchLogsExports (\ s a -> s{_pmvPendingCloudwatchLogsExports = a})

-- | Specifies the pending number of days for which automated backups are retained.
pmvBackupRetentionPeriod :: Lens' PendingModifiedValues (Maybe Int)
pmvBackupRetentionPeriod = lens _pmvBackupRetentionPeriod (\ s a -> s{_pmvBackupRetentionPeriod = a})

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ deployment.
pmvMultiAZ :: Lens' PendingModifiedValues (Maybe Bool)
pmvMultiAZ = lens _pmvMultiAZ (\ s a -> s{_pmvMultiAZ = a})

-- | Contains the new @AllocatedStorage@ size for the DB instance that will be applied or is currently being applied.
pmvAllocatedStorage :: Lens' PendingModifiedValues (Maybe Int)
pmvAllocatedStorage = lens _pmvAllocatedStorage (\ s a -> s{_pmvAllocatedStorage = a})

-- | Specifies the pending port for the DB instance.
pmvPort :: Lens' PendingModifiedValues (Maybe Int)
pmvPort = lens _pmvPort (\ s a -> s{_pmvPort = a})

-- | Specifies the storage type to be associated with the DB instance.
pmvStorageType :: Lens' PendingModifiedValues (Maybe Text)
pmvStorageType = lens _pmvStorageType (\ s a -> s{_pmvStorageType = a})

instance FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "MasterUserPassword")
                <*> (x .@? "DBSubnetGroupName")
                <*> (x .@? "Iops")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "LicenseModel")
                <*> (x .@? "CACertificateIdentifier")
                <*> (x .@? "DBInstanceIdentifier")
                <*> (x .@? "PendingCloudwatchLogsExports")
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "Port")
                <*> (x .@? "StorageType")

instance Hashable PendingModifiedValues where

instance NFData PendingModifiedValues where

-- | A range of integer values.
--
--
--
-- /See:/ 'range' smart constructor.
data Range = Range'
  { _rTo   :: !(Maybe Int)
  , _rFrom :: !(Maybe Int)
  , _rStep :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Range' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rTo' - The maximum value in the range.
--
-- * 'rFrom' - The minimum value in the range.
--
-- * 'rStep' - The step value for the range. For example, if you have a range of 5,000 to 10,000, with a step value of 1,000, the valid values start at 5,000 and step up by 1,000. Even though 7,500 is within the range, it isn't a valid value for the range. The valid values are 5,000, 6,000, 7,000, 8,000...
range
    :: Range
range = Range' {_rTo = Nothing, _rFrom = Nothing, _rStep = Nothing}


-- | The maximum value in the range.
rTo :: Lens' Range (Maybe Int)
rTo = lens _rTo (\ s a -> s{_rTo = a})

-- | The minimum value in the range.
rFrom :: Lens' Range (Maybe Int)
rFrom = lens _rFrom (\ s a -> s{_rFrom = a})

-- | The step value for the range. For example, if you have a range of 5,000 to 10,000, with a step value of 1,000, the valid values start at 5,000 and step up by 1,000. Even though 7,500 is within the range, it isn't a valid value for the range. The valid values are 5,000, 6,000, 7,000, 8,000...
rStep :: Lens' Range (Maybe Int)
rStep = lens _rStep (\ s a -> s{_rStep = a})

instance FromXML Range where
        parseXML x
          = Range' <$>
              (x .@? "To") <*> (x .@? "From") <*> (x .@? "Step")

instance Hashable Range where

instance NFData Range where

-- | This data type is used as a response element in the 'DescribeReservedDBInstances' and 'DescribeReservedDBInstancesOfferings' actions.
--
--
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { _rcRecurringChargeFrequency :: !(Maybe Text)
  , _rcRecurringChargeAmount    :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRecurringChargeFrequency' - The frequency of the recurring charge.
--
-- * 'rcRecurringChargeAmount' - The amount of the recurring charge.
recurringCharge
    :: RecurringCharge
recurringCharge =
  RecurringCharge'
    {_rcRecurringChargeFrequency = Nothing, _rcRecurringChargeAmount = Nothing}


-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\ s a -> s{_rcRecurringChargeFrequency = a})

-- | The amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\ s a -> s{_rcRecurringChargeAmount = a})

instance FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' <$>
              (x .@? "RecurringChargeFrequency") <*>
                (x .@? "RecurringChargeAmount")

instance Hashable RecurringCharge where

instance NFData RecurringCharge where

-- | This data type is used as a response element in the 'DescribeReservedDBInstances' and 'PurchaseReservedDBInstancesOffering' actions.
--
--
--
-- /See:/ 'reservedDBInstance' smart constructor.
data ReservedDBInstance = ReservedDBInstance'
  { _rdiDBInstanceCount               :: !(Maybe Int)
  , _rdiState                         :: !(Maybe Text)
  , _rdiCurrencyCode                  :: !(Maybe Text)
  , _rdiStartTime                     :: !(Maybe ISO8601)
  , _rdiProductDescription            :: !(Maybe Text)
  , _rdiReservedDBInstanceId          :: !(Maybe Text)
  , _rdiReservedDBInstanceARN         :: !(Maybe Text)
  , _rdiDBInstanceClass               :: !(Maybe Text)
  , _rdiMultiAZ                       :: !(Maybe Bool)
  , _rdiReservedDBInstancesOfferingId :: !(Maybe Text)
  , _rdiRecurringCharges              :: !(Maybe [RecurringCharge])
  , _rdiOfferingType                  :: !(Maybe Text)
  , _rdiUsagePrice                    :: !(Maybe Double)
  , _rdiFixedPrice                    :: !(Maybe Double)
  , _rdiDuration                      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdiDBInstanceCount' - The number of reserved DB instances.
--
-- * 'rdiState' - The state of the reserved DB instance.
--
-- * 'rdiCurrencyCode' - The currency code for the reserved DB instance.
--
-- * 'rdiStartTime' - The time the reservation started.
--
-- * 'rdiProductDescription' - The description of the reserved DB instance.
--
-- * 'rdiReservedDBInstanceId' - The unique identifier for the reservation.
--
-- * 'rdiReservedDBInstanceARN' - The Amazon Resource Name (ARN) for the reserved DB instance.
--
-- * 'rdiDBInstanceClass' - The DB instance class for the reserved DB instance.
--
-- * 'rdiMultiAZ' - Indicates if the reservation applies to Multi-AZ deployments.
--
-- * 'rdiReservedDBInstancesOfferingId' - The offering identifier.
--
-- * 'rdiRecurringCharges' - The recurring price charged to run this reserved DB instance.
--
-- * 'rdiOfferingType' - The offering type of this reserved DB instance.
--
-- * 'rdiUsagePrice' - The hourly price charged for this reserved DB instance.
--
-- * 'rdiFixedPrice' - The fixed price charged for this reserved DB instance.
--
-- * 'rdiDuration' - The duration of the reservation in seconds.
reservedDBInstance
    :: ReservedDBInstance
reservedDBInstance =
  ReservedDBInstance'
    { _rdiDBInstanceCount = Nothing
    , _rdiState = Nothing
    , _rdiCurrencyCode = Nothing
    , _rdiStartTime = Nothing
    , _rdiProductDescription = Nothing
    , _rdiReservedDBInstanceId = Nothing
    , _rdiReservedDBInstanceARN = Nothing
    , _rdiDBInstanceClass = Nothing
    , _rdiMultiAZ = Nothing
    , _rdiReservedDBInstancesOfferingId = Nothing
    , _rdiRecurringCharges = Nothing
    , _rdiOfferingType = Nothing
    , _rdiUsagePrice = Nothing
    , _rdiFixedPrice = Nothing
    , _rdiDuration = Nothing
    }


-- | The number of reserved DB instances.
rdiDBInstanceCount :: Lens' ReservedDBInstance (Maybe Int)
rdiDBInstanceCount = lens _rdiDBInstanceCount (\ s a -> s{_rdiDBInstanceCount = a})

-- | The state of the reserved DB instance.
rdiState :: Lens' ReservedDBInstance (Maybe Text)
rdiState = lens _rdiState (\ s a -> s{_rdiState = a})

-- | The currency code for the reserved DB instance.
rdiCurrencyCode :: Lens' ReservedDBInstance (Maybe Text)
rdiCurrencyCode = lens _rdiCurrencyCode (\ s a -> s{_rdiCurrencyCode = a})

-- | The time the reservation started.
rdiStartTime :: Lens' ReservedDBInstance (Maybe UTCTime)
rdiStartTime = lens _rdiStartTime (\ s a -> s{_rdiStartTime = a}) . mapping _Time

-- | The description of the reserved DB instance.
rdiProductDescription :: Lens' ReservedDBInstance (Maybe Text)
rdiProductDescription = lens _rdiProductDescription (\ s a -> s{_rdiProductDescription = a})

-- | The unique identifier for the reservation.
rdiReservedDBInstanceId :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstanceId = lens _rdiReservedDBInstanceId (\ s a -> s{_rdiReservedDBInstanceId = a})

-- | The Amazon Resource Name (ARN) for the reserved DB instance.
rdiReservedDBInstanceARN :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstanceARN = lens _rdiReservedDBInstanceARN (\ s a -> s{_rdiReservedDBInstanceARN = a})

-- | The DB instance class for the reserved DB instance.
rdiDBInstanceClass :: Lens' ReservedDBInstance (Maybe Text)
rdiDBInstanceClass = lens _rdiDBInstanceClass (\ s a -> s{_rdiDBInstanceClass = a})

-- | Indicates if the reservation applies to Multi-AZ deployments.
rdiMultiAZ :: Lens' ReservedDBInstance (Maybe Bool)
rdiMultiAZ = lens _rdiMultiAZ (\ s a -> s{_rdiMultiAZ = a})

-- | The offering identifier.
rdiReservedDBInstancesOfferingId :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstancesOfferingId = lens _rdiReservedDBInstancesOfferingId (\ s a -> s{_rdiReservedDBInstancesOfferingId = a})

-- | The recurring price charged to run this reserved DB instance.
rdiRecurringCharges :: Lens' ReservedDBInstance [RecurringCharge]
rdiRecurringCharges = lens _rdiRecurringCharges (\ s a -> s{_rdiRecurringCharges = a}) . _Default . _Coerce

-- | The offering type of this reserved DB instance.
rdiOfferingType :: Lens' ReservedDBInstance (Maybe Text)
rdiOfferingType = lens _rdiOfferingType (\ s a -> s{_rdiOfferingType = a})

-- | The hourly price charged for this reserved DB instance.
rdiUsagePrice :: Lens' ReservedDBInstance (Maybe Double)
rdiUsagePrice = lens _rdiUsagePrice (\ s a -> s{_rdiUsagePrice = a})

-- | The fixed price charged for this reserved DB instance.
rdiFixedPrice :: Lens' ReservedDBInstance (Maybe Double)
rdiFixedPrice = lens _rdiFixedPrice (\ s a -> s{_rdiFixedPrice = a})

-- | The duration of the reservation in seconds.
rdiDuration :: Lens' ReservedDBInstance (Maybe Int)
rdiDuration = lens _rdiDuration (\ s a -> s{_rdiDuration = a})

instance FromXML ReservedDBInstance where
        parseXML x
          = ReservedDBInstance' <$>
              (x .@? "DBInstanceCount") <*> (x .@? "State") <*>
                (x .@? "CurrencyCode")
                <*> (x .@? "StartTime")
                <*> (x .@? "ProductDescription")
                <*> (x .@? "ReservedDBInstanceId")
                <*> (x .@? "ReservedDBInstanceArn")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "ReservedDBInstancesOfferingId")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")

instance Hashable ReservedDBInstance where

instance NFData ReservedDBInstance where

-- | This data type is used as a response element in the 'DescribeReservedDBInstancesOfferings' action.
--
--
--
-- /See:/ 'reservedDBInstancesOffering' smart constructor.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering'
  { _rdioCurrencyCode                  :: !(Maybe Text)
  , _rdioProductDescription            :: !(Maybe Text)
  , _rdioDBInstanceClass               :: !(Maybe Text)
  , _rdioMultiAZ                       :: !(Maybe Bool)
  , _rdioReservedDBInstancesOfferingId :: !(Maybe Text)
  , _rdioRecurringCharges              :: !(Maybe [RecurringCharge])
  , _rdioOfferingType                  :: !(Maybe Text)
  , _rdioUsagePrice                    :: !(Maybe Double)
  , _rdioFixedPrice                    :: !(Maybe Double)
  , _rdioDuration                      :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedDBInstancesOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdioCurrencyCode' - The currency code for the reserved DB instance offering.
--
-- * 'rdioProductDescription' - The database engine used by the offering.
--
-- * 'rdioDBInstanceClass' - The DB instance class for the reserved DB instance.
--
-- * 'rdioMultiAZ' - Indicates if the offering applies to Multi-AZ deployments.
--
-- * 'rdioReservedDBInstancesOfferingId' - The offering identifier.
--
-- * 'rdioRecurringCharges' - The recurring price charged to run this reserved DB instance.
--
-- * 'rdioOfferingType' - The offering type.
--
-- * 'rdioUsagePrice' - The hourly price charged for this offering.
--
-- * 'rdioFixedPrice' - The fixed price charged for this offering.
--
-- * 'rdioDuration' - The duration of the offering in seconds.
reservedDBInstancesOffering
    :: ReservedDBInstancesOffering
reservedDBInstancesOffering =
  ReservedDBInstancesOffering'
    { _rdioCurrencyCode = Nothing
    , _rdioProductDescription = Nothing
    , _rdioDBInstanceClass = Nothing
    , _rdioMultiAZ = Nothing
    , _rdioReservedDBInstancesOfferingId = Nothing
    , _rdioRecurringCharges = Nothing
    , _rdioOfferingType = Nothing
    , _rdioUsagePrice = Nothing
    , _rdioFixedPrice = Nothing
    , _rdioDuration = Nothing
    }


-- | The currency code for the reserved DB instance offering.
rdioCurrencyCode :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioCurrencyCode = lens _rdioCurrencyCode (\ s a -> s{_rdioCurrencyCode = a})

-- | The database engine used by the offering.
rdioProductDescription :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioProductDescription = lens _rdioProductDescription (\ s a -> s{_rdioProductDescription = a})

-- | The DB instance class for the reserved DB instance.
rdioDBInstanceClass :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioDBInstanceClass = lens _rdioDBInstanceClass (\ s a -> s{_rdioDBInstanceClass = a})

-- | Indicates if the offering applies to Multi-AZ deployments.
rdioMultiAZ :: Lens' ReservedDBInstancesOffering (Maybe Bool)
rdioMultiAZ = lens _rdioMultiAZ (\ s a -> s{_rdioMultiAZ = a})

-- | The offering identifier.
rdioReservedDBInstancesOfferingId :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioReservedDBInstancesOfferingId = lens _rdioReservedDBInstancesOfferingId (\ s a -> s{_rdioReservedDBInstancesOfferingId = a})

-- | The recurring price charged to run this reserved DB instance.
rdioRecurringCharges :: Lens' ReservedDBInstancesOffering [RecurringCharge]
rdioRecurringCharges = lens _rdioRecurringCharges (\ s a -> s{_rdioRecurringCharges = a}) . _Default . _Coerce

-- | The offering type.
rdioOfferingType :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioOfferingType = lens _rdioOfferingType (\ s a -> s{_rdioOfferingType = a})

-- | The hourly price charged for this offering.
rdioUsagePrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdioUsagePrice = lens _rdioUsagePrice (\ s a -> s{_rdioUsagePrice = a})

-- | The fixed price charged for this offering.
rdioFixedPrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdioFixedPrice = lens _rdioFixedPrice (\ s a -> s{_rdioFixedPrice = a})

-- | The duration of the offering in seconds.
rdioDuration :: Lens' ReservedDBInstancesOffering (Maybe Int)
rdioDuration = lens _rdioDuration (\ s a -> s{_rdioDuration = a})

instance FromXML ReservedDBInstancesOffering where
        parseXML x
          = ReservedDBInstancesOffering' <$>
              (x .@? "CurrencyCode") <*>
                (x .@? "ProductDescription")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "ReservedDBInstancesOfferingId")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")

instance Hashable ReservedDBInstancesOffering where

instance NFData ReservedDBInstancesOffering where

-- | Describes the pending maintenance actions for a resource.
--
--
--
-- /See:/ 'resourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
  { _rpmaPendingMaintenanceActionDetails :: !(Maybe [PendingMaintenanceAction])
  , _rpmaResourceIdentifier              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourcePendingMaintenanceActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpmaPendingMaintenanceActionDetails' - A list that provides details about the pending maintenance actions for the resource.
--
-- * 'rpmaResourceIdentifier' - The ARN of the resource that has pending maintenance actions.
resourcePendingMaintenanceActions
    :: ResourcePendingMaintenanceActions
resourcePendingMaintenanceActions =
  ResourcePendingMaintenanceActions'
    { _rpmaPendingMaintenanceActionDetails = Nothing
    , _rpmaResourceIdentifier = Nothing
    }


-- | A list that provides details about the pending maintenance actions for the resource.
rpmaPendingMaintenanceActionDetails :: Lens' ResourcePendingMaintenanceActions [PendingMaintenanceAction]
rpmaPendingMaintenanceActionDetails = lens _rpmaPendingMaintenanceActionDetails (\ s a -> s{_rpmaPendingMaintenanceActionDetails = a}) . _Default . _Coerce

-- | The ARN of the resource that has pending maintenance actions.
rpmaResourceIdentifier :: Lens' ResourcePendingMaintenanceActions (Maybe Text)
rpmaResourceIdentifier = lens _rpmaResourceIdentifier (\ s a -> s{_rpmaResourceIdentifier = a})

instance FromXML ResourcePendingMaintenanceActions
         where
        parseXML x
          = ResourcePendingMaintenanceActions' <$>
              (x .@? "PendingMaintenanceActionDetails" .!@ mempty
                 >>= may (parseXMLList "PendingMaintenanceAction"))
                <*> (x .@? "ResourceIdentifier")

instance Hashable ResourcePendingMaintenanceActions
         where

instance NFData ResourcePendingMaintenanceActions
         where

-- | Contains an AWS Region name as the result of a successful call to the 'DescribeSourceRegions' action.
--
--
--
-- /See:/ 'sourceRegion' smart constructor.
data SourceRegion = SourceRegion'
  { _srStatus     :: !(Maybe Text)
  , _srRegionName :: !(Maybe Text)
  , _srEndpoint   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceRegion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srStatus' - The status of the source AWS Region.
--
-- * 'srRegionName' - The name of the source AWS Region.
--
-- * 'srEndpoint' - The endpoint for the source AWS Region endpoint.
sourceRegion
    :: SourceRegion
sourceRegion =
  SourceRegion'
    {_srStatus = Nothing, _srRegionName = Nothing, _srEndpoint = Nothing}


-- | The status of the source AWS Region.
srStatus :: Lens' SourceRegion (Maybe Text)
srStatus = lens _srStatus (\ s a -> s{_srStatus = a})

-- | The name of the source AWS Region.
srRegionName :: Lens' SourceRegion (Maybe Text)
srRegionName = lens _srRegionName (\ s a -> s{_srRegionName = a})

-- | The endpoint for the source AWS Region endpoint.
srEndpoint :: Lens' SourceRegion (Maybe Text)
srEndpoint = lens _srEndpoint (\ s a -> s{_srEndpoint = a})

instance FromXML SourceRegion where
        parseXML x
          = SourceRegion' <$>
              (x .@? "Status") <*> (x .@? "RegionName") <*>
                (x .@? "Endpoint")

instance Hashable SourceRegion where

instance NFData SourceRegion where

-- | This data type is used as a response element in the 'DescribeDBSubnetGroups' action.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetStatus           :: !(Maybe Text)
  , _sSubnetIdentifier       :: !(Maybe Text)
  , _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetStatus' - Specifies the status of the subnet.
--
-- * 'sSubnetIdentifier' - Specifies the identifier of the subnet.
--
-- * 'sSubnetAvailabilityZone' - Undocumented member.
subnet
    :: Subnet
subnet =
  Subnet'
    { _sSubnetStatus = Nothing
    , _sSubnetIdentifier = Nothing
    , _sSubnetAvailabilityZone = Nothing
    }


-- | Specifies the status of the subnet.
sSubnetStatus :: Lens' Subnet (Maybe Text)
sSubnetStatus = lens _sSubnetStatus (\ s a -> s{_sSubnetStatus = a})

-- | Specifies the identifier of the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a})

-- | Undocumented member.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a})

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "SubnetStatus") <*> (x .@? "SubnetIdentifier")
                <*> (x .@? "SubnetAvailabilityZone")

instance Hashable Subnet where

instance NFData Subnet where

-- | Metadata assigned to an Amazon RDS resource consisting of a key-value pair.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - A value is the optional value of the tag. The string value can be from 1 to 256 Unicode characters in length and can't be prefixed with "aws:" or "rds:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
--
-- * 'tagKey' - A key is the required name of the tag. The string value can be from 1 to 128 Unicode characters in length and can't be prefixed with "aws:" or "rds:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | A value is the optional value of the tag. The string value can be from 1 to 256 Unicode characters in length and can't be prefixed with "aws:" or "rds:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | A key is the required name of the tag. The string value can be from 1 to 128 Unicode characters in length and can't be prefixed with "aws:" or "rds:". The string can only contain only the set of Unicode letters, digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | A time zone associated with a 'DBInstance' or a 'DBSnapshot' . This data type is an element in the response to the 'DescribeDBInstances' , the 'DescribeDBSnapshots' , and the 'DescribeDBEngineVersions' actions.
--
--
--
-- /See:/ 'timezone' smart constructor.
newtype Timezone = Timezone'
  { _tTimezoneName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Timezone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTimezoneName' - The name of the time zone.
timezone
    :: Timezone
timezone = Timezone' {_tTimezoneName = Nothing}


-- | The name of the time zone.
tTimezoneName :: Lens' Timezone (Maybe Text)
tTimezoneName = lens _tTimezoneName (\ s a -> s{_tTimezoneName = a})

instance FromXML Timezone where
        parseXML x = Timezone' <$> (x .@? "TimezoneName")

instance Hashable Timezone where

instance NFData Timezone where

-- | The version of the database engine that a DB instance can be upgraded to.
--
--
--
-- /See:/ 'upgradeTarget' smart constructor.
data UpgradeTarget = UpgradeTarget'
  { _utEngineVersion         :: !(Maybe Text)
  , _utIsMajorVersionUpgrade :: !(Maybe Bool)
  , _utEngine                :: !(Maybe Text)
  , _utAutoUpgrade           :: !(Maybe Bool)
  , _utDescription           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpgradeTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utEngineVersion' - The version number of the upgrade target database engine.
--
-- * 'utIsMajorVersionUpgrade' - A value that indicates whether a database engine is upgraded to a major version.
--
-- * 'utEngine' - The name of the upgrade target database engine.
--
-- * 'utAutoUpgrade' - A value that indicates whether the target version is applied to any source DB instances that have AutoMinorVersionUpgrade set to true.
--
-- * 'utDescription' - The version of the database engine that a DB instance can be upgraded to.
upgradeTarget
    :: UpgradeTarget
upgradeTarget =
  UpgradeTarget'
    { _utEngineVersion = Nothing
    , _utIsMajorVersionUpgrade = Nothing
    , _utEngine = Nothing
    , _utAutoUpgrade = Nothing
    , _utDescription = Nothing
    }


-- | The version number of the upgrade target database engine.
utEngineVersion :: Lens' UpgradeTarget (Maybe Text)
utEngineVersion = lens _utEngineVersion (\ s a -> s{_utEngineVersion = a})

-- | A value that indicates whether a database engine is upgraded to a major version.
utIsMajorVersionUpgrade :: Lens' UpgradeTarget (Maybe Bool)
utIsMajorVersionUpgrade = lens _utIsMajorVersionUpgrade (\ s a -> s{_utIsMajorVersionUpgrade = a})

-- | The name of the upgrade target database engine.
utEngine :: Lens' UpgradeTarget (Maybe Text)
utEngine = lens _utEngine (\ s a -> s{_utEngine = a})

-- | A value that indicates whether the target version is applied to any source DB instances that have AutoMinorVersionUpgrade set to true.
utAutoUpgrade :: Lens' UpgradeTarget (Maybe Bool)
utAutoUpgrade = lens _utAutoUpgrade (\ s a -> s{_utAutoUpgrade = a})

-- | The version of the database engine that a DB instance can be upgraded to.
utDescription :: Lens' UpgradeTarget (Maybe Text)
utDescription = lens _utDescription (\ s a -> s{_utDescription = a})

instance FromXML UpgradeTarget where
        parseXML x
          = UpgradeTarget' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "IsMajorVersionUpgrade")
                <*> (x .@? "Engine")
                <*> (x .@? "AutoUpgrade")
                <*> (x .@? "Description")

instance Hashable UpgradeTarget where

instance NFData UpgradeTarget where

-- | This data type is used as a response element for queries on VPC security group membership.
--
--
--
-- /See:/ 'vpcSecurityGroupMembership' smart constructor.
data VPCSecurityGroupMembership = VPCSecurityGroupMembership'
  { _vsgmStatus             :: !(Maybe Text)
  , _vsgmVPCSecurityGroupId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPCSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsgmStatus' - The status of the VPC security group.
--
-- * 'vsgmVPCSecurityGroupId' - The name of the VPC security group.
vpcSecurityGroupMembership
    :: VPCSecurityGroupMembership
vpcSecurityGroupMembership =
  VPCSecurityGroupMembership'
    {_vsgmStatus = Nothing, _vsgmVPCSecurityGroupId = Nothing}


-- | The status of the VPC security group.
vsgmStatus :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\ s a -> s{_vsgmStatus = a})

-- | The name of the VPC security group.
vsgmVPCSecurityGroupId :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmVPCSecurityGroupId = lens _vsgmVPCSecurityGroupId (\ s a -> s{_vsgmVPCSecurityGroupId = a})

instance FromXML VPCSecurityGroupMembership where
        parseXML x
          = VPCSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "VpcSecurityGroupId")

instance Hashable VPCSecurityGroupMembership where

instance NFData VPCSecurityGroupMembership where

-- | Information about valid modifications that you can make to your DB instance. Contains the result of a successful call to the 'DescribeValidDBInstanceModifications' action. You can use this information when you call 'ModifyDBInstance' .
--
--
--
-- /See:/ 'validDBInstanceModificationsMessage' smart constructor.
newtype ValidDBInstanceModificationsMessage = ValidDBInstanceModificationsMessage'
  { _vdimmStorage :: Maybe [ValidStorageOptions]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidDBInstanceModificationsMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdimmStorage' - Valid storage options for your DB instance.
validDBInstanceModificationsMessage
    :: ValidDBInstanceModificationsMessage
validDBInstanceModificationsMessage =
  ValidDBInstanceModificationsMessage' {_vdimmStorage = Nothing}


-- | Valid storage options for your DB instance.
vdimmStorage :: Lens' ValidDBInstanceModificationsMessage [ValidStorageOptions]
vdimmStorage = lens _vdimmStorage (\ s a -> s{_vdimmStorage = a}) . _Default . _Coerce

instance FromXML ValidDBInstanceModificationsMessage
         where
        parseXML x
          = ValidDBInstanceModificationsMessage' <$>
              (x .@? "Storage" .!@ mempty >>=
                 may (parseXMLList "ValidStorageOptions"))

instance Hashable ValidDBInstanceModificationsMessage
         where

instance NFData ValidDBInstanceModificationsMessage
         where

-- | Information about valid modifications that you can make to your DB instance. Contains the result of a successful call to the 'DescribeValidDBInstanceModifications' action.
--
--
--
-- /See:/ 'validStorageOptions' smart constructor.
data ValidStorageOptions = ValidStorageOptions'
  { _vsoStorageSize        :: !(Maybe [Range])
  , _vsoProvisionedIOPS    :: !(Maybe [Range])
  , _vsoIOPSToStorageRatio :: !(Maybe [DoubleRange])
  , _vsoStorageType        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidStorageOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsoStorageSize' - The valid range of storage in gibibytes. For example, 100 to 16384.
--
-- * 'vsoProvisionedIOPS' - The valid range of provisioned IOPS. For example, 1000-20000.
--
-- * 'vsoIOPSToStorageRatio' - The valid range of Provisioned IOPS to gibibytes of storage multiplier. For example, 3-10, which means that provisioned IOPS can be between 3 and 10 times storage.
--
-- * 'vsoStorageType' - The valid storage types for your DB instance. For example, gp2, io1.
validStorageOptions
    :: ValidStorageOptions
validStorageOptions =
  ValidStorageOptions'
    { _vsoStorageSize = Nothing
    , _vsoProvisionedIOPS = Nothing
    , _vsoIOPSToStorageRatio = Nothing
    , _vsoStorageType = Nothing
    }


-- | The valid range of storage in gibibytes. For example, 100 to 16384.
vsoStorageSize :: Lens' ValidStorageOptions [Range]
vsoStorageSize = lens _vsoStorageSize (\ s a -> s{_vsoStorageSize = a}) . _Default . _Coerce

-- | The valid range of provisioned IOPS. For example, 1000-20000.
vsoProvisionedIOPS :: Lens' ValidStorageOptions [Range]
vsoProvisionedIOPS = lens _vsoProvisionedIOPS (\ s a -> s{_vsoProvisionedIOPS = a}) . _Default . _Coerce

-- | The valid range of Provisioned IOPS to gibibytes of storage multiplier. For example, 3-10, which means that provisioned IOPS can be between 3 and 10 times storage.
vsoIOPSToStorageRatio :: Lens' ValidStorageOptions [DoubleRange]
vsoIOPSToStorageRatio = lens _vsoIOPSToStorageRatio (\ s a -> s{_vsoIOPSToStorageRatio = a}) . _Default . _Coerce

-- | The valid storage types for your DB instance. For example, gp2, io1.
vsoStorageType :: Lens' ValidStorageOptions (Maybe Text)
vsoStorageType = lens _vsoStorageType (\ s a -> s{_vsoStorageType = a})

instance FromXML ValidStorageOptions where
        parseXML x
          = ValidStorageOptions' <$>
              (x .@? "StorageSize" .!@ mempty >>=
                 may (parseXMLList "Range"))
                <*>
                (x .@? "ProvisionedIops" .!@ mempty >>=
                   may (parseXMLList "Range"))
                <*>
                (x .@? "IopsToStorageRatio" .!@ mempty >>=
                   may (parseXMLList "DoubleRange"))
                <*> (x .@? "StorageType")

instance Hashable ValidStorageOptions where

instance NFData ValidStorageOptions where
