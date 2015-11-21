{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.Product where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types.Sum

-- | Describes a quota for an AWS account, for example, the number of DB
-- instances allowed.
--
-- /See:/ 'accountQuota' smart constructor.
data AccountQuota = AccountQuota'
    { _aqMax              :: !(Maybe Integer)
    , _aqUsed             :: !(Maybe Integer)
    , _aqAccountQuotaName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AccountQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqMax'
--
-- * 'aqUsed'
--
-- * 'aqAccountQuotaName'
accountQuota
    :: AccountQuota
accountQuota =
    AccountQuota'
    { _aqMax = Nothing
    , _aqUsed = Nothing
    , _aqAccountQuotaName = Nothing
    }

-- | The maximum allowed value for the quota.
aqMax :: Lens' AccountQuota (Maybe Integer)
aqMax = lens _aqMax (\ s a -> s{_aqMax = a});

-- | The amount currently used toward the quota maximum.
aqUsed :: Lens' AccountQuota (Maybe Integer)
aqUsed = lens _aqUsed (\ s a -> s{_aqUsed = a});

-- | The name of the Amazon RDS quota for this AWS account.
aqAccountQuotaName :: Lens' AccountQuota (Maybe Text)
aqAccountQuotaName = lens _aqAccountQuotaName (\ s a -> s{_aqAccountQuotaName = a});

instance FromXML AccountQuota where
        parseXML x
          = AccountQuota' <$>
              (x .@? "Max") <*> (x .@? "Used") <*>
                (x .@? "AccountQuotaName")

-- | Contains Availability Zone information.
--
-- This data type is used as an element in the following data type:
--
-- -   OrderableDBInstanceOption
--
-- /See:/ 'availabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
    { _azName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName'
availabilityZone
    :: AvailabilityZone
availabilityZone =
    AvailabilityZone'
    { _azName = Nothing
    }

-- | The name of the availability zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a});

instance FromXML AvailabilityZone where
        parseXML x = AvailabilityZone' <$> (x .@? "Name")

-- | A CA certificate for an AWS account.
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
    { _cCertificateType       :: !(Maybe Text)
    , _cValidTill             :: !(Maybe ISO8601)
    , _cCertificateIdentifier :: !(Maybe Text)
    , _cThumbprint            :: !(Maybe Text)
    , _cValidFrom             :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateType'
--
-- * 'cValidTill'
--
-- * 'cCertificateIdentifier'
--
-- * 'cThumbprint'
--
-- * 'cValidFrom'
certificate
    :: Certificate
certificate =
    Certificate'
    { _cCertificateType = Nothing
    , _cValidTill = Nothing
    , _cCertificateIdentifier = Nothing
    , _cThumbprint = Nothing
    , _cValidFrom = Nothing
    }

-- | The type of the certificate.
cCertificateType :: Lens' Certificate (Maybe Text)
cCertificateType = lens _cCertificateType (\ s a -> s{_cCertificateType = a});

-- | The final date that the certificate continues to be valid.
cValidTill :: Lens' Certificate (Maybe UTCTime)
cValidTill = lens _cValidTill (\ s a -> s{_cValidTill = a}) . mapping _Time;

-- | The unique key that identifies a certificate.
cCertificateIdentifier :: Lens' Certificate (Maybe Text)
cCertificateIdentifier = lens _cCertificateIdentifier (\ s a -> s{_cCertificateIdentifier = a});

-- | The thumbprint of the certificate.
cThumbprint :: Lens' Certificate (Maybe Text)
cThumbprint = lens _cThumbprint (\ s a -> s{_cThumbprint = a});

-- | The starting date from which the certificate is valid.
cValidFrom :: Lens' Certificate (Maybe UTCTime)
cValidFrom = lens _cValidFrom (\ s a -> s{_cValidFrom = a}) . mapping _Time;

instance FromXML Certificate where
        parseXML x
          = Certificate' <$>
              (x .@? "CertificateType") <*> (x .@? "ValidTill") <*>
                (x .@? "CertificateIdentifier")
                <*> (x .@? "Thumbprint")
                <*> (x .@? "ValidFrom")

-- | This data type is used as a response element in the action
-- DescribeDBEngineVersions.
--
-- /See:/ 'characterSet' smart constructor.
data CharacterSet = CharacterSet'
    { _csCharacterSetName        :: !(Maybe Text)
    , _csCharacterSetDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CharacterSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCharacterSetName'
--
-- * 'csCharacterSetDescription'
characterSet
    :: CharacterSet
characterSet =
    CharacterSet'
    { _csCharacterSetName = Nothing
    , _csCharacterSetDescription = Nothing
    }

-- | The name of the character set.
csCharacterSetName :: Lens' CharacterSet (Maybe Text)
csCharacterSetName = lens _csCharacterSetName (\ s a -> s{_csCharacterSetName = a});

-- | The description of the character set.
csCharacterSetDescription :: Lens' CharacterSet (Maybe Text)
csCharacterSetDescription = lens _csCharacterSetDescription (\ s a -> s{_csCharacterSetDescription = a});

instance FromXML CharacterSet where
        parseXML x
          = CharacterSet' <$>
              (x .@? "CharacterSetName") <*>
                (x .@? "CharacterSetDescription")

-- | Contains the result of a successful invocation of the following actions:
--
-- -   CreateDBCluster
-- -   DeleteDBCluster
-- -   FailoverDBCluster
-- -   ModifyDBCluster
-- -   RestoreDBClusterFromSnapshot
--
-- This data type is used as a response element in the DescribeDBClusters
-- action.
--
-- /See:/ 'dbCluster' smart constructor.
data DBCluster = DBCluster'
    { _dcEngineVersion                   :: !(Maybe Text)
    , _dcStatus                          :: !(Maybe Text)
    , _dcDBClusterIdentifier             :: !(Maybe Text)
    , _dcDBClusterMembers                :: !(Maybe [DBClusterMember])
    , _dcHostedZoneId                    :: !(Maybe Text)
    , _dcDBClusterParameterGroup         :: !(Maybe Text)
    , _dcMasterUsername                  :: !(Maybe Text)
    , _dcEarliestRestorableTime          :: !(Maybe ISO8601)
    , _dcEngine                          :: !(Maybe Text)
    , _dcLatestRestorableTime            :: !(Maybe ISO8601)
    , _dcPreferredMaintenanceWindow      :: !(Maybe Text)
    , _dcAvailabilityZones               :: !(Maybe [Text])
    , _dcCharacterSetName                :: !(Maybe Text)
    , _dcPreferredBackupWindow           :: !(Maybe Text)
    , _dcVPCSecurityGroups               :: !(Maybe [VPCSecurityGroupMembership])
    , _dcBackupRetentionPeriod           :: !(Maybe Int)
    , _dcDBSubnetGroup                   :: !(Maybe Text)
    , _dcDatabaseName                    :: !(Maybe Text)
    , _dcAllocatedStorage                :: !(Maybe Int)
    , _dcEndpoint                        :: !(Maybe Text)
    , _dcPercentProgress                 :: !(Maybe Text)
    , _dcPort                            :: !(Maybe Int)
    , _dcDBClusterOptionGroupMemberships :: !(Maybe [DBClusterOptionGroupStatus])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcEngineVersion'
--
-- * 'dcStatus'
--
-- * 'dcDBClusterIdentifier'
--
-- * 'dcDBClusterMembers'
--
-- * 'dcHostedZoneId'
--
-- * 'dcDBClusterParameterGroup'
--
-- * 'dcMasterUsername'
--
-- * 'dcEarliestRestorableTime'
--
-- * 'dcEngine'
--
-- * 'dcLatestRestorableTime'
--
-- * 'dcPreferredMaintenanceWindow'
--
-- * 'dcAvailabilityZones'
--
-- * 'dcCharacterSetName'
--
-- * 'dcPreferredBackupWindow'
--
-- * 'dcVPCSecurityGroups'
--
-- * 'dcBackupRetentionPeriod'
--
-- * 'dcDBSubnetGroup'
--
-- * 'dcDatabaseName'
--
-- * 'dcAllocatedStorage'
--
-- * 'dcEndpoint'
--
-- * 'dcPercentProgress'
--
-- * 'dcPort'
--
-- * 'dcDBClusterOptionGroupMemberships'
dbCluster
    :: DBCluster
dbCluster =
    DBCluster'
    { _dcEngineVersion = Nothing
    , _dcStatus = Nothing
    , _dcDBClusterIdentifier = Nothing
    , _dcDBClusterMembers = Nothing
    , _dcHostedZoneId = Nothing
    , _dcDBClusterParameterGroup = Nothing
    , _dcMasterUsername = Nothing
    , _dcEarliestRestorableTime = Nothing
    , _dcEngine = Nothing
    , _dcLatestRestorableTime = Nothing
    , _dcPreferredMaintenanceWindow = Nothing
    , _dcAvailabilityZones = Nothing
    , _dcCharacterSetName = Nothing
    , _dcPreferredBackupWindow = Nothing
    , _dcVPCSecurityGroups = Nothing
    , _dcBackupRetentionPeriod = Nothing
    , _dcDBSubnetGroup = Nothing
    , _dcDatabaseName = Nothing
    , _dcAllocatedStorage = Nothing
    , _dcEndpoint = Nothing
    , _dcPercentProgress = Nothing
    , _dcPort = Nothing
    , _dcDBClusterOptionGroupMemberships = Nothing
    }

-- | Indicates the database engine version.
dcEngineVersion :: Lens' DBCluster (Maybe Text)
dcEngineVersion = lens _dcEngineVersion (\ s a -> s{_dcEngineVersion = a});

-- | Specifies the current state of this DB cluster.
dcStatus :: Lens' DBCluster (Maybe Text)
dcStatus = lens _dcStatus (\ s a -> s{_dcStatus = a});

-- | Contains a user-supplied DB cluster identifier. This identifier is the
-- unique key that identifies a DB cluster.
dcDBClusterIdentifier :: Lens' DBCluster (Maybe Text)
dcDBClusterIdentifier = lens _dcDBClusterIdentifier (\ s a -> s{_dcDBClusterIdentifier = a});

-- | Provides the list of instances that make up the DB cluster.
dcDBClusterMembers :: Lens' DBCluster [DBClusterMember]
dcDBClusterMembers = lens _dcDBClusterMembers (\ s a -> s{_dcDBClusterMembers = a}) . _Default . _Coerce;

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
dcHostedZoneId :: Lens' DBCluster (Maybe Text)
dcHostedZoneId = lens _dcHostedZoneId (\ s a -> s{_dcHostedZoneId = a});

-- | Specifies the name of the DB cluster parameter group for the DB cluster.
dcDBClusterParameterGroup :: Lens' DBCluster (Maybe Text)
dcDBClusterParameterGroup = lens _dcDBClusterParameterGroup (\ s a -> s{_dcDBClusterParameterGroup = a});

-- | Contains the master username for the DB cluster.
dcMasterUsername :: Lens' DBCluster (Maybe Text)
dcMasterUsername = lens _dcMasterUsername (\ s a -> s{_dcMasterUsername = a});

-- | Specifies the earliest time to which a database can be restored with
-- point-in-time restore.
dcEarliestRestorableTime :: Lens' DBCluster (Maybe UTCTime)
dcEarliestRestorableTime = lens _dcEarliestRestorableTime (\ s a -> s{_dcEarliestRestorableTime = a}) . mapping _Time;

-- | Provides the name of the database engine to be used for this DB cluster.
dcEngine :: Lens' DBCluster (Maybe Text)
dcEngine = lens _dcEngine (\ s a -> s{_dcEngine = a});

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dcLatestRestorableTime :: Lens' DBCluster (Maybe UTCTime)
dcLatestRestorableTime = lens _dcLatestRestorableTime (\ s a -> s{_dcLatestRestorableTime = a}) . mapping _Time;

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dcPreferredMaintenanceWindow :: Lens' DBCluster (Maybe Text)
dcPreferredMaintenanceWindow = lens _dcPreferredMaintenanceWindow (\ s a -> s{_dcPreferredMaintenanceWindow = a});

-- | Provides the list of EC2 Availability Zones that instances in the DB
-- cluster can be created in.
dcAvailabilityZones :: Lens' DBCluster [Text]
dcAvailabilityZones = lens _dcAvailabilityZones (\ s a -> s{_dcAvailabilityZones = a}) . _Default . _Coerce;

-- | If present, specifies the name of the character set that this cluster is
-- associated with.
dcCharacterSetName :: Lens' DBCluster (Maybe Text)
dcCharacterSetName = lens _dcCharacterSetName (\ s a -> s{_dcCharacterSetName = a});

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- 'BackupRetentionPeriod'.
dcPreferredBackupWindow :: Lens' DBCluster (Maybe Text)
dcPreferredBackupWindow = lens _dcPreferredBackupWindow (\ s a -> s{_dcPreferredBackupWindow = a});

-- | Provides a list of VPC security groups that the DB cluster belongs to.
dcVPCSecurityGroups :: Lens' DBCluster [VPCSecurityGroupMembership]
dcVPCSecurityGroups = lens _dcVPCSecurityGroups (\ s a -> s{_dcVPCSecurityGroups = a}) . _Default . _Coerce;

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dcBackupRetentionPeriod :: Lens' DBCluster (Maybe Int)
dcBackupRetentionPeriod = lens _dcBackupRetentionPeriod (\ s a -> s{_dcBackupRetentionPeriod = a});

-- | Specifies information on the subnet group associated with the DB
-- cluster, including the name, description, and subnets in the subnet
-- group.
dcDBSubnetGroup :: Lens' DBCluster (Maybe Text)
dcDBSubnetGroup = lens _dcDBSubnetGroup (\ s a -> s{_dcDBSubnetGroup = a});

-- | Contains the name of the initial database of this DB cluster that was
-- provided at create time, if one was specified when the DB cluster was
-- created. This same name is returned for the life of the DB cluster.
dcDatabaseName :: Lens' DBCluster (Maybe Text)
dcDatabaseName = lens _dcDatabaseName (\ s a -> s{_dcDatabaseName = a});

-- | Specifies the allocated storage size in gigabytes (GB).
dcAllocatedStorage :: Lens' DBCluster (Maybe Int)
dcAllocatedStorage = lens _dcAllocatedStorage (\ s a -> s{_dcAllocatedStorage = a});

-- | Specifies the connection endpoint for the primary instance of the DB
-- cluster.
dcEndpoint :: Lens' DBCluster (Maybe Text)
dcEndpoint = lens _dcEndpoint (\ s a -> s{_dcEndpoint = a});

-- | Specifies the progress of the operation as a percentage.
dcPercentProgress :: Lens' DBCluster (Maybe Text)
dcPercentProgress = lens _dcPercentProgress (\ s a -> s{_dcPercentProgress = a});

-- | Specifies the port that the database engine is listening on.
dcPort :: Lens' DBCluster (Maybe Int)
dcPort = lens _dcPort (\ s a -> s{_dcPort = a});

-- | Provides the list of option group memberships for this DB cluster.
dcDBClusterOptionGroupMemberships :: Lens' DBCluster [DBClusterOptionGroupStatus]
dcDBClusterOptionGroupMemberships = lens _dcDBClusterOptionGroupMemberships (\ s a -> s{_dcDBClusterOptionGroupMemberships = a}) . _Default . _Coerce;

instance FromXML DBCluster where
        parseXML x
          = DBCluster' <$>
              (x .@? "EngineVersion") <*> (x .@? "Status") <*>
                (x .@? "DBClusterIdentifier")
                <*>
                (x .@? "DBClusterMembers" .!@ mempty >>=
                   may (parseXMLList "DBClusterMember"))
                <*> (x .@? "HostedZoneId")
                <*> (x .@? "DBClusterParameterGroup")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "EarliestRestorableTime")
                <*> (x .@? "Engine")
                <*> (x .@? "LatestRestorableTime")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "CharacterSetName")
                <*> (x .@? "PreferredBackupWindow")
                <*>
                (x .@? "VpcSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "VpcSecurityGroupMembership"))
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "DBSubnetGroup")
                <*> (x .@? "DatabaseName")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "Endpoint")
                <*> (x .@? "PercentProgress")
                <*> (x .@? "Port")
                <*>
                (x .@? "DBClusterOptionGroupMemberships" .!@ mempty
                   >>= may (parseXMLList "DBClusterOptionGroup"))

-- | Contains information about an instance that is part of a DB cluster.
--
-- /See:/ 'dbClusterMember' smart constructor.
data DBClusterMember = DBClusterMember'
    { _dcmDBInstanceIdentifier          :: !(Maybe Text)
    , _dcmIsClusterWriter               :: !(Maybe Bool)
    , _dcmDBClusterParameterGroupStatus :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBClusterMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcmDBInstanceIdentifier'
--
-- * 'dcmIsClusterWriter'
--
-- * 'dcmDBClusterParameterGroupStatus'
dbClusterMember
    :: DBClusterMember
dbClusterMember =
    DBClusterMember'
    { _dcmDBInstanceIdentifier = Nothing
    , _dcmIsClusterWriter = Nothing
    , _dcmDBClusterParameterGroupStatus = Nothing
    }

-- | Specifies the instance identifier for this member of the DB cluster.
dcmDBInstanceIdentifier :: Lens' DBClusterMember (Maybe Text)
dcmDBInstanceIdentifier = lens _dcmDBInstanceIdentifier (\ s a -> s{_dcmDBInstanceIdentifier = a});

-- | Value that is 'true' if the cluster member is the primary instance for
-- the DB cluster and 'false' otherwise.
dcmIsClusterWriter :: Lens' DBClusterMember (Maybe Bool)
dcmIsClusterWriter = lens _dcmIsClusterWriter (\ s a -> s{_dcmIsClusterWriter = a});

-- | Specifies the status of the DB cluster parameter group for this member
-- of the DB cluster.
dcmDBClusterParameterGroupStatus :: Lens' DBClusterMember (Maybe Text)
dcmDBClusterParameterGroupStatus = lens _dcmDBClusterParameterGroupStatus (\ s a -> s{_dcmDBClusterParameterGroupStatus = a});

instance FromXML DBClusterMember where
        parseXML x
          = DBClusterMember' <$>
              (x .@? "DBInstanceIdentifier") <*>
                (x .@? "IsClusterWriter")
                <*> (x .@? "DBClusterParameterGroupStatus")

-- | Contains status information for a DB cluster option group.
--
-- /See:/ 'dbClusterOptionGroupStatus' smart constructor.
data DBClusterOptionGroupStatus = DBClusterOptionGroupStatus'
    { _dcogsStatus                   :: !(Maybe Text)
    , _dcogsDBClusterOptionGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBClusterOptionGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcogsStatus'
--
-- * 'dcogsDBClusterOptionGroupName'
dbClusterOptionGroupStatus
    :: DBClusterOptionGroupStatus
dbClusterOptionGroupStatus =
    DBClusterOptionGroupStatus'
    { _dcogsStatus = Nothing
    , _dcogsDBClusterOptionGroupName = Nothing
    }

-- | Specifies the status of the DB cluster option group.
dcogsStatus :: Lens' DBClusterOptionGroupStatus (Maybe Text)
dcogsStatus = lens _dcogsStatus (\ s a -> s{_dcogsStatus = a});

-- | Specifies the name of the DB cluster option group.
dcogsDBClusterOptionGroupName :: Lens' DBClusterOptionGroupStatus (Maybe Text)
dcogsDBClusterOptionGroupName = lens _dcogsDBClusterOptionGroupName (\ s a -> s{_dcogsDBClusterOptionGroupName = a});

instance FromXML DBClusterOptionGroupStatus where
        parseXML x
          = DBClusterOptionGroupStatus' <$>
              (x .@? "Status") <*>
                (x .@? "DBClusterOptionGroupName")

-- | Contains the result of a successful invocation of the
-- CreateDBClusterParameterGroup action.
--
-- This data type is used as a request parameter in the
-- DeleteDBClusterParameterGroup action, and as a response element in the
-- DescribeDBClusterParameterGroups action.
--
-- /See:/ 'dbClusterParameterGroup' smart constructor.
data DBClusterParameterGroup = DBClusterParameterGroup'
    { _dcpgDBParameterGroupFamily      :: !(Maybe Text)
    , _dcpgDBClusterParameterGroupName :: !(Maybe Text)
    , _dcpgDescription                 :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgDBParameterGroupFamily'
--
-- * 'dcpgDBClusterParameterGroupName'
--
-- * 'dcpgDescription'
dbClusterParameterGroup
    :: DBClusterParameterGroup
dbClusterParameterGroup =
    DBClusterParameterGroup'
    { _dcpgDBParameterGroupFamily = Nothing
    , _dcpgDBClusterParameterGroupName = Nothing
    , _dcpgDescription = Nothing
    }

-- | Provides the name of the DB parameter group family that this DB cluster
-- parameter group is compatible with.
dcpgDBParameterGroupFamily :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBParameterGroupFamily = lens _dcpgDBParameterGroupFamily (\ s a -> s{_dcpgDBParameterGroupFamily = a});

-- | Provides the name of the DB cluster parameter group.
dcpgDBClusterParameterGroupName :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDBClusterParameterGroupName = lens _dcpgDBClusterParameterGroupName (\ s a -> s{_dcpgDBClusterParameterGroupName = a});

-- | Provides the customer-specified description for this DB cluster
-- parameter group.
dcpgDescription :: Lens' DBClusterParameterGroup (Maybe Text)
dcpgDescription = lens _dcpgDescription (\ s a -> s{_dcpgDescription = a});

instance FromXML DBClusterParameterGroup where
        parseXML x
          = DBClusterParameterGroup' <$>
              (x .@? "DBParameterGroupFamily") <*>
                (x .@? "DBClusterParameterGroupName")
                <*> (x .@? "Description")

-- |
--
-- /See:/ 'dbClusterParameterGroupNameMessage' smart constructor.
newtype DBClusterParameterGroupNameMessage = DBClusterParameterGroupNameMessage'
    { _dcpgnmDBClusterParameterGroupName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBClusterParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgnmDBClusterParameterGroupName'
dbClusterParameterGroupNameMessage
    :: DBClusterParameterGroupNameMessage
dbClusterParameterGroupNameMessage =
    DBClusterParameterGroupNameMessage'
    { _dcpgnmDBClusterParameterGroupName = Nothing
    }

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- This value is stored as a lowercase string.
dcpgnmDBClusterParameterGroupName :: Lens' DBClusterParameterGroupNameMessage (Maybe Text)
dcpgnmDBClusterParameterGroupName = lens _dcpgnmDBClusterParameterGroupName (\ s a -> s{_dcpgnmDBClusterParameterGroupName = a});

instance FromXML DBClusterParameterGroupNameMessage
         where
        parseXML x
          = DBClusterParameterGroupNameMessage' <$>
              (x .@? "DBClusterParameterGroupName")

-- | Contains the result of a successful invocation of the following actions:
--
-- -   CreateDBClusterSnapshot
-- -   DeleteDBClusterSnapshot
--
-- This data type is used as a response element in the
-- DescribeDBClusterSnapshots action.
--
-- /See:/ 'dbClusterSnapshot' smart constructor.
data DBClusterSnapshot = DBClusterSnapshot'
    { _dcsEngineVersion               :: !(Maybe Text)
    , _dcsStatus                      :: !(Maybe Text)
    , _dcsDBClusterIdentifier         :: !(Maybe Text)
    , _dcsMasterUsername              :: !(Maybe Text)
    , _dcsVPCId                       :: !(Maybe Text)
    , _dcsDBClusterSnapshotIdentifier :: !(Maybe Text)
    , _dcsEngine                      :: !(Maybe Text)
    , _dcsLicenseModel                :: !(Maybe Text)
    , _dcsAvailabilityZones           :: !(Maybe [Text])
    , _dcsSnapshotType                :: !(Maybe Text)
    , _dcsSnapshotCreateTime          :: !(Maybe ISO8601)
    , _dcsAllocatedStorage            :: !(Maybe Int)
    , _dcsClusterCreateTime           :: !(Maybe ISO8601)
    , _dcsPercentProgress             :: !(Maybe Int)
    , _dcsPort                        :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsEngineVersion'
--
-- * 'dcsStatus'
--
-- * 'dcsDBClusterIdentifier'
--
-- * 'dcsMasterUsername'
--
-- * 'dcsVPCId'
--
-- * 'dcsDBClusterSnapshotIdentifier'
--
-- * 'dcsEngine'
--
-- * 'dcsLicenseModel'
--
-- * 'dcsAvailabilityZones'
--
-- * 'dcsSnapshotType'
--
-- * 'dcsSnapshotCreateTime'
--
-- * 'dcsAllocatedStorage'
--
-- * 'dcsClusterCreateTime'
--
-- * 'dcsPercentProgress'
--
-- * 'dcsPort'
dbClusterSnapshot
    :: DBClusterSnapshot
dbClusterSnapshot =
    DBClusterSnapshot'
    { _dcsEngineVersion = Nothing
    , _dcsStatus = Nothing
    , _dcsDBClusterIdentifier = Nothing
    , _dcsMasterUsername = Nothing
    , _dcsVPCId = Nothing
    , _dcsDBClusterSnapshotIdentifier = Nothing
    , _dcsEngine = Nothing
    , _dcsLicenseModel = Nothing
    , _dcsAvailabilityZones = Nothing
    , _dcsSnapshotType = Nothing
    , _dcsSnapshotCreateTime = Nothing
    , _dcsAllocatedStorage = Nothing
    , _dcsClusterCreateTime = Nothing
    , _dcsPercentProgress = Nothing
    , _dcsPort = Nothing
    }

-- | Provides the version of the database engine for this DB cluster
-- snapshot.
dcsEngineVersion :: Lens' DBClusterSnapshot (Maybe Text)
dcsEngineVersion = lens _dcsEngineVersion (\ s a -> s{_dcsEngineVersion = a});

-- | Specifies the status of this DB cluster snapshot.
dcsStatus :: Lens' DBClusterSnapshot (Maybe Text)
dcsStatus = lens _dcsStatus (\ s a -> s{_dcsStatus = a});

-- | Specifies the DB cluster identifier of the DB cluster that this DB
-- cluster snapshot was created from.
dcsDBClusterIdentifier :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterIdentifier = lens _dcsDBClusterIdentifier (\ s a -> s{_dcsDBClusterIdentifier = a});

-- | Provides the master username for the DB cluster snapshot.
dcsMasterUsername :: Lens' DBClusterSnapshot (Maybe Text)
dcsMasterUsername = lens _dcsMasterUsername (\ s a -> s{_dcsMasterUsername = a});

-- | Provides the VPC ID associated with the DB cluster snapshot.
dcsVPCId :: Lens' DBClusterSnapshot (Maybe Text)
dcsVPCId = lens _dcsVPCId (\ s a -> s{_dcsVPCId = a});

-- | Specifies the identifier for the DB cluster snapshot.
dcsDBClusterSnapshotIdentifier :: Lens' DBClusterSnapshot (Maybe Text)
dcsDBClusterSnapshotIdentifier = lens _dcsDBClusterSnapshotIdentifier (\ s a -> s{_dcsDBClusterSnapshotIdentifier = a});

-- | Specifies the name of the database engine.
dcsEngine :: Lens' DBClusterSnapshot (Maybe Text)
dcsEngine = lens _dcsEngine (\ s a -> s{_dcsEngine = a});

-- | Provides the license model information for this DB cluster snapshot.
dcsLicenseModel :: Lens' DBClusterSnapshot (Maybe Text)
dcsLicenseModel = lens _dcsLicenseModel (\ s a -> s{_dcsLicenseModel = a});

-- | Provides the list of EC2 Availability Zones that instances in the DB
-- cluster snapshot can be restored in.
dcsAvailabilityZones :: Lens' DBClusterSnapshot [Text]
dcsAvailabilityZones = lens _dcsAvailabilityZones (\ s a -> s{_dcsAvailabilityZones = a}) . _Default . _Coerce;

-- | Provides the type of the DB cluster snapshot.
dcsSnapshotType :: Lens' DBClusterSnapshot (Maybe Text)
dcsSnapshotType = lens _dcsSnapshotType (\ s a -> s{_dcsSnapshotType = a});

-- | Provides the time when the snapshot was taken, in Universal Coordinated
-- Time (UTC).
dcsSnapshotCreateTime :: Lens' DBClusterSnapshot (Maybe UTCTime)
dcsSnapshotCreateTime = lens _dcsSnapshotCreateTime (\ s a -> s{_dcsSnapshotCreateTime = a}) . mapping _Time;

-- | Specifies the allocated storage size in gigabytes (GB).
dcsAllocatedStorage :: Lens' DBClusterSnapshot (Maybe Int)
dcsAllocatedStorage = lens _dcsAllocatedStorage (\ s a -> s{_dcsAllocatedStorage = a});

-- | Specifies the time when the DB cluster was created, in Universal
-- Coordinated Time (UTC).
dcsClusterCreateTime :: Lens' DBClusterSnapshot (Maybe UTCTime)
dcsClusterCreateTime = lens _dcsClusterCreateTime (\ s a -> s{_dcsClusterCreateTime = a}) . mapping _Time;

-- | Specifies the percentage of the estimated data that has been
-- transferred.
dcsPercentProgress :: Lens' DBClusterSnapshot (Maybe Int)
dcsPercentProgress = lens _dcsPercentProgress (\ s a -> s{_dcsPercentProgress = a});

-- | Specifies the port that the DB cluster was listening on at the time of
-- the snapshot.
dcsPort :: Lens' DBClusterSnapshot (Maybe Int)
dcsPort = lens _dcsPort (\ s a -> s{_dcsPort = a});

instance FromXML DBClusterSnapshot where
        parseXML x
          = DBClusterSnapshot' <$>
              (x .@? "EngineVersion") <*> (x .@? "Status") <*>
                (x .@? "DBClusterIdentifier")
                <*> (x .@? "MasterUsername")
                <*> (x .@? "VpcId")
                <*> (x .@? "DBClusterSnapshotIdentifier")
                <*> (x .@? "Engine")
                <*> (x .@? "LicenseModel")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "SnapshotType")
                <*> (x .@? "SnapshotCreateTime")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "ClusterCreateTime")
                <*> (x .@? "PercentProgress")
                <*> (x .@? "Port")

-- | This data type is used as a response element in the action
-- DescribeDBEngineVersions.
--
-- /See:/ 'dbEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
    { _devEngineVersion              :: !(Maybe Text)
    , _devDBEngineVersionDescription :: !(Maybe Text)
    , _devDefaultCharacterSet        :: !(Maybe CharacterSet)
    , _devEngine                     :: !(Maybe Text)
    , _devDBParameterGroupFamily     :: !(Maybe Text)
    , _devSupportedCharacterSets     :: !(Maybe [CharacterSet])
    , _devDBEngineDescription        :: !(Maybe Text)
    , _devValidUpgradeTarget         :: !(Maybe [UpgradeTarget])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBEngineVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'devEngineVersion'
--
-- * 'devDBEngineVersionDescription'
--
-- * 'devDefaultCharacterSet'
--
-- * 'devEngine'
--
-- * 'devDBParameterGroupFamily'
--
-- * 'devSupportedCharacterSets'
--
-- * 'devDBEngineDescription'
--
-- * 'devValidUpgradeTarget'
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
    }

-- | The version number of the database engine.
devEngineVersion :: Lens' DBEngineVersion (Maybe Text)
devEngineVersion = lens _devEngineVersion (\ s a -> s{_devEngineVersion = a});

-- | The description of the database engine version.
devDBEngineVersionDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineVersionDescription = lens _devDBEngineVersionDescription (\ s a -> s{_devDBEngineVersionDescription = a});

-- | The default character set for new instances of this engine version, if
-- the 'CharacterSetName' parameter of the CreateDBInstance API is not
-- specified.
devDefaultCharacterSet :: Lens' DBEngineVersion (Maybe CharacterSet)
devDefaultCharacterSet = lens _devDefaultCharacterSet (\ s a -> s{_devDefaultCharacterSet = a});

-- | The name of the database engine.
devEngine :: Lens' DBEngineVersion (Maybe Text)
devEngine = lens _devEngine (\ s a -> s{_devEngine = a});

-- | The name of the DB parameter group family for the database engine.
devDBParameterGroupFamily :: Lens' DBEngineVersion (Maybe Text)
devDBParameterGroupFamily = lens _devDBParameterGroupFamily (\ s a -> s{_devDBParameterGroupFamily = a});

-- | A list of the character sets supported by this engine for the
-- 'CharacterSetName' parameter of the CreateDBInstance API.
devSupportedCharacterSets :: Lens' DBEngineVersion [CharacterSet]
devSupportedCharacterSets = lens _devSupportedCharacterSets (\ s a -> s{_devSupportedCharacterSets = a}) . _Default . _Coerce;

-- | The description of the database engine.
devDBEngineDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineDescription = lens _devDBEngineDescription (\ s a -> s{_devDBEngineDescription = a});

-- | A list of engine versions that this database engine version can be
-- upgraded to.
devValidUpgradeTarget :: Lens' DBEngineVersion [UpgradeTarget]
devValidUpgradeTarget = lens _devValidUpgradeTarget (\ s a -> s{_devValidUpgradeTarget = a}) . _Default . _Coerce;

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

-- | Contains the result of a successful invocation of the following actions:
--
-- -   CreateDBInstance
-- -   DeleteDBInstance
-- -   ModifyDBInstance
--
-- This data type is used as a response element in the DescribeDBInstances
-- action.
--
-- /See:/ 'dbInstance' smart constructor.
data DBInstance = DBInstance'
    { _diEngineVersion                         :: !(Maybe Text)
    , _diDBSecurityGroups                      :: !(Maybe [DBSecurityGroupMembership])
    , _diStorageEncrypted                      :: !(Maybe Bool)
    , _diDBClusterIdentifier                   :: !(Maybe Text)
    , _diPubliclyAccessible                    :: !(Maybe Bool)
    , _diAutoMinorVersionUpgrade               :: !(Maybe Bool)
    , _diMasterUsername                        :: !(Maybe Text)
    , _diReadReplicaDBInstanceIdentifiers      :: !(Maybe [Text])
    , _diIOPS                                  :: !(Maybe Int)
    , _diInstanceCreateTime                    :: !(Maybe ISO8601)
    , _diReadReplicaSourceDBInstanceIdentifier :: !(Maybe Text)
    , _diEngine                                :: !(Maybe Text)
    , _diLatestRestorableTime                  :: !(Maybe ISO8601)
    , _diDBInstanceClass                       :: !(Maybe Text)
    , _diLicenseModel                          :: !(Maybe Text)
    , _diPreferredMaintenanceWindow            :: !(Maybe Text)
    , _diCACertificateIdentifier               :: !(Maybe Text)
    , _diDBInstanceIdentifier                  :: !(Maybe Text)
    , _diCharacterSetName                      :: !(Maybe Text)
    , _diKMSKeyId                              :: !(Maybe Text)
    , _diPreferredBackupWindow                 :: !(Maybe Text)
    , _diAvailabilityZone                      :: !(Maybe Text)
    , _diVPCSecurityGroups                     :: !(Maybe [VPCSecurityGroupMembership])
    , _diBackupRetentionPeriod                 :: !(Maybe Int)
    , _diDBSubnetGroup                         :: !(Maybe DBSubnetGroup)
    , _diMultiAZ                               :: !(Maybe Bool)
    , _diOptionGroupMemberships                :: !(Maybe [OptionGroupMembership])
    , _diSecondaryAvailabilityZone             :: !(Maybe Text)
    , _diAllocatedStorage                      :: !(Maybe Int)
    , _diDBiResourceId                         :: !(Maybe Text)
    , _diDBParameterGroups                     :: !(Maybe [DBParameterGroupStatus])
    , _diCopyTagsToSnapshot                    :: !(Maybe Bool)
    , _diTDECredentialARN                      :: !(Maybe Text)
    , _diEndpoint                              :: !(Maybe Endpoint)
    , _diDBInstanceStatus                      :: !(Maybe Text)
    , _diDBInstancePort                        :: !(Maybe Int)
    , _diPendingModifiedValues                 :: !(Maybe PendingModifiedValues)
    , _diStorageType                           :: !(Maybe Text)
    , _diStatusInfos                           :: !(Maybe [DBInstanceStatusInfo])
    , _diDBName                                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diEngineVersion'
--
-- * 'diDBSecurityGroups'
--
-- * 'diStorageEncrypted'
--
-- * 'diDBClusterIdentifier'
--
-- * 'diPubliclyAccessible'
--
-- * 'diAutoMinorVersionUpgrade'
--
-- * 'diMasterUsername'
--
-- * 'diReadReplicaDBInstanceIdentifiers'
--
-- * 'diIOPS'
--
-- * 'diInstanceCreateTime'
--
-- * 'diReadReplicaSourceDBInstanceIdentifier'
--
-- * 'diEngine'
--
-- * 'diLatestRestorableTime'
--
-- * 'diDBInstanceClass'
--
-- * 'diLicenseModel'
--
-- * 'diPreferredMaintenanceWindow'
--
-- * 'diCACertificateIdentifier'
--
-- * 'diDBInstanceIdentifier'
--
-- * 'diCharacterSetName'
--
-- * 'diKMSKeyId'
--
-- * 'diPreferredBackupWindow'
--
-- * 'diAvailabilityZone'
--
-- * 'diVPCSecurityGroups'
--
-- * 'diBackupRetentionPeriod'
--
-- * 'diDBSubnetGroup'
--
-- * 'diMultiAZ'
--
-- * 'diOptionGroupMemberships'
--
-- * 'diSecondaryAvailabilityZone'
--
-- * 'diAllocatedStorage'
--
-- * 'diDBiResourceId'
--
-- * 'diDBParameterGroups'
--
-- * 'diCopyTagsToSnapshot'
--
-- * 'diTDECredentialARN'
--
-- * 'diEndpoint'
--
-- * 'diDBInstanceStatus'
--
-- * 'diDBInstancePort'
--
-- * 'diPendingModifiedValues'
--
-- * 'diStorageType'
--
-- * 'diStatusInfos'
--
-- * 'diDBName'
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
    , _diMasterUsername = Nothing
    , _diReadReplicaDBInstanceIdentifiers = Nothing
    , _diIOPS = Nothing
    , _diInstanceCreateTime = Nothing
    , _diReadReplicaSourceDBInstanceIdentifier = Nothing
    , _diEngine = Nothing
    , _diLatestRestorableTime = Nothing
    , _diDBInstanceClass = Nothing
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
    , _diDBSubnetGroup = Nothing
    , _diMultiAZ = Nothing
    , _diOptionGroupMemberships = Nothing
    , _diSecondaryAvailabilityZone = Nothing
    , _diAllocatedStorage = Nothing
    , _diDBiResourceId = Nothing
    , _diDBParameterGroups = Nothing
    , _diCopyTagsToSnapshot = Nothing
    , _diTDECredentialARN = Nothing
    , _diEndpoint = Nothing
    , _diDBInstanceStatus = Nothing
    , _diDBInstancePort = Nothing
    , _diPendingModifiedValues = Nothing
    , _diStorageType = Nothing
    , _diStatusInfos = Nothing
    , _diDBName = Nothing
    }

-- | Indicates the database engine version.
diEngineVersion :: Lens' DBInstance (Maybe Text)
diEngineVersion = lens _diEngineVersion (\ s a -> s{_diEngineVersion = a});

-- | Provides List of DB security group elements containing only
-- 'DBSecurityGroup.Name' and 'DBSecurityGroup.Status' subelements.
diDBSecurityGroups :: Lens' DBInstance [DBSecurityGroupMembership]
diDBSecurityGroups = lens _diDBSecurityGroups (\ s a -> s{_diDBSecurityGroups = a}) . _Default . _Coerce;

-- | Specifies whether the DB instance is encrypted.
diStorageEncrypted :: Lens' DBInstance (Maybe Bool)
diStorageEncrypted = lens _diStorageEncrypted (\ s a -> s{_diStorageEncrypted = a});

-- | If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
diDBClusterIdentifier :: Lens' DBInstance (Maybe Text)
diDBClusterIdentifier = lens _diDBClusterIdentifier (\ s a -> s{_diDBClusterIdentifier = a});

-- | Specifies the accessibility options for the DB instance. A value of true
-- specifies an Internet-facing instance with a publicly resolvable DNS
-- name, which resolves to a public IP address. A value of false specifies
-- an internal instance with a DNS name that resolves to a private IP
-- address.
--
-- Default: The default behavior varies depending on whether a VPC has been
-- requested or not. The following list shows the default behavior in each
-- case.
--
-- -   __Default VPC:__true
-- -   __VPC:__false
--
-- If no DB subnet group has been specified as part of the request and the
-- PubliclyAccessible value has not been set, the DB instance will be
-- publicly accessible. If a specific DB subnet group has been specified as
-- part of the request and the PubliclyAccessible value has not been set,
-- the DB instance will be private.
diPubliclyAccessible :: Lens' DBInstance (Maybe Bool)
diPubliclyAccessible = lens _diPubliclyAccessible (\ s a -> s{_diPubliclyAccessible = a});

-- | Indicates that minor version patches are applied automatically.
diAutoMinorVersionUpgrade :: Lens' DBInstance (Maybe Bool)
diAutoMinorVersionUpgrade = lens _diAutoMinorVersionUpgrade (\ s a -> s{_diAutoMinorVersionUpgrade = a});

-- | Contains the master username for the DB instance.
diMasterUsername :: Lens' DBInstance (Maybe Text)
diMasterUsername = lens _diMasterUsername (\ s a -> s{_diMasterUsername = a});

-- | Contains one or more identifiers of the Read Replicas associated with
-- this DB instance.
diReadReplicaDBInstanceIdentifiers :: Lens' DBInstance [Text]
diReadReplicaDBInstanceIdentifiers = lens _diReadReplicaDBInstanceIdentifiers (\ s a -> s{_diReadReplicaDBInstanceIdentifiers = a}) . _Default . _Coerce;

-- | Specifies the Provisioned IOPS (I\/O operations per second) value.
diIOPS :: Lens' DBInstance (Maybe Int)
diIOPS = lens _diIOPS (\ s a -> s{_diIOPS = a});

-- | Provides the date and time the DB instance was created.
diInstanceCreateTime :: Lens' DBInstance (Maybe UTCTime)
diInstanceCreateTime = lens _diInstanceCreateTime (\ s a -> s{_diInstanceCreateTime = a}) . mapping _Time;

-- | Contains the identifier of the source DB instance if this DB instance is
-- a Read Replica.
diReadReplicaSourceDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
diReadReplicaSourceDBInstanceIdentifier = lens _diReadReplicaSourceDBInstanceIdentifier (\ s a -> s{_diReadReplicaSourceDBInstanceIdentifier = a});

-- | Provides the name of the database engine to be used for this DB
-- instance.
diEngine :: Lens' DBInstance (Maybe Text)
diEngine = lens _diEngine (\ s a -> s{_diEngine = a});

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
diLatestRestorableTime :: Lens' DBInstance (Maybe UTCTime)
diLatestRestorableTime = lens _diLatestRestorableTime (\ s a -> s{_diLatestRestorableTime = a}) . mapping _Time;

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
diDBInstanceClass :: Lens' DBInstance (Maybe Text)
diDBInstanceClass = lens _diDBInstanceClass (\ s a -> s{_diDBInstanceClass = a});

-- | License model information for this DB instance.
diLicenseModel :: Lens' DBInstance (Maybe Text)
diLicenseModel = lens _diLicenseModel (\ s a -> s{_diLicenseModel = a});

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
diPreferredMaintenanceWindow :: Lens' DBInstance (Maybe Text)
diPreferredMaintenanceWindow = lens _diPreferredMaintenanceWindow (\ s a -> s{_diPreferredMaintenanceWindow = a});

-- | The identifier of the CA certificate for this DB instance.
diCACertificateIdentifier :: Lens' DBInstance (Maybe Text)
diCACertificateIdentifier = lens _diCACertificateIdentifier (\ s a -> s{_diCACertificateIdentifier = a});

-- | Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
diDBInstanceIdentifier :: Lens' DBInstance (Maybe Text)
diDBInstanceIdentifier = lens _diDBInstanceIdentifier (\ s a -> s{_diDBInstanceIdentifier = a});

-- | If present, specifies the name of the character set that this instance
-- is associated with.
diCharacterSetName :: Lens' DBInstance (Maybe Text)
diCharacterSetName = lens _diCharacterSetName (\ s a -> s{_diCharacterSetName = a});

-- | If 'StorageEncrypted' is true, the KMS key identifier for the encrypted
-- DB instance.
diKMSKeyId :: Lens' DBInstance (Maybe Text)
diKMSKeyId = lens _diKMSKeyId (\ s a -> s{_diKMSKeyId = a});

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- 'BackupRetentionPeriod'.
diPreferredBackupWindow :: Lens' DBInstance (Maybe Text)
diPreferredBackupWindow = lens _diPreferredBackupWindow (\ s a -> s{_diPreferredBackupWindow = a});

-- | Specifies the name of the Availability Zone the DB instance is located
-- in.
diAvailabilityZone :: Lens' DBInstance (Maybe Text)
diAvailabilityZone = lens _diAvailabilityZone (\ s a -> s{_diAvailabilityZone = a});

-- | Provides List of VPC security group elements that the DB instance
-- belongs to.
diVPCSecurityGroups :: Lens' DBInstance [VPCSecurityGroupMembership]
diVPCSecurityGroups = lens _diVPCSecurityGroups (\ s a -> s{_diVPCSecurityGroups = a}) . _Default . _Coerce;

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
diBackupRetentionPeriod :: Lens' DBInstance (Maybe Int)
diBackupRetentionPeriod = lens _diBackupRetentionPeriod (\ s a -> s{_diBackupRetentionPeriod = a});

-- | Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
diDBSubnetGroup :: Lens' DBInstance (Maybe DBSubnetGroup)
diDBSubnetGroup = lens _diDBSubnetGroup (\ s a -> s{_diDBSubnetGroup = a});

-- | Specifies if the DB instance is a Multi-AZ deployment.
diMultiAZ :: Lens' DBInstance (Maybe Bool)
diMultiAZ = lens _diMultiAZ (\ s a -> s{_diMultiAZ = a});

-- | Provides the list of option group memberships for this DB instance.
diOptionGroupMemberships :: Lens' DBInstance [OptionGroupMembership]
diOptionGroupMemberships = lens _diOptionGroupMemberships (\ s a -> s{_diOptionGroupMemberships = a}) . _Default . _Coerce;

-- | If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
diSecondaryAvailabilityZone :: Lens' DBInstance (Maybe Text)
diSecondaryAvailabilityZone = lens _diSecondaryAvailabilityZone (\ s a -> s{_diSecondaryAvailabilityZone = a});

-- | Specifies the allocated storage size specified in gigabytes.
diAllocatedStorage :: Lens' DBInstance (Maybe Int)
diAllocatedStorage = lens _diAllocatedStorage (\ s a -> s{_diAllocatedStorage = a});

-- | If 'StorageEncrypted' is true, the region-unique, immutable identifier
-- for the encrypted DB instance. This identifier is found in AWS
-- CloudTrail log entries whenever the KMS key for the DB instance is
-- accessed.
diDBiResourceId :: Lens' DBInstance (Maybe Text)
diDBiResourceId = lens _diDBiResourceId (\ s a -> s{_diDBiResourceId = a});

-- | Provides the list of DB parameter groups applied to this DB instance.
diDBParameterGroups :: Lens' DBInstance [DBParameterGroupStatus]
diDBParameterGroups = lens _diDBParameterGroups (\ s a -> s{_diDBParameterGroups = a}) . _Default . _Coerce;

-- | Specifies whether tags are copied from the DB instance to snapshots of
-- the DB instance.
diCopyTagsToSnapshot :: Lens' DBInstance (Maybe Bool)
diCopyTagsToSnapshot = lens _diCopyTagsToSnapshot (\ s a -> s{_diCopyTagsToSnapshot = a});

-- | The ARN from the Key Store with which the instance is associated for TDE
-- encryption.
diTDECredentialARN :: Lens' DBInstance (Maybe Text)
diTDECredentialARN = lens _diTDECredentialARN (\ s a -> s{_diTDECredentialARN = a});

-- | Specifies the connection endpoint.
diEndpoint :: Lens' DBInstance (Maybe Endpoint)
diEndpoint = lens _diEndpoint (\ s a -> s{_diEndpoint = a});

-- | Specifies the current state of this database.
diDBInstanceStatus :: Lens' DBInstance (Maybe Text)
diDBInstanceStatus = lens _diDBInstanceStatus (\ s a -> s{_diDBInstanceStatus = a});

-- | Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
diDBInstancePort :: Lens' DBInstance (Maybe Int)
diDBInstancePort = lens _diDBInstancePort (\ s a -> s{_diDBInstancePort = a});

-- | Specifies that changes to the DB instance are pending. This element is
-- only included when changes are pending. Specific changes are identified
-- by subelements.
diPendingModifiedValues :: Lens' DBInstance (Maybe PendingModifiedValues)
diPendingModifiedValues = lens _diPendingModifiedValues (\ s a -> s{_diPendingModifiedValues = a});

-- | Specifies the storage type associated with DB instance.
diStorageType :: Lens' DBInstance (Maybe Text)
diStorageType = lens _diStorageType (\ s a -> s{_diStorageType = a});

-- | The status of a Read Replica. If the instance is not a Read Replica,
-- this will be blank.
diStatusInfos :: Lens' DBInstance [DBInstanceStatusInfo]
diStatusInfos = lens _diStatusInfos (\ s a -> s{_diStatusInfos = a}) . _Default . _Coerce;

-- | The meaning of this parameter differs according to the database engine
-- you use. For example, this value returns MySQL, MariaDB, or PostgreSQL
-- information when returning values from CreateDBInstanceReadReplica since
-- Read Replicas are only supported for these engines.
--
-- __MySQL, MariaDB, SQL Server, PostgreSQL, Amazon Aurora__
--
-- Contains the name of the initial database of this instance that was
-- provided at create time, if one was specified when the DB instance was
-- created. This same name is returned for the life of the DB instance.
--
-- Type: String
--
-- __Oracle__
--
-- Contains the Oracle System ID (SID) of the created DB instance. Not
-- shown when the returned parameters do not apply to an Oracle DB
-- instance.
diDBName :: Lens' DBInstance (Maybe Text)
diDBName = lens _diDBName (\ s a -> s{_diDBName = a});

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
                <*> (x .@? "MasterUsername")
                <*>
                (x .@? "ReadReplicaDBInstanceIdentifiers" .!@ mempty
                   >>=
                   may (parseXMLList "ReadReplicaDBInstanceIdentifier"))
                <*> (x .@? "Iops")
                <*> (x .@? "InstanceCreateTime")
                <*> (x .@? "ReadReplicaSourceDBInstanceIdentifier")
                <*> (x .@? "Engine")
                <*> (x .@? "LatestRestorableTime")
                <*> (x .@? "DBInstanceClass")
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
                <*> (x .@? "DBSubnetGroup")
                <*> (x .@? "MultiAZ")
                <*>
                (x .@? "OptionGroupMemberships" .!@ mempty >>=
                   may (parseXMLList "OptionGroupMembership"))
                <*> (x .@? "SecondaryAvailabilityZone")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "DbiResourceId")
                <*>
                (x .@? "DBParameterGroups" .!@ mempty >>=
                   may (parseXMLList "DBParameterGroup"))
                <*> (x .@? "CopyTagsToSnapshot")
                <*> (x .@? "TdeCredentialArn")
                <*> (x .@? "Endpoint")
                <*> (x .@? "DBInstanceStatus")
                <*> (x .@? "DbInstancePort")
                <*> (x .@? "PendingModifiedValues")
                <*> (x .@? "StorageType")
                <*>
                (x .@? "StatusInfos" .!@ mempty >>=
                   may (parseXMLList "DBInstanceStatusInfo"))
                <*> (x .@? "DBName")

-- | Provides a list of status information for a DB instance.
--
-- /See:/ 'dbInstanceStatusInfo' smart constructor.
data DBInstanceStatusInfo = DBInstanceStatusInfo'
    { _disiStatus     :: !(Maybe Text)
    , _disiNormal     :: !(Maybe Bool)
    , _disiStatusType :: !(Maybe Text)
    , _disiMessage    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBInstanceStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disiStatus'
--
-- * 'disiNormal'
--
-- * 'disiStatusType'
--
-- * 'disiMessage'
dbInstanceStatusInfo
    :: DBInstanceStatusInfo
dbInstanceStatusInfo =
    DBInstanceStatusInfo'
    { _disiStatus = Nothing
    , _disiNormal = Nothing
    , _disiStatusType = Nothing
    , _disiMessage = Nothing
    }

-- | Status of the DB instance. For a StatusType of read replica, the values
-- can be replicating, error, stopped, or terminated.
disiStatus :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatus = lens _disiStatus (\ s a -> s{_disiStatus = a});

-- | Boolean value that is true if the instance is operating normally, or
-- false if the instance is in an error state.
disiNormal :: Lens' DBInstanceStatusInfo (Maybe Bool)
disiNormal = lens _disiNormal (\ s a -> s{_disiNormal = a});

-- | This value is currently \"read replication.\"
disiStatusType :: Lens' DBInstanceStatusInfo (Maybe Text)
disiStatusType = lens _disiStatusType (\ s a -> s{_disiStatusType = a});

-- | Details of the error if there is an error for the instance. If the
-- instance is not in an error state, this value is blank.
disiMessage :: Lens' DBInstanceStatusInfo (Maybe Text)
disiMessage = lens _disiMessage (\ s a -> s{_disiMessage = a});

instance FromXML DBInstanceStatusInfo where
        parseXML x
          = DBInstanceStatusInfo' <$>
              (x .@? "Status") <*> (x .@? "Normal") <*>
                (x .@? "StatusType")
                <*> (x .@? "Message")

-- | Contains the result of a successful invocation of the
-- CreateDBParameterGroup action.
--
-- This data type is used as a request parameter in the
-- DeleteDBParameterGroup action, and as a response element in the
-- DescribeDBParameterGroups action.
--
-- /See:/ 'dbParameterGroup' smart constructor.
data DBParameterGroup = DBParameterGroup'
    { _dpgDBParameterGroupFamily :: !(Maybe Text)
    , _dpgDBParameterGroupName   :: !(Maybe Text)
    , _dpgDescription            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgDBParameterGroupFamily'
--
-- * 'dpgDBParameterGroupName'
--
-- * 'dpgDescription'
dbParameterGroup
    :: DBParameterGroup
dbParameterGroup =
    DBParameterGroup'
    { _dpgDBParameterGroupFamily = Nothing
    , _dpgDBParameterGroupName = Nothing
    , _dpgDescription = Nothing
    }

-- | Provides the name of the DB parameter group family that this DB
-- parameter group is compatible with.
dpgDBParameterGroupFamily :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupFamily = lens _dpgDBParameterGroupFamily (\ s a -> s{_dpgDBParameterGroupFamily = a});

-- | Provides the name of the DB parameter group.
dpgDBParameterGroupName :: Lens' DBParameterGroup (Maybe Text)
dpgDBParameterGroupName = lens _dpgDBParameterGroupName (\ s a -> s{_dpgDBParameterGroupName = a});

-- | Provides the customer-specified description for this DB parameter group.
dpgDescription :: Lens' DBParameterGroup (Maybe Text)
dpgDescription = lens _dpgDescription (\ s a -> s{_dpgDescription = a});

instance FromXML DBParameterGroup where
        parseXML x
          = DBParameterGroup' <$>
              (x .@? "DBParameterGroupFamily") <*>
                (x .@? "DBParameterGroupName")
                <*> (x .@? "Description")

-- | Contains the result of a successful invocation of the
-- ModifyDBParameterGroup or ResetDBParameterGroup action.
--
-- /See:/ 'dbParameterGroupNameMessage' smart constructor.
newtype DBParameterGroupNameMessage = DBParameterGroupNameMessage'
    { _dpgnmDBParameterGroupName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgnmDBParameterGroupName'
dbParameterGroupNameMessage
    :: DBParameterGroupNameMessage
dbParameterGroupNameMessage =
    DBParameterGroupNameMessage'
    { _dpgnmDBParameterGroupName = Nothing
    }

-- | Provides the name of the DB parameter group.
dpgnmDBParameterGroupName :: Lens' DBParameterGroupNameMessage (Maybe Text)
dpgnmDBParameterGroupName = lens _dpgnmDBParameterGroupName (\ s a -> s{_dpgnmDBParameterGroupName = a});

instance FromXML DBParameterGroupNameMessage where
        parseXML x
          = DBParameterGroupNameMessage' <$>
              (x .@? "DBParameterGroupName")

-- | The status of the DB parameter group.
--
-- This data type is used as a response element in the following actions:
--
-- -   CreateDBInstance
-- -   CreateDBInstanceReadReplica
-- -   DeleteDBInstance
-- -   ModifyDBInstance
-- -   RebootDBInstance
-- -   RestoreDBInstanceFromDBSnapshot
--
-- /See:/ 'dbParameterGroupStatus' smart constructor.
data DBParameterGroupStatus = DBParameterGroupStatus'
    { _dpgsDBParameterGroupName :: !(Maybe Text)
    , _dpgsParameterApplyStatus :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBParameterGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgsDBParameterGroupName'
--
-- * 'dpgsParameterApplyStatus'
dbParameterGroupStatus
    :: DBParameterGroupStatus
dbParameterGroupStatus =
    DBParameterGroupStatus'
    { _dpgsDBParameterGroupName = Nothing
    , _dpgsParameterApplyStatus = Nothing
    }

-- | The name of the DP parameter group.
dpgsDBParameterGroupName :: Lens' DBParameterGroupStatus (Maybe Text)
dpgsDBParameterGroupName = lens _dpgsDBParameterGroupName (\ s a -> s{_dpgsDBParameterGroupName = a});

-- | The status of parameter updates.
dpgsParameterApplyStatus :: Lens' DBParameterGroupStatus (Maybe Text)
dpgsParameterApplyStatus = lens _dpgsParameterApplyStatus (\ s a -> s{_dpgsParameterApplyStatus = a});

instance FromXML DBParameterGroupStatus where
        parseXML x
          = DBParameterGroupStatus' <$>
              (x .@? "DBParameterGroupName") <*>
                (x .@? "ParameterApplyStatus")

-- | Contains the result of a successful invocation of the following actions:
--
-- -   DescribeDBSecurityGroups
-- -   AuthorizeDBSecurityGroupIngress
-- -   CreateDBSecurityGroup
-- -   RevokeDBSecurityGroupIngress
--
-- This data type is used as a response element in the
-- DescribeDBSecurityGroups action.
--
-- /See:/ 'dbSecurityGroup' smart constructor.
data DBSecurityGroup = DBSecurityGroup'
    { _dbsgVPCId                      :: !(Maybe Text)
    , _dbsgOwnerId                    :: !(Maybe Text)
    , _dbsgIPRanges                   :: !(Maybe [IPRange])
    , _dbsgDBSecurityGroupName        :: !(Maybe Text)
    , _dbsgEC2SecurityGroups          :: !(Maybe [EC2SecurityGroup])
    , _dbsgDBSecurityGroupDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbsgVPCId'
--
-- * 'dbsgOwnerId'
--
-- * 'dbsgIPRanges'
--
-- * 'dbsgDBSecurityGroupName'
--
-- * 'dbsgEC2SecurityGroups'
--
-- * 'dbsgDBSecurityGroupDescription'
dbSecurityGroup
    :: DBSecurityGroup
dbSecurityGroup =
    DBSecurityGroup'
    { _dbsgVPCId = Nothing
    , _dbsgOwnerId = Nothing
    , _dbsgIPRanges = Nothing
    , _dbsgDBSecurityGroupName = Nothing
    , _dbsgEC2SecurityGroups = Nothing
    , _dbsgDBSecurityGroupDescription = Nothing
    }

-- | Provides the VpcId of the DB security group.
dbsgVPCId :: Lens' DBSecurityGroup (Maybe Text)
dbsgVPCId = lens _dbsgVPCId (\ s a -> s{_dbsgVPCId = a});

-- | Provides the AWS ID of the owner of a specific DB security group.
dbsgOwnerId :: Lens' DBSecurityGroup (Maybe Text)
dbsgOwnerId = lens _dbsgOwnerId (\ s a -> s{_dbsgOwnerId = a});

-- | Contains a list of IPRange elements.
dbsgIPRanges :: Lens' DBSecurityGroup [IPRange]
dbsgIPRanges = lens _dbsgIPRanges (\ s a -> s{_dbsgIPRanges = a}) . _Default . _Coerce;

-- | Specifies the name of the DB security group.
dbsgDBSecurityGroupName :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupName = lens _dbsgDBSecurityGroupName (\ s a -> s{_dbsgDBSecurityGroupName = a});

-- | Contains a list of EC2SecurityGroup elements.
dbsgEC2SecurityGroups :: Lens' DBSecurityGroup [EC2SecurityGroup]
dbsgEC2SecurityGroups = lens _dbsgEC2SecurityGroups (\ s a -> s{_dbsgEC2SecurityGroups = a}) . _Default . _Coerce;

-- | Provides the description of the DB security group.
dbsgDBSecurityGroupDescription :: Lens' DBSecurityGroup (Maybe Text)
dbsgDBSecurityGroupDescription = lens _dbsgDBSecurityGroupDescription (\ s a -> s{_dbsgDBSecurityGroupDescription = a});

instance FromXML DBSecurityGroup where
        parseXML x
          = DBSecurityGroup' <$>
              (x .@? "VpcId") <*> (x .@? "OwnerId") <*>
                (x .@? "IPRanges" .!@ mempty >>=
                   may (parseXMLList "IPRange"))
                <*> (x .@? "DBSecurityGroupName")
                <*>
                (x .@? "EC2SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "EC2SecurityGroup"))
                <*> (x .@? "DBSecurityGroupDescription")

-- | This data type is used as a response element in the following actions:
--
-- -   ModifyDBInstance
-- -   RebootDBInstance
-- -   RestoreDBInstanceFromDBSnapshot
-- -   RestoreDBInstanceToPointInTime
--
-- /See:/ 'dbSecurityGroupMembership' smart constructor.
data DBSecurityGroupMembership = DBSecurityGroupMembership'
    { _dsgmStatus              :: !(Maybe Text)
    , _dsgmDBSecurityGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgmStatus'
--
-- * 'dsgmDBSecurityGroupName'
dbSecurityGroupMembership
    :: DBSecurityGroupMembership
dbSecurityGroupMembership =
    DBSecurityGroupMembership'
    { _dsgmStatus = Nothing
    , _dsgmDBSecurityGroupName = Nothing
    }

-- | The status of the DB security group.
dsgmStatus :: Lens' DBSecurityGroupMembership (Maybe Text)
dsgmStatus = lens _dsgmStatus (\ s a -> s{_dsgmStatus = a});

-- | The name of the DB security group.
dsgmDBSecurityGroupName :: Lens' DBSecurityGroupMembership (Maybe Text)
dsgmDBSecurityGroupName = lens _dsgmDBSecurityGroupName (\ s a -> s{_dsgmDBSecurityGroupName = a});

instance FromXML DBSecurityGroupMembership where
        parseXML x
          = DBSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "DBSecurityGroupName")

-- | Contains the result of a successful invocation of the following actions:
--
-- -   CreateDBSnapshot
-- -   DeleteDBSnapshot
--
-- This data type is used as a response element in the DescribeDBSnapshots
-- action.
--
-- /See:/ 'dbSnapshot' smart constructor.
data DBSnapshot = DBSnapshot'
    { _dsEngineVersion              :: !(Maybe Text)
    , _dsStatus                     :: !(Maybe Text)
    , _dsMasterUsername             :: !(Maybe Text)
    , _dsSourceRegion               :: !(Maybe Text)
    , _dsIOPS                       :: !(Maybe Int)
    , _dsVPCId                      :: !(Maybe Text)
    , _dsInstanceCreateTime         :: !(Maybe ISO8601)
    , _dsEngine                     :: !(Maybe Text)
    , _dsEncrypted                  :: !(Maybe Bool)
    , _dsDBSnapshotIdentifier       :: !(Maybe Text)
    , _dsLicenseModel               :: !(Maybe Text)
    , _dsSourceDBSnapshotIdentifier :: !(Maybe Text)
    , _dsSnapshotType               :: !(Maybe Text)
    , _dsDBInstanceIdentifier       :: !(Maybe Text)
    , _dsKMSKeyId                   :: !(Maybe Text)
    , _dsAvailabilityZone           :: !(Maybe Text)
    , _dsSnapshotCreateTime         :: !(Maybe ISO8601)
    , _dsAllocatedStorage           :: !(Maybe Int)
    , _dsOptionGroupName            :: !(Maybe Text)
    , _dsTDECredentialARN           :: !(Maybe Text)
    , _dsPercentProgress            :: !(Maybe Int)
    , _dsPort                       :: !(Maybe Int)
    , _dsStorageType                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsEngineVersion'
--
-- * 'dsStatus'
--
-- * 'dsMasterUsername'
--
-- * 'dsSourceRegion'
--
-- * 'dsIOPS'
--
-- * 'dsVPCId'
--
-- * 'dsInstanceCreateTime'
--
-- * 'dsEngine'
--
-- * 'dsEncrypted'
--
-- * 'dsDBSnapshotIdentifier'
--
-- * 'dsLicenseModel'
--
-- * 'dsSourceDBSnapshotIdentifier'
--
-- * 'dsSnapshotType'
--
-- * 'dsDBInstanceIdentifier'
--
-- * 'dsKMSKeyId'
--
-- * 'dsAvailabilityZone'
--
-- * 'dsSnapshotCreateTime'
--
-- * 'dsAllocatedStorage'
--
-- * 'dsOptionGroupName'
--
-- * 'dsTDECredentialARN'
--
-- * 'dsPercentProgress'
--
-- * 'dsPort'
--
-- * 'dsStorageType'
dbSnapshot
    :: DBSnapshot
dbSnapshot =
    DBSnapshot'
    { _dsEngineVersion = Nothing
    , _dsStatus = Nothing
    , _dsMasterUsername = Nothing
    , _dsSourceRegion = Nothing
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
    , _dsTDECredentialARN = Nothing
    , _dsPercentProgress = Nothing
    , _dsPort = Nothing
    , _dsStorageType = Nothing
    }

-- | Specifies the version of the database engine.
dsEngineVersion :: Lens' DBSnapshot (Maybe Text)
dsEngineVersion = lens _dsEngineVersion (\ s a -> s{_dsEngineVersion = a});

-- | Specifies the status of this DB snapshot.
dsStatus :: Lens' DBSnapshot (Maybe Text)
dsStatus = lens _dsStatus (\ s a -> s{_dsStatus = a});

-- | Provides the master username for the DB snapshot.
dsMasterUsername :: Lens' DBSnapshot (Maybe Text)
dsMasterUsername = lens _dsMasterUsername (\ s a -> s{_dsMasterUsername = a});

-- | The region that the DB snapshot was created in or copied from.
dsSourceRegion :: Lens' DBSnapshot (Maybe Text)
dsSourceRegion = lens _dsSourceRegion (\ s a -> s{_dsSourceRegion = a});

-- | Specifies the Provisioned IOPS (I\/O operations per second) value of the
-- DB instance at the time of the snapshot.
dsIOPS :: Lens' DBSnapshot (Maybe Int)
dsIOPS = lens _dsIOPS (\ s a -> s{_dsIOPS = a});

-- | Provides the VPC ID associated with the DB snapshot.
dsVPCId :: Lens' DBSnapshot (Maybe Text)
dsVPCId = lens _dsVPCId (\ s a -> s{_dsVPCId = a});

-- | Specifies the time when the snapshot was taken, in Universal Coordinated
-- Time (UTC).
dsInstanceCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dsInstanceCreateTime = lens _dsInstanceCreateTime (\ s a -> s{_dsInstanceCreateTime = a}) . mapping _Time;

-- | Specifies the name of the database engine.
dsEngine :: Lens' DBSnapshot (Maybe Text)
dsEngine = lens _dsEngine (\ s a -> s{_dsEngine = a});

-- | Specifies whether the DB snapshot is encrypted.
dsEncrypted :: Lens' DBSnapshot (Maybe Bool)
dsEncrypted = lens _dsEncrypted (\ s a -> s{_dsEncrypted = a});

-- | Specifies the identifier for the DB snapshot.
dsDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dsDBSnapshotIdentifier = lens _dsDBSnapshotIdentifier (\ s a -> s{_dsDBSnapshotIdentifier = a});

-- | License model information for the restored DB instance.
dsLicenseModel :: Lens' DBSnapshot (Maybe Text)
dsLicenseModel = lens _dsLicenseModel (\ s a -> s{_dsLicenseModel = a});

-- | The DB snapshot Arn that the DB snapshot was copied from. It only has
-- value in case of cross customer or cross region copy.
dsSourceDBSnapshotIdentifier :: Lens' DBSnapshot (Maybe Text)
dsSourceDBSnapshotIdentifier = lens _dsSourceDBSnapshotIdentifier (\ s a -> s{_dsSourceDBSnapshotIdentifier = a});

-- | Provides the type of the DB snapshot.
dsSnapshotType :: Lens' DBSnapshot (Maybe Text)
dsSnapshotType = lens _dsSnapshotType (\ s a -> s{_dsSnapshotType = a});

-- | Specifies the DB instance identifier of the DB instance this DB snapshot
-- was created from.
dsDBInstanceIdentifier :: Lens' DBSnapshot (Maybe Text)
dsDBInstanceIdentifier = lens _dsDBInstanceIdentifier (\ s a -> s{_dsDBInstanceIdentifier = a});

-- | If 'Encrypted' is true, the KMS key identifier for the encrypted DB
-- snapshot.
dsKMSKeyId :: Lens' DBSnapshot (Maybe Text)
dsKMSKeyId = lens _dsKMSKeyId (\ s a -> s{_dsKMSKeyId = a});

-- | Specifies the name of the Availability Zone the DB instance was located
-- in at the time of the DB snapshot.
dsAvailabilityZone :: Lens' DBSnapshot (Maybe Text)
dsAvailabilityZone = lens _dsAvailabilityZone (\ s a -> s{_dsAvailabilityZone = a});

-- | Provides the time when the snapshot was taken, in Universal Coordinated
-- Time (UTC).
dsSnapshotCreateTime :: Lens' DBSnapshot (Maybe UTCTime)
dsSnapshotCreateTime = lens _dsSnapshotCreateTime (\ s a -> s{_dsSnapshotCreateTime = a}) . mapping _Time;

-- | Specifies the allocated storage size in gigabytes (GB).
dsAllocatedStorage :: Lens' DBSnapshot (Maybe Int)
dsAllocatedStorage = lens _dsAllocatedStorage (\ s a -> s{_dsAllocatedStorage = a});

-- | Provides the option group name for the DB snapshot.
dsOptionGroupName :: Lens' DBSnapshot (Maybe Text)
dsOptionGroupName = lens _dsOptionGroupName (\ s a -> s{_dsOptionGroupName = a});

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
dsTDECredentialARN :: Lens' DBSnapshot (Maybe Text)
dsTDECredentialARN = lens _dsTDECredentialARN (\ s a -> s{_dsTDECredentialARN = a});

-- | The percentage of the estimated data that has been transferred.
dsPercentProgress :: Lens' DBSnapshot (Maybe Int)
dsPercentProgress = lens _dsPercentProgress (\ s a -> s{_dsPercentProgress = a});

-- | Specifies the port that the database engine was listening on at the time
-- of the snapshot.
dsPort :: Lens' DBSnapshot (Maybe Int)
dsPort = lens _dsPort (\ s a -> s{_dsPort = a});

-- | Specifies the storage type associated with DB Snapshot.
dsStorageType :: Lens' DBSnapshot (Maybe Text)
dsStorageType = lens _dsStorageType (\ s a -> s{_dsStorageType = a});

instance FromXML DBSnapshot where
        parseXML x
          = DBSnapshot' <$>
              (x .@? "EngineVersion") <*> (x .@? "Status") <*>
                (x .@? "MasterUsername")
                <*> (x .@? "SourceRegion")
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
                <*> (x .@? "TdeCredentialArn")
                <*> (x .@? "PercentProgress")
                <*> (x .@? "Port")
                <*> (x .@? "StorageType")

-- | Contains the name and values of a manual DB snapshot attribute
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts
-- to restore a manual DB snapshot. For more information, see the
-- ModifyDBSnapshotAttribute API.
--
-- /See:/ 'dbSnapshotAttribute' smart constructor.
data DBSnapshotAttribute = DBSnapshotAttribute'
    { _dsaAttributeValues :: !(Maybe [Text])
    , _dsaAttributeName   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBSnapshotAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaAttributeValues'
--
-- * 'dsaAttributeName'
dbSnapshotAttribute
    :: DBSnapshotAttribute
dbSnapshotAttribute =
    DBSnapshotAttribute'
    { _dsaAttributeValues = Nothing
    , _dsaAttributeName = Nothing
    }

-- | The value(s) for the manual DB snapshot attribute.
--
-- If the 'AttributeName' field is 'restore', then this field returns a
-- list of AWS account ids that are authorized to copy or restore the
-- manual DB snapshot. If a value of 'all' is in the list, then the manual
-- DB snapshot is public and available for any AWS account to copy or
-- restore.
dsaAttributeValues :: Lens' DBSnapshotAttribute [Text]
dsaAttributeValues = lens _dsaAttributeValues (\ s a -> s{_dsaAttributeValues = a}) . _Default . _Coerce;

-- | The name of the manual DB snapshot attribute.
--
-- An attribute name of 'restore' applies to the list of AWS accounts that
-- have permission to copy or restore the manual DB snapshot.
dsaAttributeName :: Lens' DBSnapshotAttribute (Maybe Text)
dsaAttributeName = lens _dsaAttributeName (\ s a -> s{_dsaAttributeName = a});

instance FromXML DBSnapshotAttribute where
        parseXML x
          = DBSnapshotAttribute' <$>
              (x .@? "AttributeValues" .!@ mempty >>=
                 may (parseXMLList "AttributeValue"))
                <*> (x .@? "AttributeName")

-- | Contains the results of a successful call to the
-- DescribeDBSnapshotAttributes API.
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts
-- to copy or restore a manual DB snapshot. For more information, see the
-- ModifyDBSnapshotAttribute API.
--
-- /See:/ 'dbSnapshotAttributesResult' smart constructor.
data DBSnapshotAttributesResult = DBSnapshotAttributesResult'
    { _dsarDBSnapshotIdentifier :: !(Maybe Text)
    , _dsarDBSnapshotAttributes :: !(Maybe [DBSnapshotAttribute])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBSnapshotAttributesResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarDBSnapshotIdentifier'
--
-- * 'dsarDBSnapshotAttributes'
dbSnapshotAttributesResult
    :: DBSnapshotAttributesResult
dbSnapshotAttributesResult =
    DBSnapshotAttributesResult'
    { _dsarDBSnapshotIdentifier = Nothing
    , _dsarDBSnapshotAttributes = Nothing
    }

-- | The identifier of the manual DB snapshot that the attributes apply to.
dsarDBSnapshotIdentifier :: Lens' DBSnapshotAttributesResult (Maybe Text)
dsarDBSnapshotIdentifier = lens _dsarDBSnapshotIdentifier (\ s a -> s{_dsarDBSnapshotIdentifier = a});

-- | The list of attributes and values for the manual DB snapshot.
dsarDBSnapshotAttributes :: Lens' DBSnapshotAttributesResult [DBSnapshotAttribute]
dsarDBSnapshotAttributes = lens _dsarDBSnapshotAttributes (\ s a -> s{_dsarDBSnapshotAttributes = a}) . _Default . _Coerce;

instance FromXML DBSnapshotAttributesResult where
        parseXML x
          = DBSnapshotAttributesResult' <$>
              (x .@? "DBSnapshotIdentifier") <*>
                (x .@? "DBSnapshotAttributes" .!@ mempty >>=
                   may (parseXMLList "DBSnapshotAttribute"))

-- | Contains the result of a successful invocation of the following actions:
--
-- -   CreateDBSubnetGroup
-- -   ModifyDBSubnetGroup
-- -   DescribeDBSubnetGroups
-- -   DeleteDBSubnetGroup
--
-- This data type is used as a response element in the
-- DescribeDBSubnetGroups action.
--
-- /See:/ 'dbSubnetGroup' smart constructor.
data DBSubnetGroup = DBSubnetGroup'
    { _dsgDBSubnetGroupName        :: !(Maybe Text)
    , _dsgVPCId                    :: !(Maybe Text)
    , _dsgSubnets                  :: !(Maybe [Subnet])
    , _dsgDBSubnetGroupDescription :: !(Maybe Text)
    , _dsgSubnetGroupStatus        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DBSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgDBSubnetGroupName'
--
-- * 'dsgVPCId'
--
-- * 'dsgSubnets'
--
-- * 'dsgDBSubnetGroupDescription'
--
-- * 'dsgSubnetGroupStatus'
dbSubnetGroup
    :: DBSubnetGroup
dbSubnetGroup =
    DBSubnetGroup'
    { _dsgDBSubnetGroupName = Nothing
    , _dsgVPCId = Nothing
    , _dsgSubnets = Nothing
    , _dsgDBSubnetGroupDescription = Nothing
    , _dsgSubnetGroupStatus = Nothing
    }

-- | Specifies the name of the DB subnet group.
dsgDBSubnetGroupName :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupName = lens _dsgDBSubnetGroupName (\ s a -> s{_dsgDBSubnetGroupName = a});

-- | Provides the VpcId of the DB subnet group.
dsgVPCId :: Lens' DBSubnetGroup (Maybe Text)
dsgVPCId = lens _dsgVPCId (\ s a -> s{_dsgVPCId = a});

-- | Contains a list of Subnet elements.
dsgSubnets :: Lens' DBSubnetGroup [Subnet]
dsgSubnets = lens _dsgSubnets (\ s a -> s{_dsgSubnets = a}) . _Default . _Coerce;

-- | Provides the description of the DB subnet group.
dsgDBSubnetGroupDescription :: Lens' DBSubnetGroup (Maybe Text)
dsgDBSubnetGroupDescription = lens _dsgDBSubnetGroupDescription (\ s a -> s{_dsgDBSubnetGroupDescription = a});

-- | Provides the status of the DB subnet group.
dsgSubnetGroupStatus :: Lens' DBSubnetGroup (Maybe Text)
dsgSubnetGroupStatus = lens _dsgSubnetGroupStatus (\ s a -> s{_dsgSubnetGroupStatus = a});

instance FromXML DBSubnetGroup where
        parseXML x
          = DBSubnetGroup' <$>
              (x .@? "DBSubnetGroupName") <*> (x .@? "VpcId") <*>
                (x .@? "Subnets" .!@ mempty >>=
                   may (parseXMLList "Subnet"))
                <*> (x .@? "DBSubnetGroupDescription")
                <*> (x .@? "SubnetGroupStatus")

-- | This data type is used as a response element to DescribeDBLogFiles.
--
-- /See:/ 'describeDBLogFilesDetails' smart constructor.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails'
    { _ddlfdLastWritten :: !(Maybe Integer)
    , _ddlfdSize        :: !(Maybe Integer)
    , _ddlfdLogFileName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBLogFilesDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddlfdLastWritten'
--
-- * 'ddlfdSize'
--
-- * 'ddlfdLogFileName'
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
ddlfdLastWritten = lens _ddlfdLastWritten (\ s a -> s{_ddlfdLastWritten = a});

-- | The size, in bytes, of the log file for the specified DB instance.
ddlfdSize :: Lens' DescribeDBLogFilesDetails (Maybe Integer)
ddlfdSize = lens _ddlfdSize (\ s a -> s{_ddlfdSize = a});

-- | The name of the log file for the specified DB instance.
ddlfdLogFileName :: Lens' DescribeDBLogFilesDetails (Maybe Text)
ddlfdLogFileName = lens _ddlfdLogFileName (\ s a -> s{_ddlfdLogFileName = a});

instance FromXML DescribeDBLogFilesDetails where
        parseXML x
          = DescribeDBLogFilesDetails' <$>
              (x .@? "LastWritten") <*> (x .@? "Size") <*>
                (x .@? "LogFileName")

-- | This data type is used as a response element in the following actions:
--
-- -   AuthorizeDBSecurityGroupIngress
-- -   DescribeDBSecurityGroups
-- -   RevokeDBSecurityGroupIngress
--
-- /See:/ 'ec2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
    { _esgStatus                  :: !(Maybe Text)
    , _esgEC2SecurityGroupOwnerId :: !(Maybe Text)
    , _esgEC2SecurityGroupName    :: !(Maybe Text)
    , _esgEC2SecurityGroupId      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EC2SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esgStatus'
--
-- * 'esgEC2SecurityGroupOwnerId'
--
-- * 'esgEC2SecurityGroupName'
--
-- * 'esgEC2SecurityGroupId'
ec2SecurityGroup
    :: EC2SecurityGroup
ec2SecurityGroup =
    EC2SecurityGroup'
    { _esgStatus = Nothing
    , _esgEC2SecurityGroupOwnerId = Nothing
    , _esgEC2SecurityGroupName = Nothing
    , _esgEC2SecurityGroupId = Nothing
    }

-- | Provides the status of the EC2 security group. Status can be
-- \"authorizing\", \"authorized\", \"revoking\", and \"revoked\".
esgStatus :: Lens' EC2SecurityGroup (Maybe Text)
esgStatus = lens _esgStatus (\ s a -> s{_esgStatus = a});

-- | Specifies the AWS ID of the owner of the EC2 security group specified in
-- the 'EC2SecurityGroupName' field.
esgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupOwnerId = lens _esgEC2SecurityGroupOwnerId (\ s a -> s{_esgEC2SecurityGroupOwnerId = a});

-- | Specifies the name of the EC2 security group.
esgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupName = lens _esgEC2SecurityGroupName (\ s a -> s{_esgEC2SecurityGroupName = a});

-- | Specifies the id of the EC2 security group.
esgEC2SecurityGroupId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupId = lens _esgEC2SecurityGroupId (\ s a -> s{_esgEC2SecurityGroupId = a});

instance FromXML EC2SecurityGroup where
        parseXML x
          = EC2SecurityGroup' <$>
              (x .@? "Status") <*>
                (x .@? "EC2SecurityGroupOwnerId")
                <*> (x .@? "EC2SecurityGroupName")
                <*> (x .@? "EC2SecurityGroupId")

-- | This data type is used as a response element in the following actions:
--
-- -   CreateDBInstance
-- -   DescribeDBInstances
-- -   DeleteDBInstance
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
    { _eHostedZoneId :: !(Maybe Text)
    , _eAddress      :: !(Maybe Text)
    , _ePort         :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eHostedZoneId'
--
-- * 'eAddress'
--
-- * 'ePort'
endpoint
    :: Endpoint
endpoint =
    Endpoint'
    { _eHostedZoneId = Nothing
    , _eAddress = Nothing
    , _ePort = Nothing
    }

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
eHostedZoneId :: Lens' Endpoint (Maybe Text)
eHostedZoneId = lens _eHostedZoneId (\ s a -> s{_eHostedZoneId = a});

-- | Specifies the DNS address of the DB instance.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\ s a -> s{_eAddress = a});

-- | Specifies the port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a});

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$>
              (x .@? "HostedZoneId") <*> (x .@? "Address") <*>
                (x .@? "Port")

-- | Contains the result of a successful invocation of the
-- DescribeEngineDefaultParameters action.
--
-- /See:/ 'engineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
    { _edDBParameterGroupFamily :: !(Maybe Text)
    , _edMarker                 :: !(Maybe Text)
    , _edParameters             :: !(Maybe [Parameter])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EngineDefaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edDBParameterGroupFamily'
--
-- * 'edMarker'
--
-- * 'edParameters'
engineDefaults
    :: EngineDefaults
engineDefaults =
    EngineDefaults'
    { _edDBParameterGroupFamily = Nothing
    , _edMarker = Nothing
    , _edParameters = Nothing
    }

-- | Specifies the name of the DB parameter group family that the engine
-- default parameters apply to.
edDBParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edDBParameterGroupFamily = lens _edDBParameterGroupFamily (\ s a -> s{_edDBParameterGroupFamily = a});

-- | An optional pagination token provided by a previous EngineDefaults
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by 'MaxRecords' .
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\ s a -> s{_edMarker = a});

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\ s a -> s{_edParameters = a}) . _Default . _Coerce;

instance FromXML EngineDefaults where
        parseXML x
          = EngineDefaults' <$>
              (x .@? "DBParameterGroupFamily") <*> (x .@? "Marker")
                <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "Parameter"))

-- | This data type is used as a response element in the DescribeEvents
-- action.
--
-- /See:/ 'event' smart constructor.
data Event = Event'
    { _eSourceType       :: !(Maybe SourceType)
    , _eSourceIdentifier :: !(Maybe Text)
    , _eDate             :: !(Maybe ISO8601)
    , _eEventCategories  :: !(Maybe [Text])
    , _eMessage          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceType'
--
-- * 'eSourceIdentifier'
--
-- * 'eDate'
--
-- * 'eEventCategories'
--
-- * 'eMessage'
event
    :: Event
event =
    Event'
    { _eSourceType = Nothing
    , _eSourceIdentifier = Nothing
    , _eDate = Nothing
    , _eEventCategories = Nothing
    , _eMessage = Nothing
    }

-- | Specifies the source type for this event.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\ s a -> s{_eSourceType = a});

-- | Provides the identifier for the source of the event.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier = lens _eSourceIdentifier (\ s a -> s{_eSourceIdentifier = a});

-- | Specifies the date and time of the event.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\ s a -> s{_eDate = a}) . mapping _Time;

-- | Specifies the category for the event.
eEventCategories :: Lens' Event [Text]
eEventCategories = lens _eEventCategories (\ s a -> s{_eEventCategories = a}) . _Default . _Coerce;

-- | Provides the text of this event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a});

instance FromXML Event where
        parseXML x
          = Event' <$>
              (x .@? "SourceType") <*> (x .@? "SourceIdentifier")
                <*> (x .@? "Date")
                <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*> (x .@? "Message")

-- | Contains the results of a successful invocation of the
-- DescribeEventCategories action.
--
-- /See:/ 'eventCategoriesMap' smart constructor.
data EventCategoriesMap = EventCategoriesMap'
    { _ecmSourceType      :: !(Maybe Text)
    , _ecmEventCategories :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EventCategoriesMap' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecmSourceType'
--
-- * 'ecmEventCategories'
eventCategoriesMap
    :: EventCategoriesMap
eventCategoriesMap =
    EventCategoriesMap'
    { _ecmSourceType = Nothing
    , _ecmEventCategories = Nothing
    }

-- | The source type that the returned categories belong to
ecmSourceType :: Lens' EventCategoriesMap (Maybe Text)
ecmSourceType = lens _ecmSourceType (\ s a -> s{_ecmSourceType = a});

-- | The event categories for the specified source type
ecmEventCategories :: Lens' EventCategoriesMap [Text]
ecmEventCategories = lens _ecmEventCategories (\ s a -> s{_ecmEventCategories = a}) . _Default . _Coerce;

instance FromXML EventCategoriesMap where
        parseXML x
          = EventCategoriesMap' <$>
              (x .@? "SourceType") <*>
                (x .@? "EventCategories" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
--
-- /See:/ 'eventSubscription' smart constructor.
data EventSubscription = EventSubscription'
    { _esStatus                   :: !(Maybe Text)
    , _esCustomerAWSId            :: !(Maybe Text)
    , _esCustSubscriptionId       :: !(Maybe Text)
    , _esSNSTopicARN              :: !(Maybe Text)
    , _esEnabled                  :: !(Maybe Bool)
    , _esSourceType               :: !(Maybe Text)
    , _esSubscriptionCreationTime :: !(Maybe Text)
    , _esEventCategoriesList      :: !(Maybe [Text])
    , _esSourceIdsList            :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esStatus'
--
-- * 'esCustomerAWSId'
--
-- * 'esCustSubscriptionId'
--
-- * 'esSNSTopicARN'
--
-- * 'esEnabled'
--
-- * 'esSourceType'
--
-- * 'esSubscriptionCreationTime'
--
-- * 'esEventCategoriesList'
--
-- * 'esSourceIdsList'
eventSubscription
    :: EventSubscription
eventSubscription =
    EventSubscription'
    { _esStatus = Nothing
    , _esCustomerAWSId = Nothing
    , _esCustSubscriptionId = Nothing
    , _esSNSTopicARN = Nothing
    , _esEnabled = Nothing
    , _esSourceType = Nothing
    , _esSubscriptionCreationTime = Nothing
    , _esEventCategoriesList = Nothing
    , _esSourceIdsList = Nothing
    }

-- | The status of the RDS event notification subscription.
--
-- Constraints:
--
-- Can be one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist
--
-- The status \"no-permission\" indicates that RDS no longer has permission
-- to post to the SNS topic. The status \"topic-not-exist\" indicates that
-- the topic was deleted after the subscription was created.
esStatus :: Lens' EventSubscription (Maybe Text)
esStatus = lens _esStatus (\ s a -> s{_esStatus = a});

-- | The AWS customer account associated with the RDS event notification
-- subscription.
esCustomerAWSId :: Lens' EventSubscription (Maybe Text)
esCustomerAWSId = lens _esCustomerAWSId (\ s a -> s{_esCustomerAWSId = a});

-- | The RDS event notification subscription Id.
esCustSubscriptionId :: Lens' EventSubscription (Maybe Text)
esCustSubscriptionId = lens _esCustSubscriptionId (\ s a -> s{_esCustSubscriptionId = a});

-- | The topic ARN of the RDS event notification subscription.
esSNSTopicARN :: Lens' EventSubscription (Maybe Text)
esSNSTopicARN = lens _esSNSTopicARN (\ s a -> s{_esSNSTopicARN = a});

-- | A Boolean value indicating if the subscription is enabled. True
-- indicates the subscription is enabled.
esEnabled :: Lens' EventSubscription (Maybe Bool)
esEnabled = lens _esEnabled (\ s a -> s{_esEnabled = a});

-- | The source type for the RDS event notification subscription.
esSourceType :: Lens' EventSubscription (Maybe Text)
esSourceType = lens _esSourceType (\ s a -> s{_esSourceType = a});

-- | The time the RDS event notification subscription was created.
esSubscriptionCreationTime :: Lens' EventSubscription (Maybe Text)
esSubscriptionCreationTime = lens _esSubscriptionCreationTime (\ s a -> s{_esSubscriptionCreationTime = a});

-- | A list of event categories for the RDS event notification subscription.
esEventCategoriesList :: Lens' EventSubscription [Text]
esEventCategoriesList = lens _esEventCategoriesList (\ s a -> s{_esEventCategoriesList = a}) . _Default . _Coerce;

-- | A list of source IDs for the RDS event notification subscription.
esSourceIdsList :: Lens' EventSubscription [Text]
esSourceIdsList = lens _esSourceIdsList (\ s a -> s{_esSourceIdsList = a}) . _Default . _Coerce;

instance FromXML EventSubscription where
        parseXML x
          = EventSubscription' <$>
              (x .@? "Status") <*> (x .@? "CustomerAwsId") <*>
                (x .@? "CustSubscriptionId")
                <*> (x .@? "SnsTopicArn")
                <*> (x .@? "Enabled")
                <*> (x .@? "SourceType")
                <*> (x .@? "SubscriptionCreationTime")
                <*>
                (x .@? "EventCategoriesList" .!@ mempty >>=
                   may (parseXMLList "EventCategory"))
                <*>
                (x .@? "SourceIdsList" .!@ mempty >>=
                   may (parseXMLList "SourceId"))

-- | /See:/ 'filter'' smart constructor.
data Filter = Filter'
    { _fName   :: !Text
    , _fValues :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName'
--
-- * 'fValues'
filter'
    :: Text -- ^ 'fName'
    -> Filter
filter' pName_ =
    Filter'
    { _fName = pName_
    , _fValues = mempty
    }

-- | This parameter is not currently supported.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a});

-- | This parameter is not currently supported.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce;

instance ToQuery Filter where
        toQuery Filter'{..}
          = mconcat
              ["Name" =: _fName,
               "Values" =: toQueryList "Value" _fValues]

-- | This data type is used as a response element in the
-- DescribeDBSecurityGroups action.
--
-- /See:/ 'ipRange' smart constructor.
data IPRange = IPRange'
    { _irStatus :: !(Maybe Text)
    , _irCIdRIP :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IPRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irStatus'
--
-- * 'irCIdRIP'
ipRange
    :: IPRange
ipRange =
    IPRange'
    { _irStatus = Nothing
    , _irCIdRIP = Nothing
    }

-- | Specifies the status of the IP range. Status can be \"authorizing\",
-- \"authorized\", \"revoking\", and \"revoked\".
irStatus :: Lens' IPRange (Maybe Text)
irStatus = lens _irStatus (\ s a -> s{_irStatus = a});

-- | Specifies the IP range.
irCIdRIP :: Lens' IPRange (Maybe Text)
irCIdRIP = lens _irCIdRIP (\ s a -> s{_irCIdRIP = a});

instance FromXML IPRange where
        parseXML x
          = IPRange' <$> (x .@? "Status") <*> (x .@? "CIDRIP")

-- | Option details.
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
    , _oPort                        :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Option' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oOptionName'
--
-- * 'oPermanent'
--
-- * 'oPersistent'
--
-- * 'oOptionDescription'
--
-- * 'oOptionSettings'
--
-- * 'oVPCSecurityGroupMemberships'
--
-- * 'oDBSecurityGroupMemberships'
--
-- * 'oPort'
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
    , _oPort = Nothing
    }

-- | The name of the option.
oOptionName :: Lens' Option (Maybe Text)
oOptionName = lens _oOptionName (\ s a -> s{_oOptionName = a});

-- | Indicate if this option is permanent.
oPermanent :: Lens' Option (Maybe Bool)
oPermanent = lens _oPermanent (\ s a -> s{_oPermanent = a});

-- | Indicate if this option is persistent.
oPersistent :: Lens' Option (Maybe Bool)
oPersistent = lens _oPersistent (\ s a -> s{_oPersistent = a});

-- | The description of the option.
oOptionDescription :: Lens' Option (Maybe Text)
oOptionDescription = lens _oOptionDescription (\ s a -> s{_oOptionDescription = a});

-- | The option settings for this option.
oOptionSettings :: Lens' Option [OptionSetting]
oOptionSettings = lens _oOptionSettings (\ s a -> s{_oOptionSettings = a}) . _Default . _Coerce;

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
oVPCSecurityGroupMemberships :: Lens' Option [VPCSecurityGroupMembership]
oVPCSecurityGroupMemberships = lens _oVPCSecurityGroupMemberships (\ s a -> s{_oVPCSecurityGroupMemberships = a}) . _Default . _Coerce;

-- | If the option requires access to a port, then this DB security group
-- allows access to the port.
oDBSecurityGroupMemberships :: Lens' Option [DBSecurityGroupMembership]
oDBSecurityGroupMemberships = lens _oDBSecurityGroupMemberships (\ s a -> s{_oDBSecurityGroupMemberships = a}) . _Default . _Coerce;

-- | If required, the port configured for this option to use.
oPort :: Lens' Option (Maybe Int)
oPort = lens _oPort (\ s a -> s{_oPort = a});

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
                <*> (x .@? "Port")

-- | A list of all available options
--
-- /See:/ 'optionConfiguration' smart constructor.
data OptionConfiguration = OptionConfiguration'
    { _ocOptionSettings              :: !(Maybe [OptionSetting])
    , _ocVPCSecurityGroupMemberships :: !(Maybe [Text])
    , _ocDBSecurityGroupMemberships  :: !(Maybe [Text])
    , _ocPort                        :: !(Maybe Int)
    , _ocOptionName                  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocOptionSettings'
--
-- * 'ocVPCSecurityGroupMemberships'
--
-- * 'ocDBSecurityGroupMemberships'
--
-- * 'ocPort'
--
-- * 'ocOptionName'
optionConfiguration
    :: Text -- ^ 'ocOptionName'
    -> OptionConfiguration
optionConfiguration pOptionName_ =
    OptionConfiguration'
    { _ocOptionSettings = Nothing
    , _ocVPCSecurityGroupMemberships = Nothing
    , _ocDBSecurityGroupMemberships = Nothing
    , _ocPort = Nothing
    , _ocOptionName = pOptionName_
    }

-- | The option settings to include in an option group.
ocOptionSettings :: Lens' OptionConfiguration [OptionSetting]
ocOptionSettings = lens _ocOptionSettings (\ s a -> s{_ocOptionSettings = a}) . _Default . _Coerce;

-- | A list of VpcSecurityGroupMemebrship name strings used for this option.
ocVPCSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocVPCSecurityGroupMemberships = lens _ocVPCSecurityGroupMemberships (\ s a -> s{_ocVPCSecurityGroupMemberships = a}) . _Default . _Coerce;

-- | A list of DBSecurityGroupMemebrship name strings used for this option.
ocDBSecurityGroupMemberships :: Lens' OptionConfiguration [Text]
ocDBSecurityGroupMemberships = lens _ocDBSecurityGroupMemberships (\ s a -> s{_ocDBSecurityGroupMemberships = a}) . _Default . _Coerce;

-- | The optional port for the option.
ocPort :: Lens' OptionConfiguration (Maybe Int)
ocPort = lens _ocPort (\ s a -> s{_ocPort = a});

-- | The configuration of options to include in a group.
ocOptionName :: Lens' OptionConfiguration Text
ocOptionName = lens _ocOptionName (\ s a -> s{_ocOptionName = a});

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
               "Port" =: _ocPort, "OptionName" =: _ocOptionName]

-- |
--
-- /See:/ 'optionGroup' smart constructor.
data OptionGroup = OptionGroup'
    { _ogOptionGroupDescription                :: !(Maybe Text)
    , _ogVPCId                                 :: !(Maybe Text)
    , _ogAllowsVPCAndNonVPCInstanceMemberships :: !(Maybe Bool)
    , _ogEngineName                            :: !(Maybe Text)
    , _ogMajorEngineVersion                    :: !(Maybe Text)
    , _ogOptions                               :: !(Maybe [Option])
    , _ogOptionGroupName                       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogOptionGroupDescription'
--
-- * 'ogVPCId'
--
-- * 'ogAllowsVPCAndNonVPCInstanceMemberships'
--
-- * 'ogEngineName'
--
-- * 'ogMajorEngineVersion'
--
-- * 'ogOptions'
--
-- * 'ogOptionGroupName'
optionGroup
    :: OptionGroup
optionGroup =
    OptionGroup'
    { _ogOptionGroupDescription = Nothing
    , _ogVPCId = Nothing
    , _ogAllowsVPCAndNonVPCInstanceMemberships = Nothing
    , _ogEngineName = Nothing
    , _ogMajorEngineVersion = Nothing
    , _ogOptions = Nothing
    , _ogOptionGroupName = Nothing
    }

-- | Provides a description of the option group.
ogOptionGroupDescription :: Lens' OptionGroup (Maybe Text)
ogOptionGroupDescription = lens _ogOptionGroupDescription (\ s a -> s{_ogOptionGroupDescription = a});

-- | If __AllowsVpcAndNonVpcInstanceMemberships__ is 'false', this field is
-- blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is 'true' and this
-- field is blank, then this option group can be applied to both VPC and
-- non-VPC instances. If this field contains a value, then this option
-- group can only be applied to instances that are in the VPC indicated by
-- this field.
ogVPCId :: Lens' OptionGroup (Maybe Text)
ogVPCId = lens _ogVPCId (\ s a -> s{_ogVPCId = a});

-- | Indicates whether this option group can be applied to both VPC and
-- non-VPC instances. The value 'true' indicates the option group can be
-- applied to both VPC and non-VPC instances.
ogAllowsVPCAndNonVPCInstanceMemberships :: Lens' OptionGroup (Maybe Bool)
ogAllowsVPCAndNonVPCInstanceMemberships = lens _ogAllowsVPCAndNonVPCInstanceMemberships (\ s a -> s{_ogAllowsVPCAndNonVPCInstanceMemberships = a});

-- | Indicates the name of the engine that this option group can be applied
-- to.
ogEngineName :: Lens' OptionGroup (Maybe Text)
ogEngineName = lens _ogEngineName (\ s a -> s{_ogEngineName = a});

-- | Indicates the major engine version associated with this option group.
ogMajorEngineVersion :: Lens' OptionGroup (Maybe Text)
ogMajorEngineVersion = lens _ogMajorEngineVersion (\ s a -> s{_ogMajorEngineVersion = a});

-- | Indicates what options are available in the option group.
ogOptions :: Lens' OptionGroup [Option]
ogOptions = lens _ogOptions (\ s a -> s{_ogOptions = a}) . _Default . _Coerce;

-- | Specifies the name of the option group.
ogOptionGroupName :: Lens' OptionGroup (Maybe Text)
ogOptionGroupName = lens _ogOptionGroupName (\ s a -> s{_ogOptionGroupName = a});

instance FromXML OptionGroup where
        parseXML x
          = OptionGroup' <$>
              (x .@? "OptionGroupDescription") <*> (x .@? "VpcId")
                <*> (x .@? "AllowsVpcAndNonVpcInstanceMemberships")
                <*> (x .@? "EngineName")
                <*> (x .@? "MajorEngineVersion")
                <*>
                (x .@? "Options" .!@ mempty >>=
                   may (parseXMLList "Option"))
                <*> (x .@? "OptionGroupName")

-- | Provides information on the option groups the DB instance is a member
-- of.
--
-- /See:/ 'optionGroupMembership' smart constructor.
data OptionGroupMembership = OptionGroupMembership'
    { _ogmStatus          :: !(Maybe Text)
    , _ogmOptionGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogmStatus'
--
-- * 'ogmOptionGroupName'
optionGroupMembership
    :: OptionGroupMembership
optionGroupMembership =
    OptionGroupMembership'
    { _ogmStatus = Nothing
    , _ogmOptionGroupName = Nothing
    }

-- | The status of the DB instance\'s option group membership (e.g. in-sync,
-- pending, pending-maintenance, applying).
ogmStatus :: Lens' OptionGroupMembership (Maybe Text)
ogmStatus = lens _ogmStatus (\ s a -> s{_ogmStatus = a});

-- | The name of the option group that the instance belongs to.
ogmOptionGroupName :: Lens' OptionGroupMembership (Maybe Text)
ogmOptionGroupName = lens _ogmOptionGroupName (\ s a -> s{_ogmOptionGroupName = a});

instance FromXML OptionGroupMembership where
        parseXML x
          = OptionGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "OptionGroupName")

-- | Available option.
--
-- /See:/ 'optionGroupOption' smart constructor.
data OptionGroupOption = OptionGroupOption'
    { _ogoMinimumRequiredMinorEngineVersion :: !(Maybe Text)
    , _ogoPermanent                         :: !(Maybe Bool)
    , _ogoPersistent                        :: !(Maybe Bool)
    , _ogoEngineName                        :: !(Maybe Text)
    , _ogoMajorEngineVersion                :: !(Maybe Text)
    , _ogoName                              :: !(Maybe Text)
    , _ogoDefaultPort                       :: !(Maybe Int)
    , _ogoOptionGroupOptionSettings         :: !(Maybe [OptionGroupOptionSetting])
    , _ogoPortRequired                      :: !(Maybe Bool)
    , _ogoDescription                       :: !(Maybe Text)
    , _ogoOptionsDependedOn                 :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionGroupOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogoMinimumRequiredMinorEngineVersion'
--
-- * 'ogoPermanent'
--
-- * 'ogoPersistent'
--
-- * 'ogoEngineName'
--
-- * 'ogoMajorEngineVersion'
--
-- * 'ogoName'
--
-- * 'ogoDefaultPort'
--
-- * 'ogoOptionGroupOptionSettings'
--
-- * 'ogoPortRequired'
--
-- * 'ogoDescription'
--
-- * 'ogoOptionsDependedOn'
optionGroupOption
    :: OptionGroupOption
optionGroupOption =
    OptionGroupOption'
    { _ogoMinimumRequiredMinorEngineVersion = Nothing
    , _ogoPermanent = Nothing
    , _ogoPersistent = Nothing
    , _ogoEngineName = Nothing
    , _ogoMajorEngineVersion = Nothing
    , _ogoName = Nothing
    , _ogoDefaultPort = Nothing
    , _ogoOptionGroupOptionSettings = Nothing
    , _ogoPortRequired = Nothing
    , _ogoDescription = Nothing
    , _ogoOptionsDependedOn = Nothing
    }

-- | The minimum required engine version for the option to be applied.
ogoMinimumRequiredMinorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMinimumRequiredMinorEngineVersion = lens _ogoMinimumRequiredMinorEngineVersion (\ s a -> s{_ogoMinimumRequiredMinorEngineVersion = a});

-- | A permanent option cannot be removed from the option group once the
-- option group is used, and it cannot be removed from the db instance
-- after assigning an option group with this permanent option.
ogoPermanent :: Lens' OptionGroupOption (Maybe Bool)
ogoPermanent = lens _ogoPermanent (\ s a -> s{_ogoPermanent = a});

-- | A persistent option cannot be removed from the option group once the
-- option group is used, but this option can be removed from the db
-- instance while modifying the related data and assigning another option
-- group without this option.
ogoPersistent :: Lens' OptionGroupOption (Maybe Bool)
ogoPersistent = lens _ogoPersistent (\ s a -> s{_ogoPersistent = a});

-- | The name of the engine that this option can be applied to.
ogoEngineName :: Lens' OptionGroupOption (Maybe Text)
ogoEngineName = lens _ogoEngineName (\ s a -> s{_ogoEngineName = a});

-- | Indicates the major engine version that the option is available for.
ogoMajorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMajorEngineVersion = lens _ogoMajorEngineVersion (\ s a -> s{_ogoMajorEngineVersion = a});

-- | The name of the option.
ogoName :: Lens' OptionGroupOption (Maybe Text)
ogoName = lens _ogoName (\ s a -> s{_ogoName = a});

-- | If the option requires a port, specifies the default port for the
-- option.
ogoDefaultPort :: Lens' OptionGroupOption (Maybe Int)
ogoDefaultPort = lens _ogoDefaultPort (\ s a -> s{_ogoDefaultPort = a});

-- | Specifies the option settings that are available (and the default value)
-- for each option in an option group.
ogoOptionGroupOptionSettings :: Lens' OptionGroupOption [OptionGroupOptionSetting]
ogoOptionGroupOptionSettings = lens _ogoOptionGroupOptionSettings (\ s a -> s{_ogoOptionGroupOptionSettings = a}) . _Default . _Coerce;

-- | Specifies whether the option requires a port.
ogoPortRequired :: Lens' OptionGroupOption (Maybe Bool)
ogoPortRequired = lens _ogoPortRequired (\ s a -> s{_ogoPortRequired = a});

-- | The description of the option.
ogoDescription :: Lens' OptionGroupOption (Maybe Text)
ogoDescription = lens _ogoDescription (\ s a -> s{_ogoDescription = a});

-- | List of all options that are prerequisites for this option.
ogoOptionsDependedOn :: Lens' OptionGroupOption [Text]
ogoOptionsDependedOn = lens _ogoOptionsDependedOn (\ s a -> s{_ogoOptionsDependedOn = a}) . _Default . _Coerce;

instance FromXML OptionGroupOption where
        parseXML x
          = OptionGroupOption' <$>
              (x .@? "MinimumRequiredMinorEngineVersion") <*>
                (x .@? "Permanent")
                <*> (x .@? "Persistent")
                <*> (x .@? "EngineName")
                <*> (x .@? "MajorEngineVersion")
                <*> (x .@? "Name")
                <*> (x .@? "DefaultPort")
                <*>
                (x .@? "OptionGroupOptionSettings" .!@ mempty >>=
                   may (parseXMLList "OptionGroupOptionSetting"))
                <*> (x .@? "PortRequired")
                <*> (x .@? "Description")
                <*>
                (x .@? "OptionsDependedOn" .!@ mempty >>=
                   may (parseXMLList "OptionName"))

-- | Option group option settings are used to display settings available for
-- each option with their default values and other information. These
-- values are used with the DescribeOptionGroupOptions action.
--
-- /See:/ 'optionGroupOptionSetting' smart constructor.
data OptionGroupOptionSetting = OptionGroupOptionSetting'
    { _ogosApplyType          :: !(Maybe Text)
    , _ogosSettingName        :: !(Maybe Text)
    , _ogosDefaultValue       :: !(Maybe Text)
    , _ogosIsModifiable       :: !(Maybe Bool)
    , _ogosSettingDescription :: !(Maybe Text)
    , _ogosAllowedValues      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionGroupOptionSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogosApplyType'
--
-- * 'ogosSettingName'
--
-- * 'ogosDefaultValue'
--
-- * 'ogosIsModifiable'
--
-- * 'ogosSettingDescription'
--
-- * 'ogosAllowedValues'
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
ogosApplyType = lens _ogosApplyType (\ s a -> s{_ogosApplyType = a});

-- | The name of the option group option.
ogosSettingName :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingName = lens _ogosSettingName (\ s a -> s{_ogosSettingName = a});

-- | The default value for the option group option.
ogosDefaultValue :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosDefaultValue = lens _ogosDefaultValue (\ s a -> s{_ogosDefaultValue = a});

-- | Boolean value where true indicates that this option group option can be
-- changed from the default value.
ogosIsModifiable :: Lens' OptionGroupOptionSetting (Maybe Bool)
ogosIsModifiable = lens _ogosIsModifiable (\ s a -> s{_ogosIsModifiable = a});

-- | The description of the option group option.
ogosSettingDescription :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingDescription = lens _ogosSettingDescription (\ s a -> s{_ogosSettingDescription = a});

-- | Indicates the acceptable values for the option group option.
ogosAllowedValues :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosAllowedValues = lens _ogosAllowedValues (\ s a -> s{_ogosAllowedValues = a});

instance FromXML OptionGroupOptionSetting where
        parseXML x
          = OptionGroupOptionSetting' <$>
              (x .@? "ApplyType") <*> (x .@? "SettingName") <*>
                (x .@? "DefaultValue")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "SettingDescription")
                <*> (x .@? "AllowedValues")

-- | Option settings are the actual settings being applied or configured for
-- that option. It is used when you modify an option group or describe
-- option groups. For example, the NATIVE_NETWORK_ENCRYPTION option has a
-- setting called SQLNET.ENCRYPTION_SERVER that can have several different
-- values.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osIsCollection'
--
-- * 'osApplyType'
--
-- * 'osValue'
--
-- * 'osName'
--
-- * 'osDefaultValue'
--
-- * 'osIsModifiable'
--
-- * 'osDataType'
--
-- * 'osAllowedValues'
--
-- * 'osDescription'
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
osIsCollection = lens _osIsCollection (\ s a -> s{_osIsCollection = a});

-- | The DB engine specific parameter type.
osApplyType :: Lens' OptionSetting (Maybe Text)
osApplyType = lens _osApplyType (\ s a -> s{_osApplyType = a});

-- | The current value of the option setting.
osValue :: Lens' OptionSetting (Maybe Text)
osValue = lens _osValue (\ s a -> s{_osValue = a});

-- | The name of the option that has settings that you can set.
osName :: Lens' OptionSetting (Maybe Text)
osName = lens _osName (\ s a -> s{_osName = a});

-- | The default value of the option setting.
osDefaultValue :: Lens' OptionSetting (Maybe Text)
osDefaultValue = lens _osDefaultValue (\ s a -> s{_osDefaultValue = a});

-- | A Boolean value that, when true, indicates the option setting can be
-- modified from the default.
osIsModifiable :: Lens' OptionSetting (Maybe Bool)
osIsModifiable = lens _osIsModifiable (\ s a -> s{_osIsModifiable = a});

-- | The data type of the option setting.
osDataType :: Lens' OptionSetting (Maybe Text)
osDataType = lens _osDataType (\ s a -> s{_osDataType = a});

-- | The allowed values of the option setting.
osAllowedValues :: Lens' OptionSetting (Maybe Text)
osAllowedValues = lens _osAllowedValues (\ s a -> s{_osAllowedValues = a});

-- | The description of the option setting.
osDescription :: Lens' OptionSetting (Maybe Text)
osDescription = lens _osDescription (\ s a -> s{_osDescription = a});

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

-- | Contains a list of available options for a DB instance
--
-- This data type is used as a response element in the
-- DescribeOrderableDBInstanceOptions action.
--
-- /See:/ 'orderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'
    { _odioEngineVersion             :: !(Maybe Text)
    , _odioMultiAZCapable            :: !(Maybe Bool)
    , _odioEngine                    :: !(Maybe Text)
    , _odioSupportsIOPS              :: !(Maybe Bool)
    , _odioDBInstanceClass           :: !(Maybe Text)
    , _odioLicenseModel              :: !(Maybe Text)
    , _odioAvailabilityZones         :: !(Maybe [AvailabilityZone])
    , _odioSupportsStorageEncryption :: !(Maybe Bool)
    , _odioReadReplicaCapable        :: !(Maybe Bool)
    , _odioVPC                       :: !(Maybe Bool)
    , _odioStorageType               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OrderableDBInstanceOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odioEngineVersion'
--
-- * 'odioMultiAZCapable'
--
-- * 'odioEngine'
--
-- * 'odioSupportsIOPS'
--
-- * 'odioDBInstanceClass'
--
-- * 'odioLicenseModel'
--
-- * 'odioAvailabilityZones'
--
-- * 'odioSupportsStorageEncryption'
--
-- * 'odioReadReplicaCapable'
--
-- * 'odioVPC'
--
-- * 'odioStorageType'
orderableDBInstanceOption
    :: OrderableDBInstanceOption
orderableDBInstanceOption =
    OrderableDBInstanceOption'
    { _odioEngineVersion = Nothing
    , _odioMultiAZCapable = Nothing
    , _odioEngine = Nothing
    , _odioSupportsIOPS = Nothing
    , _odioDBInstanceClass = Nothing
    , _odioLicenseModel = Nothing
    , _odioAvailabilityZones = Nothing
    , _odioSupportsStorageEncryption = Nothing
    , _odioReadReplicaCapable = Nothing
    , _odioVPC = Nothing
    , _odioStorageType = Nothing
    }

-- | The engine version of the orderable DB instance.
odioEngineVersion :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngineVersion = lens _odioEngineVersion (\ s a -> s{_odioEngineVersion = a});

-- | Indicates whether this orderable DB instance is multi-AZ capable.
odioMultiAZCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioMultiAZCapable = lens _odioMultiAZCapable (\ s a -> s{_odioMultiAZCapable = a});

-- | The engine type of the orderable DB instance.
odioEngine :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngine = lens _odioEngine (\ s a -> s{_odioEngine = a});

-- | Indicates whether this orderable DB instance supports provisioned IOPS.
odioSupportsIOPS :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsIOPS = lens _odioSupportsIOPS (\ s a -> s{_odioSupportsIOPS = a});

-- | The DB instance class for the orderable DB instance.
odioDBInstanceClass :: Lens' OrderableDBInstanceOption (Maybe Text)
odioDBInstanceClass = lens _odioDBInstanceClass (\ s a -> s{_odioDBInstanceClass = a});

-- | The license model for the orderable DB instance.
odioLicenseModel :: Lens' OrderableDBInstanceOption (Maybe Text)
odioLicenseModel = lens _odioLicenseModel (\ s a -> s{_odioLicenseModel = a});

-- | A list of Availability Zones for the orderable DB instance.
odioAvailabilityZones :: Lens' OrderableDBInstanceOption [AvailabilityZone]
odioAvailabilityZones = lens _odioAvailabilityZones (\ s a -> s{_odioAvailabilityZones = a}) . _Default . _Coerce;

-- | Indicates whether this orderable DB instance supports encrypted storage.
odioSupportsStorageEncryption :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsStorageEncryption = lens _odioSupportsStorageEncryption (\ s a -> s{_odioSupportsStorageEncryption = a});

-- | Indicates whether this orderable DB instance can have a Read Replica.
odioReadReplicaCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioReadReplicaCapable = lens _odioReadReplicaCapable (\ s a -> s{_odioReadReplicaCapable = a});

-- | Indicates whether this is a VPC orderable DB instance.
odioVPC :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioVPC = lens _odioVPC (\ s a -> s{_odioVPC = a});

-- | Indicates the storage type for this orderable DB instance.
odioStorageType :: Lens' OrderableDBInstanceOption (Maybe Text)
odioStorageType = lens _odioStorageType (\ s a -> s{_odioStorageType = a});

instance FromXML OrderableDBInstanceOption where
        parseXML x
          = OrderableDBInstanceOption' <$>
              (x .@? "EngineVersion") <*> (x .@? "MultiAZCapable")
                <*> (x .@? "Engine")
                <*> (x .@? "SupportsIops")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "LicenseModel")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "SupportsStorageEncryption")
                <*> (x .@? "ReadReplicaCapable")
                <*> (x .@? "Vpc")
                <*> (x .@? "StorageType")

-- | This data type is used as a request parameter in the
-- ModifyDBParameterGroup and ResetDBParameterGroup actions.
--
-- This data type is used as a response element in the
-- DescribeEngineDefaultParameters and DescribeDBParameters actions.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pApplyType'
--
-- * 'pParameterValue'
--
-- * 'pApplyMethod'
--
-- * 'pMinimumEngineVersion'
--
-- * 'pSource'
--
-- * 'pIsModifiable'
--
-- * 'pDataType'
--
-- * 'pAllowedValues'
--
-- * 'pParameterName'
--
-- * 'pDescription'
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
pApplyType = lens _pApplyType (\ s a -> s{_pApplyType = a});

-- | Specifies the value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\ s a -> s{_pParameterValue = a});

-- | Indicates when to apply parameter updates.
pApplyMethod :: Lens' Parameter (Maybe ApplyMethod)
pApplyMethod = lens _pApplyMethod (\ s a -> s{_pApplyMethod = a});

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion = lens _pMinimumEngineVersion (\ s a -> s{_pMinimumEngineVersion = a});

-- | Indicates the source of the parameter value.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\ s a -> s{_pSource = a});

-- | Indicates whether ('true') or not ('false') the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\ s a -> s{_pIsModifiable = a});

-- | Specifies the valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\ s a -> s{_pDataType = a});

-- | Specifies the valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\ s a -> s{_pAllowedValues = a});

-- | Specifies the name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\ s a -> s{_pParameterName = a});

-- | Provides a description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a});

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

-- | Provides information about a pending maintenance action for a resource.
--
-- /See:/ 'pendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
    { _pmaAutoAppliedAfterDate :: !(Maybe ISO8601)
    , _pmaAction               :: !(Maybe Text)
    , _pmaOptInStatus          :: !(Maybe Text)
    , _pmaDescription          :: !(Maybe Text)
    , _pmaForcedApplyDate      :: !(Maybe ISO8601)
    , _pmaCurrentApplyDate     :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PendingMaintenanceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmaAutoAppliedAfterDate'
--
-- * 'pmaAction'
--
-- * 'pmaOptInStatus'
--
-- * 'pmaDescription'
--
-- * 'pmaForcedApplyDate'
--
-- * 'pmaCurrentApplyDate'
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

-- | The date of the maintenance window when the action will be applied. The
-- maintenance action will be applied to the resource during its first
-- maintenance window after this date. If this date is specified, any
-- 'next-maintenance' opt-in requests are ignored.
pmaAutoAppliedAfterDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaAutoAppliedAfterDate = lens _pmaAutoAppliedAfterDate (\ s a -> s{_pmaAutoAppliedAfterDate = a}) . mapping _Time;

-- | The type of pending maintenance action that is available for the
-- resource.
pmaAction :: Lens' PendingMaintenanceAction (Maybe Text)
pmaAction = lens _pmaAction (\ s a -> s{_pmaAction = a});

-- | Indicates the type of opt-in request that has been received for the
-- resource.
pmaOptInStatus :: Lens' PendingMaintenanceAction (Maybe Text)
pmaOptInStatus = lens _pmaOptInStatus (\ s a -> s{_pmaOptInStatus = a});

-- | A description providing more detail about the maintenance action.
pmaDescription :: Lens' PendingMaintenanceAction (Maybe Text)
pmaDescription = lens _pmaDescription (\ s a -> s{_pmaDescription = a});

-- | The date when the maintenance action will be automatically applied. The
-- maintenance action will be applied to the resource on this date
-- regardless of the maintenance window for the resource. If this date is
-- specified, any 'immediate' opt-in requests are ignored.
pmaForcedApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaForcedApplyDate = lens _pmaForcedApplyDate (\ s a -> s{_pmaForcedApplyDate = a}) . mapping _Time;

-- | The effective date when the pending maintenance action will be applied
-- to the resource. This date takes into account opt-in requests received
-- from the ApplyPendingMaintenanceAction API, the 'AutoAppliedAfterDate',
-- and the 'ForcedApplyDate'. This value is blank if an opt-in request has
-- not been received and nothing has been specified as
-- 'AutoAppliedAfterDate' or 'ForcedApplyDate'.
pmaCurrentApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaCurrentApplyDate = lens _pmaCurrentApplyDate (\ s a -> s{_pmaCurrentApplyDate = a}) . mapping _Time;

instance FromXML PendingMaintenanceAction where
        parseXML x
          = PendingMaintenanceAction' <$>
              (x .@? "AutoAppliedAfterDate") <*> (x .@? "Action")
                <*> (x .@? "OptInStatus")
                <*> (x .@? "Description")
                <*> (x .@? "ForcedApplyDate")
                <*> (x .@? "CurrentApplyDate")

-- | This data type is used as a response element in the ModifyDBInstance
-- action.
--
-- /See:/ 'pendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
    { _pmvEngineVersion           :: !(Maybe Text)
    , _pmvMasterUserPassword      :: !(Maybe Text)
    , _pmvIOPS                    :: !(Maybe Int)
    , _pmvDBInstanceClass         :: !(Maybe Text)
    , _pmvCACertificateIdentifier :: !(Maybe Text)
    , _pmvDBInstanceIdentifier    :: !(Maybe Text)
    , _pmvBackupRetentionPeriod   :: !(Maybe Int)
    , _pmvMultiAZ                 :: !(Maybe Bool)
    , _pmvAllocatedStorage        :: !(Maybe Int)
    , _pmvPort                    :: !(Maybe Int)
    , _pmvStorageType             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmvEngineVersion'
--
-- * 'pmvMasterUserPassword'
--
-- * 'pmvIOPS'
--
-- * 'pmvDBInstanceClass'
--
-- * 'pmvCACertificateIdentifier'
--
-- * 'pmvDBInstanceIdentifier'
--
-- * 'pmvBackupRetentionPeriod'
--
-- * 'pmvMultiAZ'
--
-- * 'pmvAllocatedStorage'
--
-- * 'pmvPort'
--
-- * 'pmvStorageType'
pendingModifiedValues
    :: PendingModifiedValues
pendingModifiedValues =
    PendingModifiedValues'
    { _pmvEngineVersion = Nothing
    , _pmvMasterUserPassword = Nothing
    , _pmvIOPS = Nothing
    , _pmvDBInstanceClass = Nothing
    , _pmvCACertificateIdentifier = Nothing
    , _pmvDBInstanceIdentifier = Nothing
    , _pmvBackupRetentionPeriod = Nothing
    , _pmvMultiAZ = Nothing
    , _pmvAllocatedStorage = Nothing
    , _pmvPort = Nothing
    , _pmvStorageType = Nothing
    }

-- | Indicates the database engine version.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\ s a -> s{_pmvEngineVersion = a});

-- | Contains the pending or in-progress change of the master credentials for
-- the DB instance.
pmvMasterUserPassword :: Lens' PendingModifiedValues (Maybe Text)
pmvMasterUserPassword = lens _pmvMasterUserPassword (\ s a -> s{_pmvMasterUserPassword = a});

-- | Specifies the new Provisioned IOPS value for the DB instance that will
-- be applied or is being applied.
pmvIOPS :: Lens' PendingModifiedValues (Maybe Int)
pmvIOPS = lens _pmvIOPS (\ s a -> s{_pmvIOPS = a});

-- | Contains the new 'DBInstanceClass' for the DB instance that will be
-- applied or is in progress.
pmvDBInstanceClass :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceClass = lens _pmvDBInstanceClass (\ s a -> s{_pmvDBInstanceClass = a});

-- | Specifies the identifier of the CA certificate for the DB instance.
pmvCACertificateIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvCACertificateIdentifier = lens _pmvCACertificateIdentifier (\ s a -> s{_pmvCACertificateIdentifier = a});

-- | Contains the new 'DBInstanceIdentifier' for the DB instance that will be
-- applied or is in progress.
pmvDBInstanceIdentifier :: Lens' PendingModifiedValues (Maybe Text)
pmvDBInstanceIdentifier = lens _pmvDBInstanceIdentifier (\ s a -> s{_pmvDBInstanceIdentifier = a});

-- | Specifies the pending number of days for which automated backups are
-- retained.
pmvBackupRetentionPeriod :: Lens' PendingModifiedValues (Maybe Int)
pmvBackupRetentionPeriod = lens _pmvBackupRetentionPeriod (\ s a -> s{_pmvBackupRetentionPeriod = a});

-- | Indicates that the Single-AZ DB instance is to change to a Multi-AZ
-- deployment.
pmvMultiAZ :: Lens' PendingModifiedValues (Maybe Bool)
pmvMultiAZ = lens _pmvMultiAZ (\ s a -> s{_pmvMultiAZ = a});

-- | Contains the new 'AllocatedStorage' size for the DB instance that will
-- be applied or is in progress.
pmvAllocatedStorage :: Lens' PendingModifiedValues (Maybe Int)
pmvAllocatedStorage = lens _pmvAllocatedStorage (\ s a -> s{_pmvAllocatedStorage = a});

-- | Specifies the pending port for the DB instance.
pmvPort :: Lens' PendingModifiedValues (Maybe Int)
pmvPort = lens _pmvPort (\ s a -> s{_pmvPort = a});

-- | Specifies the storage type to be associated with the DB instance.
pmvStorageType :: Lens' PendingModifiedValues (Maybe Text)
pmvStorageType = lens _pmvStorageType (\ s a -> s{_pmvStorageType = a});

instance FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "MasterUserPassword")
                <*> (x .@? "Iops")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "CACertificateIdentifier")
                <*> (x .@? "DBInstanceIdentifier")
                <*> (x .@? "BackupRetentionPeriod")
                <*> (x .@? "MultiAZ")
                <*> (x .@? "AllocatedStorage")
                <*> (x .@? "Port")
                <*> (x .@? "StorageType")

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and DescribeReservedDBInstancesOfferings
-- actions.
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
    { _rcRecurringChargeFrequency :: !(Maybe Text)
    , _rcRecurringChargeAmount    :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRecurringChargeFrequency'
--
-- * 'rcRecurringChargeAmount'
recurringCharge
    :: RecurringCharge
recurringCharge =
    RecurringCharge'
    { _rcRecurringChargeFrequency = Nothing
    , _rcRecurringChargeAmount = Nothing
    }

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\ s a -> s{_rcRecurringChargeFrequency = a});

-- | The amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\ s a -> s{_rcRecurringChargeAmount = a});

instance FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' <$>
              (x .@? "RecurringChargeFrequency") <*>
                (x .@? "RecurringChargeAmount")

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and PurchaseReservedDBInstancesOffering
-- actions.
--
-- /See:/ 'reservedDBInstance' smart constructor.
data ReservedDBInstance = ReservedDBInstance'
    { _rdiDBInstanceCount               :: !(Maybe Int)
    , _rdiState                         :: !(Maybe Text)
    , _rdiCurrencyCode                  :: !(Maybe Text)
    , _rdiStartTime                     :: !(Maybe ISO8601)
    , _rdiProductDescription            :: !(Maybe Text)
    , _rdiReservedDBInstanceId          :: !(Maybe Text)
    , _rdiDBInstanceClass               :: !(Maybe Text)
    , _rdiMultiAZ                       :: !(Maybe Bool)
    , _rdiReservedDBInstancesOfferingId :: !(Maybe Text)
    , _rdiRecurringCharges              :: !(Maybe [RecurringCharge])
    , _rdiOfferingType                  :: !(Maybe Text)
    , _rdiUsagePrice                    :: !(Maybe Double)
    , _rdiFixedPrice                    :: !(Maybe Double)
    , _rdiDuration                      :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReservedDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdiDBInstanceCount'
--
-- * 'rdiState'
--
-- * 'rdiCurrencyCode'
--
-- * 'rdiStartTime'
--
-- * 'rdiProductDescription'
--
-- * 'rdiReservedDBInstanceId'
--
-- * 'rdiDBInstanceClass'
--
-- * 'rdiMultiAZ'
--
-- * 'rdiReservedDBInstancesOfferingId'
--
-- * 'rdiRecurringCharges'
--
-- * 'rdiOfferingType'
--
-- * 'rdiUsagePrice'
--
-- * 'rdiFixedPrice'
--
-- * 'rdiDuration'
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
rdiDBInstanceCount = lens _rdiDBInstanceCount (\ s a -> s{_rdiDBInstanceCount = a});

-- | The state of the reserved DB instance.
rdiState :: Lens' ReservedDBInstance (Maybe Text)
rdiState = lens _rdiState (\ s a -> s{_rdiState = a});

-- | The currency code for the reserved DB instance.
rdiCurrencyCode :: Lens' ReservedDBInstance (Maybe Text)
rdiCurrencyCode = lens _rdiCurrencyCode (\ s a -> s{_rdiCurrencyCode = a});

-- | The time the reservation started.
rdiStartTime :: Lens' ReservedDBInstance (Maybe UTCTime)
rdiStartTime = lens _rdiStartTime (\ s a -> s{_rdiStartTime = a}) . mapping _Time;

-- | The description of the reserved DB instance.
rdiProductDescription :: Lens' ReservedDBInstance (Maybe Text)
rdiProductDescription = lens _rdiProductDescription (\ s a -> s{_rdiProductDescription = a});

-- | The unique identifier for the reservation.
rdiReservedDBInstanceId :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstanceId = lens _rdiReservedDBInstanceId (\ s a -> s{_rdiReservedDBInstanceId = a});

-- | The DB instance class for the reserved DB instance.
rdiDBInstanceClass :: Lens' ReservedDBInstance (Maybe Text)
rdiDBInstanceClass = lens _rdiDBInstanceClass (\ s a -> s{_rdiDBInstanceClass = a});

-- | Indicates if the reservation applies to Multi-AZ deployments.
rdiMultiAZ :: Lens' ReservedDBInstance (Maybe Bool)
rdiMultiAZ = lens _rdiMultiAZ (\ s a -> s{_rdiMultiAZ = a});

-- | The offering identifier.
rdiReservedDBInstancesOfferingId :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstancesOfferingId = lens _rdiReservedDBInstancesOfferingId (\ s a -> s{_rdiReservedDBInstancesOfferingId = a});

-- | The recurring price charged to run this reserved DB instance.
rdiRecurringCharges :: Lens' ReservedDBInstance [RecurringCharge]
rdiRecurringCharges = lens _rdiRecurringCharges (\ s a -> s{_rdiRecurringCharges = a}) . _Default . _Coerce;

-- | The offering type of this reserved DB instance.
rdiOfferingType :: Lens' ReservedDBInstance (Maybe Text)
rdiOfferingType = lens _rdiOfferingType (\ s a -> s{_rdiOfferingType = a});

-- | The hourly price charged for this reserved DB instance.
rdiUsagePrice :: Lens' ReservedDBInstance (Maybe Double)
rdiUsagePrice = lens _rdiUsagePrice (\ s a -> s{_rdiUsagePrice = a});

-- | The fixed price charged for this reserved DB instance.
rdiFixedPrice :: Lens' ReservedDBInstance (Maybe Double)
rdiFixedPrice = lens _rdiFixedPrice (\ s a -> s{_rdiFixedPrice = a});

-- | The duration of the reservation in seconds.
rdiDuration :: Lens' ReservedDBInstance (Maybe Int)
rdiDuration = lens _rdiDuration (\ s a -> s{_rdiDuration = a});

instance FromXML ReservedDBInstance where
        parseXML x
          = ReservedDBInstance' <$>
              (x .@? "DBInstanceCount") <*> (x .@? "State") <*>
                (x .@? "CurrencyCode")
                <*> (x .@? "StartTime")
                <*> (x .@? "ProductDescription")
                <*> (x .@? "ReservedDBInstanceId")
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

-- | This data type is used as a response element in the
-- DescribeReservedDBInstancesOfferings action.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReservedDBInstancesOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdioCurrencyCode'
--
-- * 'rdioProductDescription'
--
-- * 'rdioDBInstanceClass'
--
-- * 'rdioMultiAZ'
--
-- * 'rdioReservedDBInstancesOfferingId'
--
-- * 'rdioRecurringCharges'
--
-- * 'rdioOfferingType'
--
-- * 'rdioUsagePrice'
--
-- * 'rdioFixedPrice'
--
-- * 'rdioDuration'
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
rdioCurrencyCode = lens _rdioCurrencyCode (\ s a -> s{_rdioCurrencyCode = a});

-- | The database engine used by the offering.
rdioProductDescription :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioProductDescription = lens _rdioProductDescription (\ s a -> s{_rdioProductDescription = a});

-- | The DB instance class for the reserved DB instance.
rdioDBInstanceClass :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioDBInstanceClass = lens _rdioDBInstanceClass (\ s a -> s{_rdioDBInstanceClass = a});

-- | Indicates if the offering applies to Multi-AZ deployments.
rdioMultiAZ :: Lens' ReservedDBInstancesOffering (Maybe Bool)
rdioMultiAZ = lens _rdioMultiAZ (\ s a -> s{_rdioMultiAZ = a});

-- | The offering identifier.
rdioReservedDBInstancesOfferingId :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioReservedDBInstancesOfferingId = lens _rdioReservedDBInstancesOfferingId (\ s a -> s{_rdioReservedDBInstancesOfferingId = a});

-- | The recurring price charged to run this reserved DB instance.
rdioRecurringCharges :: Lens' ReservedDBInstancesOffering [RecurringCharge]
rdioRecurringCharges = lens _rdioRecurringCharges (\ s a -> s{_rdioRecurringCharges = a}) . _Default . _Coerce;

-- | The offering type.
rdioOfferingType :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioOfferingType = lens _rdioOfferingType (\ s a -> s{_rdioOfferingType = a});

-- | The hourly price charged for this offering.
rdioUsagePrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdioUsagePrice = lens _rdioUsagePrice (\ s a -> s{_rdioUsagePrice = a});

-- | The fixed price charged for this offering.
rdioFixedPrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdioFixedPrice = lens _rdioFixedPrice (\ s a -> s{_rdioFixedPrice = a});

-- | The duration of the offering in seconds.
rdioDuration :: Lens' ReservedDBInstancesOffering (Maybe Int)
rdioDuration = lens _rdioDuration (\ s a -> s{_rdioDuration = a});

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

-- | Describes the pending maintenance actions for a resource.
--
-- /See:/ 'resourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
    { _rpmaPendingMaintenanceActionDetails :: !(Maybe [PendingMaintenanceAction])
    , _rpmaResourceIdentifier              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourcePendingMaintenanceActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpmaPendingMaintenanceActionDetails'
--
-- * 'rpmaResourceIdentifier'
resourcePendingMaintenanceActions
    :: ResourcePendingMaintenanceActions
resourcePendingMaintenanceActions =
    ResourcePendingMaintenanceActions'
    { _rpmaPendingMaintenanceActionDetails = Nothing
    , _rpmaResourceIdentifier = Nothing
    }

-- | A list that provides details about the pending maintenance actions for
-- the resource.
rpmaPendingMaintenanceActionDetails :: Lens' ResourcePendingMaintenanceActions [PendingMaintenanceAction]
rpmaPendingMaintenanceActionDetails = lens _rpmaPendingMaintenanceActionDetails (\ s a -> s{_rpmaPendingMaintenanceActionDetails = a}) . _Default . _Coerce;

-- | The ARN of the resource that has pending maintenance actions.
rpmaResourceIdentifier :: Lens' ResourcePendingMaintenanceActions (Maybe Text)
rpmaResourceIdentifier = lens _rpmaResourceIdentifier (\ s a -> s{_rpmaResourceIdentifier = a});

instance FromXML ResourcePendingMaintenanceActions
         where
        parseXML x
          = ResourcePendingMaintenanceActions' <$>
              (x .@? "PendingMaintenanceActionDetails" .!@ mempty
                 >>= may (parseXMLList "PendingMaintenanceAction"))
                <*> (x .@? "ResourceIdentifier")

-- | This data type is used as a response element in the
-- DescribeDBSubnetGroups action.
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
    { _sSubnetStatus           :: !(Maybe Text)
    , _sSubnetIdentifier       :: !(Maybe Text)
    , _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetStatus'
--
-- * 'sSubnetIdentifier'
--
-- * 'sSubnetAvailabilityZone'
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
sSubnetStatus = lens _sSubnetStatus (\ s a -> s{_sSubnetStatus = a});

-- | Specifies the identifier of the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a});

-- | Undocumented member.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a});

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "SubnetStatus") <*> (x .@? "SubnetIdentifier")
                <*> (x .@? "SubnetAvailabilityZone")

-- | Metadata assigned to an Amazon RDS resource consisting of a key-value
-- pair.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue'
--
-- * 'tagKey'
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | A value is the optional value of the tag. The string value can be from 1
-- to 256 Unicode characters in length and cannot be prefixed with \"aws:\"
-- or \"rds:\". The string can only contain only the set of Unicode
-- letters, digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\'
-- (Java regex: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | A key is the required name of the tag. The string value can be from 1 to
-- 128 Unicode characters in length and cannot be prefixed with \"aws:\" or
-- \"rds:\". The string can only contain only the set of Unicode letters,
-- digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\' (Java
-- regex: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | The version of the database engine that a DB instance can be upgraded
-- to.
--
-- /See:/ 'upgradeTarget' smart constructor.
data UpgradeTarget = UpgradeTarget'
    { _utEngineVersion         :: !(Maybe Text)
    , _utIsMajorVersionUpgrade :: !(Maybe Bool)
    , _utEngine                :: !(Maybe Text)
    , _utAutoUpgrade           :: !(Maybe Bool)
    , _utDescription           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpgradeTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utEngineVersion'
--
-- * 'utIsMajorVersionUpgrade'
--
-- * 'utEngine'
--
-- * 'utAutoUpgrade'
--
-- * 'utDescription'
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
utEngineVersion = lens _utEngineVersion (\ s a -> s{_utEngineVersion = a});

-- | A value that indicates whether a database engine will be upgraded to a
-- major version.
utIsMajorVersionUpgrade :: Lens' UpgradeTarget (Maybe Bool)
utIsMajorVersionUpgrade = lens _utIsMajorVersionUpgrade (\ s a -> s{_utIsMajorVersionUpgrade = a});

-- | The name of the upgrade target database engine.
utEngine :: Lens' UpgradeTarget (Maybe Text)
utEngine = lens _utEngine (\ s a -> s{_utEngine = a});

-- | A value that indicates whether the target version will be applied to any
-- source DB instances that have AutoMinorVersionUpgrade set to true.
utAutoUpgrade :: Lens' UpgradeTarget (Maybe Bool)
utAutoUpgrade = lens _utAutoUpgrade (\ s a -> s{_utAutoUpgrade = a});

-- | The version of the database engine that a DB instance can be upgraded
-- to.
utDescription :: Lens' UpgradeTarget (Maybe Text)
utDescription = lens _utDescription (\ s a -> s{_utDescription = a});

instance FromXML UpgradeTarget where
        parseXML x
          = UpgradeTarget' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "IsMajorVersionUpgrade")
                <*> (x .@? "Engine")
                <*> (x .@? "AutoUpgrade")
                <*> (x .@? "Description")

-- | This data type is used as a response element for queries on VPC security
-- group membership.
--
-- /See:/ 'vpcSecurityGroupMembership' smart constructor.
data VPCSecurityGroupMembership = VPCSecurityGroupMembership'
    { _vsgmStatus             :: !(Maybe Text)
    , _vsgmVPCSecurityGroupId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VPCSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsgmStatus'
--
-- * 'vsgmVPCSecurityGroupId'
vpcSecurityGroupMembership
    :: VPCSecurityGroupMembership
vpcSecurityGroupMembership =
    VPCSecurityGroupMembership'
    { _vsgmStatus = Nothing
    , _vsgmVPCSecurityGroupId = Nothing
    }

-- | The status of the VPC security group.
vsgmStatus :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmStatus = lens _vsgmStatus (\ s a -> s{_vsgmStatus = a});

-- | The name of the VPC security group.
vsgmVPCSecurityGroupId :: Lens' VPCSecurityGroupMembership (Maybe Text)
vsgmVPCSecurityGroupId = lens _vsgmVPCSecurityGroupId (\ s a -> s{_vsgmVPCSecurityGroupId = a});

instance FromXML VPCSecurityGroupMembership where
        parseXML x
          = VPCSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "VpcSecurityGroupId")
