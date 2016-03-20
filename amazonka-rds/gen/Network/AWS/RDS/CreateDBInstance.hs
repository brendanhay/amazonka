{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance.
module Network.AWS.RDS.CreateDBInstance
    (
    -- * Creating a Request
      createDBInstance
    , CreateDBInstance
    -- * Request Lenses
    , cdiEngineVersion
    , cdiDBSecurityGroups
    , cdiStorageEncrypted
    , cdiDBClusterIdentifier
    , cdiMasterUserPassword
    , cdiPubliclyAccessible
    , cdiAutoMinorVersionUpgrade
    , cdiMasterUsername
    , cdiDBSubnetGroupName
    , cdiMonitoringRoleARN
    , cdiIOPS
    , cdiMonitoringInterval
    , cdiTDECredentialPassword
    , cdiLicenseModel
    , cdiPreferredMaintenanceWindow
    , cdiCharacterSetName
    , cdiKMSKeyId
    , cdiDBParameterGroupName
    , cdiPreferredBackupWindow
    , cdiAvailabilityZone
    , cdiBackupRetentionPeriod
    , cdiVPCSecurityGroupIds
    , cdiMultiAZ
    , cdiAllocatedStorage
    , cdiOptionGroupName
    , cdiCopyTagsToSnapshot
    , cdiTDECredentialARN
    , cdiTags
    , cdiPort
    , cdiStorageType
    , cdiDBName
    , cdiDBInstanceIdentifier
    , cdiDBInstanceClass
    , cdiEngine

    -- * Destructuring the Response
    , createDBInstanceResponse
    , CreateDBInstanceResponse
    -- * Response Lenses
    , cdirsDBInstance
    , cdirsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
    { _cdiEngineVersion              :: !(Maybe Text)
    , _cdiDBSecurityGroups           :: !(Maybe [Text])
    , _cdiStorageEncrypted           :: !(Maybe Bool)
    , _cdiDBClusterIdentifier        :: !(Maybe Text)
    , _cdiMasterUserPassword         :: !(Maybe Text)
    , _cdiPubliclyAccessible         :: !(Maybe Bool)
    , _cdiAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _cdiMasterUsername             :: !(Maybe Text)
    , _cdiDBSubnetGroupName          :: !(Maybe Text)
    , _cdiMonitoringRoleARN          :: !(Maybe Text)
    , _cdiIOPS                       :: !(Maybe Int)
    , _cdiMonitoringInterval         :: !(Maybe Int)
    , _cdiTDECredentialPassword      :: !(Maybe Text)
    , _cdiLicenseModel               :: !(Maybe Text)
    , _cdiPreferredMaintenanceWindow :: !(Maybe Text)
    , _cdiCharacterSetName           :: !(Maybe Text)
    , _cdiKMSKeyId                   :: !(Maybe Text)
    , _cdiDBParameterGroupName       :: !(Maybe Text)
    , _cdiPreferredBackupWindow      :: !(Maybe Text)
    , _cdiAvailabilityZone           :: !(Maybe Text)
    , _cdiBackupRetentionPeriod      :: !(Maybe Int)
    , _cdiVPCSecurityGroupIds        :: !(Maybe [Text])
    , _cdiMultiAZ                    :: !(Maybe Bool)
    , _cdiAllocatedStorage           :: !(Maybe Int)
    , _cdiOptionGroupName            :: !(Maybe Text)
    , _cdiCopyTagsToSnapshot         :: !(Maybe Bool)
    , _cdiTDECredentialARN           :: !(Maybe Text)
    , _cdiTags                       :: !(Maybe [Tag])
    , _cdiPort                       :: !(Maybe Int)
    , _cdiStorageType                :: !(Maybe Text)
    , _cdiDBName                     :: !(Maybe Text)
    , _cdiDBInstanceIdentifier       :: !Text
    , _cdiDBInstanceClass            :: !Text
    , _cdiEngine                     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdiEngineVersion'
--
-- * 'cdiDBSecurityGroups'
--
-- * 'cdiStorageEncrypted'
--
-- * 'cdiDBClusterIdentifier'
--
-- * 'cdiMasterUserPassword'
--
-- * 'cdiPubliclyAccessible'
--
-- * 'cdiAutoMinorVersionUpgrade'
--
-- * 'cdiMasterUsername'
--
-- * 'cdiDBSubnetGroupName'
--
-- * 'cdiMonitoringRoleARN'
--
-- * 'cdiIOPS'
--
-- * 'cdiMonitoringInterval'
--
-- * 'cdiTDECredentialPassword'
--
-- * 'cdiLicenseModel'
--
-- * 'cdiPreferredMaintenanceWindow'
--
-- * 'cdiCharacterSetName'
--
-- * 'cdiKMSKeyId'
--
-- * 'cdiDBParameterGroupName'
--
-- * 'cdiPreferredBackupWindow'
--
-- * 'cdiAvailabilityZone'
--
-- * 'cdiBackupRetentionPeriod'
--
-- * 'cdiVPCSecurityGroupIds'
--
-- * 'cdiMultiAZ'
--
-- * 'cdiAllocatedStorage'
--
-- * 'cdiOptionGroupName'
--
-- * 'cdiCopyTagsToSnapshot'
--
-- * 'cdiTDECredentialARN'
--
-- * 'cdiTags'
--
-- * 'cdiPort'
--
-- * 'cdiStorageType'
--
-- * 'cdiDBName'
--
-- * 'cdiDBInstanceIdentifier'
--
-- * 'cdiDBInstanceClass'
--
-- * 'cdiEngine'
createDBInstance
    :: Text -- ^ 'cdiDBInstanceIdentifier'
    -> Text -- ^ 'cdiDBInstanceClass'
    -> Text -- ^ 'cdiEngine'
    -> CreateDBInstance
createDBInstance pDBInstanceIdentifier_ pDBInstanceClass_ pEngine_ =
    CreateDBInstance'
    { _cdiEngineVersion = Nothing
    , _cdiDBSecurityGroups = Nothing
    , _cdiStorageEncrypted = Nothing
    , _cdiDBClusterIdentifier = Nothing
    , _cdiMasterUserPassword = Nothing
    , _cdiPubliclyAccessible = Nothing
    , _cdiAutoMinorVersionUpgrade = Nothing
    , _cdiMasterUsername = Nothing
    , _cdiDBSubnetGroupName = Nothing
    , _cdiMonitoringRoleARN = Nothing
    , _cdiIOPS = Nothing
    , _cdiMonitoringInterval = Nothing
    , _cdiTDECredentialPassword = Nothing
    , _cdiLicenseModel = Nothing
    , _cdiPreferredMaintenanceWindow = Nothing
    , _cdiCharacterSetName = Nothing
    , _cdiKMSKeyId = Nothing
    , _cdiDBParameterGroupName = Nothing
    , _cdiPreferredBackupWindow = Nothing
    , _cdiAvailabilityZone = Nothing
    , _cdiBackupRetentionPeriod = Nothing
    , _cdiVPCSecurityGroupIds = Nothing
    , _cdiMultiAZ = Nothing
    , _cdiAllocatedStorage = Nothing
    , _cdiOptionGroupName = Nothing
    , _cdiCopyTagsToSnapshot = Nothing
    , _cdiTDECredentialARN = Nothing
    , _cdiTags = Nothing
    , _cdiPort = Nothing
    , _cdiStorageType = Nothing
    , _cdiDBName = Nothing
    , _cdiDBInstanceIdentifier = pDBInstanceIdentifier_
    , _cdiDBInstanceClass = pDBInstanceClass_
    , _cdiEngine = pEngine_
    }

-- | The version number of the database engine to use.
--
-- The following are the database engines and major and minor versions that
-- are available with Amazon RDS. Not every database engine is available
-- for every AWS region.
--
-- __MySQL__
--
-- -   __Version 5.1 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ ' 5.1.73a | 5.1.73b'
-- -   __Version 5.5 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ ' 5.5.40 | 5.5.40a'
-- -   __Version 5.5 (Available in all regions):__
--     ' 5.5.40b | 5.5.41 | 5.5.42'
-- -   __Version 5.6 (Available in all regions):__
--     ' 5.6.19a | 5.6.19b | 5.6.21 | 5.6.21b | 5.6.22 | 5.6.23'
-- -   __Version 5.7 (Available in all regions):__ ' 5.7.10'
--
-- __MariaDB__
--
-- -   __Version 10.0 (Available in all regions except AWS GovCloud (US)
--     Region (us-gov-west-1)):__ ' 10.0.17 '
--
-- __Oracle Database Enterprise Edition (oracle-ee)__
--
-- -   __Version 11.2 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__
--     ' 11.2.0.2.v3 | 11.2.0.2.v4 | 11.2.0.2.v5 | 11.2.0.2.v6 | 11.2.0.2.v7'
-- -   __Version 11.2 (Available in all regions):__
--     ' 11.2.0.3.v1 | 11.2.0.3.v2 | 11.2.0.3.v3 | 11.2.0.4.v1 | 11.2.0.4.v3 | 11.2.0.4.v4'
-- -   __Version 12.1 (Available in all regions):__
--     '12.1.0.1.v1 | 12.1.0.1.v2 | 12.1.0.2.v1 '
--
-- __Oracle Database Standard Edition (oracle-se)__
--
-- -   __Version 11.2 (Only available in the following regions:
--     us-west-1):__
--     ' 11.2.0.2.v3 | 11.2.0.2.v4 | 11.2.0.2.v5 | 11.2.0.2.v6 | 11.2.0.2.v7'
-- -   __Version 11.2 (Only available in the following regions:
--     eu-central-1, us-west-1):__
--     ' 11.2.0.3.v1 | 11.2.0.3.v2 | 11.2.0.3.v3 | 11.2.0.4.v1 | 11.2.0.4.v3 | 11.2.0.4.v4'
-- -   __Version 12.1 (Only available in the following regions:
--     eu-central-1, us-west-1):__ '12.1.0.1.v1 | 12.1.0.1.v2'
--
-- __Oracle Database Standard Edition One (oracle-se1)__
--
-- -   __Version 11.2 (Only available in the following regions:
--     us-west-1):__
--     ' 11.2.0.2.v3 | 11.2.0.2.v4 | 11.2.0.2.v5 | 11.2.0.2.v6 | 11.2.0.2.v7'
-- -   __Version 11.2 (Only available in the following regions:
--     eu-central-1, us-west-1):__
--     ' 11.2.0.3.v1 | 11.2.0.3.v2 | 11.2.0.3.v3 | 11.2.0.4.v1 | 11.2.0.4.v3 | 11.2.0.4.v4'
-- -   __Version 12.1 (Only available in the following regions:
--     eu-central-1, us-west-1):__ ' 12.1.0.1.v1 | 12.1.0.1.v2'
--
-- __PostgreSQL__
--
-- -   __Version 9.3 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ ' 9.3.1 | 9.3.2'
-- -   __Version 9.3 (Available in all regions):__
--     ' 9.3.3 | 9.3.5 | 9.3.6 | 9.3.9 | 9.3.10'
-- -   __Version 9.4 (Available in all regions):__ ' 9.4.1 | 9.4.4 | 9.4.5'
--
-- __Microsoft SQL Server Enterprise Edition (sqlserver-ee)__
--
-- -   __Version 10.50 (Available in all regions):__ ' 10.50.2789.0.v1'
-- -   __Version 10.50 (Available in all regions):__ ' 10.50.6000.34.v1'
-- -   __Version 11.00 (Available in all regions):__ ' 11.00.2100.60.v1'
-- -   __Version 11.00 (Available in all regions):__ ' 11.00.5058.0.v1'
--
-- __Microsoft SQL Server Express Edition (sqlserver-ex)__
--
-- -   __Version 10.50 (Available in all regions):__ ' 10.50.2789.0.v1'
-- -   __Version 10.50 (Available in all regions):__ ' 10.50.6000.34.v1'
-- -   __Version 11.00 (Available in all regions):__ ' 11.00.2100.60.v1'
-- -   __Version 11.00 (Available in all regions):__ ' 11.00.5058.0.v1'
-- -   __Version 12.00 (Available in all regions):__ ' 12.00.4422.0.v1'
--
-- __Microsoft SQL Server Standard Edition (sqlserver-se)__
--
-- -   __Version 10.50 (Available in all regions):__ ' 10.50.2789.0.v1'
-- -   __Version 10.50 (Available in all regions):__ ' 10.50.6000.34.v1'
-- -   __Version 11.00 (Available in all regions):__ ' 11.00.2100.60.v1'
-- -   __Version 11.00 (Available in all regions):__ ' 11.00.5058.0.v1'
-- -   __Version 12.00 (Available in all regions):__ ' 12.00.4422.0.v1'
--
-- __Microsoft SQL Server Web Edition (sqlserver-web)__
--
-- -   __Version 10.50 (Available in all regions):__ ' 10.50.2789.0.v1'
-- -   __Version 10.50 (Available in all regions):__ ' 10.50.6000.34.v1'
-- -   __Version 11.00 (Available in all regions):__ ' 11.00.2100.60.v1'
-- -   __Version 11.00 (Available in all regions):__ ' 11.00.5058.0.v1'
-- -   __Version 12.00 (Available in all regions):__ ' 12.00.4422.0.v1'
cdiEngineVersion :: Lens' CreateDBInstance (Maybe Text)
cdiEngineVersion = lens _cdiEngineVersion (\ s a -> s{_cdiEngineVersion = a});

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
cdiDBSecurityGroups :: Lens' CreateDBInstance [Text]
cdiDBSecurityGroups = lens _cdiDBSecurityGroups (\ s a -> s{_cdiDBSecurityGroups = a}) . _Default . _Coerce;

-- | Specifies whether the DB instance is encrypted.
--
-- Default: false
cdiStorageEncrypted :: Lens' CreateDBInstance (Maybe Bool)
cdiStorageEncrypted = lens _cdiStorageEncrypted (\ s a -> s{_cdiStorageEncrypted = a});

-- | The identifier of the DB cluster that the instance will belong to.
--
-- For information on creating a DB cluster, see < CreateDBCluster>.
--
-- Type: String
cdiDBClusterIdentifier :: Lens' CreateDBInstance (Maybe Text)
cdiDBClusterIdentifier = lens _cdiDBClusterIdentifier (\ s a -> s{_cdiDBClusterIdentifier = a});

-- | The password for the master database user. Can be any printable ASCII
-- character except \"\/\", \"\"\", or \"\'\".
--
-- Type: String
--
-- __MySQL__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __MariaDB__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __Oracle__
--
-- Constraints: Must contain from 8 to 30 characters.
--
-- __SQL Server__
--
-- Constraints: Must contain from 8 to 128 characters.
--
-- __PostgreSQL__
--
-- Constraints: Must contain from 8 to 128 characters.
--
-- __Amazon Aurora__
--
-- Constraints: Must contain from 8 to 41 characters.
cdiMasterUserPassword :: Lens' CreateDBInstance (Maybe Text)
cdiMasterUserPassword = lens _cdiMasterUserPassword (\ s a -> s{_cdiMasterUserPassword = a});

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
-- -   __Default VPC:__ true
-- -   __VPC:__ false
--
-- If no DB subnet group has been specified as part of the request and the
-- PubliclyAccessible value has not been set, the DB instance will be
-- publicly accessible. If a specific DB subnet group has been specified as
-- part of the request and the PubliclyAccessible value has not been set,
-- the DB instance will be private.
cdiPubliclyAccessible :: Lens' CreateDBInstance (Maybe Bool)
cdiPubliclyAccessible = lens _cdiPubliclyAccessible (\ s a -> s{_cdiPubliclyAccessible = a});

-- | Indicates that minor engine upgrades will be applied automatically to
-- the DB instance during the maintenance window.
--
-- Default: 'true'
cdiAutoMinorVersionUpgrade :: Lens' CreateDBInstance (Maybe Bool)
cdiAutoMinorVersionUpgrade = lens _cdiAutoMinorVersionUpgrade (\ s a -> s{_cdiAutoMinorVersionUpgrade = a});

-- | The name of master user for the client DB instance.
--
-- __MySQL__
--
-- Constraints:
--
-- -   Must be 1 to 16 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot be a reserved word for the chosen database engine.
--
-- __MariaDB__
--
-- Constraints:
--
-- -   Must be 1 to 16 alphanumeric characters.
-- -   Cannot be a reserved word for the chosen database engine.
--
-- Type: String
--
-- __Oracle__
--
-- Constraints:
--
-- -   Must be 1 to 30 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot be a reserved word for the chosen database engine.
--
-- __SQL Server__
--
-- Constraints:
--
-- -   Must be 1 to 128 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot be a reserved word for the chosen database engine.
--
-- __PostgreSQL__
--
-- Constraints:
--
-- -   Must be 1 to 63 alphanumeric characters.
-- -   First character must be a letter.
-- -   Cannot be a reserved word for the chosen database engine.
cdiMasterUsername :: Lens' CreateDBInstance (Maybe Text)
cdiMasterUsername = lens _cdiMasterUsername (\ s a -> s{_cdiMasterUsername = a});

-- | A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
cdiDBSubnetGroupName :: Lens' CreateDBInstance (Maybe Text)
cdiDBSubnetGroupName = lens _cdiDBSubnetGroupName (\ s a -> s{_cdiDBSubnetGroupName = a});

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to CloudWatch Logs. For example,
-- 'arn:aws:iam:123456789012:role\/emaccess'. For information on creating a
-- monitoring role, go to
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>.
--
-- If 'MonitoringInterval' is set to a value other than 0, then you must
-- supply a 'MonitoringRoleArn' value.
cdiMonitoringRoleARN :: Lens' CreateDBInstance (Maybe Text)
cdiMonitoringRoleARN = lens _cdiMonitoringRoleARN (\ s a -> s{_cdiMonitoringRoleARN = a});

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- Constraints: Must be a multiple between 3 and 10 of the storage amount
-- for the DB instance. Must also be an integer multiple of 1000. For
-- example, if the size of your DB instance is 500 GB, then your 'Iops'
-- value can be 2000, 3000, 4000, or 5000.
cdiIOPS :: Lens' CreateDBInstance (Maybe Int)
cdiIOPS = lens _cdiIOPS (\ s a -> s{_cdiIOPS = a});

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 60.
--
-- If 'MonitoringRoleArn' is specified, then you must also set
-- 'MonitoringInterval' to a value other than 0.
--
-- Valid Values: '0, 1, 5, 10, 15, 30, 60'
cdiMonitoringInterval :: Lens' CreateDBInstance (Maybe Int)
cdiMonitoringInterval = lens _cdiMonitoringInterval (\ s a -> s{_cdiMonitoringInterval = a});

-- | The password for the given ARN from the Key Store in order to access the
-- device.
cdiTDECredentialPassword :: Lens' CreateDBInstance (Maybe Text)
cdiTDECredentialPassword = lens _cdiTDECredentialPassword (\ s a -> s{_cdiTDECredentialPassword = a});

-- | License model information for this DB instance.
--
-- Valid values: 'license-included' | 'bring-your-own-license' |
-- 'general-public-license'
cdiLicenseModel :: Lens' CreateDBInstance (Maybe Text)
cdiLicenseModel = lens _cdiLicenseModel (\ s a -> s{_cdiLicenseModel = a});

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC). For more information, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBMaintenance.html DB Instance Maintenance>.
--
-- Format: 'ddd:hh24:mi-ddd:hh24:mi'
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region, occurring on a random day of the week. To see the time
-- blocks available, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
cdiPreferredMaintenanceWindow :: Lens' CreateDBInstance (Maybe Text)
cdiPreferredMaintenanceWindow = lens _cdiPreferredMaintenanceWindow (\ s a -> s{_cdiPreferredMaintenanceWindow = a});

-- | For supported engines, indicates that the DB instance should be
-- associated with the specified CharacterSet.
cdiCharacterSetName :: Lens' CreateDBInstance (Maybe Text)
cdiCharacterSetName = lens _cdiCharacterSetName (\ s a -> s{_cdiCharacterSetName = a});

-- | The KMS key identifier for an encrypted DB instance.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a DB instance with the same AWS
-- account that owns the KMS encryption key used to encrypt the new DB
-- instance, then you can use the KMS key alias instead of the ARN for the
-- KM encryption key.
--
-- If the 'StorageEncrypted' parameter is true, and you do not specify a
-- value for the 'KmsKeyId' parameter, then Amazon RDS will use your
-- default encryption key. AWS KMS creates the default encryption key for
-- your AWS account. Your AWS account has a different default encryption
-- key for each AWS region.
cdiKMSKeyId :: Lens' CreateDBInstance (Maybe Text)
cdiKMSKeyId = lens _cdiKMSKeyId (\ s a -> s{_cdiKMSKeyId = a});

-- | The name of the DB parameter group to associate with this DB instance.
-- If this argument is omitted, the default DBParameterGroup for the
-- specified engine will be used.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
cdiDBParameterGroupName :: Lens' CreateDBInstance (Maybe Text)
cdiDBParameterGroupName = lens _cdiDBParameterGroupName (\ s a -> s{_cdiDBParameterGroupName = a});

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the 'BackupRetentionPeriod'
-- parameter. For more information, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.BackingUpAndRestoringAmazonRDSInstances.html DB Instance Backups>.
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region. To see the time blocks available, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Constraints:
--
-- -   Must be in the format 'hh24:mi-hh24:mi'.
-- -   Times should be in Universal Coordinated Time (UTC).
-- -   Must not conflict with the preferred maintenance window.
-- -   Must be at least 30 minutes.
cdiPreferredBackupWindow :: Lens' CreateDBInstance (Maybe Text)
cdiPreferredBackupWindow = lens _cdiPreferredBackupWindow (\ s a -> s{_cdiPreferredBackupWindow = a});

-- | The EC2 Availability Zone that the database instance will be created in.
-- For information on regions and Availability Zones, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- region.
--
-- Example: 'us-east-1d'
--
-- Constraint: The AvailabilityZone parameter cannot be specified if the
-- MultiAZ parameter is set to 'true'. The specified Availability Zone must
-- be in the same region as the current endpoint.
cdiAvailabilityZone :: Lens' CreateDBInstance (Maybe Text)
cdiAvailabilityZone = lens _cdiAvailabilityZone (\ s a -> s{_cdiAvailabilityZone = a});

-- | The number of days for which automated backups are retained. Setting
-- this parameter to a positive number enables backups. Setting this
-- parameter to 0 disables automated backups.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 0 to 35
-- -   Cannot be set to 0 if the DB instance is a source to Read Replicas
cdiBackupRetentionPeriod :: Lens' CreateDBInstance (Maybe Int)
cdiBackupRetentionPeriod = lens _cdiBackupRetentionPeriod (\ s a -> s{_cdiBackupRetentionPeriod = a});

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
cdiVPCSecurityGroupIds :: Lens' CreateDBInstance [Text]
cdiVPCSecurityGroupIds = lens _cdiVPCSecurityGroupIds (\ s a -> s{_cdiVPCSecurityGroupIds = a}) . _Default . _Coerce;

-- | Specifies if the DB instance is a Multi-AZ deployment. You cannot set
-- the AvailabilityZone parameter if the MultiAZ parameter is set to true.
-- Do not set this value if you want a Multi-AZ deployment for a SQL Server
-- DB instance. Multi-AZ for SQL Server is set using the Mirroring option
-- in an option group.
cdiMultiAZ :: Lens' CreateDBInstance (Maybe Bool)
cdiMultiAZ = lens _cdiMultiAZ (\ s a -> s{_cdiMultiAZ = a});

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- database instance.
--
-- Type: Integer
--
-- __MySQL__
--
-- Constraints: Must be an integer from 5 to 6144.
--
-- __MariaDB__
--
-- Constraints: Must be an integer from 5 to 6144.
--
-- __PostgreSQL__
--
-- Constraints: Must be an integer from 5 to 6144.
--
-- __Oracle__
--
-- Constraints: Must be an integer from 10 to 6144.
--
-- __SQL Server__
--
-- Constraints: Must be an integer from 200 to 4096 (Standard Edition and
-- Enterprise Edition) or from 20 to 4096 (Express Edition and Web Edition)
cdiAllocatedStorage :: Lens' CreateDBInstance (Maybe Int)
cdiAllocatedStorage = lens _cdiAllocatedStorage (\ s a -> s{_cdiAllocatedStorage = a});

-- | Indicates that the DB instance should be associated with the specified
-- option group.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group
-- cannot be removed from a DB instance once it is associated with a DB
-- instance
cdiOptionGroupName :: Lens' CreateDBInstance (Maybe Text)
cdiOptionGroupName = lens _cdiOptionGroupName (\ s a -> s{_cdiOptionGroupName = a});

-- | True to copy all tags from the DB instance to snapshots of the DB
-- instance; otherwise false. The default is false.
cdiCopyTagsToSnapshot :: Lens' CreateDBInstance (Maybe Bool)
cdiCopyTagsToSnapshot = lens _cdiCopyTagsToSnapshot (\ s a -> s{_cdiCopyTagsToSnapshot = a});

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
cdiTDECredentialARN :: Lens' CreateDBInstance (Maybe Text)
cdiTDECredentialARN = lens _cdiTDECredentialARN (\ s a -> s{_cdiTDECredentialARN = a});

-- | Undocumented member.
cdiTags :: Lens' CreateDBInstance [Tag]
cdiTags = lens _cdiTags (\ s a -> s{_cdiTags = a}) . _Default . _Coerce;

-- | The port number on which the database accepts connections.
--
-- __MySQL__
--
-- Default: '3306'
--
-- Valid Values: '1150-65535'
--
-- Type: Integer
--
-- __MariaDB__
--
-- Default: '3306'
--
-- Valid Values: '1150-65535'
--
-- Type: Integer
--
-- __PostgreSQL__
--
-- Default: '5432'
--
-- Valid Values: '1150-65535'
--
-- Type: Integer
--
-- __Oracle__
--
-- Default: '1521'
--
-- Valid Values: '1150-65535'
--
-- __SQL Server__
--
-- Default: '1433'
--
-- Valid Values: '1150-65535' except for '1434', '3389', '47001', '49152',
-- and '49152' through '49156'.
--
-- __Amazon Aurora__
--
-- Default: '3306'
--
-- Valid Values: '1150-65535'
--
-- Type: Integer
cdiPort :: Lens' CreateDBInstance (Maybe Int)
cdiPort = lens _cdiPort (\ s a -> s{_cdiPort = a});

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: 'standard | gp2 | io1'
--
-- If you specify 'io1', you must also include a value for the 'Iops'
-- parameter.
--
-- Default: 'io1' if the 'Iops' parameter is specified; otherwise
-- 'standard'
cdiStorageType :: Lens' CreateDBInstance (Maybe Text)
cdiStorageType = lens _cdiStorageType (\ s a -> s{_cdiStorageType = a});

-- | The meaning of this parameter differs according to the database engine
-- you use.
--
-- Type: String
--
-- __MySQL__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter is not specified, no database is created in the DB
-- instance.
--
-- Constraints:
--
-- -   Must contain 1 to 64 alphanumeric characters
-- -   Cannot be a word reserved by the specified database engine
--
-- __MariaDB__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter is not specified, no database is created in the DB
-- instance.
--
-- Constraints:
--
-- -   Must contain 1 to 64 alphanumeric characters
-- -   Cannot be a word reserved by the specified database engine
--
-- __PostgreSQL__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter is not specified, the default \"postgres\" database is
-- created in the DB instance.
--
-- Constraints:
--
-- -   Must contain 1 to 63 alphanumeric characters
-- -   Must begin with a letter or an underscore. Subsequent characters can
--     be letters, underscores, or digits (0-9).
-- -   Cannot be a word reserved by the specified database engine
--
-- __Oracle__
--
-- The Oracle System ID (SID) of the created DB instance.
--
-- Default: 'ORCL'
--
-- Constraints:
--
-- -   Cannot be longer than 8 characters
--
-- __SQL Server__
--
-- Not applicable. Must be null.
--
-- __Amazon Aurora__
--
-- The name of the database to create when the primary instance of the DB
-- cluster is created. If this parameter is not specified, no database is
-- created in the DB instance.
--
-- Constraints:
--
-- -   Must contain 1 to 64 alphanumeric characters
-- -   Cannot be a word reserved by the specified database engine
cdiDBName :: Lens' CreateDBInstance (Maybe Text)
cdiDBName = lens _cdiDBName (\ s a -> s{_cdiDBName = a});

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens (1 to
--     15 for SQL Server).
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: 'mydbinstance'
cdiDBInstanceIdentifier :: Lens' CreateDBInstance Text
cdiDBInstanceIdentifier = lens _cdiDBInstanceIdentifier (\ s a -> s{_cdiDBInstanceIdentifier = a});

-- | The compute and memory capacity of the DB instance.
--
-- Valid Values:
-- 'db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.m4.large | db.m4.xlarge | db.m4.2xlarge | db.m4.4xlarge | db.m4.10xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium | db.t2.large'
cdiDBInstanceClass :: Lens' CreateDBInstance Text
cdiDBInstanceClass = lens _cdiDBInstanceClass (\ s a -> s{_cdiDBInstanceClass = a});

-- | The name of the database engine to be used for this instance.
--
-- Valid Values: 'MySQL' | 'mariadb' | 'oracle-se1' | 'oracle-se' |
-- 'oracle-ee' | 'sqlserver-ee' | 'sqlserver-se' | 'sqlserver-ex' |
-- 'sqlserver-web' | 'postgres' | 'aurora'
--
-- Not every database engine is available for every AWS region.
cdiEngine :: Lens' CreateDBInstance Text
cdiEngine = lens _cdiEngine (\ s a -> s{_cdiEngine = a});

instance AWSRequest CreateDBInstance where
        type Rs CreateDBInstance = CreateDBInstanceResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper "CreateDBInstanceResult"
              (\ s h x ->
                 CreateDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable CreateDBInstance

instance ToHeaders CreateDBInstance where
        toHeaders = const mempty

instance ToPath CreateDBInstance where
        toPath = const "/"

instance ToQuery CreateDBInstance where
        toQuery CreateDBInstance'{..}
          = mconcat
              ["Action" =: ("CreateDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _cdiEngineVersion,
               "DBSecurityGroups" =:
                 toQuery
                   (toQueryList "DBSecurityGroupName" <$>
                      _cdiDBSecurityGroups),
               "StorageEncrypted" =: _cdiStorageEncrypted,
               "DBClusterIdentifier" =: _cdiDBClusterIdentifier,
               "MasterUserPassword" =: _cdiMasterUserPassword,
               "PubliclyAccessible" =: _cdiPubliclyAccessible,
               "AutoMinorVersionUpgrade" =:
                 _cdiAutoMinorVersionUpgrade,
               "MasterUsername" =: _cdiMasterUsername,
               "DBSubnetGroupName" =: _cdiDBSubnetGroupName,
               "MonitoringRoleArn" =: _cdiMonitoringRoleARN,
               "Iops" =: _cdiIOPS,
               "MonitoringInterval" =: _cdiMonitoringInterval,
               "TdeCredentialPassword" =: _cdiTDECredentialPassword,
               "LicenseModel" =: _cdiLicenseModel,
               "PreferredMaintenanceWindow" =:
                 _cdiPreferredMaintenanceWindow,
               "CharacterSetName" =: _cdiCharacterSetName,
               "KmsKeyId" =: _cdiKMSKeyId,
               "DBParameterGroupName" =: _cdiDBParameterGroupName,
               "PreferredBackupWindow" =: _cdiPreferredBackupWindow,
               "AvailabilityZone" =: _cdiAvailabilityZone,
               "BackupRetentionPeriod" =: _cdiBackupRetentionPeriod,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _cdiVPCSecurityGroupIds),
               "MultiAZ" =: _cdiMultiAZ,
               "AllocatedStorage" =: _cdiAllocatedStorage,
               "OptionGroupName" =: _cdiOptionGroupName,
               "CopyTagsToSnapshot" =: _cdiCopyTagsToSnapshot,
               "TdeCredentialArn" =: _cdiTDECredentialARN,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdiTags),
               "Port" =: _cdiPort, "StorageType" =: _cdiStorageType,
               "DBName" =: _cdiDBName,
               "DBInstanceIdentifier" =: _cdiDBInstanceIdentifier,
               "DBInstanceClass" =: _cdiDBInstanceClass,
               "Engine" =: _cdiEngine]

-- | /See:/ 'createDBInstanceResponse' smart constructor.
data CreateDBInstanceResponse = CreateDBInstanceResponse'
    { _cdirsDBInstance     :: !(Maybe DBInstance)
    , _cdirsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDBInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdirsDBInstance'
--
-- * 'cdirsResponseStatus'
createDBInstanceResponse
    :: Int -- ^ 'cdirsResponseStatus'
    -> CreateDBInstanceResponse
createDBInstanceResponse pResponseStatus_ =
    CreateDBInstanceResponse'
    { _cdirsDBInstance = Nothing
    , _cdirsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cdirsDBInstance :: Lens' CreateDBInstanceResponse (Maybe DBInstance)
cdirsDBInstance = lens _cdirsDBInstance (\ s a -> s{_cdirsDBInstance = a});

-- | The response status code.
cdirsResponseStatus :: Lens' CreateDBInstanceResponse Int
cdirsResponseStatus = lens _cdirsResponseStatus (\ s a -> s{_cdirsResponseStatus = a});
