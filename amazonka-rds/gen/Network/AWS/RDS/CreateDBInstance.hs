{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a new DB instance.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBInstance.html>
module Network.AWS.RDS.CreateDBInstance
    (
    -- * Request
      CreateDBInstance
    -- ** Request constructor
    , createDBInstance
    -- ** Request lenses
    , cdiDBSecurityGroups
    , cdiEngineVersion
    , cdiStorageEncrypted
    , cdiAutoMinorVersionUpgrade
    , cdiPubliclyAccessible
    , cdiDBSubnetGroupName
    , cdiIOPS
    , cdiTDECredentialPassword
    , cdiLicenseModel
    , cdiPreferredMaintenanceWindow
    , cdiCharacterSetName
    , cdiPreferredBackupWindow
    , cdiAvailabilityZone
    , cdiBackupRetentionPeriod
    , cdiKMSKeyId
    , cdiDBParameterGroupName
    , cdiVPCSecurityGroupIds
    , cdiMultiAZ
    , cdiTDECredentialARN
    , cdiOptionGroupName
    , cdiDBName
    , cdiTags
    , cdiPort
    , cdiStorageType
    , cdiDBInstanceIdentifier
    , cdiAllocatedStorage
    , cdiDBInstanceClass
    , cdiEngine
    , cdiMasterUsername
    , cdiMasterUserPassword

    -- * Response
    , CreateDBInstanceResponse
    -- ** Response constructor
    , createDBInstanceResponse
    -- ** Response lenses
    , cdirDBInstance
    , cdirStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
-- /See:/ 'createDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdiDBSecurityGroups'
--
-- * 'cdiEngineVersion'
--
-- * 'cdiStorageEncrypted'
--
-- * 'cdiAutoMinorVersionUpgrade'
--
-- * 'cdiPubliclyAccessible'
--
-- * 'cdiDBSubnetGroupName'
--
-- * 'cdiIOPS'
--
-- * 'cdiTDECredentialPassword'
--
-- * 'cdiLicenseModel'
--
-- * 'cdiPreferredMaintenanceWindow'
--
-- * 'cdiCharacterSetName'
--
-- * 'cdiPreferredBackupWindow'
--
-- * 'cdiAvailabilityZone'
--
-- * 'cdiBackupRetentionPeriod'
--
-- * 'cdiKMSKeyId'
--
-- * 'cdiDBParameterGroupName'
--
-- * 'cdiVPCSecurityGroupIds'
--
-- * 'cdiMultiAZ'
--
-- * 'cdiTDECredentialARN'
--
-- * 'cdiOptionGroupName'
--
-- * 'cdiDBName'
--
-- * 'cdiTags'
--
-- * 'cdiPort'
--
-- * 'cdiStorageType'
--
-- * 'cdiDBInstanceIdentifier'
--
-- * 'cdiAllocatedStorage'
--
-- * 'cdiDBInstanceClass'
--
-- * 'cdiEngine'
--
-- * 'cdiMasterUsername'
--
-- * 'cdiMasterUserPassword'
data CreateDBInstance = CreateDBInstance'{_cdiDBSecurityGroups :: Maybe [Text], _cdiEngineVersion :: Maybe Text, _cdiStorageEncrypted :: Maybe Bool, _cdiAutoMinorVersionUpgrade :: Maybe Bool, _cdiPubliclyAccessible :: Maybe Bool, _cdiDBSubnetGroupName :: Maybe Text, _cdiIOPS :: Maybe Int, _cdiTDECredentialPassword :: Maybe Text, _cdiLicenseModel :: Maybe Text, _cdiPreferredMaintenanceWindow :: Maybe Text, _cdiCharacterSetName :: Maybe Text, _cdiPreferredBackupWindow :: Maybe Text, _cdiAvailabilityZone :: Maybe Text, _cdiBackupRetentionPeriod :: Maybe Int, _cdiKMSKeyId :: Maybe Text, _cdiDBParameterGroupName :: Maybe Text, _cdiVPCSecurityGroupIds :: Maybe [Text], _cdiMultiAZ :: Maybe Bool, _cdiTDECredentialARN :: Maybe Text, _cdiOptionGroupName :: Maybe Text, _cdiDBName :: Maybe Text, _cdiTags :: Maybe [Tag], _cdiPort :: Maybe Int, _cdiStorageType :: Maybe Text, _cdiDBInstanceIdentifier :: Text, _cdiAllocatedStorage :: Int, _cdiDBInstanceClass :: Text, _cdiEngine :: Text, _cdiMasterUsername :: Text, _cdiMasterUserPassword :: Text} deriving (Eq, Read, Show)

-- | 'CreateDBInstance' smart constructor.
createDBInstance :: Text -> Int -> Text -> Text -> Text -> Text -> CreateDBInstance
createDBInstance pDBInstanceIdentifier pAllocatedStorage pDBInstanceClass pEngine pMasterUsername pMasterUserPassword = CreateDBInstance'{_cdiDBSecurityGroups = Nothing, _cdiEngineVersion = Nothing, _cdiStorageEncrypted = Nothing, _cdiAutoMinorVersionUpgrade = Nothing, _cdiPubliclyAccessible = Nothing, _cdiDBSubnetGroupName = Nothing, _cdiIOPS = Nothing, _cdiTDECredentialPassword = Nothing, _cdiLicenseModel = Nothing, _cdiPreferredMaintenanceWindow = Nothing, _cdiCharacterSetName = Nothing, _cdiPreferredBackupWindow = Nothing, _cdiAvailabilityZone = Nothing, _cdiBackupRetentionPeriod = Nothing, _cdiKMSKeyId = Nothing, _cdiDBParameterGroupName = Nothing, _cdiVPCSecurityGroupIds = Nothing, _cdiMultiAZ = Nothing, _cdiTDECredentialARN = Nothing, _cdiOptionGroupName = Nothing, _cdiDBName = Nothing, _cdiTags = Nothing, _cdiPort = Nothing, _cdiStorageType = Nothing, _cdiDBInstanceIdentifier = pDBInstanceIdentifier, _cdiAllocatedStorage = pAllocatedStorage, _cdiDBInstanceClass = pDBInstanceClass, _cdiEngine = pEngine, _cdiMasterUsername = pMasterUsername, _cdiMasterUserPassword = pMasterUserPassword};

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
cdiDBSecurityGroups :: Lens' CreateDBInstance [Text]
cdiDBSecurityGroups = lens _cdiDBSecurityGroups (\ s a -> s{_cdiDBSecurityGroups = a}) . _Default;

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
--     sa-east-1, us-west-1, us-west-2):__ @ 5.1.73a | 5.1.73b@
-- -   __Version 5.5 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ @ 5.5.40 | 5.5.40a@
-- -   __Version 5.5 (Available in all regions):__ @ 5.5.40b | 5.5.41@
-- -   __Version 5.6 (Available in all regions):__
--     @ 5.6.19a | 5.6.19b | 5.6.21 | 5.6.21b | 5.6.22@
--
-- __MySQL__
--
-- -   __Version 5.1 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ @ 5.1.73a | 5.1.73b@
-- -   __Version 5.5 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ @ 5.5.40 | 5.5.40a@
-- -   __Version 5.5 (Available in all regions):__ @ 5.5.40b | 5.5.41@
-- -   __Version 5.6 (Available in all regions):__
--     @ 5.6.19a | 5.6.19b | 5.6.21 | 5.6.21b | 5.6.22@
--
-- __MySQL__
--
-- -   __Version 5.1 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ @ 5.1.73a | 5.1.73b@
-- -   __Version 5.5 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ @ 5.5.40 | 5.5.40a@
-- -   __Version 5.5 (Available in all regions):__ @ 5.5.40b | 5.5.41@
-- -   __Version 5.6 (Available in all regions):__
--     @ 5.6.19a | 5.6.19b | 5.6.21 | 5.6.21b | 5.6.22@
--
-- __MySQL__
--
-- -   __Version 5.1 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ @ 5.1.73a | 5.1.73b@
-- -   __Version 5.5 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ @ 5.5.40 | 5.5.40a@
-- -   __Version 5.5 (Available in all regions):__ @ 5.5.40b | 5.5.41@
-- -   __Version 5.6 (Available in all regions):__
--     @ 5.6.19a | 5.6.19b | 5.6.21 | 5.6.21b | 5.6.22@
--
-- __Oracle Database Enterprise Edition (oracle-ee)__
--
-- -   __Version 11.2 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__
--     @ 11.2.0.2.v3 | 11.2.0.2.v4 | 11.2.0.2.v5 | 11.2.0.2.v6 | 11.2.0.2.v7@
-- -   __Version 11.2 (Available in all regions):__
--     @ 11.2.0.3.v1 | 11.2.0.3.v2 | 11.2.0.4.v1 | 11.2.0.4.v3@
--
-- __Oracle Database Enterprise Edition (oracle-ee)__
--
-- -   __Version 11.2 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__
--     @ 11.2.0.2.v3 | 11.2.0.2.v4 | 11.2.0.2.v5 | 11.2.0.2.v6 | 11.2.0.2.v7@
-- -   __Version 11.2 (Available in all regions):__
--     @ 11.2.0.3.v1 | 11.2.0.3.v2 | 11.2.0.4.v1 | 11.2.0.4.v3@
--
-- __Oracle Database Standard Edition (oracle-se)__
--
-- -   __Version 11.2 (Only available in the following regions:
--     us-west-1):__
--     @ 11.2.0.2.v3 | 11.2.0.2.v4 | 11.2.0.2.v5 | 11.2.0.2.v6 | 11.2.0.2.v7@
-- -   __Version 11.2 (Only available in the following regions:
--     eu-central-1, us-west-1):__
--     @ 11.2.0.3.v1 | 11.2.0.3.v2 | 11.2.0.4.v1 | 11.2.0.4.v3@
--
-- __Oracle Database Standard Edition (oracle-se)__
--
-- -   __Version 11.2 (Only available in the following regions:
--     us-west-1):__
--     @ 11.2.0.2.v3 | 11.2.0.2.v4 | 11.2.0.2.v5 | 11.2.0.2.v6 | 11.2.0.2.v7@
-- -   __Version 11.2 (Only available in the following regions:
--     eu-central-1, us-west-1):__
--     @ 11.2.0.3.v1 | 11.2.0.3.v2 | 11.2.0.4.v1 | 11.2.0.4.v3@
--
-- __Oracle Database Standard Edition One (oracle-se1)__
--
-- -   __Version 11.2 (Only available in the following regions:
--     us-west-1):__
--     @ 11.2.0.2.v3 | 11.2.0.2.v4 | 11.2.0.2.v5 | 11.2.0.2.v6 | 11.2.0.2.v7@
-- -   __Version 11.2 (Only available in the following regions:
--     eu-central-1, us-west-1):__
--     @ 11.2.0.3.v1 | 11.2.0.3.v2 | 11.2.0.4.v1 | 11.2.0.4.v3@
--
-- __Oracle Database Standard Edition One (oracle-se1)__
--
-- -   __Version 11.2 (Only available in the following regions:
--     us-west-1):__
--     @ 11.2.0.2.v3 | 11.2.0.2.v4 | 11.2.0.2.v5 | 11.2.0.2.v6 | 11.2.0.2.v7@
-- -   __Version 11.2 (Only available in the following regions:
--     eu-central-1, us-west-1):__
--     @ 11.2.0.3.v1 | 11.2.0.3.v2 | 11.2.0.4.v1 | 11.2.0.4.v3@
--
-- __PostgreSQL__
--
-- -   __Version 9.3 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ @ 9.3.1 | 9.3.2@
-- -   __Version 9.3 (Available in all regions):__ @ 9.3.3 | 9.3.5@
--
-- __PostgreSQL__
--
-- -   __Version 9.3 (Only available in the following regions:
--     ap-northeast-1, ap-southeast-1, ap-southeast-2, eu-west-1,
--     sa-east-1, us-west-1, us-west-2):__ @ 9.3.1 | 9.3.2@
-- -   __Version 9.3 (Available in all regions):__ @ 9.3.3 | 9.3.5@
--
-- __Microsoft SQL Server Enterprise Edition (sqlserver-ee)__
--
-- -   __Version 10.50 (Only available in the following regions:
--     eu-central-1, us-west-1):__ @ 10.50.2789.0.v1@
-- -   __Version 11.00 (Only available in the following regions:
--     eu-central-1, us-west-1):__ @ 11.00.2100.60.v1@
--
-- __Microsoft SQL Server Enterprise Edition (sqlserver-ee)__
--
-- -   __Version 10.50 (Only available in the following regions:
--     eu-central-1, us-west-1):__ @ 10.50.2789.0.v1@
-- -   __Version 11.00 (Only available in the following regions:
--     eu-central-1, us-west-1):__ @ 11.00.2100.60.v1@
--
-- __Microsoft SQL Server Express Edition (sqlserver-ex)__
--
-- -   __Version 10.50 (Available in all regions):__ @ 10.50.2789.0.v1@
-- -   __Version 11.00 (Available in all regions):__ @ 11.00.2100.60.v1@
--
-- __Microsoft SQL Server Express Edition (sqlserver-ex)__
--
-- -   __Version 10.50 (Available in all regions):__ @ 10.50.2789.0.v1@
-- -   __Version 11.00 (Available in all regions):__ @ 11.00.2100.60.v1@
--
-- __Microsoft SQL Server Standard Edition (sqlserver-se)__
--
-- -   __Version 10.50 (Available in all regions):__ @ 10.50.2789.0.v1@
-- -   __Version 11.00 (Available in all regions):__ @ 11.00.2100.60.v1@
--
-- __Microsoft SQL Server Standard Edition (sqlserver-se)__
--
-- -   __Version 10.50 (Available in all regions):__ @ 10.50.2789.0.v1@
-- -   __Version 11.00 (Available in all regions):__ @ 11.00.2100.60.v1@
--
-- __Microsoft SQL Server Web Edition (sqlserver-web)__
--
-- -   __Version 10.50 (Available in all regions):__ @ 10.50.2789.0.v1@
-- -   __Version 11.00 (Available in all regions):__ @ 11.00.2100.60.v1@
--
-- __Microsoft SQL Server Web Edition (sqlserver-web)__
--
-- -   __Version 10.50 (Available in all regions):__ @ 10.50.2789.0.v1@
-- -   __Version 11.00 (Available in all regions):__ @ 11.00.2100.60.v1@
cdiEngineVersion :: Lens' CreateDBInstance (Maybe Text)
cdiEngineVersion = lens _cdiEngineVersion (\ s a -> s{_cdiEngineVersion = a});

-- | Specifies whether the DB instance is encrypted.
--
-- Default: false
cdiStorageEncrypted :: Lens' CreateDBInstance (Maybe Bool)
cdiStorageEncrypted = lens _cdiStorageEncrypted (\ s a -> s{_cdiStorageEncrypted = a});

-- | Indicates that minor engine upgrades will be applied automatically to
-- the DB instance during the maintenance window.
--
-- Default: @true@
cdiAutoMinorVersionUpgrade :: Lens' CreateDBInstance (Maybe Bool)
cdiAutoMinorVersionUpgrade = lens _cdiAutoMinorVersionUpgrade (\ s a -> s{_cdiAutoMinorVersionUpgrade = a});

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

-- | A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
cdiDBSubnetGroupName :: Lens' CreateDBInstance (Maybe Text)
cdiDBSubnetGroupName = lens _cdiDBSubnetGroupName (\ s a -> s{_cdiDBSubnetGroupName = a});

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- Constraints: To use PIOPS, this value must be an integer greater than
-- 1000.
cdiIOPS :: Lens' CreateDBInstance (Maybe Int)
cdiIOPS = lens _cdiIOPS (\ s a -> s{_cdiIOPS = a});

-- | The password for the given ARN from the Key Store in order to access the
-- device.
cdiTDECredentialPassword :: Lens' CreateDBInstance (Maybe Text)
cdiTDECredentialPassword = lens _cdiTDECredentialPassword (\ s a -> s{_cdiTDECredentialPassword = a});

-- | License model information for this DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
cdiLicenseModel :: Lens' CreateDBInstance (Maybe Text)
cdiLicenseModel = lens _cdiLicenseModel (\ s a -> s{_cdiLicenseModel = a});

-- | The weekly time range (in UTC) during which system maintenance can
-- occur. For more information, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBMaintenance.html DB Instance Maintenance>.
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region, occurring on a random day of the week. To see the time
-- blocks available, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the Amazon RDS User Guide.
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

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
-- parameter. For more information, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.BackingUpAndRestoringAmazonRDSInstances.html DB Instance Backups>.
--
-- Default: A 30-minute window selected at random from an 8-hour block of
-- time per region. See the Amazon RDS User Guide for the time blocks for
-- each region from which the default backup windows are assigned.
--
-- Constraints: Must be in the format @hh24:mi-hh24:mi@. Times should be
-- Universal Time Coordinated (UTC). Must not conflict with the preferred
-- maintenance window. Must be at least 30 minutes.
cdiPreferredBackupWindow :: Lens' CreateDBInstance (Maybe Text)
cdiPreferredBackupWindow = lens _cdiPreferredBackupWindow (\ s a -> s{_cdiPreferredBackupWindow = a});

-- | The EC2 Availability Zone that the database instance will be created in.
-- For information on regions and Availability Zones, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones>.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- region.
--
-- Example: @us-east-1d@
--
-- Constraint: The AvailabilityZone parameter cannot be specified if the
-- MultiAZ parameter is set to @true@. The specified Availability Zone must
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

-- | The KMS key identifier for an encrypted DB instance.
--
-- The KMS key identifier is the Amazon Resoure Name (ARN) for the KMS
-- encryption key. If you are creating a DB instance with the same AWS
-- account that owns the KMS encryption key used to encrypt the new DB
-- instance, then you can use the KMS key alias instead of the ARN for the
-- KM encryption key.
--
-- If the @StorageEncrypted@ parameter is true, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
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

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
cdiVPCSecurityGroupIds :: Lens' CreateDBInstance [Text]
cdiVPCSecurityGroupIds = lens _cdiVPCSecurityGroupIds (\ s a -> s{_cdiVPCSecurityGroupIds = a}) . _Default;

-- | Specifies if the DB instance is a Multi-AZ deployment. You cannot set
-- the AvailabilityZone parameter if the MultiAZ parameter is set to true.
cdiMultiAZ :: Lens' CreateDBInstance (Maybe Bool)
cdiMultiAZ = lens _cdiMultiAZ (\ s a -> s{_cdiMultiAZ = a});

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
cdiTDECredentialARN :: Lens' CreateDBInstance (Maybe Text)
cdiTDECredentialARN = lens _cdiTDECredentialARN (\ s a -> s{_cdiTDECredentialARN = a});

-- | Indicates that the DB instance should be associated with the specified
-- option group.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group
-- cannot be removed from a DB instance once it is associated with a DB
-- instance
cdiOptionGroupName :: Lens' CreateDBInstance (Maybe Text)
cdiOptionGroupName = lens _cdiOptionGroupName (\ s a -> s{_cdiOptionGroupName = a});

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
-- __PostgreSQL__
--
-- The name of the database to create when the DB instance is created. If
-- this parameter is not specified, no database is created in the DB
-- instance.
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
-- Default: @ORCL@
--
-- Constraints:
--
-- -   Cannot be longer than 8 characters
--
-- __SQL Server__
--
-- Not applicable. Must be null.
cdiDBName :: Lens' CreateDBInstance (Maybe Text)
cdiDBName = lens _cdiDBName (\ s a -> s{_cdiDBName = a});

-- | FIXME: Undocumented member.
cdiTags :: Lens' CreateDBInstance [Tag]
cdiTags = lens _cdiTags (\ s a -> s{_cdiTags = a}) . _Default;

-- | The port number on which the database accepts connections.
--
-- __MySQL__
--
-- Default: @3306@
--
-- Valid Values: @1150-65535@
--
-- Type: Integer
--
-- __PostgreSQL__
--
-- Default: @5432@
--
-- Valid Values: @1150-65535@
--
-- Type: Integer
--
-- __Oracle__
--
-- Default: @1521@
--
-- Valid Values: @1150-65535@
--
-- __SQL Server__
--
-- Default: @1433@
--
-- Valid Values: @1150-65535@ except for @1434@, @3389@, @47001@, @49152@,
-- and @49152@ through @49156@.
cdiPort :: Lens' CreateDBInstance (Maybe Int)
cdiPort = lens _cdiPort (\ s a -> s{_cdiPort = a});

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise
-- @standard@
cdiStorageType :: Lens' CreateDBInstance (Maybe Text)
cdiStorageType = lens _cdiStorageType (\ s a -> s{_cdiStorageType = a});

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
-- Example: @mydbinstance@
cdiDBInstanceIdentifier :: Lens' CreateDBInstance Text
cdiDBInstanceIdentifier = lens _cdiDBInstanceIdentifier (\ s a -> s{_cdiDBInstanceIdentifier = a});

-- | The amount of storage (in gigabytes) to be initially allocated for the
-- database instance.
--
-- Type: Integer
--
-- __MySQL__
--
-- Constraints: Must be an integer from 5 to 3072.
--
-- __PostgreSQL__
--
-- Constraints: Must be an integer from 5 to 3072.
--
-- __Oracle__
--
-- Constraints: Must be an integer from 10 to 3072.
--
-- __SQL Server__
--
-- Constraints: Must be an integer from 200 to 1024 (Standard Edition and
-- Enterprise Edition) or from 20 to 1024 (Express Edition and Web Edition)
cdiAllocatedStorage :: Lens' CreateDBInstance Int
cdiAllocatedStorage = lens _cdiAllocatedStorage (\ s a -> s{_cdiAllocatedStorage = a});

-- | The compute and memory capacity of the DB instance.
--
-- Valid Values:
-- @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium@
cdiDBInstanceClass :: Lens' CreateDBInstance Text
cdiDBInstanceClass = lens _cdiDBInstanceClass (\ s a -> s{_cdiDBInstanceClass = a});

-- | The name of the database engine to be used for this instance.
--
-- Valid Values: @MySQL@ | @oracle-se1@ | @oracle-se@ | @oracle-ee@ |
-- @sqlserver-ee@ | @sqlserver-se@ | @sqlserver-ex@ | @sqlserver-web@ |
-- @postgres@
--
-- Not every database engine is available for every AWS region.
cdiEngine :: Lens' CreateDBInstance Text
cdiEngine = lens _cdiEngine (\ s a -> s{_cdiEngine = a});

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
cdiMasterUsername :: Lens' CreateDBInstance Text
cdiMasterUsername = lens _cdiMasterUsername (\ s a -> s{_cdiMasterUsername = a});

-- | The password for the master database user. Can be any printable ASCII
-- character except \"\/\", \"\"\", or \"\@\".
--
-- Type: String
--
-- __MySQL__
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
cdiMasterUserPassword :: Lens' CreateDBInstance Text
cdiMasterUserPassword = lens _cdiMasterUserPassword (\ s a -> s{_cdiMasterUserPassword = a});

instance AWSRequest CreateDBInstance where
        type Sv CreateDBInstance = RDS
        type Rs CreateDBInstance = CreateDBInstanceResponse
        request = post
        response
          = receiveXMLWrapper "CreateDBInstanceResult"
              (\ s h x ->
                 CreateDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance ToHeaders CreateDBInstance where
        toHeaders = const mempty

instance ToPath CreateDBInstance where
        toPath = const "/"

instance ToQuery CreateDBInstance where
        toQuery CreateDBInstance'{..}
          = mconcat
              ["Action" =: ("CreateDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSecurityGroups" =:
                 toQuery
                   (toQueryList "DBSecurityGroupName" <$>
                      _cdiDBSecurityGroups),
               "EngineVersion" =: _cdiEngineVersion,
               "StorageEncrypted" =: _cdiStorageEncrypted,
               "AutoMinorVersionUpgrade" =:
                 _cdiAutoMinorVersionUpgrade,
               "PubliclyAccessible" =: _cdiPubliclyAccessible,
               "DBSubnetGroupName" =: _cdiDBSubnetGroupName,
               "Iops" =: _cdiIOPS,
               "TdeCredentialPassword" =: _cdiTDECredentialPassword,
               "LicenseModel" =: _cdiLicenseModel,
               "PreferredMaintenanceWindow" =:
                 _cdiPreferredMaintenanceWindow,
               "CharacterSetName" =: _cdiCharacterSetName,
               "PreferredBackupWindow" =: _cdiPreferredBackupWindow,
               "AvailabilityZone" =: _cdiAvailabilityZone,
               "BackupRetentionPeriod" =: _cdiBackupRetentionPeriod,
               "KmsKeyId" =: _cdiKMSKeyId,
               "DBParameterGroupName" =: _cdiDBParameterGroupName,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _cdiVPCSecurityGroupIds),
               "MultiAZ" =: _cdiMultiAZ,
               "TdeCredentialArn" =: _cdiTDECredentialARN,
               "OptionGroupName" =: _cdiOptionGroupName,
               "DBName" =: _cdiDBName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdiTags),
               "Port" =: _cdiPort, "StorageType" =: _cdiStorageType,
               "DBInstanceIdentifier" =: _cdiDBInstanceIdentifier,
               "AllocatedStorage" =: _cdiAllocatedStorage,
               "DBInstanceClass" =: _cdiDBInstanceClass,
               "Engine" =: _cdiEngine,
               "MasterUsername" =: _cdiMasterUsername,
               "MasterUserPassword" =: _cdiMasterUserPassword]

-- | /See:/ 'createDBInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdirDBInstance'
--
-- * 'cdirStatusCode'
data CreateDBInstanceResponse = CreateDBInstanceResponse'{_cdirDBInstance :: Maybe DBInstance, _cdirStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'CreateDBInstanceResponse' smart constructor.
createDBInstanceResponse :: Int -> CreateDBInstanceResponse
createDBInstanceResponse pStatusCode = CreateDBInstanceResponse'{_cdirDBInstance = Nothing, _cdirStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
cdirDBInstance :: Lens' CreateDBInstanceResponse (Maybe DBInstance)
cdirDBInstance = lens _cdirDBInstance (\ s a -> s{_cdirDBInstance = a});

-- | FIXME: Undocumented member.
cdirStatusCode :: Lens' CreateDBInstanceResponse Int
cdirStatusCode = lens _cdirStatusCode (\ s a -> s{_cdirStatusCode = a});
