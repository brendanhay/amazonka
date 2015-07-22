{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBInstance.html>
module Network.AWS.RDS.CreateDBInstance
    (
    -- * Request
      CreateDBInstance
    -- ** Request constructor
    , createDBInstance
    -- ** Request lenses
    , cdirqDBSecurityGroups
    , cdirqEngineVersion
    , cdirqStorageEncrypted
    , cdirqAutoMinorVersionUpgrade
    , cdirqPubliclyAccessible
    , cdirqDBSubnetGroupName
    , cdirqIOPS
    , cdirqTDECredentialPassword
    , cdirqLicenseModel
    , cdirqPreferredMaintenanceWindow
    , cdirqCharacterSetName
    , cdirqPreferredBackupWindow
    , cdirqAvailabilityZone
    , cdirqBackupRetentionPeriod
    , cdirqKMSKeyId
    , cdirqDBParameterGroupName
    , cdirqVPCSecurityGroupIds
    , cdirqMultiAZ
    , cdirqTDECredentialARN
    , cdirqOptionGroupName
    , cdirqDBName
    , cdirqTags
    , cdirqPort
    , cdirqStorageType
    , cdirqDBInstanceIdentifier
    , cdirqAllocatedStorage
    , cdirqDBInstanceClass
    , cdirqEngine
    , cdirqMasterUsername
    , cdirqMasterUserPassword

    -- * Response
    , CreateDBInstanceResponse
    -- ** Response constructor
    , createDBInstanceResponse
    -- ** Response lenses
    , cdirsDBInstance
    , cdirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdirqDBSecurityGroups'
--
-- * 'cdirqEngineVersion'
--
-- * 'cdirqStorageEncrypted'
--
-- * 'cdirqAutoMinorVersionUpgrade'
--
-- * 'cdirqPubliclyAccessible'
--
-- * 'cdirqDBSubnetGroupName'
--
-- * 'cdirqIOPS'
--
-- * 'cdirqTDECredentialPassword'
--
-- * 'cdirqLicenseModel'
--
-- * 'cdirqPreferredMaintenanceWindow'
--
-- * 'cdirqCharacterSetName'
--
-- * 'cdirqPreferredBackupWindow'
--
-- * 'cdirqAvailabilityZone'
--
-- * 'cdirqBackupRetentionPeriod'
--
-- * 'cdirqKMSKeyId'
--
-- * 'cdirqDBParameterGroupName'
--
-- * 'cdirqVPCSecurityGroupIds'
--
-- * 'cdirqMultiAZ'
--
-- * 'cdirqTDECredentialARN'
--
-- * 'cdirqOptionGroupName'
--
-- * 'cdirqDBName'
--
-- * 'cdirqTags'
--
-- * 'cdirqPort'
--
-- * 'cdirqStorageType'
--
-- * 'cdirqDBInstanceIdentifier'
--
-- * 'cdirqAllocatedStorage'
--
-- * 'cdirqDBInstanceClass'
--
-- * 'cdirqEngine'
--
-- * 'cdirqMasterUsername'
--
-- * 'cdirqMasterUserPassword'
data CreateDBInstance = CreateDBInstance'
    { _cdirqDBSecurityGroups           :: !(Maybe [Text])
    , _cdirqEngineVersion              :: !(Maybe Text)
    , _cdirqStorageEncrypted           :: !(Maybe Bool)
    , _cdirqAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _cdirqPubliclyAccessible         :: !(Maybe Bool)
    , _cdirqDBSubnetGroupName          :: !(Maybe Text)
    , _cdirqIOPS                       :: !(Maybe Int)
    , _cdirqTDECredentialPassword      :: !(Maybe Text)
    , _cdirqLicenseModel               :: !(Maybe Text)
    , _cdirqPreferredMaintenanceWindow :: !(Maybe Text)
    , _cdirqCharacterSetName           :: !(Maybe Text)
    , _cdirqPreferredBackupWindow      :: !(Maybe Text)
    , _cdirqAvailabilityZone           :: !(Maybe Text)
    , _cdirqBackupRetentionPeriod      :: !(Maybe Int)
    , _cdirqKMSKeyId                   :: !(Maybe Text)
    , _cdirqDBParameterGroupName       :: !(Maybe Text)
    , _cdirqVPCSecurityGroupIds        :: !(Maybe [Text])
    , _cdirqMultiAZ                    :: !(Maybe Bool)
    , _cdirqTDECredentialARN           :: !(Maybe Text)
    , _cdirqOptionGroupName            :: !(Maybe Text)
    , _cdirqDBName                     :: !(Maybe Text)
    , _cdirqTags                       :: !(Maybe [Tag])
    , _cdirqPort                       :: !(Maybe Int)
    , _cdirqStorageType                :: !(Maybe Text)
    , _cdirqDBInstanceIdentifier       :: !Text
    , _cdirqAllocatedStorage           :: !Int
    , _cdirqDBInstanceClass            :: !Text
    , _cdirqEngine                     :: !Text
    , _cdirqMasterUsername             :: !Text
    , _cdirqMasterUserPassword         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBInstance' smart constructor.
createDBInstance :: Text -> Int -> Text -> Text -> Text -> Text -> CreateDBInstance
createDBInstance pDBInstanceIdentifier pAllocatedStorage pDBInstanceClass pEngine pMasterUsername pMasterUserPassword =
    CreateDBInstance'
    { _cdirqDBSecurityGroups = Nothing
    , _cdirqEngineVersion = Nothing
    , _cdirqStorageEncrypted = Nothing
    , _cdirqAutoMinorVersionUpgrade = Nothing
    , _cdirqPubliclyAccessible = Nothing
    , _cdirqDBSubnetGroupName = Nothing
    , _cdirqIOPS = Nothing
    , _cdirqTDECredentialPassword = Nothing
    , _cdirqLicenseModel = Nothing
    , _cdirqPreferredMaintenanceWindow = Nothing
    , _cdirqCharacterSetName = Nothing
    , _cdirqPreferredBackupWindow = Nothing
    , _cdirqAvailabilityZone = Nothing
    , _cdirqBackupRetentionPeriod = Nothing
    , _cdirqKMSKeyId = Nothing
    , _cdirqDBParameterGroupName = Nothing
    , _cdirqVPCSecurityGroupIds = Nothing
    , _cdirqMultiAZ = Nothing
    , _cdirqTDECredentialARN = Nothing
    , _cdirqOptionGroupName = Nothing
    , _cdirqDBName = Nothing
    , _cdirqTags = Nothing
    , _cdirqPort = Nothing
    , _cdirqStorageType = Nothing
    , _cdirqDBInstanceIdentifier = pDBInstanceIdentifier
    , _cdirqAllocatedStorage = pAllocatedStorage
    , _cdirqDBInstanceClass = pDBInstanceClass
    , _cdirqEngine = pEngine
    , _cdirqMasterUsername = pMasterUsername
    , _cdirqMasterUserPassword = pMasterUserPassword
    }

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
cdirqDBSecurityGroups :: Lens' CreateDBInstance [Text]
cdirqDBSecurityGroups = lens _cdirqDBSecurityGroups (\ s a -> s{_cdirqDBSecurityGroups = a}) . _Default;

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
cdirqEngineVersion :: Lens' CreateDBInstance (Maybe Text)
cdirqEngineVersion = lens _cdirqEngineVersion (\ s a -> s{_cdirqEngineVersion = a});

-- | Specifies whether the DB instance is encrypted.
--
-- Default: false
cdirqStorageEncrypted :: Lens' CreateDBInstance (Maybe Bool)
cdirqStorageEncrypted = lens _cdirqStorageEncrypted (\ s a -> s{_cdirqStorageEncrypted = a});

-- | Indicates that minor engine upgrades will be applied automatically to
-- the DB instance during the maintenance window.
--
-- Default: @true@
cdirqAutoMinorVersionUpgrade :: Lens' CreateDBInstance (Maybe Bool)
cdirqAutoMinorVersionUpgrade = lens _cdirqAutoMinorVersionUpgrade (\ s a -> s{_cdirqAutoMinorVersionUpgrade = a});

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
cdirqPubliclyAccessible :: Lens' CreateDBInstance (Maybe Bool)
cdirqPubliclyAccessible = lens _cdirqPubliclyAccessible (\ s a -> s{_cdirqPubliclyAccessible = a});

-- | A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
cdirqDBSubnetGroupName :: Lens' CreateDBInstance (Maybe Text)
cdirqDBSubnetGroupName = lens _cdirqDBSubnetGroupName (\ s a -> s{_cdirqDBSubnetGroupName = a});

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- Constraints: To use PIOPS, this value must be an integer greater than
-- 1000.
cdirqIOPS :: Lens' CreateDBInstance (Maybe Int)
cdirqIOPS = lens _cdirqIOPS (\ s a -> s{_cdirqIOPS = a});

-- | The password for the given ARN from the Key Store in order to access the
-- device.
cdirqTDECredentialPassword :: Lens' CreateDBInstance (Maybe Text)
cdirqTDECredentialPassword = lens _cdirqTDECredentialPassword (\ s a -> s{_cdirqTDECredentialPassword = a});

-- | License model information for this DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
cdirqLicenseModel :: Lens' CreateDBInstance (Maybe Text)
cdirqLicenseModel = lens _cdirqLicenseModel (\ s a -> s{_cdirqLicenseModel = a});

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
cdirqPreferredMaintenanceWindow :: Lens' CreateDBInstance (Maybe Text)
cdirqPreferredMaintenanceWindow = lens _cdirqPreferredMaintenanceWindow (\ s a -> s{_cdirqPreferredMaintenanceWindow = a});

-- | For supported engines, indicates that the DB instance should be
-- associated with the specified CharacterSet.
cdirqCharacterSetName :: Lens' CreateDBInstance (Maybe Text)
cdirqCharacterSetName = lens _cdirqCharacterSetName (\ s a -> s{_cdirqCharacterSetName = a});

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
cdirqPreferredBackupWindow :: Lens' CreateDBInstance (Maybe Text)
cdirqPreferredBackupWindow = lens _cdirqPreferredBackupWindow (\ s a -> s{_cdirqPreferredBackupWindow = a});

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
cdirqAvailabilityZone :: Lens' CreateDBInstance (Maybe Text)
cdirqAvailabilityZone = lens _cdirqAvailabilityZone (\ s a -> s{_cdirqAvailabilityZone = a});

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
cdirqBackupRetentionPeriod :: Lens' CreateDBInstance (Maybe Int)
cdirqBackupRetentionPeriod = lens _cdirqBackupRetentionPeriod (\ s a -> s{_cdirqBackupRetentionPeriod = a});

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
cdirqKMSKeyId :: Lens' CreateDBInstance (Maybe Text)
cdirqKMSKeyId = lens _cdirqKMSKeyId (\ s a -> s{_cdirqKMSKeyId = a});

-- | The name of the DB parameter group to associate with this DB instance.
-- If this argument is omitted, the default DBParameterGroup for the
-- specified engine will be used.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
cdirqDBParameterGroupName :: Lens' CreateDBInstance (Maybe Text)
cdirqDBParameterGroupName = lens _cdirqDBParameterGroupName (\ s a -> s{_cdirqDBParameterGroupName = a});

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
cdirqVPCSecurityGroupIds :: Lens' CreateDBInstance [Text]
cdirqVPCSecurityGroupIds = lens _cdirqVPCSecurityGroupIds (\ s a -> s{_cdirqVPCSecurityGroupIds = a}) . _Default;

-- | Specifies if the DB instance is a Multi-AZ deployment. You cannot set
-- the AvailabilityZone parameter if the MultiAZ parameter is set to true.
cdirqMultiAZ :: Lens' CreateDBInstance (Maybe Bool)
cdirqMultiAZ = lens _cdirqMultiAZ (\ s a -> s{_cdirqMultiAZ = a});

-- | The ARN from the Key Store with which to associate the instance for TDE
-- encryption.
cdirqTDECredentialARN :: Lens' CreateDBInstance (Maybe Text)
cdirqTDECredentialARN = lens _cdirqTDECredentialARN (\ s a -> s{_cdirqTDECredentialARN = a});

-- | Indicates that the DB instance should be associated with the specified
-- option group.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, cannot be removed from an option group, and that option group
-- cannot be removed from a DB instance once it is associated with a DB
-- instance
cdirqOptionGroupName :: Lens' CreateDBInstance (Maybe Text)
cdirqOptionGroupName = lens _cdirqOptionGroupName (\ s a -> s{_cdirqOptionGroupName = a});

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
cdirqDBName :: Lens' CreateDBInstance (Maybe Text)
cdirqDBName = lens _cdirqDBName (\ s a -> s{_cdirqDBName = a});

-- | FIXME: Undocumented member.
cdirqTags :: Lens' CreateDBInstance [Tag]
cdirqTags = lens _cdirqTags (\ s a -> s{_cdirqTags = a}) . _Default;

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
cdirqPort :: Lens' CreateDBInstance (Maybe Int)
cdirqPort = lens _cdirqPort (\ s a -> s{_cdirqPort = a});

-- | Specifies the storage type to be associated with the DB instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified; otherwise
-- @standard@
cdirqStorageType :: Lens' CreateDBInstance (Maybe Text)
cdirqStorageType = lens _cdirqStorageType (\ s a -> s{_cdirqStorageType = a});

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
cdirqDBInstanceIdentifier :: Lens' CreateDBInstance Text
cdirqDBInstanceIdentifier = lens _cdirqDBInstanceIdentifier (\ s a -> s{_cdirqDBInstanceIdentifier = a});

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
cdirqAllocatedStorage :: Lens' CreateDBInstance Int
cdirqAllocatedStorage = lens _cdirqAllocatedStorage (\ s a -> s{_cdirqAllocatedStorage = a});

-- | The compute and memory capacity of the DB instance.
--
-- Valid Values:
-- @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium@
cdirqDBInstanceClass :: Lens' CreateDBInstance Text
cdirqDBInstanceClass = lens _cdirqDBInstanceClass (\ s a -> s{_cdirqDBInstanceClass = a});

-- | The name of the database engine to be used for this instance.
--
-- Valid Values: @MySQL@ | @oracle-se1@ | @oracle-se@ | @oracle-ee@ |
-- @sqlserver-ee@ | @sqlserver-se@ | @sqlserver-ex@ | @sqlserver-web@ |
-- @postgres@
--
-- Not every database engine is available for every AWS region.
cdirqEngine :: Lens' CreateDBInstance Text
cdirqEngine = lens _cdirqEngine (\ s a -> s{_cdirqEngine = a});

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
cdirqMasterUsername :: Lens' CreateDBInstance Text
cdirqMasterUsername = lens _cdirqMasterUsername (\ s a -> s{_cdirqMasterUsername = a});

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
cdirqMasterUserPassword :: Lens' CreateDBInstance Text
cdirqMasterUserPassword = lens _cdirqMasterUserPassword (\ s a -> s{_cdirqMasterUserPassword = a});

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
                      _cdirqDBSecurityGroups),
               "EngineVersion" =: _cdirqEngineVersion,
               "StorageEncrypted" =: _cdirqStorageEncrypted,
               "AutoMinorVersionUpgrade" =:
                 _cdirqAutoMinorVersionUpgrade,
               "PubliclyAccessible" =: _cdirqPubliclyAccessible,
               "DBSubnetGroupName" =: _cdirqDBSubnetGroupName,
               "Iops" =: _cdirqIOPS,
               "TdeCredentialPassword" =:
                 _cdirqTDECredentialPassword,
               "LicenseModel" =: _cdirqLicenseModel,
               "PreferredMaintenanceWindow" =:
                 _cdirqPreferredMaintenanceWindow,
               "CharacterSetName" =: _cdirqCharacterSetName,
               "PreferredBackupWindow" =:
                 _cdirqPreferredBackupWindow,
               "AvailabilityZone" =: _cdirqAvailabilityZone,
               "BackupRetentionPeriod" =:
                 _cdirqBackupRetentionPeriod,
               "KmsKeyId" =: _cdirqKMSKeyId,
               "DBParameterGroupName" =: _cdirqDBParameterGroupName,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _cdirqVPCSecurityGroupIds),
               "MultiAZ" =: _cdirqMultiAZ,
               "TdeCredentialArn" =: _cdirqTDECredentialARN,
               "OptionGroupName" =: _cdirqOptionGroupName,
               "DBName" =: _cdirqDBName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdirqTags),
               "Port" =: _cdirqPort,
               "StorageType" =: _cdirqStorageType,
               "DBInstanceIdentifier" =: _cdirqDBInstanceIdentifier,
               "AllocatedStorage" =: _cdirqAllocatedStorage,
               "DBInstanceClass" =: _cdirqDBInstanceClass,
               "Engine" =: _cdirqEngine,
               "MasterUsername" =: _cdirqMasterUsername,
               "MasterUserPassword" =: _cdirqMasterUserPassword]

-- | /See:/ 'createDBInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdirsDBInstance'
--
-- * 'cdirsStatus'
data CreateDBInstanceResponse = CreateDBInstanceResponse'
    { _cdirsDBInstance :: !(Maybe DBInstance)
    , _cdirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBInstanceResponse' smart constructor.
createDBInstanceResponse :: Int -> CreateDBInstanceResponse
createDBInstanceResponse pStatus =
    CreateDBInstanceResponse'
    { _cdirsDBInstance = Nothing
    , _cdirsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdirsDBInstance :: Lens' CreateDBInstanceResponse (Maybe DBInstance)
cdirsDBInstance = lens _cdirsDBInstance (\ s a -> s{_cdirsDBInstance = a});

-- | FIXME: Undocumented member.
cdirsStatus :: Lens' CreateDBInstanceResponse Int
cdirsStatus = lens _cdirsStatus (\ s a -> s{_cdirsStatus = a});
