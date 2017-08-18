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
--
--
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
    , cdiDomain
    , cdiMonitoringInterval
    , cdiTDECredentialPassword
    , cdiPromotionTier
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
    , cdiTimezone
    , cdiTDECredentialARN
    , cdiDomainIAMRoleName
    , cdiTags
    , cdiPort
    , cdiEnableIAMDatabaseAuthentication
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
--
--
-- /See:/ 'createDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
    { _cdiEngineVersion                   :: !(Maybe Text)
    , _cdiDBSecurityGroups                :: !(Maybe [Text])
    , _cdiStorageEncrypted                :: !(Maybe Bool)
    , _cdiDBClusterIdentifier             :: !(Maybe Text)
    , _cdiMasterUserPassword              :: !(Maybe Text)
    , _cdiPubliclyAccessible              :: !(Maybe Bool)
    , _cdiAutoMinorVersionUpgrade         :: !(Maybe Bool)
    , _cdiMasterUsername                  :: !(Maybe Text)
    , _cdiDBSubnetGroupName               :: !(Maybe Text)
    , _cdiMonitoringRoleARN               :: !(Maybe Text)
    , _cdiIOPS                            :: !(Maybe Int)
    , _cdiDomain                          :: !(Maybe Text)
    , _cdiMonitoringInterval              :: !(Maybe Int)
    , _cdiTDECredentialPassword           :: !(Maybe Text)
    , _cdiPromotionTier                   :: !(Maybe Int)
    , _cdiLicenseModel                    :: !(Maybe Text)
    , _cdiPreferredMaintenanceWindow      :: !(Maybe Text)
    , _cdiCharacterSetName                :: !(Maybe Text)
    , _cdiKMSKeyId                        :: !(Maybe Text)
    , _cdiDBParameterGroupName            :: !(Maybe Text)
    , _cdiPreferredBackupWindow           :: !(Maybe Text)
    , _cdiAvailabilityZone                :: !(Maybe Text)
    , _cdiBackupRetentionPeriod           :: !(Maybe Int)
    , _cdiVPCSecurityGroupIds             :: !(Maybe [Text])
    , _cdiMultiAZ                         :: !(Maybe Bool)
    , _cdiAllocatedStorage                :: !(Maybe Int)
    , _cdiOptionGroupName                 :: !(Maybe Text)
    , _cdiCopyTagsToSnapshot              :: !(Maybe Bool)
    , _cdiTimezone                        :: !(Maybe Text)
    , _cdiTDECredentialARN                :: !(Maybe Text)
    , _cdiDomainIAMRoleName               :: !(Maybe Text)
    , _cdiTags                            :: !(Maybe [Tag])
    , _cdiPort                            :: !(Maybe Int)
    , _cdiEnableIAMDatabaseAuthentication :: !(Maybe Bool)
    , _cdiStorageType                     :: !(Maybe Text)
    , _cdiDBName                          :: !(Maybe Text)
    , _cdiDBInstanceIdentifier            :: !Text
    , _cdiDBInstanceClass                 :: !Text
    , _cdiEngine                          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdiEngineVersion' - The version number of the database engine to use. The following are the database engines and major and minor versions that are available with Amazon RDS. Not every database engine is available for every AWS Region. __Amazon Aurora__  Not applicable. The version number of the database engine to be used by the DB instance is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MariaDB__      * @10.1.19@ (supported in all AWS regions)     * @10.1.14@ (supported in all regions except us-east-2)     * @10.0.28@ (supported in all AWS regions)     * @10.0.24@ (supported in all AWS regions)     * @10.0.17@ (supported in all regions except us-east-2, ca-central-1, eu-west-2) __Microsoft SQL Server 2016__      * @13.00.4422.0.v1@ (supported for all editions, and all AWS regions)     * @13.00.2164.0.v1@ (supported for all editions, and all AWS regions) __Microsoft SQL Server 2014__      * @12.00.5546.0.v1@ (supported for all editions, and all AWS regions)     * @12.00.5000.0.v1@ (supported for all editions, and all AWS regions)     * @12.00.4422.0.v1@ (supported for all editions except Enterprise Edition, and all AWS regions except ca-central-1 and eu-west-2) __Microsoft SQL Server 2012__      * @11.00.6594.0.v1@ (supported for all editions, and all AWS regions)     * @11.00.6020.0.v1@ (supported for all editions, and all AWS regions)     * @11.00.5058.0.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2)     * @11.00.2100.60.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2) __Microsoft SQL Server 2008 R2__      * @10.50.6529.0.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2)     * @10.50.6000.34.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2)     * @10.50.2789.0.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2) __MySQL__      * @5.7.17@ (supported in all AWS regions)     * @5.7.16@ (supported in all AWS regions)     * @5.7.11@ (supported in all AWS regions)     * @5.6.35@ (supported in all AWS regions)     * @5.6.34@ (supported in all AWS regions)     * @5.6.29@ (supported in all AWS regions)     * @5.6.27@ (supported in all regions except us-east-2, ca-central-1, eu-west-2)     * @5.5.54@ (supported in all AWS regions)     * @5.5.53@ (supported in all AWS regions)     * @5.5.46@ (supported in all AWS regions) __Oracle 12c__      * @12.1.0.2.v8@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v7@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v6@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v5@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v4@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v3@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v2@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v1@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1) __Oracle 11g__      * @11.2.0.4.v12@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v11@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v10@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v9@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v8@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v7@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v6@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v5@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v4@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v3@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v1@ (supported for EE, SE1, and SE, in all AWS regions) __PostgreSQL__      * __Version 9.6.x:__ @9.6.1 | 9.6.2@      * __Version 9.5.x:__ @9.5.6 | 9.5.4 | 9.5.2@      * __Version 9.4.x:__ @9.4.11 | 9.4.9 | 9.4.7@      * __Version 9.3.x:__ @9.3.16 | 9.3.14 | 9.3.12@
--
-- * 'cdiDBSecurityGroups' - A list of DB security groups to associate with this DB instance. Default: The default DB security group for the database engine.
--
-- * 'cdiStorageEncrypted' - Specifies whether the DB instance is encrypted. __Amazon Aurora__  Not applicable. The encryption for DB instances is managed by the DB cluster. For more information, see 'CreateDBCluster' . Default: false
--
-- * 'cdiDBClusterIdentifier' - The identifier of the DB cluster that the instance will belong to. For information on creating a DB cluster, see 'CreateDBCluster' . Type: String
--
-- * 'cdiMasterUserPassword' - The password for the master user. Can be any printable ASCII character except "/", """, or "@". __Amazon Aurora__  Not applicable. The password for the master user is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MariaDB__  Constraints: Must contain from 8 to 41 characters. __Microsoft SQL Server__  Constraints: Must contain from 8 to 128 characters. __MySQL__  Constraints: Must contain from 8 to 41 characters. __Oracle__  Constraints: Must contain from 8 to 30 characters. __PostgreSQL__  Constraints: Must contain from 8 to 128 characters.
--
-- * 'cdiPubliclyAccessible' - Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be private.
--
-- * 'cdiAutoMinorVersionUpgrade' - Indicates that minor engine upgrades will be applied automatically to the DB instance during the maintenance window. Default: @true@
--
-- * 'cdiMasterUsername' - The name for the master user. __Amazon Aurora__  Not applicable. The name for the master user is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MariaDB__  Constraints:     * Must be 1 to 16 alphanumeric characters.     * Cannot be a reserved word for the chosen database engine. __Microsoft SQL Server__  Constraints:     * Must be 1 to 128 alphanumeric characters.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine. __MySQL__  Constraints:     * Must be 1 to 16 alphanumeric characters.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine. __Oracle__  Constraints:     * Must be 1 to 30 alphanumeric characters.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine. __PostgreSQL__  Constraints:     * Must be 1 to 63 alphanumeric characters.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine.
--
-- * 'cdiDBSubnetGroupName' - A DB subnet group to associate with this DB instance. If there is no DB subnet group, then it is a non-VPC DB instance.
--
-- * 'cdiMonitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> . If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- * 'cdiIOPS' - The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance. Constraints: Must be a multiple between 3 and 10 of the storage amount for the DB instance. Must also be an integer multiple of 1000. For example, if the size of your DB instance is 500 GB, then your @Iops@ value can be 2000, 3000, 4000, or 5000.
--
-- * 'cdiDomain' - Specify the Active Directory Domain to create the instance in.
--
-- * 'cdiMonitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- * 'cdiTDECredentialPassword' - The password for the given ARN from the Key Store in order to access the device.
--
-- * 'cdiPromotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Managing.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> .  Default: 1 Valid Values: 0 - 15
--
-- * 'cdiLicenseModel' - License model information for this DB instance. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- * 'cdiPreferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBMaintenance.html DB Instance Maintenance> .  Format: @ddd:hh24:mi-ddd:hh24:mi@  Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region, occurring on a random day of the week. To see the time blocks available, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./  Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
--
-- * 'cdiCharacterSetName' - For supported engines, indicates that the DB instance should be associated with the specified CharacterSet. __Amazon Aurora__  Not applicable. The character set is managed by the DB cluster. For more information, see 'CreateDBCluster' .
--
-- * 'cdiKMSKeyId' - The KMS key identifier for an encrypted DB instance. The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key. __Amazon Aurora__  Not applicable. The KMS key identifier is managed by the DB cluster. For more information, see 'CreateDBCluster' . If the @StorageEncrypted@ parameter is true, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'cdiDBParameterGroupName' - The name of the DB parameter group to associate with this DB instance. If this argument is omitted, the default DBParameterGroup for the specified engine will be used. Constraints:     * Must be 1 to 255 alphanumeric characters     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
--
-- * 'cdiPreferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.BackingUpAndRestoringAmazonRDSInstances.html DB Instance Backups> .  __Amazon Aurora__  Not applicable. The daily time range for creating automated backups is managed by the DB cluster. For more information, see 'CreateDBCluster' . Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region. To see the time blocks available, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window> .  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Times should be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
--
-- * 'cdiAvailabilityZone' - The EC2 Availability Zone that the database instance will be created in. For information on regions and Availability Zones, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .  Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. Example: @us-east-1d@  Constraint: The AvailabilityZone parameter cannot be specified if the MultiAZ parameter is set to @true@ . The specified Availability Zone must be in the same AWS Region as the current endpoint.
--
-- * 'cdiBackupRetentionPeriod' - The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups. __Amazon Aurora__  Not applicable. The retention period for automated backups is managed by the DB cluster. For more information, see 'CreateDBCluster' . Default: 1 Constraints:     * Must be a value from 0 to 35     * Cannot be set to 0 if the DB instance is a source to Read Replicas
--
-- * 'cdiVPCSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB instance. __Amazon Aurora__  Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster. For more information, see 'CreateDBCluster' . Default: The default EC2 VPC security group for the DB subnet group's VPC.
--
-- * 'cdiMultiAZ' - Specifies if the DB instance is a Multi-AZ deployment. You cannot set the AvailabilityZone parameter if the MultiAZ parameter is set to true.
--
-- * 'cdiAllocatedStorage' - The amount of storage (in gigabytes) to be initially allocated for the database instance. Type: Integer __Amazon Aurora__  Not applicable. Aurora cluster volumes automatically grow as the amount of data in your database increases, though you are only charged for the space that you use in an Aurora cluster volume. __MySQL__  Constraints: Must be an integer from 5 to 6144. __MariaDB__  Constraints: Must be an integer from 5 to 6144. __PostgreSQL__  Constraints: Must be an integer from 5 to 6144. __Oracle__  Constraints: Must be an integer from 10 to 6144. __SQL Server__  Constraints: Must be an integer from 200 to 4096 (Standard Edition and Enterprise Edition) or from 20 to 4096 (Express Edition and Web Edition)
--
-- * 'cdiOptionGroupName' - Indicates that the DB instance should be associated with the specified option group. Permanent options, such as the TDE option for Oracle Advanced Security TDE, cannot be removed from an option group, and that option group cannot be removed from a DB instance once it is associated with a DB instance
--
-- * 'cdiCopyTagsToSnapshot' - True to copy all tags from the DB instance to snapshots of the DB instance; otherwise false. The default is false.
--
-- * 'cdiTimezone' - The time zone of the DB instance. The time zone parameter is currently supported only by <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server> .
--
-- * 'cdiTDECredentialARN' - The ARN from the Key Store with which to associate the instance for TDE encryption.
--
-- * 'cdiDomainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- * 'cdiTags' - Undocumented member.
--
-- * 'cdiPort' - The port number on which the database accepts connections. __MySQL__  Default: @3306@  Valid Values: @1150-65535@  Type: Integer __MariaDB__  Default: @3306@  Valid Values: @1150-65535@  Type: Integer __PostgreSQL__  Default: @5432@  Valid Values: @1150-65535@  Type: Integer __Oracle__  Default: @1521@  Valid Values: @1150-65535@  __SQL Server__  Default: @1433@  Valid Values: @1150-65535@ except for @1434@ , @3389@ , @47001@ , @49152@ , and @49152@ through @49156@ .  __Amazon Aurora__  Default: @3306@  Valid Values: @1150-65535@  Type: Integer
--
-- * 'cdiEnableIAMDatabaseAuthentication' - True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts; otherwise false.  You can enable IAM database authentication for the following database engines: __Amazon Aurora__  Not applicable. Mapping AWS IAM accounts to database accounts is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MySQL__      * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher Default: @false@
--
-- * 'cdiStorageType' - Specifies the storage type to be associated with the DB instance. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified; otherwise @standard@
--
-- * 'cdiDBName' - The meaning of this parameter differs according to the database engine you use. Type: String __MySQL__  The name of the database to create when the DB instance is created. If this parameter is not specified, no database is created in the DB instance. Constraints:     * Must contain 1 to 64 alphanumeric characters     * Cannot be a word reserved by the specified database engine __MariaDB__  The name of the database to create when the DB instance is created. If this parameter is not specified, no database is created in the DB instance. Constraints:     * Must contain 1 to 64 alphanumeric characters     * Cannot be a word reserved by the specified database engine __PostgreSQL__  The name of the database to create when the DB instance is created. If this parameter is not specified, the default "postgres" database is created in the DB instance. Constraints:     * Must contain 1 to 63 alphanumeric characters     * Must begin with a letter or an underscore. Subsequent characters can be letters, underscores, or digits (0-9).     * Cannot be a word reserved by the specified database engine __Oracle__  The Oracle System ID (SID) of the created DB instance. If you specify @null@ , the default value @ORCL@ is used. You can't specify the string NULL, or any other reserved word, for @DBName@ .  Default: @ORCL@  Constraints:     * Cannot be longer than 8 characters __SQL Server__  Not applicable. Must be null. __Amazon Aurora__  The name of the database to create when the primary instance of the DB cluster is created. If this parameter is not specified, no database is created in the DB instance. Constraints:     * Must contain 1 to 64 alphanumeric characters     * Cannot be a word reserved by the specified database engine
--
-- * 'cdiDBInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@
--
-- * 'cdiDBInstanceClass' - The compute and memory capacity of the DB instance. Note that not all instance classes are available in all regions for all DB engines. Valid Values: @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.m4.large | db.m4.xlarge | db.m4.2xlarge | db.m4.4xlarge | db.m4.10xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium | db.t2.large@
--
-- * 'cdiEngine' - The name of the database engine to be used for this instance.  Not every database engine is available for every AWS Region.  Valid Values:      * @aurora@      * @mariadb@      * @mysql@      * @oracle-ee@      * @oracle-se2@      * @oracle-se1@      * @oracle-se@      * @postgres@      * @sqlserver-ee@      * @sqlserver-se@      * @sqlserver-ex@      * @sqlserver-web@
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
    , _cdiDomain = Nothing
    , _cdiMonitoringInterval = Nothing
    , _cdiTDECredentialPassword = Nothing
    , _cdiPromotionTier = Nothing
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
    , _cdiTimezone = Nothing
    , _cdiTDECredentialARN = Nothing
    , _cdiDomainIAMRoleName = Nothing
    , _cdiTags = Nothing
    , _cdiPort = Nothing
    , _cdiEnableIAMDatabaseAuthentication = Nothing
    , _cdiStorageType = Nothing
    , _cdiDBName = Nothing
    , _cdiDBInstanceIdentifier = pDBInstanceIdentifier_
    , _cdiDBInstanceClass = pDBInstanceClass_
    , _cdiEngine = pEngine_
    }

-- | The version number of the database engine to use. The following are the database engines and major and minor versions that are available with Amazon RDS. Not every database engine is available for every AWS Region. __Amazon Aurora__  Not applicable. The version number of the database engine to be used by the DB instance is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MariaDB__      * @10.1.19@ (supported in all AWS regions)     * @10.1.14@ (supported in all regions except us-east-2)     * @10.0.28@ (supported in all AWS regions)     * @10.0.24@ (supported in all AWS regions)     * @10.0.17@ (supported in all regions except us-east-2, ca-central-1, eu-west-2) __Microsoft SQL Server 2016__      * @13.00.4422.0.v1@ (supported for all editions, and all AWS regions)     * @13.00.2164.0.v1@ (supported for all editions, and all AWS regions) __Microsoft SQL Server 2014__      * @12.00.5546.0.v1@ (supported for all editions, and all AWS regions)     * @12.00.5000.0.v1@ (supported for all editions, and all AWS regions)     * @12.00.4422.0.v1@ (supported for all editions except Enterprise Edition, and all AWS regions except ca-central-1 and eu-west-2) __Microsoft SQL Server 2012__      * @11.00.6594.0.v1@ (supported for all editions, and all AWS regions)     * @11.00.6020.0.v1@ (supported for all editions, and all AWS regions)     * @11.00.5058.0.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2)     * @11.00.2100.60.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2) __Microsoft SQL Server 2008 R2__      * @10.50.6529.0.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2)     * @10.50.6000.34.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2)     * @10.50.2789.0.v1@ (supported for all editions, and all AWS regions except us-east-2, ca-central-1, and eu-west-2) __MySQL__      * @5.7.17@ (supported in all AWS regions)     * @5.7.16@ (supported in all AWS regions)     * @5.7.11@ (supported in all AWS regions)     * @5.6.35@ (supported in all AWS regions)     * @5.6.34@ (supported in all AWS regions)     * @5.6.29@ (supported in all AWS regions)     * @5.6.27@ (supported in all regions except us-east-2, ca-central-1, eu-west-2)     * @5.5.54@ (supported in all AWS regions)     * @5.5.53@ (supported in all AWS regions)     * @5.5.46@ (supported in all AWS regions) __Oracle 12c__      * @12.1.0.2.v8@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v7@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v6@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v5@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v4@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v3@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v2@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1)     * @12.1.0.2.v1@ (supported for EE in all AWS regions, and SE2 in all AWS regions except us-gov-west-1) __Oracle 11g__      * @11.2.0.4.v12@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v11@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v10@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v9@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v8@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v7@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v6@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v5@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v4@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v3@ (supported for EE, SE1, and SE, in all AWS regions)     * @11.2.0.4.v1@ (supported for EE, SE1, and SE, in all AWS regions) __PostgreSQL__      * __Version 9.6.x:__ @9.6.1 | 9.6.2@      * __Version 9.5.x:__ @9.5.6 | 9.5.4 | 9.5.2@      * __Version 9.4.x:__ @9.4.11 | 9.4.9 | 9.4.7@      * __Version 9.3.x:__ @9.3.16 | 9.3.14 | 9.3.12@
cdiEngineVersion :: Lens' CreateDBInstance (Maybe Text)
cdiEngineVersion = lens _cdiEngineVersion (\ s a -> s{_cdiEngineVersion = a});

-- | A list of DB security groups to associate with this DB instance. Default: The default DB security group for the database engine.
cdiDBSecurityGroups :: Lens' CreateDBInstance [Text]
cdiDBSecurityGroups = lens _cdiDBSecurityGroups (\ s a -> s{_cdiDBSecurityGroups = a}) . _Default . _Coerce;

-- | Specifies whether the DB instance is encrypted. __Amazon Aurora__  Not applicable. The encryption for DB instances is managed by the DB cluster. For more information, see 'CreateDBCluster' . Default: false
cdiStorageEncrypted :: Lens' CreateDBInstance (Maybe Bool)
cdiStorageEncrypted = lens _cdiStorageEncrypted (\ s a -> s{_cdiStorageEncrypted = a});

-- | The identifier of the DB cluster that the instance will belong to. For information on creating a DB cluster, see 'CreateDBCluster' . Type: String
cdiDBClusterIdentifier :: Lens' CreateDBInstance (Maybe Text)
cdiDBClusterIdentifier = lens _cdiDBClusterIdentifier (\ s a -> s{_cdiDBClusterIdentifier = a});

-- | The password for the master user. Can be any printable ASCII character except "/", """, or "@". __Amazon Aurora__  Not applicable. The password for the master user is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MariaDB__  Constraints: Must contain from 8 to 41 characters. __Microsoft SQL Server__  Constraints: Must contain from 8 to 128 characters. __MySQL__  Constraints: Must contain from 8 to 41 characters. __Oracle__  Constraints: Must contain from 8 to 30 characters. __PostgreSQL__  Constraints: Must contain from 8 to 128 characters.
cdiMasterUserPassword :: Lens' CreateDBInstance (Maybe Text)
cdiMasterUserPassword = lens _cdiMasterUserPassword (\ s a -> s{_cdiMasterUserPassword = a});

-- | Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be private.
cdiPubliclyAccessible :: Lens' CreateDBInstance (Maybe Bool)
cdiPubliclyAccessible = lens _cdiPubliclyAccessible (\ s a -> s{_cdiPubliclyAccessible = a});

-- | Indicates that minor engine upgrades will be applied automatically to the DB instance during the maintenance window. Default: @true@
cdiAutoMinorVersionUpgrade :: Lens' CreateDBInstance (Maybe Bool)
cdiAutoMinorVersionUpgrade = lens _cdiAutoMinorVersionUpgrade (\ s a -> s{_cdiAutoMinorVersionUpgrade = a});

-- | The name for the master user. __Amazon Aurora__  Not applicable. The name for the master user is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MariaDB__  Constraints:     * Must be 1 to 16 alphanumeric characters.     * Cannot be a reserved word for the chosen database engine. __Microsoft SQL Server__  Constraints:     * Must be 1 to 128 alphanumeric characters.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine. __MySQL__  Constraints:     * Must be 1 to 16 alphanumeric characters.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine. __Oracle__  Constraints:     * Must be 1 to 30 alphanumeric characters.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine. __PostgreSQL__  Constraints:     * Must be 1 to 63 alphanumeric characters.     * First character must be a letter.     * Cannot be a reserved word for the chosen database engine.
cdiMasterUsername :: Lens' CreateDBInstance (Maybe Text)
cdiMasterUsername = lens _cdiMasterUsername (\ s a -> s{_cdiMasterUsername = a});

-- | A DB subnet group to associate with this DB instance. If there is no DB subnet group, then it is a non-VPC DB instance.
cdiDBSubnetGroupName :: Lens' CreateDBInstance (Maybe Text)
cdiDBSubnetGroupName = lens _cdiDBSubnetGroupName (\ s a -> s{_cdiDBSubnetGroupName = a});

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting Up and Enabling Enhanced Monitoring> . If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
cdiMonitoringRoleARN :: Lens' CreateDBInstance (Maybe Text)
cdiMonitoringRoleARN = lens _cdiMonitoringRoleARN (\ s a -> s{_cdiMonitoringRoleARN = a});

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance. Constraints: Must be a multiple between 3 and 10 of the storage amount for the DB instance. Must also be an integer multiple of 1000. For example, if the size of your DB instance is 500 GB, then your @Iops@ value can be 2000, 3000, 4000, or 5000.
cdiIOPS :: Lens' CreateDBInstance (Maybe Int)
cdiIOPS = lens _cdiIOPS (\ s a -> s{_cdiIOPS = a});

-- | Specify the Active Directory Domain to create the instance in.
cdiDomain :: Lens' CreateDBInstance (Maybe Text)
cdiDomain = lens _cdiDomain (\ s a -> s{_cdiDomain = a});

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@
cdiMonitoringInterval :: Lens' CreateDBInstance (Maybe Int)
cdiMonitoringInterval = lens _cdiMonitoringInterval (\ s a -> s{_cdiMonitoringInterval = a});

-- | The password for the given ARN from the Key Store in order to access the device.
cdiTDECredentialPassword :: Lens' CreateDBInstance (Maybe Text)
cdiTDECredentialPassword = lens _cdiTDECredentialPassword (\ s a -> s{_cdiTDECredentialPassword = a});

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Managing.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> .  Default: 1 Valid Values: 0 - 15
cdiPromotionTier :: Lens' CreateDBInstance (Maybe Int)
cdiPromotionTier = lens _cdiPromotionTier (\ s a -> s{_cdiPromotionTier = a});

-- | License model information for this DB instance. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
cdiLicenseModel :: Lens' CreateDBInstance (Maybe Text)
cdiLicenseModel = lens _cdiLicenseModel (\ s a -> s{_cdiLicenseModel = a});

-- | The weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC). For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBMaintenance.html DB Instance Maintenance> .  Format: @ddd:hh24:mi-ddd:hh24:mi@  Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region, occurring on a random day of the week. To see the time blocks available, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window> in the /Amazon RDS User Guide./  Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
cdiPreferredMaintenanceWindow :: Lens' CreateDBInstance (Maybe Text)
cdiPreferredMaintenanceWindow = lens _cdiPreferredMaintenanceWindow (\ s a -> s{_cdiPreferredMaintenanceWindow = a});

-- | For supported engines, indicates that the DB instance should be associated with the specified CharacterSet. __Amazon Aurora__  Not applicable. The character set is managed by the DB cluster. For more information, see 'CreateDBCluster' .
cdiCharacterSetName :: Lens' CreateDBInstance (Maybe Text)
cdiCharacterSetName = lens _cdiCharacterSetName (\ s a -> s{_cdiCharacterSetName = a});

-- | The KMS key identifier for an encrypted DB instance. The KMS key identifier is the Amazon Resource Name (ARN) for the KMS encryption key. If you are creating a DB instance with the same AWS account that owns the KMS encryption key used to encrypt the new DB instance, then you can use the KMS key alias instead of the ARN for the KM encryption key. __Amazon Aurora__  Not applicable. The KMS key identifier is managed by the DB cluster. For more information, see 'CreateDBCluster' . If the @StorageEncrypted@ parameter is true, and you do not specify a value for the @KmsKeyId@ parameter, then Amazon RDS will use your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
cdiKMSKeyId :: Lens' CreateDBInstance (Maybe Text)
cdiKMSKeyId = lens _cdiKMSKeyId (\ s a -> s{_cdiKMSKeyId = a});

-- | The name of the DB parameter group to associate with this DB instance. If this argument is omitted, the default DBParameterGroup for the specified engine will be used. Constraints:     * Must be 1 to 255 alphanumeric characters     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
cdiDBParameterGroupName :: Lens' CreateDBInstance (Maybe Text)
cdiDBParameterGroupName = lens _cdiDBParameterGroupName (\ s a -> s{_cdiDBParameterGroupName = a});

-- | The daily time range during which automated backups are created if automated backups are enabled, using the @BackupRetentionPeriod@ parameter. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.BackingUpAndRestoringAmazonRDSInstances.html DB Instance Backups> .  __Amazon Aurora__  Not applicable. The daily time range for creating automated backups is managed by the DB cluster. For more information, see 'CreateDBCluster' . Default: A 30-minute window selected at random from an 8-hour block of time per AWS Region. To see the time blocks available, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow Adjusting the Preferred DB Instance Maintenance Window> .  Constraints:     * Must be in the format @hh24:mi-hh24:mi@ .     * Times should be in Universal Coordinated Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
cdiPreferredBackupWindow :: Lens' CreateDBInstance (Maybe Text)
cdiPreferredBackupWindow = lens _cdiPreferredBackupWindow (\ s a -> s{_cdiPreferredBackupWindow = a});

-- | The EC2 Availability Zone that the database instance will be created in. For information on regions and Availability Zones, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.RegionsAndAvailabilityZones.html Regions and Availability Zones> .  Default: A random, system-chosen Availability Zone in the endpoint's AWS Region. Example: @us-east-1d@  Constraint: The AvailabilityZone parameter cannot be specified if the MultiAZ parameter is set to @true@ . The specified Availability Zone must be in the same AWS Region as the current endpoint.
cdiAvailabilityZone :: Lens' CreateDBInstance (Maybe Text)
cdiAvailabilityZone = lens _cdiAvailabilityZone (\ s a -> s{_cdiAvailabilityZone = a});

-- | The number of days for which automated backups are retained. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups. __Amazon Aurora__  Not applicable. The retention period for automated backups is managed by the DB cluster. For more information, see 'CreateDBCluster' . Default: 1 Constraints:     * Must be a value from 0 to 35     * Cannot be set to 0 if the DB instance is a source to Read Replicas
cdiBackupRetentionPeriod :: Lens' CreateDBInstance (Maybe Int)
cdiBackupRetentionPeriod = lens _cdiBackupRetentionPeriod (\ s a -> s{_cdiBackupRetentionPeriod = a});

-- | A list of EC2 VPC security groups to associate with this DB instance. __Amazon Aurora__  Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster. For more information, see 'CreateDBCluster' . Default: The default EC2 VPC security group for the DB subnet group's VPC.
cdiVPCSecurityGroupIds :: Lens' CreateDBInstance [Text]
cdiVPCSecurityGroupIds = lens _cdiVPCSecurityGroupIds (\ s a -> s{_cdiVPCSecurityGroupIds = a}) . _Default . _Coerce;

-- | Specifies if the DB instance is a Multi-AZ deployment. You cannot set the AvailabilityZone parameter if the MultiAZ parameter is set to true.
cdiMultiAZ :: Lens' CreateDBInstance (Maybe Bool)
cdiMultiAZ = lens _cdiMultiAZ (\ s a -> s{_cdiMultiAZ = a});

-- | The amount of storage (in gigabytes) to be initially allocated for the database instance. Type: Integer __Amazon Aurora__  Not applicable. Aurora cluster volumes automatically grow as the amount of data in your database increases, though you are only charged for the space that you use in an Aurora cluster volume. __MySQL__  Constraints: Must be an integer from 5 to 6144. __MariaDB__  Constraints: Must be an integer from 5 to 6144. __PostgreSQL__  Constraints: Must be an integer from 5 to 6144. __Oracle__  Constraints: Must be an integer from 10 to 6144. __SQL Server__  Constraints: Must be an integer from 200 to 4096 (Standard Edition and Enterprise Edition) or from 20 to 4096 (Express Edition and Web Edition)
cdiAllocatedStorage :: Lens' CreateDBInstance (Maybe Int)
cdiAllocatedStorage = lens _cdiAllocatedStorage (\ s a -> s{_cdiAllocatedStorage = a});

-- | Indicates that the DB instance should be associated with the specified option group. Permanent options, such as the TDE option for Oracle Advanced Security TDE, cannot be removed from an option group, and that option group cannot be removed from a DB instance once it is associated with a DB instance
cdiOptionGroupName :: Lens' CreateDBInstance (Maybe Text)
cdiOptionGroupName = lens _cdiOptionGroupName (\ s a -> s{_cdiOptionGroupName = a});

-- | True to copy all tags from the DB instance to snapshots of the DB instance; otherwise false. The default is false.
cdiCopyTagsToSnapshot :: Lens' CreateDBInstance (Maybe Bool)
cdiCopyTagsToSnapshot = lens _cdiCopyTagsToSnapshot (\ s a -> s{_cdiCopyTagsToSnapshot = a});

-- | The time zone of the DB instance. The time zone parameter is currently supported only by <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.TimeZone Microsoft SQL Server> .
cdiTimezone :: Lens' CreateDBInstance (Maybe Text)
cdiTimezone = lens _cdiTimezone (\ s a -> s{_cdiTimezone = a});

-- | The ARN from the Key Store with which to associate the instance for TDE encryption.
cdiTDECredentialARN :: Lens' CreateDBInstance (Maybe Text)
cdiTDECredentialARN = lens _cdiTDECredentialARN (\ s a -> s{_cdiTDECredentialARN = a});

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
cdiDomainIAMRoleName :: Lens' CreateDBInstance (Maybe Text)
cdiDomainIAMRoleName = lens _cdiDomainIAMRoleName (\ s a -> s{_cdiDomainIAMRoleName = a});

-- | Undocumented member.
cdiTags :: Lens' CreateDBInstance [Tag]
cdiTags = lens _cdiTags (\ s a -> s{_cdiTags = a}) . _Default . _Coerce;

-- | The port number on which the database accepts connections. __MySQL__  Default: @3306@  Valid Values: @1150-65535@  Type: Integer __MariaDB__  Default: @3306@  Valid Values: @1150-65535@  Type: Integer __PostgreSQL__  Default: @5432@  Valid Values: @1150-65535@  Type: Integer __Oracle__  Default: @1521@  Valid Values: @1150-65535@  __SQL Server__  Default: @1433@  Valid Values: @1150-65535@ except for @1434@ , @3389@ , @47001@ , @49152@ , and @49152@ through @49156@ .  __Amazon Aurora__  Default: @3306@  Valid Values: @1150-65535@  Type: Integer
cdiPort :: Lens' CreateDBInstance (Maybe Int)
cdiPort = lens _cdiPort (\ s a -> s{_cdiPort = a});

-- | True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts; otherwise false.  You can enable IAM database authentication for the following database engines: __Amazon Aurora__  Not applicable. Mapping AWS IAM accounts to database accounts is managed by the DB cluster. For more information, see 'CreateDBCluster' . __MySQL__      * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher Default: @false@
cdiEnableIAMDatabaseAuthentication :: Lens' CreateDBInstance (Maybe Bool)
cdiEnableIAMDatabaseAuthentication = lens _cdiEnableIAMDatabaseAuthentication (\ s a -> s{_cdiEnableIAMDatabaseAuthentication = a});

-- | Specifies the storage type to be associated with the DB instance. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified; otherwise @standard@
cdiStorageType :: Lens' CreateDBInstance (Maybe Text)
cdiStorageType = lens _cdiStorageType (\ s a -> s{_cdiStorageType = a});

-- | The meaning of this parameter differs according to the database engine you use. Type: String __MySQL__  The name of the database to create when the DB instance is created. If this parameter is not specified, no database is created in the DB instance. Constraints:     * Must contain 1 to 64 alphanumeric characters     * Cannot be a word reserved by the specified database engine __MariaDB__  The name of the database to create when the DB instance is created. If this parameter is not specified, no database is created in the DB instance. Constraints:     * Must contain 1 to 64 alphanumeric characters     * Cannot be a word reserved by the specified database engine __PostgreSQL__  The name of the database to create when the DB instance is created. If this parameter is not specified, the default "postgres" database is created in the DB instance. Constraints:     * Must contain 1 to 63 alphanumeric characters     * Must begin with a letter or an underscore. Subsequent characters can be letters, underscores, or digits (0-9).     * Cannot be a word reserved by the specified database engine __Oracle__  The Oracle System ID (SID) of the created DB instance. If you specify @null@ , the default value @ORCL@ is used. You can't specify the string NULL, or any other reserved word, for @DBName@ .  Default: @ORCL@  Constraints:     * Cannot be longer than 8 characters __SQL Server__  Not applicable. Must be null. __Amazon Aurora__  The name of the database to create when the primary instance of the DB cluster is created. If this parameter is not specified, no database is created in the DB instance. Constraints:     * Must contain 1 to 64 alphanumeric characters     * Cannot be a word reserved by the specified database engine
cdiDBName :: Lens' CreateDBInstance (Maybe Text)
cdiDBName = lens _cdiDBName (\ s a -> s{_cdiDBName = a});

-- | The DB instance identifier. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@
cdiDBInstanceIdentifier :: Lens' CreateDBInstance Text
cdiDBInstanceIdentifier = lens _cdiDBInstanceIdentifier (\ s a -> s{_cdiDBInstanceIdentifier = a});

-- | The compute and memory capacity of the DB instance. Note that not all instance classes are available in all regions for all DB engines. Valid Values: @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.m4.large | db.m4.xlarge | db.m4.2xlarge | db.m4.4xlarge | db.m4.10xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium | db.t2.large@
cdiDBInstanceClass :: Lens' CreateDBInstance Text
cdiDBInstanceClass = lens _cdiDBInstanceClass (\ s a -> s{_cdiDBInstanceClass = a});

-- | The name of the database engine to be used for this instance.  Not every database engine is available for every AWS Region.  Valid Values:      * @aurora@      * @mariadb@      * @mysql@      * @oracle-ee@      * @oracle-se2@      * @oracle-se1@      * @oracle-se@      * @postgres@      * @sqlserver-ee@      * @sqlserver-se@      * @sqlserver-ex@      * @sqlserver-web@
cdiEngine :: Lens' CreateDBInstance Text
cdiEngine = lens _cdiEngine (\ s a -> s{_cdiEngine = a});

instance AWSRequest CreateDBInstance where
        type Rs CreateDBInstance = CreateDBInstanceResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "CreateDBInstanceResult"
              (\ s h x ->
                 CreateDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable CreateDBInstance

instance NFData CreateDBInstance

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
               "Iops" =: _cdiIOPS, "Domain" =: _cdiDomain,
               "MonitoringInterval" =: _cdiMonitoringInterval,
               "TdeCredentialPassword" =: _cdiTDECredentialPassword,
               "PromotionTier" =: _cdiPromotionTier,
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
               "Timezone" =: _cdiTimezone,
               "TdeCredentialArn" =: _cdiTDECredentialARN,
               "DomainIAMRoleName" =: _cdiDomainIAMRoleName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdiTags),
               "Port" =: _cdiPort,
               "EnableIAMDatabaseAuthentication" =:
                 _cdiEnableIAMDatabaseAuthentication,
               "StorageType" =: _cdiStorageType,
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
-- * 'cdirsDBInstance' - Undocumented member.
--
-- * 'cdirsResponseStatus' - -- | The response status code.
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

-- | -- | The response status code.
cdirsResponseStatus :: Lens' CreateDBInstanceResponse Int
cdirsResponseStatus = lens _cdirsResponseStatus (\ s a -> s{_cdirsResponseStatus = a});

instance NFData CreateDBInstanceResponse
