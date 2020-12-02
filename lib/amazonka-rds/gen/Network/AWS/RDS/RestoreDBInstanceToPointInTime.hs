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
-- Module      : Network.AWS.RDS.RestoreDBInstanceToPointInTime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a DB instance to an arbitrary point in time. You can restore to any point in time before the time identified by the LatestRestorableTime property. You can restore to a point up to the number of days specified by the BackupRetentionPeriod property.
--
--
-- The target database is created with most of the original configuration, but in a system-selected availability zone, with the default security group, the default subnet group, and the default DB parameter group. By default, the new DB instance is created as a single-AZ deployment except when the instance is a SQL Server instance that has an option group that is associated with mirroring; in this case, the instance becomes a mirrored deployment and not a single-AZ deployment.
--
module Network.AWS.RDS.RestoreDBInstanceToPointInTime
    (
    -- * Creating a Request
      restoreDBInstanceToPointInTime
    , RestoreDBInstanceToPointInTime
    -- * Request Lenses
    , rditpitUseLatestRestorableTime
    , rditpitPubliclyAccessible
    , rditpitAutoMinorVersionUpgrade
    , rditpitDBSubnetGroupName
    , rditpitRestoreTime
    , rditpitIOPS
    , rditpitDomain
    , rditpitEngine
    , rditpitTDECredentialPassword
    , rditpitDBInstanceClass
    , rditpitLicenseModel
    , rditpitAvailabilityZone
    , rditpitMultiAZ
    , rditpitOptionGroupName
    , rditpitCopyTagsToSnapshot
    , rditpitTDECredentialARN
    , rditpitDomainIAMRoleName
    , rditpitTags
    , rditpitPort
    , rditpitEnableIAMDatabaseAuthentication
    , rditpitStorageType
    , rditpitEnableCloudwatchLogsExports
    , rditpitDBName
    , rditpitSourceDBInstanceIdentifier
    , rditpitTargetDBInstanceIdentifier

    -- * Destructuring the Response
    , restoreDBInstanceToPointInTimeResponse
    , RestoreDBInstanceToPointInTimeResponse
    -- * Response Lenses
    , rditpitrsDBInstance
    , rditpitrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'restoreDBInstanceToPointInTime' smart constructor.
data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime'
  { _rditpitUseLatestRestorableTime         :: !(Maybe Bool)
  , _rditpitPubliclyAccessible              :: !(Maybe Bool)
  , _rditpitAutoMinorVersionUpgrade         :: !(Maybe Bool)
  , _rditpitDBSubnetGroupName               :: !(Maybe Text)
  , _rditpitRestoreTime                     :: !(Maybe ISO8601)
  , _rditpitIOPS                            :: !(Maybe Int)
  , _rditpitDomain                          :: !(Maybe Text)
  , _rditpitEngine                          :: !(Maybe Text)
  , _rditpitTDECredentialPassword           :: !(Maybe Text)
  , _rditpitDBInstanceClass                 :: !(Maybe Text)
  , _rditpitLicenseModel                    :: !(Maybe Text)
  , _rditpitAvailabilityZone                :: !(Maybe Text)
  , _rditpitMultiAZ                         :: !(Maybe Bool)
  , _rditpitOptionGroupName                 :: !(Maybe Text)
  , _rditpitCopyTagsToSnapshot              :: !(Maybe Bool)
  , _rditpitTDECredentialARN                :: !(Maybe Text)
  , _rditpitDomainIAMRoleName               :: !(Maybe Text)
  , _rditpitTags                            :: !(Maybe [Tag])
  , _rditpitPort                            :: !(Maybe Int)
  , _rditpitEnableIAMDatabaseAuthentication :: !(Maybe Bool)
  , _rditpitStorageType                     :: !(Maybe Text)
  , _rditpitEnableCloudwatchLogsExports     :: !(Maybe [Text])
  , _rditpitDBName                          :: !(Maybe Text)
  , _rditpitSourceDBInstanceIdentifier      :: !Text
  , _rditpitTargetDBInstanceIdentifier      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBInstanceToPointInTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rditpitUseLatestRestorableTime' - Specifies whether (@true@ ) or not (@false@ ) the DB instance is restored from the latest backup time.  Default: @false@  Constraints: Cannot be specified if RestoreTime parameter is provided.
--
-- * 'rditpitPubliclyAccessible' - Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is private.
--
-- * 'rditpitAutoMinorVersionUpgrade' - Indicates that minor version upgrades are applied automatically to the DB instance during the maintenance window.
--
-- * 'rditpitDBSubnetGroupName' - The DB subnet group name to use for the new instance. Constraints: If supplied, must match the name of an existing DBSubnetGroup. Example: @mySubnetgroup@
--
-- * 'rditpitRestoreTime' - The date and time to restore from. Valid Values: Value must be a time in Universal Coordinated Time (UTC) format Constraints:     * Must be before the latest restorable time for the DB instance     * Cannot be specified if UseLatestRestorableTime parameter is true Example: @2009-09-07T23:45:00Z@
--
-- * 'rditpitIOPS' - The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance. Constraints: Must be an integer greater than 1000. __SQL Server__  Setting the IOPS value for the SQL Server database engine is not supported.
--
-- * 'rditpitDomain' - Specify the Active Directory Domain to restore the instance in.
--
-- * 'rditpitEngine' - The database engine to use for the new instance. Default: The same as source Constraint: Must be compatible with the engine of the source Valid Values:     * @mariadb@      * @mysql@      * @oracle-ee@      * @oracle-se2@      * @oracle-se1@      * @oracle-se@      * @postgres@      * @sqlserver-ee@      * @sqlserver-se@      * @sqlserver-ex@      * @sqlserver-web@
--
-- * 'rditpitTDECredentialPassword' - The password for the given ARN from the key store in order to access the device.
--
-- * 'rditpitDBInstanceClass' - The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the Amazon RDS User Guide.  Default: The same DBInstanceClass as the original DB instance.
--
-- * 'rditpitLicenseModel' - License model information for the restored DB instance. Default: Same as source. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- * 'rditpitAvailabilityZone' - The EC2 Availability Zone that the DB instance is created in. Default: A random, system-chosen Availability Zone. Constraint: You can't specify the AvailabilityZone parameter if the MultiAZ parameter is set to true. Example: @us-east-1a@
--
-- * 'rditpitMultiAZ' - Specifies if the DB instance is a Multi-AZ deployment. Constraint: You can't specify the AvailabilityZone parameter if the MultiAZ parameter is set to @true@ .
--
-- * 'rditpitOptionGroupName' - The name of the option group to be used for the restored DB instance. Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- * 'rditpitCopyTagsToSnapshot' - True to copy all tags from the restored DB instance to snapshots of the DB instance, and otherwise false. The default is false.
--
-- * 'rditpitTDECredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
--
-- * 'rditpitDomainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- * 'rditpitTags' - Undocumented member.
--
-- * 'rditpitPort' - The port number on which the database accepts connections. Constraints: Value must be @1150-65535@  Default: The same port as the original DB instance.
--
-- * 'rditpitEnableIAMDatabaseAuthentication' - True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. You can enable IAM database authentication for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher Default: @false@
--
-- * 'rditpitStorageType' - Specifies the storage type to be associated with the DB instance. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified, otherwise @standard@
--
-- * 'rditpitEnableCloudwatchLogsExports' - The list of logs that the restored DB instance is to export to CloudWatch Logs.
--
-- * 'rditpitDBName' - The database name for the restored DB instance.
--
-- * 'rditpitSourceDBInstanceIdentifier' - The identifier of the source DB instance from which to restore. Constraints:     * Must match the identifier of an existing DB instance.
--
-- * 'rditpitTargetDBInstanceIdentifier' - The name of the new DB instance to be created. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
restoreDBInstanceToPointInTime
    :: Text -- ^ 'rditpitSourceDBInstanceIdentifier'
    -> Text -- ^ 'rditpitTargetDBInstanceIdentifier'
    -> RestoreDBInstanceToPointInTime
restoreDBInstanceToPointInTime pSourceDBInstanceIdentifier_ pTargetDBInstanceIdentifier_ =
  RestoreDBInstanceToPointInTime'
    { _rditpitUseLatestRestorableTime = Nothing
    , _rditpitPubliclyAccessible = Nothing
    , _rditpitAutoMinorVersionUpgrade = Nothing
    , _rditpitDBSubnetGroupName = Nothing
    , _rditpitRestoreTime = Nothing
    , _rditpitIOPS = Nothing
    , _rditpitDomain = Nothing
    , _rditpitEngine = Nothing
    , _rditpitTDECredentialPassword = Nothing
    , _rditpitDBInstanceClass = Nothing
    , _rditpitLicenseModel = Nothing
    , _rditpitAvailabilityZone = Nothing
    , _rditpitMultiAZ = Nothing
    , _rditpitOptionGroupName = Nothing
    , _rditpitCopyTagsToSnapshot = Nothing
    , _rditpitTDECredentialARN = Nothing
    , _rditpitDomainIAMRoleName = Nothing
    , _rditpitTags = Nothing
    , _rditpitPort = Nothing
    , _rditpitEnableIAMDatabaseAuthentication = Nothing
    , _rditpitStorageType = Nothing
    , _rditpitEnableCloudwatchLogsExports = Nothing
    , _rditpitDBName = Nothing
    , _rditpitSourceDBInstanceIdentifier = pSourceDBInstanceIdentifier_
    , _rditpitTargetDBInstanceIdentifier = pTargetDBInstanceIdentifier_
    }


-- | Specifies whether (@true@ ) or not (@false@ ) the DB instance is restored from the latest backup time.  Default: @false@  Constraints: Cannot be specified if RestoreTime parameter is provided.
rditpitUseLatestRestorableTime :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitUseLatestRestorableTime = lens _rditpitUseLatestRestorableTime (\ s a -> s{_rditpitUseLatestRestorableTime = a})

-- | Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance is private.
rditpitPubliclyAccessible :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitPubliclyAccessible = lens _rditpitPubliclyAccessible (\ s a -> s{_rditpitPubliclyAccessible = a})

-- | Indicates that minor version upgrades are applied automatically to the DB instance during the maintenance window.
rditpitAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitAutoMinorVersionUpgrade = lens _rditpitAutoMinorVersionUpgrade (\ s a -> s{_rditpitAutoMinorVersionUpgrade = a})

-- | The DB subnet group name to use for the new instance. Constraints: If supplied, must match the name of an existing DBSubnetGroup. Example: @mySubnetgroup@
rditpitDBSubnetGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitDBSubnetGroupName = lens _rditpitDBSubnetGroupName (\ s a -> s{_rditpitDBSubnetGroupName = a})

-- | The date and time to restore from. Valid Values: Value must be a time in Universal Coordinated Time (UTC) format Constraints:     * Must be before the latest restorable time for the DB instance     * Cannot be specified if UseLatestRestorableTime parameter is true Example: @2009-09-07T23:45:00Z@
rditpitRestoreTime :: Lens' RestoreDBInstanceToPointInTime (Maybe UTCTime)
rditpitRestoreTime = lens _rditpitRestoreTime (\ s a -> s{_rditpitRestoreTime = a}) . mapping _Time

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance. Constraints: Must be an integer greater than 1000. __SQL Server__  Setting the IOPS value for the SQL Server database engine is not supported.
rditpitIOPS :: Lens' RestoreDBInstanceToPointInTime (Maybe Int)
rditpitIOPS = lens _rditpitIOPS (\ s a -> s{_rditpitIOPS = a})

-- | Specify the Active Directory Domain to restore the instance in.
rditpitDomain :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitDomain = lens _rditpitDomain (\ s a -> s{_rditpitDomain = a})

-- | The database engine to use for the new instance. Default: The same as source Constraint: Must be compatible with the engine of the source Valid Values:     * @mariadb@      * @mysql@      * @oracle-ee@      * @oracle-se2@      * @oracle-se1@      * @oracle-se@      * @postgres@      * @sqlserver-ee@      * @sqlserver-se@      * @sqlserver-ex@      * @sqlserver-web@
rditpitEngine :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitEngine = lens _rditpitEngine (\ s a -> s{_rditpitEngine = a})

-- | The password for the given ARN from the key store in order to access the device.
rditpitTDECredentialPassword :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitTDECredentialPassword = lens _rditpitTDECredentialPassword (\ s a -> s{_rditpitTDECredentialPassword = a})

-- | The compute and memory capacity of the Amazon RDS DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the Amazon RDS User Guide.  Default: The same DBInstanceClass as the original DB instance.
rditpitDBInstanceClass :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitDBInstanceClass = lens _rditpitDBInstanceClass (\ s a -> s{_rditpitDBInstanceClass = a})

-- | License model information for the restored DB instance. Default: Same as source. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
rditpitLicenseModel :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitLicenseModel = lens _rditpitLicenseModel (\ s a -> s{_rditpitLicenseModel = a})

-- | The EC2 Availability Zone that the DB instance is created in. Default: A random, system-chosen Availability Zone. Constraint: You can't specify the AvailabilityZone parameter if the MultiAZ parameter is set to true. Example: @us-east-1a@
rditpitAvailabilityZone :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitAvailabilityZone = lens _rditpitAvailabilityZone (\ s a -> s{_rditpitAvailabilityZone = a})

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You can't specify the AvailabilityZone parameter if the MultiAZ parameter is set to @true@ .
rditpitMultiAZ :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitMultiAZ = lens _rditpitMultiAZ (\ s a -> s{_rditpitMultiAZ = a})

-- | The name of the option group to be used for the restored DB instance. Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
rditpitOptionGroupName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitOptionGroupName = lens _rditpitOptionGroupName (\ s a -> s{_rditpitOptionGroupName = a})

-- | True to copy all tags from the restored DB instance to snapshots of the DB instance, and otherwise false. The default is false.
rditpitCopyTagsToSnapshot :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitCopyTagsToSnapshot = lens _rditpitCopyTagsToSnapshot (\ s a -> s{_rditpitCopyTagsToSnapshot = a})

-- | The ARN from the key store with which to associate the instance for TDE encryption.
rditpitTDECredentialARN :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitTDECredentialARN = lens _rditpitTDECredentialARN (\ s a -> s{_rditpitTDECredentialARN = a})

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
rditpitDomainIAMRoleName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitDomainIAMRoleName = lens _rditpitDomainIAMRoleName (\ s a -> s{_rditpitDomainIAMRoleName = a})

-- | Undocumented member.
rditpitTags :: Lens' RestoreDBInstanceToPointInTime [Tag]
rditpitTags = lens _rditpitTags (\ s a -> s{_rditpitTags = a}) . _Default . _Coerce

-- | The port number on which the database accepts connections. Constraints: Value must be @1150-65535@  Default: The same port as the original DB instance.
rditpitPort :: Lens' RestoreDBInstanceToPointInTime (Maybe Int)
rditpitPort = lens _rditpitPort (\ s a -> s{_rditpitPort = a})

-- | True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts, and otherwise false. You can enable IAM database authentication for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher Default: @false@
rditpitEnableIAMDatabaseAuthentication :: Lens' RestoreDBInstanceToPointInTime (Maybe Bool)
rditpitEnableIAMDatabaseAuthentication = lens _rditpitEnableIAMDatabaseAuthentication (\ s a -> s{_rditpitEnableIAMDatabaseAuthentication = a})

-- | Specifies the storage type to be associated with the DB instance. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified, otherwise @standard@
rditpitStorageType :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitStorageType = lens _rditpitStorageType (\ s a -> s{_rditpitStorageType = a})

-- | The list of logs that the restored DB instance is to export to CloudWatch Logs.
rditpitEnableCloudwatchLogsExports :: Lens' RestoreDBInstanceToPointInTime [Text]
rditpitEnableCloudwatchLogsExports = lens _rditpitEnableCloudwatchLogsExports (\ s a -> s{_rditpitEnableCloudwatchLogsExports = a}) . _Default . _Coerce

-- | The database name for the restored DB instance.
rditpitDBName :: Lens' RestoreDBInstanceToPointInTime (Maybe Text)
rditpitDBName = lens _rditpitDBName (\ s a -> s{_rditpitDBName = a})

-- | The identifier of the source DB instance from which to restore. Constraints:     * Must match the identifier of an existing DB instance.
rditpitSourceDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rditpitSourceDBInstanceIdentifier = lens _rditpitSourceDBInstanceIdentifier (\ s a -> s{_rditpitSourceDBInstanceIdentifier = a})

-- | The name of the new DB instance to be created. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
rditpitTargetDBInstanceIdentifier :: Lens' RestoreDBInstanceToPointInTime Text
rditpitTargetDBInstanceIdentifier = lens _rditpitTargetDBInstanceIdentifier (\ s a -> s{_rditpitTargetDBInstanceIdentifier = a})

instance AWSRequest RestoreDBInstanceToPointInTime
         where
        type Rs RestoreDBInstanceToPointInTime =
             RestoreDBInstanceToPointInTimeResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "RestoreDBInstanceToPointInTimeResult"
              (\ s h x ->
                 RestoreDBInstanceToPointInTimeResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable RestoreDBInstanceToPointInTime
         where

instance NFData RestoreDBInstanceToPointInTime where

instance ToHeaders RestoreDBInstanceToPointInTime
         where
        toHeaders = const mempty

instance ToPath RestoreDBInstanceToPointInTime where
        toPath = const "/"

instance ToQuery RestoreDBInstanceToPointInTime where
        toQuery RestoreDBInstanceToPointInTime'{..}
          = mconcat
              ["Action" =:
                 ("RestoreDBInstanceToPointInTime" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "UseLatestRestorableTime" =:
                 _rditpitUseLatestRestorableTime,
               "PubliclyAccessible" =: _rditpitPubliclyAccessible,
               "AutoMinorVersionUpgrade" =:
                 _rditpitAutoMinorVersionUpgrade,
               "DBSubnetGroupName" =: _rditpitDBSubnetGroupName,
               "RestoreTime" =: _rditpitRestoreTime,
               "Iops" =: _rditpitIOPS, "Domain" =: _rditpitDomain,
               "Engine" =: _rditpitEngine,
               "TdeCredentialPassword" =:
                 _rditpitTDECredentialPassword,
               "DBInstanceClass" =: _rditpitDBInstanceClass,
               "LicenseModel" =: _rditpitLicenseModel,
               "AvailabilityZone" =: _rditpitAvailabilityZone,
               "MultiAZ" =: _rditpitMultiAZ,
               "OptionGroupName" =: _rditpitOptionGroupName,
               "CopyTagsToSnapshot" =: _rditpitCopyTagsToSnapshot,
               "TdeCredentialArn" =: _rditpitTDECredentialARN,
               "DomainIAMRoleName" =: _rditpitDomainIAMRoleName,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _rditpitTags),
               "Port" =: _rditpitPort,
               "EnableIAMDatabaseAuthentication" =:
                 _rditpitEnableIAMDatabaseAuthentication,
               "StorageType" =: _rditpitStorageType,
               "EnableCloudwatchLogsExports" =:
                 toQuery
                   (toQueryList "member" <$>
                      _rditpitEnableCloudwatchLogsExports),
               "DBName" =: _rditpitDBName,
               "SourceDBInstanceIdentifier" =:
                 _rditpitSourceDBInstanceIdentifier,
               "TargetDBInstanceIdentifier" =:
                 _rditpitTargetDBInstanceIdentifier]

-- | /See:/ 'restoreDBInstanceToPointInTimeResponse' smart constructor.
data RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse'
  { _rditpitrsDBInstance     :: !(Maybe DBInstance)
  , _rditpitrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreDBInstanceToPointInTimeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rditpitrsDBInstance' - Undocumented member.
--
-- * 'rditpitrsResponseStatus' - -- | The response status code.
restoreDBInstanceToPointInTimeResponse
    :: Int -- ^ 'rditpitrsResponseStatus'
    -> RestoreDBInstanceToPointInTimeResponse
restoreDBInstanceToPointInTimeResponse pResponseStatus_ =
  RestoreDBInstanceToPointInTimeResponse'
    { _rditpitrsDBInstance = Nothing
    , _rditpitrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
rditpitrsDBInstance :: Lens' RestoreDBInstanceToPointInTimeResponse (Maybe DBInstance)
rditpitrsDBInstance = lens _rditpitrsDBInstance (\ s a -> s{_rditpitrsDBInstance = a})

-- | -- | The response status code.
rditpitrsResponseStatus :: Lens' RestoreDBInstanceToPointInTimeResponse Int
rditpitrsResponseStatus = lens _rditpitrsResponseStatus (\ s a -> s{_rditpitrsResponseStatus = a})

instance NFData
           RestoreDBInstanceToPointInTimeResponse
         where
