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
-- Module      : Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance from a DB snapshot. The target database is created from the source database restore point with the most of original configuration with the default security group and the default DB parameter group. By default, the new DB instance is created as a single-AZ deployment except when the instance is a SQL Server instance that has an option group that is associated with mirroring; in this case, the instance becomes a mirrored AZ deployment and not a single-AZ deployment.
--
--
-- If your intent is to replace your original DB instance with the new, restored DB instance, then rename your original DB instance before you call the RestoreDBInstanceFromDBSnapshot action. RDS does not allow two DB instances with the same name. Once you have renamed your original DB instance with a different identifier, then you can pass the original name of the DB instance as the DBInstanceIdentifier in the call to the RestoreDBInstanceFromDBSnapshot action. The result is that you will replace the original DB instance with the DB instance created from the snapshot.
--
-- If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
--
module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
    (
    -- * Creating a Request
      restoreDBInstanceFromDBSnapshot
    , RestoreDBInstanceFromDBSnapshot
    -- * Request Lenses
    , rdifdsPubliclyAccessible
    , rdifdsAutoMinorVersionUpgrade
    , rdifdsDBSubnetGroupName
    , rdifdsIOPS
    , rdifdsDomain
    , rdifdsEngine
    , rdifdsTDECredentialPassword
    , rdifdsDBInstanceClass
    , rdifdsLicenseModel
    , rdifdsAvailabilityZone
    , rdifdsMultiAZ
    , rdifdsOptionGroupName
    , rdifdsCopyTagsToSnapshot
    , rdifdsTDECredentialARN
    , rdifdsDomainIAMRoleName
    , rdifdsTags
    , rdifdsPort
    , rdifdsEnableIAMDatabaseAuthentication
    , rdifdsStorageType
    , rdifdsDBName
    , rdifdsDBInstanceIdentifier
    , rdifdsDBSnapshotIdentifier

    -- * Destructuring the Response
    , restoreDBInstanceFromDBSnapshotResponse
    , RestoreDBInstanceFromDBSnapshotResponse
    -- * Response Lenses
    , rdifdsrsDBInstance
    , rdifdsrsResponseStatus
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
-- /See:/ 'restoreDBInstanceFromDBSnapshot' smart constructor.
data RestoreDBInstanceFromDBSnapshot = RestoreDBInstanceFromDBSnapshot'
    { _rdifdsPubliclyAccessible              :: !(Maybe Bool)
    , _rdifdsAutoMinorVersionUpgrade         :: !(Maybe Bool)
    , _rdifdsDBSubnetGroupName               :: !(Maybe Text)
    , _rdifdsIOPS                            :: !(Maybe Int)
    , _rdifdsDomain                          :: !(Maybe Text)
    , _rdifdsEngine                          :: !(Maybe Text)
    , _rdifdsTDECredentialPassword           :: !(Maybe Text)
    , _rdifdsDBInstanceClass                 :: !(Maybe Text)
    , _rdifdsLicenseModel                    :: !(Maybe Text)
    , _rdifdsAvailabilityZone                :: !(Maybe Text)
    , _rdifdsMultiAZ                         :: !(Maybe Bool)
    , _rdifdsOptionGroupName                 :: !(Maybe Text)
    , _rdifdsCopyTagsToSnapshot              :: !(Maybe Bool)
    , _rdifdsTDECredentialARN                :: !(Maybe Text)
    , _rdifdsDomainIAMRoleName               :: !(Maybe Text)
    , _rdifdsTags                            :: !(Maybe [Tag])
    , _rdifdsPort                            :: !(Maybe Int)
    , _rdifdsEnableIAMDatabaseAuthentication :: !(Maybe Bool)
    , _rdifdsStorageType                     :: !(Maybe Text)
    , _rdifdsDBName                          :: !(Maybe Text)
    , _rdifdsDBInstanceIdentifier            :: !Text
    , _rdifdsDBSnapshotIdentifier            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RestoreDBInstanceFromDBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdifdsPubliclyAccessible' - Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be private.
--
-- * 'rdifdsAutoMinorVersionUpgrade' - Indicates that minor version upgrades will be applied automatically to the DB instance during the maintenance window.
--
-- * 'rdifdsDBSubnetGroupName' - The DB subnet group name to use for the new instance. Constraints: Must contain no more than 255 alphanumeric characters, periods, underscores, spaces, or hyphens. Must not be default. Example: @mySubnetgroup@
--
-- * 'rdifdsIOPS' - Specifies the amount of provisioned IOPS for the DB instance, expressed in I/O operations per second. If this parameter is not specified, the IOPS value will be taken from the backup. If this parameter is set to 0, the new instance will be converted to a non-PIOPS instance, which will take additional time, though your DB instance will be available for connections before the conversion starts. Constraints: Must be an integer greater than 1000. __SQL Server__  Setting the IOPS value for the SQL Server database engine is not supported.
--
-- * 'rdifdsDomain' - Specify the Active Directory Domain to restore the instance in.
--
-- * 'rdifdsEngine' - The database engine to use for the new instance. Default: The same as source Constraint: Must be compatible with the engine of the source. You can restore a MariaDB 10.1 DB instance from a MySQL 5.6 snapshot. Valid Values: @MySQL@ | @mariadb@ | @oracle-se1@ | @oracle-se@ | @oracle-ee@ | @sqlserver-ee@ | @sqlserver-se@ | @sqlserver-ex@ | @sqlserver-web@ | @postgres@ | @aurora@
--
-- * 'rdifdsTDECredentialPassword' - The password for the given ARN from the Key Store in order to access the device.
--
-- * 'rdifdsDBInstanceClass' - The compute and memory capacity of the Amazon RDS DB instance. Valid Values: @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.m4.large | db.m4.xlarge | db.m4.2xlarge | db.m4.4xlarge | db.m4.10xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium | db.t2.large@
--
-- * 'rdifdsLicenseModel' - License model information for the restored DB instance. Default: Same as source. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
--
-- * 'rdifdsAvailabilityZone' - The EC2 Availability Zone that the database instance will be created in. Default: A random, system-chosen Availability Zone. Constraint: You cannot specify the AvailabilityZone parameter if the MultiAZ parameter is set to @true@ . Example: @us-east-1a@
--
-- * 'rdifdsMultiAZ' - Specifies if the DB instance is a Multi-AZ deployment. Constraint: You cannot specify the AvailabilityZone parameter if the MultiAZ parameter is set to @true@ .
--
-- * 'rdifdsOptionGroupName' - The name of the option group to be used for the restored DB instance. Permanent options, such as the TDE option for Oracle Advanced Security TDE, cannot be removed from an option group, and that option group cannot be removed from a DB instance once it is associated with a DB instance
--
-- * 'rdifdsCopyTagsToSnapshot' - True to copy all tags from the restored DB instance to snapshots of the DB instance; otherwise false. The default is false.
--
-- * 'rdifdsTDECredentialARN' - The ARN from the Key Store with which to associate the instance for TDE encryption.
--
-- * 'rdifdsDomainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- * 'rdifdsTags' - Undocumented member.
--
-- * 'rdifdsPort' - The port number on which the database accepts connections. Default: The same port as the original DB instance Constraints: Value must be @1150-65535@
--
-- * 'rdifdsEnableIAMDatabaseAuthentication' - True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts; otherwise false. You can enable IAM database authentication for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher     * Aurora 5.6 or higher. Default: @false@
--
-- * 'rdifdsStorageType' - Specifies the storage type to be associated with the DB instance. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified; otherwise @standard@
--
-- * 'rdifdsDBName' - The database name for the restored DB instance.
--
-- * 'rdifdsDBInstanceIdentifier' - Name of the DB instance to create from the DB snapshot. This parameter isn't case-sensitive. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-snapshot-id@
--
-- * 'rdifdsDBSnapshotIdentifier' - The identifier for the DB snapshot to restore from. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
restoreDBInstanceFromDBSnapshot
    :: Text -- ^ 'rdifdsDBInstanceIdentifier'
    -> Text -- ^ 'rdifdsDBSnapshotIdentifier'
    -> RestoreDBInstanceFromDBSnapshot
restoreDBInstanceFromDBSnapshot pDBInstanceIdentifier_ pDBSnapshotIdentifier_ =
    RestoreDBInstanceFromDBSnapshot'
    { _rdifdsPubliclyAccessible = Nothing
    , _rdifdsAutoMinorVersionUpgrade = Nothing
    , _rdifdsDBSubnetGroupName = Nothing
    , _rdifdsIOPS = Nothing
    , _rdifdsDomain = Nothing
    , _rdifdsEngine = Nothing
    , _rdifdsTDECredentialPassword = Nothing
    , _rdifdsDBInstanceClass = Nothing
    , _rdifdsLicenseModel = Nothing
    , _rdifdsAvailabilityZone = Nothing
    , _rdifdsMultiAZ = Nothing
    , _rdifdsOptionGroupName = Nothing
    , _rdifdsCopyTagsToSnapshot = Nothing
    , _rdifdsTDECredentialARN = Nothing
    , _rdifdsDomainIAMRoleName = Nothing
    , _rdifdsTags = Nothing
    , _rdifdsPort = Nothing
    , _rdifdsEnableIAMDatabaseAuthentication = Nothing
    , _rdifdsStorageType = Nothing
    , _rdifdsDBName = Nothing
    , _rdifdsDBInstanceIdentifier = pDBInstanceIdentifier_
    , _rdifdsDBSnapshotIdentifier = pDBSnapshotIdentifier_
    }

-- | Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be private.
rdifdsPubliclyAccessible :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsPubliclyAccessible = lens _rdifdsPubliclyAccessible (\ s a -> s{_rdifdsPubliclyAccessible = a});

-- | Indicates that minor version upgrades will be applied automatically to the DB instance during the maintenance window.
rdifdsAutoMinorVersionUpgrade :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsAutoMinorVersionUpgrade = lens _rdifdsAutoMinorVersionUpgrade (\ s a -> s{_rdifdsAutoMinorVersionUpgrade = a});

-- | The DB subnet group name to use for the new instance. Constraints: Must contain no more than 255 alphanumeric characters, periods, underscores, spaces, or hyphens. Must not be default. Example: @mySubnetgroup@
rdifdsDBSubnetGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBSubnetGroupName = lens _rdifdsDBSubnetGroupName (\ s a -> s{_rdifdsDBSubnetGroupName = a});

-- | Specifies the amount of provisioned IOPS for the DB instance, expressed in I/O operations per second. If this parameter is not specified, the IOPS value will be taken from the backup. If this parameter is set to 0, the new instance will be converted to a non-PIOPS instance, which will take additional time, though your DB instance will be available for connections before the conversion starts. Constraints: Must be an integer greater than 1000. __SQL Server__  Setting the IOPS value for the SQL Server database engine is not supported.
rdifdsIOPS :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Int)
rdifdsIOPS = lens _rdifdsIOPS (\ s a -> s{_rdifdsIOPS = a});

-- | Specify the Active Directory Domain to restore the instance in.
rdifdsDomain :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDomain = lens _rdifdsDomain (\ s a -> s{_rdifdsDomain = a});

-- | The database engine to use for the new instance. Default: The same as source Constraint: Must be compatible with the engine of the source. You can restore a MariaDB 10.1 DB instance from a MySQL 5.6 snapshot. Valid Values: @MySQL@ | @mariadb@ | @oracle-se1@ | @oracle-se@ | @oracle-ee@ | @sqlserver-ee@ | @sqlserver-se@ | @sqlserver-ex@ | @sqlserver-web@ | @postgres@ | @aurora@
rdifdsEngine :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsEngine = lens _rdifdsEngine (\ s a -> s{_rdifdsEngine = a});

-- | The password for the given ARN from the Key Store in order to access the device.
rdifdsTDECredentialPassword :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsTDECredentialPassword = lens _rdifdsTDECredentialPassword (\ s a -> s{_rdifdsTDECredentialPassword = a});

-- | The compute and memory capacity of the Amazon RDS DB instance. Valid Values: @db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.m4.large | db.m4.xlarge | db.m4.2xlarge | db.m4.4xlarge | db.m4.10xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium | db.t2.large@
rdifdsDBInstanceClass :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBInstanceClass = lens _rdifdsDBInstanceClass (\ s a -> s{_rdifdsDBInstanceClass = a});

-- | License model information for the restored DB instance. Default: Same as source. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@
rdifdsLicenseModel :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsLicenseModel = lens _rdifdsLicenseModel (\ s a -> s{_rdifdsLicenseModel = a});

-- | The EC2 Availability Zone that the database instance will be created in. Default: A random, system-chosen Availability Zone. Constraint: You cannot specify the AvailabilityZone parameter if the MultiAZ parameter is set to @true@ . Example: @us-east-1a@
rdifdsAvailabilityZone :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsAvailabilityZone = lens _rdifdsAvailabilityZone (\ s a -> s{_rdifdsAvailabilityZone = a});

-- | Specifies if the DB instance is a Multi-AZ deployment. Constraint: You cannot specify the AvailabilityZone parameter if the MultiAZ parameter is set to @true@ .
rdifdsMultiAZ :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsMultiAZ = lens _rdifdsMultiAZ (\ s a -> s{_rdifdsMultiAZ = a});

-- | The name of the option group to be used for the restored DB instance. Permanent options, such as the TDE option for Oracle Advanced Security TDE, cannot be removed from an option group, and that option group cannot be removed from a DB instance once it is associated with a DB instance
rdifdsOptionGroupName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsOptionGroupName = lens _rdifdsOptionGroupName (\ s a -> s{_rdifdsOptionGroupName = a});

-- | True to copy all tags from the restored DB instance to snapshots of the DB instance; otherwise false. The default is false.
rdifdsCopyTagsToSnapshot :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsCopyTagsToSnapshot = lens _rdifdsCopyTagsToSnapshot (\ s a -> s{_rdifdsCopyTagsToSnapshot = a});

-- | The ARN from the Key Store with which to associate the instance for TDE encryption.
rdifdsTDECredentialARN :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsTDECredentialARN = lens _rdifdsTDECredentialARN (\ s a -> s{_rdifdsTDECredentialARN = a});

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
rdifdsDomainIAMRoleName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDomainIAMRoleName = lens _rdifdsDomainIAMRoleName (\ s a -> s{_rdifdsDomainIAMRoleName = a});

-- | Undocumented member.
rdifdsTags :: Lens' RestoreDBInstanceFromDBSnapshot [Tag]
rdifdsTags = lens _rdifdsTags (\ s a -> s{_rdifdsTags = a}) . _Default . _Coerce;

-- | The port number on which the database accepts connections. Default: The same port as the original DB instance Constraints: Value must be @1150-65535@
rdifdsPort :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Int)
rdifdsPort = lens _rdifdsPort (\ s a -> s{_rdifdsPort = a});

-- | True to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts; otherwise false. You can enable IAM database authentication for the following database engines     * For MySQL 5.6, minor version 5.6.34 or higher     * For MySQL 5.7, minor version 5.7.16 or higher     * Aurora 5.6 or higher. Default: @false@
rdifdsEnableIAMDatabaseAuthentication :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Bool)
rdifdsEnableIAMDatabaseAuthentication = lens _rdifdsEnableIAMDatabaseAuthentication (\ s a -> s{_rdifdsEnableIAMDatabaseAuthentication = a});

-- | Specifies the storage type to be associated with the DB instance. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified; otherwise @standard@
rdifdsStorageType :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsStorageType = lens _rdifdsStorageType (\ s a -> s{_rdifdsStorageType = a});

-- | The database name for the restored DB instance.
rdifdsDBName :: Lens' RestoreDBInstanceFromDBSnapshot (Maybe Text)
rdifdsDBName = lens _rdifdsDBName (\ s a -> s{_rdifdsDBName = a});

-- | Name of the DB instance to create from the DB snapshot. This parameter isn't case-sensitive. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-snapshot-id@
rdifdsDBInstanceIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdifdsDBInstanceIdentifier = lens _rdifdsDBInstanceIdentifier (\ s a -> s{_rdifdsDBInstanceIdentifier = a});

-- | The identifier for the DB snapshot to restore from. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens If you are restoring from a shared manual DB snapshot, the @DBSnapshotIdentifier@ must be the ARN of the shared DB snapshot.
rdifdsDBSnapshotIdentifier :: Lens' RestoreDBInstanceFromDBSnapshot Text
rdifdsDBSnapshotIdentifier = lens _rdifdsDBSnapshotIdentifier (\ s a -> s{_rdifdsDBSnapshotIdentifier = a});

instance AWSRequest RestoreDBInstanceFromDBSnapshot
         where
        type Rs RestoreDBInstanceFromDBSnapshot =
             RestoreDBInstanceFromDBSnapshotResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "RestoreDBInstanceFromDBSnapshotResult"
              (\ s h x ->
                 RestoreDBInstanceFromDBSnapshotResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable RestoreDBInstanceFromDBSnapshot

instance NFData RestoreDBInstanceFromDBSnapshot

instance ToHeaders RestoreDBInstanceFromDBSnapshot
         where
        toHeaders = const mempty

instance ToPath RestoreDBInstanceFromDBSnapshot where
        toPath = const "/"

instance ToQuery RestoreDBInstanceFromDBSnapshot
         where
        toQuery RestoreDBInstanceFromDBSnapshot'{..}
          = mconcat
              ["Action" =:
                 ("RestoreDBInstanceFromDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "PubliclyAccessible" =: _rdifdsPubliclyAccessible,
               "AutoMinorVersionUpgrade" =:
                 _rdifdsAutoMinorVersionUpgrade,
               "DBSubnetGroupName" =: _rdifdsDBSubnetGroupName,
               "Iops" =: _rdifdsIOPS, "Domain" =: _rdifdsDomain,
               "Engine" =: _rdifdsEngine,
               "TdeCredentialPassword" =:
                 _rdifdsTDECredentialPassword,
               "DBInstanceClass" =: _rdifdsDBInstanceClass,
               "LicenseModel" =: _rdifdsLicenseModel,
               "AvailabilityZone" =: _rdifdsAvailabilityZone,
               "MultiAZ" =: _rdifdsMultiAZ,
               "OptionGroupName" =: _rdifdsOptionGroupName,
               "CopyTagsToSnapshot" =: _rdifdsCopyTagsToSnapshot,
               "TdeCredentialArn" =: _rdifdsTDECredentialARN,
               "DomainIAMRoleName" =: _rdifdsDomainIAMRoleName,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _rdifdsTags),
               "Port" =: _rdifdsPort,
               "EnableIAMDatabaseAuthentication" =:
                 _rdifdsEnableIAMDatabaseAuthentication,
               "StorageType" =: _rdifdsStorageType,
               "DBName" =: _rdifdsDBName,
               "DBInstanceIdentifier" =:
                 _rdifdsDBInstanceIdentifier,
               "DBSnapshotIdentifier" =:
                 _rdifdsDBSnapshotIdentifier]

-- | /See:/ 'restoreDBInstanceFromDBSnapshotResponse' smart constructor.
data RestoreDBInstanceFromDBSnapshotResponse = RestoreDBInstanceFromDBSnapshotResponse'
    { _rdifdsrsDBInstance     :: !(Maybe DBInstance)
    , _rdifdsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RestoreDBInstanceFromDBSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdifdsrsDBInstance' - Undocumented member.
--
-- * 'rdifdsrsResponseStatus' - -- | The response status code.
restoreDBInstanceFromDBSnapshotResponse
    :: Int -- ^ 'rdifdsrsResponseStatus'
    -> RestoreDBInstanceFromDBSnapshotResponse
restoreDBInstanceFromDBSnapshotResponse pResponseStatus_ =
    RestoreDBInstanceFromDBSnapshotResponse'
    { _rdifdsrsDBInstance = Nothing
    , _rdifdsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rdifdsrsDBInstance :: Lens' RestoreDBInstanceFromDBSnapshotResponse (Maybe DBInstance)
rdifdsrsDBInstance = lens _rdifdsrsDBInstance (\ s a -> s{_rdifdsrsDBInstance = a});

-- | -- | The response status code.
rdifdsrsResponseStatus :: Lens' RestoreDBInstanceFromDBSnapshotResponse Int
rdifdsrsResponseStatus = lens _rdifdsrsResponseStatus (\ s a -> s{_rdifdsrsResponseStatus = a});

instance NFData
         RestoreDBInstanceFromDBSnapshotResponse
