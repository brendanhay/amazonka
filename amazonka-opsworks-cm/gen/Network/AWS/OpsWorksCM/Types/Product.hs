{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Types.Product where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types.Sum
import Network.AWS.Prelude

-- | Stores account attributes.
--
--
--
-- /See:/ 'accountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { _aaUsed    :: !(Maybe Int)
  , _aaMaximum :: !(Maybe Int)
  , _aaName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaUsed' - The current usage, such as the current number of servers that are associated with the account.
--
-- * 'aaMaximum' - The maximum allowed value.
--
-- * 'aaName' - The attribute name. The following are supported attribute names.      * /ServerLimit:/ The number of current servers/maximum number of servers allowed. By default, you can have a maximum of 10 servers.      * /ManualBackupLimit:/ The number of current manual backups/maximum number of backups allowed. By default, you can have a maximum of 50 manual backups saved.
accountAttribute
    :: AccountAttribute
accountAttribute =
  AccountAttribute' {_aaUsed = Nothing, _aaMaximum = Nothing, _aaName = Nothing}


-- | The current usage, such as the current number of servers that are associated with the account.
aaUsed :: Lens' AccountAttribute (Maybe Int)
aaUsed = lens _aaUsed (\ s a -> s{_aaUsed = a})

-- | The maximum allowed value.
aaMaximum :: Lens' AccountAttribute (Maybe Int)
aaMaximum = lens _aaMaximum (\ s a -> s{_aaMaximum = a})

-- | The attribute name. The following are supported attribute names.      * /ServerLimit:/ The number of current servers/maximum number of servers allowed. By default, you can have a maximum of 10 servers.      * /ManualBackupLimit:/ The number of current manual backups/maximum number of backups allowed. By default, you can have a maximum of 50 manual backups saved.
aaName :: Lens' AccountAttribute (Maybe Text)
aaName = lens _aaName (\ s a -> s{_aaName = a})

instance FromJSON AccountAttribute where
        parseJSON
          = withObject "AccountAttribute"
              (\ x ->
                 AccountAttribute' <$>
                   (x .:? "Used") <*> (x .:? "Maximum") <*>
                     (x .:? "Name"))

instance Hashable AccountAttribute where

instance NFData AccountAttribute where

-- | Describes a single backup.
--
--
--
-- /See:/ 'backup' smart constructor.
data Backup = Backup'
  { _bEngineVersion              :: !(Maybe Text)
  , _bServiceRoleARN             :: !(Maybe Text)
  , _bStatus                     :: !(Maybe BackupStatus)
  , _bInstanceProfileARN         :: !(Maybe Text)
  , _bSecurityGroupIds           :: !(Maybe [Text])
  , _bStatusDescription          :: !(Maybe Text)
  , _bServerName                 :: !(Maybe Text)
  , _bSubnetIds                  :: !(Maybe [Text])
  , _bKeyPair                    :: !(Maybe Text)
  , _bCreatedAt                  :: !(Maybe POSIX)
  , _bBackupId                   :: !(Maybe Text)
  , _bEngine                     :: !(Maybe Text)
  , _bInstanceType               :: !(Maybe Text)
  , _bEngineModel                :: !(Maybe Text)
  , _bPreferredMaintenanceWindow :: !(Maybe Text)
  , _bUserARN                    :: !(Maybe Text)
  , _bPreferredBackupWindow      :: !(Maybe Text)
  , _bS3LogURL                   :: !(Maybe Text)
  , _bS3DataSize                 :: !(Maybe Int)
  , _bBackupARN                  :: !(Maybe Text)
  , _bS3DataURL                  :: !(Maybe Text)
  , _bDescription                :: !(Maybe Text)
  , _bBackupType                 :: !(Maybe BackupType)
  , _bToolsVersion               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Backup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bEngineVersion' - The engine version that is obtained from the server when the backup is created.
--
-- * 'bServiceRoleARN' - The service role ARN that is obtained from the server when the backup is created.
--
-- * 'bStatus' - The status of a backup while in progress.
--
-- * 'bInstanceProfileARN' - The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup.
--
-- * 'bSecurityGroupIds' - The security group IDs that are obtained from the server when the backup is created.
--
-- * 'bStatusDescription' - An informational message about backup status.
--
-- * 'bServerName' - The name of the server from which the backup was made.
--
-- * 'bSubnetIds' - The subnet IDs that are obtained from the server when the backup is created.
--
-- * 'bKeyPair' - The key pair that is obtained from the server when the backup is created.
--
-- * 'bCreatedAt' - The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@
--
-- * 'bBackupId' - The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@
--
-- * 'bEngine' - The engine type that is obtained from the server when the backup is created.
--
-- * 'bInstanceType' - The instance type that is obtained from the server when the backup is created.
--
-- * 'bEngineModel' - The engine model that is obtained from the server when the backup is created.
--
-- * 'bPreferredMaintenanceWindow' - The preferred maintenance period that is obtained from the server when the backup is created.
--
-- * 'bUserARN' - The IAM user ARN of the requester for manual backups. This field is empty for automated backups.
--
-- * 'bPreferredBackupWindow' - The preferred backup period that is obtained from the server when the backup is created.
--
-- * 'bS3LogURL' - The Amazon S3 URL of the backup's log file.
--
-- * 'bS3DataSize' - This field is deprecated and is no longer used.
--
-- * 'bBackupARN' - The ARN of the backup.
--
-- * 'bS3DataURL' - This field is deprecated and is no longer used.
--
-- * 'bDescription' - A user-provided description for a manual backup. This field is empty for automated backups.
--
-- * 'bBackupType' - The backup type. Valid values are @automated@ or @manual@ .
--
-- * 'bToolsVersion' - The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created.
backup
    :: Backup
backup =
  Backup'
    { _bEngineVersion = Nothing
    , _bServiceRoleARN = Nothing
    , _bStatus = Nothing
    , _bInstanceProfileARN = Nothing
    , _bSecurityGroupIds = Nothing
    , _bStatusDescription = Nothing
    , _bServerName = Nothing
    , _bSubnetIds = Nothing
    , _bKeyPair = Nothing
    , _bCreatedAt = Nothing
    , _bBackupId = Nothing
    , _bEngine = Nothing
    , _bInstanceType = Nothing
    , _bEngineModel = Nothing
    , _bPreferredMaintenanceWindow = Nothing
    , _bUserARN = Nothing
    , _bPreferredBackupWindow = Nothing
    , _bS3LogURL = Nothing
    , _bS3DataSize = Nothing
    , _bBackupARN = Nothing
    , _bS3DataURL = Nothing
    , _bDescription = Nothing
    , _bBackupType = Nothing
    , _bToolsVersion = Nothing
    }


-- | The engine version that is obtained from the server when the backup is created.
bEngineVersion :: Lens' Backup (Maybe Text)
bEngineVersion = lens _bEngineVersion (\ s a -> s{_bEngineVersion = a})

-- | The service role ARN that is obtained from the server when the backup is created.
bServiceRoleARN :: Lens' Backup (Maybe Text)
bServiceRoleARN = lens _bServiceRoleARN (\ s a -> s{_bServiceRoleARN = a})

-- | The status of a backup while in progress.
bStatus :: Lens' Backup (Maybe BackupStatus)
bStatus = lens _bStatus (\ s a -> s{_bStatus = a})

-- | The EC2 instance profile ARN that is obtained from the server when the backup is created. Because this value is stored, you are not required to provide the InstanceProfileArn again if you restore a backup.
bInstanceProfileARN :: Lens' Backup (Maybe Text)
bInstanceProfileARN = lens _bInstanceProfileARN (\ s a -> s{_bInstanceProfileARN = a})

-- | The security group IDs that are obtained from the server when the backup is created.
bSecurityGroupIds :: Lens' Backup [Text]
bSecurityGroupIds = lens _bSecurityGroupIds (\ s a -> s{_bSecurityGroupIds = a}) . _Default . _Coerce

-- | An informational message about backup status.
bStatusDescription :: Lens' Backup (Maybe Text)
bStatusDescription = lens _bStatusDescription (\ s a -> s{_bStatusDescription = a})

-- | The name of the server from which the backup was made.
bServerName :: Lens' Backup (Maybe Text)
bServerName = lens _bServerName (\ s a -> s{_bServerName = a})

-- | The subnet IDs that are obtained from the server when the backup is created.
bSubnetIds :: Lens' Backup [Text]
bSubnetIds = lens _bSubnetIds (\ s a -> s{_bSubnetIds = a}) . _Default . _Coerce

-- | The key pair that is obtained from the server when the backup is created.
bKeyPair :: Lens' Backup (Maybe Text)
bKeyPair = lens _bKeyPair (\ s a -> s{_bKeyPair = a})

-- | The time stamp when the backup was created in the database. Example: @2016-07-29T13:38:47.520Z@
bCreatedAt :: Lens' Backup (Maybe UTCTime)
bCreatedAt = lens _bCreatedAt (\ s a -> s{_bCreatedAt = a}) . mapping _Time

-- | The generated ID of the backup. Example: @myServerName-yyyyMMddHHmmssSSS@
bBackupId :: Lens' Backup (Maybe Text)
bBackupId = lens _bBackupId (\ s a -> s{_bBackupId = a})

-- | The engine type that is obtained from the server when the backup is created.
bEngine :: Lens' Backup (Maybe Text)
bEngine = lens _bEngine (\ s a -> s{_bEngine = a})

-- | The instance type that is obtained from the server when the backup is created.
bInstanceType :: Lens' Backup (Maybe Text)
bInstanceType = lens _bInstanceType (\ s a -> s{_bInstanceType = a})

-- | The engine model that is obtained from the server when the backup is created.
bEngineModel :: Lens' Backup (Maybe Text)
bEngineModel = lens _bEngineModel (\ s a -> s{_bEngineModel = a})

-- | The preferred maintenance period that is obtained from the server when the backup is created.
bPreferredMaintenanceWindow :: Lens' Backup (Maybe Text)
bPreferredMaintenanceWindow = lens _bPreferredMaintenanceWindow (\ s a -> s{_bPreferredMaintenanceWindow = a})

-- | The IAM user ARN of the requester for manual backups. This field is empty for automated backups.
bUserARN :: Lens' Backup (Maybe Text)
bUserARN = lens _bUserARN (\ s a -> s{_bUserARN = a})

-- | The preferred backup period that is obtained from the server when the backup is created.
bPreferredBackupWindow :: Lens' Backup (Maybe Text)
bPreferredBackupWindow = lens _bPreferredBackupWindow (\ s a -> s{_bPreferredBackupWindow = a})

-- | The Amazon S3 URL of the backup's log file.
bS3LogURL :: Lens' Backup (Maybe Text)
bS3LogURL = lens _bS3LogURL (\ s a -> s{_bS3LogURL = a})

-- | This field is deprecated and is no longer used.
bS3DataSize :: Lens' Backup (Maybe Int)
bS3DataSize = lens _bS3DataSize (\ s a -> s{_bS3DataSize = a})

-- | The ARN of the backup.
bBackupARN :: Lens' Backup (Maybe Text)
bBackupARN = lens _bBackupARN (\ s a -> s{_bBackupARN = a})

-- | This field is deprecated and is no longer used.
bS3DataURL :: Lens' Backup (Maybe Text)
bS3DataURL = lens _bS3DataURL (\ s a -> s{_bS3DataURL = a})

-- | A user-provided description for a manual backup. This field is empty for automated backups.
bDescription :: Lens' Backup (Maybe Text)
bDescription = lens _bDescription (\ s a -> s{_bDescription = a})

-- | The backup type. Valid values are @automated@ or @manual@ .
bBackupType :: Lens' Backup (Maybe BackupType)
bBackupType = lens _bBackupType (\ s a -> s{_bBackupType = a})

-- | The version of AWS OpsWorks CM-specific tools that is obtained from the server when the backup is created.
bToolsVersion :: Lens' Backup (Maybe Text)
bToolsVersion = lens _bToolsVersion (\ s a -> s{_bToolsVersion = a})

instance FromJSON Backup where
        parseJSON
          = withObject "Backup"
              (\ x ->
                 Backup' <$>
                   (x .:? "EngineVersion") <*> (x .:? "ServiceRoleArn")
                     <*> (x .:? "Status")
                     <*> (x .:? "InstanceProfileArn")
                     <*> (x .:? "SecurityGroupIds" .!= mempty)
                     <*> (x .:? "StatusDescription")
                     <*> (x .:? "ServerName")
                     <*> (x .:? "SubnetIds" .!= mempty)
                     <*> (x .:? "KeyPair")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "BackupId")
                     <*> (x .:? "Engine")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "EngineModel")
                     <*> (x .:? "PreferredMaintenanceWindow")
                     <*> (x .:? "UserArn")
                     <*> (x .:? "PreferredBackupWindow")
                     <*> (x .:? "S3LogUrl")
                     <*> (x .:? "S3DataSize")
                     <*> (x .:? "BackupArn")
                     <*> (x .:? "S3DataUrl")
                     <*> (x .:? "Description")
                     <*> (x .:? "BackupType")
                     <*> (x .:? "ToolsVersion"))

instance Hashable Backup where

instance NFData Backup where

-- | A name and value pair that is specific to the engine of the server.
--
--
--
-- /See:/ 'engineAttribute' smart constructor.
data EngineAttribute = EngineAttribute'
  { _eaValue :: !(Maybe (Sensitive Text))
  , _eaName  :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'EngineAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaValue' - The value of the engine attribute.
--
-- * 'eaName' - The name of the engine attribute.
engineAttribute
    :: EngineAttribute
engineAttribute = EngineAttribute' {_eaValue = Nothing, _eaName = Nothing}


-- | The value of the engine attribute.
eaValue :: Lens' EngineAttribute (Maybe Text)
eaValue = lens _eaValue (\ s a -> s{_eaValue = a}) . mapping _Sensitive

-- | The name of the engine attribute.
eaName :: Lens' EngineAttribute (Maybe Text)
eaName = lens _eaName (\ s a -> s{_eaName = a})

instance FromJSON EngineAttribute where
        parseJSON
          = withObject "EngineAttribute"
              (\ x ->
                 EngineAttribute' <$>
                   (x .:? "Value") <*> (x .:? "Name"))

instance Hashable EngineAttribute where

instance NFData EngineAttribute where

instance ToJSON EngineAttribute where
        toJSON EngineAttribute'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _eaValue, ("Name" .=) <$> _eaName])

-- | Describes a configuration management server.
--
--
--
-- /See:/ 'server' smart constructor.
data Server = Server'
  { _sEngineVersion              :: !(Maybe Text)
  , _sServiceRoleARN             :: !(Maybe Text)
  , _sDisableAutomatedBackup     :: !(Maybe Bool)
  , _sStatus                     :: !(Maybe ServerStatus)
  , _sInstanceProfileARN         :: !(Maybe Text)
  , _sSecurityGroupIds           :: !(Maybe [Text])
  , _sAssociatePublicIPAddress   :: !(Maybe Bool)
  , _sServerName                 :: !(Maybe Text)
  , _sSubnetIds                  :: !(Maybe [Text])
  , _sKeyPair                    :: !(Maybe Text)
  , _sCreatedAt                  :: !(Maybe POSIX)
  , _sServerARN                  :: !(Maybe Text)
  , _sEngine                     :: !(Maybe Text)
  , _sMaintenanceStatus          :: !(Maybe MaintenanceStatus)
  , _sInstanceType               :: !(Maybe Text)
  , _sEngineModel                :: !(Maybe Text)
  , _sEngineAttributes           :: !(Maybe [EngineAttribute])
  , _sPreferredMaintenanceWindow :: !(Maybe Text)
  , _sPreferredBackupWindow      :: !(Maybe Text)
  , _sStatusReason               :: !(Maybe Text)
  , _sEndpoint                   :: !(Maybe Text)
  , _sCloudFormationStackARN     :: !(Maybe Text)
  , _sBackupRetentionCount       :: !(Maybe Int)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Server' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sEngineVersion' - The engine version of the server. For a Chef server, the valid value for EngineVersion is currently @12@ . For a Puppet server, the valid value is @2017@ .
--
-- * 'sServiceRoleARN' - The service role ARN used to create the server.
--
-- * 'sDisableAutomatedBackup' - Disables automated backups. The number of stored backups is dependent on the value of PreferredBackupCount.
--
-- * 'sStatus' - The server's status. This field displays the states of actions in progress, such as creating, running, or backing up the server, as well as the server's health state.
--
-- * 'sInstanceProfileARN' - The instance profile ARN of the server.
--
-- * 'sSecurityGroupIds' - The security group IDs for the server, as specified in the CloudFormation stack. These might not be the same security groups that are shown in the EC2 console.
--
-- * 'sAssociatePublicIPAddress' - Associate a public IP address with a server that you are launching.
--
-- * 'sServerName' - The name of the server.
--
-- * 'sSubnetIds' - The subnet IDs specified in a CreateServer request.
--
-- * 'sKeyPair' - The key pair associated with the server.
--
-- * 'sCreatedAt' - Time stamp of server creation. Example @2016-07-29T13:38:47.520Z@
--
-- * 'sServerARN' - The ARN of the server.
--
-- * 'sEngine' - The engine type of the server. Valid values in this release include @Chef@ and @Puppet@ .
--
-- * 'sMaintenanceStatus' - The status of the most recent server maintenance run. Shows @SUCCESS@ or @FAILED@ .
--
-- * 'sInstanceType' - The instance type for the server, as specified in the CloudFormation stack. This might not be the same instance type that is shown in the EC2 console.
--
-- * 'sEngineModel' - The engine model of the server. Valid values in this release include @Monolithic@ for Puppet and @Single@ for Chef.
--
-- * 'sEngineAttributes' - The response of a createServer() request returns the master credential to access the server in EngineAttributes. These credentials are not stored by AWS OpsWorks CM; they are returned only as part of the result of createServer().  __Attributes returned in a createServer response for Chef__      * @CHEF_PIVOTAL_KEY@ : A base64-encoded RSA private key that is generated by AWS OpsWorks for Chef Automate. This private key is required to access the Chef API.     * @CHEF_STARTER_KIT@ : A base64-encoded ZIP file. The ZIP file contains a Chef starter kit, which includes a README, a configuration file, and the required RSA private key. Save this file, unzip it, and then change to the directory where you've unzipped the file contents. From this directory, you can run Knife commands. __Attributes returned in a createServer response for Puppet__      * @PUPPET_STARTER_KIT@ : A base64-encoded ZIP file. The ZIP file contains a Puppet starter kit, including a README and a required private key. Save this file, unzip it, and then change to the directory where you've unzipped the file contents.     * @PUPPET_ADMIN_PASSWORD@ : An administrator password that you can use to sign in to the Puppet Enterprise console after the server is online.
--
-- * 'sPreferredMaintenanceWindow' - The preferred maintenance period specified for the server.
--
-- * 'sPreferredBackupWindow' - The preferred backup period specified for the server.
--
-- * 'sStatusReason' - Depending on the server status, this field has either a human-readable message (such as a create or backup error), or an escaped block of JSON (used for health check results).
--
-- * 'sEndpoint' - A DNS name that can be used to access the engine. Example: @myserver-asdfghjkl.us-east-1.opsworks.io@
--
-- * 'sCloudFormationStackARN' - The ARN of the CloudFormation stack that was used to create the server.
--
-- * 'sBackupRetentionCount' - The number of automated backups to keep.
server
    :: Server
server =
  Server'
    { _sEngineVersion = Nothing
    , _sServiceRoleARN = Nothing
    , _sDisableAutomatedBackup = Nothing
    , _sStatus = Nothing
    , _sInstanceProfileARN = Nothing
    , _sSecurityGroupIds = Nothing
    , _sAssociatePublicIPAddress = Nothing
    , _sServerName = Nothing
    , _sSubnetIds = Nothing
    , _sKeyPair = Nothing
    , _sCreatedAt = Nothing
    , _sServerARN = Nothing
    , _sEngine = Nothing
    , _sMaintenanceStatus = Nothing
    , _sInstanceType = Nothing
    , _sEngineModel = Nothing
    , _sEngineAttributes = Nothing
    , _sPreferredMaintenanceWindow = Nothing
    , _sPreferredBackupWindow = Nothing
    , _sStatusReason = Nothing
    , _sEndpoint = Nothing
    , _sCloudFormationStackARN = Nothing
    , _sBackupRetentionCount = Nothing
    }


-- | The engine version of the server. For a Chef server, the valid value for EngineVersion is currently @12@ . For a Puppet server, the valid value is @2017@ .
sEngineVersion :: Lens' Server (Maybe Text)
sEngineVersion = lens _sEngineVersion (\ s a -> s{_sEngineVersion = a})

-- | The service role ARN used to create the server.
sServiceRoleARN :: Lens' Server (Maybe Text)
sServiceRoleARN = lens _sServiceRoleARN (\ s a -> s{_sServiceRoleARN = a})

-- | Disables automated backups. The number of stored backups is dependent on the value of PreferredBackupCount.
sDisableAutomatedBackup :: Lens' Server (Maybe Bool)
sDisableAutomatedBackup = lens _sDisableAutomatedBackup (\ s a -> s{_sDisableAutomatedBackup = a})

-- | The server's status. This field displays the states of actions in progress, such as creating, running, or backing up the server, as well as the server's health state.
sStatus :: Lens' Server (Maybe ServerStatus)
sStatus = lens _sStatus (\ s a -> s{_sStatus = a})

-- | The instance profile ARN of the server.
sInstanceProfileARN :: Lens' Server (Maybe Text)
sInstanceProfileARN = lens _sInstanceProfileARN (\ s a -> s{_sInstanceProfileARN = a})

-- | The security group IDs for the server, as specified in the CloudFormation stack. These might not be the same security groups that are shown in the EC2 console.
sSecurityGroupIds :: Lens' Server [Text]
sSecurityGroupIds = lens _sSecurityGroupIds (\ s a -> s{_sSecurityGroupIds = a}) . _Default . _Coerce

-- | Associate a public IP address with a server that you are launching.
sAssociatePublicIPAddress :: Lens' Server (Maybe Bool)
sAssociatePublicIPAddress = lens _sAssociatePublicIPAddress (\ s a -> s{_sAssociatePublicIPAddress = a})

-- | The name of the server.
sServerName :: Lens' Server (Maybe Text)
sServerName = lens _sServerName (\ s a -> s{_sServerName = a})

-- | The subnet IDs specified in a CreateServer request.
sSubnetIds :: Lens' Server [Text]
sSubnetIds = lens _sSubnetIds (\ s a -> s{_sSubnetIds = a}) . _Default . _Coerce

-- | The key pair associated with the server.
sKeyPair :: Lens' Server (Maybe Text)
sKeyPair = lens _sKeyPair (\ s a -> s{_sKeyPair = a})

-- | Time stamp of server creation. Example @2016-07-29T13:38:47.520Z@
sCreatedAt :: Lens' Server (Maybe UTCTime)
sCreatedAt = lens _sCreatedAt (\ s a -> s{_sCreatedAt = a}) . mapping _Time

-- | The ARN of the server.
sServerARN :: Lens' Server (Maybe Text)
sServerARN = lens _sServerARN (\ s a -> s{_sServerARN = a})

-- | The engine type of the server. Valid values in this release include @Chef@ and @Puppet@ .
sEngine :: Lens' Server (Maybe Text)
sEngine = lens _sEngine (\ s a -> s{_sEngine = a})

-- | The status of the most recent server maintenance run. Shows @SUCCESS@ or @FAILED@ .
sMaintenanceStatus :: Lens' Server (Maybe MaintenanceStatus)
sMaintenanceStatus = lens _sMaintenanceStatus (\ s a -> s{_sMaintenanceStatus = a})

-- | The instance type for the server, as specified in the CloudFormation stack. This might not be the same instance type that is shown in the EC2 console.
sInstanceType :: Lens' Server (Maybe Text)
sInstanceType = lens _sInstanceType (\ s a -> s{_sInstanceType = a})

-- | The engine model of the server. Valid values in this release include @Monolithic@ for Puppet and @Single@ for Chef.
sEngineModel :: Lens' Server (Maybe Text)
sEngineModel = lens _sEngineModel (\ s a -> s{_sEngineModel = a})

-- | The response of a createServer() request returns the master credential to access the server in EngineAttributes. These credentials are not stored by AWS OpsWorks CM; they are returned only as part of the result of createServer().  __Attributes returned in a createServer response for Chef__      * @CHEF_PIVOTAL_KEY@ : A base64-encoded RSA private key that is generated by AWS OpsWorks for Chef Automate. This private key is required to access the Chef API.     * @CHEF_STARTER_KIT@ : A base64-encoded ZIP file. The ZIP file contains a Chef starter kit, which includes a README, a configuration file, and the required RSA private key. Save this file, unzip it, and then change to the directory where you've unzipped the file contents. From this directory, you can run Knife commands. __Attributes returned in a createServer response for Puppet__      * @PUPPET_STARTER_KIT@ : A base64-encoded ZIP file. The ZIP file contains a Puppet starter kit, including a README and a required private key. Save this file, unzip it, and then change to the directory where you've unzipped the file contents.     * @PUPPET_ADMIN_PASSWORD@ : An administrator password that you can use to sign in to the Puppet Enterprise console after the server is online.
sEngineAttributes :: Lens' Server [EngineAttribute]
sEngineAttributes = lens _sEngineAttributes (\ s a -> s{_sEngineAttributes = a}) . _Default . _Coerce

-- | The preferred maintenance period specified for the server.
sPreferredMaintenanceWindow :: Lens' Server (Maybe Text)
sPreferredMaintenanceWindow = lens _sPreferredMaintenanceWindow (\ s a -> s{_sPreferredMaintenanceWindow = a})

-- | The preferred backup period specified for the server.
sPreferredBackupWindow :: Lens' Server (Maybe Text)
sPreferredBackupWindow = lens _sPreferredBackupWindow (\ s a -> s{_sPreferredBackupWindow = a})

-- | Depending on the server status, this field has either a human-readable message (such as a create or backup error), or an escaped block of JSON (used for health check results).
sStatusReason :: Lens' Server (Maybe Text)
sStatusReason = lens _sStatusReason (\ s a -> s{_sStatusReason = a})

-- | A DNS name that can be used to access the engine. Example: @myserver-asdfghjkl.us-east-1.opsworks.io@
sEndpoint :: Lens' Server (Maybe Text)
sEndpoint = lens _sEndpoint (\ s a -> s{_sEndpoint = a})

-- | The ARN of the CloudFormation stack that was used to create the server.
sCloudFormationStackARN :: Lens' Server (Maybe Text)
sCloudFormationStackARN = lens _sCloudFormationStackARN (\ s a -> s{_sCloudFormationStackARN = a})

-- | The number of automated backups to keep.
sBackupRetentionCount :: Lens' Server (Maybe Int)
sBackupRetentionCount = lens _sBackupRetentionCount (\ s a -> s{_sBackupRetentionCount = a})

instance FromJSON Server where
        parseJSON
          = withObject "Server"
              (\ x ->
                 Server' <$>
                   (x .:? "EngineVersion") <*> (x .:? "ServiceRoleArn")
                     <*> (x .:? "DisableAutomatedBackup")
                     <*> (x .:? "Status")
                     <*> (x .:? "InstanceProfileArn")
                     <*> (x .:? "SecurityGroupIds" .!= mempty)
                     <*> (x .:? "AssociatePublicIpAddress")
                     <*> (x .:? "ServerName")
                     <*> (x .:? "SubnetIds" .!= mempty)
                     <*> (x .:? "KeyPair")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "ServerArn")
                     <*> (x .:? "Engine")
                     <*> (x .:? "MaintenanceStatus")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "EngineModel")
                     <*> (x .:? "EngineAttributes" .!= mempty)
                     <*> (x .:? "PreferredMaintenanceWindow")
                     <*> (x .:? "PreferredBackupWindow")
                     <*> (x .:? "StatusReason")
                     <*> (x .:? "Endpoint")
                     <*> (x .:? "CloudFormationStackArn")
                     <*> (x .:? "BackupRetentionCount"))

instance Hashable Server where

instance NFData Server where

-- | An event that is related to the server, such as the start of maintenance or backup.
--
--
--
-- /See:/ 'serverEvent' smart constructor.
data ServerEvent = ServerEvent'
  { _seLogURL     :: !(Maybe Text)
  , _seServerName :: !(Maybe Text)
  , _seCreatedAt  :: !(Maybe POSIX)
  , _seMessage    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServerEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seLogURL' - The Amazon S3 URL of the event's log file.
--
-- * 'seServerName' - The name of the server on or for which the event occurred.
--
-- * 'seCreatedAt' - The time when the event occurred.
--
-- * 'seMessage' - A human-readable informational or status message.
serverEvent
    :: ServerEvent
serverEvent =
  ServerEvent'
    { _seLogURL = Nothing
    , _seServerName = Nothing
    , _seCreatedAt = Nothing
    , _seMessage = Nothing
    }


-- | The Amazon S3 URL of the event's log file.
seLogURL :: Lens' ServerEvent (Maybe Text)
seLogURL = lens _seLogURL (\ s a -> s{_seLogURL = a})

-- | The name of the server on or for which the event occurred.
seServerName :: Lens' ServerEvent (Maybe Text)
seServerName = lens _seServerName (\ s a -> s{_seServerName = a})

-- | The time when the event occurred.
seCreatedAt :: Lens' ServerEvent (Maybe UTCTime)
seCreatedAt = lens _seCreatedAt (\ s a -> s{_seCreatedAt = a}) . mapping _Time

-- | A human-readable informational or status message.
seMessage :: Lens' ServerEvent (Maybe Text)
seMessage = lens _seMessage (\ s a -> s{_seMessage = a})

instance FromJSON ServerEvent where
        parseJSON
          = withObject "ServerEvent"
              (\ x ->
                 ServerEvent' <$>
                   (x .:? "LogUrl") <*> (x .:? "ServerName") <*>
                     (x .:? "CreatedAt")
                     <*> (x .:? "Message"))

instance Hashable ServerEvent where

instance NFData ServerEvent where
