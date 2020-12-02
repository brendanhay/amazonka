{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabase where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.PendingMaintenanceAction
import Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
import Network.AWS.Lightsail.Types.RelationalDatabaseHardware
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes a database.
--
--
--
-- /See:/ 'relationalDatabase' smart constructor.
data RelationalDatabase = RelationalDatabase'
  { _rdEngineVersion ::
      !(Maybe Text),
    _rdRelationalDatabaseBundleId :: !(Maybe Text),
    _rdMasterEndpoint ::
      !(Maybe RelationalDatabaseEndpoint),
    _rdState :: !(Maybe Text),
    _rdResourceType :: !(Maybe ResourceType),
    _rdPubliclyAccessible :: !(Maybe Bool),
    _rdMasterUsername :: !(Maybe Text),
    _rdArn :: !(Maybe Text),
    _rdCreatedAt :: !(Maybe POSIX),
    _rdLocation :: !(Maybe ResourceLocation),
    _rdEngine :: !(Maybe Text),
    _rdLatestRestorableTime :: !(Maybe POSIX),
    _rdPreferredMaintenanceWindow :: !(Maybe Text),
    _rdRelationalDatabaseBlueprintId :: !(Maybe Text),
    _rdCaCertificateIdentifier :: !(Maybe Text),
    _rdName :: !(Maybe Text),
    _rdBackupRetentionEnabled :: !(Maybe Bool),
    _rdPreferredBackupWindow :: !(Maybe Text),
    _rdPendingMaintenanceActions ::
      !(Maybe [PendingMaintenanceAction]),
    _rdSupportCode :: !(Maybe Text),
    _rdSecondaryAvailabilityZone :: !(Maybe Text),
    _rdPendingModifiedValues ::
      !(Maybe PendingModifiedRelationalDatabaseValues),
    _rdMasterDatabaseName :: !(Maybe Text),
    _rdHardware :: !(Maybe RelationalDatabaseHardware),
    _rdParameterApplyStatus :: !(Maybe Text),
    _rdTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RelationalDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdEngineVersion' - The database engine version (for example, @5.7.23@ ).
--
-- * 'rdRelationalDatabaseBundleId' - The bundle ID for the database. A bundle describes the performance specifications for your database.
--
-- * 'rdMasterEndpoint' - The master endpoint for the database.
--
-- * 'rdState' - Describes the current state of the database.
--
-- * 'rdResourceType' - The Lightsail resource type for the database (for example, @RelationalDatabase@ ).
--
-- * 'rdPubliclyAccessible' - A Boolean value indicating whether the database is publicly accessible.
--
-- * 'rdMasterUsername' - The master user name of the database.
--
-- * 'rdArn' - The Amazon Resource Name (ARN) of the database.
--
-- * 'rdCreatedAt' - The timestamp when the database was created. Formatted in Unix time.
--
-- * 'rdLocation' - The Region name and Availability Zone where the database is located.
--
-- * 'rdEngine' - The database software (for example, @MySQL@ ).
--
-- * 'rdLatestRestorableTime' - The latest point in time to which the database can be restored. Formatted in Unix time.
--
-- * 'rdPreferredMaintenanceWindow' - The weekly time range during which system maintenance can occur on the database. In the format @ddd:hh24:mi-ddd:hh24:mi@ . For example, @Tue:17:00-Tue:17:30@ .
--
-- * 'rdRelationalDatabaseBlueprintId' - The blueprint ID for the database. A blueprint describes the major engine version of a database.
--
-- * 'rdCaCertificateIdentifier' - The certificate associated with the database.
--
-- * 'rdName' - The unique name of the database resource in Lightsail.
--
-- * 'rdBackupRetentionEnabled' - A Boolean value indicating whether automated backup retention is enabled for the database.
--
-- * 'rdPreferredBackupWindow' - The daily time range during which automated backups are created for the database (for example, @16:00-16:30@ ).
--
-- * 'rdPendingMaintenanceActions' - Describes the pending maintenance actions for the database.
--
-- * 'rdSupportCode' - The support code for the database. Include this code in your email to support when you have questions about a database in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'rdSecondaryAvailabilityZone' - Describes the secondary Availability Zone of a high availability database. The secondary database is used for failover support of a high availability database.
--
-- * 'rdPendingModifiedValues' - Describes pending database value modifications.
--
-- * 'rdMasterDatabaseName' - The name of the master database created when the Lightsail database resource is created.
--
-- * 'rdHardware' - Describes the hardware of the database.
--
-- * 'rdParameterApplyStatus' - The status of parameter updates for the database.
--
-- * 'rdTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
relationalDatabase ::
  RelationalDatabase
relationalDatabase =
  RelationalDatabase'
    { _rdEngineVersion = Nothing,
      _rdRelationalDatabaseBundleId = Nothing,
      _rdMasterEndpoint = Nothing,
      _rdState = Nothing,
      _rdResourceType = Nothing,
      _rdPubliclyAccessible = Nothing,
      _rdMasterUsername = Nothing,
      _rdArn = Nothing,
      _rdCreatedAt = Nothing,
      _rdLocation = Nothing,
      _rdEngine = Nothing,
      _rdLatestRestorableTime = Nothing,
      _rdPreferredMaintenanceWindow = Nothing,
      _rdRelationalDatabaseBlueprintId = Nothing,
      _rdCaCertificateIdentifier = Nothing,
      _rdName = Nothing,
      _rdBackupRetentionEnabled = Nothing,
      _rdPreferredBackupWindow = Nothing,
      _rdPendingMaintenanceActions = Nothing,
      _rdSupportCode = Nothing,
      _rdSecondaryAvailabilityZone = Nothing,
      _rdPendingModifiedValues = Nothing,
      _rdMasterDatabaseName = Nothing,
      _rdHardware = Nothing,
      _rdParameterApplyStatus = Nothing,
      _rdTags = Nothing
    }

-- | The database engine version (for example, @5.7.23@ ).
rdEngineVersion :: Lens' RelationalDatabase (Maybe Text)
rdEngineVersion = lens _rdEngineVersion (\s a -> s {_rdEngineVersion = a})

-- | The bundle ID for the database. A bundle describes the performance specifications for your database.
rdRelationalDatabaseBundleId :: Lens' RelationalDatabase (Maybe Text)
rdRelationalDatabaseBundleId = lens _rdRelationalDatabaseBundleId (\s a -> s {_rdRelationalDatabaseBundleId = a})

-- | The master endpoint for the database.
rdMasterEndpoint :: Lens' RelationalDatabase (Maybe RelationalDatabaseEndpoint)
rdMasterEndpoint = lens _rdMasterEndpoint (\s a -> s {_rdMasterEndpoint = a})

-- | Describes the current state of the database.
rdState :: Lens' RelationalDatabase (Maybe Text)
rdState = lens _rdState (\s a -> s {_rdState = a})

-- | The Lightsail resource type for the database (for example, @RelationalDatabase@ ).
rdResourceType :: Lens' RelationalDatabase (Maybe ResourceType)
rdResourceType = lens _rdResourceType (\s a -> s {_rdResourceType = a})

-- | A Boolean value indicating whether the database is publicly accessible.
rdPubliclyAccessible :: Lens' RelationalDatabase (Maybe Bool)
rdPubliclyAccessible = lens _rdPubliclyAccessible (\s a -> s {_rdPubliclyAccessible = a})

-- | The master user name of the database.
rdMasterUsername :: Lens' RelationalDatabase (Maybe Text)
rdMasterUsername = lens _rdMasterUsername (\s a -> s {_rdMasterUsername = a})

-- | The Amazon Resource Name (ARN) of the database.
rdArn :: Lens' RelationalDatabase (Maybe Text)
rdArn = lens _rdArn (\s a -> s {_rdArn = a})

-- | The timestamp when the database was created. Formatted in Unix time.
rdCreatedAt :: Lens' RelationalDatabase (Maybe UTCTime)
rdCreatedAt = lens _rdCreatedAt (\s a -> s {_rdCreatedAt = a}) . mapping _Time

-- | The Region name and Availability Zone where the database is located.
rdLocation :: Lens' RelationalDatabase (Maybe ResourceLocation)
rdLocation = lens _rdLocation (\s a -> s {_rdLocation = a})

-- | The database software (for example, @MySQL@ ).
rdEngine :: Lens' RelationalDatabase (Maybe Text)
rdEngine = lens _rdEngine (\s a -> s {_rdEngine = a})

-- | The latest point in time to which the database can be restored. Formatted in Unix time.
rdLatestRestorableTime :: Lens' RelationalDatabase (Maybe UTCTime)
rdLatestRestorableTime = lens _rdLatestRestorableTime (\s a -> s {_rdLatestRestorableTime = a}) . mapping _Time

-- | The weekly time range during which system maintenance can occur on the database. In the format @ddd:hh24:mi-ddd:hh24:mi@ . For example, @Tue:17:00-Tue:17:30@ .
rdPreferredMaintenanceWindow :: Lens' RelationalDatabase (Maybe Text)
rdPreferredMaintenanceWindow = lens _rdPreferredMaintenanceWindow (\s a -> s {_rdPreferredMaintenanceWindow = a})

-- | The blueprint ID for the database. A blueprint describes the major engine version of a database.
rdRelationalDatabaseBlueprintId :: Lens' RelationalDatabase (Maybe Text)
rdRelationalDatabaseBlueprintId = lens _rdRelationalDatabaseBlueprintId (\s a -> s {_rdRelationalDatabaseBlueprintId = a})

-- | The certificate associated with the database.
rdCaCertificateIdentifier :: Lens' RelationalDatabase (Maybe Text)
rdCaCertificateIdentifier = lens _rdCaCertificateIdentifier (\s a -> s {_rdCaCertificateIdentifier = a})

-- | The unique name of the database resource in Lightsail.
rdName :: Lens' RelationalDatabase (Maybe Text)
rdName = lens _rdName (\s a -> s {_rdName = a})

-- | A Boolean value indicating whether automated backup retention is enabled for the database.
rdBackupRetentionEnabled :: Lens' RelationalDatabase (Maybe Bool)
rdBackupRetentionEnabled = lens _rdBackupRetentionEnabled (\s a -> s {_rdBackupRetentionEnabled = a})

-- | The daily time range during which automated backups are created for the database (for example, @16:00-16:30@ ).
rdPreferredBackupWindow :: Lens' RelationalDatabase (Maybe Text)
rdPreferredBackupWindow = lens _rdPreferredBackupWindow (\s a -> s {_rdPreferredBackupWindow = a})

-- | Describes the pending maintenance actions for the database.
rdPendingMaintenanceActions :: Lens' RelationalDatabase [PendingMaintenanceAction]
rdPendingMaintenanceActions = lens _rdPendingMaintenanceActions (\s a -> s {_rdPendingMaintenanceActions = a}) . _Default . _Coerce

-- | The support code for the database. Include this code in your email to support when you have questions about a database in Lightsail. This code enables our support team to look up your Lightsail information more easily.
rdSupportCode :: Lens' RelationalDatabase (Maybe Text)
rdSupportCode = lens _rdSupportCode (\s a -> s {_rdSupportCode = a})

-- | Describes the secondary Availability Zone of a high availability database. The secondary database is used for failover support of a high availability database.
rdSecondaryAvailabilityZone :: Lens' RelationalDatabase (Maybe Text)
rdSecondaryAvailabilityZone = lens _rdSecondaryAvailabilityZone (\s a -> s {_rdSecondaryAvailabilityZone = a})

-- | Describes pending database value modifications.
rdPendingModifiedValues :: Lens' RelationalDatabase (Maybe PendingModifiedRelationalDatabaseValues)
rdPendingModifiedValues = lens _rdPendingModifiedValues (\s a -> s {_rdPendingModifiedValues = a})

-- | The name of the master database created when the Lightsail database resource is created.
rdMasterDatabaseName :: Lens' RelationalDatabase (Maybe Text)
rdMasterDatabaseName = lens _rdMasterDatabaseName (\s a -> s {_rdMasterDatabaseName = a})

-- | Describes the hardware of the database.
rdHardware :: Lens' RelationalDatabase (Maybe RelationalDatabaseHardware)
rdHardware = lens _rdHardware (\s a -> s {_rdHardware = a})

-- | The status of parameter updates for the database.
rdParameterApplyStatus :: Lens' RelationalDatabase (Maybe Text)
rdParameterApplyStatus = lens _rdParameterApplyStatus (\s a -> s {_rdParameterApplyStatus = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
rdTags :: Lens' RelationalDatabase [Tag]
rdTags = lens _rdTags (\s a -> s {_rdTags = a}) . _Default . _Coerce

instance FromJSON RelationalDatabase where
  parseJSON =
    withObject
      "RelationalDatabase"
      ( \x ->
          RelationalDatabase'
            <$> (x .:? "engineVersion")
            <*> (x .:? "relationalDatabaseBundleId")
            <*> (x .:? "masterEndpoint")
            <*> (x .:? "state")
            <*> (x .:? "resourceType")
            <*> (x .:? "publiclyAccessible")
            <*> (x .:? "masterUsername")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "engine")
            <*> (x .:? "latestRestorableTime")
            <*> (x .:? "preferredMaintenanceWindow")
            <*> (x .:? "relationalDatabaseBlueprintId")
            <*> (x .:? "caCertificateIdentifier")
            <*> (x .:? "name")
            <*> (x .:? "backupRetentionEnabled")
            <*> (x .:? "preferredBackupWindow")
            <*> (x .:? "pendingMaintenanceActions" .!= mempty)
            <*> (x .:? "supportCode")
            <*> (x .:? "secondaryAvailabilityZone")
            <*> (x .:? "pendingModifiedValues")
            <*> (x .:? "masterDatabaseName")
            <*> (x .:? "hardware")
            <*> (x .:? "parameterApplyStatus")
            <*> (x .:? "tags" .!= mempty)
      )

instance Hashable RelationalDatabase

instance NFData RelationalDatabase
