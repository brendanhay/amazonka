{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateRelationalDatabase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database in Amazon Lightsail.
--
--
-- The @create relational database@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateRelationalDatabase
  ( -- * Creating a Request
    createRelationalDatabase,
    CreateRelationalDatabase,

    -- * Request Lenses
    crdMasterUserPassword,
    crdPubliclyAccessible,
    crdPreferredMaintenanceWindow,
    crdPreferredBackupWindow,
    crdAvailabilityZone,
    crdTags,
    crdRelationalDatabaseName,
    crdRelationalDatabaseBlueprintId,
    crdRelationalDatabaseBundleId,
    crdMasterDatabaseName,
    crdMasterUsername,

    -- * Destructuring the Response
    createRelationalDatabaseResponse,
    CreateRelationalDatabaseResponse,

    -- * Response Lenses
    crdrsOperations,
    crdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRelationalDatabase' smart constructor.
data CreateRelationalDatabase = CreateRelationalDatabase'
  { _crdMasterUserPassword ::
      !(Maybe (Sensitive Text)),
    _crdPubliclyAccessible :: !(Maybe Bool),
    _crdPreferredMaintenanceWindow ::
      !(Maybe Text),
    _crdPreferredBackupWindow ::
      !(Maybe Text),
    _crdAvailabilityZone :: !(Maybe Text),
    _crdTags :: !(Maybe [Tag]),
    _crdRelationalDatabaseName :: !Text,
    _crdRelationalDatabaseBlueprintId ::
      !Text,
    _crdRelationalDatabaseBundleId :: !Text,
    _crdMasterDatabaseName :: !Text,
    _crdMasterUsername :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRelationalDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdMasterUserPassword' - The password for the master user of your new database. The password can include any printable ASCII character except "/", """, or "@". Constraints: Must contain 8 to 41 characters.
--
-- * 'crdPubliclyAccessible' - Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
--
-- * 'crdPreferredMaintenanceWindow' - The weekly time range during which system maintenance can occur on your new database. The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. Constraints:     * Must be in the @ddd:hh24:mi-ddd:hh24:mi@ format.     * Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.     * Must be at least 30 minutes.     * Specified in Coordinated Universal Time (UTC).     * Example: @Tue:17:00-Tue:17:30@
--
-- * 'crdPreferredBackupWindow' - The daily time range during which automated backups are created for your new database if automated backups are enabled. The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. For more information about the preferred backup window time blocks for each region, see the <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Working With Backups> guide in the Amazon Relational Database Service (Amazon RDS) documentation. Constraints:     * Must be in the @hh24:mi-hh24:mi@ format. Example: @16:00-16:30@      * Specified in Coordinated Universal Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
--
-- * 'crdAvailabilityZone' - The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format. You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
--
-- * 'crdTags' - The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
--
-- * 'crdRelationalDatabaseName' - The name to use for your new database. Constraints:     * Must contain from 2 to 255 alphanumeric characters, or hyphens.     * The first and last character must be a letter or number.
--
-- * 'crdRelationalDatabaseBlueprintId' - The blueprint ID for your new database. A blueprint describes the major engine version of a database. You can get a list of database blueprints IDs by using the @get relational database blueprints@ operation.
--
-- * 'crdRelationalDatabaseBundleId' - The bundle ID for your new database. A bundle describes the performance specifications for your database. You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
--
-- * 'crdMasterDatabaseName' - The name of the master database created when the Lightsail database resource is created. Constraints:     * Must contain from 1 to 64 alphanumeric characters.     * Cannot be a word reserved by the specified database engine
--
-- * 'crdMasterUsername' - The master user name for your new database. Constraints:     * Master user name is required.     * Must contain from 1 to 16 alphanumeric characters.     * The first character must be a letter.     * Cannot be a reserved word for the database engine you choose. For more information about reserved words in MySQL 5.6 or 5.7, see the Keywords and Reserved Words articles for <https://dev.mysql.com/doc/refman/5.6/en/keywords.html MySQL 5.6> or <https://dev.mysql.com/doc/refman/5.7/en/keywords.html MySQL 5.7> respectively.
createRelationalDatabase ::
  -- | 'crdRelationalDatabaseName'
  Text ->
  -- | 'crdRelationalDatabaseBlueprintId'
  Text ->
  -- | 'crdRelationalDatabaseBundleId'
  Text ->
  -- | 'crdMasterDatabaseName'
  Text ->
  -- | 'crdMasterUsername'
  Text ->
  CreateRelationalDatabase
createRelationalDatabase
  pRelationalDatabaseName_
  pRelationalDatabaseBlueprintId_
  pRelationalDatabaseBundleId_
  pMasterDatabaseName_
  pMasterUsername_ =
    CreateRelationalDatabase'
      { _crdMasterUserPassword = Nothing,
        _crdPubliclyAccessible = Nothing,
        _crdPreferredMaintenanceWindow = Nothing,
        _crdPreferredBackupWindow = Nothing,
        _crdAvailabilityZone = Nothing,
        _crdTags = Nothing,
        _crdRelationalDatabaseName = pRelationalDatabaseName_,
        _crdRelationalDatabaseBlueprintId =
          pRelationalDatabaseBlueprintId_,
        _crdRelationalDatabaseBundleId = pRelationalDatabaseBundleId_,
        _crdMasterDatabaseName = pMasterDatabaseName_,
        _crdMasterUsername = pMasterUsername_
      }

-- | The password for the master user of your new database. The password can include any printable ASCII character except "/", """, or "@". Constraints: Must contain 8 to 41 characters.
crdMasterUserPassword :: Lens' CreateRelationalDatabase (Maybe Text)
crdMasterUserPassword = lens _crdMasterUserPassword (\s a -> s {_crdMasterUserPassword = a}) . mapping _Sensitive

-- | Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
crdPubliclyAccessible :: Lens' CreateRelationalDatabase (Maybe Bool)
crdPubliclyAccessible = lens _crdPubliclyAccessible (\s a -> s {_crdPubliclyAccessible = a})

-- | The weekly time range during which system maintenance can occur on your new database. The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region, occurring on a random day of the week. Constraints:     * Must be in the @ddd:hh24:mi-ddd:hh24:mi@ format.     * Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.     * Must be at least 30 minutes.     * Specified in Coordinated Universal Time (UTC).     * Example: @Tue:17:00-Tue:17:30@
crdPreferredMaintenanceWindow :: Lens' CreateRelationalDatabase (Maybe Text)
crdPreferredMaintenanceWindow = lens _crdPreferredMaintenanceWindow (\s a -> s {_crdPreferredMaintenanceWindow = a})

-- | The daily time range during which automated backups are created for your new database if automated backups are enabled. The default is a 30-minute window selected at random from an 8-hour block of time for each AWS Region. For more information about the preferred backup window time blocks for each region, see the <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Working With Backups> guide in the Amazon Relational Database Service (Amazon RDS) documentation. Constraints:     * Must be in the @hh24:mi-hh24:mi@ format. Example: @16:00-16:30@      * Specified in Coordinated Universal Time (UTC).     * Must not conflict with the preferred maintenance window.     * Must be at least 30 minutes.
crdPreferredBackupWindow :: Lens' CreateRelationalDatabase (Maybe Text)
crdPreferredBackupWindow = lens _crdPreferredBackupWindow (\s a -> s {_crdPreferredBackupWindow = a})

-- | The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format. You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
crdAvailabilityZone :: Lens' CreateRelationalDatabase (Maybe Text)
crdAvailabilityZone = lens _crdAvailabilityZone (\s a -> s {_crdAvailabilityZone = a})

-- | The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
crdTags :: Lens' CreateRelationalDatabase [Tag]
crdTags = lens _crdTags (\s a -> s {_crdTags = a}) . _Default . _Coerce

-- | The name to use for your new database. Constraints:     * Must contain from 2 to 255 alphanumeric characters, or hyphens.     * The first and last character must be a letter or number.
crdRelationalDatabaseName :: Lens' CreateRelationalDatabase Text
crdRelationalDatabaseName = lens _crdRelationalDatabaseName (\s a -> s {_crdRelationalDatabaseName = a})

-- | The blueprint ID for your new database. A blueprint describes the major engine version of a database. You can get a list of database blueprints IDs by using the @get relational database blueprints@ operation.
crdRelationalDatabaseBlueprintId :: Lens' CreateRelationalDatabase Text
crdRelationalDatabaseBlueprintId = lens _crdRelationalDatabaseBlueprintId (\s a -> s {_crdRelationalDatabaseBlueprintId = a})

-- | The bundle ID for your new database. A bundle describes the performance specifications for your database. You can get a list of database bundle IDs by using the @get relational database bundles@ operation.
crdRelationalDatabaseBundleId :: Lens' CreateRelationalDatabase Text
crdRelationalDatabaseBundleId = lens _crdRelationalDatabaseBundleId (\s a -> s {_crdRelationalDatabaseBundleId = a})

-- | The name of the master database created when the Lightsail database resource is created. Constraints:     * Must contain from 1 to 64 alphanumeric characters.     * Cannot be a word reserved by the specified database engine
crdMasterDatabaseName :: Lens' CreateRelationalDatabase Text
crdMasterDatabaseName = lens _crdMasterDatabaseName (\s a -> s {_crdMasterDatabaseName = a})

-- | The master user name for your new database. Constraints:     * Master user name is required.     * Must contain from 1 to 16 alphanumeric characters.     * The first character must be a letter.     * Cannot be a reserved word for the database engine you choose. For more information about reserved words in MySQL 5.6 or 5.7, see the Keywords and Reserved Words articles for <https://dev.mysql.com/doc/refman/5.6/en/keywords.html MySQL 5.6> or <https://dev.mysql.com/doc/refman/5.7/en/keywords.html MySQL 5.7> respectively.
crdMasterUsername :: Lens' CreateRelationalDatabase Text
crdMasterUsername = lens _crdMasterUsername (\s a -> s {_crdMasterUsername = a})

instance AWSRequest CreateRelationalDatabase where
  type Rs CreateRelationalDatabase = CreateRelationalDatabaseResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable CreateRelationalDatabase

instance NFData CreateRelationalDatabase

instance ToHeaders CreateRelationalDatabase where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.CreateRelationalDatabase" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateRelationalDatabase where
  toJSON CreateRelationalDatabase' {..} =
    object
      ( catMaybes
          [ ("masterUserPassword" .=) <$> _crdMasterUserPassword,
            ("publiclyAccessible" .=) <$> _crdPubliclyAccessible,
            ("preferredMaintenanceWindow" .=)
              <$> _crdPreferredMaintenanceWindow,
            ("preferredBackupWindow" .=) <$> _crdPreferredBackupWindow,
            ("availabilityZone" .=) <$> _crdAvailabilityZone,
            ("tags" .=) <$> _crdTags,
            Just ("relationalDatabaseName" .= _crdRelationalDatabaseName),
            Just
              ( "relationalDatabaseBlueprintId"
                  .= _crdRelationalDatabaseBlueprintId
              ),
            Just
              ("relationalDatabaseBundleId" .= _crdRelationalDatabaseBundleId),
            Just ("masterDatabaseName" .= _crdMasterDatabaseName),
            Just ("masterUsername" .= _crdMasterUsername)
          ]
      )

instance ToPath CreateRelationalDatabase where
  toPath = const "/"

instance ToQuery CreateRelationalDatabase where
  toQuery = const mempty

-- | /See:/ 'createRelationalDatabaseResponse' smart constructor.
data CreateRelationalDatabaseResponse = CreateRelationalDatabaseResponse'
  { _crdrsOperations ::
      !(Maybe [Operation]),
    _crdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'crdrsResponseStatus' - -- | The response status code.
createRelationalDatabaseResponse ::
  -- | 'crdrsResponseStatus'
  Int ->
  CreateRelationalDatabaseResponse
createRelationalDatabaseResponse pResponseStatus_ =
  CreateRelationalDatabaseResponse'
    { _crdrsOperations = Nothing,
      _crdrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
crdrsOperations :: Lens' CreateRelationalDatabaseResponse [Operation]
crdrsOperations = lens _crdrsOperations (\s a -> s {_crdrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
crdrsResponseStatus :: Lens' CreateRelationalDatabaseResponse Int
crdrsResponseStatus = lens _crdrsResponseStatus (\s a -> s {_crdrsResponseStatus = a})

instance NFData CreateRelationalDatabaseResponse
