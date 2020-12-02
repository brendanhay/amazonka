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
-- Module      : Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database from an existing database snapshot in Amazon Lightsail.
--
--
-- You can create a new database from a snapshot in if something goes wrong with your original database, or to change it to a different plan, such as a high availability or standard plan.
--
-- The @create relational database from snapshot@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by relationalDatabaseSnapshotName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot
  ( -- * Creating a Request
    createRelationalDatabaseFromSnapshot,
    CreateRelationalDatabaseFromSnapshot,

    -- * Request Lenses
    crdfsSourceRelationalDatabaseName,
    crdfsRelationalDatabaseBundleId,
    crdfsPubliclyAccessible,
    crdfsUseLatestRestorableTime,
    crdfsRestoreTime,
    crdfsAvailabilityZone,
    crdfsRelationalDatabaseSnapshotName,
    crdfsTags,
    crdfsRelationalDatabaseName,

    -- * Destructuring the Response
    createRelationalDatabaseFromSnapshotResponse,
    CreateRelationalDatabaseFromSnapshotResponse,

    -- * Response Lenses
    crdfsrsOperations,
    crdfsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRelationalDatabaseFromSnapshot' smart constructor.
data CreateRelationalDatabaseFromSnapshot = CreateRelationalDatabaseFromSnapshot'
  { _crdfsSourceRelationalDatabaseName ::
      !(Maybe Text),
    _crdfsRelationalDatabaseBundleId ::
      !(Maybe Text),
    _crdfsPubliclyAccessible ::
      !(Maybe Bool),
    _crdfsUseLatestRestorableTime ::
      !(Maybe Bool),
    _crdfsRestoreTime ::
      !(Maybe POSIX),
    _crdfsAvailabilityZone ::
      !(Maybe Text),
    _crdfsRelationalDatabaseSnapshotName ::
      !(Maybe Text),
    _crdfsTags ::
      !(Maybe [Tag]),
    _crdfsRelationalDatabaseName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRelationalDatabaseFromSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdfsSourceRelationalDatabaseName' - The name of the source database.
--
-- * 'crdfsRelationalDatabaseBundleId' - The bundle ID for your new database. A bundle describes the performance specifications for your database. You can get a list of database bundle IDs by using the @get relational database bundles@ operation. When creating a new database from a snapshot, you cannot choose a bundle that is smaller than the bundle of the source database.
--
-- * 'crdfsPubliclyAccessible' - Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
--
-- * 'crdfsUseLatestRestorableTime' - Specifies whether your database is restored from the latest backup time. A value of @true@ restores from the latest backup time.  Default: @false@  Constraints: Cannot be specified if the @restore time@ parameter is provided.
--
-- * 'crdfsRestoreTime' - The date and time to restore your database from. Constraints:     * Must be before the latest restorable time for the database.     * Cannot be specified if the @use latest restorable time@ parameter is @true@ .     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use a restore time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the restore time.
--
-- * 'crdfsAvailabilityZone' - The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format. You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
--
-- * 'crdfsRelationalDatabaseSnapshotName' - The name of the database snapshot from which to create your new database.
--
-- * 'crdfsTags' - The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
--
-- * 'crdfsRelationalDatabaseName' - The name to use for your new database. Constraints:     * Must contain from 2 to 255 alphanumeric characters, or hyphens.     * The first and last character must be a letter or number.
createRelationalDatabaseFromSnapshot ::
  -- | 'crdfsRelationalDatabaseName'
  Text ->
  CreateRelationalDatabaseFromSnapshot
createRelationalDatabaseFromSnapshot pRelationalDatabaseName_ =
  CreateRelationalDatabaseFromSnapshot'
    { _crdfsSourceRelationalDatabaseName =
        Nothing,
      _crdfsRelationalDatabaseBundleId = Nothing,
      _crdfsPubliclyAccessible = Nothing,
      _crdfsUseLatestRestorableTime = Nothing,
      _crdfsRestoreTime = Nothing,
      _crdfsAvailabilityZone = Nothing,
      _crdfsRelationalDatabaseSnapshotName = Nothing,
      _crdfsTags = Nothing,
      _crdfsRelationalDatabaseName = pRelationalDatabaseName_
    }

-- | The name of the source database.
crdfsSourceRelationalDatabaseName :: Lens' CreateRelationalDatabaseFromSnapshot (Maybe Text)
crdfsSourceRelationalDatabaseName = lens _crdfsSourceRelationalDatabaseName (\s a -> s {_crdfsSourceRelationalDatabaseName = a})

-- | The bundle ID for your new database. A bundle describes the performance specifications for your database. You can get a list of database bundle IDs by using the @get relational database bundles@ operation. When creating a new database from a snapshot, you cannot choose a bundle that is smaller than the bundle of the source database.
crdfsRelationalDatabaseBundleId :: Lens' CreateRelationalDatabaseFromSnapshot (Maybe Text)
crdfsRelationalDatabaseBundleId = lens _crdfsRelationalDatabaseBundleId (\s a -> s {_crdfsRelationalDatabaseBundleId = a})

-- | Specifies the accessibility options for your new database. A value of @true@ specifies a database that is available to resources outside of your Lightsail account. A value of @false@ specifies a database that is available only to your Lightsail resources in the same region as your database.
crdfsPubliclyAccessible :: Lens' CreateRelationalDatabaseFromSnapshot (Maybe Bool)
crdfsPubliclyAccessible = lens _crdfsPubliclyAccessible (\s a -> s {_crdfsPubliclyAccessible = a})

-- | Specifies whether your database is restored from the latest backup time. A value of @true@ restores from the latest backup time.  Default: @false@  Constraints: Cannot be specified if the @restore time@ parameter is provided.
crdfsUseLatestRestorableTime :: Lens' CreateRelationalDatabaseFromSnapshot (Maybe Bool)
crdfsUseLatestRestorableTime = lens _crdfsUseLatestRestorableTime (\s a -> s {_crdfsUseLatestRestorableTime = a})

-- | The date and time to restore your database from. Constraints:     * Must be before the latest restorable time for the database.     * Cannot be specified if the @use latest restorable time@ parameter is @true@ .     * Specified in Coordinated Universal Time (UTC).     * Specified in the Unix time format. For example, if you wish to use a restore time of October 1, 2018, at 8 PM UTC, then you input @1538424000@ as the restore time.
crdfsRestoreTime :: Lens' CreateRelationalDatabaseFromSnapshot (Maybe UTCTime)
crdfsRestoreTime = lens _crdfsRestoreTime (\s a -> s {_crdfsRestoreTime = a}) . mapping _Time

-- | The Availability Zone in which to create your new database. Use the @us-east-2a@ case-sensitive format. You can get a list of Availability Zones by using the @get regions@ operation. Be sure to add the @include relational database Availability Zones@ parameter to your request.
crdfsAvailabilityZone :: Lens' CreateRelationalDatabaseFromSnapshot (Maybe Text)
crdfsAvailabilityZone = lens _crdfsAvailabilityZone (\s a -> s {_crdfsAvailabilityZone = a})

-- | The name of the database snapshot from which to create your new database.
crdfsRelationalDatabaseSnapshotName :: Lens' CreateRelationalDatabaseFromSnapshot (Maybe Text)
crdfsRelationalDatabaseSnapshotName = lens _crdfsRelationalDatabaseSnapshotName (\s a -> s {_crdfsRelationalDatabaseSnapshotName = a})

-- | The tag keys and optional values to add to the resource during create. Use the @TagResource@ action to tag a resource after it's created.
crdfsTags :: Lens' CreateRelationalDatabaseFromSnapshot [Tag]
crdfsTags = lens _crdfsTags (\s a -> s {_crdfsTags = a}) . _Default . _Coerce

-- | The name to use for your new database. Constraints:     * Must contain from 2 to 255 alphanumeric characters, or hyphens.     * The first and last character must be a letter or number.
crdfsRelationalDatabaseName :: Lens' CreateRelationalDatabaseFromSnapshot Text
crdfsRelationalDatabaseName = lens _crdfsRelationalDatabaseName (\s a -> s {_crdfsRelationalDatabaseName = a})

instance AWSRequest CreateRelationalDatabaseFromSnapshot where
  type
    Rs CreateRelationalDatabaseFromSnapshot =
      CreateRelationalDatabaseFromSnapshotResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseFromSnapshotResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable CreateRelationalDatabaseFromSnapshot

instance NFData CreateRelationalDatabaseFromSnapshot

instance ToHeaders CreateRelationalDatabaseFromSnapshot where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Lightsail_20161128.CreateRelationalDatabaseFromSnapshot" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateRelationalDatabaseFromSnapshot where
  toJSON CreateRelationalDatabaseFromSnapshot' {..} =
    object
      ( catMaybes
          [ ("sourceRelationalDatabaseName" .=)
              <$> _crdfsSourceRelationalDatabaseName,
            ("relationalDatabaseBundleId" .=)
              <$> _crdfsRelationalDatabaseBundleId,
            ("publiclyAccessible" .=) <$> _crdfsPubliclyAccessible,
            ("useLatestRestorableTime" .=) <$> _crdfsUseLatestRestorableTime,
            ("restoreTime" .=) <$> _crdfsRestoreTime,
            ("availabilityZone" .=) <$> _crdfsAvailabilityZone,
            ("relationalDatabaseSnapshotName" .=)
              <$> _crdfsRelationalDatabaseSnapshotName,
            ("tags" .=) <$> _crdfsTags,
            Just ("relationalDatabaseName" .= _crdfsRelationalDatabaseName)
          ]
      )

instance ToPath CreateRelationalDatabaseFromSnapshot where
  toPath = const "/"

instance ToQuery CreateRelationalDatabaseFromSnapshot where
  toQuery = const mempty

-- | /See:/ 'createRelationalDatabaseFromSnapshotResponse' smart constructor.
data CreateRelationalDatabaseFromSnapshotResponse = CreateRelationalDatabaseFromSnapshotResponse'
  { _crdfsrsOperations ::
      !( Maybe
           [Operation]
       ),
    _crdfsrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CreateRelationalDatabaseFromSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdfsrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'crdfsrsResponseStatus' - -- | The response status code.
createRelationalDatabaseFromSnapshotResponse ::
  -- | 'crdfsrsResponseStatus'
  Int ->
  CreateRelationalDatabaseFromSnapshotResponse
createRelationalDatabaseFromSnapshotResponse pResponseStatus_ =
  CreateRelationalDatabaseFromSnapshotResponse'
    { _crdfsrsOperations =
        Nothing,
      _crdfsrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
crdfsrsOperations :: Lens' CreateRelationalDatabaseFromSnapshotResponse [Operation]
crdfsrsOperations = lens _crdfsrsOperations (\s a -> s {_crdfsrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
crdfsrsResponseStatus :: Lens' CreateRelationalDatabaseFromSnapshotResponse Int
crdfsrsResponseStatus = lens _crdfsrsResponseStatus (\s a -> s {_crdfsrsResponseStatus = a})

instance NFData CreateRelationalDatabaseFromSnapshotResponse
