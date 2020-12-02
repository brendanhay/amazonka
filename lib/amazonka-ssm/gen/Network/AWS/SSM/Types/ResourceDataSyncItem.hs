{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.LastResourceDataSyncStatus
import Network.AWS.SSM.Types.ResourceDataSyncS3Destination
import Network.AWS.SSM.Types.ResourceDataSyncSourceWithState

-- | Information about a Resource Data Sync configuration, including its current status and last successful sync.
--
--
--
-- /See:/ 'resourceDataSyncItem' smart constructor.
data ResourceDataSyncItem = ResourceDataSyncItem'
  { _rdsiSyncType ::
      !(Maybe Text),
    _rdsiSyncSource ::
      !(Maybe ResourceDataSyncSourceWithState),
    _rdsiLastSyncStatusMessage :: !(Maybe Text),
    _rdsiSyncCreatedTime :: !(Maybe POSIX),
    _rdsiLastSyncTime :: !(Maybe POSIX),
    _rdsiSyncName :: !(Maybe Text),
    _rdsiLastStatus ::
      !(Maybe LastResourceDataSyncStatus),
    _rdsiSyncLastModifiedTime :: !(Maybe POSIX),
    _rdsiS3Destination ::
      !(Maybe ResourceDataSyncS3Destination),
    _rdsiLastSuccessfulSyncTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDataSyncItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdsiSyncType' - The type of resource data sync. If @SyncType@ is @SyncToDestination@ , then the resource data sync synchronizes data to an S3 bucket. If the @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes data from AWS Organizations or from multiple AWS Regions.
--
-- * 'rdsiSyncSource' - Information about the source where the data was synchronized.
--
-- * 'rdsiLastSyncStatusMessage' - The status message details reported by the last sync.
--
-- * 'rdsiSyncCreatedTime' - The date and time the configuration was created (UTC).
--
-- * 'rdsiLastSyncTime' - The last time the configuration attempted to sync (UTC).
--
-- * 'rdsiSyncName' - The name of the Resource Data Sync.
--
-- * 'rdsiLastStatus' - The status reported by the last sync.
--
-- * 'rdsiSyncLastModifiedTime' - The date and time the resource data sync was changed.
--
-- * 'rdsiS3Destination' - Configuration information for the target S3 bucket.
--
-- * 'rdsiLastSuccessfulSyncTime' - The last time the sync operations returned a status of @SUCCESSFUL@ (UTC).
resourceDataSyncItem ::
  ResourceDataSyncItem
resourceDataSyncItem =
  ResourceDataSyncItem'
    { _rdsiSyncType = Nothing,
      _rdsiSyncSource = Nothing,
      _rdsiLastSyncStatusMessage = Nothing,
      _rdsiSyncCreatedTime = Nothing,
      _rdsiLastSyncTime = Nothing,
      _rdsiSyncName = Nothing,
      _rdsiLastStatus = Nothing,
      _rdsiSyncLastModifiedTime = Nothing,
      _rdsiS3Destination = Nothing,
      _rdsiLastSuccessfulSyncTime = Nothing
    }

-- | The type of resource data sync. If @SyncType@ is @SyncToDestination@ , then the resource data sync synchronizes data to an S3 bucket. If the @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes data from AWS Organizations or from multiple AWS Regions.
rdsiSyncType :: Lens' ResourceDataSyncItem (Maybe Text)
rdsiSyncType = lens _rdsiSyncType (\s a -> s {_rdsiSyncType = a})

-- | Information about the source where the data was synchronized.
rdsiSyncSource :: Lens' ResourceDataSyncItem (Maybe ResourceDataSyncSourceWithState)
rdsiSyncSource = lens _rdsiSyncSource (\s a -> s {_rdsiSyncSource = a})

-- | The status message details reported by the last sync.
rdsiLastSyncStatusMessage :: Lens' ResourceDataSyncItem (Maybe Text)
rdsiLastSyncStatusMessage = lens _rdsiLastSyncStatusMessage (\s a -> s {_rdsiLastSyncStatusMessage = a})

-- | The date and time the configuration was created (UTC).
rdsiSyncCreatedTime :: Lens' ResourceDataSyncItem (Maybe UTCTime)
rdsiSyncCreatedTime = lens _rdsiSyncCreatedTime (\s a -> s {_rdsiSyncCreatedTime = a}) . mapping _Time

-- | The last time the configuration attempted to sync (UTC).
rdsiLastSyncTime :: Lens' ResourceDataSyncItem (Maybe UTCTime)
rdsiLastSyncTime = lens _rdsiLastSyncTime (\s a -> s {_rdsiLastSyncTime = a}) . mapping _Time

-- | The name of the Resource Data Sync.
rdsiSyncName :: Lens' ResourceDataSyncItem (Maybe Text)
rdsiSyncName = lens _rdsiSyncName (\s a -> s {_rdsiSyncName = a})

-- | The status reported by the last sync.
rdsiLastStatus :: Lens' ResourceDataSyncItem (Maybe LastResourceDataSyncStatus)
rdsiLastStatus = lens _rdsiLastStatus (\s a -> s {_rdsiLastStatus = a})

-- | The date and time the resource data sync was changed.
rdsiSyncLastModifiedTime :: Lens' ResourceDataSyncItem (Maybe UTCTime)
rdsiSyncLastModifiedTime = lens _rdsiSyncLastModifiedTime (\s a -> s {_rdsiSyncLastModifiedTime = a}) . mapping _Time

-- | Configuration information for the target S3 bucket.
rdsiS3Destination :: Lens' ResourceDataSyncItem (Maybe ResourceDataSyncS3Destination)
rdsiS3Destination = lens _rdsiS3Destination (\s a -> s {_rdsiS3Destination = a})

-- | The last time the sync operations returned a status of @SUCCESSFUL@ (UTC).
rdsiLastSuccessfulSyncTime :: Lens' ResourceDataSyncItem (Maybe UTCTime)
rdsiLastSuccessfulSyncTime = lens _rdsiLastSuccessfulSyncTime (\s a -> s {_rdsiLastSuccessfulSyncTime = a}) . mapping _Time

instance FromJSON ResourceDataSyncItem where
  parseJSON =
    withObject
      "ResourceDataSyncItem"
      ( \x ->
          ResourceDataSyncItem'
            <$> (x .:? "SyncType")
            <*> (x .:? "SyncSource")
            <*> (x .:? "LastSyncStatusMessage")
            <*> (x .:? "SyncCreatedTime")
            <*> (x .:? "LastSyncTime")
            <*> (x .:? "SyncName")
            <*> (x .:? "LastStatus")
            <*> (x .:? "SyncLastModifiedTime")
            <*> (x .:? "S3Destination")
            <*> (x .:? "LastSuccessfulSyncTime")
      )

instance Hashable ResourceDataSyncItem

instance NFData ResourceDataSyncItem
