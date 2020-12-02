{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Datastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Datastore where

import Network.AWS.IoTAnalytics.Types.DatastoreStatus
import Network.AWS.IoTAnalytics.Types.DatastoreStorage
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a data store.
--
--
--
-- /See:/ 'datastore' smart constructor.
data Datastore = Datastore'
  { _datCreationTime :: !(Maybe POSIX),
    _datStatus :: !(Maybe DatastoreStatus),
    _datLastMessageArrivalTime :: !(Maybe POSIX),
    _datArn :: !(Maybe Text),
    _datStorage :: !(Maybe DatastoreStorage),
    _datRetentionPeriod :: !(Maybe RetentionPeriod),
    _datName :: !(Maybe Text),
    _datLastUpdateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Datastore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datCreationTime' - When the data store was created.
--
-- * 'datStatus' - The status of a data store:     * CREATING    * The data store is being created.     * ACTIVE    * The data store has been created and can be used.     * DELETING    * The data store is being deleted.
--
-- * 'datLastMessageArrivalTime' - The last time when a new message arrived in the data store. AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation. This feature only applies to messages that arrived in the data store after October 23, 2020.
--
-- * 'datArn' - The ARN of the data store.
--
-- * 'datStorage' - Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
--
-- * 'datRetentionPeriod' - How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- * 'datName' - The name of the data store.
--
-- * 'datLastUpdateTime' - The last time the data store was updated.
datastore ::
  Datastore
datastore =
  Datastore'
    { _datCreationTime = Nothing,
      _datStatus = Nothing,
      _datLastMessageArrivalTime = Nothing,
      _datArn = Nothing,
      _datStorage = Nothing,
      _datRetentionPeriod = Nothing,
      _datName = Nothing,
      _datLastUpdateTime = Nothing
    }

-- | When the data store was created.
datCreationTime :: Lens' Datastore (Maybe UTCTime)
datCreationTime = lens _datCreationTime (\s a -> s {_datCreationTime = a}) . mapping _Time

-- | The status of a data store:     * CREATING    * The data store is being created.     * ACTIVE    * The data store has been created and can be used.     * DELETING    * The data store is being deleted.
datStatus :: Lens' Datastore (Maybe DatastoreStatus)
datStatus = lens _datStatus (\s a -> s {_datStatus = a})

-- | The last time when a new message arrived in the data store. AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation. This feature only applies to messages that arrived in the data store after October 23, 2020.
datLastMessageArrivalTime :: Lens' Datastore (Maybe UTCTime)
datLastMessageArrivalTime = lens _datLastMessageArrivalTime (\s a -> s {_datLastMessageArrivalTime = a}) . mapping _Time

-- | The ARN of the data store.
datArn :: Lens' Datastore (Maybe Text)
datArn = lens _datArn (\s a -> s {_datArn = a})

-- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
datStorage :: Lens' Datastore (Maybe DatastoreStorage)
datStorage = lens _datStorage (\s a -> s {_datStorage = a})

-- | How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
datRetentionPeriod :: Lens' Datastore (Maybe RetentionPeriod)
datRetentionPeriod = lens _datRetentionPeriod (\s a -> s {_datRetentionPeriod = a})

-- | The name of the data store.
datName :: Lens' Datastore (Maybe Text)
datName = lens _datName (\s a -> s {_datName = a})

-- | The last time the data store was updated.
datLastUpdateTime :: Lens' Datastore (Maybe UTCTime)
datLastUpdateTime = lens _datLastUpdateTime (\s a -> s {_datLastUpdateTime = a}) . mapping _Time

instance FromJSON Datastore where
  parseJSON =
    withObject
      "Datastore"
      ( \x ->
          Datastore'
            <$> (x .:? "creationTime")
            <*> (x .:? "status")
            <*> (x .:? "lastMessageArrivalTime")
            <*> (x .:? "arn")
            <*> (x .:? "storage")
            <*> (x .:? "retentionPeriod")
            <*> (x .:? "name")
            <*> (x .:? "lastUpdateTime")
      )

instance Hashable Datastore

instance NFData Datastore
