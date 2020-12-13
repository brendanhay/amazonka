{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Datastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Datastore
  ( Datastore (..),

    -- * Smart constructor
    mkDatastore,

    -- * Lenses
    dfCreationTime,
    dfStatus,
    dfLastMessageArrivalTime,
    dfArn,
    dfStorage,
    dfRetentionPeriod,
    dfName,
    dfLastUpdateTime,
  )
where

import Network.AWS.IoTAnalytics.Types.DatastoreStatus
import Network.AWS.IoTAnalytics.Types.DatastoreStorage
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a data store.
--
-- /See:/ 'mkDatastore' smart constructor.
data Datastore = Datastore'
  { -- | When the data store was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of a data store:
    --
    --
    --     * CREATING
    --
    --     * The data store is being created.
    --
    --
    --     * ACTIVE
    --
    --     * The data store has been created and can be used.
    --
    --
    --     * DELETING
    --
    --     * The data store is being deleted.
    status :: Lude.Maybe DatastoreStatus,
    -- | The last time when a new message arrived in the data store.
    --
    -- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
    -- This feature only applies to messages that arrived in the data store after October 23, 2020.
    lastMessageArrivalTime :: Lude.Maybe Lude.Timestamp,
    -- | The ARN of the data store.
    arn :: Lude.Maybe Lude.Text,
    -- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
    storage :: Lude.Maybe DatastoreStorage,
    -- | How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    -- | The name of the data store.
    name :: Lude.Maybe Lude.Text,
    -- | The last time the data store was updated.
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Datastore' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the data store was created.
-- * 'status' - The status of a data store:
--
--
--     * CREATING
--
--     * The data store is being created.
--
--
--     * ACTIVE
--
--     * The data store has been created and can be used.
--
--
--     * DELETING
--
--     * The data store is being deleted.
--
--
-- * 'lastMessageArrivalTime' - The last time when a new message arrived in the data store.
--
-- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
-- * 'arn' - The ARN of the data store.
-- * 'storage' - Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
-- * 'retentionPeriod' - How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
-- * 'name' - The name of the data store.
-- * 'lastUpdateTime' - The last time the data store was updated.
mkDatastore ::
  Datastore
mkDatastore =
  Datastore'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      lastMessageArrivalTime = Lude.Nothing,
      arn = Lude.Nothing,
      storage = Lude.Nothing,
      retentionPeriod = Lude.Nothing,
      name = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

-- | When the data store was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfCreationTime :: Lens.Lens' Datastore (Lude.Maybe Lude.Timestamp)
dfCreationTime = Lens.lens (creationTime :: Datastore -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Datastore)
{-# DEPRECATED dfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of a data store:
--
--
--     * CREATING
--
--     * The data store is being created.
--
--
--     * ACTIVE
--
--     * The data store has been created and can be used.
--
--
--     * DELETING
--
--     * The data store is being deleted.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfStatus :: Lens.Lens' Datastore (Lude.Maybe DatastoreStatus)
dfStatus = Lens.lens (status :: Datastore -> Lude.Maybe DatastoreStatus) (\s a -> s {status = a} :: Datastore)
{-# DEPRECATED dfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The last time when a new message arrived in the data store.
--
-- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
--
-- /Note:/ Consider using 'lastMessageArrivalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfLastMessageArrivalTime :: Lens.Lens' Datastore (Lude.Maybe Lude.Timestamp)
dfLastMessageArrivalTime = Lens.lens (lastMessageArrivalTime :: Datastore -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastMessageArrivalTime = a} :: Datastore)
{-# DEPRECATED dfLastMessageArrivalTime "Use generic-lens or generic-optics with 'lastMessageArrivalTime' instead." #-}

-- | The ARN of the data store.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfArn :: Lens.Lens' Datastore (Lude.Maybe Lude.Text)
dfArn = Lens.lens (arn :: Datastore -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Datastore)
{-# DEPRECATED dfArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfStorage :: Lens.Lens' Datastore (Lude.Maybe DatastoreStorage)
dfStorage = Lens.lens (storage :: Datastore -> Lude.Maybe DatastoreStorage) (\s a -> s {storage = a} :: Datastore)
{-# DEPRECATED dfStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfRetentionPeriod :: Lens.Lens' Datastore (Lude.Maybe RetentionPeriod)
dfRetentionPeriod = Lens.lens (retentionPeriod :: Datastore -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: Datastore)
{-# DEPRECATED dfRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The name of the data store.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' Datastore (Lude.Maybe Lude.Text)
dfName = Lens.lens (name :: Datastore -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Datastore)
{-# DEPRECATED dfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The last time the data store was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfLastUpdateTime :: Lens.Lens' Datastore (Lude.Maybe Lude.Timestamp)
dfLastUpdateTime = Lens.lens (lastUpdateTime :: Datastore -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: Datastore)
{-# DEPRECATED dfLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON Datastore where
  parseJSON =
    Lude.withObject
      "Datastore"
      ( \x ->
          Datastore'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "lastMessageArrivalTime")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "storage")
            Lude.<*> (x Lude..:? "retentionPeriod")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "lastUpdateTime")
      )
