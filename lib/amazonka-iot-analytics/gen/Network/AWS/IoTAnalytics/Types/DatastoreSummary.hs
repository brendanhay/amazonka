{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreSummary
  ( DatastoreSummary (..),

    -- * Smart constructor
    mkDatastoreSummary,

    -- * Lenses
    dsCreationTime,
    dsStatus,
    dsLastMessageArrivalTime,
    dsDatastoreName,
    dsLastUpdateTime,
    dsDatastoreStorage,
  )
where

import Network.AWS.IoTAnalytics.Types.DatastoreStatus
import Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of information about a data store.
--
-- /See:/ 'mkDatastoreSummary' smart constructor.
data DatastoreSummary = DatastoreSummary'
  { -- | When the data store was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of the data store.
    status :: Lude.Maybe DatastoreStatus,
    -- | The last time when a new message arrived in the data store.
    --
    -- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
    -- This feature only applies to messages that arrived in the data store after October 23, 2020.
    lastMessageArrivalTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the data store.
    datastoreName :: Lude.Maybe Lude.Text,
    -- | The last time the data store was updated.
    lastUpdateTime :: Lude.Maybe Lude.Timestamp,
    -- | Where data store data is stored.
    datastoreStorage :: Lude.Maybe DatastoreStorageSummary
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatastoreSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the data store was created.
-- * 'status' - The status of the data store.
-- * 'lastMessageArrivalTime' - The last time when a new message arrived in the data store.
--
-- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
-- * 'datastoreName' - The name of the data store.
-- * 'lastUpdateTime' - The last time the data store was updated.
-- * 'datastoreStorage' - Where data store data is stored.
mkDatastoreSummary ::
  DatastoreSummary
mkDatastoreSummary =
  DatastoreSummary'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      lastMessageArrivalTime = Lude.Nothing,
      datastoreName = Lude.Nothing,
      lastUpdateTime = Lude.Nothing,
      datastoreStorage = Lude.Nothing
    }

-- | When the data store was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreationTime :: Lens.Lens' DatastoreSummary (Lude.Maybe Lude.Timestamp)
dsCreationTime = Lens.lens (creationTime :: DatastoreSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DatastoreSummary)
{-# DEPRECATED dsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the data store.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStatus :: Lens.Lens' DatastoreSummary (Lude.Maybe DatastoreStatus)
dsStatus = Lens.lens (status :: DatastoreSummary -> Lude.Maybe DatastoreStatus) (\s a -> s {status = a} :: DatastoreSummary)
{-# DEPRECATED dsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The last time when a new message arrived in the data store.
--
-- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
--
-- /Note:/ Consider using 'lastMessageArrivalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLastMessageArrivalTime :: Lens.Lens' DatastoreSummary (Lude.Maybe Lude.Timestamp)
dsLastMessageArrivalTime = Lens.lens (lastMessageArrivalTime :: DatastoreSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastMessageArrivalTime = a} :: DatastoreSummary)
{-# DEPRECATED dsLastMessageArrivalTime "Use generic-lens or generic-optics with 'lastMessageArrivalTime' instead." #-}

-- | The name of the data store.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDatastoreName :: Lens.Lens' DatastoreSummary (Lude.Maybe Lude.Text)
dsDatastoreName = Lens.lens (datastoreName :: DatastoreSummary -> Lude.Maybe Lude.Text) (\s a -> s {datastoreName = a} :: DatastoreSummary)
{-# DEPRECATED dsDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

-- | The last time the data store was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLastUpdateTime :: Lens.Lens' DatastoreSummary (Lude.Maybe Lude.Timestamp)
dsLastUpdateTime = Lens.lens (lastUpdateTime :: DatastoreSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: DatastoreSummary)
{-# DEPRECATED dsLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | Where data store data is stored.
--
-- /Note:/ Consider using 'datastoreStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDatastoreStorage :: Lens.Lens' DatastoreSummary (Lude.Maybe DatastoreStorageSummary)
dsDatastoreStorage = Lens.lens (datastoreStorage :: DatastoreSummary -> Lude.Maybe DatastoreStorageSummary) (\s a -> s {datastoreStorage = a} :: DatastoreSummary)
{-# DEPRECATED dsDatastoreStorage "Use generic-lens or generic-optics with 'datastoreStorage' instead." #-}

instance Lude.FromJSON DatastoreSummary where
  parseJSON =
    Lude.withObject
      "DatastoreSummary"
      ( \x ->
          DatastoreSummary'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "lastMessageArrivalTime")
            Lude.<*> (x Lude..:? "datastoreName")
            Lude.<*> (x Lude..:? "lastUpdateTime")
            Lude.<*> (x Lude..:? "datastoreStorage")
      )
