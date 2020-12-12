{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Record
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Record
  ( Record (..),

    -- * Smart constructor
    mkRecord,

    -- * Lenses
    rSyncCount,
    rDeviceLastModifiedDate,
    rLastModifiedDate,
    rValue,
    rKey,
    rLastModifiedBy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The basic data structure of a dataset.
--
-- /See:/ 'mkRecord' smart constructor.
data Record = Record'
  { syncCount :: Lude.Maybe Lude.Integer,
    deviceLastModifiedDate :: Lude.Maybe Lude.Timestamp,
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    value :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- * 'deviceLastModifiedDate' - The last modified date of the client device.
-- * 'key' - The key for the record.
-- * 'lastModifiedBy' - The user/device that made the last change to this record.
-- * 'lastModifiedDate' - The date on which the record was last modified.
-- * 'syncCount' - The server sync count for this record.
-- * 'value' - The value for the record.
mkRecord ::
  Record
mkRecord =
  Record'
    { syncCount = Lude.Nothing,
      deviceLastModifiedDate = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      value = Lude.Nothing,
      key = Lude.Nothing,
      lastModifiedBy = Lude.Nothing
    }

-- | The server sync count for this record.
--
-- /Note:/ Consider using 'syncCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSyncCount :: Lens.Lens' Record (Lude.Maybe Lude.Integer)
rSyncCount = Lens.lens (syncCount :: Record -> Lude.Maybe Lude.Integer) (\s a -> s {syncCount = a} :: Record)
{-# DEPRECATED rSyncCount "Use generic-lens or generic-optics with 'syncCount' instead." #-}

-- | The last modified date of the client device.
--
-- /Note:/ Consider using 'deviceLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDeviceLastModifiedDate :: Lens.Lens' Record (Lude.Maybe Lude.Timestamp)
rDeviceLastModifiedDate = Lens.lens (deviceLastModifiedDate :: Record -> Lude.Maybe Lude.Timestamp) (\s a -> s {deviceLastModifiedDate = a} :: Record)
{-# DEPRECATED rDeviceLastModifiedDate "Use generic-lens or generic-optics with 'deviceLastModifiedDate' instead." #-}

-- | The date on which the record was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLastModifiedDate :: Lens.Lens' Record (Lude.Maybe Lude.Timestamp)
rLastModifiedDate = Lens.lens (lastModifiedDate :: Record -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: Record)
{-# DEPRECATED rLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The value for the record.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rValue :: Lens.Lens' Record (Lude.Maybe Lude.Text)
rValue = Lens.lens (value :: Record -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Record)
{-# DEPRECATED rValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The key for the record.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKey :: Lens.Lens' Record (Lude.Maybe Lude.Text)
rKey = Lens.lens (key :: Record -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: Record)
{-# DEPRECATED rKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The user/device that made the last change to this record.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLastModifiedBy :: Lens.Lens' Record (Lude.Maybe Lude.Text)
rLastModifiedBy = Lens.lens (lastModifiedBy :: Record -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: Record)
{-# DEPRECATED rLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

instance Lude.FromJSON Record where
  parseJSON =
    Lude.withObject
      "Record"
      ( \x ->
          Record'
            Lude.<$> (x Lude..:? "SyncCount")
            Lude.<*> (x Lude..:? "DeviceLastModifiedDate")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "LastModifiedBy")
      )
