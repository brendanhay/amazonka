{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationTime
  ( ReplicationTime (..),

    -- * Smart constructor
    mkReplicationTime,

    -- * Lenses
    rtStatus,
    rtTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ReplicationTimeStatus
import Network.AWS.S3.Types.ReplicationTimeValue

-- | A container specifying S3 Replication Time Control (S3 RTC) related information, including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
--
-- /See:/ 'mkReplicationTime' smart constructor.
data ReplicationTime = ReplicationTime'
  { -- | Specifies whether the replication time is enabled.
    status :: ReplicationTimeStatus,
    -- | A container specifying the time by which replication should be complete for all objects and operations on objects.
    time :: ReplicationTimeValue
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationTime' with the minimum fields required to make a request.
--
-- * 'status' - Specifies whether the replication time is enabled.
-- * 'time' - A container specifying the time by which replication should be complete for all objects and operations on objects.
mkReplicationTime ::
  -- | 'status'
  ReplicationTimeStatus ->
  -- | 'time'
  ReplicationTimeValue ->
  ReplicationTime
mkReplicationTime pStatus_ pTime_ =
  ReplicationTime' {status = pStatus_, time = pTime_}

-- | Specifies whether the replication time is enabled.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtStatus :: Lens.Lens' ReplicationTime ReplicationTimeStatus
rtStatus = Lens.lens (status :: ReplicationTime -> ReplicationTimeStatus) (\s a -> s {status = a} :: ReplicationTime)
{-# DEPRECATED rtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A container specifying the time by which replication should be complete for all objects and operations on objects.
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTime :: Lens.Lens' ReplicationTime ReplicationTimeValue
rtTime = Lens.lens (time :: ReplicationTime -> ReplicationTimeValue) (\s a -> s {time = a} :: ReplicationTime)
{-# DEPRECATED rtTime "Use generic-lens or generic-optics with 'time' instead." #-}

instance Lude.FromXML ReplicationTime where
  parseXML x =
    ReplicationTime'
      Lude.<$> (x Lude..@ "Status") Lude.<*> (x Lude..@ "Time")

instance Lude.ToXML ReplicationTime where
  toXML ReplicationTime' {..} =
    Lude.mconcat ["Status" Lude.@= status, "Time" Lude.@= time]
