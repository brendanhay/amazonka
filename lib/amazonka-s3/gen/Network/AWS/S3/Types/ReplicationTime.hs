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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ReplicationTimeStatus as Types
import qualified Network.AWS.S3.Types.ReplicationTimeValue as Types

-- | A container specifying S3 Replication Time Control (S3 RTC) related information, including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
--
-- /See:/ 'mkReplicationTime' smart constructor.
data ReplicationTime = ReplicationTime'
  { -- | Specifies whether the replication time is enabled.
    status :: Types.ReplicationTimeStatus,
    -- | A container specifying the time by which replication should be complete for all objects and operations on objects.
    time :: Types.ReplicationTimeValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationTime' value with any optional fields omitted.
mkReplicationTime ::
  -- | 'status'
  Types.ReplicationTimeStatus ->
  -- | 'time'
  Types.ReplicationTimeValue ->
  ReplicationTime
mkReplicationTime status time = ReplicationTime' {status, time}

-- | Specifies whether the replication time is enabled.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtStatus :: Lens.Lens' ReplicationTime Types.ReplicationTimeStatus
rtStatus = Lens.field @"status"
{-# DEPRECATED rtStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A container specifying the time by which replication should be complete for all objects and operations on objects.
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTime :: Lens.Lens' ReplicationTime Types.ReplicationTimeValue
rtTime = Lens.field @"time"
{-# DEPRECATED rtTime "Use generic-lens or generic-optics with 'time' instead." #-}

instance Core.ToXML ReplicationTime where
  toXML ReplicationTime {..} =
    Core.toXMLNode "Status" status Core.<> Core.toXMLNode "Time" time

instance Core.FromXML ReplicationTime where
  parseXML x =
    ReplicationTime'
      Core.<$> (x Core..@ "Status") Core.<*> (x Core..@ "Time")
