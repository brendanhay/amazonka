{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Metrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Metrics
  ( Metrics (..),

    -- * Smart constructor
    mkMetrics,

    -- * Lenses
    mStatus,
    mEventThreshold,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.MetricsStatus
import Network.AWS.S3.Types.ReplicationTimeValue

-- | A container specifying replication metrics-related settings enabling replication metrics and events.
--
-- /See:/ 'mkMetrics' smart constructor.
data Metrics = Metrics'
  { -- | Specifies whether the replication metrics are enabled.
    status :: MetricsStatus,
    -- | A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event.
    eventThreshold :: Lude.Maybe ReplicationTimeValue
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Metrics' with the minimum fields required to make a request.
--
-- * 'status' - Specifies whether the replication metrics are enabled.
-- * 'eventThreshold' - A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event.
mkMetrics ::
  -- | 'status'
  MetricsStatus ->
  Metrics
mkMetrics pStatus_ =
  Metrics' {status = pStatus_, eventThreshold = Lude.Nothing}

-- | Specifies whether the replication metrics are enabled.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mStatus :: Lens.Lens' Metrics MetricsStatus
mStatus = Lens.lens (status :: Metrics -> MetricsStatus) (\s a -> s {status = a} :: Metrics)
{-# DEPRECATED mStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event.
--
-- /Note:/ Consider using 'eventThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEventThreshold :: Lens.Lens' Metrics (Lude.Maybe ReplicationTimeValue)
mEventThreshold = Lens.lens (eventThreshold :: Metrics -> Lude.Maybe ReplicationTimeValue) (\s a -> s {eventThreshold = a} :: Metrics)
{-# DEPRECATED mEventThreshold "Use generic-lens or generic-optics with 'eventThreshold' instead." #-}

instance Lude.FromXML Metrics where
  parseXML x =
    Metrics'
      Lude.<$> (x Lude..@ "Status") Lude.<*> (x Lude..@? "EventThreshold")

instance Lude.ToXML Metrics where
  toXML Metrics' {..} =
    Lude.mconcat
      ["Status" Lude.@= status, "EventThreshold" Lude.@= eventThreshold]
