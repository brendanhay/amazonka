{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SendDataPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SendDataPoint
  ( SendDataPoint (..),

    -- * Smart constructor
    mkSendDataPoint,

    -- * Lenses
    sdpBounces,
    sdpComplaints,
    sdpDeliveryAttempts,
    sdpRejects,
    sdpTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents sending statistics data. Each @SendDataPoint@ contains statistics for a 15-minute period of sending activity.
--
-- /See:/ 'mkSendDataPoint' smart constructor.
data SendDataPoint = SendDataPoint'
  { -- | Number of emails that have bounced.
    bounces :: Core.Maybe Core.Integer,
    -- | Number of unwanted emails that were rejected by recipients.
    complaints :: Core.Maybe Core.Integer,
    -- | Number of emails that have been sent.
    deliveryAttempts :: Core.Maybe Core.Integer,
    -- | Number of emails rejected by Amazon SES.
    rejects :: Core.Maybe Core.Integer,
    -- | Time of the data point.
    timestamp :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SendDataPoint' value with any optional fields omitted.
mkSendDataPoint ::
  SendDataPoint
mkSendDataPoint =
  SendDataPoint'
    { bounces = Core.Nothing,
      complaints = Core.Nothing,
      deliveryAttempts = Core.Nothing,
      rejects = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | Number of emails that have bounced.
--
-- /Note:/ Consider using 'bounces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpBounces :: Lens.Lens' SendDataPoint (Core.Maybe Core.Integer)
sdpBounces = Lens.field @"bounces"
{-# DEPRECATED sdpBounces "Use generic-lens or generic-optics with 'bounces' instead." #-}

-- | Number of unwanted emails that were rejected by recipients.
--
-- /Note:/ Consider using 'complaints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpComplaints :: Lens.Lens' SendDataPoint (Core.Maybe Core.Integer)
sdpComplaints = Lens.field @"complaints"
{-# DEPRECATED sdpComplaints "Use generic-lens or generic-optics with 'complaints' instead." #-}

-- | Number of emails that have been sent.
--
-- /Note:/ Consider using 'deliveryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpDeliveryAttempts :: Lens.Lens' SendDataPoint (Core.Maybe Core.Integer)
sdpDeliveryAttempts = Lens.field @"deliveryAttempts"
{-# DEPRECATED sdpDeliveryAttempts "Use generic-lens or generic-optics with 'deliveryAttempts' instead." #-}

-- | Number of emails rejected by Amazon SES.
--
-- /Note:/ Consider using 'rejects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpRejects :: Lens.Lens' SendDataPoint (Core.Maybe Core.Integer)
sdpRejects = Lens.field @"rejects"
{-# DEPRECATED sdpRejects "Use generic-lens or generic-optics with 'rejects' instead." #-}

-- | Time of the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpTimestamp :: Lens.Lens' SendDataPoint (Core.Maybe Core.UTCTime)
sdpTimestamp = Lens.field @"timestamp"
{-# DEPRECATED sdpTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromXML SendDataPoint where
  parseXML x =
    SendDataPoint'
      Core.<$> (x Core..@? "Bounces")
      Core.<*> (x Core..@? "Complaints")
      Core.<*> (x Core..@? "DeliveryAttempts")
      Core.<*> (x Core..@? "Rejects")
      Core.<*> (x Core..@? "Timestamp")
