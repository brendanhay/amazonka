{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SendDataPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.SendDataPoint
  ( SendDataPoint (..)
  -- * Smart constructor
  , mkSendDataPoint
  -- * Lenses
  , sdpBounces
  , sdpComplaints
  , sdpDeliveryAttempts
  , sdpRejects
  , sdpTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents sending statistics data. Each @SendDataPoint@ contains statistics for a 15-minute period of sending activity. 
--
-- /See:/ 'mkSendDataPoint' smart constructor.
data SendDataPoint = SendDataPoint'
  { bounces :: Core.Maybe Core.Integer
    -- ^ Number of emails that have bounced.
  , complaints :: Core.Maybe Core.Integer
    -- ^ Number of unwanted emails that were rejected by recipients.
  , deliveryAttempts :: Core.Maybe Core.Integer
    -- ^ Number of emails that have been sent.
  , rejects :: Core.Maybe Core.Integer
    -- ^ Number of emails rejected by Amazon SES.
  , timestamp :: Core.Maybe Core.UTCTime
    -- ^ Time of the data point.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SendDataPoint' value with any optional fields omitted.
mkSendDataPoint
    :: SendDataPoint
mkSendDataPoint
  = SendDataPoint'{bounces = Core.Nothing, complaints = Core.Nothing,
                   deliveryAttempts = Core.Nothing, rejects = Core.Nothing,
                   timestamp = Core.Nothing}

-- | Number of emails that have bounced.
--
-- /Note:/ Consider using 'bounces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpBounces :: Lens.Lens' SendDataPoint (Core.Maybe Core.Integer)
sdpBounces = Lens.field @"bounces"
{-# INLINEABLE sdpBounces #-}
{-# DEPRECATED bounces "Use generic-lens or generic-optics with 'bounces' instead"  #-}

-- | Number of unwanted emails that were rejected by recipients.
--
-- /Note:/ Consider using 'complaints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpComplaints :: Lens.Lens' SendDataPoint (Core.Maybe Core.Integer)
sdpComplaints = Lens.field @"complaints"
{-# INLINEABLE sdpComplaints #-}
{-# DEPRECATED complaints "Use generic-lens or generic-optics with 'complaints' instead"  #-}

-- | Number of emails that have been sent.
--
-- /Note:/ Consider using 'deliveryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpDeliveryAttempts :: Lens.Lens' SendDataPoint (Core.Maybe Core.Integer)
sdpDeliveryAttempts = Lens.field @"deliveryAttempts"
{-# INLINEABLE sdpDeliveryAttempts #-}
{-# DEPRECATED deliveryAttempts "Use generic-lens or generic-optics with 'deliveryAttempts' instead"  #-}

-- | Number of emails rejected by Amazon SES.
--
-- /Note:/ Consider using 'rejects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpRejects :: Lens.Lens' SendDataPoint (Core.Maybe Core.Integer)
sdpRejects = Lens.field @"rejects"
{-# INLINEABLE sdpRejects #-}
{-# DEPRECATED rejects "Use generic-lens or generic-optics with 'rejects' instead"  #-}

-- | Time of the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpTimestamp :: Lens.Lens' SendDataPoint (Core.Maybe Core.UTCTime)
sdpTimestamp = Lens.field @"timestamp"
{-# INLINEABLE sdpTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromXML SendDataPoint where
        parseXML x
          = SendDataPoint' Core.<$>
              (x Core..@? "Bounces") Core.<*> x Core..@? "Complaints" Core.<*>
                x Core..@? "DeliveryAttempts"
                Core.<*> x Core..@? "Rejects"
                Core.<*> x Core..@? "Timestamp"
