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
    sdpRejects,
    sdpComplaints,
    sdpDeliveryAttempts,
    sdpBounces,
    sdpTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents sending statistics data. Each @SendDataPoint@ contains statistics for a 15-minute period of sending activity.
--
-- /See:/ 'mkSendDataPoint' smart constructor.
data SendDataPoint = SendDataPoint'
  { rejects ::
      Lude.Maybe Lude.Integer,
    complaints :: Lude.Maybe Lude.Integer,
    deliveryAttempts :: Lude.Maybe Lude.Integer,
    bounces :: Lude.Maybe Lude.Integer,
    timestamp :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SendDataPoint' with the minimum fields required to make a request.
--
-- * 'bounces' - Number of emails that have bounced.
-- * 'complaints' - Number of unwanted emails that were rejected by recipients.
-- * 'deliveryAttempts' - Number of emails that have been sent.
-- * 'rejects' - Number of emails rejected by Amazon SES.
-- * 'timestamp' - Time of the data point.
mkSendDataPoint ::
  SendDataPoint
mkSendDataPoint =
  SendDataPoint'
    { rejects = Lude.Nothing,
      complaints = Lude.Nothing,
      deliveryAttempts = Lude.Nothing,
      bounces = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | Number of emails rejected by Amazon SES.
--
-- /Note:/ Consider using 'rejects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpRejects :: Lens.Lens' SendDataPoint (Lude.Maybe Lude.Integer)
sdpRejects = Lens.lens (rejects :: SendDataPoint -> Lude.Maybe Lude.Integer) (\s a -> s {rejects = a} :: SendDataPoint)
{-# DEPRECATED sdpRejects "Use generic-lens or generic-optics with 'rejects' instead." #-}

-- | Number of unwanted emails that were rejected by recipients.
--
-- /Note:/ Consider using 'complaints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpComplaints :: Lens.Lens' SendDataPoint (Lude.Maybe Lude.Integer)
sdpComplaints = Lens.lens (complaints :: SendDataPoint -> Lude.Maybe Lude.Integer) (\s a -> s {complaints = a} :: SendDataPoint)
{-# DEPRECATED sdpComplaints "Use generic-lens or generic-optics with 'complaints' instead." #-}

-- | Number of emails that have been sent.
--
-- /Note:/ Consider using 'deliveryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpDeliveryAttempts :: Lens.Lens' SendDataPoint (Lude.Maybe Lude.Integer)
sdpDeliveryAttempts = Lens.lens (deliveryAttempts :: SendDataPoint -> Lude.Maybe Lude.Integer) (\s a -> s {deliveryAttempts = a} :: SendDataPoint)
{-# DEPRECATED sdpDeliveryAttempts "Use generic-lens or generic-optics with 'deliveryAttempts' instead." #-}

-- | Number of emails that have bounced.
--
-- /Note:/ Consider using 'bounces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpBounces :: Lens.Lens' SendDataPoint (Lude.Maybe Lude.Integer)
sdpBounces = Lens.lens (bounces :: SendDataPoint -> Lude.Maybe Lude.Integer) (\s a -> s {bounces = a} :: SendDataPoint)
{-# DEPRECATED sdpBounces "Use generic-lens or generic-optics with 'bounces' instead." #-}

-- | Time of the data point.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpTimestamp :: Lens.Lens' SendDataPoint (Lude.Maybe Lude.DateTime)
sdpTimestamp = Lens.lens (timestamp :: SendDataPoint -> Lude.Maybe Lude.DateTime) (\s a -> s {timestamp = a} :: SendDataPoint)
{-# DEPRECATED sdpTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromXML SendDataPoint where
  parseXML x =
    SendDataPoint'
      Lude.<$> (x Lude..@? "Rejects")
      Lude.<*> (x Lude..@? "Complaints")
      Lude.<*> (x Lude..@? "DeliveryAttempts")
      Lude.<*> (x Lude..@? "Bounces")
      Lude.<*> (x Lude..@? "Timestamp")
