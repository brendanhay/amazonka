{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SendDataPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SendDataPoint where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents sending statistics data. Each @SendDataPoint@ contains
-- statistics for a 15-minute period of sending activity.
--
-- /See:/ 'newSendDataPoint' smart constructor.
data SendDataPoint = SendDataPoint'
  { -- | Number of emails that have bounced.
    bounces :: Prelude.Maybe Prelude.Integer,
    -- | Number of unwanted emails that were rejected by recipients.
    complaints :: Prelude.Maybe Prelude.Integer,
    -- | Number of emails rejected by Amazon SES.
    rejects :: Prelude.Maybe Prelude.Integer,
    -- | Time of the data point.
    timestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | Number of emails that have been sent.
    deliveryAttempts :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SendDataPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bounces', 'sendDataPoint_bounces' - Number of emails that have bounced.
--
-- 'complaints', 'sendDataPoint_complaints' - Number of unwanted emails that were rejected by recipients.
--
-- 'rejects', 'sendDataPoint_rejects' - Number of emails rejected by Amazon SES.
--
-- 'timestamp', 'sendDataPoint_timestamp' - Time of the data point.
--
-- 'deliveryAttempts', 'sendDataPoint_deliveryAttempts' - Number of emails that have been sent.
newSendDataPoint ::
  SendDataPoint
newSendDataPoint =
  SendDataPoint'
    { bounces = Prelude.Nothing,
      complaints = Prelude.Nothing,
      rejects = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      deliveryAttempts = Prelude.Nothing
    }

-- | Number of emails that have bounced.
sendDataPoint_bounces :: Lens.Lens' SendDataPoint (Prelude.Maybe Prelude.Integer)
sendDataPoint_bounces = Lens.lens (\SendDataPoint' {bounces} -> bounces) (\s@SendDataPoint' {} a -> s {bounces = a} :: SendDataPoint)

-- | Number of unwanted emails that were rejected by recipients.
sendDataPoint_complaints :: Lens.Lens' SendDataPoint (Prelude.Maybe Prelude.Integer)
sendDataPoint_complaints = Lens.lens (\SendDataPoint' {complaints} -> complaints) (\s@SendDataPoint' {} a -> s {complaints = a} :: SendDataPoint)

-- | Number of emails rejected by Amazon SES.
sendDataPoint_rejects :: Lens.Lens' SendDataPoint (Prelude.Maybe Prelude.Integer)
sendDataPoint_rejects = Lens.lens (\SendDataPoint' {rejects} -> rejects) (\s@SendDataPoint' {} a -> s {rejects = a} :: SendDataPoint)

-- | Time of the data point.
sendDataPoint_timestamp :: Lens.Lens' SendDataPoint (Prelude.Maybe Prelude.UTCTime)
sendDataPoint_timestamp = Lens.lens (\SendDataPoint' {timestamp} -> timestamp) (\s@SendDataPoint' {} a -> s {timestamp = a} :: SendDataPoint) Prelude.. Lens.mapping Prelude._Time

-- | Number of emails that have been sent.
sendDataPoint_deliveryAttempts :: Lens.Lens' SendDataPoint (Prelude.Maybe Prelude.Integer)
sendDataPoint_deliveryAttempts = Lens.lens (\SendDataPoint' {deliveryAttempts} -> deliveryAttempts) (\s@SendDataPoint' {} a -> s {deliveryAttempts = a} :: SendDataPoint)

instance Prelude.FromXML SendDataPoint where
  parseXML x =
    SendDataPoint'
      Prelude.<$> (x Prelude..@? "Bounces")
      Prelude.<*> (x Prelude..@? "Complaints")
      Prelude.<*> (x Prelude..@? "Rejects")
      Prelude.<*> (x Prelude..@? "Timestamp")
      Prelude.<*> (x Prelude..@? "DeliveryAttempts")

instance Prelude.Hashable SendDataPoint

instance Prelude.NFData SendDataPoint
