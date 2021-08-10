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
-- Module      : Network.AWS.SESv2.Types.SendQuota
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.SendQuota where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains information about the per-day and per-second
-- sending limits for your Amazon SES account in the current AWS Region.
--
-- /See:/ 'newSendQuota' smart constructor.
data SendQuota = SendQuota'
  { -- | The maximum number of emails that you can send in the current AWS Region
    -- over a 24-hour period. This value is also called your /sending quota/.
    max24HourSend :: Prelude.Maybe Prelude.Double,
    -- | The number of emails sent from your Amazon SES account in the current
    -- AWS Region over the past 24 hours.
    sentLast24Hours :: Prelude.Maybe Prelude.Double,
    -- | The maximum number of emails that you can send per second in the current
    -- AWS Region. This value is also called your /maximum sending rate/ or
    -- your /maximum TPS (transactions per second) rate/.
    maxSendRate :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max24HourSend', 'sendQuota_max24HourSend' - The maximum number of emails that you can send in the current AWS Region
-- over a 24-hour period. This value is also called your /sending quota/.
--
-- 'sentLast24Hours', 'sendQuota_sentLast24Hours' - The number of emails sent from your Amazon SES account in the current
-- AWS Region over the past 24 hours.
--
-- 'maxSendRate', 'sendQuota_maxSendRate' - The maximum number of emails that you can send per second in the current
-- AWS Region. This value is also called your /maximum sending rate/ or
-- your /maximum TPS (transactions per second) rate/.
newSendQuota ::
  SendQuota
newSendQuota =
  SendQuota'
    { max24HourSend = Prelude.Nothing,
      sentLast24Hours = Prelude.Nothing,
      maxSendRate = Prelude.Nothing
    }

-- | The maximum number of emails that you can send in the current AWS Region
-- over a 24-hour period. This value is also called your /sending quota/.
sendQuota_max24HourSend :: Lens.Lens' SendQuota (Prelude.Maybe Prelude.Double)
sendQuota_max24HourSend = Lens.lens (\SendQuota' {max24HourSend} -> max24HourSend) (\s@SendQuota' {} a -> s {max24HourSend = a} :: SendQuota)

-- | The number of emails sent from your Amazon SES account in the current
-- AWS Region over the past 24 hours.
sendQuota_sentLast24Hours :: Lens.Lens' SendQuota (Prelude.Maybe Prelude.Double)
sendQuota_sentLast24Hours = Lens.lens (\SendQuota' {sentLast24Hours} -> sentLast24Hours) (\s@SendQuota' {} a -> s {sentLast24Hours = a} :: SendQuota)

-- | The maximum number of emails that you can send per second in the current
-- AWS Region. This value is also called your /maximum sending rate/ or
-- your /maximum TPS (transactions per second) rate/.
sendQuota_maxSendRate :: Lens.Lens' SendQuota (Prelude.Maybe Prelude.Double)
sendQuota_maxSendRate = Lens.lens (\SendQuota' {maxSendRate} -> maxSendRate) (\s@SendQuota' {} a -> s {maxSendRate = a} :: SendQuota)

instance Core.FromJSON SendQuota where
  parseJSON =
    Core.withObject
      "SendQuota"
      ( \x ->
          SendQuota'
            Prelude.<$> (x Core..:? "Max24HourSend")
            Prelude.<*> (x Core..:? "SentLast24Hours")
            Prelude.<*> (x Core..:? "MaxSendRate")
      )

instance Prelude.Hashable SendQuota

instance Prelude.NFData SendQuota
