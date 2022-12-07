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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANStartFuotaTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANStartFuotaTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The LoRaWAN information used to start a FUOTA task.
--
-- /See:/ 'newLoRaWANStartFuotaTask' smart constructor.
data LoRaWANStartFuotaTask = LoRaWANStartFuotaTask'
  { startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANStartFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'loRaWANStartFuotaTask_startTime' - Undocumented member.
newLoRaWANStartFuotaTask ::
  LoRaWANStartFuotaTask
newLoRaWANStartFuotaTask =
  LoRaWANStartFuotaTask' {startTime = Prelude.Nothing}

-- | Undocumented member.
loRaWANStartFuotaTask_startTime :: Lens.Lens' LoRaWANStartFuotaTask (Prelude.Maybe Prelude.UTCTime)
loRaWANStartFuotaTask_startTime = Lens.lens (\LoRaWANStartFuotaTask' {startTime} -> startTime) (\s@LoRaWANStartFuotaTask' {} a -> s {startTime = a} :: LoRaWANStartFuotaTask) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable LoRaWANStartFuotaTask where
  hashWithSalt _salt LoRaWANStartFuotaTask' {..} =
    _salt `Prelude.hashWithSalt` startTime

instance Prelude.NFData LoRaWANStartFuotaTask where
  rnf LoRaWANStartFuotaTask' {..} =
    Prelude.rnf startTime

instance Data.ToJSON LoRaWANStartFuotaTask where
  toJSON LoRaWANStartFuotaTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [("StartTime" Data..=) Prelude.<$> startTime]
      )
