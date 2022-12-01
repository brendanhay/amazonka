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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANFuotaTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANFuotaTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.SupportedRfRegion
import qualified Amazonka.Prelude as Prelude

-- | The LoRaWAN information used with a FUOTA task.
--
-- /See:/ 'newLoRaWANFuotaTask' smart constructor.
data LoRaWANFuotaTask = LoRaWANFuotaTask'
  { rfRegion :: Prelude.Maybe SupportedRfRegion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rfRegion', 'loRaWANFuotaTask_rfRegion' - Undocumented member.
newLoRaWANFuotaTask ::
  LoRaWANFuotaTask
newLoRaWANFuotaTask =
  LoRaWANFuotaTask' {rfRegion = Prelude.Nothing}

-- | Undocumented member.
loRaWANFuotaTask_rfRegion :: Lens.Lens' LoRaWANFuotaTask (Prelude.Maybe SupportedRfRegion)
loRaWANFuotaTask_rfRegion = Lens.lens (\LoRaWANFuotaTask' {rfRegion} -> rfRegion) (\s@LoRaWANFuotaTask' {} a -> s {rfRegion = a} :: LoRaWANFuotaTask)

instance Prelude.Hashable LoRaWANFuotaTask where
  hashWithSalt _salt LoRaWANFuotaTask' {..} =
    _salt `Prelude.hashWithSalt` rfRegion

instance Prelude.NFData LoRaWANFuotaTask where
  rnf LoRaWANFuotaTask' {..} = Prelude.rnf rfRegion

instance Core.ToJSON LoRaWANFuotaTask where
  toJSON LoRaWANFuotaTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [("RfRegion" Core..=) Prelude.<$> rfRegion]
      )
