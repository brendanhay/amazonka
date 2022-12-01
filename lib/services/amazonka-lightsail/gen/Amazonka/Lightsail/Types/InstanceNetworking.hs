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
-- Module      : Amazonka.Lightsail.Types.InstanceNetworking
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceNetworking where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.InstancePortInfo
import Amazonka.Lightsail.Types.MonthlyTransfer
import qualified Amazonka.Prelude as Prelude

-- | Describes monthly data transfer rates and port information for an
-- instance.
--
-- /See:/ 'newInstanceNetworking' smart constructor.
data InstanceNetworking = InstanceNetworking'
  { -- | An array of key-value pairs containing information about the ports on
    -- the instance.
    ports :: Prelude.Maybe [InstancePortInfo],
    -- | The amount of data in GB allocated for monthly data transfers.
    monthlyTransfer :: Prelude.Maybe MonthlyTransfer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceNetworking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ports', 'instanceNetworking_ports' - An array of key-value pairs containing information about the ports on
-- the instance.
--
-- 'monthlyTransfer', 'instanceNetworking_monthlyTransfer' - The amount of data in GB allocated for monthly data transfers.
newInstanceNetworking ::
  InstanceNetworking
newInstanceNetworking =
  InstanceNetworking'
    { ports = Prelude.Nothing,
      monthlyTransfer = Prelude.Nothing
    }

-- | An array of key-value pairs containing information about the ports on
-- the instance.
instanceNetworking_ports :: Lens.Lens' InstanceNetworking (Prelude.Maybe [InstancePortInfo])
instanceNetworking_ports = Lens.lens (\InstanceNetworking' {ports} -> ports) (\s@InstanceNetworking' {} a -> s {ports = a} :: InstanceNetworking) Prelude.. Lens.mapping Lens.coerced

-- | The amount of data in GB allocated for monthly data transfers.
instanceNetworking_monthlyTransfer :: Lens.Lens' InstanceNetworking (Prelude.Maybe MonthlyTransfer)
instanceNetworking_monthlyTransfer = Lens.lens (\InstanceNetworking' {monthlyTransfer} -> monthlyTransfer) (\s@InstanceNetworking' {} a -> s {monthlyTransfer = a} :: InstanceNetworking)

instance Core.FromJSON InstanceNetworking where
  parseJSON =
    Core.withObject
      "InstanceNetworking"
      ( \x ->
          InstanceNetworking'
            Prelude.<$> (x Core..:? "ports" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "monthlyTransfer")
      )

instance Prelude.Hashable InstanceNetworking where
  hashWithSalt _salt InstanceNetworking' {..} =
    _salt `Prelude.hashWithSalt` ports
      `Prelude.hashWithSalt` monthlyTransfer

instance Prelude.NFData InstanceNetworking where
  rnf InstanceNetworking' {..} =
    Prelude.rnf ports
      `Prelude.seq` Prelude.rnf monthlyTransfer
