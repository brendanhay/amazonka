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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceNetworking where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.InstancePortInfo
import Amazonka.Lightsail.Types.MonthlyTransfer
import qualified Amazonka.Prelude as Prelude

-- | Describes monthly data transfer rates and port information for an
-- instance.
--
-- /See:/ 'newInstanceNetworking' smart constructor.
data InstanceNetworking = InstanceNetworking'
  { -- | The amount of data in GB allocated for monthly data transfers.
    monthlyTransfer :: Prelude.Maybe MonthlyTransfer,
    -- | An array of key-value pairs containing information about the ports on
    -- the instance.
    ports :: Prelude.Maybe [InstancePortInfo]
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
-- 'monthlyTransfer', 'instanceNetworking_monthlyTransfer' - The amount of data in GB allocated for monthly data transfers.
--
-- 'ports', 'instanceNetworking_ports' - An array of key-value pairs containing information about the ports on
-- the instance.
newInstanceNetworking ::
  InstanceNetworking
newInstanceNetworking =
  InstanceNetworking'
    { monthlyTransfer =
        Prelude.Nothing,
      ports = Prelude.Nothing
    }

-- | The amount of data in GB allocated for monthly data transfers.
instanceNetworking_monthlyTransfer :: Lens.Lens' InstanceNetworking (Prelude.Maybe MonthlyTransfer)
instanceNetworking_monthlyTransfer = Lens.lens (\InstanceNetworking' {monthlyTransfer} -> monthlyTransfer) (\s@InstanceNetworking' {} a -> s {monthlyTransfer = a} :: InstanceNetworking)

-- | An array of key-value pairs containing information about the ports on
-- the instance.
instanceNetworking_ports :: Lens.Lens' InstanceNetworking (Prelude.Maybe [InstancePortInfo])
instanceNetworking_ports = Lens.lens (\InstanceNetworking' {ports} -> ports) (\s@InstanceNetworking' {} a -> s {ports = a} :: InstanceNetworking) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InstanceNetworking where
  parseJSON =
    Data.withObject
      "InstanceNetworking"
      ( \x ->
          InstanceNetworking'
            Prelude.<$> (x Data..:? "monthlyTransfer")
            Prelude.<*> (x Data..:? "ports" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable InstanceNetworking where
  hashWithSalt _salt InstanceNetworking' {..} =
    _salt
      `Prelude.hashWithSalt` monthlyTransfer
      `Prelude.hashWithSalt` ports

instance Prelude.NFData InstanceNetworking where
  rnf InstanceNetworking' {..} =
    Prelude.rnf monthlyTransfer
      `Prelude.seq` Prelude.rnf ports
