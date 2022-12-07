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
-- Module      : Amazonka.EC2.Types.AvailableCapacity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AvailableCapacity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceCapacity
import qualified Amazonka.Prelude as Prelude

-- | The capacity information for instances that can be launched onto the
-- Dedicated Host.
--
-- /See:/ 'newAvailableCapacity' smart constructor.
data AvailableCapacity = AvailableCapacity'
  { -- | The number of vCPUs available for launching instances onto the Dedicated
    -- Host.
    availableVCpus :: Prelude.Maybe Prelude.Int,
    -- | The number of instances that can be launched onto the Dedicated Host
    -- depending on the host\'s available capacity. For Dedicated Hosts that
    -- support multiple instance types, this parameter represents the number of
    -- instances for each instance size that is supported on the host.
    availableInstanceCapacity :: Prelude.Maybe [InstanceCapacity]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailableCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availableVCpus', 'availableCapacity_availableVCpus' - The number of vCPUs available for launching instances onto the Dedicated
-- Host.
--
-- 'availableInstanceCapacity', 'availableCapacity_availableInstanceCapacity' - The number of instances that can be launched onto the Dedicated Host
-- depending on the host\'s available capacity. For Dedicated Hosts that
-- support multiple instance types, this parameter represents the number of
-- instances for each instance size that is supported on the host.
newAvailableCapacity ::
  AvailableCapacity
newAvailableCapacity =
  AvailableCapacity'
    { availableVCpus =
        Prelude.Nothing,
      availableInstanceCapacity = Prelude.Nothing
    }

-- | The number of vCPUs available for launching instances onto the Dedicated
-- Host.
availableCapacity_availableVCpus :: Lens.Lens' AvailableCapacity (Prelude.Maybe Prelude.Int)
availableCapacity_availableVCpus = Lens.lens (\AvailableCapacity' {availableVCpus} -> availableVCpus) (\s@AvailableCapacity' {} a -> s {availableVCpus = a} :: AvailableCapacity)

-- | The number of instances that can be launched onto the Dedicated Host
-- depending on the host\'s available capacity. For Dedicated Hosts that
-- support multiple instance types, this parameter represents the number of
-- instances for each instance size that is supported on the host.
availableCapacity_availableInstanceCapacity :: Lens.Lens' AvailableCapacity (Prelude.Maybe [InstanceCapacity])
availableCapacity_availableInstanceCapacity = Lens.lens (\AvailableCapacity' {availableInstanceCapacity} -> availableInstanceCapacity) (\s@AvailableCapacity' {} a -> s {availableInstanceCapacity = a} :: AvailableCapacity) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML AvailableCapacity where
  parseXML x =
    AvailableCapacity'
      Prelude.<$> (x Data..@? "availableVCpus")
      Prelude.<*> ( x Data..@? "availableInstanceCapacity"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable AvailableCapacity where
  hashWithSalt _salt AvailableCapacity' {..} =
    _salt `Prelude.hashWithSalt` availableVCpus
      `Prelude.hashWithSalt` availableInstanceCapacity

instance Prelude.NFData AvailableCapacity where
  rnf AvailableCapacity' {..} =
    Prelude.rnf availableVCpus
      `Prelude.seq` Prelude.rnf availableInstanceCapacity
