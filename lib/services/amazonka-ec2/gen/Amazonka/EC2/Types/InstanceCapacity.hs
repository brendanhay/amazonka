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
-- Module      : Amazonka.EC2.Types.InstanceCapacity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceCapacity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the number of instances that can be launched onto the
-- Dedicated Host.
--
-- /See:/ 'newInstanceCapacity' smart constructor.
data InstanceCapacity = InstanceCapacity'
  { -- | The total number of instances that can be launched onto the Dedicated
    -- Host if there are no instances running on it.
    totalCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of instances that can be launched onto the Dedicated Host
    -- based on the host\'s available capacity.
    availableCapacity :: Prelude.Maybe Prelude.Int,
    -- | The instance type supported by the Dedicated Host.
    instanceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalCapacity', 'instanceCapacity_totalCapacity' - The total number of instances that can be launched onto the Dedicated
-- Host if there are no instances running on it.
--
-- 'availableCapacity', 'instanceCapacity_availableCapacity' - The number of instances that can be launched onto the Dedicated Host
-- based on the host\'s available capacity.
--
-- 'instanceType', 'instanceCapacity_instanceType' - The instance type supported by the Dedicated Host.
newInstanceCapacity ::
  InstanceCapacity
newInstanceCapacity =
  InstanceCapacity'
    { totalCapacity = Prelude.Nothing,
      availableCapacity = Prelude.Nothing,
      instanceType = Prelude.Nothing
    }

-- | The total number of instances that can be launched onto the Dedicated
-- Host if there are no instances running on it.
instanceCapacity_totalCapacity :: Lens.Lens' InstanceCapacity (Prelude.Maybe Prelude.Int)
instanceCapacity_totalCapacity = Lens.lens (\InstanceCapacity' {totalCapacity} -> totalCapacity) (\s@InstanceCapacity' {} a -> s {totalCapacity = a} :: InstanceCapacity)

-- | The number of instances that can be launched onto the Dedicated Host
-- based on the host\'s available capacity.
instanceCapacity_availableCapacity :: Lens.Lens' InstanceCapacity (Prelude.Maybe Prelude.Int)
instanceCapacity_availableCapacity = Lens.lens (\InstanceCapacity' {availableCapacity} -> availableCapacity) (\s@InstanceCapacity' {} a -> s {availableCapacity = a} :: InstanceCapacity)

-- | The instance type supported by the Dedicated Host.
instanceCapacity_instanceType :: Lens.Lens' InstanceCapacity (Prelude.Maybe Prelude.Text)
instanceCapacity_instanceType = Lens.lens (\InstanceCapacity' {instanceType} -> instanceType) (\s@InstanceCapacity' {} a -> s {instanceType = a} :: InstanceCapacity)

instance Data.FromXML InstanceCapacity where
  parseXML x =
    InstanceCapacity'
      Prelude.<$> (x Data..@? "totalCapacity")
      Prelude.<*> (x Data..@? "availableCapacity")
      Prelude.<*> (x Data..@? "instanceType")

instance Prelude.Hashable InstanceCapacity where
  hashWithSalt _salt InstanceCapacity' {..} =
    _salt `Prelude.hashWithSalt` totalCapacity
      `Prelude.hashWithSalt` availableCapacity
      `Prelude.hashWithSalt` instanceType

instance Prelude.NFData InstanceCapacity where
  rnf InstanceCapacity' {..} =
    Prelude.rnf totalCapacity
      `Prelude.seq` Prelude.rnf availableCapacity
      `Prelude.seq` Prelude.rnf instanceType
