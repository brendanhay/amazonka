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
-- Module      : Amazonka.EC2.Types.InstanceCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ListingState
import qualified Amazonka.Prelude as Prelude

-- | Describes a Reserved Instance listing state.
--
-- /See:/ 'newInstanceCount' smart constructor.
data InstanceCount = InstanceCount'
  { -- | The number of listed Reserved Instances in the state specified by the
    -- @state@.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The states of the listed Reserved Instances.
    state :: Prelude.Maybe ListingState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCount', 'instanceCount_instanceCount' - The number of listed Reserved Instances in the state specified by the
-- @state@.
--
-- 'state', 'instanceCount_state' - The states of the listed Reserved Instances.
newInstanceCount ::
  InstanceCount
newInstanceCount =
  InstanceCount'
    { instanceCount = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The number of listed Reserved Instances in the state specified by the
-- @state@.
instanceCount_instanceCount :: Lens.Lens' InstanceCount (Prelude.Maybe Prelude.Int)
instanceCount_instanceCount = Lens.lens (\InstanceCount' {instanceCount} -> instanceCount) (\s@InstanceCount' {} a -> s {instanceCount = a} :: InstanceCount)

-- | The states of the listed Reserved Instances.
instanceCount_state :: Lens.Lens' InstanceCount (Prelude.Maybe ListingState)
instanceCount_state = Lens.lens (\InstanceCount' {state} -> state) (\s@InstanceCount' {} a -> s {state = a} :: InstanceCount)

instance Data.FromXML InstanceCount where
  parseXML x =
    InstanceCount'
      Prelude.<$> (x Data..@? "instanceCount")
      Prelude.<*> (x Data..@? "state")

instance Prelude.Hashable InstanceCount where
  hashWithSalt _salt InstanceCount' {..} =
    _salt `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` state

instance Prelude.NFData InstanceCount where
  rnf InstanceCount' {..} =
    Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf state
