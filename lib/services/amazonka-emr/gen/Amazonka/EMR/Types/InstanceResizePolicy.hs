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
-- Module      : Amazonka.EMR.Types.InstanceResizePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceResizePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Custom policy for requesting termination protection or termination of
-- specific instances when shrinking an instance group.
--
-- /See:/ 'newInstanceResizePolicy' smart constructor.
data InstanceResizePolicy = InstanceResizePolicy'
  { -- | Specific list of instances to be protected when shrinking an instance
    -- group.
    instancesToProtect :: Prelude.Maybe [Prelude.Text],
    -- | Specific list of instances to be terminated when shrinking an instance
    -- group.
    instancesToTerminate :: Prelude.Maybe [Prelude.Text],
    -- | Decommissioning timeout override for the specific list of instances to
    -- be terminated.
    instanceTerminationTimeout :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceResizePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancesToProtect', 'instanceResizePolicy_instancesToProtect' - Specific list of instances to be protected when shrinking an instance
-- group.
--
-- 'instancesToTerminate', 'instanceResizePolicy_instancesToTerminate' - Specific list of instances to be terminated when shrinking an instance
-- group.
--
-- 'instanceTerminationTimeout', 'instanceResizePolicy_instanceTerminationTimeout' - Decommissioning timeout override for the specific list of instances to
-- be terminated.
newInstanceResizePolicy ::
  InstanceResizePolicy
newInstanceResizePolicy =
  InstanceResizePolicy'
    { instancesToProtect =
        Prelude.Nothing,
      instancesToTerminate = Prelude.Nothing,
      instanceTerminationTimeout = Prelude.Nothing
    }

-- | Specific list of instances to be protected when shrinking an instance
-- group.
instanceResizePolicy_instancesToProtect :: Lens.Lens' InstanceResizePolicy (Prelude.Maybe [Prelude.Text])
instanceResizePolicy_instancesToProtect = Lens.lens (\InstanceResizePolicy' {instancesToProtect} -> instancesToProtect) (\s@InstanceResizePolicy' {} a -> s {instancesToProtect = a} :: InstanceResizePolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specific list of instances to be terminated when shrinking an instance
-- group.
instanceResizePolicy_instancesToTerminate :: Lens.Lens' InstanceResizePolicy (Prelude.Maybe [Prelude.Text])
instanceResizePolicy_instancesToTerminate = Lens.lens (\InstanceResizePolicy' {instancesToTerminate} -> instancesToTerminate) (\s@InstanceResizePolicy' {} a -> s {instancesToTerminate = a} :: InstanceResizePolicy) Prelude.. Lens.mapping Lens.coerced

-- | Decommissioning timeout override for the specific list of instances to
-- be terminated.
instanceResizePolicy_instanceTerminationTimeout :: Lens.Lens' InstanceResizePolicy (Prelude.Maybe Prelude.Int)
instanceResizePolicy_instanceTerminationTimeout = Lens.lens (\InstanceResizePolicy' {instanceTerminationTimeout} -> instanceTerminationTimeout) (\s@InstanceResizePolicy' {} a -> s {instanceTerminationTimeout = a} :: InstanceResizePolicy)

instance Core.FromJSON InstanceResizePolicy where
  parseJSON =
    Core.withObject
      "InstanceResizePolicy"
      ( \x ->
          InstanceResizePolicy'
            Prelude.<$> ( x Core..:? "InstancesToProtect"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "InstancesToTerminate"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "InstanceTerminationTimeout")
      )

instance Prelude.Hashable InstanceResizePolicy

instance Prelude.NFData InstanceResizePolicy

instance Core.ToJSON InstanceResizePolicy where
  toJSON InstanceResizePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InstancesToProtect" Core..=)
              Prelude.<$> instancesToProtect,
            ("InstancesToTerminate" Core..=)
              Prelude.<$> instancesToTerminate,
            ("InstanceTerminationTimeout" Core..=)
              Prelude.<$> instanceTerminationTimeout
          ]
      )
