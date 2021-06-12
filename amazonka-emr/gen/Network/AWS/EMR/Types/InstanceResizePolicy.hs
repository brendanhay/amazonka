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
-- Module      : Network.AWS.EMR.Types.InstanceResizePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceResizePolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Custom policy for requesting termination protection or termination of
-- specific instances when shrinking an instance group.
--
-- /See:/ 'newInstanceResizePolicy' smart constructor.
data InstanceResizePolicy = InstanceResizePolicy'
  { -- | Decommissioning timeout override for the specific list of instances to
    -- be terminated.
    instanceTerminationTimeout :: Core.Maybe Core.Int,
    -- | Specific list of instances to be terminated when shrinking an instance
    -- group.
    instancesToTerminate :: Core.Maybe [Core.Text],
    -- | Specific list of instances to be protected when shrinking an instance
    -- group.
    instancesToProtect :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceResizePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTerminationTimeout', 'instanceResizePolicy_instanceTerminationTimeout' - Decommissioning timeout override for the specific list of instances to
-- be terminated.
--
-- 'instancesToTerminate', 'instanceResizePolicy_instancesToTerminate' - Specific list of instances to be terminated when shrinking an instance
-- group.
--
-- 'instancesToProtect', 'instanceResizePolicy_instancesToProtect' - Specific list of instances to be protected when shrinking an instance
-- group.
newInstanceResizePolicy ::
  InstanceResizePolicy
newInstanceResizePolicy =
  InstanceResizePolicy'
    { instanceTerminationTimeout =
        Core.Nothing,
      instancesToTerminate = Core.Nothing,
      instancesToProtect = Core.Nothing
    }

-- | Decommissioning timeout override for the specific list of instances to
-- be terminated.
instanceResizePolicy_instanceTerminationTimeout :: Lens.Lens' InstanceResizePolicy (Core.Maybe Core.Int)
instanceResizePolicy_instanceTerminationTimeout = Lens.lens (\InstanceResizePolicy' {instanceTerminationTimeout} -> instanceTerminationTimeout) (\s@InstanceResizePolicy' {} a -> s {instanceTerminationTimeout = a} :: InstanceResizePolicy)

-- | Specific list of instances to be terminated when shrinking an instance
-- group.
instanceResizePolicy_instancesToTerminate :: Lens.Lens' InstanceResizePolicy (Core.Maybe [Core.Text])
instanceResizePolicy_instancesToTerminate = Lens.lens (\InstanceResizePolicy' {instancesToTerminate} -> instancesToTerminate) (\s@InstanceResizePolicy' {} a -> s {instancesToTerminate = a} :: InstanceResizePolicy) Core.. Lens.mapping Lens._Coerce

-- | Specific list of instances to be protected when shrinking an instance
-- group.
instanceResizePolicy_instancesToProtect :: Lens.Lens' InstanceResizePolicy (Core.Maybe [Core.Text])
instanceResizePolicy_instancesToProtect = Lens.lens (\InstanceResizePolicy' {instancesToProtect} -> instancesToProtect) (\s@InstanceResizePolicy' {} a -> s {instancesToProtect = a} :: InstanceResizePolicy) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON InstanceResizePolicy where
  parseJSON =
    Core.withObject
      "InstanceResizePolicy"
      ( \x ->
          InstanceResizePolicy'
            Core.<$> (x Core..:? "InstanceTerminationTimeout")
            Core.<*> ( x Core..:? "InstancesToTerminate"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "InstancesToProtect"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable InstanceResizePolicy

instance Core.NFData InstanceResizePolicy

instance Core.ToJSON InstanceResizePolicy where
  toJSON InstanceResizePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceTerminationTimeout" Core..=)
              Core.<$> instanceTerminationTimeout,
            ("InstancesToTerminate" Core..=)
              Core.<$> instancesToTerminate,
            ("InstancesToProtect" Core..=)
              Core.<$> instancesToProtect
          ]
      )
