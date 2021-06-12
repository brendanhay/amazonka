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
-- Module      : Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ShutdownEventConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Shutdown event configuration.
--
-- /See:/ 'newShutdownEventConfiguration' smart constructor.
data ShutdownEventConfiguration = ShutdownEventConfiguration'
  { -- | The time, in seconds, that AWS OpsWorks Stacks will wait after
    -- triggering a Shutdown event before shutting down an instance.
    executionTimeout :: Core.Maybe Core.Int,
    -- | Whether to enable Elastic Load Balancing connection draining. For more
    -- information, see
    -- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
    delayUntilElbConnectionsDrained :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ShutdownEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionTimeout', 'shutdownEventConfiguration_executionTimeout' - The time, in seconds, that AWS OpsWorks Stacks will wait after
-- triggering a Shutdown event before shutting down an instance.
--
-- 'delayUntilElbConnectionsDrained', 'shutdownEventConfiguration_delayUntilElbConnectionsDrained' - Whether to enable Elastic Load Balancing connection draining. For more
-- information, see
-- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
newShutdownEventConfiguration ::
  ShutdownEventConfiguration
newShutdownEventConfiguration =
  ShutdownEventConfiguration'
    { executionTimeout =
        Core.Nothing,
      delayUntilElbConnectionsDrained = Core.Nothing
    }

-- | The time, in seconds, that AWS OpsWorks Stacks will wait after
-- triggering a Shutdown event before shutting down an instance.
shutdownEventConfiguration_executionTimeout :: Lens.Lens' ShutdownEventConfiguration (Core.Maybe Core.Int)
shutdownEventConfiguration_executionTimeout = Lens.lens (\ShutdownEventConfiguration' {executionTimeout} -> executionTimeout) (\s@ShutdownEventConfiguration' {} a -> s {executionTimeout = a} :: ShutdownEventConfiguration)

-- | Whether to enable Elastic Load Balancing connection draining. For more
-- information, see
-- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
shutdownEventConfiguration_delayUntilElbConnectionsDrained :: Lens.Lens' ShutdownEventConfiguration (Core.Maybe Core.Bool)
shutdownEventConfiguration_delayUntilElbConnectionsDrained = Lens.lens (\ShutdownEventConfiguration' {delayUntilElbConnectionsDrained} -> delayUntilElbConnectionsDrained) (\s@ShutdownEventConfiguration' {} a -> s {delayUntilElbConnectionsDrained = a} :: ShutdownEventConfiguration)

instance Core.FromJSON ShutdownEventConfiguration where
  parseJSON =
    Core.withObject
      "ShutdownEventConfiguration"
      ( \x ->
          ShutdownEventConfiguration'
            Core.<$> (x Core..:? "ExecutionTimeout")
            Core.<*> (x Core..:? "DelayUntilElbConnectionsDrained")
      )

instance Core.Hashable ShutdownEventConfiguration

instance Core.NFData ShutdownEventConfiguration

instance Core.ToJSON ShutdownEventConfiguration where
  toJSON ShutdownEventConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExecutionTimeout" Core..=)
              Core.<$> executionTimeout,
            ("DelayUntilElbConnectionsDrained" Core..=)
              Core.<$> delayUntilElbConnectionsDrained
          ]
      )
