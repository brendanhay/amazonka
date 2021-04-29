{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Shutdown event configuration.
--
-- /See:/ 'newShutdownEventConfiguration' smart constructor.
data ShutdownEventConfiguration = ShutdownEventConfiguration'
  { -- | The time, in seconds, that AWS OpsWorks Stacks will wait after
    -- triggering a Shutdown event before shutting down an instance.
    executionTimeout :: Prelude.Maybe Prelude.Int,
    -- | Whether to enable Elastic Load Balancing connection draining. For more
    -- information, see
    -- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
    delayUntilElbConnectionsDrained :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      delayUntilElbConnectionsDrained =
        Prelude.Nothing
    }

-- | The time, in seconds, that AWS OpsWorks Stacks will wait after
-- triggering a Shutdown event before shutting down an instance.
shutdownEventConfiguration_executionTimeout :: Lens.Lens' ShutdownEventConfiguration (Prelude.Maybe Prelude.Int)
shutdownEventConfiguration_executionTimeout = Lens.lens (\ShutdownEventConfiguration' {executionTimeout} -> executionTimeout) (\s@ShutdownEventConfiguration' {} a -> s {executionTimeout = a} :: ShutdownEventConfiguration)

-- | Whether to enable Elastic Load Balancing connection draining. For more
-- information, see
-- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
shutdownEventConfiguration_delayUntilElbConnectionsDrained :: Lens.Lens' ShutdownEventConfiguration (Prelude.Maybe Prelude.Bool)
shutdownEventConfiguration_delayUntilElbConnectionsDrained = Lens.lens (\ShutdownEventConfiguration' {delayUntilElbConnectionsDrained} -> delayUntilElbConnectionsDrained) (\s@ShutdownEventConfiguration' {} a -> s {delayUntilElbConnectionsDrained = a} :: ShutdownEventConfiguration)

instance Prelude.FromJSON ShutdownEventConfiguration where
  parseJSON =
    Prelude.withObject
      "ShutdownEventConfiguration"
      ( \x ->
          ShutdownEventConfiguration'
            Prelude.<$> (x Prelude..:? "ExecutionTimeout")
            Prelude.<*> (x Prelude..:? "DelayUntilElbConnectionsDrained")
      )

instance Prelude.Hashable ShutdownEventConfiguration

instance Prelude.NFData ShutdownEventConfiguration

instance Prelude.ToJSON ShutdownEventConfiguration where
  toJSON ShutdownEventConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExecutionTimeout" Prelude..=)
              Prelude.<$> executionTimeout,
            ("DelayUntilElbConnectionsDrained" Prelude..=)
              Prelude.<$> delayUntilElbConnectionsDrained
          ]
      )
