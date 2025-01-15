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
-- Module      : Amazonka.OpsWorks.Types.ShutdownEventConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.ShutdownEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Shutdown event configuration.
--
-- /See:/ 'newShutdownEventConfiguration' smart constructor.
data ShutdownEventConfiguration = ShutdownEventConfiguration'
  { -- | Whether to enable Elastic Load Balancing connection draining. For more
    -- information, see
    -- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
    delayUntilElbConnectionsDrained :: Prelude.Maybe Prelude.Bool,
    -- | The time, in seconds, that AWS OpsWorks Stacks will wait after
    -- triggering a Shutdown event before shutting down an instance.
    executionTimeout :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShutdownEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delayUntilElbConnectionsDrained', 'shutdownEventConfiguration_delayUntilElbConnectionsDrained' - Whether to enable Elastic Load Balancing connection draining. For more
-- information, see
-- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
--
-- 'executionTimeout', 'shutdownEventConfiguration_executionTimeout' - The time, in seconds, that AWS OpsWorks Stacks will wait after
-- triggering a Shutdown event before shutting down an instance.
newShutdownEventConfiguration ::
  ShutdownEventConfiguration
newShutdownEventConfiguration =
  ShutdownEventConfiguration'
    { delayUntilElbConnectionsDrained =
        Prelude.Nothing,
      executionTimeout = Prelude.Nothing
    }

-- | Whether to enable Elastic Load Balancing connection draining. For more
-- information, see
-- <https://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
shutdownEventConfiguration_delayUntilElbConnectionsDrained :: Lens.Lens' ShutdownEventConfiguration (Prelude.Maybe Prelude.Bool)
shutdownEventConfiguration_delayUntilElbConnectionsDrained = Lens.lens (\ShutdownEventConfiguration' {delayUntilElbConnectionsDrained} -> delayUntilElbConnectionsDrained) (\s@ShutdownEventConfiguration' {} a -> s {delayUntilElbConnectionsDrained = a} :: ShutdownEventConfiguration)

-- | The time, in seconds, that AWS OpsWorks Stacks will wait after
-- triggering a Shutdown event before shutting down an instance.
shutdownEventConfiguration_executionTimeout :: Lens.Lens' ShutdownEventConfiguration (Prelude.Maybe Prelude.Int)
shutdownEventConfiguration_executionTimeout = Lens.lens (\ShutdownEventConfiguration' {executionTimeout} -> executionTimeout) (\s@ShutdownEventConfiguration' {} a -> s {executionTimeout = a} :: ShutdownEventConfiguration)

instance Data.FromJSON ShutdownEventConfiguration where
  parseJSON =
    Data.withObject
      "ShutdownEventConfiguration"
      ( \x ->
          ShutdownEventConfiguration'
            Prelude.<$> (x Data..:? "DelayUntilElbConnectionsDrained")
            Prelude.<*> (x Data..:? "ExecutionTimeout")
      )

instance Prelude.Hashable ShutdownEventConfiguration where
  hashWithSalt _salt ShutdownEventConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` delayUntilElbConnectionsDrained
      `Prelude.hashWithSalt` executionTimeout

instance Prelude.NFData ShutdownEventConfiguration where
  rnf ShutdownEventConfiguration' {..} =
    Prelude.rnf delayUntilElbConnectionsDrained `Prelude.seq`
      Prelude.rnf executionTimeout

instance Data.ToJSON ShutdownEventConfiguration where
  toJSON ShutdownEventConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DelayUntilElbConnectionsDrained" Data..=)
              Prelude.<$> delayUntilElbConnectionsDrained,
            ("ExecutionTimeout" Data..=)
              Prelude.<$> executionTimeout
          ]
      )
