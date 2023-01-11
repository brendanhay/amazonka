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
-- Module      : Amazonka.OpsWorks.Types.LifecycleEventConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.LifecycleEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types.ShutdownEventConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Specifies the lifecycle event configuration
--
-- /See:/ 'newLifecycleEventConfiguration' smart constructor.
data LifecycleEventConfiguration = LifecycleEventConfiguration'
  { -- | A @ShutdownEventConfiguration@ object that specifies the Shutdown event
    -- configuration.
    shutdown :: Prelude.Maybe ShutdownEventConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifecycleEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shutdown', 'lifecycleEventConfiguration_shutdown' - A @ShutdownEventConfiguration@ object that specifies the Shutdown event
-- configuration.
newLifecycleEventConfiguration ::
  LifecycleEventConfiguration
newLifecycleEventConfiguration =
  LifecycleEventConfiguration'
    { shutdown =
        Prelude.Nothing
    }

-- | A @ShutdownEventConfiguration@ object that specifies the Shutdown event
-- configuration.
lifecycleEventConfiguration_shutdown :: Lens.Lens' LifecycleEventConfiguration (Prelude.Maybe ShutdownEventConfiguration)
lifecycleEventConfiguration_shutdown = Lens.lens (\LifecycleEventConfiguration' {shutdown} -> shutdown) (\s@LifecycleEventConfiguration' {} a -> s {shutdown = a} :: LifecycleEventConfiguration)

instance Data.FromJSON LifecycleEventConfiguration where
  parseJSON =
    Data.withObject
      "LifecycleEventConfiguration"
      ( \x ->
          LifecycleEventConfiguration'
            Prelude.<$> (x Data..:? "Shutdown")
      )

instance Prelude.Hashable LifecycleEventConfiguration where
  hashWithSalt _salt LifecycleEventConfiguration' {..} =
    _salt `Prelude.hashWithSalt` shutdown

instance Prelude.NFData LifecycleEventConfiguration where
  rnf LifecycleEventConfiguration' {..} =
    Prelude.rnf shutdown

instance Data.ToJSON LifecycleEventConfiguration where
  toJSON LifecycleEventConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Shutdown" Data..=) Prelude.<$> shutdown]
      )
