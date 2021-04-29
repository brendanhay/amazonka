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
-- Module      : Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.LifecycleEventConfiguration where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the lifecycle event configuration
--
-- /See:/ 'newLifecycleEventConfiguration' smart constructor.
data LifecycleEventConfiguration = LifecycleEventConfiguration'
  { -- | A @ShutdownEventConfiguration@ object that specifies the Shutdown event
    -- configuration.
    shutdown :: Prelude.Maybe ShutdownEventConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON LifecycleEventConfiguration where
  parseJSON =
    Prelude.withObject
      "LifecycleEventConfiguration"
      ( \x ->
          LifecycleEventConfiguration'
            Prelude.<$> (x Prelude..:? "Shutdown")
      )

instance Prelude.Hashable LifecycleEventConfiguration

instance Prelude.NFData LifecycleEventConfiguration

instance Prelude.ToJSON LifecycleEventConfiguration where
  toJSON LifecycleEventConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Shutdown" Prelude..=) Prelude.<$> shutdown]
      )
