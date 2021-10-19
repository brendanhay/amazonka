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
-- Module      : Network.AWS.MQ.Types.Configurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.Configurations where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.ConfigurationId
import qualified Network.AWS.Prelude as Prelude

-- | Broker configuration information
--
-- /See:/ 'newConfigurations' smart constructor.
data Configurations = Configurations'
  { -- | The broker\'s pending configuration.
    pending :: Prelude.Maybe ConfigurationId,
    -- | The history of configurations applied to the broker.
    history :: Prelude.Maybe [ConfigurationId],
    -- | The broker\'s current configuration.
    current :: Prelude.Maybe ConfigurationId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Configurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pending', 'configurations_pending' - The broker\'s pending configuration.
--
-- 'history', 'configurations_history' - The history of configurations applied to the broker.
--
-- 'current', 'configurations_current' - The broker\'s current configuration.
newConfigurations ::
  Configurations
newConfigurations =
  Configurations'
    { pending = Prelude.Nothing,
      history = Prelude.Nothing,
      current = Prelude.Nothing
    }

-- | The broker\'s pending configuration.
configurations_pending :: Lens.Lens' Configurations (Prelude.Maybe ConfigurationId)
configurations_pending = Lens.lens (\Configurations' {pending} -> pending) (\s@Configurations' {} a -> s {pending = a} :: Configurations)

-- | The history of configurations applied to the broker.
configurations_history :: Lens.Lens' Configurations (Prelude.Maybe [ConfigurationId])
configurations_history = Lens.lens (\Configurations' {history} -> history) (\s@Configurations' {} a -> s {history = a} :: Configurations) Prelude.. Lens.mapping Lens.coerced

-- | The broker\'s current configuration.
configurations_current :: Lens.Lens' Configurations (Prelude.Maybe ConfigurationId)
configurations_current = Lens.lens (\Configurations' {current} -> current) (\s@Configurations' {} a -> s {current = a} :: Configurations)

instance Core.FromJSON Configurations where
  parseJSON =
    Core.withObject
      "Configurations"
      ( \x ->
          Configurations'
            Prelude.<$> (x Core..:? "pending")
            Prelude.<*> (x Core..:? "history" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "current")
      )

instance Prelude.Hashable Configurations

instance Prelude.NFData Configurations
