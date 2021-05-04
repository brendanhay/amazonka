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
-- Module      : Network.AWS.MQ.Types.Configurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.Configurations where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.ConfigurationId
import qualified Network.AWS.Prelude as Prelude

-- | Broker configuration information
--
-- /See:/ 'newConfigurations' smart constructor.
data Configurations = Configurations'
  { -- | The pending configuration of the broker.
    pending :: Prelude.Maybe ConfigurationId,
    -- | The current configuration of the broker.
    current :: Prelude.Maybe ConfigurationId,
    -- | The history of configurations applied to the broker.
    history :: Prelude.Maybe [ConfigurationId]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Configurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pending', 'configurations_pending' - The pending configuration of the broker.
--
-- 'current', 'configurations_current' - The current configuration of the broker.
--
-- 'history', 'configurations_history' - The history of configurations applied to the broker.
newConfigurations ::
  Configurations
newConfigurations =
  Configurations'
    { pending = Prelude.Nothing,
      current = Prelude.Nothing,
      history = Prelude.Nothing
    }

-- | The pending configuration of the broker.
configurations_pending :: Lens.Lens' Configurations (Prelude.Maybe ConfigurationId)
configurations_pending = Lens.lens (\Configurations' {pending} -> pending) (\s@Configurations' {} a -> s {pending = a} :: Configurations)

-- | The current configuration of the broker.
configurations_current :: Lens.Lens' Configurations (Prelude.Maybe ConfigurationId)
configurations_current = Lens.lens (\Configurations' {current} -> current) (\s@Configurations' {} a -> s {current = a} :: Configurations)

-- | The history of configurations applied to the broker.
configurations_history :: Lens.Lens' Configurations (Prelude.Maybe [ConfigurationId])
configurations_history = Lens.lens (\Configurations' {history} -> history) (\s@Configurations' {} a -> s {history = a} :: Configurations) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Configurations where
  parseJSON =
    Prelude.withObject
      "Configurations"
      ( \x ->
          Configurations'
            Prelude.<$> (x Prelude..:? "pending")
            Prelude.<*> (x Prelude..:? "current")
            Prelude.<*> (x Prelude..:? "history" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable Configurations

instance Prelude.NFData Configurations
