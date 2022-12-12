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
-- Module      : Amazonka.MQ.Types.Configurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.Configurations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types.ConfigurationId
import qualified Amazonka.Prelude as Prelude

-- | Broker configuration information
--
-- /See:/ 'newConfigurations' smart constructor.
data Configurations = Configurations'
  { -- | The broker\'s current configuration.
    current :: Prelude.Maybe ConfigurationId,
    -- | The history of configurations applied to the broker.
    history :: Prelude.Maybe [ConfigurationId],
    -- | The broker\'s pending configuration.
    pending :: Prelude.Maybe ConfigurationId
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
-- 'current', 'configurations_current' - The broker\'s current configuration.
--
-- 'history', 'configurations_history' - The history of configurations applied to the broker.
--
-- 'pending', 'configurations_pending' - The broker\'s pending configuration.
newConfigurations ::
  Configurations
newConfigurations =
  Configurations'
    { current = Prelude.Nothing,
      history = Prelude.Nothing,
      pending = Prelude.Nothing
    }

-- | The broker\'s current configuration.
configurations_current :: Lens.Lens' Configurations (Prelude.Maybe ConfigurationId)
configurations_current = Lens.lens (\Configurations' {current} -> current) (\s@Configurations' {} a -> s {current = a} :: Configurations)

-- | The history of configurations applied to the broker.
configurations_history :: Lens.Lens' Configurations (Prelude.Maybe [ConfigurationId])
configurations_history = Lens.lens (\Configurations' {history} -> history) (\s@Configurations' {} a -> s {history = a} :: Configurations) Prelude.. Lens.mapping Lens.coerced

-- | The broker\'s pending configuration.
configurations_pending :: Lens.Lens' Configurations (Prelude.Maybe ConfigurationId)
configurations_pending = Lens.lens (\Configurations' {pending} -> pending) (\s@Configurations' {} a -> s {pending = a} :: Configurations)

instance Data.FromJSON Configurations where
  parseJSON =
    Data.withObject
      "Configurations"
      ( \x ->
          Configurations'
            Prelude.<$> (x Data..:? "current")
            Prelude.<*> (x Data..:? "history" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "pending")
      )

instance Prelude.Hashable Configurations where
  hashWithSalt _salt Configurations' {..} =
    _salt `Prelude.hashWithSalt` current
      `Prelude.hashWithSalt` history
      `Prelude.hashWithSalt` pending

instance Prelude.NFData Configurations where
  rnf Configurations' {..} =
    Prelude.rnf current
      `Prelude.seq` Prelude.rnf history
      `Prelude.seq` Prelude.rnf pending
