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
-- Module      : Amazonka.RobOMaker.Types.PortForwardingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.PortForwardingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.PortMapping

-- | Configuration information for port forwarding.
--
-- /See:/ 'newPortForwardingConfig' smart constructor.
data PortForwardingConfig = PortForwardingConfig'
  { -- | The port mappings for the configuration.
    portMappings :: Prelude.Maybe [PortMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortForwardingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portMappings', 'portForwardingConfig_portMappings' - The port mappings for the configuration.
newPortForwardingConfig ::
  PortForwardingConfig
newPortForwardingConfig =
  PortForwardingConfig'
    { portMappings =
        Prelude.Nothing
    }

-- | The port mappings for the configuration.
portForwardingConfig_portMappings :: Lens.Lens' PortForwardingConfig (Prelude.Maybe [PortMapping])
portForwardingConfig_portMappings = Lens.lens (\PortForwardingConfig' {portMappings} -> portMappings) (\s@PortForwardingConfig' {} a -> s {portMappings = a} :: PortForwardingConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PortForwardingConfig where
  parseJSON =
    Data.withObject
      "PortForwardingConfig"
      ( \x ->
          PortForwardingConfig'
            Prelude.<$> (x Data..:? "portMappings" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PortForwardingConfig where
  hashWithSalt _salt PortForwardingConfig' {..} =
    _salt `Prelude.hashWithSalt` portMappings

instance Prelude.NFData PortForwardingConfig where
  rnf PortForwardingConfig' {..} =
    Prelude.rnf portMappings

instance Data.ToJSON PortForwardingConfig where
  toJSON PortForwardingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("portMappings" Data..=) Prelude.<$> portMappings]
      )
