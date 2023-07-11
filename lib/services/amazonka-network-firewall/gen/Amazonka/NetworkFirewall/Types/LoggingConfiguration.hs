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
-- Module      : Amazonka.NetworkFirewall.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.LoggingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.LogDestinationConfig
import qualified Amazonka.Prelude as Prelude

-- | Defines how Network Firewall performs logging for a Firewall.
--
-- /See:/ 'newLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { -- | Defines the logging destinations for the logs for a firewall. Network
    -- Firewall generates logs for stateful rule groups.
    logDestinationConfigs :: [LogDestinationConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logDestinationConfigs', 'loggingConfiguration_logDestinationConfigs' - Defines the logging destinations for the logs for a firewall. Network
-- Firewall generates logs for stateful rule groups.
newLoggingConfiguration ::
  LoggingConfiguration
newLoggingConfiguration =
  LoggingConfiguration'
    { logDestinationConfigs =
        Prelude.mempty
    }

-- | Defines the logging destinations for the logs for a firewall. Network
-- Firewall generates logs for stateful rule groups.
loggingConfiguration_logDestinationConfigs :: Lens.Lens' LoggingConfiguration [LogDestinationConfig]
loggingConfiguration_logDestinationConfigs = Lens.lens (\LoggingConfiguration' {logDestinationConfigs} -> logDestinationConfigs) (\s@LoggingConfiguration' {} a -> s {logDestinationConfigs = a} :: LoggingConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON LoggingConfiguration where
  parseJSON =
    Data.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Prelude.<$> ( x
                            Data..:? "LogDestinationConfigs"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LoggingConfiguration where
  hashWithSalt _salt LoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` logDestinationConfigs

instance Prelude.NFData LoggingConfiguration where
  rnf LoggingConfiguration' {..} =
    Prelude.rnf logDestinationConfigs

instance Data.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LogDestinationConfigs"
                  Data..= logDestinationConfigs
              )
          ]
      )
