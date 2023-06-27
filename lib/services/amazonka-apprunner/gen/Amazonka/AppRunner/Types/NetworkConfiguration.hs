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
-- Module      : Amazonka.AppRunner.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.NetworkConfiguration where

import Amazonka.AppRunner.Types.EgressConfiguration
import Amazonka.AppRunner.Types.IngressConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes configuration settings related to network traffic of an App
-- Runner service. Consists of embedded objects for each configurable
-- network feature.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | Network configuration settings for outbound message traffic.
    egressConfiguration :: Prelude.Maybe EgressConfiguration,
    -- | Network configuration settings for inbound message traffic.
    ingressConfiguration :: Prelude.Maybe IngressConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressConfiguration', 'networkConfiguration_egressConfiguration' - Network configuration settings for outbound message traffic.
--
-- 'ingressConfiguration', 'networkConfiguration_ingressConfiguration' - Network configuration settings for inbound message traffic.
newNetworkConfiguration ::
  NetworkConfiguration
newNetworkConfiguration =
  NetworkConfiguration'
    { egressConfiguration =
        Prelude.Nothing,
      ingressConfiguration = Prelude.Nothing
    }

-- | Network configuration settings for outbound message traffic.
networkConfiguration_egressConfiguration :: Lens.Lens' NetworkConfiguration (Prelude.Maybe EgressConfiguration)
networkConfiguration_egressConfiguration = Lens.lens (\NetworkConfiguration' {egressConfiguration} -> egressConfiguration) (\s@NetworkConfiguration' {} a -> s {egressConfiguration = a} :: NetworkConfiguration)

-- | Network configuration settings for inbound message traffic.
networkConfiguration_ingressConfiguration :: Lens.Lens' NetworkConfiguration (Prelude.Maybe IngressConfiguration)
networkConfiguration_ingressConfiguration = Lens.lens (\NetworkConfiguration' {ingressConfiguration} -> ingressConfiguration) (\s@NetworkConfiguration' {} a -> s {ingressConfiguration = a} :: NetworkConfiguration)

instance Data.FromJSON NetworkConfiguration where
  parseJSON =
    Data.withObject
      "NetworkConfiguration"
      ( \x ->
          NetworkConfiguration'
            Prelude.<$> (x Data..:? "EgressConfiguration")
            Prelude.<*> (x Data..:? "IngressConfiguration")
      )

instance Prelude.Hashable NetworkConfiguration where
  hashWithSalt _salt NetworkConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` egressConfiguration
      `Prelude.hashWithSalt` ingressConfiguration

instance Prelude.NFData NetworkConfiguration where
  rnf NetworkConfiguration' {..} =
    Prelude.rnf egressConfiguration
      `Prelude.seq` Prelude.rnf ingressConfiguration

instance Data.ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EgressConfiguration" Data..=)
              Prelude.<$> egressConfiguration,
            ("IngressConfiguration" Data..=)
              Prelude.<$> ingressConfiguration
          ]
      )
