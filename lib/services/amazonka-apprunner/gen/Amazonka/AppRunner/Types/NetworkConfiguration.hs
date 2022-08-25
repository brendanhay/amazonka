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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.NetworkConfiguration where

import Amazonka.AppRunner.Types.EgressConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes configuration settings related to network traffic of an App
-- Runner service. Consists of embedded objects for each configurable
-- network feature.
--
-- /See:/ 'newNetworkConfiguration' smart constructor.
data NetworkConfiguration = NetworkConfiguration'
  { -- | Network configuration settings for outbound message traffic.
    egressConfiguration :: Prelude.Maybe EgressConfiguration
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
newNetworkConfiguration ::
  NetworkConfiguration
newNetworkConfiguration =
  NetworkConfiguration'
    { egressConfiguration =
        Prelude.Nothing
    }

-- | Network configuration settings for outbound message traffic.
networkConfiguration_egressConfiguration :: Lens.Lens' NetworkConfiguration (Prelude.Maybe EgressConfiguration)
networkConfiguration_egressConfiguration = Lens.lens (\NetworkConfiguration' {egressConfiguration} -> egressConfiguration) (\s@NetworkConfiguration' {} a -> s {egressConfiguration = a} :: NetworkConfiguration)

instance Core.FromJSON NetworkConfiguration where
  parseJSON =
    Core.withObject
      "NetworkConfiguration"
      ( \x ->
          NetworkConfiguration'
            Prelude.<$> (x Core..:? "EgressConfiguration")
      )

instance Prelude.Hashable NetworkConfiguration where
  hashWithSalt _salt NetworkConfiguration' {..} =
    _salt `Prelude.hashWithSalt` egressConfiguration

instance Prelude.NFData NetworkConfiguration where
  rnf NetworkConfiguration' {..} =
    Prelude.rnf egressConfiguration

instance Core.ToJSON NetworkConfiguration where
  toJSON NetworkConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EgressConfiguration" Core..=)
              Prelude.<$> egressConfiguration
          ]
      )
