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
-- Module      : Amazonka.CloudWatchEvents.Types.RoutingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.RoutingConfig where

import Amazonka.CloudWatchEvents.Types.FailoverConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The routing configuration of the endpoint.
--
-- /See:/ 'newRoutingConfig' smart constructor.
data RoutingConfig = RoutingConfig'
  { -- | The failover configuration for an endpoint. This includes what triggers
    -- failover and what happens when it\'s triggered.
    failoverConfig :: FailoverConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failoverConfig', 'routingConfig_failoverConfig' - The failover configuration for an endpoint. This includes what triggers
-- failover and what happens when it\'s triggered.
newRoutingConfig ::
  -- | 'failoverConfig'
  FailoverConfig ->
  RoutingConfig
newRoutingConfig pFailoverConfig_ =
  RoutingConfig' {failoverConfig = pFailoverConfig_}

-- | The failover configuration for an endpoint. This includes what triggers
-- failover and what happens when it\'s triggered.
routingConfig_failoverConfig :: Lens.Lens' RoutingConfig FailoverConfig
routingConfig_failoverConfig = Lens.lens (\RoutingConfig' {failoverConfig} -> failoverConfig) (\s@RoutingConfig' {} a -> s {failoverConfig = a} :: RoutingConfig)

instance Data.FromJSON RoutingConfig where
  parseJSON =
    Data.withObject
      "RoutingConfig"
      ( \x ->
          RoutingConfig'
            Prelude.<$> (x Data..: "FailoverConfig")
      )

instance Prelude.Hashable RoutingConfig where
  hashWithSalt _salt RoutingConfig' {..} =
    _salt `Prelude.hashWithSalt` failoverConfig

instance Prelude.NFData RoutingConfig where
  rnf RoutingConfig' {..} = Prelude.rnf failoverConfig

instance Data.ToJSON RoutingConfig where
  toJSON RoutingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FailoverConfig" Data..= failoverConfig)
          ]
      )
