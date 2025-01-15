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
-- Module      : Amazonka.CloudWatchEvents.Types.FailoverConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.FailoverConfig where

import Amazonka.CloudWatchEvents.Types.Primary
import Amazonka.CloudWatchEvents.Types.Secondary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The failover configuration for an endpoint. This includes what triggers
-- failover and what happens when it\'s triggered.
--
-- /See:/ 'newFailoverConfig' smart constructor.
data FailoverConfig = FailoverConfig'
  { -- | The main Region of the endpoint.
    primary :: Primary,
    -- | The Region that events are routed to when failover is triggered or event
    -- replication is enabled.
    secondary :: Secondary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primary', 'failoverConfig_primary' - The main Region of the endpoint.
--
-- 'secondary', 'failoverConfig_secondary' - The Region that events are routed to when failover is triggered or event
-- replication is enabled.
newFailoverConfig ::
  -- | 'primary'
  Primary ->
  -- | 'secondary'
  Secondary ->
  FailoverConfig
newFailoverConfig pPrimary_ pSecondary_ =
  FailoverConfig'
    { primary = pPrimary_,
      secondary = pSecondary_
    }

-- | The main Region of the endpoint.
failoverConfig_primary :: Lens.Lens' FailoverConfig Primary
failoverConfig_primary = Lens.lens (\FailoverConfig' {primary} -> primary) (\s@FailoverConfig' {} a -> s {primary = a} :: FailoverConfig)

-- | The Region that events are routed to when failover is triggered or event
-- replication is enabled.
failoverConfig_secondary :: Lens.Lens' FailoverConfig Secondary
failoverConfig_secondary = Lens.lens (\FailoverConfig' {secondary} -> secondary) (\s@FailoverConfig' {} a -> s {secondary = a} :: FailoverConfig)

instance Data.FromJSON FailoverConfig where
  parseJSON =
    Data.withObject
      "FailoverConfig"
      ( \x ->
          FailoverConfig'
            Prelude.<$> (x Data..: "Primary")
            Prelude.<*> (x Data..: "Secondary")
      )

instance Prelude.Hashable FailoverConfig where
  hashWithSalt _salt FailoverConfig' {..} =
    _salt
      `Prelude.hashWithSalt` primary
      `Prelude.hashWithSalt` secondary

instance Prelude.NFData FailoverConfig where
  rnf FailoverConfig' {..} =
    Prelude.rnf primary `Prelude.seq`
      Prelude.rnf secondary

instance Data.ToJSON FailoverConfig where
  toJSON FailoverConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Primary" Data..= primary),
            Prelude.Just ("Secondary" Data..= secondary)
          ]
      )
