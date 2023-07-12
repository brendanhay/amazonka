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
-- Module      : Amazonka.SecurityHub.Types.AwsElbLoadBalancerConnectionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElbLoadBalancerConnectionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains connection settings for the load balancer.
--
-- /See:/ 'newAwsElbLoadBalancerConnectionSettings' smart constructor.
data AwsElbLoadBalancerConnectionSettings = AwsElbLoadBalancerConnectionSettings'
  { -- | The time, in seconds, that the connection can be idle (no data is sent
    -- over the connection) before it is closed by the load balancer.
    idleTimeout :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElbLoadBalancerConnectionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idleTimeout', 'awsElbLoadBalancerConnectionSettings_idleTimeout' - The time, in seconds, that the connection can be idle (no data is sent
-- over the connection) before it is closed by the load balancer.
newAwsElbLoadBalancerConnectionSettings ::
  AwsElbLoadBalancerConnectionSettings
newAwsElbLoadBalancerConnectionSettings =
  AwsElbLoadBalancerConnectionSettings'
    { idleTimeout =
        Prelude.Nothing
    }

-- | The time, in seconds, that the connection can be idle (no data is sent
-- over the connection) before it is closed by the load balancer.
awsElbLoadBalancerConnectionSettings_idleTimeout :: Lens.Lens' AwsElbLoadBalancerConnectionSettings (Prelude.Maybe Prelude.Int)
awsElbLoadBalancerConnectionSettings_idleTimeout = Lens.lens (\AwsElbLoadBalancerConnectionSettings' {idleTimeout} -> idleTimeout) (\s@AwsElbLoadBalancerConnectionSettings' {} a -> s {idleTimeout = a} :: AwsElbLoadBalancerConnectionSettings)

instance
  Data.FromJSON
    AwsElbLoadBalancerConnectionSettings
  where
  parseJSON =
    Data.withObject
      "AwsElbLoadBalancerConnectionSettings"
      ( \x ->
          AwsElbLoadBalancerConnectionSettings'
            Prelude.<$> (x Data..:? "IdleTimeout")
      )

instance
  Prelude.Hashable
    AwsElbLoadBalancerConnectionSettings
  where
  hashWithSalt
    _salt
    AwsElbLoadBalancerConnectionSettings' {..} =
      _salt `Prelude.hashWithSalt` idleTimeout

instance
  Prelude.NFData
    AwsElbLoadBalancerConnectionSettings
  where
  rnf AwsElbLoadBalancerConnectionSettings' {..} =
    Prelude.rnf idleTimeout

instance
  Data.ToJSON
    AwsElbLoadBalancerConnectionSettings
  where
  toJSON AwsElbLoadBalancerConnectionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("IdleTimeout" Data..=) Prelude.<$> idleTimeout]
      )
