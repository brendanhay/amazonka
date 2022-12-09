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
-- Module      : Amazonka.ELBV2.Types.Limit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.Limit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Elastic Load Balancing resource limit for your
-- Amazon Web Services account.
--
-- /See:/ 'newLimit' smart constructor.
data Limit = Limit'
  { -- | The maximum value of the limit.
    max :: Prelude.Maybe Prelude.Text,
    -- | The name of the limit. The possible values are:
    --
    -- -   application-load-balancers
    --
    -- -   condition-values-per-alb-rule
    --
    -- -   condition-wildcards-per-alb-rule
    --
    -- -   gateway-load-balancers
    --
    -- -   gateway-load-balancers-per-vpc
    --
    -- -   geneve-target-groups
    --
    -- -   listeners-per-application-load-balancer
    --
    -- -   listeners-per-network-load-balancer
    --
    -- -   network-load-balancers
    --
    -- -   rules-per-application-load-balancer
    --
    -- -   target-groups
    --
    -- -   target-groups-per-action-on-application-load-balancer
    --
    -- -   target-groups-per-action-on-network-load-balancer
    --
    -- -   target-groups-per-application-load-balancer
    --
    -- -   targets-per-application-load-balancer
    --
    -- -   targets-per-availability-zone-per-gateway-load-balancer
    --
    -- -   targets-per-availability-zone-per-network-load-balancer
    --
    -- -   targets-per-network-load-balancer
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Limit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'limit_max' - The maximum value of the limit.
--
-- 'name', 'limit_name' - The name of the limit. The possible values are:
--
-- -   application-load-balancers
--
-- -   condition-values-per-alb-rule
--
-- -   condition-wildcards-per-alb-rule
--
-- -   gateway-load-balancers
--
-- -   gateway-load-balancers-per-vpc
--
-- -   geneve-target-groups
--
-- -   listeners-per-application-load-balancer
--
-- -   listeners-per-network-load-balancer
--
-- -   network-load-balancers
--
-- -   rules-per-application-load-balancer
--
-- -   target-groups
--
-- -   target-groups-per-action-on-application-load-balancer
--
-- -   target-groups-per-action-on-network-load-balancer
--
-- -   target-groups-per-application-load-balancer
--
-- -   targets-per-application-load-balancer
--
-- -   targets-per-availability-zone-per-gateway-load-balancer
--
-- -   targets-per-availability-zone-per-network-load-balancer
--
-- -   targets-per-network-load-balancer
newLimit ::
  Limit
newLimit =
  Limit'
    { max = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The maximum value of the limit.
limit_max :: Lens.Lens' Limit (Prelude.Maybe Prelude.Text)
limit_max = Lens.lens (\Limit' {max} -> max) (\s@Limit' {} a -> s {max = a} :: Limit)

-- | The name of the limit. The possible values are:
--
-- -   application-load-balancers
--
-- -   condition-values-per-alb-rule
--
-- -   condition-wildcards-per-alb-rule
--
-- -   gateway-load-balancers
--
-- -   gateway-load-balancers-per-vpc
--
-- -   geneve-target-groups
--
-- -   listeners-per-application-load-balancer
--
-- -   listeners-per-network-load-balancer
--
-- -   network-load-balancers
--
-- -   rules-per-application-load-balancer
--
-- -   target-groups
--
-- -   target-groups-per-action-on-application-load-balancer
--
-- -   target-groups-per-action-on-network-load-balancer
--
-- -   target-groups-per-application-load-balancer
--
-- -   targets-per-application-load-balancer
--
-- -   targets-per-availability-zone-per-gateway-load-balancer
--
-- -   targets-per-availability-zone-per-network-load-balancer
--
-- -   targets-per-network-load-balancer
limit_name :: Lens.Lens' Limit (Prelude.Maybe Prelude.Text)
limit_name = Lens.lens (\Limit' {name} -> name) (\s@Limit' {} a -> s {name = a} :: Limit)

instance Data.FromXML Limit where
  parseXML x =
    Limit'
      Prelude.<$> (x Data..@? "Max") Prelude.<*> (x Data..@? "Name")

instance Prelude.Hashable Limit where
  hashWithSalt _salt Limit' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` name

instance Prelude.NFData Limit where
  rnf Limit' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf name
