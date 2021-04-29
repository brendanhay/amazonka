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
-- Module      : Network.AWS.ELBv2.Types.Limit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Limit where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an Elastic Load Balancing resource limit for your AWS
-- account.
--
-- /See:/ 'newLimit' smart constructor.
data Limit = Limit'
  { -- | The name of the limit. The possible values are:
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
    name :: Prelude.Maybe Prelude.Text,
    -- | The maximum value of the limit.
    max :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Limit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'max', 'limit_max' - The maximum value of the limit.
newLimit ::
  Limit
newLimit =
  Limit'
    { name = Prelude.Nothing,
      max = Prelude.Nothing
    }

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

-- | The maximum value of the limit.
limit_max :: Lens.Lens' Limit (Prelude.Maybe Prelude.Text)
limit_max = Lens.lens (\Limit' {max} -> max) (\s@Limit' {} a -> s {max = a} :: Limit)

instance Prelude.FromXML Limit where
  parseXML x =
    Limit'
      Prelude.<$> (x Prelude..@? "Name")
      Prelude.<*> (x Prelude..@? "Max")

instance Prelude.Hashable Limit

instance Prelude.NFData Limit
