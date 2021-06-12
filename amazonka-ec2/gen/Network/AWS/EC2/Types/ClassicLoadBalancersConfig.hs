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
-- Module      : Network.AWS.EC2.Types.ClassicLoadBalancersConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLoadBalancersConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClassicLoadBalancer
import qualified Network.AWS.Lens as Lens

-- | Describes the Classic Load Balancers to attach to a Spot Fleet. Spot
-- Fleet registers the running Spot Instances with these Classic Load
-- Balancers.
--
-- /See:/ 'newClassicLoadBalancersConfig' smart constructor.
data ClassicLoadBalancersConfig = ClassicLoadBalancersConfig'
  { -- | One or more Classic Load Balancers.
    classicLoadBalancers :: Core.Maybe (Core.NonEmpty ClassicLoadBalancer)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClassicLoadBalancersConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classicLoadBalancers', 'classicLoadBalancersConfig_classicLoadBalancers' - One or more Classic Load Balancers.
newClassicLoadBalancersConfig ::
  ClassicLoadBalancersConfig
newClassicLoadBalancersConfig =
  ClassicLoadBalancersConfig'
    { classicLoadBalancers =
        Core.Nothing
    }

-- | One or more Classic Load Balancers.
classicLoadBalancersConfig_classicLoadBalancers :: Lens.Lens' ClassicLoadBalancersConfig (Core.Maybe (Core.NonEmpty ClassicLoadBalancer))
classicLoadBalancersConfig_classicLoadBalancers = Lens.lens (\ClassicLoadBalancersConfig' {classicLoadBalancers} -> classicLoadBalancers) (\s@ClassicLoadBalancersConfig' {} a -> s {classicLoadBalancers = a} :: ClassicLoadBalancersConfig) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML ClassicLoadBalancersConfig where
  parseXML x =
    ClassicLoadBalancersConfig'
      Core.<$> ( x Core..@? "classicLoadBalancers"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList1 "item")
               )

instance Core.Hashable ClassicLoadBalancersConfig

instance Core.NFData ClassicLoadBalancersConfig

instance Core.ToQuery ClassicLoadBalancersConfig where
  toQuery ClassicLoadBalancersConfig' {..} =
    Core.mconcat
      [ Core.toQuery
          ( Core.toQueryList "ClassicLoadBalancers"
              Core.<$> classicLoadBalancers
          )
      ]
