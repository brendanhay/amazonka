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
-- Module      : Network.AWS.EC2.Types.ClassicLoadBalancersConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLoadBalancersConfig where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClassicLoadBalancer
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Classic Load Balancers to attach to a Spot Fleet. Spot
-- Fleet registers the running Spot Instances with these Classic Load
-- Balancers.
--
-- /See:/ 'newClassicLoadBalancersConfig' smart constructor.
data ClassicLoadBalancersConfig = ClassicLoadBalancersConfig'
  { -- | One or more Classic Load Balancers.
    classicLoadBalancers :: Prelude.Maybe (Prelude.NonEmpty ClassicLoadBalancer)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | One or more Classic Load Balancers.
classicLoadBalancersConfig_classicLoadBalancers :: Lens.Lens' ClassicLoadBalancersConfig (Prelude.Maybe (Prelude.NonEmpty ClassicLoadBalancer))
classicLoadBalancersConfig_classicLoadBalancers = Lens.lens (\ClassicLoadBalancersConfig' {classicLoadBalancers} -> classicLoadBalancers) (\s@ClassicLoadBalancersConfig' {} a -> s {classicLoadBalancers = a} :: ClassicLoadBalancersConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML ClassicLoadBalancersConfig where
  parseXML x =
    ClassicLoadBalancersConfig'
      Prelude.<$> ( x Prelude..@? "classicLoadBalancers"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList1 "item")
                  )

instance Prelude.Hashable ClassicLoadBalancersConfig

instance Prelude.NFData ClassicLoadBalancersConfig

instance Prelude.ToQuery ClassicLoadBalancersConfig where
  toQuery ClassicLoadBalancersConfig' {..} =
    Prelude.mconcat
      [ Prelude.toQuery
          ( Prelude.toQueryList "ClassicLoadBalancers"
              Prelude.<$> classicLoadBalancers
          )
      ]
