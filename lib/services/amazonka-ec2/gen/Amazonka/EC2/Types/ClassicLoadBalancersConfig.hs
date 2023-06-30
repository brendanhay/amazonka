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
-- Module      : Amazonka.EC2.Types.ClassicLoadBalancersConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClassicLoadBalancersConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClassicLoadBalancer
import qualified Amazonka.Prelude as Prelude

-- | Describes the Classic Load Balancers to attach to a Spot Fleet. Spot
-- Fleet registers the running Spot Instances with these Classic Load
-- Balancers.
--
-- /See:/ 'newClassicLoadBalancersConfig' smart constructor.
data ClassicLoadBalancersConfig = ClassicLoadBalancersConfig'
  { -- | One or more Classic Load Balancers.
    classicLoadBalancers :: Prelude.Maybe (Prelude.NonEmpty ClassicLoadBalancer)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
classicLoadBalancersConfig_classicLoadBalancers = Lens.lens (\ClassicLoadBalancersConfig' {classicLoadBalancers} -> classicLoadBalancers) (\s@ClassicLoadBalancersConfig' {} a -> s {classicLoadBalancers = a} :: ClassicLoadBalancersConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ClassicLoadBalancersConfig where
  parseXML x =
    ClassicLoadBalancersConfig'
      Prelude.<$> ( x
                      Data..@? "classicLoadBalancers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList1 "item")
                  )

instance Prelude.Hashable ClassicLoadBalancersConfig where
  hashWithSalt _salt ClassicLoadBalancersConfig' {..} =
    _salt `Prelude.hashWithSalt` classicLoadBalancers

instance Prelude.NFData ClassicLoadBalancersConfig where
  rnf ClassicLoadBalancersConfig' {..} =
    Prelude.rnf classicLoadBalancers

instance Data.ToQuery ClassicLoadBalancersConfig where
  toQuery ClassicLoadBalancersConfig' {..} =
    Prelude.mconcat
      [ Data.toQuery
          ( Data.toQueryList "ClassicLoadBalancers"
              Prelude.<$> classicLoadBalancers
          )
      ]
