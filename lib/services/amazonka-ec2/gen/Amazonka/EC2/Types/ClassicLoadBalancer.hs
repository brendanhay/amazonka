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
-- Module      : Amazonka.EC2.Types.ClassicLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClassicLoadBalancer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a Classic Load Balancer.
--
-- /See:/ 'newClassicLoadBalancer' smart constructor.
data ClassicLoadBalancer = ClassicLoadBalancer'
  { -- | The name of the load balancer.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassicLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'classicLoadBalancer_name' - The name of the load balancer.
newClassicLoadBalancer ::
  ClassicLoadBalancer
newClassicLoadBalancer =
  ClassicLoadBalancer' {name = Prelude.Nothing}

-- | The name of the load balancer.
classicLoadBalancer_name :: Lens.Lens' ClassicLoadBalancer (Prelude.Maybe Prelude.Text)
classicLoadBalancer_name = Lens.lens (\ClassicLoadBalancer' {name} -> name) (\s@ClassicLoadBalancer' {} a -> s {name = a} :: ClassicLoadBalancer)

instance Data.FromXML ClassicLoadBalancer where
  parseXML x =
    ClassicLoadBalancer'
      Prelude.<$> (x Data..@? "name")

instance Prelude.Hashable ClassicLoadBalancer where
  hashWithSalt _salt ClassicLoadBalancer' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData ClassicLoadBalancer where
  rnf ClassicLoadBalancer' {..} = Prelude.rnf name

instance Data.ToQuery ClassicLoadBalancer where
  toQuery ClassicLoadBalancer' {..} =
    Prelude.mconcat ["Name" Data.=: name]
