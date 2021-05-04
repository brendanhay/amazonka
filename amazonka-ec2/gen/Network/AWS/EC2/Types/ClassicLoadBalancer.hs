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
-- Module      : Network.AWS.EC2.Types.ClassicLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLoadBalancer where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Classic Load Balancer.
--
-- /See:/ 'newClassicLoadBalancer' smart constructor.
data ClassicLoadBalancer = ClassicLoadBalancer'
  { -- | The name of the load balancer.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML ClassicLoadBalancer where
  parseXML x =
    ClassicLoadBalancer'
      Prelude.<$> (x Prelude..@? "name")

instance Prelude.Hashable ClassicLoadBalancer

instance Prelude.NFData ClassicLoadBalancer

instance Prelude.ToQuery ClassicLoadBalancer where
  toQuery ClassicLoadBalancer' {..} =
    Prelude.mconcat ["Name" Prelude.=: name]
