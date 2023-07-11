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
-- Module      : Amazonka.EC2.Types.AnalysisLoadBalancerListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AnalysisLoadBalancerListener where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a load balancer listener.
--
-- /See:/ 'newAnalysisLoadBalancerListener' smart constructor.
data AnalysisLoadBalancerListener = AnalysisLoadBalancerListener'
  { -- | [Classic Load Balancers] The back-end port for the listener.
    instancePort :: Prelude.Maybe Prelude.Natural,
    -- | The port on which the load balancer is listening.
    loadBalancerPort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisLoadBalancerListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instancePort', 'analysisLoadBalancerListener_instancePort' - [Classic Load Balancers] The back-end port for the listener.
--
-- 'loadBalancerPort', 'analysisLoadBalancerListener_loadBalancerPort' - The port on which the load balancer is listening.
newAnalysisLoadBalancerListener ::
  AnalysisLoadBalancerListener
newAnalysisLoadBalancerListener =
  AnalysisLoadBalancerListener'
    { instancePort =
        Prelude.Nothing,
      loadBalancerPort = Prelude.Nothing
    }

-- | [Classic Load Balancers] The back-end port for the listener.
analysisLoadBalancerListener_instancePort :: Lens.Lens' AnalysisLoadBalancerListener (Prelude.Maybe Prelude.Natural)
analysisLoadBalancerListener_instancePort = Lens.lens (\AnalysisLoadBalancerListener' {instancePort} -> instancePort) (\s@AnalysisLoadBalancerListener' {} a -> s {instancePort = a} :: AnalysisLoadBalancerListener)

-- | The port on which the load balancer is listening.
analysisLoadBalancerListener_loadBalancerPort :: Lens.Lens' AnalysisLoadBalancerListener (Prelude.Maybe Prelude.Natural)
analysisLoadBalancerListener_loadBalancerPort = Lens.lens (\AnalysisLoadBalancerListener' {loadBalancerPort} -> loadBalancerPort) (\s@AnalysisLoadBalancerListener' {} a -> s {loadBalancerPort = a} :: AnalysisLoadBalancerListener)

instance Data.FromXML AnalysisLoadBalancerListener where
  parseXML x =
    AnalysisLoadBalancerListener'
      Prelude.<$> (x Data..@? "instancePort")
      Prelude.<*> (x Data..@? "loadBalancerPort")

instance
  Prelude.Hashable
    AnalysisLoadBalancerListener
  where
  hashWithSalt _salt AnalysisLoadBalancerListener' {..} =
    _salt
      `Prelude.hashWithSalt` instancePort
      `Prelude.hashWithSalt` loadBalancerPort

instance Prelude.NFData AnalysisLoadBalancerListener where
  rnf AnalysisLoadBalancerListener' {..} =
    Prelude.rnf instancePort
      `Prelude.seq` Prelude.rnf loadBalancerPort
