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
-- Module      : Network.AWS.EC2.Types.AnalysisLoadBalancerTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AnalysisLoadBalancerTarget where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AnalysisComponent
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a load balancer target.
--
-- /See:/ 'newAnalysisLoadBalancerTarget' smart constructor.
data AnalysisLoadBalancerTarget = AnalysisLoadBalancerTarget'
  { -- | The IP address.
    address :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Information about the instance.
    instance' :: Prelude.Maybe AnalysisComponent,
    -- | The port on which the target is listening.
    port :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AnalysisLoadBalancerTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'analysisLoadBalancerTarget_address' - The IP address.
--
-- 'availabilityZone', 'analysisLoadBalancerTarget_availabilityZone' - The Availability Zone.
--
-- 'instance'', 'analysisLoadBalancerTarget_instance' - Information about the instance.
--
-- 'port', 'analysisLoadBalancerTarget_port' - The port on which the target is listening.
newAnalysisLoadBalancerTarget ::
  AnalysisLoadBalancerTarget
newAnalysisLoadBalancerTarget =
  AnalysisLoadBalancerTarget'
    { address =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instance' = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The IP address.
analysisLoadBalancerTarget_address :: Lens.Lens' AnalysisLoadBalancerTarget (Prelude.Maybe Prelude.Text)
analysisLoadBalancerTarget_address = Lens.lens (\AnalysisLoadBalancerTarget' {address} -> address) (\s@AnalysisLoadBalancerTarget' {} a -> s {address = a} :: AnalysisLoadBalancerTarget)

-- | The Availability Zone.
analysisLoadBalancerTarget_availabilityZone :: Lens.Lens' AnalysisLoadBalancerTarget (Prelude.Maybe Prelude.Text)
analysisLoadBalancerTarget_availabilityZone = Lens.lens (\AnalysisLoadBalancerTarget' {availabilityZone} -> availabilityZone) (\s@AnalysisLoadBalancerTarget' {} a -> s {availabilityZone = a} :: AnalysisLoadBalancerTarget)

-- | Information about the instance.
analysisLoadBalancerTarget_instance :: Lens.Lens' AnalysisLoadBalancerTarget (Prelude.Maybe AnalysisComponent)
analysisLoadBalancerTarget_instance = Lens.lens (\AnalysisLoadBalancerTarget' {instance'} -> instance') (\s@AnalysisLoadBalancerTarget' {} a -> s {instance' = a} :: AnalysisLoadBalancerTarget)

-- | The port on which the target is listening.
analysisLoadBalancerTarget_port :: Lens.Lens' AnalysisLoadBalancerTarget (Prelude.Maybe Prelude.Natural)
analysisLoadBalancerTarget_port = Lens.lens (\AnalysisLoadBalancerTarget' {port} -> port) (\s@AnalysisLoadBalancerTarget' {} a -> s {port = a} :: AnalysisLoadBalancerTarget)

instance Prelude.FromXML AnalysisLoadBalancerTarget where
  parseXML x =
    AnalysisLoadBalancerTarget'
      Prelude.<$> (x Prelude..@? "address")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "instance")
      Prelude.<*> (x Prelude..@? "port")

instance Prelude.Hashable AnalysisLoadBalancerTarget

instance Prelude.NFData AnalysisLoadBalancerTarget
