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
-- Module      : Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.Listener
import qualified Network.AWS.Lens as Lens

-- | Describes the details of a LoadBalancer.
--
-- /See:/ 'newLoadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { -- | The domain name of the LoadBalancer.
    domain :: Core.Maybe Core.Text,
    -- | A list of Listeners used by the LoadBalancer.
    listeners :: Core.Maybe [Listener],
    -- | The name of the LoadBalancer.
    loadBalancerName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoadBalancerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'loadBalancerDescription_domain' - The domain name of the LoadBalancer.
--
-- 'listeners', 'loadBalancerDescription_listeners' - A list of Listeners used by the LoadBalancer.
--
-- 'loadBalancerName', 'loadBalancerDescription_loadBalancerName' - The name of the LoadBalancer.
newLoadBalancerDescription ::
  LoadBalancerDescription
newLoadBalancerDescription =
  LoadBalancerDescription'
    { domain = Core.Nothing,
      listeners = Core.Nothing,
      loadBalancerName = Core.Nothing
    }

-- | The domain name of the LoadBalancer.
loadBalancerDescription_domain :: Lens.Lens' LoadBalancerDescription (Core.Maybe Core.Text)
loadBalancerDescription_domain = Lens.lens (\LoadBalancerDescription' {domain} -> domain) (\s@LoadBalancerDescription' {} a -> s {domain = a} :: LoadBalancerDescription)

-- | A list of Listeners used by the LoadBalancer.
loadBalancerDescription_listeners :: Lens.Lens' LoadBalancerDescription (Core.Maybe [Listener])
loadBalancerDescription_listeners = Lens.lens (\LoadBalancerDescription' {listeners} -> listeners) (\s@LoadBalancerDescription' {} a -> s {listeners = a} :: LoadBalancerDescription) Core.. Lens.mapping Lens._Coerce

-- | The name of the LoadBalancer.
loadBalancerDescription_loadBalancerName :: Lens.Lens' LoadBalancerDescription (Core.Maybe Core.Text)
loadBalancerDescription_loadBalancerName = Lens.lens (\LoadBalancerDescription' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancerDescription' {} a -> s {loadBalancerName = a} :: LoadBalancerDescription)

instance Core.FromXML LoadBalancerDescription where
  parseXML x =
    LoadBalancerDescription'
      Core.<$> (x Core..@? "Domain")
      Core.<*> ( x Core..@? "Listeners" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "LoadBalancerName")

instance Core.Hashable LoadBalancerDescription

instance Core.NFData LoadBalancerDescription
