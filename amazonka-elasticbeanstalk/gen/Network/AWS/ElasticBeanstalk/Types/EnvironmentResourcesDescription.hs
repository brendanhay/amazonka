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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
import qualified Network.AWS.Lens as Lens

-- | Describes the AWS resources in use by this environment. This data is not
-- live data.
--
-- /See:/ 'newEnvironmentResourcesDescription' smart constructor.
data EnvironmentResourcesDescription = EnvironmentResourcesDescription'
  { -- | Describes the LoadBalancer.
    loadBalancer :: Core.Maybe LoadBalancerDescription
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnvironmentResourcesDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancer', 'environmentResourcesDescription_loadBalancer' - Describes the LoadBalancer.
newEnvironmentResourcesDescription ::
  EnvironmentResourcesDescription
newEnvironmentResourcesDescription =
  EnvironmentResourcesDescription'
    { loadBalancer =
        Core.Nothing
    }

-- | Describes the LoadBalancer.
environmentResourcesDescription_loadBalancer :: Lens.Lens' EnvironmentResourcesDescription (Core.Maybe LoadBalancerDescription)
environmentResourcesDescription_loadBalancer = Lens.lens (\EnvironmentResourcesDescription' {loadBalancer} -> loadBalancer) (\s@EnvironmentResourcesDescription' {} a -> s {loadBalancer = a} :: EnvironmentResourcesDescription)

instance Core.FromXML EnvironmentResourcesDescription where
  parseXML x =
    EnvironmentResourcesDescription'
      Core.<$> (x Core..@? "LoadBalancer")

instance
  Core.Hashable
    EnvironmentResourcesDescription

instance Core.NFData EnvironmentResourcesDescription
