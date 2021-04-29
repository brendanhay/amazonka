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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription where

import Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the AWS resources in use by this environment. This data is not
-- live data.
--
-- /See:/ 'newEnvironmentResourcesDescription' smart constructor.
data EnvironmentResourcesDescription = EnvironmentResourcesDescription'
  { -- | Describes the LoadBalancer.
    loadBalancer :: Prelude.Maybe LoadBalancerDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Describes the LoadBalancer.
environmentResourcesDescription_loadBalancer :: Lens.Lens' EnvironmentResourcesDescription (Prelude.Maybe LoadBalancerDescription)
environmentResourcesDescription_loadBalancer = Lens.lens (\EnvironmentResourcesDescription' {loadBalancer} -> loadBalancer) (\s@EnvironmentResourcesDescription' {} a -> s {loadBalancer = a} :: EnvironmentResourcesDescription)

instance
  Prelude.FromXML
    EnvironmentResourcesDescription
  where
  parseXML x =
    EnvironmentResourcesDescription'
      Prelude.<$> (x Prelude..@? "LoadBalancer")

instance
  Prelude.Hashable
    EnvironmentResourcesDescription

instance
  Prelude.NFData
    EnvironmentResourcesDescription
