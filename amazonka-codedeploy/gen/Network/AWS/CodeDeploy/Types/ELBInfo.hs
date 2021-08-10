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
-- Module      : Network.AWS.CodeDeploy.Types.ELBInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ELBInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a load balancer in Elastic Load Balancing to use in a
-- deployment. Instances are registered directly with a load balancer, and
-- traffic is routed to the load balancer.
--
-- /See:/ 'newELBInfo' smart constructor.
data ELBInfo = ELBInfo'
  { -- | For blue\/green deployments, the name of the load balancer that is used
    -- to route traffic from original instances to replacement instances in a
    -- blue\/green deployment. For in-place deployments, the name of the load
    -- balancer that instances are deregistered from so they are not serving
    -- traffic during a deployment, and then re-registered with after the
    -- deployment is complete.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ELBInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eLBInfo_name' - For blue\/green deployments, the name of the load balancer that is used
-- to route traffic from original instances to replacement instances in a
-- blue\/green deployment. For in-place deployments, the name of the load
-- balancer that instances are deregistered from so they are not serving
-- traffic during a deployment, and then re-registered with after the
-- deployment is complete.
newELBInfo ::
  ELBInfo
newELBInfo = ELBInfo' {name = Prelude.Nothing}

-- | For blue\/green deployments, the name of the load balancer that is used
-- to route traffic from original instances to replacement instances in a
-- blue\/green deployment. For in-place deployments, the name of the load
-- balancer that instances are deregistered from so they are not serving
-- traffic during a deployment, and then re-registered with after the
-- deployment is complete.
eLBInfo_name :: Lens.Lens' ELBInfo (Prelude.Maybe Prelude.Text)
eLBInfo_name = Lens.lens (\ELBInfo' {name} -> name) (\s@ELBInfo' {} a -> s {name = a} :: ELBInfo)

instance Core.FromJSON ELBInfo where
  parseJSON =
    Core.withObject
      "ELBInfo"
      (\x -> ELBInfo' Prelude.<$> (x Core..:? "name"))

instance Prelude.Hashable ELBInfo

instance Prelude.NFData ELBInfo

instance Core.ToJSON ELBInfo where
  toJSON ELBInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [("name" Core..=) Prelude.<$> name]
      )
