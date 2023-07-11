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
-- Module      : Amazonka.CodeDeploy.Types.ELBInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.ELBInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON ELBInfo where
  parseJSON =
    Data.withObject
      "ELBInfo"
      (\x -> ELBInfo' Prelude.<$> (x Data..:? "name"))

instance Prelude.Hashable ELBInfo where
  hashWithSalt _salt ELBInfo' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData ELBInfo where
  rnf ELBInfo' {..} = Prelude.rnf name

instance Data.ToJSON ELBInfo where
  toJSON ELBInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [("name" Data..=) Prelude.<$> name]
      )
