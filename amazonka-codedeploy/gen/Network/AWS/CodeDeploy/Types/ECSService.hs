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
-- Module      : Network.AWS.CodeDeploy.Types.ECSService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ECSService where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the service and cluster names used to identify an Amazon ECS
-- deployment\'s target.
--
-- /See:/ 'newECSService' smart constructor.
data ECSService = ECSService'
  { -- | The name of the target Amazon ECS service.
    serviceName :: Core.Maybe Core.Text,
    -- | The name of the cluster that the Amazon ECS service is associated with.
    clusterName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ECSService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'eCSService_serviceName' - The name of the target Amazon ECS service.
--
-- 'clusterName', 'eCSService_clusterName' - The name of the cluster that the Amazon ECS service is associated with.
newECSService ::
  ECSService
newECSService =
  ECSService'
    { serviceName = Core.Nothing,
      clusterName = Core.Nothing
    }

-- | The name of the target Amazon ECS service.
eCSService_serviceName :: Lens.Lens' ECSService (Core.Maybe Core.Text)
eCSService_serviceName = Lens.lens (\ECSService' {serviceName} -> serviceName) (\s@ECSService' {} a -> s {serviceName = a} :: ECSService)

-- | The name of the cluster that the Amazon ECS service is associated with.
eCSService_clusterName :: Lens.Lens' ECSService (Core.Maybe Core.Text)
eCSService_clusterName = Lens.lens (\ECSService' {clusterName} -> clusterName) (\s@ECSService' {} a -> s {clusterName = a} :: ECSService)

instance Core.FromJSON ECSService where
  parseJSON =
    Core.withObject
      "ECSService"
      ( \x ->
          ECSService'
            Core.<$> (x Core..:? "serviceName")
            Core.<*> (x Core..:? "clusterName")
      )

instance Core.Hashable ECSService

instance Core.NFData ECSService

instance Core.ToJSON ECSService where
  toJSON ECSService' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serviceName" Core..=) Core.<$> serviceName,
            ("clusterName" Core..=) Core.<$> clusterName
          ]
      )
