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
-- Module      : Network.AWS.CodeDeploy.Types.LastDeploymentInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LastDeploymentInfo where

import Network.AWS.CodeDeploy.Types.DeploymentStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the most recent attempted or successful deployment to
-- a deployment group.
--
-- /See:/ 'newLastDeploymentInfo' smart constructor.
data LastDeploymentInfo = LastDeploymentInfo'
  { -- | The unique ID of a deployment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The status of the most recent deployment.
    status :: Core.Maybe DeploymentStatus,
    -- | A timestamp that indicates when the most recent deployment to the
    -- deployment group was complete.
    endTime :: Core.Maybe Core.POSIX,
    -- | A timestamp that indicates when the most recent deployment to the
    -- deployment group started.
    createTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LastDeploymentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'lastDeploymentInfo_deploymentId' - The unique ID of a deployment.
--
-- 'status', 'lastDeploymentInfo_status' - The status of the most recent deployment.
--
-- 'endTime', 'lastDeploymentInfo_endTime' - A timestamp that indicates when the most recent deployment to the
-- deployment group was complete.
--
-- 'createTime', 'lastDeploymentInfo_createTime' - A timestamp that indicates when the most recent deployment to the
-- deployment group started.
newLastDeploymentInfo ::
  LastDeploymentInfo
newLastDeploymentInfo =
  LastDeploymentInfo'
    { deploymentId = Core.Nothing,
      status = Core.Nothing,
      endTime = Core.Nothing,
      createTime = Core.Nothing
    }

-- | The unique ID of a deployment.
lastDeploymentInfo_deploymentId :: Lens.Lens' LastDeploymentInfo (Core.Maybe Core.Text)
lastDeploymentInfo_deploymentId = Lens.lens (\LastDeploymentInfo' {deploymentId} -> deploymentId) (\s@LastDeploymentInfo' {} a -> s {deploymentId = a} :: LastDeploymentInfo)

-- | The status of the most recent deployment.
lastDeploymentInfo_status :: Lens.Lens' LastDeploymentInfo (Core.Maybe DeploymentStatus)
lastDeploymentInfo_status = Lens.lens (\LastDeploymentInfo' {status} -> status) (\s@LastDeploymentInfo' {} a -> s {status = a} :: LastDeploymentInfo)

-- | A timestamp that indicates when the most recent deployment to the
-- deployment group was complete.
lastDeploymentInfo_endTime :: Lens.Lens' LastDeploymentInfo (Core.Maybe Core.UTCTime)
lastDeploymentInfo_endTime = Lens.lens (\LastDeploymentInfo' {endTime} -> endTime) (\s@LastDeploymentInfo' {} a -> s {endTime = a} :: LastDeploymentInfo) Core.. Lens.mapping Core._Time

-- | A timestamp that indicates when the most recent deployment to the
-- deployment group started.
lastDeploymentInfo_createTime :: Lens.Lens' LastDeploymentInfo (Core.Maybe Core.UTCTime)
lastDeploymentInfo_createTime = Lens.lens (\LastDeploymentInfo' {createTime} -> createTime) (\s@LastDeploymentInfo' {} a -> s {createTime = a} :: LastDeploymentInfo) Core.. Lens.mapping Core._Time

instance Core.FromJSON LastDeploymentInfo where
  parseJSON =
    Core.withObject
      "LastDeploymentInfo"
      ( \x ->
          LastDeploymentInfo'
            Core.<$> (x Core..:? "deploymentId")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "endTime")
            Core.<*> (x Core..:? "createTime")
      )

instance Core.Hashable LastDeploymentInfo

instance Core.NFData LastDeploymentInfo
