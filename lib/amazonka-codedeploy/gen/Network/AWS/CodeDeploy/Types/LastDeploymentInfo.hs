{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LastDeploymentInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LastDeploymentInfo
  ( LastDeploymentInfo (..),

    -- * Smart constructor
    mkLastDeploymentInfo,

    -- * Lenses
    ldiStatus,
    ldiDeploymentId,
    ldiEndTime,
    ldiCreateTime,
  )
where

import Network.AWS.CodeDeploy.Types.DeploymentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the most recent attempted or successful deployment to a deployment group.
--
-- /See:/ 'mkLastDeploymentInfo' smart constructor.
data LastDeploymentInfo = LastDeploymentInfo'
  { -- | The status of the most recent deployment.
    status :: Lude.Maybe DeploymentStatus,
    -- | The unique ID of a deployment.
    deploymentId :: Lude.Maybe Lude.Text,
    -- | A timestamp that indicates when the most recent deployment to the deployment group was complete.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | A timestamp that indicates when the most recent deployment to the deployment group started.
    createTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LastDeploymentInfo' with the minimum fields required to make a request.
--
-- * 'status' - The status of the most recent deployment.
-- * 'deploymentId' - The unique ID of a deployment.
-- * 'endTime' - A timestamp that indicates when the most recent deployment to the deployment group was complete.
-- * 'createTime' - A timestamp that indicates when the most recent deployment to the deployment group started.
mkLastDeploymentInfo ::
  LastDeploymentInfo
mkLastDeploymentInfo =
  LastDeploymentInfo'
    { status = Lude.Nothing,
      deploymentId = Lude.Nothing,
      endTime = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | The status of the most recent deployment.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiStatus :: Lens.Lens' LastDeploymentInfo (Lude.Maybe DeploymentStatus)
ldiStatus = Lens.lens (status :: LastDeploymentInfo -> Lude.Maybe DeploymentStatus) (\s a -> s {status = a} :: LastDeploymentInfo)
{-# DEPRECATED ldiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiDeploymentId :: Lens.Lens' LastDeploymentInfo (Lude.Maybe Lude.Text)
ldiDeploymentId = Lens.lens (deploymentId :: LastDeploymentInfo -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: LastDeploymentInfo)
{-# DEPRECATED ldiDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | A timestamp that indicates when the most recent deployment to the deployment group was complete.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiEndTime :: Lens.Lens' LastDeploymentInfo (Lude.Maybe Lude.Timestamp)
ldiEndTime = Lens.lens (endTime :: LastDeploymentInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: LastDeploymentInfo)
{-# DEPRECATED ldiEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | A timestamp that indicates when the most recent deployment to the deployment group started.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiCreateTime :: Lens.Lens' LastDeploymentInfo (Lude.Maybe Lude.Timestamp)
ldiCreateTime = Lens.lens (createTime :: LastDeploymentInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: LastDeploymentInfo)
{-# DEPRECATED ldiCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromJSON LastDeploymentInfo where
  parseJSON =
    Lude.withObject
      "LastDeploymentInfo"
      ( \x ->
          LastDeploymentInfo'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "deploymentId")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "createTime")
      )
