{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ProcessType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ProcessType
  ( ProcessType (..),

    -- * Smart constructor
    mkProcessType,

    -- * Lenses
    ptProcessName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a process type.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html#process-types Scaling processes> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkProcessType' smart constructor.
newtype ProcessType = ProcessType'
  { -- | One of the following processes:
    --
    --
    --     * @Launch@
    --
    --
    --     * @Terminate@
    --
    --
    --     * @AddToLoadBalancer@
    --
    --
    --     * @AlarmNotification@
    --
    --
    --     * @AZRebalance@
    --
    --
    --     * @HealthCheck@
    --
    --
    --     * @InstanceRefresh@
    --
    --
    --     * @ReplaceUnhealthy@
    --
    --
    --     * @ScheduledActions@
    processName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessType' with the minimum fields required to make a request.
--
-- * 'processName' - One of the following processes:
--
--
--     * @Launch@
--
--
--     * @Terminate@
--
--
--     * @AddToLoadBalancer@
--
--
--     * @AlarmNotification@
--
--
--     * @AZRebalance@
--
--
--     * @HealthCheck@
--
--
--     * @InstanceRefresh@
--
--
--     * @ReplaceUnhealthy@
--
--
--     * @ScheduledActions@
mkProcessType ::
  -- | 'processName'
  Lude.Text ->
  ProcessType
mkProcessType pProcessName_ =
  ProcessType' {processName = pProcessName_}

-- | One of the following processes:
--
--
--     * @Launch@
--
--
--     * @Terminate@
--
--
--     * @AddToLoadBalancer@
--
--
--     * @AlarmNotification@
--
--
--     * @AZRebalance@
--
--
--     * @HealthCheck@
--
--
--     * @InstanceRefresh@
--
--
--     * @ReplaceUnhealthy@
--
--
--     * @ScheduledActions@
--
--
--
-- /Note:/ Consider using 'processName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptProcessName :: Lens.Lens' ProcessType Lude.Text
ptProcessName = Lens.lens (processName :: ProcessType -> Lude.Text) (\s a -> s {processName = a} :: ProcessType)
{-# DEPRECATED ptProcessName "Use generic-lens or generic-optics with 'processName' instead." #-}

instance Lude.FromXML ProcessType where
  parseXML x = ProcessType' Lude.<$> (x Lude..@ "ProcessName")
