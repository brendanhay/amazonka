{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ProcessType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.ProcessType
  ( ProcessType (..)
  -- * Smart constructor
  , mkProcessType
  -- * Lenses
  , ptProcessName
  ) where

import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a process type.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html#process-types Scaling processes> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkProcessType' smart constructor.
newtype ProcessType = ProcessType'
  { processName :: Types.XmlStringMaxLen255
    -- ^ One of the following processes:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessType' value with any optional fields omitted.
mkProcessType
    :: Types.XmlStringMaxLen255 -- ^ 'processName'
    -> ProcessType
mkProcessType processName = ProcessType'{processName}

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
ptProcessName :: Lens.Lens' ProcessType Types.XmlStringMaxLen255
ptProcessName = Lens.field @"processName"
{-# INLINEABLE ptProcessName #-}
{-# DEPRECATED processName "Use generic-lens or generic-optics with 'processName' instead"  #-}

instance Core.FromXML ProcessType where
        parseXML x = ProcessType' Core.<$> (x Core..@ "ProcessName")
