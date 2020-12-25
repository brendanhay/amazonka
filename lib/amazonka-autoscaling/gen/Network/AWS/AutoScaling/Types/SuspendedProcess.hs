{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.SuspendedProcess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.SuspendedProcess
  ( SuspendedProcess (..),

    -- * Smart constructor
    mkSuspendedProcess,

    -- * Lenses
    spProcessName,
    spSuspensionReason,
  )
where

import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an auto scaling process that has been suspended.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html#process-types Scaling processes> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkSuspendedProcess' smart constructor.
data SuspendedProcess = SuspendedProcess'
  { -- | The name of the suspended process.
    processName :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The reason that the process was suspended.
    suspensionReason :: Core.Maybe Types.XmlStringMaxLen255
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SuspendedProcess' value with any optional fields omitted.
mkSuspendedProcess ::
  SuspendedProcess
mkSuspendedProcess =
  SuspendedProcess'
    { processName = Core.Nothing,
      suspensionReason = Core.Nothing
    }

-- | The name of the suspended process.
--
-- /Note:/ Consider using 'processName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spProcessName :: Lens.Lens' SuspendedProcess (Core.Maybe Types.XmlStringMaxLen255)
spProcessName = Lens.field @"processName"
{-# DEPRECATED spProcessName "Use generic-lens or generic-optics with 'processName' instead." #-}

-- | The reason that the process was suspended.
--
-- /Note:/ Consider using 'suspensionReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSuspensionReason :: Lens.Lens' SuspendedProcess (Core.Maybe Types.XmlStringMaxLen255)
spSuspensionReason = Lens.field @"suspensionReason"
{-# DEPRECATED spSuspensionReason "Use generic-lens or generic-optics with 'suspensionReason' instead." #-}

instance Core.FromXML SuspendedProcess where
  parseXML x =
    SuspendedProcess'
      Core.<$> (x Core..@? "ProcessName") Core.<*> (x Core..@? "SuspensionReason")
