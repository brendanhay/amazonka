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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an auto scaling process that has been suspended.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html#process-types Scaling processes> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkSuspendedProcess' smart constructor.
data SuspendedProcess = SuspendedProcess'
  { processName ::
      Lude.Maybe Lude.Text,
    suspensionReason :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuspendedProcess' with the minimum fields required to make a request.
--
-- * 'processName' - The name of the suspended process.
-- * 'suspensionReason' - The reason that the process was suspended.
mkSuspendedProcess ::
  SuspendedProcess
mkSuspendedProcess =
  SuspendedProcess'
    { processName = Lude.Nothing,
      suspensionReason = Lude.Nothing
    }

-- | The name of the suspended process.
--
-- /Note:/ Consider using 'processName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spProcessName :: Lens.Lens' SuspendedProcess (Lude.Maybe Lude.Text)
spProcessName = Lens.lens (processName :: SuspendedProcess -> Lude.Maybe Lude.Text) (\s a -> s {processName = a} :: SuspendedProcess)
{-# DEPRECATED spProcessName "Use generic-lens or generic-optics with 'processName' instead." #-}

-- | The reason that the process was suspended.
--
-- /Note:/ Consider using 'suspensionReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSuspensionReason :: Lens.Lens' SuspendedProcess (Lude.Maybe Lude.Text)
spSuspensionReason = Lens.lens (suspensionReason :: SuspendedProcess -> Lude.Maybe Lude.Text) (\s a -> s {suspensionReason = a} :: SuspendedProcess)
{-# DEPRECATED spSuspensionReason "Use generic-lens or generic-optics with 'suspensionReason' instead." #-}

instance Lude.FromXML SuspendedProcess where
  parseXML x =
    SuspendedProcess'
      Lude.<$> (x Lude..@? "ProcessName") Lude.<*> (x Lude..@? "SuspensionReason")
