{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AlertTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AlertTarget
  ( AlertTarget (..),

    -- * Smart constructor
    mkAlertTarget,

    -- * Lenses
    atAlertTargetArn,
    atRoleArn,
  )
where

import qualified Network.AWS.IoT.Types.AlertTargetArn as Types
import qualified Network.AWS.IoT.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure containing the alert target ARN and the role ARN.
--
-- /See:/ 'mkAlertTarget' smart constructor.
data AlertTarget = AlertTarget'
  { -- | The ARN of the notification target to which alerts are sent.
    alertTargetArn :: Types.AlertTargetArn,
    -- | The ARN of the role that grants permission to send alerts to the notification target.
    roleArn :: Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AlertTarget' value with any optional fields omitted.
mkAlertTarget ::
  -- | 'alertTargetArn'
  Types.AlertTargetArn ->
  -- | 'roleArn'
  Types.RoleArn ->
  AlertTarget
mkAlertTarget alertTargetArn roleArn =
  AlertTarget' {alertTargetArn, roleArn}

-- | The ARN of the notification target to which alerts are sent.
--
-- /Note:/ Consider using 'alertTargetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atAlertTargetArn :: Lens.Lens' AlertTarget Types.AlertTargetArn
atAlertTargetArn = Lens.field @"alertTargetArn"
{-# DEPRECATED atAlertTargetArn "Use generic-lens or generic-optics with 'alertTargetArn' instead." #-}

-- | The ARN of the role that grants permission to send alerts to the notification target.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atRoleArn :: Lens.Lens' AlertTarget Types.RoleArn
atRoleArn = Lens.field @"roleArn"
{-# DEPRECATED atRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON AlertTarget where
  toJSON AlertTarget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("alertTargetArn" Core..= alertTargetArn),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.FromJSON AlertTarget where
  parseJSON =
    Core.withObject "AlertTarget" Core.$
      \x ->
        AlertTarget'
          Core.<$> (x Core..: "alertTargetArn") Core.<*> (x Core..: "roleArn")
