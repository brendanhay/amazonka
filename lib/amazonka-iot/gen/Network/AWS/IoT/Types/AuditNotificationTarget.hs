{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditNotificationTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditNotificationTarget
  ( AuditNotificationTarget (..)
  -- * Smart constructor
  , mkAuditNotificationTarget
  -- * Lenses
  , antEnabled
  , antRoleArn
  , antTargetArn
  ) where

import qualified Network.AWS.IoT.Types.RoleArn as Types
import qualified Network.AWS.IoT.Types.TargetArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the targets to which audit notifications are sent.
--
-- /See:/ 'mkAuditNotificationTarget' smart constructor.
data AuditNotificationTarget = AuditNotificationTarget'
  { enabled :: Core.Maybe Core.Bool
    -- ^ True if notifications to the target are enabled.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the role that grants permission to send notifications to the target.
  , targetArn :: Core.Maybe Types.TargetArn
    -- ^ The ARN of the target (SNS topic) to which audit notifications are sent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuditNotificationTarget' value with any optional fields omitted.
mkAuditNotificationTarget
    :: AuditNotificationTarget
mkAuditNotificationTarget
  = AuditNotificationTarget'{enabled = Core.Nothing,
                             roleArn = Core.Nothing, targetArn = Core.Nothing}

-- | True if notifications to the target are enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
antEnabled :: Lens.Lens' AuditNotificationTarget (Core.Maybe Core.Bool)
antEnabled = Lens.field @"enabled"
{-# INLINEABLE antEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The ARN of the role that grants permission to send notifications to the target.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
antRoleArn :: Lens.Lens' AuditNotificationTarget (Core.Maybe Types.RoleArn)
antRoleArn = Lens.field @"roleArn"
{-# INLINEABLE antRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The ARN of the target (SNS topic) to which audit notifications are sent.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
antTargetArn :: Lens.Lens' AuditNotificationTarget (Core.Maybe Types.TargetArn)
antTargetArn = Lens.field @"targetArn"
{-# INLINEABLE antTargetArn #-}
{-# DEPRECATED targetArn "Use generic-lens or generic-optics with 'targetArn' instead"  #-}

instance Core.FromJSON AuditNotificationTarget where
        toJSON AuditNotificationTarget{..}
          = Core.object
              (Core.catMaybes
                 [("enabled" Core..=) Core.<$> enabled,
                  ("roleArn" Core..=) Core.<$> roleArn,
                  ("targetArn" Core..=) Core.<$> targetArn])

instance Core.FromJSON AuditNotificationTarget where
        parseJSON
          = Core.withObject "AuditNotificationTarget" Core.$
              \ x ->
                AuditNotificationTarget' Core.<$>
                  (x Core..:? "enabled") Core.<*> x Core..:? "roleArn" Core.<*>
                    x Core..:? "targetArn"
