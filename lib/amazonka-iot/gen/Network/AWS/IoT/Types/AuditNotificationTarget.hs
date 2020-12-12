{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditNotificationTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditNotificationTarget
  ( AuditNotificationTarget (..),

    -- * Smart constructor
    mkAuditNotificationTarget,

    -- * Lenses
    antTargetARN,
    antEnabled,
    antRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the targets to which audit notifications are sent.
--
-- /See:/ 'mkAuditNotificationTarget' smart constructor.
data AuditNotificationTarget = AuditNotificationTarget'
  { targetARN ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuditNotificationTarget' with the minimum fields required to make a request.
--
-- * 'enabled' - True if notifications to the target are enabled.
-- * 'roleARN' - The ARN of the role that grants permission to send notifications to the target.
-- * 'targetARN' - The ARN of the target (SNS topic) to which audit notifications are sent.
mkAuditNotificationTarget ::
  AuditNotificationTarget
mkAuditNotificationTarget =
  AuditNotificationTarget'
    { targetARN = Lude.Nothing,
      enabled = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The ARN of the target (SNS topic) to which audit notifications are sent.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
antTargetARN :: Lens.Lens' AuditNotificationTarget (Lude.Maybe Lude.Text)
antTargetARN = Lens.lens (targetARN :: AuditNotificationTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetARN = a} :: AuditNotificationTarget)
{-# DEPRECATED antTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | True if notifications to the target are enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
antEnabled :: Lens.Lens' AuditNotificationTarget (Lude.Maybe Lude.Bool)
antEnabled = Lens.lens (enabled :: AuditNotificationTarget -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: AuditNotificationTarget)
{-# DEPRECATED antEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The ARN of the role that grants permission to send notifications to the target.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
antRoleARN :: Lens.Lens' AuditNotificationTarget (Lude.Maybe Lude.Text)
antRoleARN = Lens.lens (roleARN :: AuditNotificationTarget -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: AuditNotificationTarget)
{-# DEPRECATED antRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON AuditNotificationTarget where
  parseJSON =
    Lude.withObject
      "AuditNotificationTarget"
      ( \x ->
          AuditNotificationTarget'
            Lude.<$> (x Lude..:? "targetArn")
            Lude.<*> (x Lude..:? "enabled")
            Lude.<*> (x Lude..:? "roleArn")
      )

instance Lude.ToJSON AuditNotificationTarget where
  toJSON AuditNotificationTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("targetArn" Lude..=) Lude.<$> targetARN,
            ("enabled" Lude..=) Lude.<$> enabled,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )
