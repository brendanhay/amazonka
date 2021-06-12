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
-- Module      : Network.AWS.IoT.Types.AuditNotificationTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditNotificationTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the targets to which audit notifications are sent.
--
-- /See:/ 'newAuditNotificationTarget' smart constructor.
data AuditNotificationTarget = AuditNotificationTarget'
  { -- | The ARN of the role that grants permission to send notifications to the
    -- target.
    roleArn :: Core.Maybe Core.Text,
    -- | True if notifications to the target are enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The ARN of the target (SNS topic) to which audit notifications are sent.
    targetArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AuditNotificationTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'auditNotificationTarget_roleArn' - The ARN of the role that grants permission to send notifications to the
-- target.
--
-- 'enabled', 'auditNotificationTarget_enabled' - True if notifications to the target are enabled.
--
-- 'targetArn', 'auditNotificationTarget_targetArn' - The ARN of the target (SNS topic) to which audit notifications are sent.
newAuditNotificationTarget ::
  AuditNotificationTarget
newAuditNotificationTarget =
  AuditNotificationTarget'
    { roleArn = Core.Nothing,
      enabled = Core.Nothing,
      targetArn = Core.Nothing
    }

-- | The ARN of the role that grants permission to send notifications to the
-- target.
auditNotificationTarget_roleArn :: Lens.Lens' AuditNotificationTarget (Core.Maybe Core.Text)
auditNotificationTarget_roleArn = Lens.lens (\AuditNotificationTarget' {roleArn} -> roleArn) (\s@AuditNotificationTarget' {} a -> s {roleArn = a} :: AuditNotificationTarget)

-- | True if notifications to the target are enabled.
auditNotificationTarget_enabled :: Lens.Lens' AuditNotificationTarget (Core.Maybe Core.Bool)
auditNotificationTarget_enabled = Lens.lens (\AuditNotificationTarget' {enabled} -> enabled) (\s@AuditNotificationTarget' {} a -> s {enabled = a} :: AuditNotificationTarget)

-- | The ARN of the target (SNS topic) to which audit notifications are sent.
auditNotificationTarget_targetArn :: Lens.Lens' AuditNotificationTarget (Core.Maybe Core.Text)
auditNotificationTarget_targetArn = Lens.lens (\AuditNotificationTarget' {targetArn} -> targetArn) (\s@AuditNotificationTarget' {} a -> s {targetArn = a} :: AuditNotificationTarget)

instance Core.FromJSON AuditNotificationTarget where
  parseJSON =
    Core.withObject
      "AuditNotificationTarget"
      ( \x ->
          AuditNotificationTarget'
            Core.<$> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "enabled")
            Core.<*> (x Core..:? "targetArn")
      )

instance Core.Hashable AuditNotificationTarget

instance Core.NFData AuditNotificationTarget

instance Core.ToJSON AuditNotificationTarget where
  toJSON AuditNotificationTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("roleArn" Core..=) Core.<$> roleArn,
            ("enabled" Core..=) Core.<$> enabled,
            ("targetArn" Core..=) Core.<$> targetArn
          ]
      )
