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
-- Module      : Amazonka.IoT.Types.AuditNotificationTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditNotificationTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the targets to which audit notifications are sent.
--
-- /See:/ 'newAuditNotificationTarget' smart constructor.
data AuditNotificationTarget = AuditNotificationTarget'
  { -- | True if notifications to the target are enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the role that grants permission to send notifications to the
    -- target.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the target (SNS topic) to which audit notifications are sent.
    targetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditNotificationTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'auditNotificationTarget_enabled' - True if notifications to the target are enabled.
--
-- 'roleArn', 'auditNotificationTarget_roleArn' - The ARN of the role that grants permission to send notifications to the
-- target.
--
-- 'targetArn', 'auditNotificationTarget_targetArn' - The ARN of the target (SNS topic) to which audit notifications are sent.
newAuditNotificationTarget ::
  AuditNotificationTarget
newAuditNotificationTarget =
  AuditNotificationTarget'
    { enabled = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      targetArn = Prelude.Nothing
    }

-- | True if notifications to the target are enabled.
auditNotificationTarget_enabled :: Lens.Lens' AuditNotificationTarget (Prelude.Maybe Prelude.Bool)
auditNotificationTarget_enabled = Lens.lens (\AuditNotificationTarget' {enabled} -> enabled) (\s@AuditNotificationTarget' {} a -> s {enabled = a} :: AuditNotificationTarget)

-- | The ARN of the role that grants permission to send notifications to the
-- target.
auditNotificationTarget_roleArn :: Lens.Lens' AuditNotificationTarget (Prelude.Maybe Prelude.Text)
auditNotificationTarget_roleArn = Lens.lens (\AuditNotificationTarget' {roleArn} -> roleArn) (\s@AuditNotificationTarget' {} a -> s {roleArn = a} :: AuditNotificationTarget)

-- | The ARN of the target (SNS topic) to which audit notifications are sent.
auditNotificationTarget_targetArn :: Lens.Lens' AuditNotificationTarget (Prelude.Maybe Prelude.Text)
auditNotificationTarget_targetArn = Lens.lens (\AuditNotificationTarget' {targetArn} -> targetArn) (\s@AuditNotificationTarget' {} a -> s {targetArn = a} :: AuditNotificationTarget)

instance Data.FromJSON AuditNotificationTarget where
  parseJSON =
    Data.withObject
      "AuditNotificationTarget"
      ( \x ->
          AuditNotificationTarget'
            Prelude.<$> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "targetArn")
      )

instance Prelude.Hashable AuditNotificationTarget where
  hashWithSalt _salt AuditNotificationTarget' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData AuditNotificationTarget where
  rnf AuditNotificationTarget' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf targetArn

instance Data.ToJSON AuditNotificationTarget where
  toJSON AuditNotificationTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("roleArn" Data..=) Prelude.<$> roleArn,
            ("targetArn" Data..=) Prelude.<$> targetArn
          ]
      )
