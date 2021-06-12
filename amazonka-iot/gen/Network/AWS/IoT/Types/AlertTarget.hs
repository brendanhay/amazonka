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
-- Module      : Network.AWS.IoT.Types.AlertTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AlertTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure containing the alert target ARN and the role ARN.
--
-- /See:/ 'newAlertTarget' smart constructor.
data AlertTarget = AlertTarget'
  { -- | The Amazon Resource Name (ARN) of the notification target to which
    -- alerts are sent.
    alertTargetArn :: Core.Text,
    -- | The ARN of the role that grants permission to send alerts to the
    -- notification target.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AlertTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alertTargetArn', 'alertTarget_alertTargetArn' - The Amazon Resource Name (ARN) of the notification target to which
-- alerts are sent.
--
-- 'roleArn', 'alertTarget_roleArn' - The ARN of the role that grants permission to send alerts to the
-- notification target.
newAlertTarget ::
  -- | 'alertTargetArn'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  AlertTarget
newAlertTarget pAlertTargetArn_ pRoleArn_ =
  AlertTarget'
    { alertTargetArn = pAlertTargetArn_,
      roleArn = pRoleArn_
    }

-- | The Amazon Resource Name (ARN) of the notification target to which
-- alerts are sent.
alertTarget_alertTargetArn :: Lens.Lens' AlertTarget Core.Text
alertTarget_alertTargetArn = Lens.lens (\AlertTarget' {alertTargetArn} -> alertTargetArn) (\s@AlertTarget' {} a -> s {alertTargetArn = a} :: AlertTarget)

-- | The ARN of the role that grants permission to send alerts to the
-- notification target.
alertTarget_roleArn :: Lens.Lens' AlertTarget Core.Text
alertTarget_roleArn = Lens.lens (\AlertTarget' {roleArn} -> roleArn) (\s@AlertTarget' {} a -> s {roleArn = a} :: AlertTarget)

instance Core.FromJSON AlertTarget where
  parseJSON =
    Core.withObject
      "AlertTarget"
      ( \x ->
          AlertTarget'
            Core.<$> (x Core..: "alertTargetArn")
            Core.<*> (x Core..: "roleArn")
      )

instance Core.Hashable AlertTarget

instance Core.NFData AlertTarget

instance Core.ToJSON AlertTarget where
  toJSON AlertTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("alertTargetArn" Core..= alertTargetArn),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )
