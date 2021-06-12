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
-- Module      : Network.AWS.IoT.Types.ProvisioningHook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningHook where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Structure that contains @payloadVersion@ and @targetArn@.
--
-- /See:/ 'newProvisioningHook' smart constructor.
data ProvisioningHook = ProvisioningHook'
  { -- | The payload that was sent to the target function.
    --
    -- /Note:/ Only Lambda functions are currently supported.
    payloadVersion :: Core.Maybe Core.Text,
    -- | The ARN of the target function.
    --
    -- /Note:/ Only Lambda functions are currently supported.
    targetArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisioningHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payloadVersion', 'provisioningHook_payloadVersion' - The payload that was sent to the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
--
-- 'targetArn', 'provisioningHook_targetArn' - The ARN of the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
newProvisioningHook ::
  -- | 'targetArn'
  Core.Text ->
  ProvisioningHook
newProvisioningHook pTargetArn_ =
  ProvisioningHook'
    { payloadVersion = Core.Nothing,
      targetArn = pTargetArn_
    }

-- | The payload that was sent to the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
provisioningHook_payloadVersion :: Lens.Lens' ProvisioningHook (Core.Maybe Core.Text)
provisioningHook_payloadVersion = Lens.lens (\ProvisioningHook' {payloadVersion} -> payloadVersion) (\s@ProvisioningHook' {} a -> s {payloadVersion = a} :: ProvisioningHook)

-- | The ARN of the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
provisioningHook_targetArn :: Lens.Lens' ProvisioningHook Core.Text
provisioningHook_targetArn = Lens.lens (\ProvisioningHook' {targetArn} -> targetArn) (\s@ProvisioningHook' {} a -> s {targetArn = a} :: ProvisioningHook)

instance Core.FromJSON ProvisioningHook where
  parseJSON =
    Core.withObject
      "ProvisioningHook"
      ( \x ->
          ProvisioningHook'
            Core.<$> (x Core..:? "payloadVersion")
            Core.<*> (x Core..: "targetArn")
      )

instance Core.Hashable ProvisioningHook

instance Core.NFData ProvisioningHook

instance Core.ToJSON ProvisioningHook where
  toJSON ProvisioningHook' {..} =
    Core.object
      ( Core.catMaybes
          [ ("payloadVersion" Core..=) Core.<$> payloadVersion,
            Core.Just ("targetArn" Core..= targetArn)
          ]
      )
