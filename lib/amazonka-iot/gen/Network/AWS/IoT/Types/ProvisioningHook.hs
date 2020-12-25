{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ProvisioningHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningHook
  ( ProvisioningHook (..),

    -- * Smart constructor
    mkProvisioningHook,

    -- * Lenses
    phTargetArn,
    phPayloadVersion,
  )
where

import qualified Network.AWS.IoT.Types.PayloadVersion as Types
import qualified Network.AWS.IoT.Types.TargetArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Structure that contains @payloadVersion@ and @targetArn@ .
--
-- /See:/ 'mkProvisioningHook' smart constructor.
data ProvisioningHook = ProvisioningHook'
  { -- | The ARN of the target function.
    --
    -- /Note:/ Only Lambda functions are currently supported.
    targetArn :: Types.TargetArn,
    -- | The payload that was sent to the target function.
    --
    -- /Note:/ Only Lambda functions are currently supported.
    payloadVersion :: Core.Maybe Types.PayloadVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisioningHook' value with any optional fields omitted.
mkProvisioningHook ::
  -- | 'targetArn'
  Types.TargetArn ->
  ProvisioningHook
mkProvisioningHook targetArn =
  ProvisioningHook' {targetArn, payloadVersion = Core.Nothing}

-- | The ARN of the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phTargetArn :: Lens.Lens' ProvisioningHook Types.TargetArn
phTargetArn = Lens.field @"targetArn"
{-# DEPRECATED phTargetArn "Use generic-lens or generic-optics with 'targetArn' instead." #-}

-- | The payload that was sent to the target function.
--
-- /Note:/ Only Lambda functions are currently supported.
--
-- /Note:/ Consider using 'payloadVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phPayloadVersion :: Lens.Lens' ProvisioningHook (Core.Maybe Types.PayloadVersion)
phPayloadVersion = Lens.field @"payloadVersion"
{-# DEPRECATED phPayloadVersion "Use generic-lens or generic-optics with 'payloadVersion' instead." #-}

instance Core.FromJSON ProvisioningHook where
  toJSON ProvisioningHook {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("targetArn" Core..= targetArn),
            ("payloadVersion" Core..=) Core.<$> payloadVersion
          ]
      )

instance Core.FromJSON ProvisioningHook where
  parseJSON =
    Core.withObject "ProvisioningHook" Core.$
      \x ->
        ProvisioningHook'
          Core.<$> (x Core..: "targetArn") Core.<*> (x Core..:? "payloadVersion")
