{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelClientConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelClientConfig
  ( ModelClientConfig (..),

    -- * Smart constructor
    mkModelClientConfig,

    -- * Lenses
    mccInvocationsMaxRetries,
    mccInvocationsTimeoutInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configures the timeout and maximum number of retries for processing a transform job invocation.
--
-- /See:/ 'mkModelClientConfig' smart constructor.
data ModelClientConfig = ModelClientConfig'
  { -- | The maximum number of retries when invocation requests are failing.
    invocationsMaxRetries :: Core.Maybe Core.Natural,
    -- | The timeout value in seconds for an invocation request.
    invocationsTimeoutInSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModelClientConfig' value with any optional fields omitted.
mkModelClientConfig ::
  ModelClientConfig
mkModelClientConfig =
  ModelClientConfig'
    { invocationsMaxRetries = Core.Nothing,
      invocationsTimeoutInSeconds = Core.Nothing
    }

-- | The maximum number of retries when invocation requests are failing.
--
-- /Note:/ Consider using 'invocationsMaxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccInvocationsMaxRetries :: Lens.Lens' ModelClientConfig (Core.Maybe Core.Natural)
mccInvocationsMaxRetries = Lens.field @"invocationsMaxRetries"
{-# DEPRECATED mccInvocationsMaxRetries "Use generic-lens or generic-optics with 'invocationsMaxRetries' instead." #-}

-- | The timeout value in seconds for an invocation request.
--
-- /Note:/ Consider using 'invocationsTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccInvocationsTimeoutInSeconds :: Lens.Lens' ModelClientConfig (Core.Maybe Core.Natural)
mccInvocationsTimeoutInSeconds = Lens.field @"invocationsTimeoutInSeconds"
{-# DEPRECATED mccInvocationsTimeoutInSeconds "Use generic-lens or generic-optics with 'invocationsTimeoutInSeconds' instead." #-}

instance Core.FromJSON ModelClientConfig where
  toJSON ModelClientConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("InvocationsMaxRetries" Core..=) Core.<$> invocationsMaxRetries,
            ("InvocationsTimeoutInSeconds" Core..=)
              Core.<$> invocationsTimeoutInSeconds
          ]
      )

instance Core.FromJSON ModelClientConfig where
  parseJSON =
    Core.withObject "ModelClientConfig" Core.$
      \x ->
        ModelClientConfig'
          Core.<$> (x Core..:? "InvocationsMaxRetries")
          Core.<*> (x Core..:? "InvocationsTimeoutInSeconds")
