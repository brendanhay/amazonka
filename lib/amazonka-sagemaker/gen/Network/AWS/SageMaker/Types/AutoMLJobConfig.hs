{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobConfig
  ( AutoMLJobConfig (..),

    -- * Smart constructor
    mkAutoMLJobConfig,

    -- * Lenses
    amljcCompletionCriteria,
    amljcSecurityConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria as Types
import qualified Network.AWS.SageMaker.Types.AutoMLSecurityConfig as Types

-- | A collection of settings used for a job.
--
-- /See:/ 'mkAutoMLJobConfig' smart constructor.
data AutoMLJobConfig = AutoMLJobConfig'
  { -- | How long a job is allowed to run, or how many candidates a job is allowed to generate.
    completionCriteria :: Core.Maybe Types.AutoMLJobCompletionCriteria,
    -- | Security configuration for traffic encryption or Amazon VPC settings.
    securityConfig :: Core.Maybe Types.AutoMLSecurityConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLJobConfig' value with any optional fields omitted.
mkAutoMLJobConfig ::
  AutoMLJobConfig
mkAutoMLJobConfig =
  AutoMLJobConfig'
    { completionCriteria = Core.Nothing,
      securityConfig = Core.Nothing
    }

-- | How long a job is allowed to run, or how many candidates a job is allowed to generate.
--
-- /Note:/ Consider using 'completionCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljcCompletionCriteria :: Lens.Lens' AutoMLJobConfig (Core.Maybe Types.AutoMLJobCompletionCriteria)
amljcCompletionCriteria = Lens.field @"completionCriteria"
{-# DEPRECATED amljcCompletionCriteria "Use generic-lens or generic-optics with 'completionCriteria' instead." #-}

-- | Security configuration for traffic encryption or Amazon VPC settings.
--
-- /Note:/ Consider using 'securityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljcSecurityConfig :: Lens.Lens' AutoMLJobConfig (Core.Maybe Types.AutoMLSecurityConfig)
amljcSecurityConfig = Lens.field @"securityConfig"
{-# DEPRECATED amljcSecurityConfig "Use generic-lens or generic-optics with 'securityConfig' instead." #-}

instance Core.FromJSON AutoMLJobConfig where
  toJSON AutoMLJobConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("CompletionCriteria" Core..=) Core.<$> completionCriteria,
            ("SecurityConfig" Core..=) Core.<$> securityConfig
          ]
      )

instance Core.FromJSON AutoMLJobConfig where
  parseJSON =
    Core.withObject "AutoMLJobConfig" Core.$
      \x ->
        AutoMLJobConfig'
          Core.<$> (x Core..:? "CompletionCriteria")
          Core.<*> (x Core..:? "SecurityConfig")
