{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopActivationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopActivationConfig
  ( HumanLoopActivationConfig (..),

    -- * Smart constructor
    mkHumanLoopActivationConfig,

    -- * Lenses
    hlacHumanLoopActivationConditionsConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig as Types

-- | Provides information about how and under what conditions SageMaker creates a human loop. If @HumanLoopActivationConfig@ is not given, then all requests go to humans.
--
-- /See:/ 'mkHumanLoopActivationConfig' smart constructor.
newtype HumanLoopActivationConfig = HumanLoopActivationConfig'
  { -- | Container structure for defining under what conditions SageMaker creates a human loop.
    humanLoopActivationConditionsConfig :: Types.HumanLoopActivationConditionsConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'HumanLoopActivationConfig' value with any optional fields omitted.
mkHumanLoopActivationConfig ::
  -- | 'humanLoopActivationConditionsConfig'
  Types.HumanLoopActivationConditionsConfig ->
  HumanLoopActivationConfig
mkHumanLoopActivationConfig humanLoopActivationConditionsConfig =
  HumanLoopActivationConfig' {humanLoopActivationConditionsConfig}

-- | Container structure for defining under what conditions SageMaker creates a human loop.
--
-- /Note:/ Consider using 'humanLoopActivationConditionsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlacHumanLoopActivationConditionsConfig :: Lens.Lens' HumanLoopActivationConfig Types.HumanLoopActivationConditionsConfig
hlacHumanLoopActivationConditionsConfig = Lens.field @"humanLoopActivationConditionsConfig"
{-# DEPRECATED hlacHumanLoopActivationConditionsConfig "Use generic-lens or generic-optics with 'humanLoopActivationConditionsConfig' instead." #-}

instance Core.FromJSON HumanLoopActivationConfig where
  toJSON HumanLoopActivationConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "HumanLoopActivationConditionsConfig"
                  Core..= humanLoopActivationConditionsConfig
              )
          ]
      )

instance Core.FromJSON HumanLoopActivationConfig where
  parseJSON =
    Core.withObject "HumanLoopActivationConfig" Core.$
      \x ->
        HumanLoopActivationConfig'
          Core.<$> (x Core..: "HumanLoopActivationConditionsConfig")
