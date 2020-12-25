{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.RollbackConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.RollbackConfiguration
  ( RollbackConfiguration (..),

    -- * Smart constructor
    mkRollbackConfiguration,

    -- * Lenses
    rcMonitoringTimeInMinutes,
    rcRollbackTriggers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServerlessApplicationRepository.Types.RollbackTrigger as Types

-- | This property corresponds to the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
--
-- /See:/ 'mkRollbackConfiguration' smart constructor.
data RollbackConfiguration = RollbackConfiguration'
  { -- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
    monitoringTimeInMinutes :: Core.Maybe Core.Int,
    -- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
    rollbackTriggers :: Core.Maybe [Types.RollbackTrigger]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RollbackConfiguration' value with any optional fields omitted.
mkRollbackConfiguration ::
  RollbackConfiguration
mkRollbackConfiguration =
  RollbackConfiguration'
    { monitoringTimeInMinutes = Core.Nothing,
      rollbackTriggers = Core.Nothing
    }

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
--
-- /Note:/ Consider using 'monitoringTimeInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcMonitoringTimeInMinutes :: Lens.Lens' RollbackConfiguration (Core.Maybe Core.Int)
rcMonitoringTimeInMinutes = Lens.field @"monitoringTimeInMinutes"
{-# DEPRECATED rcMonitoringTimeInMinutes "Use generic-lens or generic-optics with 'monitoringTimeInMinutes' instead." #-}

-- | This property corresponds to the content of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/RollbackConfiguration RollbackConfiguration> / Data Type.
--
-- /Note:/ Consider using 'rollbackTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRollbackTriggers :: Lens.Lens' RollbackConfiguration (Core.Maybe [Types.RollbackTrigger])
rcRollbackTriggers = Lens.field @"rollbackTriggers"
{-# DEPRECATED rcRollbackTriggers "Use generic-lens or generic-optics with 'rollbackTriggers' instead." #-}

instance Core.FromJSON RollbackConfiguration where
  toJSON RollbackConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("monitoringTimeInMinutes" Core..=)
              Core.<$> monitoringTimeInMinutes,
            ("rollbackTriggers" Core..=) Core.<$> rollbackTriggers
          ]
      )
