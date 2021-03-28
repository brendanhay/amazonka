{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ExecutionTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ExecutionTrigger
  ( ExecutionTrigger (..)
  -- * Smart constructor
  , mkExecutionTrigger
  -- * Lenses
  , etTriggerDetail
  , etTriggerType
  ) where

import qualified Network.AWS.CodePipeline.Types.TriggerDetail as Types
import qualified Network.AWS.CodePipeline.Types.TriggerType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The interaction or event that started a pipeline execution.
--
-- /See:/ 'mkExecutionTrigger' smart constructor.
data ExecutionTrigger = ExecutionTrigger'
  { triggerDetail :: Core.Maybe Types.TriggerDetail
    -- ^ Detail related to the event that started a pipeline execution, such as the webhook ARN of the webhook that triggered the pipeline execution or the user ARN for a user-initiated @start-pipeline-execution@ CLI command.
  , triggerType :: Core.Maybe Types.TriggerType
    -- ^ The type of change-detection method, command, or user interaction that started a pipeline execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionTrigger' value with any optional fields omitted.
mkExecutionTrigger
    :: ExecutionTrigger
mkExecutionTrigger
  = ExecutionTrigger'{triggerDetail = Core.Nothing,
                      triggerType = Core.Nothing}

-- | Detail related to the event that started a pipeline execution, such as the webhook ARN of the webhook that triggered the pipeline execution or the user ARN for a user-initiated @start-pipeline-execution@ CLI command.
--
-- /Note:/ Consider using 'triggerDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTriggerDetail :: Lens.Lens' ExecutionTrigger (Core.Maybe Types.TriggerDetail)
etTriggerDetail = Lens.field @"triggerDetail"
{-# INLINEABLE etTriggerDetail #-}
{-# DEPRECATED triggerDetail "Use generic-lens or generic-optics with 'triggerDetail' instead"  #-}

-- | The type of change-detection method, command, or user interaction that started a pipeline execution.
--
-- /Note:/ Consider using 'triggerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTriggerType :: Lens.Lens' ExecutionTrigger (Core.Maybe Types.TriggerType)
etTriggerType = Lens.field @"triggerType"
{-# INLINEABLE etTriggerType #-}
{-# DEPRECATED triggerType "Use generic-lens or generic-optics with 'triggerType' instead"  #-}

instance Core.FromJSON ExecutionTrigger where
        parseJSON
          = Core.withObject "ExecutionTrigger" Core.$
              \ x ->
                ExecutionTrigger' Core.<$>
                  (x Core..:? "triggerDetail") Core.<*> x Core..:? "triggerType"
