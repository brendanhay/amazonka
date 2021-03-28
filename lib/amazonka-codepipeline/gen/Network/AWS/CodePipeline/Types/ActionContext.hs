{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionContext
  ( ActionContext (..)
  -- * Smart constructor
  , mkActionContext
  -- * Lenses
  , acActionExecutionId
  , acName
  ) where

import qualified Network.AWS.CodePipeline.Types.ActionExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.ActionName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the context of an action in the stage of a pipeline to a job worker.
--
-- /See:/ 'mkActionContext' smart constructor.
data ActionContext = ActionContext'
  { actionExecutionId :: Core.Maybe Types.ActionExecutionId
    -- ^ The system-generated unique ID that corresponds to an action's execution.
  , name :: Core.Maybe Types.ActionName
    -- ^ The name of the action in the context of a job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionContext' value with any optional fields omitted.
mkActionContext
    :: ActionContext
mkActionContext
  = ActionContext'{actionExecutionId = Core.Nothing,
                   name = Core.Nothing}

-- | The system-generated unique ID that corresponds to an action's execution.
--
-- /Note:/ Consider using 'actionExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acActionExecutionId :: Lens.Lens' ActionContext (Core.Maybe Types.ActionExecutionId)
acActionExecutionId = Lens.field @"actionExecutionId"
{-# INLINEABLE acActionExecutionId #-}
{-# DEPRECATED actionExecutionId "Use generic-lens or generic-optics with 'actionExecutionId' instead"  #-}

-- | The name of the action in the context of a job.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acName :: Lens.Lens' ActionContext (Core.Maybe Types.ActionName)
acName = Lens.field @"name"
{-# INLINEABLE acName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON ActionContext where
        parseJSON
          = Core.withObject "ActionContext" Core.$
              \ x ->
                ActionContext' Core.<$>
                  (x Core..:? "actionExecutionId") Core.<*> x Core..:? "name"
