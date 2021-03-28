{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionState
  ( ActionState (..)
  -- * Smart constructor
  , mkActionState
  -- * Lenses
  , asActionName
  , asCurrentRevision
  , asEntityUrl
  , asLatestExecution
  , asRevisionUrl
  ) where

import qualified Network.AWS.CodePipeline.Types.ActionExecution as Types
import qualified Network.AWS.CodePipeline.Types.ActionName as Types
import qualified Network.AWS.CodePipeline.Types.ActionRevision as Types
import qualified Network.AWS.CodePipeline.Types.Url as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the state of an action.
--
-- /See:/ 'mkActionState' smart constructor.
data ActionState = ActionState'
  { actionName :: Core.Maybe Types.ActionName
    -- ^ The name of the action.
  , currentRevision :: Core.Maybe Types.ActionRevision
    -- ^ Represents information about the version (or revision) of an action.
  , entityUrl :: Core.Maybe Types.Url
    -- ^ A URL link for more information about the state of the action, such as a deployment group details page.
  , latestExecution :: Core.Maybe Types.ActionExecution
    -- ^ Represents information about the run of an action.
  , revisionUrl :: Core.Maybe Types.Url
    -- ^ A URL link for more information about the revision, such as a commit details page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ActionState' value with any optional fields omitted.
mkActionState
    :: ActionState
mkActionState
  = ActionState'{actionName = Core.Nothing,
                 currentRevision = Core.Nothing, entityUrl = Core.Nothing,
                 latestExecution = Core.Nothing, revisionUrl = Core.Nothing}

-- | The name of the action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asActionName :: Lens.Lens' ActionState (Core.Maybe Types.ActionName)
asActionName = Lens.field @"actionName"
{-# INLINEABLE asActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

-- | Represents information about the version (or revision) of an action.
--
-- /Note:/ Consider using 'currentRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCurrentRevision :: Lens.Lens' ActionState (Core.Maybe Types.ActionRevision)
asCurrentRevision = Lens.field @"currentRevision"
{-# INLINEABLE asCurrentRevision #-}
{-# DEPRECATED currentRevision "Use generic-lens or generic-optics with 'currentRevision' instead"  #-}

-- | A URL link for more information about the state of the action, such as a deployment group details page.
--
-- /Note:/ Consider using 'entityUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asEntityUrl :: Lens.Lens' ActionState (Core.Maybe Types.Url)
asEntityUrl = Lens.field @"entityUrl"
{-# INLINEABLE asEntityUrl #-}
{-# DEPRECATED entityUrl "Use generic-lens or generic-optics with 'entityUrl' instead"  #-}

-- | Represents information about the run of an action.
--
-- /Note:/ Consider using 'latestExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLatestExecution :: Lens.Lens' ActionState (Core.Maybe Types.ActionExecution)
asLatestExecution = Lens.field @"latestExecution"
{-# INLINEABLE asLatestExecution #-}
{-# DEPRECATED latestExecution "Use generic-lens or generic-optics with 'latestExecution' instead"  #-}

-- | A URL link for more information about the revision, such as a commit details page.
--
-- /Note:/ Consider using 'revisionUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asRevisionUrl :: Lens.Lens' ActionState (Core.Maybe Types.Url)
asRevisionUrl = Lens.field @"revisionUrl"
{-# INLINEABLE asRevisionUrl #-}
{-# DEPRECATED revisionUrl "Use generic-lens or generic-optics with 'revisionUrl' instead"  #-}

instance Core.FromJSON ActionState where
        parseJSON
          = Core.withObject "ActionState" Core.$
              \ x ->
                ActionState' Core.<$>
                  (x Core..:? "actionName") Core.<*> x Core..:? "currentRevision"
                    Core.<*> x Core..:? "entityUrl"
                    Core.<*> x Core..:? "latestExecution"
                    Core.<*> x Core..:? "revisionUrl"
