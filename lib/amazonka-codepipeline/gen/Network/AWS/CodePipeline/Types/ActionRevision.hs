{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionRevision
  ( ActionRevision (..)
  -- * Smart constructor
  , mkActionRevision
  -- * Lenses
  , aRevisionId
  , aRevisionChangeId
  , aCreated
  ) where

import qualified Network.AWS.CodePipeline.Types.RevisionChangeId as Types
import qualified Network.AWS.CodePipeline.Types.RevisionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the version (or revision) of an action.
--
-- /See:/ 'mkActionRevision' smart constructor.
data ActionRevision = ActionRevision'
  { revisionId :: Types.RevisionId
    -- ^ The system-generated unique ID that identifies the revision number of the action.
  , revisionChangeId :: Types.RevisionChangeId
    -- ^ The unique identifier of the change that set the state to this revision (for example, a deployment ID or timestamp).
  , created :: Core.NominalDiffTime
    -- ^ The date and time when the most recent version of the action was created, in timestamp format.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ActionRevision' value with any optional fields omitted.
mkActionRevision
    :: Types.RevisionId -- ^ 'revisionId'
    -> Types.RevisionChangeId -- ^ 'revisionChangeId'
    -> Core.NominalDiffTime -- ^ 'created'
    -> ActionRevision
mkActionRevision revisionId revisionChangeId created
  = ActionRevision'{revisionId, revisionChangeId, created}

-- | The system-generated unique ID that identifies the revision number of the action.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRevisionId :: Lens.Lens' ActionRevision Types.RevisionId
aRevisionId = Lens.field @"revisionId"
{-# INLINEABLE aRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

-- | The unique identifier of the change that set the state to this revision (for example, a deployment ID or timestamp).
--
-- /Note:/ Consider using 'revisionChangeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aRevisionChangeId :: Lens.Lens' ActionRevision Types.RevisionChangeId
aRevisionChangeId = Lens.field @"revisionChangeId"
{-# INLINEABLE aRevisionChangeId #-}
{-# DEPRECATED revisionChangeId "Use generic-lens or generic-optics with 'revisionChangeId' instead"  #-}

-- | The date and time when the most recent version of the action was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCreated :: Lens.Lens' ActionRevision Core.NominalDiffTime
aCreated = Lens.field @"created"
{-# INLINEABLE aCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

instance Core.FromJSON ActionRevision where
        toJSON ActionRevision{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("revisionId" Core..= revisionId),
                  Core.Just ("revisionChangeId" Core..= revisionChangeId),
                  Core.Just ("created" Core..= created)])

instance Core.FromJSON ActionRevision where
        parseJSON
          = Core.withObject "ActionRevision" Core.$
              \ x ->
                ActionRevision' Core.<$>
                  (x Core..: "revisionId") Core.<*> x Core..: "revisionChangeId"
                    Core.<*> x Core..: "created"
