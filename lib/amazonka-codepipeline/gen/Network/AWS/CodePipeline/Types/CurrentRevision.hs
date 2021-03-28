{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.CurrentRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.CurrentRevision
  ( CurrentRevision (..)
  -- * Smart constructor
  , mkCurrentRevision
  -- * Lenses
  , crRevision
  , crChangeIdentifier
  , crCreated
  , crRevisionSummary
  ) where

import qualified Network.AWS.CodePipeline.Types.ChangeIdentifier as Types
import qualified Network.AWS.CodePipeline.Types.Revision as Types
import qualified Network.AWS.CodePipeline.Types.RevisionSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about a current revision.
--
-- /See:/ 'mkCurrentRevision' smart constructor.
data CurrentRevision = CurrentRevision'
  { revision :: Types.Revision
    -- ^ The revision ID of the current version of an artifact.
  , changeIdentifier :: Types.ChangeIdentifier
    -- ^ The change identifier for the current revision.
  , created :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the most recent revision of the artifact was created, in timestamp format.
  , revisionSummary :: Core.Maybe Types.RevisionSummary
    -- ^ The summary of the most recent revision of the artifact.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CurrentRevision' value with any optional fields omitted.
mkCurrentRevision
    :: Types.Revision -- ^ 'revision'
    -> Types.ChangeIdentifier -- ^ 'changeIdentifier'
    -> CurrentRevision
mkCurrentRevision revision changeIdentifier
  = CurrentRevision'{revision, changeIdentifier,
                     created = Core.Nothing, revisionSummary = Core.Nothing}

-- | The revision ID of the current version of an artifact.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRevision :: Lens.Lens' CurrentRevision Types.Revision
crRevision = Lens.field @"revision"
{-# INLINEABLE crRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

-- | The change identifier for the current revision.
--
-- /Note:/ Consider using 'changeIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crChangeIdentifier :: Lens.Lens' CurrentRevision Types.ChangeIdentifier
crChangeIdentifier = Lens.field @"changeIdentifier"
{-# INLINEABLE crChangeIdentifier #-}
{-# DEPRECATED changeIdentifier "Use generic-lens or generic-optics with 'changeIdentifier' instead"  #-}

-- | The date and time when the most recent revision of the artifact was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCreated :: Lens.Lens' CurrentRevision (Core.Maybe Core.NominalDiffTime)
crCreated = Lens.field @"created"
{-# INLINEABLE crCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | The summary of the most recent revision of the artifact.
--
-- /Note:/ Consider using 'revisionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRevisionSummary :: Lens.Lens' CurrentRevision (Core.Maybe Types.RevisionSummary)
crRevisionSummary = Lens.field @"revisionSummary"
{-# INLINEABLE crRevisionSummary #-}
{-# DEPRECATED revisionSummary "Use generic-lens or generic-optics with 'revisionSummary' instead"  #-}

instance Core.FromJSON CurrentRevision where
        toJSON CurrentRevision{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("revision" Core..= revision),
                  Core.Just ("changeIdentifier" Core..= changeIdentifier),
                  ("created" Core..=) Core.<$> created,
                  ("revisionSummary" Core..=) Core.<$> revisionSummary])
