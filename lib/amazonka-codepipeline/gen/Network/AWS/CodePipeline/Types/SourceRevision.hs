{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.SourceRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.SourceRevision
  ( SourceRevision (..)
  -- * Smart constructor
  , mkSourceRevision
  -- * Lenses
  , srActionName
  , srRevisionId
  , srRevisionSummary
  , srRevisionUrl
  ) where

import qualified Network.AWS.CodePipeline.Types.ActionName as Types
import qualified Network.AWS.CodePipeline.Types.Revision as Types
import qualified Network.AWS.CodePipeline.Types.RevisionSummary as Types
import qualified Network.AWS.CodePipeline.Types.Url as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the version (or revision) of a source artifact that initiated a pipeline execution.
--
-- /See:/ 'mkSourceRevision' smart constructor.
data SourceRevision = SourceRevision'
  { actionName :: Types.ActionName
    -- ^ The name of the action that processed the revision to the source artifact.
  , revisionId :: Core.Maybe Types.Revision
    -- ^ The system-generated unique ID that identifies the revision number of the artifact.
  , revisionSummary :: Core.Maybe Types.RevisionSummary
    -- ^ Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
  , revisionUrl :: Core.Maybe Types.Url
    -- ^ The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceRevision' value with any optional fields omitted.
mkSourceRevision
    :: Types.ActionName -- ^ 'actionName'
    -> SourceRevision
mkSourceRevision actionName
  = SourceRevision'{actionName, revisionId = Core.Nothing,
                    revisionSummary = Core.Nothing, revisionUrl = Core.Nothing}

-- | The name of the action that processed the revision to the source artifact.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srActionName :: Lens.Lens' SourceRevision Types.ActionName
srActionName = Lens.field @"actionName"
{-# INLINEABLE srActionName #-}
{-# DEPRECATED actionName "Use generic-lens or generic-optics with 'actionName' instead"  #-}

-- | The system-generated unique ID that identifies the revision number of the artifact.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRevisionId :: Lens.Lens' SourceRevision (Core.Maybe Types.Revision)
srRevisionId = Lens.field @"revisionId"
{-# INLINEABLE srRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

-- | Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
--
-- /Note:/ Consider using 'revisionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRevisionSummary :: Lens.Lens' SourceRevision (Core.Maybe Types.RevisionSummary)
srRevisionSummary = Lens.field @"revisionSummary"
{-# INLINEABLE srRevisionSummary #-}
{-# DEPRECATED revisionSummary "Use generic-lens or generic-optics with 'revisionSummary' instead"  #-}

-- | The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
--
-- /Note:/ Consider using 'revisionUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRevisionUrl :: Lens.Lens' SourceRevision (Core.Maybe Types.Url)
srRevisionUrl = Lens.field @"revisionUrl"
{-# INLINEABLE srRevisionUrl #-}
{-# DEPRECATED revisionUrl "Use generic-lens or generic-optics with 'revisionUrl' instead"  #-}

instance Core.FromJSON SourceRevision where
        parseJSON
          = Core.withObject "SourceRevision" Core.$
              \ x ->
                SourceRevision' Core.<$>
                  (x Core..: "actionName") Core.<*> x Core..:? "revisionId" Core.<*>
                    x Core..:? "revisionSummary"
                    Core.<*> x Core..:? "revisionUrl"
