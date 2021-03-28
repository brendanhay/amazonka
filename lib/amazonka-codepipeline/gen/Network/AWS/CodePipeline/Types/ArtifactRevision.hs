{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ArtifactRevision
  ( ArtifactRevision (..)
  -- * Smart constructor
  , mkArtifactRevision
  -- * Lenses
  , arCreated
  , arName
  , arRevisionChangeIdentifier
  , arRevisionId
  , arRevisionSummary
  , arRevisionUrl
  ) where

import qualified Network.AWS.CodePipeline.Types.Name as Types
import qualified Network.AWS.CodePipeline.Types.RevisionChangeIdentifier as Types
import qualified Network.AWS.CodePipeline.Types.RevisionId as Types
import qualified Network.AWS.CodePipeline.Types.RevisionSummary as Types
import qualified Network.AWS.CodePipeline.Types.RevisionUrl as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents revision details of an artifact. 
--
-- /See:/ 'mkArtifactRevision' smart constructor.
data ArtifactRevision = ArtifactRevision'
  { created :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the most recent revision of the artifact was created, in timestamp format.
  , name :: Core.Maybe Types.Name
    -- ^ The name of an artifact. This name might be system-generated, such as "MyApp", or defined by the user when an action is created.
  , revisionChangeIdentifier :: Core.Maybe Types.RevisionChangeIdentifier
    -- ^ An additional identifier for a revision, such as a commit date or, for artifacts stored in Amazon S3 buckets, the ETag value.
  , revisionId :: Core.Maybe Types.RevisionId
    -- ^ The revision ID of the artifact.
  , revisionSummary :: Core.Maybe Types.RevisionSummary
    -- ^ Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
  , revisionUrl :: Core.Maybe Types.RevisionUrl
    -- ^ The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ArtifactRevision' value with any optional fields omitted.
mkArtifactRevision
    :: ArtifactRevision
mkArtifactRevision
  = ArtifactRevision'{created = Core.Nothing, name = Core.Nothing,
                      revisionChangeIdentifier = Core.Nothing, revisionId = Core.Nothing,
                      revisionSummary = Core.Nothing, revisionUrl = Core.Nothing}

-- | The date and time when the most recent revision of the artifact was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCreated :: Lens.Lens' ArtifactRevision (Core.Maybe Core.NominalDiffTime)
arCreated = Lens.field @"created"
{-# INLINEABLE arCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | The name of an artifact. This name might be system-generated, such as "MyApp", or defined by the user when an action is created.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arName :: Lens.Lens' ArtifactRevision (Core.Maybe Types.Name)
arName = Lens.field @"name"
{-# INLINEABLE arName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | An additional identifier for a revision, such as a commit date or, for artifacts stored in Amazon S3 buckets, the ETag value.
--
-- /Note:/ Consider using 'revisionChangeIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRevisionChangeIdentifier :: Lens.Lens' ArtifactRevision (Core.Maybe Types.RevisionChangeIdentifier)
arRevisionChangeIdentifier = Lens.field @"revisionChangeIdentifier"
{-# INLINEABLE arRevisionChangeIdentifier #-}
{-# DEPRECATED revisionChangeIdentifier "Use generic-lens or generic-optics with 'revisionChangeIdentifier' instead"  #-}

-- | The revision ID of the artifact.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRevisionId :: Lens.Lens' ArtifactRevision (Core.Maybe Types.RevisionId)
arRevisionId = Lens.field @"revisionId"
{-# INLINEABLE arRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

-- | Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
--
-- /Note:/ Consider using 'revisionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRevisionSummary :: Lens.Lens' ArtifactRevision (Core.Maybe Types.RevisionSummary)
arRevisionSummary = Lens.field @"revisionSummary"
{-# INLINEABLE arRevisionSummary #-}
{-# DEPRECATED revisionSummary "Use generic-lens or generic-optics with 'revisionSummary' instead"  #-}

-- | The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
--
-- /Note:/ Consider using 'revisionUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRevisionUrl :: Lens.Lens' ArtifactRevision (Core.Maybe Types.RevisionUrl)
arRevisionUrl = Lens.field @"revisionUrl"
{-# INLINEABLE arRevisionUrl #-}
{-# DEPRECATED revisionUrl "Use generic-lens or generic-optics with 'revisionUrl' instead"  #-}

instance Core.FromJSON ArtifactRevision where
        parseJSON
          = Core.withObject "ArtifactRevision" Core.$
              \ x ->
                ArtifactRevision' Core.<$>
                  (x Core..:? "created") Core.<*> x Core..:? "name" Core.<*>
                    x Core..:? "revisionChangeIdentifier"
                    Core.<*> x Core..:? "revisionId"
                    Core.<*> x Core..:? "revisionSummary"
                    Core.<*> x Core..:? "revisionUrl"
