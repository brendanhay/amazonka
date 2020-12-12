{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.SourceRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.SourceRevision
  ( SourceRevision (..),

    -- * Smart constructor
    mkSourceRevision,

    -- * Lenses
    srRevisionSummary,
    srRevisionURL,
    srRevisionId,
    srActionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the version (or revision) of a source artifact that initiated a pipeline execution.
--
-- /See:/ 'mkSourceRevision' smart constructor.
data SourceRevision = SourceRevision'
  { revisionSummary ::
      Lude.Maybe Lude.Text,
    revisionURL :: Lude.Maybe Lude.Text,
    revisionId :: Lude.Maybe Lude.Text,
    actionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceRevision' with the minimum fields required to make a request.
--
-- * 'actionName' - The name of the action that processed the revision to the source artifact.
-- * 'revisionId' - The system-generated unique ID that identifies the revision number of the artifact.
-- * 'revisionSummary' - Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
-- * 'revisionURL' - The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
mkSourceRevision ::
  -- | 'actionName'
  Lude.Text ->
  SourceRevision
mkSourceRevision pActionName_ =
  SourceRevision'
    { revisionSummary = Lude.Nothing,
      revisionURL = Lude.Nothing,
      revisionId = Lude.Nothing,
      actionName = pActionName_
    }

-- | Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
--
-- /Note:/ Consider using 'revisionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRevisionSummary :: Lens.Lens' SourceRevision (Lude.Maybe Lude.Text)
srRevisionSummary = Lens.lens (revisionSummary :: SourceRevision -> Lude.Maybe Lude.Text) (\s a -> s {revisionSummary = a} :: SourceRevision)
{-# DEPRECATED srRevisionSummary "Use generic-lens or generic-optics with 'revisionSummary' instead." #-}

-- | The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
--
-- /Note:/ Consider using 'revisionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRevisionURL :: Lens.Lens' SourceRevision (Lude.Maybe Lude.Text)
srRevisionURL = Lens.lens (revisionURL :: SourceRevision -> Lude.Maybe Lude.Text) (\s a -> s {revisionURL = a} :: SourceRevision)
{-# DEPRECATED srRevisionURL "Use generic-lens or generic-optics with 'revisionURL' instead." #-}

-- | The system-generated unique ID that identifies the revision number of the artifact.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRevisionId :: Lens.Lens' SourceRevision (Lude.Maybe Lude.Text)
srRevisionId = Lens.lens (revisionId :: SourceRevision -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: SourceRevision)
{-# DEPRECATED srRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | The name of the action that processed the revision to the source artifact.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srActionName :: Lens.Lens' SourceRevision Lude.Text
srActionName = Lens.lens (actionName :: SourceRevision -> Lude.Text) (\s a -> s {actionName = a} :: SourceRevision)
{-# DEPRECATED srActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

instance Lude.FromJSON SourceRevision where
  parseJSON =
    Lude.withObject
      "SourceRevision"
      ( \x ->
          SourceRevision'
            Lude.<$> (x Lude..:? "revisionSummary")
            Lude.<*> (x Lude..:? "revisionUrl")
            Lude.<*> (x Lude..:? "revisionId")
            Lude.<*> (x Lude..: "actionName")
      )
