{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactRevision
  ( ArtifactRevision (..),

    -- * Smart constructor
    mkArtifactRevision,

    -- * Lenses
    arRevisionSummary,
    arRevisionURL,
    arCreated,
    arName,
    arRevisionId,
    arRevisionChangeIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents revision details of an artifact.
--
-- /See:/ 'mkArtifactRevision' smart constructor.
data ArtifactRevision = ArtifactRevision'
  { revisionSummary ::
      Lude.Maybe Lude.Text,
    revisionURL :: Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    revisionId :: Lude.Maybe Lude.Text,
    revisionChangeIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArtifactRevision' with the minimum fields required to make a request.
--
-- * 'created' - The date and time when the most recent revision of the artifact was created, in timestamp format.
-- * 'name' - The name of an artifact. This name might be system-generated, such as "MyApp", or defined by the user when an action is created.
-- * 'revisionChangeIdentifier' - An additional identifier for a revision, such as a commit date or, for artifacts stored in Amazon S3 buckets, the ETag value.
-- * 'revisionId' - The revision ID of the artifact.
-- * 'revisionSummary' - Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
-- * 'revisionURL' - The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
mkArtifactRevision ::
  ArtifactRevision
mkArtifactRevision =
  ArtifactRevision'
    { revisionSummary = Lude.Nothing,
      revisionURL = Lude.Nothing,
      created = Lude.Nothing,
      name = Lude.Nothing,
      revisionId = Lude.Nothing,
      revisionChangeIdentifier = Lude.Nothing
    }

-- | Summary information about the most recent revision of the artifact. For GitHub and AWS CodeCommit repositories, the commit message. For Amazon S3 buckets or actions, the user-provided content of a @codepipeline-artifact-revision-summary@ key specified in the object metadata.
--
-- /Note:/ Consider using 'revisionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRevisionSummary :: Lens.Lens' ArtifactRevision (Lude.Maybe Lude.Text)
arRevisionSummary = Lens.lens (revisionSummary :: ArtifactRevision -> Lude.Maybe Lude.Text) (\s a -> s {revisionSummary = a} :: ArtifactRevision)
{-# DEPRECATED arRevisionSummary "Use generic-lens or generic-optics with 'revisionSummary' instead." #-}

-- | The commit ID for the artifact revision. For artifacts stored in GitHub or AWS CodeCommit repositories, the commit ID is linked to a commit details page.
--
-- /Note:/ Consider using 'revisionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRevisionURL :: Lens.Lens' ArtifactRevision (Lude.Maybe Lude.Text)
arRevisionURL = Lens.lens (revisionURL :: ArtifactRevision -> Lude.Maybe Lude.Text) (\s a -> s {revisionURL = a} :: ArtifactRevision)
{-# DEPRECATED arRevisionURL "Use generic-lens or generic-optics with 'revisionURL' instead." #-}

-- | The date and time when the most recent revision of the artifact was created, in timestamp format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arCreated :: Lens.Lens' ArtifactRevision (Lude.Maybe Lude.Timestamp)
arCreated = Lens.lens (created :: ArtifactRevision -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: ArtifactRevision)
{-# DEPRECATED arCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The name of an artifact. This name might be system-generated, such as "MyApp", or defined by the user when an action is created.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arName :: Lens.Lens' ArtifactRevision (Lude.Maybe Lude.Text)
arName = Lens.lens (name :: ArtifactRevision -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ArtifactRevision)
{-# DEPRECATED arName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The revision ID of the artifact.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRevisionId :: Lens.Lens' ArtifactRevision (Lude.Maybe Lude.Text)
arRevisionId = Lens.lens (revisionId :: ArtifactRevision -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: ArtifactRevision)
{-# DEPRECATED arRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

-- | An additional identifier for a revision, such as a commit date or, for artifacts stored in Amazon S3 buckets, the ETag value.
--
-- /Note:/ Consider using 'revisionChangeIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arRevisionChangeIdentifier :: Lens.Lens' ArtifactRevision (Lude.Maybe Lude.Text)
arRevisionChangeIdentifier = Lens.lens (revisionChangeIdentifier :: ArtifactRevision -> Lude.Maybe Lude.Text) (\s a -> s {revisionChangeIdentifier = a} :: ArtifactRevision)
{-# DEPRECATED arRevisionChangeIdentifier "Use generic-lens or generic-optics with 'revisionChangeIdentifier' instead." #-}

instance Lude.FromJSON ArtifactRevision where
  parseJSON =
    Lude.withObject
      "ArtifactRevision"
      ( \x ->
          ArtifactRevision'
            Lude.<$> (x Lude..:? "revisionSummary")
            Lude.<*> (x Lude..:? "revisionUrl")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "revisionId")
            Lude.<*> (x Lude..:? "revisionChangeIdentifier")
      )
