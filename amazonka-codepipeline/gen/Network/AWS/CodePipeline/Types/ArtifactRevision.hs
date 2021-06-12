{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactRevision where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents revision details of an artifact.
--
-- /See:/ 'newArtifactRevision' smart constructor.
data ArtifactRevision = ArtifactRevision'
  { -- | The revision ID of the artifact.
    revisionId :: Core.Maybe Core.Text,
    -- | An additional identifier for a revision, such as a commit date or, for
    -- artifacts stored in Amazon S3 buckets, the ETag value.
    revisionChangeIdentifier :: Core.Maybe Core.Text,
    -- | The name of an artifact. This name might be system-generated, such as
    -- \"MyApp\", or defined by the user when an action is created.
    name :: Core.Maybe Core.Text,
    -- | Summary information about the most recent revision of the artifact. For
    -- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
    -- S3 buckets or actions, the user-provided content of a
    -- @codepipeline-artifact-revision-summary@ key specified in the object
    -- metadata.
    revisionSummary :: Core.Maybe Core.Text,
    -- | The date and time when the most recent revision of the artifact was
    -- created, in timestamp format.
    created :: Core.Maybe Core.POSIX,
    -- | The commit ID for the artifact revision. For artifacts stored in GitHub
    -- or AWS CodeCommit repositories, the commit ID is linked to a commit
    -- details page.
    revisionUrl :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ArtifactRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'artifactRevision_revisionId' - The revision ID of the artifact.
--
-- 'revisionChangeIdentifier', 'artifactRevision_revisionChangeIdentifier' - An additional identifier for a revision, such as a commit date or, for
-- artifacts stored in Amazon S3 buckets, the ETag value.
--
-- 'name', 'artifactRevision_name' - The name of an artifact. This name might be system-generated, such as
-- \"MyApp\", or defined by the user when an action is created.
--
-- 'revisionSummary', 'artifactRevision_revisionSummary' - Summary information about the most recent revision of the artifact. For
-- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
-- S3 buckets or actions, the user-provided content of a
-- @codepipeline-artifact-revision-summary@ key specified in the object
-- metadata.
--
-- 'created', 'artifactRevision_created' - The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
--
-- 'revisionUrl', 'artifactRevision_revisionUrl' - The commit ID for the artifact revision. For artifacts stored in GitHub
-- or AWS CodeCommit repositories, the commit ID is linked to a commit
-- details page.
newArtifactRevision ::
  ArtifactRevision
newArtifactRevision =
  ArtifactRevision'
    { revisionId = Core.Nothing,
      revisionChangeIdentifier = Core.Nothing,
      name = Core.Nothing,
      revisionSummary = Core.Nothing,
      created = Core.Nothing,
      revisionUrl = Core.Nothing
    }

-- | The revision ID of the artifact.
artifactRevision_revisionId :: Lens.Lens' ArtifactRevision (Core.Maybe Core.Text)
artifactRevision_revisionId = Lens.lens (\ArtifactRevision' {revisionId} -> revisionId) (\s@ArtifactRevision' {} a -> s {revisionId = a} :: ArtifactRevision)

-- | An additional identifier for a revision, such as a commit date or, for
-- artifacts stored in Amazon S3 buckets, the ETag value.
artifactRevision_revisionChangeIdentifier :: Lens.Lens' ArtifactRevision (Core.Maybe Core.Text)
artifactRevision_revisionChangeIdentifier = Lens.lens (\ArtifactRevision' {revisionChangeIdentifier} -> revisionChangeIdentifier) (\s@ArtifactRevision' {} a -> s {revisionChangeIdentifier = a} :: ArtifactRevision)

-- | The name of an artifact. This name might be system-generated, such as
-- \"MyApp\", or defined by the user when an action is created.
artifactRevision_name :: Lens.Lens' ArtifactRevision (Core.Maybe Core.Text)
artifactRevision_name = Lens.lens (\ArtifactRevision' {name} -> name) (\s@ArtifactRevision' {} a -> s {name = a} :: ArtifactRevision)

-- | Summary information about the most recent revision of the artifact. For
-- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
-- S3 buckets or actions, the user-provided content of a
-- @codepipeline-artifact-revision-summary@ key specified in the object
-- metadata.
artifactRevision_revisionSummary :: Lens.Lens' ArtifactRevision (Core.Maybe Core.Text)
artifactRevision_revisionSummary = Lens.lens (\ArtifactRevision' {revisionSummary} -> revisionSummary) (\s@ArtifactRevision' {} a -> s {revisionSummary = a} :: ArtifactRevision)

-- | The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
artifactRevision_created :: Lens.Lens' ArtifactRevision (Core.Maybe Core.UTCTime)
artifactRevision_created = Lens.lens (\ArtifactRevision' {created} -> created) (\s@ArtifactRevision' {} a -> s {created = a} :: ArtifactRevision) Core.. Lens.mapping Core._Time

-- | The commit ID for the artifact revision. For artifacts stored in GitHub
-- or AWS CodeCommit repositories, the commit ID is linked to a commit
-- details page.
artifactRevision_revisionUrl :: Lens.Lens' ArtifactRevision (Core.Maybe Core.Text)
artifactRevision_revisionUrl = Lens.lens (\ArtifactRevision' {revisionUrl} -> revisionUrl) (\s@ArtifactRevision' {} a -> s {revisionUrl = a} :: ArtifactRevision)

instance Core.FromJSON ArtifactRevision where
  parseJSON =
    Core.withObject
      "ArtifactRevision"
      ( \x ->
          ArtifactRevision'
            Core.<$> (x Core..:? "revisionId")
            Core.<*> (x Core..:? "revisionChangeIdentifier")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "revisionSummary")
            Core.<*> (x Core..:? "created")
            Core.<*> (x Core..:? "revisionUrl")
      )

instance Core.Hashable ArtifactRevision

instance Core.NFData ArtifactRevision
