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
-- Module      : Amazonka.CodePipeline.Types.ArtifactRevision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ArtifactRevision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents revision details of an artifact.
--
-- /See:/ 'newArtifactRevision' smart constructor.
data ArtifactRevision = ArtifactRevision'
  { -- | The date and time when the most recent revision of the artifact was
    -- created, in timestamp format.
    created :: Prelude.Maybe Data.POSIX,
    -- | The name of an artifact. This name might be system-generated, such as
    -- \"MyApp\", or defined by the user when an action is created.
    name :: Prelude.Maybe Prelude.Text,
    -- | An additional identifier for a revision, such as a commit date or, for
    -- artifacts stored in Amazon S3 buckets, the ETag value.
    revisionChangeIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The revision ID of the artifact.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | Summary information about the most recent revision of the artifact. For
    -- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
    -- S3 buckets or actions, the user-provided content of a
    -- @codepipeline-artifact-revision-summary@ key specified in the object
    -- metadata.
    revisionSummary :: Prelude.Maybe Prelude.Text,
    -- | The commit ID for the artifact revision. For artifacts stored in GitHub
    -- or AWS CodeCommit repositories, the commit ID is linked to a commit
    -- details page.
    revisionUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArtifactRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'created', 'artifactRevision_created' - The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
--
-- 'name', 'artifactRevision_name' - The name of an artifact. This name might be system-generated, such as
-- \"MyApp\", or defined by the user when an action is created.
--
-- 'revisionChangeIdentifier', 'artifactRevision_revisionChangeIdentifier' - An additional identifier for a revision, such as a commit date or, for
-- artifacts stored in Amazon S3 buckets, the ETag value.
--
-- 'revisionId', 'artifactRevision_revisionId' - The revision ID of the artifact.
--
-- 'revisionSummary', 'artifactRevision_revisionSummary' - Summary information about the most recent revision of the artifact. For
-- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
-- S3 buckets or actions, the user-provided content of a
-- @codepipeline-artifact-revision-summary@ key specified in the object
-- metadata.
--
-- 'revisionUrl', 'artifactRevision_revisionUrl' - The commit ID for the artifact revision. For artifacts stored in GitHub
-- or AWS CodeCommit repositories, the commit ID is linked to a commit
-- details page.
newArtifactRevision ::
  ArtifactRevision
newArtifactRevision =
  ArtifactRevision'
    { created = Prelude.Nothing,
      name = Prelude.Nothing,
      revisionChangeIdentifier = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      revisionSummary = Prelude.Nothing,
      revisionUrl = Prelude.Nothing
    }

-- | The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
artifactRevision_created :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.UTCTime)
artifactRevision_created = Lens.lens (\ArtifactRevision' {created} -> created) (\s@ArtifactRevision' {} a -> s {created = a} :: ArtifactRevision) Prelude.. Lens.mapping Data._Time

-- | The name of an artifact. This name might be system-generated, such as
-- \"MyApp\", or defined by the user when an action is created.
artifactRevision_name :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_name = Lens.lens (\ArtifactRevision' {name} -> name) (\s@ArtifactRevision' {} a -> s {name = a} :: ArtifactRevision)

-- | An additional identifier for a revision, such as a commit date or, for
-- artifacts stored in Amazon S3 buckets, the ETag value.
artifactRevision_revisionChangeIdentifier :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_revisionChangeIdentifier = Lens.lens (\ArtifactRevision' {revisionChangeIdentifier} -> revisionChangeIdentifier) (\s@ArtifactRevision' {} a -> s {revisionChangeIdentifier = a} :: ArtifactRevision)

-- | The revision ID of the artifact.
artifactRevision_revisionId :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_revisionId = Lens.lens (\ArtifactRevision' {revisionId} -> revisionId) (\s@ArtifactRevision' {} a -> s {revisionId = a} :: ArtifactRevision)

-- | Summary information about the most recent revision of the artifact. For
-- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
-- S3 buckets or actions, the user-provided content of a
-- @codepipeline-artifact-revision-summary@ key specified in the object
-- metadata.
artifactRevision_revisionSummary :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_revisionSummary = Lens.lens (\ArtifactRevision' {revisionSummary} -> revisionSummary) (\s@ArtifactRevision' {} a -> s {revisionSummary = a} :: ArtifactRevision)

-- | The commit ID for the artifact revision. For artifacts stored in GitHub
-- or AWS CodeCommit repositories, the commit ID is linked to a commit
-- details page.
artifactRevision_revisionUrl :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_revisionUrl = Lens.lens (\ArtifactRevision' {revisionUrl} -> revisionUrl) (\s@ArtifactRevision' {} a -> s {revisionUrl = a} :: ArtifactRevision)

instance Data.FromJSON ArtifactRevision where
  parseJSON =
    Data.withObject
      "ArtifactRevision"
      ( \x ->
          ArtifactRevision'
            Prelude.<$> (x Data..:? "created")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "revisionChangeIdentifier")
            Prelude.<*> (x Data..:? "revisionId")
            Prelude.<*> (x Data..:? "revisionSummary")
            Prelude.<*> (x Data..:? "revisionUrl")
      )

instance Prelude.Hashable ArtifactRevision where
  hashWithSalt _salt ArtifactRevision' {..} =
    _salt
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` revisionChangeIdentifier
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` revisionSummary
      `Prelude.hashWithSalt` revisionUrl

instance Prelude.NFData ArtifactRevision where
  rnf ArtifactRevision' {..} =
    Prelude.rnf created
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf revisionChangeIdentifier
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf revisionSummary
      `Prelude.seq` Prelude.rnf revisionUrl
