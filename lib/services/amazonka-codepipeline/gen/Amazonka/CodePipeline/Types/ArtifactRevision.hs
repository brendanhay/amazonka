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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | An additional identifier for a revision, such as a commit date or, for
    -- artifacts stored in Amazon S3 buckets, the ETag value.
    revisionChangeIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of an artifact. This name might be system-generated, such as
    -- \"MyApp\", or defined by the user when an action is created.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the most recent revision of the artifact was
    -- created, in timestamp format.
    created :: Prelude.Maybe Data.POSIX,
    -- | The revision ID of the artifact.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The commit ID for the artifact revision. For artifacts stored in GitHub
    -- or AWS CodeCommit repositories, the commit ID is linked to a commit
    -- details page.
    revisionUrl :: Prelude.Maybe Prelude.Text,
    -- | Summary information about the most recent revision of the artifact. For
    -- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
    -- S3 buckets or actions, the user-provided content of a
    -- @codepipeline-artifact-revision-summary@ key specified in the object
    -- metadata.
    revisionSummary :: Prelude.Maybe Prelude.Text
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
-- 'revisionChangeIdentifier', 'artifactRevision_revisionChangeIdentifier' - An additional identifier for a revision, such as a commit date or, for
-- artifacts stored in Amazon S3 buckets, the ETag value.
--
-- 'name', 'artifactRevision_name' - The name of an artifact. This name might be system-generated, such as
-- \"MyApp\", or defined by the user when an action is created.
--
-- 'created', 'artifactRevision_created' - The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
--
-- 'revisionId', 'artifactRevision_revisionId' - The revision ID of the artifact.
--
-- 'revisionUrl', 'artifactRevision_revisionUrl' - The commit ID for the artifact revision. For artifacts stored in GitHub
-- or AWS CodeCommit repositories, the commit ID is linked to a commit
-- details page.
--
-- 'revisionSummary', 'artifactRevision_revisionSummary' - Summary information about the most recent revision of the artifact. For
-- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
-- S3 buckets or actions, the user-provided content of a
-- @codepipeline-artifact-revision-summary@ key specified in the object
-- metadata.
newArtifactRevision ::
  ArtifactRevision
newArtifactRevision =
  ArtifactRevision'
    { revisionChangeIdentifier =
        Prelude.Nothing,
      name = Prelude.Nothing,
      created = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      revisionUrl = Prelude.Nothing,
      revisionSummary = Prelude.Nothing
    }

-- | An additional identifier for a revision, such as a commit date or, for
-- artifacts stored in Amazon S3 buckets, the ETag value.
artifactRevision_revisionChangeIdentifier :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_revisionChangeIdentifier = Lens.lens (\ArtifactRevision' {revisionChangeIdentifier} -> revisionChangeIdentifier) (\s@ArtifactRevision' {} a -> s {revisionChangeIdentifier = a} :: ArtifactRevision)

-- | The name of an artifact. This name might be system-generated, such as
-- \"MyApp\", or defined by the user when an action is created.
artifactRevision_name :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_name = Lens.lens (\ArtifactRevision' {name} -> name) (\s@ArtifactRevision' {} a -> s {name = a} :: ArtifactRevision)

-- | The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
artifactRevision_created :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.UTCTime)
artifactRevision_created = Lens.lens (\ArtifactRevision' {created} -> created) (\s@ArtifactRevision' {} a -> s {created = a} :: ArtifactRevision) Prelude.. Lens.mapping Data._Time

-- | The revision ID of the artifact.
artifactRevision_revisionId :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_revisionId = Lens.lens (\ArtifactRevision' {revisionId} -> revisionId) (\s@ArtifactRevision' {} a -> s {revisionId = a} :: ArtifactRevision)

-- | The commit ID for the artifact revision. For artifacts stored in GitHub
-- or AWS CodeCommit repositories, the commit ID is linked to a commit
-- details page.
artifactRevision_revisionUrl :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_revisionUrl = Lens.lens (\ArtifactRevision' {revisionUrl} -> revisionUrl) (\s@ArtifactRevision' {} a -> s {revisionUrl = a} :: ArtifactRevision)

-- | Summary information about the most recent revision of the artifact. For
-- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
-- S3 buckets or actions, the user-provided content of a
-- @codepipeline-artifact-revision-summary@ key specified in the object
-- metadata.
artifactRevision_revisionSummary :: Lens.Lens' ArtifactRevision (Prelude.Maybe Prelude.Text)
artifactRevision_revisionSummary = Lens.lens (\ArtifactRevision' {revisionSummary} -> revisionSummary) (\s@ArtifactRevision' {} a -> s {revisionSummary = a} :: ArtifactRevision)

instance Data.FromJSON ArtifactRevision where
  parseJSON =
    Data.withObject
      "ArtifactRevision"
      ( \x ->
          ArtifactRevision'
            Prelude.<$> (x Data..:? "revisionChangeIdentifier")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "revisionId")
            Prelude.<*> (x Data..:? "revisionUrl")
            Prelude.<*> (x Data..:? "revisionSummary")
      )

instance Prelude.Hashable ArtifactRevision where
  hashWithSalt _salt ArtifactRevision' {..} =
    _salt
      `Prelude.hashWithSalt` revisionChangeIdentifier
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` revisionUrl
      `Prelude.hashWithSalt` revisionSummary

instance Prelude.NFData ArtifactRevision where
  rnf ArtifactRevision' {..} =
    Prelude.rnf revisionChangeIdentifier
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf revisionUrl
      `Prelude.seq` Prelude.rnf revisionSummary
