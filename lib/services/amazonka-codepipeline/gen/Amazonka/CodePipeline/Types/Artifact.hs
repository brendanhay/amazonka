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
-- Module      : Amazonka.CodePipeline.Types.Artifact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.Artifact where

import Amazonka.CodePipeline.Types.ArtifactLocation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about an artifact that is worked on by actions in
-- the pipeline.
--
-- /See:/ 'newArtifact' smart constructor.
data Artifact = Artifact'
  { -- | The artifact\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The artifact\'s revision ID. Depending on the type of object, this could
    -- be a commit ID (GitHub) or a revision ID (Amazon S3).
    revision :: Prelude.Maybe Prelude.Text,
    -- | The location of an artifact.
    location :: Prelude.Maybe ArtifactLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Artifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'artifact_name' - The artifact\'s name.
--
-- 'revision', 'artifact_revision' - The artifact\'s revision ID. Depending on the type of object, this could
-- be a commit ID (GitHub) or a revision ID (Amazon S3).
--
-- 'location', 'artifact_location' - The location of an artifact.
newArtifact ::
  Artifact
newArtifact =
  Artifact'
    { name = Prelude.Nothing,
      revision = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The artifact\'s name.
artifact_name :: Lens.Lens' Artifact (Prelude.Maybe Prelude.Text)
artifact_name = Lens.lens (\Artifact' {name} -> name) (\s@Artifact' {} a -> s {name = a} :: Artifact)

-- | The artifact\'s revision ID. Depending on the type of object, this could
-- be a commit ID (GitHub) or a revision ID (Amazon S3).
artifact_revision :: Lens.Lens' Artifact (Prelude.Maybe Prelude.Text)
artifact_revision = Lens.lens (\Artifact' {revision} -> revision) (\s@Artifact' {} a -> s {revision = a} :: Artifact)

-- | The location of an artifact.
artifact_location :: Lens.Lens' Artifact (Prelude.Maybe ArtifactLocation)
artifact_location = Lens.lens (\Artifact' {location} -> location) (\s@Artifact' {} a -> s {location = a} :: Artifact)

instance Data.FromJSON Artifact where
  parseJSON =
    Data.withObject
      "Artifact"
      ( \x ->
          Artifact'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "revision")
            Prelude.<*> (x Data..:? "location")
      )

instance Prelude.Hashable Artifact where
  hashWithSalt _salt Artifact' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` location

instance Prelude.NFData Artifact where
  rnf Artifact' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf location
