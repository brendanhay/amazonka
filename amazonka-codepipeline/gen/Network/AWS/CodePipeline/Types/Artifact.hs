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
-- Module      : Network.AWS.CodePipeline.Types.Artifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.Artifact where

import Network.AWS.CodePipeline.Types.ArtifactLocation
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents information about an artifact that is worked on by actions in
-- the pipeline.
--
-- /See:/ 'newArtifact' smart constructor.
data Artifact = Artifact'
  { -- | The artifact\'s name.
    name :: Core.Maybe Core.Text,
    -- | The artifact\'s revision ID. Depending on the type of object, this could
    -- be a commit ID (GitHub) or a revision ID (Amazon S3).
    revision :: Core.Maybe Core.Text,
    -- | The location of an artifact.
    location :: Core.Maybe ArtifactLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { name = Core.Nothing,
      revision = Core.Nothing,
      location = Core.Nothing
    }

-- | The artifact\'s name.
artifact_name :: Lens.Lens' Artifact (Core.Maybe Core.Text)
artifact_name = Lens.lens (\Artifact' {name} -> name) (\s@Artifact' {} a -> s {name = a} :: Artifact)

-- | The artifact\'s revision ID. Depending on the type of object, this could
-- be a commit ID (GitHub) or a revision ID (Amazon S3).
artifact_revision :: Lens.Lens' Artifact (Core.Maybe Core.Text)
artifact_revision = Lens.lens (\Artifact' {revision} -> revision) (\s@Artifact' {} a -> s {revision = a} :: Artifact)

-- | The location of an artifact.
artifact_location :: Lens.Lens' Artifact (Core.Maybe ArtifactLocation)
artifact_location = Lens.lens (\Artifact' {location} -> location) (\s@Artifact' {} a -> s {location = a} :: Artifact)

instance Core.FromJSON Artifact where
  parseJSON =
    Core.withObject
      "Artifact"
      ( \x ->
          Artifact'
            Core.<$> (x Core..:? "name")
            Core.<*> (x Core..:? "revision")
            Core.<*> (x Core..:? "location")
      )

instance Core.Hashable Artifact

instance Core.NFData Artifact
