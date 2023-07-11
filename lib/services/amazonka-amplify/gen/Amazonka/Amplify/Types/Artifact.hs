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
-- Module      : Amazonka.Amplify.Types.Artifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.Artifact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an artifact.
--
-- /See:/ 'newArtifact' smart constructor.
data Artifact = Artifact'
  { -- | The file name for the artifact.
    artifactFileName :: Prelude.Text,
    -- | The unique ID for the artifact.
    artifactId :: Prelude.Text
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
-- 'artifactFileName', 'artifact_artifactFileName' - The file name for the artifact.
--
-- 'artifactId', 'artifact_artifactId' - The unique ID for the artifact.
newArtifact ::
  -- | 'artifactFileName'
  Prelude.Text ->
  -- | 'artifactId'
  Prelude.Text ->
  Artifact
newArtifact pArtifactFileName_ pArtifactId_ =
  Artifact'
    { artifactFileName = pArtifactFileName_,
      artifactId = pArtifactId_
    }

-- | The file name for the artifact.
artifact_artifactFileName :: Lens.Lens' Artifact Prelude.Text
artifact_artifactFileName = Lens.lens (\Artifact' {artifactFileName} -> artifactFileName) (\s@Artifact' {} a -> s {artifactFileName = a} :: Artifact)

-- | The unique ID for the artifact.
artifact_artifactId :: Lens.Lens' Artifact Prelude.Text
artifact_artifactId = Lens.lens (\Artifact' {artifactId} -> artifactId) (\s@Artifact' {} a -> s {artifactId = a} :: Artifact)

instance Data.FromJSON Artifact where
  parseJSON =
    Data.withObject
      "Artifact"
      ( \x ->
          Artifact'
            Prelude.<$> (x Data..: "artifactFileName")
            Prelude.<*> (x Data..: "artifactId")
      )

instance Prelude.Hashable Artifact where
  hashWithSalt _salt Artifact' {..} =
    _salt
      `Prelude.hashWithSalt` artifactFileName
      `Prelude.hashWithSalt` artifactId

instance Prelude.NFData Artifact where
  rnf Artifact' {..} =
    Prelude.rnf artifactFileName
      `Prelude.seq` Prelude.rnf artifactId
