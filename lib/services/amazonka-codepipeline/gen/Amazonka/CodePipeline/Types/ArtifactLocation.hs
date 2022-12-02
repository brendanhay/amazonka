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
-- Module      : Amazonka.CodePipeline.Types.ArtifactLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ArtifactLocation where

import Amazonka.CodePipeline.Types.ArtifactLocationType
import Amazonka.CodePipeline.Types.S3ArtifactLocation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the location of an artifact.
--
-- /See:/ 'newArtifactLocation' smart constructor.
data ArtifactLocation = ArtifactLocation'
  { -- | The type of artifact in the location.
    type' :: Prelude.Maybe ArtifactLocationType,
    -- | The S3 bucket that contains the artifact.
    s3Location :: Prelude.Maybe S3ArtifactLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArtifactLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'artifactLocation_type' - The type of artifact in the location.
--
-- 's3Location', 'artifactLocation_s3Location' - The S3 bucket that contains the artifact.
newArtifactLocation ::
  ArtifactLocation
newArtifactLocation =
  ArtifactLocation'
    { type' = Prelude.Nothing,
      s3Location = Prelude.Nothing
    }

-- | The type of artifact in the location.
artifactLocation_type :: Lens.Lens' ArtifactLocation (Prelude.Maybe ArtifactLocationType)
artifactLocation_type = Lens.lens (\ArtifactLocation' {type'} -> type') (\s@ArtifactLocation' {} a -> s {type' = a} :: ArtifactLocation)

-- | The S3 bucket that contains the artifact.
artifactLocation_s3Location :: Lens.Lens' ArtifactLocation (Prelude.Maybe S3ArtifactLocation)
artifactLocation_s3Location = Lens.lens (\ArtifactLocation' {s3Location} -> s3Location) (\s@ArtifactLocation' {} a -> s {s3Location = a} :: ArtifactLocation)

instance Data.FromJSON ArtifactLocation where
  parseJSON =
    Data.withObject
      "ArtifactLocation"
      ( \x ->
          ArtifactLocation'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "s3Location")
      )

instance Prelude.Hashable ArtifactLocation where
  hashWithSalt _salt ArtifactLocation' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` s3Location

instance Prelude.NFData ArtifactLocation where
  rnf ArtifactLocation' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf s3Location
