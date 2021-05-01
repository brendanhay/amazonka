{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodePipeline.Types.ArtifactLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactLocation where

import Network.AWS.CodePipeline.Types.ArtifactLocationType
import Network.AWS.CodePipeline.Types.S3ArtifactLocation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about the location of an artifact.
--
-- /See:/ 'newArtifactLocation' smart constructor.
data ArtifactLocation = ArtifactLocation'
  { -- | The S3 bucket that contains the artifact.
    s3Location :: Prelude.Maybe S3ArtifactLocation,
    -- | The type of artifact in the location.
    type' :: Prelude.Maybe ArtifactLocationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ArtifactLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'artifactLocation_s3Location' - The S3 bucket that contains the artifact.
--
-- 'type'', 'artifactLocation_type' - The type of artifact in the location.
newArtifactLocation ::
  ArtifactLocation
newArtifactLocation =
  ArtifactLocation'
    { s3Location = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The S3 bucket that contains the artifact.
artifactLocation_s3Location :: Lens.Lens' ArtifactLocation (Prelude.Maybe S3ArtifactLocation)
artifactLocation_s3Location = Lens.lens (\ArtifactLocation' {s3Location} -> s3Location) (\s@ArtifactLocation' {} a -> s {s3Location = a} :: ArtifactLocation)

-- | The type of artifact in the location.
artifactLocation_type :: Lens.Lens' ArtifactLocation (Prelude.Maybe ArtifactLocationType)
artifactLocation_type = Lens.lens (\ArtifactLocation' {type'} -> type') (\s@ArtifactLocation' {} a -> s {type' = a} :: ArtifactLocation)

instance Prelude.FromJSON ArtifactLocation where
  parseJSON =
    Prelude.withObject
      "ArtifactLocation"
      ( \x ->
          ArtifactLocation'
            Prelude.<$> (x Prelude..:? "s3Location")
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable ArtifactLocation

instance Prelude.NFData ArtifactLocation
