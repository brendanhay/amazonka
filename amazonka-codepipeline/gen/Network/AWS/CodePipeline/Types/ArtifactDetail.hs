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
-- Module      : Network.AWS.CodePipeline.Types.ArtifactDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactDetail where

import Network.AWS.CodePipeline.Types.S3Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Artifact details for the action execution, such as the artifact
-- location.
--
-- /See:/ 'newArtifactDetail' smart constructor.
data ArtifactDetail = ArtifactDetail'
  { -- | The Amazon S3 artifact location for the action execution.
    s3location :: Prelude.Maybe S3Location,
    -- | The artifact object name for the action execution.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ArtifactDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3location', 'artifactDetail_s3location' - The Amazon S3 artifact location for the action execution.
--
-- 'name', 'artifactDetail_name' - The artifact object name for the action execution.
newArtifactDetail ::
  ArtifactDetail
newArtifactDetail =
  ArtifactDetail'
    { s3location = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon S3 artifact location for the action execution.
artifactDetail_s3location :: Lens.Lens' ArtifactDetail (Prelude.Maybe S3Location)
artifactDetail_s3location = Lens.lens (\ArtifactDetail' {s3location} -> s3location) (\s@ArtifactDetail' {} a -> s {s3location = a} :: ArtifactDetail)

-- | The artifact object name for the action execution.
artifactDetail_name :: Lens.Lens' ArtifactDetail (Prelude.Maybe Prelude.Text)
artifactDetail_name = Lens.lens (\ArtifactDetail' {name} -> name) (\s@ArtifactDetail' {} a -> s {name = a} :: ArtifactDetail)

instance Prelude.FromJSON ArtifactDetail where
  parseJSON =
    Prelude.withObject
      "ArtifactDetail"
      ( \x ->
          ArtifactDetail'
            Prelude.<$> (x Prelude..:? "s3location")
            Prelude.<*> (x Prelude..:? "name")
      )

instance Prelude.Hashable ArtifactDetail

instance Prelude.NFData ArtifactDetail
