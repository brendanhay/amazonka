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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Artifact details for the action execution, such as the artifact
-- location.
--
-- /See:/ 'newArtifactDetail' smart constructor.
data ArtifactDetail = ArtifactDetail'
  { -- | The Amazon S3 artifact location for the action execution.
    s3location :: Core.Maybe S3Location,
    -- | The artifact object name for the action execution.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { s3location = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon S3 artifact location for the action execution.
artifactDetail_s3location :: Lens.Lens' ArtifactDetail (Core.Maybe S3Location)
artifactDetail_s3location = Lens.lens (\ArtifactDetail' {s3location} -> s3location) (\s@ArtifactDetail' {} a -> s {s3location = a} :: ArtifactDetail)

-- | The artifact object name for the action execution.
artifactDetail_name :: Lens.Lens' ArtifactDetail (Core.Maybe Core.Text)
artifactDetail_name = Lens.lens (\ArtifactDetail' {name} -> name) (\s@ArtifactDetail' {} a -> s {name = a} :: ArtifactDetail)

instance Core.FromJSON ArtifactDetail where
  parseJSON =
    Core.withObject
      "ArtifactDetail"
      ( \x ->
          ArtifactDetail'
            Core.<$> (x Core..:? "s3location")
            Core.<*> (x Core..:? "name")
      )

instance Core.Hashable ArtifactDetail

instance Core.NFData ArtifactDetail
