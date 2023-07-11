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
-- Module      : Amazonka.CodePipeline.Types.ArtifactDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ArtifactDetail where

import Amazonka.CodePipeline.Types.S3Location
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Artifact details for the action execution, such as the artifact
-- location.
--
-- /See:/ 'newArtifactDetail' smart constructor.
data ArtifactDetail = ArtifactDetail'
  { -- | The artifact object name for the action execution.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 artifact location for the action execution.
    s3location :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArtifactDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'artifactDetail_name' - The artifact object name for the action execution.
--
-- 's3location', 'artifactDetail_s3location' - The Amazon S3 artifact location for the action execution.
newArtifactDetail ::
  ArtifactDetail
newArtifactDetail =
  ArtifactDetail'
    { name = Prelude.Nothing,
      s3location = Prelude.Nothing
    }

-- | The artifact object name for the action execution.
artifactDetail_name :: Lens.Lens' ArtifactDetail (Prelude.Maybe Prelude.Text)
artifactDetail_name = Lens.lens (\ArtifactDetail' {name} -> name) (\s@ArtifactDetail' {} a -> s {name = a} :: ArtifactDetail)

-- | The Amazon S3 artifact location for the action execution.
artifactDetail_s3location :: Lens.Lens' ArtifactDetail (Prelude.Maybe S3Location)
artifactDetail_s3location = Lens.lens (\ArtifactDetail' {s3location} -> s3location) (\s@ArtifactDetail' {} a -> s {s3location = a} :: ArtifactDetail)

instance Data.FromJSON ArtifactDetail where
  parseJSON =
    Data.withObject
      "ArtifactDetail"
      ( \x ->
          ArtifactDetail'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "s3location")
      )

instance Prelude.Hashable ArtifactDetail where
  hashWithSalt _salt ArtifactDetail' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3location

instance Prelude.NFData ArtifactDetail where
  rnf ArtifactDetail' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf s3location
