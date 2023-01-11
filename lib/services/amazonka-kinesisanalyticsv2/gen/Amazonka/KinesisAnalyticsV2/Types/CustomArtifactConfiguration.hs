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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CustomArtifactConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CustomArtifactConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ArtifactType
import Amazonka.KinesisAnalyticsV2.Types.MavenReference
import Amazonka.KinesisAnalyticsV2.Types.S3ContentLocation
import qualified Amazonka.Prelude as Prelude

-- | Specifies dependency JARs, as well as JAR files that contain
-- user-defined functions (UDF).
--
-- /See:/ 'newCustomArtifactConfiguration' smart constructor.
data CustomArtifactConfiguration = CustomArtifactConfiguration'
  { -- | The parameters required to fully specify a Maven reference.
    mavenReference :: Prelude.Maybe MavenReference,
    s3ContentLocation :: Prelude.Maybe S3ContentLocation,
    -- | @UDF@ stands for user-defined functions. This type of artifact must be
    -- in an S3 bucket. A @DEPENDENCY_JAR@ can be in either Maven or an S3
    -- bucket.
    artifactType :: ArtifactType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomArtifactConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mavenReference', 'customArtifactConfiguration_mavenReference' - The parameters required to fully specify a Maven reference.
--
-- 's3ContentLocation', 'customArtifactConfiguration_s3ContentLocation' - Undocumented member.
--
-- 'artifactType', 'customArtifactConfiguration_artifactType' - @UDF@ stands for user-defined functions. This type of artifact must be
-- in an S3 bucket. A @DEPENDENCY_JAR@ can be in either Maven or an S3
-- bucket.
newCustomArtifactConfiguration ::
  -- | 'artifactType'
  ArtifactType ->
  CustomArtifactConfiguration
newCustomArtifactConfiguration pArtifactType_ =
  CustomArtifactConfiguration'
    { mavenReference =
        Prelude.Nothing,
      s3ContentLocation = Prelude.Nothing,
      artifactType = pArtifactType_
    }

-- | The parameters required to fully specify a Maven reference.
customArtifactConfiguration_mavenReference :: Lens.Lens' CustomArtifactConfiguration (Prelude.Maybe MavenReference)
customArtifactConfiguration_mavenReference = Lens.lens (\CustomArtifactConfiguration' {mavenReference} -> mavenReference) (\s@CustomArtifactConfiguration' {} a -> s {mavenReference = a} :: CustomArtifactConfiguration)

-- | Undocumented member.
customArtifactConfiguration_s3ContentLocation :: Lens.Lens' CustomArtifactConfiguration (Prelude.Maybe S3ContentLocation)
customArtifactConfiguration_s3ContentLocation = Lens.lens (\CustomArtifactConfiguration' {s3ContentLocation} -> s3ContentLocation) (\s@CustomArtifactConfiguration' {} a -> s {s3ContentLocation = a} :: CustomArtifactConfiguration)

-- | @UDF@ stands for user-defined functions. This type of artifact must be
-- in an S3 bucket. A @DEPENDENCY_JAR@ can be in either Maven or an S3
-- bucket.
customArtifactConfiguration_artifactType :: Lens.Lens' CustomArtifactConfiguration ArtifactType
customArtifactConfiguration_artifactType = Lens.lens (\CustomArtifactConfiguration' {artifactType} -> artifactType) (\s@CustomArtifactConfiguration' {} a -> s {artifactType = a} :: CustomArtifactConfiguration)

instance Prelude.Hashable CustomArtifactConfiguration where
  hashWithSalt _salt CustomArtifactConfiguration' {..} =
    _salt `Prelude.hashWithSalt` mavenReference
      `Prelude.hashWithSalt` s3ContentLocation
      `Prelude.hashWithSalt` artifactType

instance Prelude.NFData CustomArtifactConfiguration where
  rnf CustomArtifactConfiguration' {..} =
    Prelude.rnf mavenReference
      `Prelude.seq` Prelude.rnf s3ContentLocation
      `Prelude.seq` Prelude.rnf artifactType

instance Data.ToJSON CustomArtifactConfiguration where
  toJSON CustomArtifactConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MavenReference" Data..=)
              Prelude.<$> mavenReference,
            ("S3ContentLocation" Data..=)
              Prelude.<$> s3ContentLocation,
            Prelude.Just ("ArtifactType" Data..= artifactType)
          ]
      )
