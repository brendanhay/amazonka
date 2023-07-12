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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CustomArtifactConfigurationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CustomArtifactConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.ArtifactType
import Amazonka.KinesisAnalyticsV2.Types.MavenReference
import Amazonka.KinesisAnalyticsV2.Types.S3ContentLocation
import qualified Amazonka.Prelude as Prelude

-- | Specifies a dependency JAR or a JAR of user-defined functions.
--
-- /See:/ 'newCustomArtifactConfigurationDescription' smart constructor.
data CustomArtifactConfigurationDescription = CustomArtifactConfigurationDescription'
  { -- | @UDF@ stands for user-defined functions. This type of artifact must be
    -- in an S3 bucket. A @DEPENDENCY_JAR@ can be in either Maven or an S3
    -- bucket.
    artifactType :: Prelude.Maybe ArtifactType,
    -- | The parameters that are required to specify a Maven dependency.
    mavenReferenceDescription :: Prelude.Maybe MavenReference,
    s3ContentLocationDescription :: Prelude.Maybe S3ContentLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomArtifactConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactType', 'customArtifactConfigurationDescription_artifactType' - @UDF@ stands for user-defined functions. This type of artifact must be
-- in an S3 bucket. A @DEPENDENCY_JAR@ can be in either Maven or an S3
-- bucket.
--
-- 'mavenReferenceDescription', 'customArtifactConfigurationDescription_mavenReferenceDescription' - The parameters that are required to specify a Maven dependency.
--
-- 's3ContentLocationDescription', 'customArtifactConfigurationDescription_s3ContentLocationDescription' - Undocumented member.
newCustomArtifactConfigurationDescription ::
  CustomArtifactConfigurationDescription
newCustomArtifactConfigurationDescription =
  CustomArtifactConfigurationDescription'
    { artifactType =
        Prelude.Nothing,
      mavenReferenceDescription =
        Prelude.Nothing,
      s3ContentLocationDescription =
        Prelude.Nothing
    }

-- | @UDF@ stands for user-defined functions. This type of artifact must be
-- in an S3 bucket. A @DEPENDENCY_JAR@ can be in either Maven or an S3
-- bucket.
customArtifactConfigurationDescription_artifactType :: Lens.Lens' CustomArtifactConfigurationDescription (Prelude.Maybe ArtifactType)
customArtifactConfigurationDescription_artifactType = Lens.lens (\CustomArtifactConfigurationDescription' {artifactType} -> artifactType) (\s@CustomArtifactConfigurationDescription' {} a -> s {artifactType = a} :: CustomArtifactConfigurationDescription)

-- | The parameters that are required to specify a Maven dependency.
customArtifactConfigurationDescription_mavenReferenceDescription :: Lens.Lens' CustomArtifactConfigurationDescription (Prelude.Maybe MavenReference)
customArtifactConfigurationDescription_mavenReferenceDescription = Lens.lens (\CustomArtifactConfigurationDescription' {mavenReferenceDescription} -> mavenReferenceDescription) (\s@CustomArtifactConfigurationDescription' {} a -> s {mavenReferenceDescription = a} :: CustomArtifactConfigurationDescription)

-- | Undocumented member.
customArtifactConfigurationDescription_s3ContentLocationDescription :: Lens.Lens' CustomArtifactConfigurationDescription (Prelude.Maybe S3ContentLocation)
customArtifactConfigurationDescription_s3ContentLocationDescription = Lens.lens (\CustomArtifactConfigurationDescription' {s3ContentLocationDescription} -> s3ContentLocationDescription) (\s@CustomArtifactConfigurationDescription' {} a -> s {s3ContentLocationDescription = a} :: CustomArtifactConfigurationDescription)

instance
  Data.FromJSON
    CustomArtifactConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "CustomArtifactConfigurationDescription"
      ( \x ->
          CustomArtifactConfigurationDescription'
            Prelude.<$> (x Data..:? "ArtifactType")
            Prelude.<*> (x Data..:? "MavenReferenceDescription")
            Prelude.<*> (x Data..:? "S3ContentLocationDescription")
      )

instance
  Prelude.Hashable
    CustomArtifactConfigurationDescription
  where
  hashWithSalt
    _salt
    CustomArtifactConfigurationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` artifactType
        `Prelude.hashWithSalt` mavenReferenceDescription
        `Prelude.hashWithSalt` s3ContentLocationDescription

instance
  Prelude.NFData
    CustomArtifactConfigurationDescription
  where
  rnf CustomArtifactConfigurationDescription' {..} =
    Prelude.rnf artifactType
      `Prelude.seq` Prelude.rnf mavenReferenceDescription
      `Prelude.seq` Prelude.rnf s3ContentLocationDescription
