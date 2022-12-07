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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectArtifactsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectArtifactsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the build artifacts for the CodeBuild project.
--
-- /See:/ 'newAwsCodeBuildProjectArtifactsDetails' smart constructor.
data AwsCodeBuildProjectArtifactsDetails = AwsCodeBuildProjectArtifactsDetails'
  { -- | Indicates whether to disable encryption on the artifact. Only valid when
    -- @Type@ is @S3@.
    encryptionDisabled :: Prelude.Maybe Prelude.Bool,
    -- | Only used when Type is S3. The name of the artifact. Used with
    -- @NamepaceType@ and @Path@ to determine the pattern for storing the
    -- artifact.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of build artifact.
    type' :: Prelude.Maybe Prelude.Text,
    -- | Only used when @Type@ is @S3@. The path to the artifact. Used with
    -- @Name@ and @NamespaceType@ to determine the pattern for storing the
    -- artifact.
    path :: Prelude.Maybe Prelude.Text,
    -- | An identifier for the artifact definition.
    artifactIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Only used when @Type@ is @S3@. The type of output artifact to create.
    packaging :: Prelude.Maybe Prelude.Text,
    -- | Only used when @Type@ is @S3@. The name of the S3 bucket where the
    -- artifact is located.
    location :: Prelude.Maybe Prelude.Text,
    -- | Whether the name specified in the buildspec file overrides the artifact
    -- name.
    overrideArtifactName :: Prelude.Maybe Prelude.Bool,
    -- | Only used when @Type@ is @S3@. The value to use for the namespace. Used
    -- with @Name@ and @Path@ to determine the pattern for storing the
    -- artifact.
    namespaceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectArtifactsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionDisabled', 'awsCodeBuildProjectArtifactsDetails_encryptionDisabled' - Indicates whether to disable encryption on the artifact. Only valid when
-- @Type@ is @S3@.
--
-- 'name', 'awsCodeBuildProjectArtifactsDetails_name' - Only used when Type is S3. The name of the artifact. Used with
-- @NamepaceType@ and @Path@ to determine the pattern for storing the
-- artifact.
--
-- 'type'', 'awsCodeBuildProjectArtifactsDetails_type' - The type of build artifact.
--
-- 'path', 'awsCodeBuildProjectArtifactsDetails_path' - Only used when @Type@ is @S3@. The path to the artifact. Used with
-- @Name@ and @NamespaceType@ to determine the pattern for storing the
-- artifact.
--
-- 'artifactIdentifier', 'awsCodeBuildProjectArtifactsDetails_artifactIdentifier' - An identifier for the artifact definition.
--
-- 'packaging', 'awsCodeBuildProjectArtifactsDetails_packaging' - Only used when @Type@ is @S3@. The type of output artifact to create.
--
-- 'location', 'awsCodeBuildProjectArtifactsDetails_location' - Only used when @Type@ is @S3@. The name of the S3 bucket where the
-- artifact is located.
--
-- 'overrideArtifactName', 'awsCodeBuildProjectArtifactsDetails_overrideArtifactName' - Whether the name specified in the buildspec file overrides the artifact
-- name.
--
-- 'namespaceType', 'awsCodeBuildProjectArtifactsDetails_namespaceType' - Only used when @Type@ is @S3@. The value to use for the namespace. Used
-- with @Name@ and @Path@ to determine the pattern for storing the
-- artifact.
newAwsCodeBuildProjectArtifactsDetails ::
  AwsCodeBuildProjectArtifactsDetails
newAwsCodeBuildProjectArtifactsDetails =
  AwsCodeBuildProjectArtifactsDetails'
    { encryptionDisabled =
        Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      path = Prelude.Nothing,
      artifactIdentifier = Prelude.Nothing,
      packaging = Prelude.Nothing,
      location = Prelude.Nothing,
      overrideArtifactName = Prelude.Nothing,
      namespaceType = Prelude.Nothing
    }

-- | Indicates whether to disable encryption on the artifact. Only valid when
-- @Type@ is @S3@.
awsCodeBuildProjectArtifactsDetails_encryptionDisabled :: Lens.Lens' AwsCodeBuildProjectArtifactsDetails (Prelude.Maybe Prelude.Bool)
awsCodeBuildProjectArtifactsDetails_encryptionDisabled = Lens.lens (\AwsCodeBuildProjectArtifactsDetails' {encryptionDisabled} -> encryptionDisabled) (\s@AwsCodeBuildProjectArtifactsDetails' {} a -> s {encryptionDisabled = a} :: AwsCodeBuildProjectArtifactsDetails)

-- | Only used when Type is S3. The name of the artifact. Used with
-- @NamepaceType@ and @Path@ to determine the pattern for storing the
-- artifact.
awsCodeBuildProjectArtifactsDetails_name :: Lens.Lens' AwsCodeBuildProjectArtifactsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectArtifactsDetails_name = Lens.lens (\AwsCodeBuildProjectArtifactsDetails' {name} -> name) (\s@AwsCodeBuildProjectArtifactsDetails' {} a -> s {name = a} :: AwsCodeBuildProjectArtifactsDetails)

-- | The type of build artifact.
awsCodeBuildProjectArtifactsDetails_type :: Lens.Lens' AwsCodeBuildProjectArtifactsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectArtifactsDetails_type = Lens.lens (\AwsCodeBuildProjectArtifactsDetails' {type'} -> type') (\s@AwsCodeBuildProjectArtifactsDetails' {} a -> s {type' = a} :: AwsCodeBuildProjectArtifactsDetails)

-- | Only used when @Type@ is @S3@. The path to the artifact. Used with
-- @Name@ and @NamespaceType@ to determine the pattern for storing the
-- artifact.
awsCodeBuildProjectArtifactsDetails_path :: Lens.Lens' AwsCodeBuildProjectArtifactsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectArtifactsDetails_path = Lens.lens (\AwsCodeBuildProjectArtifactsDetails' {path} -> path) (\s@AwsCodeBuildProjectArtifactsDetails' {} a -> s {path = a} :: AwsCodeBuildProjectArtifactsDetails)

-- | An identifier for the artifact definition.
awsCodeBuildProjectArtifactsDetails_artifactIdentifier :: Lens.Lens' AwsCodeBuildProjectArtifactsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectArtifactsDetails_artifactIdentifier = Lens.lens (\AwsCodeBuildProjectArtifactsDetails' {artifactIdentifier} -> artifactIdentifier) (\s@AwsCodeBuildProjectArtifactsDetails' {} a -> s {artifactIdentifier = a} :: AwsCodeBuildProjectArtifactsDetails)

-- | Only used when @Type@ is @S3@. The type of output artifact to create.
awsCodeBuildProjectArtifactsDetails_packaging :: Lens.Lens' AwsCodeBuildProjectArtifactsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectArtifactsDetails_packaging = Lens.lens (\AwsCodeBuildProjectArtifactsDetails' {packaging} -> packaging) (\s@AwsCodeBuildProjectArtifactsDetails' {} a -> s {packaging = a} :: AwsCodeBuildProjectArtifactsDetails)

-- | Only used when @Type@ is @S3@. The name of the S3 bucket where the
-- artifact is located.
awsCodeBuildProjectArtifactsDetails_location :: Lens.Lens' AwsCodeBuildProjectArtifactsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectArtifactsDetails_location = Lens.lens (\AwsCodeBuildProjectArtifactsDetails' {location} -> location) (\s@AwsCodeBuildProjectArtifactsDetails' {} a -> s {location = a} :: AwsCodeBuildProjectArtifactsDetails)

-- | Whether the name specified in the buildspec file overrides the artifact
-- name.
awsCodeBuildProjectArtifactsDetails_overrideArtifactName :: Lens.Lens' AwsCodeBuildProjectArtifactsDetails (Prelude.Maybe Prelude.Bool)
awsCodeBuildProjectArtifactsDetails_overrideArtifactName = Lens.lens (\AwsCodeBuildProjectArtifactsDetails' {overrideArtifactName} -> overrideArtifactName) (\s@AwsCodeBuildProjectArtifactsDetails' {} a -> s {overrideArtifactName = a} :: AwsCodeBuildProjectArtifactsDetails)

-- | Only used when @Type@ is @S3@. The value to use for the namespace. Used
-- with @Name@ and @Path@ to determine the pattern for storing the
-- artifact.
awsCodeBuildProjectArtifactsDetails_namespaceType :: Lens.Lens' AwsCodeBuildProjectArtifactsDetails (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectArtifactsDetails_namespaceType = Lens.lens (\AwsCodeBuildProjectArtifactsDetails' {namespaceType} -> namespaceType) (\s@AwsCodeBuildProjectArtifactsDetails' {} a -> s {namespaceType = a} :: AwsCodeBuildProjectArtifactsDetails)

instance
  Data.FromJSON
    AwsCodeBuildProjectArtifactsDetails
  where
  parseJSON =
    Data.withObject
      "AwsCodeBuildProjectArtifactsDetails"
      ( \x ->
          AwsCodeBuildProjectArtifactsDetails'
            Prelude.<$> (x Data..:? "EncryptionDisabled")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "ArtifactIdentifier")
            Prelude.<*> (x Data..:? "Packaging")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "OverrideArtifactName")
            Prelude.<*> (x Data..:? "NamespaceType")
      )

instance
  Prelude.Hashable
    AwsCodeBuildProjectArtifactsDetails
  where
  hashWithSalt
    _salt
    AwsCodeBuildProjectArtifactsDetails' {..} =
      _salt `Prelude.hashWithSalt` encryptionDisabled
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` path
        `Prelude.hashWithSalt` artifactIdentifier
        `Prelude.hashWithSalt` packaging
        `Prelude.hashWithSalt` location
        `Prelude.hashWithSalt` overrideArtifactName
        `Prelude.hashWithSalt` namespaceType

instance
  Prelude.NFData
    AwsCodeBuildProjectArtifactsDetails
  where
  rnf AwsCodeBuildProjectArtifactsDetails' {..} =
    Prelude.rnf encryptionDisabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf artifactIdentifier
      `Prelude.seq` Prelude.rnf packaging
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf overrideArtifactName
      `Prelude.seq` Prelude.rnf namespaceType

instance
  Data.ToJSON
    AwsCodeBuildProjectArtifactsDetails
  where
  toJSON AwsCodeBuildProjectArtifactsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionDisabled" Data..=)
              Prelude.<$> encryptionDisabled,
            ("Name" Data..=) Prelude.<$> name,
            ("Type" Data..=) Prelude.<$> type',
            ("Path" Data..=) Prelude.<$> path,
            ("ArtifactIdentifier" Data..=)
              Prelude.<$> artifactIdentifier,
            ("Packaging" Data..=) Prelude.<$> packaging,
            ("Location" Data..=) Prelude.<$> location,
            ("OverrideArtifactName" Data..=)
              Prelude.<$> overrideArtifactName,
            ("NamespaceType" Data..=) Prelude.<$> namespaceType
          ]
      )
