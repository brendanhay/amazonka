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
-- Module      : Network.AWS.CodeBuild.Types.ProjectArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectArtifacts where

import Network.AWS.CodeBuild.Types.ArtifactNamespace
import Network.AWS.CodeBuild.Types.ArtifactPackaging
import Network.AWS.CodeBuild.Types.ArtifactsType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the build output artifacts for the build project.
--
-- /See:/ 'newProjectArtifacts' smart constructor.
data ProjectArtifacts = ProjectArtifacts'
  { -- | Along with @path@ and @name@, the pattern that AWS CodeBuild uses to
    -- determine the name and location to store the output artifact:
    --
    -- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
    --     value if specified. This is because AWS CodePipeline manages its
    --     build output names instead of AWS CodeBuild.
    --
    -- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
    --     specified, because no build output is produced.
    --
    -- -   If @type@ is set to @S3@, valid values include:
    --
    --     -   @BUILD_ID@: Include the build ID in the location of the build
    --         output artifact.
    --
    --     -   @NONE@: Do not include the build ID. This is the default if
    --         @namespaceType@ is not specified.
    --
    -- For example, if @path@ is set to @MyArtifacts@, @namespaceType@ is set
    -- to @BUILD_ID@, and @name@ is set to @MyArtifact.zip@, the output
    -- artifact is stored in @MyArtifacts\/\<build-ID>\/MyArtifact.zip@.
    namespaceType :: Prelude.Maybe ArtifactNamespace,
    -- | If this flag is set, a name specified in the buildspec file overrides
    -- the artifact name. The name specified in a buildspec file is calculated
    -- at build time and uses the Shell Command Language. For example, you can
    -- append a date and time to your artifact name so that it is always
    -- unique.
    overrideArtifactName :: Prelude.Maybe Prelude.Bool,
    -- | An identifier for this artifact definition.
    artifactIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Along with @path@ and @namespaceType@, the pattern that AWS CodeBuild
    -- uses to name and store the output artifact:
    --
    -- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
    --     value if specified. This is because AWS CodePipeline manages its
    --     build output names instead of AWS CodeBuild.
    --
    -- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
    --     specified, because no build output is produced.
    --
    -- -   If @type@ is set to @S3@, this is the name of the output artifact
    --     object. If you set the name to be a forward slash (\"\/\"), the
    --     artifact is stored in the root of the output bucket.
    --
    -- For example:
    --
    -- -   If @path@ is set to @MyArtifacts@, @namespaceType@ is set to
    --     @BUILD_ID@, and @name@ is set to @MyArtifact.zip@, then the output
    --     artifact is stored in @MyArtifacts\/\<build-ID>\/MyArtifact.zip@.
    --
    -- -   If @path@ is empty, @namespaceType@ is set to @NONE@, and @name@ is
    --     set to \"@\/@\", the output artifact is stored in the root of the
    --     output bucket.
    --
    -- -   If @path@ is set to @MyArtifacts@, @namespaceType@ is set to
    --     @BUILD_ID@, and @name@ is set to \"@\/@\", the output artifact is
    --     stored in @MyArtifacts\/\<build-ID>@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of build output artifact to create:
    --
    -- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
    --     value if specified. This is because AWS CodePipeline manages its
    --     build output artifacts instead of AWS CodeBuild.
    --
    -- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
    --     specified, because no build output is produced.
    --
    -- -   If @type@ is set to @S3@, valid values include:
    --
    --     -   @NONE@: AWS CodeBuild creates in the output bucket a folder that
    --         contains the build output. This is the default if @packaging@ is
    --         not specified.
    --
    --     -   @ZIP@: AWS CodeBuild creates in the output bucket a ZIP file
    --         that contains the build output.
    packaging :: Prelude.Maybe ArtifactPackaging,
    -- | Set to true if you do not want your output artifacts encrypted. This
    -- option is valid only if your artifacts type is Amazon S3. If this is set
    -- with another artifacts type, an invalidInputException is thrown.
    encryptionDisabled :: Prelude.Maybe Prelude.Bool,
    -- | Information about the build output artifact location:
    --
    -- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
    --     value if specified. This is because AWS CodePipeline manages its
    --     build output locations instead of AWS CodeBuild.
    --
    -- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
    --     specified, because no build output is produced.
    --
    -- -   If @type@ is set to @S3@, this is the name of the output bucket.
    location :: Prelude.Maybe Prelude.Text,
    -- | Along with @namespaceType@ and @name@, the pattern that AWS CodeBuild
    -- uses to name and store the output artifact:
    --
    -- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
    --     value if specified. This is because AWS CodePipeline manages its
    --     build output names instead of AWS CodeBuild.
    --
    -- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
    --     specified, because no build output is produced.
    --
    -- -   If @type@ is set to @S3@, this is the path to the output artifact.
    --     If @path@ is not specified, @path@ is not used.
    --
    -- For example, if @path@ is set to @MyArtifacts@, @namespaceType@ is set
    -- to @NONE@, and @name@ is set to @MyArtifact.zip@, the output artifact is
    -- stored in the output bucket at @MyArtifacts\/MyArtifact.zip@.
    path :: Prelude.Maybe Prelude.Text,
    -- | The type of build output artifact. Valid values include:
    --
    -- -   @CODEPIPELINE@: The build project has build output generated through
    --     AWS CodePipeline.
    --
    --     The @CODEPIPELINE@ type is not supported for @secondaryArtifacts@.
    --
    -- -   @NO_ARTIFACTS@: The build project does not produce any build output.
    --
    -- -   @S3@: The build project stores build output in Amazon S3.
    type' :: ArtifactsType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProjectArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceType', 'projectArtifacts_namespaceType' - Along with @path@ and @name@, the pattern that AWS CodeBuild uses to
-- determine the name and location to store the output artifact:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output names instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, valid values include:
--
--     -   @BUILD_ID@: Include the build ID in the location of the build
--         output artifact.
--
--     -   @NONE@: Do not include the build ID. This is the default if
--         @namespaceType@ is not specified.
--
-- For example, if @path@ is set to @MyArtifacts@, @namespaceType@ is set
-- to @BUILD_ID@, and @name@ is set to @MyArtifact.zip@, the output
-- artifact is stored in @MyArtifacts\/\<build-ID>\/MyArtifact.zip@.
--
-- 'overrideArtifactName', 'projectArtifacts_overrideArtifactName' - If this flag is set, a name specified in the buildspec file overrides
-- the artifact name. The name specified in a buildspec file is calculated
-- at build time and uses the Shell Command Language. For example, you can
-- append a date and time to your artifact name so that it is always
-- unique.
--
-- 'artifactIdentifier', 'projectArtifacts_artifactIdentifier' - An identifier for this artifact definition.
--
-- 'name', 'projectArtifacts_name' - Along with @path@ and @namespaceType@, the pattern that AWS CodeBuild
-- uses to name and store the output artifact:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output names instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, this is the name of the output artifact
--     object. If you set the name to be a forward slash (\"\/\"), the
--     artifact is stored in the root of the output bucket.
--
-- For example:
--
-- -   If @path@ is set to @MyArtifacts@, @namespaceType@ is set to
--     @BUILD_ID@, and @name@ is set to @MyArtifact.zip@, then the output
--     artifact is stored in @MyArtifacts\/\<build-ID>\/MyArtifact.zip@.
--
-- -   If @path@ is empty, @namespaceType@ is set to @NONE@, and @name@ is
--     set to \"@\/@\", the output artifact is stored in the root of the
--     output bucket.
--
-- -   If @path@ is set to @MyArtifacts@, @namespaceType@ is set to
--     @BUILD_ID@, and @name@ is set to \"@\/@\", the output artifact is
--     stored in @MyArtifacts\/\<build-ID>@.
--
-- 'packaging', 'projectArtifacts_packaging' - The type of build output artifact to create:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output artifacts instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, valid values include:
--
--     -   @NONE@: AWS CodeBuild creates in the output bucket a folder that
--         contains the build output. This is the default if @packaging@ is
--         not specified.
--
--     -   @ZIP@: AWS CodeBuild creates in the output bucket a ZIP file
--         that contains the build output.
--
-- 'encryptionDisabled', 'projectArtifacts_encryptionDisabled' - Set to true if you do not want your output artifacts encrypted. This
-- option is valid only if your artifacts type is Amazon S3. If this is set
-- with another artifacts type, an invalidInputException is thrown.
--
-- 'location', 'projectArtifacts_location' - Information about the build output artifact location:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output locations instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, this is the name of the output bucket.
--
-- 'path', 'projectArtifacts_path' - Along with @namespaceType@ and @name@, the pattern that AWS CodeBuild
-- uses to name and store the output artifact:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output names instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, this is the path to the output artifact.
--     If @path@ is not specified, @path@ is not used.
--
-- For example, if @path@ is set to @MyArtifacts@, @namespaceType@ is set
-- to @NONE@, and @name@ is set to @MyArtifact.zip@, the output artifact is
-- stored in the output bucket at @MyArtifacts\/MyArtifact.zip@.
--
-- 'type'', 'projectArtifacts_type' - The type of build output artifact. Valid values include:
--
-- -   @CODEPIPELINE@: The build project has build output generated through
--     AWS CodePipeline.
--
--     The @CODEPIPELINE@ type is not supported for @secondaryArtifacts@.
--
-- -   @NO_ARTIFACTS@: The build project does not produce any build output.
--
-- -   @S3@: The build project stores build output in Amazon S3.
newProjectArtifacts ::
  -- | 'type''
  ArtifactsType ->
  ProjectArtifacts
newProjectArtifacts pType_ =
  ProjectArtifacts'
    { namespaceType = Prelude.Nothing,
      overrideArtifactName = Prelude.Nothing,
      artifactIdentifier = Prelude.Nothing,
      name = Prelude.Nothing,
      packaging = Prelude.Nothing,
      encryptionDisabled = Prelude.Nothing,
      location = Prelude.Nothing,
      path = Prelude.Nothing,
      type' = pType_
    }

-- | Along with @path@ and @name@, the pattern that AWS CodeBuild uses to
-- determine the name and location to store the output artifact:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output names instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, valid values include:
--
--     -   @BUILD_ID@: Include the build ID in the location of the build
--         output artifact.
--
--     -   @NONE@: Do not include the build ID. This is the default if
--         @namespaceType@ is not specified.
--
-- For example, if @path@ is set to @MyArtifacts@, @namespaceType@ is set
-- to @BUILD_ID@, and @name@ is set to @MyArtifact.zip@, the output
-- artifact is stored in @MyArtifacts\/\<build-ID>\/MyArtifact.zip@.
projectArtifacts_namespaceType :: Lens.Lens' ProjectArtifacts (Prelude.Maybe ArtifactNamespace)
projectArtifacts_namespaceType = Lens.lens (\ProjectArtifacts' {namespaceType} -> namespaceType) (\s@ProjectArtifacts' {} a -> s {namespaceType = a} :: ProjectArtifacts)

-- | If this flag is set, a name specified in the buildspec file overrides
-- the artifact name. The name specified in a buildspec file is calculated
-- at build time and uses the Shell Command Language. For example, you can
-- append a date and time to your artifact name so that it is always
-- unique.
projectArtifacts_overrideArtifactName :: Lens.Lens' ProjectArtifacts (Prelude.Maybe Prelude.Bool)
projectArtifacts_overrideArtifactName = Lens.lens (\ProjectArtifacts' {overrideArtifactName} -> overrideArtifactName) (\s@ProjectArtifacts' {} a -> s {overrideArtifactName = a} :: ProjectArtifacts)

-- | An identifier for this artifact definition.
projectArtifacts_artifactIdentifier :: Lens.Lens' ProjectArtifacts (Prelude.Maybe Prelude.Text)
projectArtifacts_artifactIdentifier = Lens.lens (\ProjectArtifacts' {artifactIdentifier} -> artifactIdentifier) (\s@ProjectArtifacts' {} a -> s {artifactIdentifier = a} :: ProjectArtifacts)

-- | Along with @path@ and @namespaceType@, the pattern that AWS CodeBuild
-- uses to name and store the output artifact:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output names instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, this is the name of the output artifact
--     object. If you set the name to be a forward slash (\"\/\"), the
--     artifact is stored in the root of the output bucket.
--
-- For example:
--
-- -   If @path@ is set to @MyArtifacts@, @namespaceType@ is set to
--     @BUILD_ID@, and @name@ is set to @MyArtifact.zip@, then the output
--     artifact is stored in @MyArtifacts\/\<build-ID>\/MyArtifact.zip@.
--
-- -   If @path@ is empty, @namespaceType@ is set to @NONE@, and @name@ is
--     set to \"@\/@\", the output artifact is stored in the root of the
--     output bucket.
--
-- -   If @path@ is set to @MyArtifacts@, @namespaceType@ is set to
--     @BUILD_ID@, and @name@ is set to \"@\/@\", the output artifact is
--     stored in @MyArtifacts\/\<build-ID>@.
projectArtifacts_name :: Lens.Lens' ProjectArtifacts (Prelude.Maybe Prelude.Text)
projectArtifacts_name = Lens.lens (\ProjectArtifacts' {name} -> name) (\s@ProjectArtifacts' {} a -> s {name = a} :: ProjectArtifacts)

-- | The type of build output artifact to create:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output artifacts instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, valid values include:
--
--     -   @NONE@: AWS CodeBuild creates in the output bucket a folder that
--         contains the build output. This is the default if @packaging@ is
--         not specified.
--
--     -   @ZIP@: AWS CodeBuild creates in the output bucket a ZIP file
--         that contains the build output.
projectArtifacts_packaging :: Lens.Lens' ProjectArtifacts (Prelude.Maybe ArtifactPackaging)
projectArtifacts_packaging = Lens.lens (\ProjectArtifacts' {packaging} -> packaging) (\s@ProjectArtifacts' {} a -> s {packaging = a} :: ProjectArtifacts)

-- | Set to true if you do not want your output artifacts encrypted. This
-- option is valid only if your artifacts type is Amazon S3. If this is set
-- with another artifacts type, an invalidInputException is thrown.
projectArtifacts_encryptionDisabled :: Lens.Lens' ProjectArtifacts (Prelude.Maybe Prelude.Bool)
projectArtifacts_encryptionDisabled = Lens.lens (\ProjectArtifacts' {encryptionDisabled} -> encryptionDisabled) (\s@ProjectArtifacts' {} a -> s {encryptionDisabled = a} :: ProjectArtifacts)

-- | Information about the build output artifact location:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output locations instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, this is the name of the output bucket.
projectArtifacts_location :: Lens.Lens' ProjectArtifacts (Prelude.Maybe Prelude.Text)
projectArtifacts_location = Lens.lens (\ProjectArtifacts' {location} -> location) (\s@ProjectArtifacts' {} a -> s {location = a} :: ProjectArtifacts)

-- | Along with @namespaceType@ and @name@, the pattern that AWS CodeBuild
-- uses to name and store the output artifact:
--
-- -   If @type@ is set to @CODEPIPELINE@, AWS CodePipeline ignores this
--     value if specified. This is because AWS CodePipeline manages its
--     build output names instead of AWS CodeBuild.
--
-- -   If @type@ is set to @NO_ARTIFACTS@, this value is ignored if
--     specified, because no build output is produced.
--
-- -   If @type@ is set to @S3@, this is the path to the output artifact.
--     If @path@ is not specified, @path@ is not used.
--
-- For example, if @path@ is set to @MyArtifacts@, @namespaceType@ is set
-- to @NONE@, and @name@ is set to @MyArtifact.zip@, the output artifact is
-- stored in the output bucket at @MyArtifacts\/MyArtifact.zip@.
projectArtifacts_path :: Lens.Lens' ProjectArtifacts (Prelude.Maybe Prelude.Text)
projectArtifacts_path = Lens.lens (\ProjectArtifacts' {path} -> path) (\s@ProjectArtifacts' {} a -> s {path = a} :: ProjectArtifacts)

-- | The type of build output artifact. Valid values include:
--
-- -   @CODEPIPELINE@: The build project has build output generated through
--     AWS CodePipeline.
--
--     The @CODEPIPELINE@ type is not supported for @secondaryArtifacts@.
--
-- -   @NO_ARTIFACTS@: The build project does not produce any build output.
--
-- -   @S3@: The build project stores build output in Amazon S3.
projectArtifacts_type :: Lens.Lens' ProjectArtifacts ArtifactsType
projectArtifacts_type = Lens.lens (\ProjectArtifacts' {type'} -> type') (\s@ProjectArtifacts' {} a -> s {type' = a} :: ProjectArtifacts)

instance Prelude.FromJSON ProjectArtifacts where
  parseJSON =
    Prelude.withObject
      "ProjectArtifacts"
      ( \x ->
          ProjectArtifacts'
            Prelude.<$> (x Prelude..:? "namespaceType")
            Prelude.<*> (x Prelude..:? "overrideArtifactName")
            Prelude.<*> (x Prelude..:? "artifactIdentifier")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "packaging")
            Prelude.<*> (x Prelude..:? "encryptionDisabled")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "path")
            Prelude.<*> (x Prelude..: "type")
      )

instance Prelude.Hashable ProjectArtifacts

instance Prelude.NFData ProjectArtifacts

instance Prelude.ToJSON ProjectArtifacts where
  toJSON ProjectArtifacts' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("namespaceType" Prelude..=)
              Prelude.<$> namespaceType,
            ("overrideArtifactName" Prelude..=)
              Prelude.<$> overrideArtifactName,
            ("artifactIdentifier" Prelude..=)
              Prelude.<$> artifactIdentifier,
            ("name" Prelude..=) Prelude.<$> name,
            ("packaging" Prelude..=) Prelude.<$> packaging,
            ("encryptionDisabled" Prelude..=)
              Prelude.<$> encryptionDisabled,
            ("location" Prelude..=) Prelude.<$> location,
            ("path" Prelude..=) Prelude.<$> path,
            Prelude.Just ("type" Prelude..= type')
          ]
      )
