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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    namespaceType :: Core.Maybe ArtifactNamespace,
    -- | If this flag is set, a name specified in the buildspec file overrides
    -- the artifact name. The name specified in a buildspec file is calculated
    -- at build time and uses the Shell Command Language. For example, you can
    -- append a date and time to your artifact name so that it is always
    -- unique.
    overrideArtifactName :: Core.Maybe Core.Bool,
    -- | An identifier for this artifact definition.
    artifactIdentifier :: Core.Maybe Core.Text,
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
    name :: Core.Maybe Core.Text,
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
    packaging :: Core.Maybe ArtifactPackaging,
    -- | Set to true if you do not want your output artifacts encrypted. This
    -- option is valid only if your artifacts type is Amazon S3. If this is set
    -- with another artifacts type, an invalidInputException is thrown.
    encryptionDisabled :: Core.Maybe Core.Bool,
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
    location :: Core.Maybe Core.Text,
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
    path :: Core.Maybe Core.Text,
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { namespaceType = Core.Nothing,
      overrideArtifactName = Core.Nothing,
      artifactIdentifier = Core.Nothing,
      name = Core.Nothing,
      packaging = Core.Nothing,
      encryptionDisabled = Core.Nothing,
      location = Core.Nothing,
      path = Core.Nothing,
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
projectArtifacts_namespaceType :: Lens.Lens' ProjectArtifacts (Core.Maybe ArtifactNamespace)
projectArtifacts_namespaceType = Lens.lens (\ProjectArtifacts' {namespaceType} -> namespaceType) (\s@ProjectArtifacts' {} a -> s {namespaceType = a} :: ProjectArtifacts)

-- | If this flag is set, a name specified in the buildspec file overrides
-- the artifact name. The name specified in a buildspec file is calculated
-- at build time and uses the Shell Command Language. For example, you can
-- append a date and time to your artifact name so that it is always
-- unique.
projectArtifacts_overrideArtifactName :: Lens.Lens' ProjectArtifacts (Core.Maybe Core.Bool)
projectArtifacts_overrideArtifactName = Lens.lens (\ProjectArtifacts' {overrideArtifactName} -> overrideArtifactName) (\s@ProjectArtifacts' {} a -> s {overrideArtifactName = a} :: ProjectArtifacts)

-- | An identifier for this artifact definition.
projectArtifacts_artifactIdentifier :: Lens.Lens' ProjectArtifacts (Core.Maybe Core.Text)
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
projectArtifacts_name :: Lens.Lens' ProjectArtifacts (Core.Maybe Core.Text)
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
projectArtifacts_packaging :: Lens.Lens' ProjectArtifacts (Core.Maybe ArtifactPackaging)
projectArtifacts_packaging = Lens.lens (\ProjectArtifacts' {packaging} -> packaging) (\s@ProjectArtifacts' {} a -> s {packaging = a} :: ProjectArtifacts)

-- | Set to true if you do not want your output artifacts encrypted. This
-- option is valid only if your artifacts type is Amazon S3. If this is set
-- with another artifacts type, an invalidInputException is thrown.
projectArtifacts_encryptionDisabled :: Lens.Lens' ProjectArtifacts (Core.Maybe Core.Bool)
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
projectArtifacts_location :: Lens.Lens' ProjectArtifacts (Core.Maybe Core.Text)
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
projectArtifacts_path :: Lens.Lens' ProjectArtifacts (Core.Maybe Core.Text)
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

instance Core.FromJSON ProjectArtifacts where
  parseJSON =
    Core.withObject
      "ProjectArtifacts"
      ( \x ->
          ProjectArtifacts'
            Core.<$> (x Core..:? "namespaceType")
            Core.<*> (x Core..:? "overrideArtifactName")
            Core.<*> (x Core..:? "artifactIdentifier")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "packaging")
            Core.<*> (x Core..:? "encryptionDisabled")
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "path")
            Core.<*> (x Core..: "type")
      )

instance Core.Hashable ProjectArtifacts

instance Core.NFData ProjectArtifacts

instance Core.ToJSON ProjectArtifacts where
  toJSON ProjectArtifacts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("namespaceType" Core..=) Core.<$> namespaceType,
            ("overrideArtifactName" Core..=)
              Core.<$> overrideArtifactName,
            ("artifactIdentifier" Core..=)
              Core.<$> artifactIdentifier,
            ("name" Core..=) Core.<$> name,
            ("packaging" Core..=) Core.<$> packaging,
            ("encryptionDisabled" Core..=)
              Core.<$> encryptionDisabled,
            ("location" Core..=) Core.<$> location,
            ("path" Core..=) Core.<$> path,
            Core.Just ("type" Core..= type')
          ]
      )
