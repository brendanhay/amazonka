{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectArtifacts
  ( ProjectArtifacts (..),

    -- * Smart constructor
    mkProjectArtifacts,

    -- * Lenses
    paType,
    paArtifactIdentifier,
    paEncryptionDisabled,
    paLocation,
    paName,
    paNamespaceType,
    paOverrideArtifactName,
    paPackaging,
    paPath,
  )
where

import qualified Network.AWS.CodeBuild.Types.ArtifactNamespace as Types
import qualified Network.AWS.CodeBuild.Types.ArtifactPackaging as Types
import qualified Network.AWS.CodeBuild.Types.ArtifactsType as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the build output artifacts for the build project.
--
-- /See:/ 'mkProjectArtifacts' smart constructor.
data ProjectArtifacts = ProjectArtifacts'
  { -- | The type of build output artifact. Valid values include:
    --
    --
    --     * @CODEPIPELINE@ : The build project has build output generated through AWS CodePipeline.
    --
    --
    --     * @NO_ARTIFACTS@ : The build project does not produce any build output.
    --
    --
    --     * @S3@ : The build project stores build output in Amazon Simple Storage Service (Amazon S3).
    type' :: Types.ArtifactsType,
    -- | An identifier for this artifact definition.
    artifactIdentifier :: Core.Maybe Types.String,
    -- | Set to true if you do not want your output artifacts encrypted. This option is valid only if your artifacts type is Amazon Simple Storage Service (Amazon S3). If this is set with another artifacts type, an invalidInputException is thrown.
    encryptionDisabled :: Core.Maybe Core.Bool,
    -- | Information about the build output artifact location:
    --
    --
    --     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output locations instead of AWS CodeBuild.
    --
    --
    --     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
    --
    --
    --     * If @type@ is set to @S3@ , this is the name of the output bucket.
    location :: Core.Maybe Types.String,
    -- | Along with @path@ and @namespaceType@ , the pattern that AWS CodeBuild uses to name and store the output artifact:
    --
    --
    --     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.
    --
    --
    --     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
    --
    --
    --     * If @type@ is set to @S3@ , this is the name of the output artifact object. If you set the name to be a forward slash ("/"), the artifact is stored in the root of the output bucket.
    --
    --
    -- For example:
    --
    --     * If @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact is stored in @MyArtifacts/<build-ID>/MyArtifact.zip@ .
    --
    --
    --     * If @path@ is empty, @namespaceType@ is set to @NONE@ , and @name@ is set to "@/@ ", the output artifact is stored in the root of the output bucket.
    --
    --
    --     * If @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to "@/@ ", the output artifact is stored in @MyArtifacts/<build-ID>@ .
    name :: Core.Maybe Types.String,
    -- | Along with @path@ and @name@ , the pattern that AWS CodeBuild uses to determine the name and location to store the output artifact:
    --
    --
    --     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.
    --
    --
    --     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
    --
    --
    --     * If @type@ is set to @S3@ , valid values include:
    --
    --     * @BUILD_ID@ : Include the build ID in the location of the build output artifact.
    --
    --
    --     * @NONE@ : Do not include the build ID. This is the default if @namespaceType@ is not specified.
    --
    --
    --
    --
    -- For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , the output artifact is stored in @MyArtifacts/<build-ID>/MyArtifact.zip@ .
    namespaceType :: Core.Maybe Types.ArtifactNamespace,
    -- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
    overrideArtifactName :: Core.Maybe Core.Bool,
    -- | The type of build output artifact to create:
    --
    --
    --     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output artifacts instead of AWS CodeBuild.
    --
    --
    --     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
    --
    --
    --     * If @type@ is set to @S3@ , valid values include:
    --
    --     * @NONE@ : AWS CodeBuild creates in the output bucket a folder that contains the build output. This is the default if @packaging@ is not specified.
    --
    --
    --     * @ZIP@ : AWS CodeBuild creates in the output bucket a ZIP file that contains the build output.
    packaging :: Core.Maybe Types.ArtifactPackaging,
    -- | Along with @namespaceType@ and @name@ , the pattern that AWS CodeBuild uses to name and store the output artifact:
    --
    --
    --     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.
    --
    --
    --     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
    --
    --
    --     * If @type@ is set to @S3@ , this is the path to the output artifact. If @path@ is not specified, @path@ is not used.
    --
    --
    -- For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @NONE@ , and @name@ is set to @MyArtifact.zip@ , the output artifact is stored in the output bucket at @MyArtifacts/MyArtifact.zip@ .
    path :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectArtifacts' value with any optional fields omitted.
mkProjectArtifacts ::
  -- | 'type\''
  Types.ArtifactsType ->
  ProjectArtifacts
mkProjectArtifacts type' =
  ProjectArtifacts'
    { type',
      artifactIdentifier = Core.Nothing,
      encryptionDisabled = Core.Nothing,
      location = Core.Nothing,
      name = Core.Nothing,
      namespaceType = Core.Nothing,
      overrideArtifactName = Core.Nothing,
      packaging = Core.Nothing,
      path = Core.Nothing
    }

-- | The type of build output artifact. Valid values include:
--
--
--     * @CODEPIPELINE@ : The build project has build output generated through AWS CodePipeline.
--
--
--     * @NO_ARTIFACTS@ : The build project does not produce any build output.
--
--
--     * @S3@ : The build project stores build output in Amazon Simple Storage Service (Amazon S3).
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paType :: Lens.Lens' ProjectArtifacts Types.ArtifactsType
paType = Lens.field @"type'"
{-# DEPRECATED paType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | An identifier for this artifact definition.
--
-- /Note:/ Consider using 'artifactIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paArtifactIdentifier :: Lens.Lens' ProjectArtifacts (Core.Maybe Types.String)
paArtifactIdentifier = Lens.field @"artifactIdentifier"
{-# DEPRECATED paArtifactIdentifier "Use generic-lens or generic-optics with 'artifactIdentifier' instead." #-}

-- | Set to true if you do not want your output artifacts encrypted. This option is valid only if your artifacts type is Amazon Simple Storage Service (Amazon S3). If this is set with another artifacts type, an invalidInputException is thrown.
--
-- /Note:/ Consider using 'encryptionDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paEncryptionDisabled :: Lens.Lens' ProjectArtifacts (Core.Maybe Core.Bool)
paEncryptionDisabled = Lens.field @"encryptionDisabled"
{-# DEPRECATED paEncryptionDisabled "Use generic-lens or generic-optics with 'encryptionDisabled' instead." #-}

-- | Information about the build output artifact location:
--
--
--     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output locations instead of AWS CodeBuild.
--
--
--     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
--
--
--     * If @type@ is set to @S3@ , this is the name of the output bucket.
--
--
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paLocation :: Lens.Lens' ProjectArtifacts (Core.Maybe Types.String)
paLocation = Lens.field @"location"
{-# DEPRECATED paLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Along with @path@ and @namespaceType@ , the pattern that AWS CodeBuild uses to name and store the output artifact:
--
--
--     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.
--
--
--     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
--
--
--     * If @type@ is set to @S3@ , this is the name of the output artifact object. If you set the name to be a forward slash ("/"), the artifact is stored in the root of the output bucket.
--
--
-- For example:
--
--     * If @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact is stored in @MyArtifacts/<build-ID>/MyArtifact.zip@ .
--
--
--     * If @path@ is empty, @namespaceType@ is set to @NONE@ , and @name@ is set to "@/@ ", the output artifact is stored in the root of the output bucket.
--
--
--     * If @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to "@/@ ", the output artifact is stored in @MyArtifacts/<build-ID>@ .
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paName :: Lens.Lens' ProjectArtifacts (Core.Maybe Types.String)
paName = Lens.field @"name"
{-# DEPRECATED paName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Along with @path@ and @name@ , the pattern that AWS CodeBuild uses to determine the name and location to store the output artifact:
--
--
--     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.
--
--
--     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
--
--
--     * If @type@ is set to @S3@ , valid values include:
--
--     * @BUILD_ID@ : Include the build ID in the location of the build output artifact.
--
--
--     * @NONE@ : Do not include the build ID. This is the default if @namespaceType@ is not specified.
--
--
--
--
-- For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , the output artifact is stored in @MyArtifacts/<build-ID>/MyArtifact.zip@ .
--
-- /Note:/ Consider using 'namespaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paNamespaceType :: Lens.Lens' ProjectArtifacts (Core.Maybe Types.ArtifactNamespace)
paNamespaceType = Lens.field @"namespaceType"
{-# DEPRECATED paNamespaceType "Use generic-lens or generic-optics with 'namespaceType' instead." #-}

-- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
--
-- /Note:/ Consider using 'overrideArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paOverrideArtifactName :: Lens.Lens' ProjectArtifacts (Core.Maybe Core.Bool)
paOverrideArtifactName = Lens.field @"overrideArtifactName"
{-# DEPRECATED paOverrideArtifactName "Use generic-lens or generic-optics with 'overrideArtifactName' instead." #-}

-- | The type of build output artifact to create:
--
--
--     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output artifacts instead of AWS CodeBuild.
--
--
--     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
--
--
--     * If @type@ is set to @S3@ , valid values include:
--
--     * @NONE@ : AWS CodeBuild creates in the output bucket a folder that contains the build output. This is the default if @packaging@ is not specified.
--
--
--     * @ZIP@ : AWS CodeBuild creates in the output bucket a ZIP file that contains the build output.
--
--
--
--
--
-- /Note:/ Consider using 'packaging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPackaging :: Lens.Lens' ProjectArtifacts (Core.Maybe Types.ArtifactPackaging)
paPackaging = Lens.field @"packaging"
{-# DEPRECATED paPackaging "Use generic-lens or generic-optics with 'packaging' instead." #-}

-- | Along with @namespaceType@ and @name@ , the pattern that AWS CodeBuild uses to name and store the output artifact:
--
--
--     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.
--
--
--     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.
--
--
--     * If @type@ is set to @S3@ , this is the path to the output artifact. If @path@ is not specified, @path@ is not used.
--
--
-- For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @NONE@ , and @name@ is set to @MyArtifact.zip@ , the output artifact is stored in the output bucket at @MyArtifacts/MyArtifact.zip@ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPath :: Lens.Lens' ProjectArtifacts (Core.Maybe Types.String)
paPath = Lens.field @"path"
{-# DEPRECATED paPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Core.FromJSON ProjectArtifacts where
  toJSON ProjectArtifacts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("type" Core..= type'),
            ("artifactIdentifier" Core..=) Core.<$> artifactIdentifier,
            ("encryptionDisabled" Core..=) Core.<$> encryptionDisabled,
            ("location" Core..=) Core.<$> location,
            ("name" Core..=) Core.<$> name,
            ("namespaceType" Core..=) Core.<$> namespaceType,
            ("overrideArtifactName" Core..=) Core.<$> overrideArtifactName,
            ("packaging" Core..=) Core.<$> packaging,
            ("path" Core..=) Core.<$> path
          ]
      )

instance Core.FromJSON ProjectArtifacts where
  parseJSON =
    Core.withObject "ProjectArtifacts" Core.$
      \x ->
        ProjectArtifacts'
          Core.<$> (x Core..: "type")
          Core.<*> (x Core..:? "artifactIdentifier")
          Core.<*> (x Core..:? "encryptionDisabled")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "namespaceType")
          Core.<*> (x Core..:? "overrideArtifactName")
          Core.<*> (x Core..:? "packaging")
          Core.<*> (x Core..:? "path")
