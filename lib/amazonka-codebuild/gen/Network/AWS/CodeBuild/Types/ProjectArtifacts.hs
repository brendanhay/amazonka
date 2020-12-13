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
    paPackaging,
    paPath,
    paLocation,
    paName,
    paEncryptionDisabled,
    paOverrideArtifactName,
    paArtifactIdentifier,
    paType,
    paNamespaceType,
  )
where

import Network.AWS.CodeBuild.Types.ArtifactNamespace
import Network.AWS.CodeBuild.Types.ArtifactPackaging
import Network.AWS.CodeBuild.Types.ArtifactsType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the build output artifacts for the build project.
--
-- /See:/ 'mkProjectArtifacts' smart constructor.
data ProjectArtifacts = ProjectArtifacts'
  { -- | The type of build output artifact to create:
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
    packaging :: Lude.Maybe ArtifactPackaging,
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
    path :: Lude.Maybe Lude.Text,
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
    location :: Lude.Maybe Lude.Text,
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
    name :: Lude.Maybe Lude.Text,
    -- | Set to true if you do not want your output artifacts encrypted. This option is valid only if your artifacts type is Amazon Simple Storage Service (Amazon S3). If this is set with another artifacts type, an invalidInputException is thrown.
    encryptionDisabled :: Lude.Maybe Lude.Bool,
    -- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
    overrideArtifactName :: Lude.Maybe Lude.Bool,
    -- | An identifier for this artifact definition.
    artifactIdentifier :: Lude.Maybe Lude.Text,
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
    type' :: ArtifactsType,
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
    namespaceType :: Lude.Maybe ArtifactNamespace
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectArtifacts' with the minimum fields required to make a request.
--
-- * 'packaging' - The type of build output artifact to create:
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
-- * 'path' - Along with @namespaceType@ and @name@ , the pattern that AWS CodeBuild uses to name and store the output artifact:
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
-- * 'location' - Information about the build output artifact location:
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
-- * 'name' - Along with @path@ and @namespaceType@ , the pattern that AWS CodeBuild uses to name and store the output artifact:
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
-- * 'encryptionDisabled' - Set to true if you do not want your output artifacts encrypted. This option is valid only if your artifacts type is Amazon Simple Storage Service (Amazon S3). If this is set with another artifacts type, an invalidInputException is thrown.
-- * 'overrideArtifactName' - If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
-- * 'artifactIdentifier' - An identifier for this artifact definition.
-- * 'type'' - The type of build output artifact. Valid values include:
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
-- * 'namespaceType' - Along with @path@ and @name@ , the pattern that AWS CodeBuild uses to determine the name and location to store the output artifact:
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
mkProjectArtifacts ::
  -- | 'type''
  ArtifactsType ->
  ProjectArtifacts
mkProjectArtifacts pType_ =
  ProjectArtifacts'
    { packaging = Lude.Nothing,
      path = Lude.Nothing,
      location = Lude.Nothing,
      name = Lude.Nothing,
      encryptionDisabled = Lude.Nothing,
      overrideArtifactName = Lude.Nothing,
      artifactIdentifier = Lude.Nothing,
      type' = pType_,
      namespaceType = Lude.Nothing
    }

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
paPackaging :: Lens.Lens' ProjectArtifacts (Lude.Maybe ArtifactPackaging)
paPackaging = Lens.lens (packaging :: ProjectArtifacts -> Lude.Maybe ArtifactPackaging) (\s a -> s {packaging = a} :: ProjectArtifacts)
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
paPath :: Lens.Lens' ProjectArtifacts (Lude.Maybe Lude.Text)
paPath = Lens.lens (path :: ProjectArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: ProjectArtifacts)
{-# DEPRECATED paPath "Use generic-lens or generic-optics with 'path' instead." #-}

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
paLocation :: Lens.Lens' ProjectArtifacts (Lude.Maybe Lude.Text)
paLocation = Lens.lens (location :: ProjectArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: ProjectArtifacts)
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
paName :: Lens.Lens' ProjectArtifacts (Lude.Maybe Lude.Text)
paName = Lens.lens (name :: ProjectArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProjectArtifacts)
{-# DEPRECATED paName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Set to true if you do not want your output artifacts encrypted. This option is valid only if your artifacts type is Amazon Simple Storage Service (Amazon S3). If this is set with another artifacts type, an invalidInputException is thrown.
--
-- /Note:/ Consider using 'encryptionDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paEncryptionDisabled :: Lens.Lens' ProjectArtifacts (Lude.Maybe Lude.Bool)
paEncryptionDisabled = Lens.lens (encryptionDisabled :: ProjectArtifacts -> Lude.Maybe Lude.Bool) (\s a -> s {encryptionDisabled = a} :: ProjectArtifacts)
{-# DEPRECATED paEncryptionDisabled "Use generic-lens or generic-optics with 'encryptionDisabled' instead." #-}

-- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
--
-- /Note:/ Consider using 'overrideArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paOverrideArtifactName :: Lens.Lens' ProjectArtifacts (Lude.Maybe Lude.Bool)
paOverrideArtifactName = Lens.lens (overrideArtifactName :: ProjectArtifacts -> Lude.Maybe Lude.Bool) (\s a -> s {overrideArtifactName = a} :: ProjectArtifacts)
{-# DEPRECATED paOverrideArtifactName "Use generic-lens or generic-optics with 'overrideArtifactName' instead." #-}

-- | An identifier for this artifact definition.
--
-- /Note:/ Consider using 'artifactIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paArtifactIdentifier :: Lens.Lens' ProjectArtifacts (Lude.Maybe Lude.Text)
paArtifactIdentifier = Lens.lens (artifactIdentifier :: ProjectArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {artifactIdentifier = a} :: ProjectArtifacts)
{-# DEPRECATED paArtifactIdentifier "Use generic-lens or generic-optics with 'artifactIdentifier' instead." #-}

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
paType :: Lens.Lens' ProjectArtifacts ArtifactsType
paType = Lens.lens (type' :: ProjectArtifacts -> ArtifactsType) (\s a -> s {type' = a} :: ProjectArtifacts)
{-# DEPRECATED paType "Use generic-lens or generic-optics with 'type'' instead." #-}

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
paNamespaceType :: Lens.Lens' ProjectArtifacts (Lude.Maybe ArtifactNamespace)
paNamespaceType = Lens.lens (namespaceType :: ProjectArtifacts -> Lude.Maybe ArtifactNamespace) (\s a -> s {namespaceType = a} :: ProjectArtifacts)
{-# DEPRECATED paNamespaceType "Use generic-lens or generic-optics with 'namespaceType' instead." #-}

instance Lude.FromJSON ProjectArtifacts where
  parseJSON =
    Lude.withObject
      "ProjectArtifacts"
      ( \x ->
          ProjectArtifacts'
            Lude.<$> (x Lude..:? "packaging")
            Lude.<*> (x Lude..:? "path")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "encryptionDisabled")
            Lude.<*> (x Lude..:? "overrideArtifactName")
            Lude.<*> (x Lude..:? "artifactIdentifier")
            Lude.<*> (x Lude..: "type")
            Lude.<*> (x Lude..:? "namespaceType")
      )

instance Lude.ToJSON ProjectArtifacts where
  toJSON ProjectArtifacts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("packaging" Lude..=) Lude.<$> packaging,
            ("path" Lude..=) Lude.<$> path,
            ("location" Lude..=) Lude.<$> location,
            ("name" Lude..=) Lude.<$> name,
            ("encryptionDisabled" Lude..=) Lude.<$> encryptionDisabled,
            ("overrideArtifactName" Lude..=) Lude.<$> overrideArtifactName,
            ("artifactIdentifier" Lude..=) Lude.<$> artifactIdentifier,
            Lude.Just ("type" Lude..= type'),
            ("namespaceType" Lude..=) Lude.<$> namespaceType
          ]
      )
