{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectArtifacts where

import Network.AWS.CodeBuild.Types.ArtifactNamespace
import Network.AWS.CodeBuild.Types.ArtifactPackaging
import Network.AWS.CodeBuild.Types.ArtifactsType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the build output artifacts for the build project.
--
--
--
-- /See:/ 'projectArtifacts' smart constructor.
data ProjectArtifacts = ProjectArtifacts'
  { _paPackaging ::
      !(Maybe ArtifactPackaging),
    _paPath :: !(Maybe Text),
    _paLocation :: !(Maybe Text),
    _paName :: !(Maybe Text),
    _paEncryptionDisabled :: !(Maybe Bool),
    _paOverrideArtifactName :: !(Maybe Bool),
    _paArtifactIdentifier :: !(Maybe Text),
    _paNamespaceType :: !(Maybe ArtifactNamespace),
    _paType :: !ArtifactsType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paPackaging' - The type of build output artifact to create:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output artifacts instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , valid values include:     * @NONE@ : AWS CodeBuild creates in the output bucket a folder that contains the build output. This is the default if @packaging@ is not specified.     * @ZIP@ : AWS CodeBuild creates in the output bucket a ZIP file that contains the build output.
--
-- * 'paPath' - Along with @namespaceType@ and @name@ , the pattern that AWS CodeBuild uses to name and store the output artifact:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , this is the path to the output artifact. If @path@ is not specified, @path@ is not used. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @NONE@ , and @name@ is set to @MyArtifact.zip@ , the output artifact is stored in the output bucket at @MyArtifacts/MyArtifact.zip@ .
--
-- * 'paLocation' - Information about the build output artifact location:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output locations instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , this is the name of the output bucket.
--
-- * 'paName' - Along with @path@ and @namespaceType@ , the pattern that AWS CodeBuild uses to name and store the output artifact:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , this is the name of the output artifact object. If you set the name to be a forward slash ("/"), the artifact is stored in the root of the output bucket. For example:     * If @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact is stored in @MyArtifacts/<build-ID>/MyArtifact.zip@ .      * If @path@ is empty, @namespaceType@ is set to @NONE@ , and @name@ is set to "@/@ ", the output artifact is stored in the root of the output bucket.      * If @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to "@/@ ", the output artifact is stored in @MyArtifacts/<build-ID>@ .
--
-- * 'paEncryptionDisabled' - Set to true if you do not want your output artifacts encrypted. This option is valid only if your artifacts type is Amazon Simple Storage Service (Amazon S3). If this is set with another artifacts type, an invalidInputException is thrown.
--
-- * 'paOverrideArtifactName' - If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
--
-- * 'paArtifactIdentifier' - An identifier for this artifact definition.
--
-- * 'paNamespaceType' - Along with @path@ and @name@ , the pattern that AWS CodeBuild uses to determine the name and location to store the output artifact:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , valid values include:     * @BUILD_ID@ : Include the build ID in the location of the build output artifact.     * @NONE@ : Do not include the build ID. This is the default if @namespaceType@ is not specified. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , the output artifact is stored in @MyArtifacts/<build-ID>/MyArtifact.zip@ .
--
-- * 'paType' - The type of build output artifact. Valid values include:     * @CODEPIPELINE@ : The build project has build output generated through AWS CodePipeline.      * @NO_ARTIFACTS@ : The build project does not produce any build output.     * @S3@ : The build project stores build output in Amazon Simple Storage Service (Amazon S3).
projectArtifacts ::
  -- | 'paType'
  ArtifactsType ->
  ProjectArtifacts
projectArtifacts pType_ =
  ProjectArtifacts'
    { _paPackaging = Nothing,
      _paPath = Nothing,
      _paLocation = Nothing,
      _paName = Nothing,
      _paEncryptionDisabled = Nothing,
      _paOverrideArtifactName = Nothing,
      _paArtifactIdentifier = Nothing,
      _paNamespaceType = Nothing,
      _paType = pType_
    }

-- | The type of build output artifact to create:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output artifacts instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , valid values include:     * @NONE@ : AWS CodeBuild creates in the output bucket a folder that contains the build output. This is the default if @packaging@ is not specified.     * @ZIP@ : AWS CodeBuild creates in the output bucket a ZIP file that contains the build output.
paPackaging :: Lens' ProjectArtifacts (Maybe ArtifactPackaging)
paPackaging = lens _paPackaging (\s a -> s {_paPackaging = a})

-- | Along with @namespaceType@ and @name@ , the pattern that AWS CodeBuild uses to name and store the output artifact:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , this is the path to the output artifact. If @path@ is not specified, @path@ is not used. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @NONE@ , and @name@ is set to @MyArtifact.zip@ , the output artifact is stored in the output bucket at @MyArtifacts/MyArtifact.zip@ .
paPath :: Lens' ProjectArtifacts (Maybe Text)
paPath = lens _paPath (\s a -> s {_paPath = a})

-- | Information about the build output artifact location:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output locations instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , this is the name of the output bucket.
paLocation :: Lens' ProjectArtifacts (Maybe Text)
paLocation = lens _paLocation (\s a -> s {_paLocation = a})

-- | Along with @path@ and @namespaceType@ , the pattern that AWS CodeBuild uses to name and store the output artifact:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , this is the name of the output artifact object. If you set the name to be a forward slash ("/"), the artifact is stored in the root of the output bucket. For example:     * If @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact is stored in @MyArtifacts/<build-ID>/MyArtifact.zip@ .      * If @path@ is empty, @namespaceType@ is set to @NONE@ , and @name@ is set to "@/@ ", the output artifact is stored in the root of the output bucket.      * If @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to "@/@ ", the output artifact is stored in @MyArtifacts/<build-ID>@ .
paName :: Lens' ProjectArtifacts (Maybe Text)
paName = lens _paName (\s a -> s {_paName = a})

-- | Set to true if you do not want your output artifacts encrypted. This option is valid only if your artifacts type is Amazon Simple Storage Service (Amazon S3). If this is set with another artifacts type, an invalidInputException is thrown.
paEncryptionDisabled :: Lens' ProjectArtifacts (Maybe Bool)
paEncryptionDisabled = lens _paEncryptionDisabled (\s a -> s {_paEncryptionDisabled = a})

-- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
paOverrideArtifactName :: Lens' ProjectArtifacts (Maybe Bool)
paOverrideArtifactName = lens _paOverrideArtifactName (\s a -> s {_paOverrideArtifactName = a})

-- | An identifier for this artifact definition.
paArtifactIdentifier :: Lens' ProjectArtifacts (Maybe Text)
paArtifactIdentifier = lens _paArtifactIdentifier (\s a -> s {_paArtifactIdentifier = a})

-- | Along with @path@ and @name@ , the pattern that AWS CodeBuild uses to determine the name and location to store the output artifact:     * If @type@ is set to @CODEPIPELINE@ , AWS CodePipeline ignores this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , this value is ignored if specified, because no build output is produced.     * If @type@ is set to @S3@ , valid values include:     * @BUILD_ID@ : Include the build ID in the location of the build output artifact.     * @NONE@ : Do not include the build ID. This is the default if @namespaceType@ is not specified. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , the output artifact is stored in @MyArtifacts/<build-ID>/MyArtifact.zip@ .
paNamespaceType :: Lens' ProjectArtifacts (Maybe ArtifactNamespace)
paNamespaceType = lens _paNamespaceType (\s a -> s {_paNamespaceType = a})

-- | The type of build output artifact. Valid values include:     * @CODEPIPELINE@ : The build project has build output generated through AWS CodePipeline.      * @NO_ARTIFACTS@ : The build project does not produce any build output.     * @S3@ : The build project stores build output in Amazon Simple Storage Service (Amazon S3).
paType :: Lens' ProjectArtifacts ArtifactsType
paType = lens _paType (\s a -> s {_paType = a})

instance FromJSON ProjectArtifacts where
  parseJSON =
    withObject
      "ProjectArtifacts"
      ( \x ->
          ProjectArtifacts'
            <$> (x .:? "packaging")
            <*> (x .:? "path")
            <*> (x .:? "location")
            <*> (x .:? "name")
            <*> (x .:? "encryptionDisabled")
            <*> (x .:? "overrideArtifactName")
            <*> (x .:? "artifactIdentifier")
            <*> (x .:? "namespaceType")
            <*> (x .: "type")
      )

instance Hashable ProjectArtifacts

instance NFData ProjectArtifacts

instance ToJSON ProjectArtifacts where
  toJSON ProjectArtifacts' {..} =
    object
      ( catMaybes
          [ ("packaging" .=) <$> _paPackaging,
            ("path" .=) <$> _paPath,
            ("location" .=) <$> _paLocation,
            ("name" .=) <$> _paName,
            ("encryptionDisabled" .=) <$> _paEncryptionDisabled,
            ("overrideArtifactName" .=) <$> _paOverrideArtifactName,
            ("artifactIdentifier" .=) <$> _paArtifactIdentifier,
            ("namespaceType" .=) <$> _paNamespaceType,
            Just ("type" .= _paType)
          ]
      )
