{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildArtifacts where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about build output artifacts.
--
--
--
-- /See:/ 'buildArtifacts' smart constructor.
data BuildArtifacts = BuildArtifacts'
  { _baLocation :: !(Maybe Text),
    _baMd5sum :: !(Maybe Text),
    _baEncryptionDisabled :: !(Maybe Bool),
    _baOverrideArtifactName :: !(Maybe Bool),
    _baArtifactIdentifier :: !(Maybe Text),
    _baSha256sum :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baLocation' - Information about the location of the build artifacts.
--
-- * 'baMd5sum' - The MD5 hash of the build artifact. You can use this hash along with a checksum tool to confirm file integrity and authenticity.
--
-- * 'baEncryptionDisabled' - Information that tells you if encryption for build artifacts is disabled.
--
-- * 'baOverrideArtifactName' - If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
--
-- * 'baArtifactIdentifier' - An identifier for this artifact definition.
--
-- * 'baSha256sum' - The SHA-256 hash of the build artifact. You can use this hash along with a checksum tool to confirm file integrity and authenticity.
buildArtifacts ::
  BuildArtifacts
buildArtifacts =
  BuildArtifacts'
    { _baLocation = Nothing,
      _baMd5sum = Nothing,
      _baEncryptionDisabled = Nothing,
      _baOverrideArtifactName = Nothing,
      _baArtifactIdentifier = Nothing,
      _baSha256sum = Nothing
    }

-- | Information about the location of the build artifacts.
baLocation :: Lens' BuildArtifacts (Maybe Text)
baLocation = lens _baLocation (\s a -> s {_baLocation = a})

-- | The MD5 hash of the build artifact. You can use this hash along with a checksum tool to confirm file integrity and authenticity.
baMd5sum :: Lens' BuildArtifacts (Maybe Text)
baMd5sum = lens _baMd5sum (\s a -> s {_baMd5sum = a})

-- | Information that tells you if encryption for build artifacts is disabled.
baEncryptionDisabled :: Lens' BuildArtifacts (Maybe Bool)
baEncryptionDisabled = lens _baEncryptionDisabled (\s a -> s {_baEncryptionDisabled = a})

-- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
baOverrideArtifactName :: Lens' BuildArtifacts (Maybe Bool)
baOverrideArtifactName = lens _baOverrideArtifactName (\s a -> s {_baOverrideArtifactName = a})

-- | An identifier for this artifact definition.
baArtifactIdentifier :: Lens' BuildArtifacts (Maybe Text)
baArtifactIdentifier = lens _baArtifactIdentifier (\s a -> s {_baArtifactIdentifier = a})

-- | The SHA-256 hash of the build artifact. You can use this hash along with a checksum tool to confirm file integrity and authenticity.
baSha256sum :: Lens' BuildArtifacts (Maybe Text)
baSha256sum = lens _baSha256sum (\s a -> s {_baSha256sum = a})

instance FromJSON BuildArtifacts where
  parseJSON =
    withObject
      "BuildArtifacts"
      ( \x ->
          BuildArtifacts'
            <$> (x .:? "location")
            <*> (x .:? "md5sum")
            <*> (x .:? "encryptionDisabled")
            <*> (x .:? "overrideArtifactName")
            <*> (x .:? "artifactIdentifier")
            <*> (x .:? "sha256sum")
      )

instance Hashable BuildArtifacts

instance NFData BuildArtifacts
