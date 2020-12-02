{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactStore where

import Network.AWS.CodePipeline.Types.ArtifactStoreType
import Network.AWS.CodePipeline.Types.EncryptionKey
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The S3 bucket where artifacts for the pipeline are stored.
--
--
--
-- /See:/ 'artifactStore' smart constructor.
data ArtifactStore = ArtifactStore'
  { _asEncryptionKey ::
      !(Maybe EncryptionKey),
    _asType :: !ArtifactStoreType,
    _asLocation :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArtifactStore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asEncryptionKey' - The encryption key used to encrypt the data in the artifact store, such as an AWS Key Management Service (AWS KMS) key. If this is undefined, the default key for Amazon S3 is used.
--
-- * 'asType' - The type of the artifact store, such as S3.
--
-- * 'asLocation' - The S3 bucket used for storing the artifacts for a pipeline. You can specify the name of an S3 bucket but not a folder in the bucket. A folder to contain the pipeline artifacts is created for you based on the name of the pipeline. You can use any S3 bucket in the same AWS Region as the pipeline to store your pipeline artifacts.
artifactStore ::
  -- | 'asType'
  ArtifactStoreType ->
  -- | 'asLocation'
  Text ->
  ArtifactStore
artifactStore pType_ pLocation_ =
  ArtifactStore'
    { _asEncryptionKey = Nothing,
      _asType = pType_,
      _asLocation = pLocation_
    }

-- | The encryption key used to encrypt the data in the artifact store, such as an AWS Key Management Service (AWS KMS) key. If this is undefined, the default key for Amazon S3 is used.
asEncryptionKey :: Lens' ArtifactStore (Maybe EncryptionKey)
asEncryptionKey = lens _asEncryptionKey (\s a -> s {_asEncryptionKey = a})

-- | The type of the artifact store, such as S3.
asType :: Lens' ArtifactStore ArtifactStoreType
asType = lens _asType (\s a -> s {_asType = a})

-- | The S3 bucket used for storing the artifacts for a pipeline. You can specify the name of an S3 bucket but not a folder in the bucket. A folder to contain the pipeline artifacts is created for you based on the name of the pipeline. You can use any S3 bucket in the same AWS Region as the pipeline to store your pipeline artifacts.
asLocation :: Lens' ArtifactStore Text
asLocation = lens _asLocation (\s a -> s {_asLocation = a})

instance FromJSON ArtifactStore where
  parseJSON =
    withObject
      "ArtifactStore"
      ( \x ->
          ArtifactStore'
            <$> (x .:? "encryptionKey") <*> (x .: "type") <*> (x .: "location")
      )

instance Hashable ArtifactStore

instance NFData ArtifactStore

instance ToJSON ArtifactStore where
  toJSON ArtifactStore' {..} =
    object
      ( catMaybes
          [ ("encryptionKey" .=) <$> _asEncryptionKey,
            Just ("type" .= _asType),
            Just ("location" .= _asLocation)
          ]
      )
