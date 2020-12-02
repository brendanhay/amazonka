{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactLocation where

import Network.AWS.CodePipeline.Types.ArtifactLocationType
import Network.AWS.CodePipeline.Types.S3ArtifactLocation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the location of an artifact.
--
--
--
-- /See:/ 'artifactLocation' smart constructor.
data ArtifactLocation = ArtifactLocation'
  { _alS3Location ::
      !(Maybe S3ArtifactLocation),
    _alType :: !(Maybe ArtifactLocationType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArtifactLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alS3Location' - The S3 bucket that contains the artifact.
--
-- * 'alType' - The type of artifact in the location.
artifactLocation ::
  ArtifactLocation
artifactLocation =
  ArtifactLocation' {_alS3Location = Nothing, _alType = Nothing}

-- | The S3 bucket that contains the artifact.
alS3Location :: Lens' ArtifactLocation (Maybe S3ArtifactLocation)
alS3Location = lens _alS3Location (\s a -> s {_alS3Location = a})

-- | The type of artifact in the location.
alType :: Lens' ArtifactLocation (Maybe ArtifactLocationType)
alType = lens _alType (\s a -> s {_alType = a})

instance FromJSON ArtifactLocation where
  parseJSON =
    withObject
      "ArtifactLocation"
      ( \x ->
          ArtifactLocation' <$> (x .:? "s3Location") <*> (x .:? "type")
      )

instance Hashable ArtifactLocation

instance NFData ArtifactLocation
