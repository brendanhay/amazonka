{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ArtifactDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactDetail where

import Network.AWS.CodePipeline.Types.S3Location
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Artifact details for the action execution, such as the artifact location.
--
--
--
-- /See:/ 'artifactDetail' smart constructor.
data ArtifactDetail = ArtifactDetail'
  { _aName :: !(Maybe Text),
    _aS3location :: !(Maybe S3Location)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArtifactDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aName' - The artifact object name for the action execution.
--
-- * 'aS3location' - The Amazon S3 artifact location for the action execution.
artifactDetail ::
  ArtifactDetail
artifactDetail =
  ArtifactDetail' {_aName = Nothing, _aS3location = Nothing}

-- | The artifact object name for the action execution.
aName :: Lens' ArtifactDetail (Maybe Text)
aName = lens _aName (\s a -> s {_aName = a})

-- | The Amazon S3 artifact location for the action execution.
aS3location :: Lens' ArtifactDetail (Maybe S3Location)
aS3location = lens _aS3location (\s a -> s {_aS3location = a})

instance FromJSON ArtifactDetail where
  parseJSON =
    withObject
      "ArtifactDetail"
      ( \x ->
          ArtifactDetail' <$> (x .:? "name") <*> (x .:? "s3location")
      )

instance Hashable ArtifactDetail

instance NFData ArtifactDetail
