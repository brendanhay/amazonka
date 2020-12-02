{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.Artifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.Artifact where

import Network.AWS.CodePipeline.Types.ArtifactLocation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about an artifact that is worked on by actions in the pipeline.
--
--
--
-- /See:/ 'artifact' smart constructor.
data Artifact = Artifact'
  { _artLocation ::
      !(Maybe ArtifactLocation),
    _artName :: !(Maybe Text),
    _artRevision :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Artifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artLocation' - The location of an artifact.
--
-- * 'artName' - The artifact's name.
--
-- * 'artRevision' - The artifact's revision ID. Depending on the type of object, this could be a commit ID (GitHub) or a revision ID (Amazon S3).
artifact ::
  Artifact
artifact =
  Artifact'
    { _artLocation = Nothing,
      _artName = Nothing,
      _artRevision = Nothing
    }

-- | The location of an artifact.
artLocation :: Lens' Artifact (Maybe ArtifactLocation)
artLocation = lens _artLocation (\s a -> s {_artLocation = a})

-- | The artifact's name.
artName :: Lens' Artifact (Maybe Text)
artName = lens _artName (\s a -> s {_artName = a})

-- | The artifact's revision ID. Depending on the type of object, this could be a commit ID (GitHub) or a revision ID (Amazon S3).
artRevision :: Lens' Artifact (Maybe Text)
artRevision = lens _artRevision (\s a -> s {_artRevision = a})

instance FromJSON Artifact where
  parseJSON =
    withObject
      "Artifact"
      ( \x ->
          Artifact'
            <$> (x .:? "location") <*> (x .:? "name") <*> (x .:? "revision")
      )

instance Hashable Artifact

instance NFData Artifact
