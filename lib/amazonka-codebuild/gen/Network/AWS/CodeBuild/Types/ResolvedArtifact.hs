{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ResolvedArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ResolvedArtifact where

import Network.AWS.CodeBuild.Types.ArtifactsType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a resolved build artifact. A resolve artifact is an artifact that is built and deployed to the destination, such as Amazon Simple Storage Service (Amazon S3).
--
--
--
-- /See:/ 'resolvedArtifact' smart constructor.
data ResolvedArtifact = ResolvedArtifact'
  { _raLocation ::
      !(Maybe Text),
    _raIdentifier :: !(Maybe Text),
    _raType :: !(Maybe ArtifactsType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResolvedArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raLocation' - The location of the artifact.
--
-- * 'raIdentifier' - The identifier of the artifact.
--
-- * 'raType' - Specifies the type of artifact.
resolvedArtifact ::
  ResolvedArtifact
resolvedArtifact =
  ResolvedArtifact'
    { _raLocation = Nothing,
      _raIdentifier = Nothing,
      _raType = Nothing
    }

-- | The location of the artifact.
raLocation :: Lens' ResolvedArtifact (Maybe Text)
raLocation = lens _raLocation (\s a -> s {_raLocation = a})

-- | The identifier of the artifact.
raIdentifier :: Lens' ResolvedArtifact (Maybe Text)
raIdentifier = lens _raIdentifier (\s a -> s {_raIdentifier = a})

-- | Specifies the type of artifact.
raType :: Lens' ResolvedArtifact (Maybe ArtifactsType)
raType = lens _raType (\s a -> s {_raType = a})

instance FromJSON ResolvedArtifact where
  parseJSON =
    withObject
      "ResolvedArtifact"
      ( \x ->
          ResolvedArtifact'
            <$> (x .:? "location") <*> (x .:? "identifier") <*> (x .:? "type")
      )

instance Hashable ResolvedArtifact

instance NFData ResolvedArtifact
