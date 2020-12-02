{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.InputArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.InputArtifact where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about an artifact to be worked on, such as a test or build artifact.
--
--
--
-- /See:/ 'inputArtifact' smart constructor.
newtype InputArtifact = InputArtifact' {_iaName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaName' - The name of the artifact to be worked on (for example, "My App"). The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
inputArtifact ::
  -- | 'iaName'
  Text ->
  InputArtifact
inputArtifact pName_ = InputArtifact' {_iaName = pName_}

-- | The name of the artifact to be worked on (for example, "My App"). The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions.
iaName :: Lens' InputArtifact Text
iaName = lens _iaName (\s a -> s {_iaName = a})

instance FromJSON InputArtifact where
  parseJSON =
    withObject
      "InputArtifact"
      (\x -> InputArtifact' <$> (x .: "name"))

instance Hashable InputArtifact

instance NFData InputArtifact

instance ToJSON InputArtifact where
  toJSON InputArtifact' {..} =
    object (catMaybes [Just ("name" .= _iaName)])
