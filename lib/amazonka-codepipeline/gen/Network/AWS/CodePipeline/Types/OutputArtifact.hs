{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.OutputArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.OutputArtifact where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the output of an action.
--
--
--
-- /See:/ 'outputArtifact' smart constructor.
newtype OutputArtifact = OutputArtifact' {_oaName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oaName' - The name of the output of an artifact, such as "My App". The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions. Output artifact names must be unique within a pipeline.
outputArtifact ::
  -- | 'oaName'
  Text ->
  OutputArtifact
outputArtifact pName_ = OutputArtifact' {_oaName = pName_}

-- | The name of the output of an artifact, such as "My App". The input artifact of an action must exactly match the output artifact declared in a preceding action, but the input artifact does not have to be the next action in strict sequence from the action that provided the output artifact. Actions in parallel can declare different output artifacts, which are in turn consumed by different following actions. Output artifact names must be unique within a pipeline.
oaName :: Lens' OutputArtifact Text
oaName = lens _oaName (\s a -> s {_oaName = a})

instance FromJSON OutputArtifact where
  parseJSON =
    withObject
      "OutputArtifact"
      (\x -> OutputArtifact' <$> (x .: "name"))

instance Hashable OutputArtifact

instance NFData OutputArtifact

instance ToJSON OutputArtifact where
  toJSON OutputArtifact' {..} =
    object (catMaybes [Just ("name" .= _oaName)])
