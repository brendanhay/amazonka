{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageDeclaration where

import Network.AWS.CodePipeline.Types.ActionDeclaration
import Network.AWS.CodePipeline.Types.BlockerDeclaration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about a stage and its definition.
--
--
--
-- /See:/ 'stageDeclaration' smart constructor.
data StageDeclaration = StageDeclaration'
  { _sdBlockers ::
      !(Maybe [BlockerDeclaration]),
    _sdName :: !Text,
    _sdActions :: ![ActionDeclaration]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StageDeclaration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdBlockers' - Reserved for future use.
--
-- * 'sdName' - The name of the stage.
--
-- * 'sdActions' - The actions included in a stage.
stageDeclaration ::
  -- | 'sdName'
  Text ->
  StageDeclaration
stageDeclaration pName_ =
  StageDeclaration'
    { _sdBlockers = Nothing,
      _sdName = pName_,
      _sdActions = mempty
    }

-- | Reserved for future use.
sdBlockers :: Lens' StageDeclaration [BlockerDeclaration]
sdBlockers = lens _sdBlockers (\s a -> s {_sdBlockers = a}) . _Default . _Coerce

-- | The name of the stage.
sdName :: Lens' StageDeclaration Text
sdName = lens _sdName (\s a -> s {_sdName = a})

-- | The actions included in a stage.
sdActions :: Lens' StageDeclaration [ActionDeclaration]
sdActions = lens _sdActions (\s a -> s {_sdActions = a}) . _Coerce

instance FromJSON StageDeclaration where
  parseJSON =
    withObject
      "StageDeclaration"
      ( \x ->
          StageDeclaration'
            <$> (x .:? "blockers" .!= mempty)
            <*> (x .: "name")
            <*> (x .:? "actions" .!= mempty)
      )

instance Hashable StageDeclaration

instance NFData StageDeclaration

instance ToJSON StageDeclaration where
  toJSON StageDeclaration' {..} =
    object
      ( catMaybes
          [ ("blockers" .=) <$> _sdBlockers,
            Just ("name" .= _sdName),
            Just ("actions" .= _sdActions)
          ]
      )
