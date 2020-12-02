{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Command
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Command where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An entity describing an executable that runs on a cluster.
--
--
--
-- /See:/ 'command' smart constructor.
data Command = Command'
  { _comArgs :: !(Maybe [Text]),
    _comScriptPath :: !(Maybe Text),
    _comName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'comArgs' - Arguments for Amazon EMR to pass to the command for execution.
--
-- * 'comScriptPath' - The Amazon S3 location of the command script.
--
-- * 'comName' - The name of the command.
command ::
  Command
command =
  Command'
    { _comArgs = Nothing,
      _comScriptPath = Nothing,
      _comName = Nothing
    }

-- | Arguments for Amazon EMR to pass to the command for execution.
comArgs :: Lens' Command [Text]
comArgs = lens _comArgs (\s a -> s {_comArgs = a}) . _Default . _Coerce

-- | The Amazon S3 location of the command script.
comScriptPath :: Lens' Command (Maybe Text)
comScriptPath = lens _comScriptPath (\s a -> s {_comScriptPath = a})

-- | The name of the command.
comName :: Lens' Command (Maybe Text)
comName = lens _comName (\s a -> s {_comName = a})

instance FromJSON Command where
  parseJSON =
    withObject
      "Command"
      ( \x ->
          Command'
            <$> (x .:? "Args" .!= mempty)
            <*> (x .:? "ScriptPath")
            <*> (x .:? "Name")
      )

instance Hashable Command

instance NFData Command
