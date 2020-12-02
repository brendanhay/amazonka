{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ServerProcess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ServerProcess where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A set of instructions for launching server processes on each instance in a fleet. Server processes run either a custom game build executable or a Realtime Servers script. Each instruction set identifies the location of the custom game build executable or Realtime launch script, optional launch parameters, and the number of server processes with this configuration to maintain concurrently on the instance. Server process configurations make up a fleet's @'RuntimeConfiguration' @ .
--
--
--
-- /See:/ 'serverProcess' smart constructor.
data ServerProcess = ServerProcess'
  { _spParameters :: !(Maybe Text),
    _spLaunchPath :: !Text,
    _spConcurrentExecutions :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerProcess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spParameters' - An optional list of parameters to pass to the server executable or Realtime script on launch.
--
-- * 'spLaunchPath' - The location of the server executable in a custom game build or the name of the Realtime script file that contains the @Init()@ function. Game builds and Realtime scripts are installed on instances at the root:      * Windows (for custom game builds only): @C:\game@ . Example: "@C:\game\MyGame\server.exe@ "      * Linux: @/local/game@ . Examples: "@/local/game/MyGame/server.exe@ " or "@/local/game/MyRealtimeScript.js@ "
--
-- * 'spConcurrentExecutions' - The number of server processes that use this configuration to run concurrently on an instance.
serverProcess ::
  -- | 'spLaunchPath'
  Text ->
  -- | 'spConcurrentExecutions'
  Natural ->
  ServerProcess
serverProcess pLaunchPath_ pConcurrentExecutions_ =
  ServerProcess'
    { _spParameters = Nothing,
      _spLaunchPath = pLaunchPath_,
      _spConcurrentExecutions = _Nat # pConcurrentExecutions_
    }

-- | An optional list of parameters to pass to the server executable or Realtime script on launch.
spParameters :: Lens' ServerProcess (Maybe Text)
spParameters = lens _spParameters (\s a -> s {_spParameters = a})

-- | The location of the server executable in a custom game build or the name of the Realtime script file that contains the @Init()@ function. Game builds and Realtime scripts are installed on instances at the root:      * Windows (for custom game builds only): @C:\game@ . Example: "@C:\game\MyGame\server.exe@ "      * Linux: @/local/game@ . Examples: "@/local/game/MyGame/server.exe@ " or "@/local/game/MyRealtimeScript.js@ "
spLaunchPath :: Lens' ServerProcess Text
spLaunchPath = lens _spLaunchPath (\s a -> s {_spLaunchPath = a})

-- | The number of server processes that use this configuration to run concurrently on an instance.
spConcurrentExecutions :: Lens' ServerProcess Natural
spConcurrentExecutions = lens _spConcurrentExecutions (\s a -> s {_spConcurrentExecutions = a}) . _Nat

instance FromJSON ServerProcess where
  parseJSON =
    withObject
      "ServerProcess"
      ( \x ->
          ServerProcess'
            <$> (x .:? "Parameters")
            <*> (x .: "LaunchPath")
            <*> (x .: "ConcurrentExecutions")
      )

instance Hashable ServerProcess

instance NFData ServerProcess

instance ToJSON ServerProcess where
  toJSON ServerProcess' {..} =
    object
      ( catMaybes
          [ ("Parameters" .=) <$> _spParameters,
            Just ("LaunchPath" .= _spLaunchPath),
            Just ("ConcurrentExecutions" .= _spConcurrentExecutions)
          ]
      )
