{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.DebugSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.DebugSession where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the debug session for a build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
--
--
--
-- /See:/ 'debugSession' smart constructor.
data DebugSession = DebugSession'
  { _dsSessionEnabled ::
      !(Maybe Bool),
    _dsSessionTarget :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DebugSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSessionEnabled' - Specifies if session debugging is enabled for this build.
--
-- * 'dsSessionTarget' - Contains the identifier of the Session Manager session used for the build. To work with the paused build, you open this session to examine, control, and resume the build.
debugSession ::
  DebugSession
debugSession =
  DebugSession'
    { _dsSessionEnabled = Nothing,
      _dsSessionTarget = Nothing
    }

-- | Specifies if session debugging is enabled for this build.
dsSessionEnabled :: Lens' DebugSession (Maybe Bool)
dsSessionEnabled = lens _dsSessionEnabled (\s a -> s {_dsSessionEnabled = a})

-- | Contains the identifier of the Session Manager session used for the build. To work with the paused build, you open this session to examine, control, and resume the build.
dsSessionTarget :: Lens' DebugSession (Maybe Text)
dsSessionTarget = lens _dsSessionTarget (\s a -> s {_dsSessionTarget = a})

instance FromJSON DebugSession where
  parseJSON =
    withObject
      "DebugSession"
      ( \x ->
          DebugSession'
            <$> (x .:? "sessionEnabled") <*> (x .:? "sessionTarget")
      )

instance Hashable DebugSession

instance NFData DebugSession
