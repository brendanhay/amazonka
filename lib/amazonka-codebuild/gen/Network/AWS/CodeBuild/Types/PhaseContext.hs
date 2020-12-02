{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.PhaseContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.PhaseContext where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Additional information about a build phase that has an error. You can use this information for troubleshooting.
--
--
--
-- /See:/ 'phaseContext' smart constructor.
data PhaseContext = PhaseContext'
  { _pcMessage :: !(Maybe Text),
    _pcStatusCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PhaseContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcMessage' - An explanation of the build phase's context. This might include a command ID and an exit code.
--
-- * 'pcStatusCode' - The status code for the context of the build phase.
phaseContext ::
  PhaseContext
phaseContext =
  PhaseContext' {_pcMessage = Nothing, _pcStatusCode = Nothing}

-- | An explanation of the build phase's context. This might include a command ID and an exit code.
pcMessage :: Lens' PhaseContext (Maybe Text)
pcMessage = lens _pcMessage (\s a -> s {_pcMessage = a})

-- | The status code for the context of the build phase.
pcStatusCode :: Lens' PhaseContext (Maybe Text)
pcStatusCode = lens _pcStatusCode (\s a -> s {_pcStatusCode = a})

instance FromJSON PhaseContext where
  parseJSON =
    withObject
      "PhaseContext"
      ( \x ->
          PhaseContext' <$> (x .:? "message") <*> (x .:? "statusCode")
      )

instance Hashable PhaseContext

instance NFData PhaseContext
