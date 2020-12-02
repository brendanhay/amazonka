{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A trigger failed to run.
--
--
--
-- /See:/ 'repositoryTriggerExecutionFailure' smart constructor.
data RepositoryTriggerExecutionFailure = RepositoryTriggerExecutionFailure'
  { _rtefFailureMessage ::
      !(Maybe Text),
    _rtefTrigger ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RepositoryTriggerExecutionFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtefFailureMessage' - Message information about the trigger that did not run.
--
-- * 'rtefTrigger' - The name of the trigger that did not run.
repositoryTriggerExecutionFailure ::
  RepositoryTriggerExecutionFailure
repositoryTriggerExecutionFailure =
  RepositoryTriggerExecutionFailure'
    { _rtefFailureMessage = Nothing,
      _rtefTrigger = Nothing
    }

-- | Message information about the trigger that did not run.
rtefFailureMessage :: Lens' RepositoryTriggerExecutionFailure (Maybe Text)
rtefFailureMessage = lens _rtefFailureMessage (\s a -> s {_rtefFailureMessage = a})

-- | The name of the trigger that did not run.
rtefTrigger :: Lens' RepositoryTriggerExecutionFailure (Maybe Text)
rtefTrigger = lens _rtefTrigger (\s a -> s {_rtefTrigger = a})

instance FromJSON RepositoryTriggerExecutionFailure where
  parseJSON =
    withObject
      "RepositoryTriggerExecutionFailure"
      ( \x ->
          RepositoryTriggerExecutionFailure'
            <$> (x .:? "failureMessage") <*> (x .:? "trigger")
      )

instance Hashable RepositoryTriggerExecutionFailure

instance NFData RepositoryTriggerExecutionFailure
