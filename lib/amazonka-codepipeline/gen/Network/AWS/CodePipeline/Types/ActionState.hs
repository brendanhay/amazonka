{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionState where

import Network.AWS.CodePipeline.Types.ActionExecution
import Network.AWS.CodePipeline.Types.ActionRevision
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the state of an action.
--
--
--
-- /See:/ 'actionState' smart constructor.
data ActionState = ActionState'
  { _asRevisionURL :: !(Maybe Text),
    _asEntityURL :: !(Maybe Text),
    _asActionName :: !(Maybe Text),
    _asCurrentRevision :: !(Maybe ActionRevision),
    _asLatestExecution :: !(Maybe ActionExecution)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asRevisionURL' - A URL link for more information about the revision, such as a commit details page.
--
-- * 'asEntityURL' - A URL link for more information about the state of the action, such as a deployment group details page.
--
-- * 'asActionName' - The name of the action.
--
-- * 'asCurrentRevision' - Represents information about the version (or revision) of an action.
--
-- * 'asLatestExecution' - Represents information about the run of an action.
actionState ::
  ActionState
actionState =
  ActionState'
    { _asRevisionURL = Nothing,
      _asEntityURL = Nothing,
      _asActionName = Nothing,
      _asCurrentRevision = Nothing,
      _asLatestExecution = Nothing
    }

-- | A URL link for more information about the revision, such as a commit details page.
asRevisionURL :: Lens' ActionState (Maybe Text)
asRevisionURL = lens _asRevisionURL (\s a -> s {_asRevisionURL = a})

-- | A URL link for more information about the state of the action, such as a deployment group details page.
asEntityURL :: Lens' ActionState (Maybe Text)
asEntityURL = lens _asEntityURL (\s a -> s {_asEntityURL = a})

-- | The name of the action.
asActionName :: Lens' ActionState (Maybe Text)
asActionName = lens _asActionName (\s a -> s {_asActionName = a})

-- | Represents information about the version (or revision) of an action.
asCurrentRevision :: Lens' ActionState (Maybe ActionRevision)
asCurrentRevision = lens _asCurrentRevision (\s a -> s {_asCurrentRevision = a})

-- | Represents information about the run of an action.
asLatestExecution :: Lens' ActionState (Maybe ActionExecution)
asLatestExecution = lens _asLatestExecution (\s a -> s {_asLatestExecution = a})

instance FromJSON ActionState where
  parseJSON =
    withObject
      "ActionState"
      ( \x ->
          ActionState'
            <$> (x .:? "revisionUrl")
            <*> (x .:? "entityUrl")
            <*> (x .:? "actionName")
            <*> (x .:? "currentRevision")
            <*> (x .:? "latestExecution")
      )

instance Hashable ActionState

instance NFData ActionState
