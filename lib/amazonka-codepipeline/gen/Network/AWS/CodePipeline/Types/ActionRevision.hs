{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionRevision where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the version (or revision) of an action.
--
--
--
-- /See:/ 'actionRevision' smart constructor.
data ActionRevision = ActionRevision'
  { _aRevisionId :: !Text,
    _aRevisionChangeId :: !Text,
    _aCreated :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActionRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aRevisionId' - The system-generated unique ID that identifies the revision number of the action.
--
-- * 'aRevisionChangeId' - The unique identifier of the change that set the state to this revision (for example, a deployment ID or timestamp).
--
-- * 'aCreated' - The date and time when the most recent version of the action was created, in timestamp format.
actionRevision ::
  -- | 'aRevisionId'
  Text ->
  -- | 'aRevisionChangeId'
  Text ->
  -- | 'aCreated'
  UTCTime ->
  ActionRevision
actionRevision pRevisionId_ pRevisionChangeId_ pCreated_ =
  ActionRevision'
    { _aRevisionId = pRevisionId_,
      _aRevisionChangeId = pRevisionChangeId_,
      _aCreated = _Time # pCreated_
    }

-- | The system-generated unique ID that identifies the revision number of the action.
aRevisionId :: Lens' ActionRevision Text
aRevisionId = lens _aRevisionId (\s a -> s {_aRevisionId = a})

-- | The unique identifier of the change that set the state to this revision (for example, a deployment ID or timestamp).
aRevisionChangeId :: Lens' ActionRevision Text
aRevisionChangeId = lens _aRevisionChangeId (\s a -> s {_aRevisionChangeId = a})

-- | The date and time when the most recent version of the action was created, in timestamp format.
aCreated :: Lens' ActionRevision UTCTime
aCreated = lens _aCreated (\s a -> s {_aCreated = a}) . _Time

instance FromJSON ActionRevision where
  parseJSON =
    withObject
      "ActionRevision"
      ( \x ->
          ActionRevision'
            <$> (x .: "revisionId")
            <*> (x .: "revisionChangeId")
            <*> (x .: "created")
      )

instance Hashable ActionRevision

instance NFData ActionRevision

instance ToJSON ActionRevision where
  toJSON ActionRevision' {..} =
    object
      ( catMaybes
          [ Just ("revisionId" .= _aRevisionId),
            Just ("revisionChangeId" .= _aRevisionChangeId),
            Just ("created" .= _aCreated)
          ]
      )
