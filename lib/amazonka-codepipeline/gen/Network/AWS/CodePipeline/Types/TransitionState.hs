{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.TransitionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.TransitionState where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the state of transitions between one stage and another stage.
--
--
--
-- /See:/ 'transitionState' smart constructor.
data TransitionState = TransitionState'
  { _tsEnabled ::
      !(Maybe Bool),
    _tsDisabledReason :: !(Maybe Text),
    _tsLastChangedAt :: !(Maybe POSIX),
    _tsLastChangedBy :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransitionState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsEnabled' - Whether the transition between stages is enabled (true) or disabled (false).
--
-- * 'tsDisabledReason' - The user-specified reason why the transition between two stages of a pipeline was disabled.
--
-- * 'tsLastChangedAt' - The timestamp when the transition state was last changed.
--
-- * 'tsLastChangedBy' - The ID of the user who last changed the transition state.
transitionState ::
  TransitionState
transitionState =
  TransitionState'
    { _tsEnabled = Nothing,
      _tsDisabledReason = Nothing,
      _tsLastChangedAt = Nothing,
      _tsLastChangedBy = Nothing
    }

-- | Whether the transition between stages is enabled (true) or disabled (false).
tsEnabled :: Lens' TransitionState (Maybe Bool)
tsEnabled = lens _tsEnabled (\s a -> s {_tsEnabled = a})

-- | The user-specified reason why the transition between two stages of a pipeline was disabled.
tsDisabledReason :: Lens' TransitionState (Maybe Text)
tsDisabledReason = lens _tsDisabledReason (\s a -> s {_tsDisabledReason = a})

-- | The timestamp when the transition state was last changed.
tsLastChangedAt :: Lens' TransitionState (Maybe UTCTime)
tsLastChangedAt = lens _tsLastChangedAt (\s a -> s {_tsLastChangedAt = a}) . mapping _Time

-- | The ID of the user who last changed the transition state.
tsLastChangedBy :: Lens' TransitionState (Maybe Text)
tsLastChangedBy = lens _tsLastChangedBy (\s a -> s {_tsLastChangedBy = a})

instance FromJSON TransitionState where
  parseJSON =
    withObject
      "TransitionState"
      ( \x ->
          TransitionState'
            <$> (x .:? "enabled")
            <*> (x .:? "disabledReason")
            <*> (x .:? "lastChangedAt")
            <*> (x .:? "lastChangedBy")
      )

instance Hashable TransitionState

instance NFData TransitionState
