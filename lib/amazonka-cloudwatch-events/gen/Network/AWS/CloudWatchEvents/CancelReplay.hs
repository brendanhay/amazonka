{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.CancelReplay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified replay.
module Network.AWS.CloudWatchEvents.CancelReplay
  ( -- * Creating a Request
    cancelReplay,
    CancelReplay,

    -- * Request Lenses
    crReplayName,

    -- * Destructuring the Response
    cancelReplayResponse,
    CancelReplayResponse,

    -- * Response Lenses
    crrsState,
    crrsReplayARN,
    crrsStateReason,
    crrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelReplay' smart constructor.
newtype CancelReplay = CancelReplay' {_crReplayName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelReplay' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crReplayName' - The name of the replay to cancel.
cancelReplay ::
  -- | 'crReplayName'
  Text ->
  CancelReplay
cancelReplay pReplayName_ =
  CancelReplay' {_crReplayName = pReplayName_}

-- | The name of the replay to cancel.
crReplayName :: Lens' CancelReplay Text
crReplayName = lens _crReplayName (\s a -> s {_crReplayName = a})

instance AWSRequest CancelReplay where
  type Rs CancelReplay = CancelReplayResponse
  request = postJSON cloudWatchEvents
  response =
    receiveJSON
      ( \s h x ->
          CancelReplayResponse'
            <$> (x .?> "State")
            <*> (x .?> "ReplayArn")
            <*> (x .?> "StateReason")
            <*> (pure (fromEnum s))
      )

instance Hashable CancelReplay

instance NFData CancelReplay

instance ToHeaders CancelReplay where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.CancelReplay" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CancelReplay where
  toJSON CancelReplay' {..} =
    object (catMaybes [Just ("ReplayName" .= _crReplayName)])

instance ToPath CancelReplay where
  toPath = const "/"

instance ToQuery CancelReplay where
  toQuery = const mempty

-- | /See:/ 'cancelReplayResponse' smart constructor.
data CancelReplayResponse = CancelReplayResponse'
  { _crrsState ::
      !(Maybe ReplayState),
    _crrsReplayARN :: !(Maybe Text),
    _crrsStateReason :: !(Maybe Text),
    _crrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelReplayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsState' - The current state of the replay.
--
-- * 'crrsReplayARN' - The ARN of the replay to cancel.
--
-- * 'crrsStateReason' - The reason that the replay is in the current state.
--
-- * 'crrsResponseStatus' - -- | The response status code.
cancelReplayResponse ::
  -- | 'crrsResponseStatus'
  Int ->
  CancelReplayResponse
cancelReplayResponse pResponseStatus_ =
  CancelReplayResponse'
    { _crrsState = Nothing,
      _crrsReplayARN = Nothing,
      _crrsStateReason = Nothing,
      _crrsResponseStatus = pResponseStatus_
    }

-- | The current state of the replay.
crrsState :: Lens' CancelReplayResponse (Maybe ReplayState)
crrsState = lens _crrsState (\s a -> s {_crrsState = a})

-- | The ARN of the replay to cancel.
crrsReplayARN :: Lens' CancelReplayResponse (Maybe Text)
crrsReplayARN = lens _crrsReplayARN (\s a -> s {_crrsReplayARN = a})

-- | The reason that the replay is in the current state.
crrsStateReason :: Lens' CancelReplayResponse (Maybe Text)
crrsStateReason = lens _crrsStateReason (\s a -> s {_crrsStateReason = a})

-- | -- | The response status code.
crrsResponseStatus :: Lens' CancelReplayResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\s a -> s {_crrsResponseStatus = a})

instance NFData CancelReplayResponse
