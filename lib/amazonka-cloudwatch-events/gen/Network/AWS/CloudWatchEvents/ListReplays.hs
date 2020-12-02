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
-- Module      : Network.AWS.CloudWatchEvents.ListReplays
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your replays. You can either list all the replays or you can provide a prefix to match to the replay names. Filter parameters are exclusive.
module Network.AWS.CloudWatchEvents.ListReplays
  ( -- * Creating a Request
    listReplays,
    ListReplays,

    -- * Request Lenses
    lEventSourceARN,
    lState,
    lNextToken,
    lNamePrefix,
    lLimit,

    -- * Destructuring the Response
    listReplaysResponse,
    ListReplaysResponse,

    -- * Response Lenses
    lrsReplays,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listReplays' smart constructor.
data ListReplays = ListReplays'
  { _lEventSourceARN :: !(Maybe Text),
    _lState :: !(Maybe ReplayState),
    _lNextToken :: !(Maybe Text),
    _lNamePrefix :: !(Maybe Text),
    _lLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListReplays' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lEventSourceARN' - The ARN of the event source associated with the replay.
--
-- * 'lState' - The state of the replay.
--
-- * 'lNextToken' - The token returned by a previous call to retrieve the next set of results.
--
-- * 'lNamePrefix' - A name prefix to filter the replays returned. Only replays with name that match the prefix are returned.
--
-- * 'lLimit' - The maximum number of replays to retrieve.
listReplays ::
  ListReplays
listReplays =
  ListReplays'
    { _lEventSourceARN = Nothing,
      _lState = Nothing,
      _lNextToken = Nothing,
      _lNamePrefix = Nothing,
      _lLimit = Nothing
    }

-- | The ARN of the event source associated with the replay.
lEventSourceARN :: Lens' ListReplays (Maybe Text)
lEventSourceARN = lens _lEventSourceARN (\s a -> s {_lEventSourceARN = a})

-- | The state of the replay.
lState :: Lens' ListReplays (Maybe ReplayState)
lState = lens _lState (\s a -> s {_lState = a})

-- | The token returned by a previous call to retrieve the next set of results.
lNextToken :: Lens' ListReplays (Maybe Text)
lNextToken = lens _lNextToken (\s a -> s {_lNextToken = a})

-- | A name prefix to filter the replays returned. Only replays with name that match the prefix are returned.
lNamePrefix :: Lens' ListReplays (Maybe Text)
lNamePrefix = lens _lNamePrefix (\s a -> s {_lNamePrefix = a})

-- | The maximum number of replays to retrieve.
lLimit :: Lens' ListReplays (Maybe Natural)
lLimit = lens _lLimit (\s a -> s {_lLimit = a}) . mapping _Nat

instance AWSRequest ListReplays where
  type Rs ListReplays = ListReplaysResponse
  request = postJSON cloudWatchEvents
  response =
    receiveJSON
      ( \s h x ->
          ListReplaysResponse'
            <$> (x .?> "Replays" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListReplays

instance NFData ListReplays

instance ToHeaders ListReplays where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSEvents.ListReplays" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListReplays where
  toJSON ListReplays' {..} =
    object
      ( catMaybes
          [ ("EventSourceArn" .=) <$> _lEventSourceARN,
            ("State" .=) <$> _lState,
            ("NextToken" .=) <$> _lNextToken,
            ("NamePrefix" .=) <$> _lNamePrefix,
            ("Limit" .=) <$> _lLimit
          ]
      )

instance ToPath ListReplays where
  toPath = const "/"

instance ToQuery ListReplays where
  toQuery = const mempty

-- | /See:/ 'listReplaysResponse' smart constructor.
data ListReplaysResponse = ListReplaysResponse'
  { _lrsReplays ::
      !(Maybe [Replay]),
    _lrsNextToken :: !(Maybe Text),
    _lrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListReplaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsReplays' - An array of @Replay@ objects that contain information about the replay.
--
-- * 'lrsNextToken' - The token returned by a previous call to retrieve the next set of results.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listReplaysResponse ::
  -- | 'lrsResponseStatus'
  Int ->
  ListReplaysResponse
listReplaysResponse pResponseStatus_ =
  ListReplaysResponse'
    { _lrsReplays = Nothing,
      _lrsNextToken = Nothing,
      _lrsResponseStatus = pResponseStatus_
    }

-- | An array of @Replay@ objects that contain information about the replay.
lrsReplays :: Lens' ListReplaysResponse [Replay]
lrsReplays = lens _lrsReplays (\s a -> s {_lrsReplays = a}) . _Default . _Coerce

-- | The token returned by a previous call to retrieve the next set of results.
lrsNextToken :: Lens' ListReplaysResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\s a -> s {_lrsNextToken = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListReplaysResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\s a -> s {_lrsResponseStatus = a})

instance NFData ListReplaysResponse
