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
-- Module      : Network.AWS.GuardDuty.StartMonitoringMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Turns on GuardDuty monitoring of the specified member accounts. Use this operation to restart monitoring of accounts that you stopped monitoring with the @StopMonitoringMembers@ operation.
module Network.AWS.GuardDuty.StartMonitoringMembers
  ( -- * Creating a Request
    startMonitoringMembers,
    StartMonitoringMembers,

    -- * Request Lenses
    sDetectorId,
    sAccountIds,

    -- * Destructuring the Response
    startMonitoringMembersResponse,
    StartMonitoringMembersResponse,

    -- * Response Lenses
    srsResponseStatus,
    srsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startMonitoringMembers' smart constructor.
data StartMonitoringMembers = StartMonitoringMembers'
  { _sDetectorId ::
      !Text,
    _sAccountIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMonitoringMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDetectorId' - The unique ID of the detector of the GuardDuty master account associated with the member accounts to monitor.
--
-- * 'sAccountIds' - A list of account IDs of the GuardDuty member accounts to start monitoring.
startMonitoringMembers ::
  -- | 'sDetectorId'
  Text ->
  -- | 'sAccountIds'
  NonEmpty Text ->
  StartMonitoringMembers
startMonitoringMembers pDetectorId_ pAccountIds_ =
  StartMonitoringMembers'
    { _sDetectorId = pDetectorId_,
      _sAccountIds = _List1 # pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty master account associated with the member accounts to monitor.
sDetectorId :: Lens' StartMonitoringMembers Text
sDetectorId = lens _sDetectorId (\s a -> s {_sDetectorId = a})

-- | A list of account IDs of the GuardDuty member accounts to start monitoring.
sAccountIds :: Lens' StartMonitoringMembers (NonEmpty Text)
sAccountIds = lens _sAccountIds (\s a -> s {_sAccountIds = a}) . _List1

instance AWSRequest StartMonitoringMembers where
  type Rs StartMonitoringMembers = StartMonitoringMembersResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          StartMonitoringMembersResponse'
            <$> (pure (fromEnum s)) <*> (x .?> "unprocessedAccounts" .!@ mempty)
      )

instance Hashable StartMonitoringMembers

instance NFData StartMonitoringMembers

instance ToHeaders StartMonitoringMembers where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON StartMonitoringMembers where
  toJSON StartMonitoringMembers' {..} =
    object (catMaybes [Just ("accountIds" .= _sAccountIds)])

instance ToPath StartMonitoringMembers where
  toPath StartMonitoringMembers' {..} =
    mconcat ["/detector/", toBS _sDetectorId, "/member/start"]

instance ToQuery StartMonitoringMembers where
  toQuery = const mempty

-- | /See:/ 'startMonitoringMembersResponse' smart constructor.
data StartMonitoringMembersResponse = StartMonitoringMembersResponse'
  { _srsResponseStatus ::
      !Int,
    _srsUnprocessedAccounts ::
      ![UnprocessedAccount]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartMonitoringMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
--
-- * 'srsUnprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
startMonitoringMembersResponse ::
  -- | 'srsResponseStatus'
  Int ->
  StartMonitoringMembersResponse
startMonitoringMembersResponse pResponseStatus_ =
  StartMonitoringMembersResponse'
    { _srsResponseStatus =
        pResponseStatus_,
      _srsUnprocessedAccounts = mempty
    }

-- | -- | The response status code.
srsResponseStatus :: Lens' StartMonitoringMembersResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
srsUnprocessedAccounts :: Lens' StartMonitoringMembersResponse [UnprocessedAccount]
srsUnprocessedAccounts = lens _srsUnprocessedAccounts (\s a -> s {_srsUnprocessedAccounts = a}) . _Coerce

instance NFData StartMonitoringMembersResponse
