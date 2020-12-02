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
-- Module      : Network.AWS.GuardDuty.StopMonitoringMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops GuardDuty monitoring for the specified member accounts. Use the @StartMonitoringMembers@ operation to restart monitoring for those accounts.
module Network.AWS.GuardDuty.StopMonitoringMembers
  ( -- * Creating a Request
    stopMonitoringMembers,
    StopMonitoringMembers,

    -- * Request Lenses
    smmDetectorId,
    smmAccountIds,

    -- * Destructuring the Response
    stopMonitoringMembersResponse,
    StopMonitoringMembersResponse,

    -- * Response Lenses
    smmrsResponseStatus,
    smmrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopMonitoringMembers' smart constructor.
data StopMonitoringMembers = StopMonitoringMembers'
  { _smmDetectorId ::
      !Text,
    _smmAccountIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopMonitoringMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smmDetectorId' - The unique ID of the detector associated with the GuardDuty master account that is monitoring member accounts.
--
-- * 'smmAccountIds' - A list of account IDs for the member accounts to stop monitoring.
stopMonitoringMembers ::
  -- | 'smmDetectorId'
  Text ->
  -- | 'smmAccountIds'
  NonEmpty Text ->
  StopMonitoringMembers
stopMonitoringMembers pDetectorId_ pAccountIds_ =
  StopMonitoringMembers'
    { _smmDetectorId = pDetectorId_,
      _smmAccountIds = _List1 # pAccountIds_
    }

-- | The unique ID of the detector associated with the GuardDuty master account that is monitoring member accounts.
smmDetectorId :: Lens' StopMonitoringMembers Text
smmDetectorId = lens _smmDetectorId (\s a -> s {_smmDetectorId = a})

-- | A list of account IDs for the member accounts to stop monitoring.
smmAccountIds :: Lens' StopMonitoringMembers (NonEmpty Text)
smmAccountIds = lens _smmAccountIds (\s a -> s {_smmAccountIds = a}) . _List1

instance AWSRequest StopMonitoringMembers where
  type Rs StopMonitoringMembers = StopMonitoringMembersResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          StopMonitoringMembersResponse'
            <$> (pure (fromEnum s)) <*> (x .?> "unprocessedAccounts" .!@ mempty)
      )

instance Hashable StopMonitoringMembers

instance NFData StopMonitoringMembers

instance ToHeaders StopMonitoringMembers where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON StopMonitoringMembers where
  toJSON StopMonitoringMembers' {..} =
    object (catMaybes [Just ("accountIds" .= _smmAccountIds)])

instance ToPath StopMonitoringMembers where
  toPath StopMonitoringMembers' {..} =
    mconcat ["/detector/", toBS _smmDetectorId, "/member/stop"]

instance ToQuery StopMonitoringMembers where
  toQuery = const mempty

-- | /See:/ 'stopMonitoringMembersResponse' smart constructor.
data StopMonitoringMembersResponse = StopMonitoringMembersResponse'
  { _smmrsResponseStatus ::
      !Int,
    _smmrsUnprocessedAccounts ::
      ![UnprocessedAccount]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopMonitoringMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smmrsResponseStatus' - -- | The response status code.
--
-- * 'smmrsUnprocessedAccounts' - A list of objects that contain an accountId for each account that could not be processed, and a result string that indicates why the account was not processed.
stopMonitoringMembersResponse ::
  -- | 'smmrsResponseStatus'
  Int ->
  StopMonitoringMembersResponse
stopMonitoringMembersResponse pResponseStatus_ =
  StopMonitoringMembersResponse'
    { _smmrsResponseStatus =
        pResponseStatus_,
      _smmrsUnprocessedAccounts = mempty
    }

-- | -- | The response status code.
smmrsResponseStatus :: Lens' StopMonitoringMembersResponse Int
smmrsResponseStatus = lens _smmrsResponseStatus (\s a -> s {_smmrsResponseStatus = a})

-- | A list of objects that contain an accountId for each account that could not be processed, and a result string that indicates why the account was not processed.
smmrsUnprocessedAccounts :: Lens' StopMonitoringMembersResponse [UnprocessedAccount]
smmrsUnprocessedAccounts = lens _smmrsUnprocessedAccounts (\s a -> s {_smmrsUnprocessedAccounts = a}) . _Coerce

instance NFData StopMonitoringMembersResponse
