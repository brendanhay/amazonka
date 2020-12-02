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
-- Module      : Network.AWS.GuardDuty.GetMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.GetMembers
  ( -- * Creating a Request
    getMembers,
    GetMembers,

    -- * Request Lenses
    gmDetectorId,
    gmAccountIds,

    -- * Destructuring the Response
    getMembersResponse,
    GetMembersResponse,

    -- * Response Lenses
    gmrsResponseStatus,
    gmrsMembers,
    gmrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMembers' smart constructor.
data GetMembers = GetMembers'
  { _gmDetectorId :: !Text,
    _gmAccountIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmDetectorId' - The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
--
-- * 'gmAccountIds' - A list of account IDs of the GuardDuty member accounts that you want to describe.
getMembers ::
  -- | 'gmDetectorId'
  Text ->
  -- | 'gmAccountIds'
  NonEmpty Text ->
  GetMembers
getMembers pDetectorId_ pAccountIds_ =
  GetMembers'
    { _gmDetectorId = pDetectorId_,
      _gmAccountIds = _List1 # pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
gmDetectorId :: Lens' GetMembers Text
gmDetectorId = lens _gmDetectorId (\s a -> s {_gmDetectorId = a})

-- | A list of account IDs of the GuardDuty member accounts that you want to describe.
gmAccountIds :: Lens' GetMembers (NonEmpty Text)
gmAccountIds = lens _gmAccountIds (\s a -> s {_gmAccountIds = a}) . _List1

instance AWSRequest GetMembers where
  type Rs GetMembers = GetMembersResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          GetMembersResponse'
            <$> (pure (fromEnum s))
            <*> (x .?> "members" .!@ mempty)
            <*> (x .?> "unprocessedAccounts" .!@ mempty)
      )

instance Hashable GetMembers

instance NFData GetMembers

instance ToHeaders GetMembers where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON GetMembers where
  toJSON GetMembers' {..} =
    object (catMaybes [Just ("accountIds" .= _gmAccountIds)])

instance ToPath GetMembers where
  toPath GetMembers' {..} =
    mconcat ["/detector/", toBS _gmDetectorId, "/member/get"]

instance ToQuery GetMembers where
  toQuery = const mempty

-- | /See:/ 'getMembersResponse' smart constructor.
data GetMembersResponse = GetMembersResponse'
  { _gmrsResponseStatus ::
      !Int,
    _gmrsMembers :: ![Member],
    _gmrsUnprocessedAccounts :: ![UnprocessedAccount]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmrsResponseStatus' - -- | The response status code.
--
-- * 'gmrsMembers' - A list of members.
--
-- * 'gmrsUnprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
getMembersResponse ::
  -- | 'gmrsResponseStatus'
  Int ->
  GetMembersResponse
getMembersResponse pResponseStatus_ =
  GetMembersResponse'
    { _gmrsResponseStatus = pResponseStatus_,
      _gmrsMembers = mempty,
      _gmrsUnprocessedAccounts = mempty
    }

-- | -- | The response status code.
gmrsResponseStatus :: Lens' GetMembersResponse Int
gmrsResponseStatus = lens _gmrsResponseStatus (\s a -> s {_gmrsResponseStatus = a})

-- | A list of members.
gmrsMembers :: Lens' GetMembersResponse [Member]
gmrsMembers = lens _gmrsMembers (\s a -> s {_gmrsMembers = a}) . _Coerce

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
gmrsUnprocessedAccounts :: Lens' GetMembersResponse [UnprocessedAccount]
gmrsUnprocessedAccounts = lens _gmrsUnprocessedAccounts (\s a -> s {_gmrsUnprocessedAccounts = a}) . _Coerce

instance NFData GetMembersResponse
