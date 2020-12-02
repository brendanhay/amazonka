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
-- Module      : Network.AWS.GuardDuty.DeleteMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.DeleteMembers
  ( -- * Creating a Request
    deleteMembers,
    DeleteMembers,

    -- * Request Lenses
    dmDetectorId,
    dmAccountIds,

    -- * Destructuring the Response
    deleteMembersResponse,
    DeleteMembersResponse,

    -- * Response Lenses
    drsResponseStatus,
    drsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteMembers' smart constructor.
data DeleteMembers = DeleteMembers'
  { _dmDetectorId :: !Text,
    _dmAccountIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmDetectorId' - The unique ID of the detector of the GuardDuty account whose members you want to delete.
--
-- * 'dmAccountIds' - A list of account IDs of the GuardDuty member accounts that you want to delete.
deleteMembers ::
  -- | 'dmDetectorId'
  Text ->
  -- | 'dmAccountIds'
  NonEmpty Text ->
  DeleteMembers
deleteMembers pDetectorId_ pAccountIds_ =
  DeleteMembers'
    { _dmDetectorId = pDetectorId_,
      _dmAccountIds = _List1 # pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you want to delete.
dmDetectorId :: Lens' DeleteMembers Text
dmDetectorId = lens _dmDetectorId (\s a -> s {_dmDetectorId = a})

-- | A list of account IDs of the GuardDuty member accounts that you want to delete.
dmAccountIds :: Lens' DeleteMembers (NonEmpty Text)
dmAccountIds = lens _dmAccountIds (\s a -> s {_dmAccountIds = a}) . _List1

instance AWSRequest DeleteMembers where
  type Rs DeleteMembers = DeleteMembersResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          DeleteMembersResponse'
            <$> (pure (fromEnum s)) <*> (x .?> "unprocessedAccounts" .!@ mempty)
      )

instance Hashable DeleteMembers

instance NFData DeleteMembers

instance ToHeaders DeleteMembers where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON DeleteMembers where
  toJSON DeleteMembers' {..} =
    object (catMaybes [Just ("accountIds" .= _dmAccountIds)])

instance ToPath DeleteMembers where
  toPath DeleteMembers' {..} =
    mconcat ["/detector/", toBS _dmDetectorId, "/member/delete"]

instance ToQuery DeleteMembers where
  toQuery = const mempty

-- | /See:/ 'deleteMembersResponse' smart constructor.
data DeleteMembersResponse = DeleteMembersResponse'
  { _drsResponseStatus ::
      !Int,
    _drsUnprocessedAccounts ::
      ![UnprocessedAccount]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
--
-- * 'drsUnprocessedAccounts' - The accounts that could not be processed.
deleteMembersResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DeleteMembersResponse
deleteMembersResponse pResponseStatus_ =
  DeleteMembersResponse'
    { _drsResponseStatus = pResponseStatus_,
      _drsUnprocessedAccounts = mempty
    }

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteMembersResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

-- | The accounts that could not be processed.
drsUnprocessedAccounts :: Lens' DeleteMembersResponse [UnprocessedAccount]
drsUnprocessedAccounts = lens _drsUnprocessedAccounts (\s a -> s {_drsUnprocessedAccounts = a}) . _Coerce

instance NFData DeleteMembersResponse
