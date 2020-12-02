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
-- Module      : Network.AWS.GuardDuty.DisassociateMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.DisassociateMembers
  ( -- * Creating a Request
    disassociateMembers,
    DisassociateMembers,

    -- * Request Lenses
    dmsDetectorId,
    dmsAccountIds,

    -- * Destructuring the Response
    disassociateMembersResponse,
    DisassociateMembersResponse,

    -- * Response Lenses
    dmrsResponseStatus,
    dmrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateMembers' smart constructor.
data DisassociateMembers = DisassociateMembers'
  { _dmsDetectorId ::
      !Text,
    _dmsAccountIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmsDetectorId' - The unique ID of the detector of the GuardDuty account whose members you want to disassociate from the master account.
--
-- * 'dmsAccountIds' - A list of account IDs of the GuardDuty member accounts that you want to disassociate from the master account.
disassociateMembers ::
  -- | 'dmsDetectorId'
  Text ->
  -- | 'dmsAccountIds'
  NonEmpty Text ->
  DisassociateMembers
disassociateMembers pDetectorId_ pAccountIds_ =
  DisassociateMembers'
    { _dmsDetectorId = pDetectorId_,
      _dmsAccountIds = _List1 # pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you want to disassociate from the master account.
dmsDetectorId :: Lens' DisassociateMembers Text
dmsDetectorId = lens _dmsDetectorId (\s a -> s {_dmsDetectorId = a})

-- | A list of account IDs of the GuardDuty member accounts that you want to disassociate from the master account.
dmsAccountIds :: Lens' DisassociateMembers (NonEmpty Text)
dmsAccountIds = lens _dmsAccountIds (\s a -> s {_dmsAccountIds = a}) . _List1

instance AWSRequest DisassociateMembers where
  type Rs DisassociateMembers = DisassociateMembersResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          DisassociateMembersResponse'
            <$> (pure (fromEnum s)) <*> (x .?> "unprocessedAccounts" .!@ mempty)
      )

instance Hashable DisassociateMembers

instance NFData DisassociateMembers

instance ToHeaders DisassociateMembers where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON DisassociateMembers where
  toJSON DisassociateMembers' {..} =
    object (catMaybes [Just ("accountIds" .= _dmsAccountIds)])

instance ToPath DisassociateMembers where
  toPath DisassociateMembers' {..} =
    mconcat
      ["/detector/", toBS _dmsDetectorId, "/member/disassociate"]

instance ToQuery DisassociateMembers where
  toQuery = const mempty

-- | /See:/ 'disassociateMembersResponse' smart constructor.
data DisassociateMembersResponse = DisassociateMembersResponse'
  { _dmrsResponseStatus ::
      !Int,
    _dmrsUnprocessedAccounts ::
      ![UnprocessedAccount]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsResponseStatus' - -- | The response status code.
--
-- * 'dmrsUnprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
disassociateMembersResponse ::
  -- | 'dmrsResponseStatus'
  Int ->
  DisassociateMembersResponse
disassociateMembersResponse pResponseStatus_ =
  DisassociateMembersResponse'
    { _dmrsResponseStatus =
        pResponseStatus_,
      _dmrsUnprocessedAccounts = mempty
    }

-- | -- | The response status code.
dmrsResponseStatus :: Lens' DisassociateMembersResponse Int
dmrsResponseStatus = lens _dmrsResponseStatus (\s a -> s {_dmrsResponseStatus = a})

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
dmrsUnprocessedAccounts :: Lens' DisassociateMembersResponse [UnprocessedAccount]
dmrsUnprocessedAccounts = lens _dmrsUnprocessedAccounts (\s a -> s {_dmrsUnprocessedAccounts = a}) . _Coerce

instance NFData DisassociateMembersResponse
