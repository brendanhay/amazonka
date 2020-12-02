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
-- Module      : Network.AWS.GuardDuty.DeclineInvitations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Declines invitations sent to the current member account by AWS accounts specified by their account IDs.
module Network.AWS.GuardDuty.DeclineInvitations
  ( -- * Creating a Request
    declineInvitations,
    DeclineInvitations,

    -- * Request Lenses
    dAccountIds,

    -- * Destructuring the Response
    declineInvitationsResponse,
    DeclineInvitationsResponse,

    -- * Response Lenses
    disrsResponseStatus,
    disrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'declineInvitations' smart constructor.
newtype DeclineInvitations = DeclineInvitations'
  { _dAccountIds ::
      List1 Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeclineInvitations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAccountIds' - A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to decline invitations from.
declineInvitations ::
  -- | 'dAccountIds'
  NonEmpty Text ->
  DeclineInvitations
declineInvitations pAccountIds_ =
  DeclineInvitations' {_dAccountIds = _List1 # pAccountIds_}

-- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to decline invitations from.
dAccountIds :: Lens' DeclineInvitations (NonEmpty Text)
dAccountIds = lens _dAccountIds (\s a -> s {_dAccountIds = a}) . _List1

instance AWSRequest DeclineInvitations where
  type Rs DeclineInvitations = DeclineInvitationsResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          DeclineInvitationsResponse'
            <$> (pure (fromEnum s)) <*> (x .?> "unprocessedAccounts" .!@ mempty)
      )

instance Hashable DeclineInvitations

instance NFData DeclineInvitations

instance ToHeaders DeclineInvitations where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON DeclineInvitations where
  toJSON DeclineInvitations' {..} =
    object (catMaybes [Just ("accountIds" .= _dAccountIds)])

instance ToPath DeclineInvitations where
  toPath = const "/invitation/decline"

instance ToQuery DeclineInvitations where
  toQuery = const mempty

-- | /See:/ 'declineInvitationsResponse' smart constructor.
data DeclineInvitationsResponse = DeclineInvitationsResponse'
  { _disrsResponseStatus ::
      !Int,
    _disrsUnprocessedAccounts ::
      ![UnprocessedAccount]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeclineInvitationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsResponseStatus' - -- | The response status code.
--
-- * 'disrsUnprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
declineInvitationsResponse ::
  -- | 'disrsResponseStatus'
  Int ->
  DeclineInvitationsResponse
declineInvitationsResponse pResponseStatus_ =
  DeclineInvitationsResponse'
    { _disrsResponseStatus =
        pResponseStatus_,
      _disrsUnprocessedAccounts = mempty
    }

-- | -- | The response status code.
disrsResponseStatus :: Lens' DeclineInvitationsResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\s a -> s {_disrsResponseStatus = a})

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
disrsUnprocessedAccounts :: Lens' DeclineInvitationsResponse [UnprocessedAccount]
disrsUnprocessedAccounts = lens _disrsUnprocessedAccounts (\s a -> s {_disrsUnprocessedAccounts = a}) . _Coerce

instance NFData DeclineInvitationsResponse
