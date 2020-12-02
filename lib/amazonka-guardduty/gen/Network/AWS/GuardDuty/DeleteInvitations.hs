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
-- Module      : Network.AWS.GuardDuty.DeleteInvitations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes invitations sent to the current member account by AWS accounts specified by their account IDs.
module Network.AWS.GuardDuty.DeleteInvitations
  ( -- * Creating a Request
    deleteInvitations,
    DeleteInvitations,

    -- * Request Lenses
    diAccountIds,

    -- * Destructuring the Response
    deleteInvitationsResponse,
    DeleteInvitationsResponse,

    -- * Response Lenses
    dirsResponseStatus,
    dirsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInvitations' smart constructor.
newtype DeleteInvitations = DeleteInvitations'
  { _diAccountIds ::
      List1 Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteInvitations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diAccountIds' - A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to delete invitations from.
deleteInvitations ::
  -- | 'diAccountIds'
  NonEmpty Text ->
  DeleteInvitations
deleteInvitations pAccountIds_ =
  DeleteInvitations' {_diAccountIds = _List1 # pAccountIds_}

-- | A list of account IDs of the AWS accounts that sent invitations to the current member account that you want to delete invitations from.
diAccountIds :: Lens' DeleteInvitations (NonEmpty Text)
diAccountIds = lens _diAccountIds (\s a -> s {_diAccountIds = a}) . _List1

instance AWSRequest DeleteInvitations where
  type Rs DeleteInvitations = DeleteInvitationsResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          DeleteInvitationsResponse'
            <$> (pure (fromEnum s)) <*> (x .?> "unprocessedAccounts" .!@ mempty)
      )

instance Hashable DeleteInvitations

instance NFData DeleteInvitations

instance ToHeaders DeleteInvitations where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON DeleteInvitations where
  toJSON DeleteInvitations' {..} =
    object (catMaybes [Just ("accountIds" .= _diAccountIds)])

instance ToPath DeleteInvitations where
  toPath = const "/invitation/delete"

instance ToQuery DeleteInvitations where
  toQuery = const mempty

-- | /See:/ 'deleteInvitationsResponse' smart constructor.
data DeleteInvitationsResponse = DeleteInvitationsResponse'
  { _dirsResponseStatus ::
      !Int,
    _dirsUnprocessedAccounts ::
      ![UnprocessedAccount]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteInvitationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsResponseStatus' - -- | The response status code.
--
-- * 'dirsUnprocessedAccounts' - A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
deleteInvitationsResponse ::
  -- | 'dirsResponseStatus'
  Int ->
  DeleteInvitationsResponse
deleteInvitationsResponse pResponseStatus_ =
  DeleteInvitationsResponse'
    { _dirsResponseStatus =
        pResponseStatus_,
      _dirsUnprocessedAccounts = mempty
    }

-- | -- | The response status code.
dirsResponseStatus :: Lens' DeleteInvitationsResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\s a -> s {_dirsResponseStatus = a})

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
dirsUnprocessedAccounts :: Lens' DeleteInvitationsResponse [UnprocessedAccount]
dirsUnprocessedAccounts = lens _dirsUnprocessedAccounts (\s a -> s {_dirsUnprocessedAccounts = a}) . _Coerce

instance NFData DeleteInvitationsResponse
