{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.InviteMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Invites other AWS accounts (created as members of the current AWS account by CreateMembers) to enable GuardDuty and allow the current AWS account to view and manage these accounts' GuardDuty findings on their behalf as the master account.
module Network.AWS.GuardDuty.InviteMembers
    (
    -- * Creating a Request
      inviteMembers
    , InviteMembers
    -- * Request Lenses
    , imAccountIds
    , imDisableEmailNotification
    , imMessage
    , imDetectorId

    -- * Destructuring the Response
    , inviteMembersResponse
    , InviteMembersResponse
    -- * Response Lenses
    , imrsUnprocessedAccounts
    , imrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | InviteMembers request body.
--
-- /See:/ 'inviteMembers' smart constructor.
data InviteMembers = InviteMembers'
  { _imAccountIds               :: !(Maybe [Text])
  , _imDisableEmailNotification :: !(Maybe Bool)
  , _imMessage                  :: !(Maybe Text)
  , _imDetectorId               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InviteMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imAccountIds' - A list of account IDs of the accounts that you want to invite to GuardDuty as members.
--
-- * 'imDisableEmailNotification' - A boolean value that specifies whether you want to disable email notification to the accounts that you’re inviting to GuardDuty as members.
--
-- * 'imMessage' - The invitation message that you want to send to the accounts that you’re inviting to GuardDuty as members.
--
-- * 'imDetectorId' - The unique ID of the detector of the GuardDuty account with which you want to invite members.
inviteMembers
    :: Text -- ^ 'imDetectorId'
    -> InviteMembers
inviteMembers pDetectorId_ =
  InviteMembers'
    { _imAccountIds = Nothing
    , _imDisableEmailNotification = Nothing
    , _imMessage = Nothing
    , _imDetectorId = pDetectorId_
    }


-- | A list of account IDs of the accounts that you want to invite to GuardDuty as members.
imAccountIds :: Lens' InviteMembers [Text]
imAccountIds = lens _imAccountIds (\ s a -> s{_imAccountIds = a}) . _Default . _Coerce

-- | A boolean value that specifies whether you want to disable email notification to the accounts that you’re inviting to GuardDuty as members.
imDisableEmailNotification :: Lens' InviteMembers (Maybe Bool)
imDisableEmailNotification = lens _imDisableEmailNotification (\ s a -> s{_imDisableEmailNotification = a})

-- | The invitation message that you want to send to the accounts that you’re inviting to GuardDuty as members.
imMessage :: Lens' InviteMembers (Maybe Text)
imMessage = lens _imMessage (\ s a -> s{_imMessage = a})

-- | The unique ID of the detector of the GuardDuty account with which you want to invite members.
imDetectorId :: Lens' InviteMembers Text
imDetectorId = lens _imDetectorId (\ s a -> s{_imDetectorId = a})

instance AWSRequest InviteMembers where
        type Rs InviteMembers = InviteMembersResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 InviteMembersResponse' <$>
                   (x .?> "unprocessedAccounts" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable InviteMembers where

instance NFData InviteMembers where

instance ToHeaders InviteMembers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON InviteMembers where
        toJSON InviteMembers'{..}
          = object
              (catMaybes
                 [("accountIds" .=) <$> _imAccountIds,
                  ("disableEmailNotification" .=) <$>
                    _imDisableEmailNotification,
                  ("message" .=) <$> _imMessage])

instance ToPath InviteMembers where
        toPath InviteMembers'{..}
          = mconcat
              ["/detector/", toBS _imDetectorId, "/member/invite"]

instance ToQuery InviteMembers where
        toQuery = const mempty

-- | /See:/ 'inviteMembersResponse' smart constructor.
data InviteMembersResponse = InviteMembersResponse'
  { _imrsUnprocessedAccounts :: !(Maybe [UnprocessedAccount])
  , _imrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InviteMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imrsUnprocessedAccounts' - A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- * 'imrsResponseStatus' - -- | The response status code.
inviteMembersResponse
    :: Int -- ^ 'imrsResponseStatus'
    -> InviteMembersResponse
inviteMembersResponse pResponseStatus_ =
  InviteMembersResponse'
    {_imrsUnprocessedAccounts = Nothing, _imrsResponseStatus = pResponseStatus_}


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
imrsUnprocessedAccounts :: Lens' InviteMembersResponse [UnprocessedAccount]
imrsUnprocessedAccounts = lens _imrsUnprocessedAccounts (\ s a -> s{_imrsUnprocessedAccounts = a}) . _Default . _Coerce

-- | -- | The response status code.
imrsResponseStatus :: Lens' InviteMembersResponse Int
imrsResponseStatus = lens _imrsResponseStatus (\ s a -> s{_imrsResponseStatus = a})

instance NFData InviteMembersResponse where
