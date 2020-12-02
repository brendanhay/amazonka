{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Invitation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Invitation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the invitation to become a member account.
--
--
--
-- /See:/ 'invitation' smart constructor.
data Invitation = Invitation'
  { _iInvitedAt :: !(Maybe Text),
    _iRelationshipStatus :: !(Maybe Text),
    _iInvitationId :: !(Maybe Text),
    _iAccountId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Invitation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iInvitedAt' - The timestamp when the invitation was sent.
--
-- * 'iRelationshipStatus' - The status of the relationship between the inviter and invitee accounts.
--
-- * 'iInvitationId' - The ID of the invitation. This value is used to validate the inviter account to the member account.
--
-- * 'iAccountId' - The ID of the account that the invitation was sent from.
invitation ::
  Invitation
invitation =
  Invitation'
    { _iInvitedAt = Nothing,
      _iRelationshipStatus = Nothing,
      _iInvitationId = Nothing,
      _iAccountId = Nothing
    }

-- | The timestamp when the invitation was sent.
iInvitedAt :: Lens' Invitation (Maybe Text)
iInvitedAt = lens _iInvitedAt (\s a -> s {_iInvitedAt = a})

-- | The status of the relationship between the inviter and invitee accounts.
iRelationshipStatus :: Lens' Invitation (Maybe Text)
iRelationshipStatus = lens _iRelationshipStatus (\s a -> s {_iRelationshipStatus = a})

-- | The ID of the invitation. This value is used to validate the inviter account to the member account.
iInvitationId :: Lens' Invitation (Maybe Text)
iInvitationId = lens _iInvitationId (\s a -> s {_iInvitationId = a})

-- | The ID of the account that the invitation was sent from.
iAccountId :: Lens' Invitation (Maybe Text)
iAccountId = lens _iAccountId (\s a -> s {_iAccountId = a})

instance FromJSON Invitation where
  parseJSON =
    withObject
      "Invitation"
      ( \x ->
          Invitation'
            <$> (x .:? "invitedAt")
            <*> (x .:? "relationshipStatus")
            <*> (x .:? "invitationId")
            <*> (x .:? "accountId")
      )

instance Hashable Invitation

instance NFData Invitation
