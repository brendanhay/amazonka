{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Member
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Member where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the member account.
--
--
--
-- /See:/ 'member' smart constructor.
data Member = Member'
  { _mInvitedAt :: !(Maybe Text),
    _mDetectorId :: !(Maybe Text),
    _mAccountId :: !Text,
    _mMasterId :: !Text,
    _mEmail :: !Text,
    _mRelationshipStatus :: !Text,
    _mUpdatedAt :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Member' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mInvitedAt' - The timestamp when the invitation was sent.
--
-- * 'mDetectorId' - The detector ID of the member account.
--
-- * 'mAccountId' - The ID of the member account.
--
-- * 'mMasterId' - The master account ID.
--
-- * 'mEmail' - The email address of the member account.
--
-- * 'mRelationshipStatus' - The status of the relationship between the member and the master.
--
-- * 'mUpdatedAt' - The last-updated timestamp of the member.
member ::
  -- | 'mAccountId'
  Text ->
  -- | 'mMasterId'
  Text ->
  -- | 'mEmail'
  Text ->
  -- | 'mRelationshipStatus'
  Text ->
  -- | 'mUpdatedAt'
  Text ->
  Member
member
  pAccountId_
  pMasterId_
  pEmail_
  pRelationshipStatus_
  pUpdatedAt_ =
    Member'
      { _mInvitedAt = Nothing,
        _mDetectorId = Nothing,
        _mAccountId = pAccountId_,
        _mMasterId = pMasterId_,
        _mEmail = pEmail_,
        _mRelationshipStatus = pRelationshipStatus_,
        _mUpdatedAt = pUpdatedAt_
      }

-- | The timestamp when the invitation was sent.
mInvitedAt :: Lens' Member (Maybe Text)
mInvitedAt = lens _mInvitedAt (\s a -> s {_mInvitedAt = a})

-- | The detector ID of the member account.
mDetectorId :: Lens' Member (Maybe Text)
mDetectorId = lens _mDetectorId (\s a -> s {_mDetectorId = a})

-- | The ID of the member account.
mAccountId :: Lens' Member Text
mAccountId = lens _mAccountId (\s a -> s {_mAccountId = a})

-- | The master account ID.
mMasterId :: Lens' Member Text
mMasterId = lens _mMasterId (\s a -> s {_mMasterId = a})

-- | The email address of the member account.
mEmail :: Lens' Member Text
mEmail = lens _mEmail (\s a -> s {_mEmail = a})

-- | The status of the relationship between the member and the master.
mRelationshipStatus :: Lens' Member Text
mRelationshipStatus = lens _mRelationshipStatus (\s a -> s {_mRelationshipStatus = a})

-- | The last-updated timestamp of the member.
mUpdatedAt :: Lens' Member Text
mUpdatedAt = lens _mUpdatedAt (\s a -> s {_mUpdatedAt = a})

instance FromJSON Member where
  parseJSON =
    withObject
      "Member"
      ( \x ->
          Member'
            <$> (x .:? "invitedAt")
            <*> (x .:? "detectorId")
            <*> (x .: "accountId")
            <*> (x .: "masterId")
            <*> (x .: "email")
            <*> (x .: "relationshipStatus")
            <*> (x .: "updatedAt")
      )

instance Hashable Member

instance NFData Member
