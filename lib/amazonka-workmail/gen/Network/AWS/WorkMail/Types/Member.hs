{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Member
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Member where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.MemberType

-- | The representation of a user or group.
--
--
--
-- /See:/ 'member' smart constructor.
data Member = Member'
  { _mState :: !(Maybe EntityState),
    _mDisabledDate :: !(Maybe POSIX),
    _mName :: !(Maybe Text),
    _mId :: !(Maybe Text),
    _mType :: !(Maybe MemberType),
    _mEnabledDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Member' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mState' - The state of the member, which can be ENABLED, DISABLED, or DELETED.
--
-- * 'mDisabledDate' - The date indicating when the member was disabled from Amazon WorkMail use.
--
-- * 'mName' - The name of the member.
--
-- * 'mId' - The identifier of the member.
--
-- * 'mType' - A member can be a user or group.
--
-- * 'mEnabledDate' - The date indicating when the member was enabled for Amazon WorkMail use.
member ::
  Member
member =
  Member'
    { _mState = Nothing,
      _mDisabledDate = Nothing,
      _mName = Nothing,
      _mId = Nothing,
      _mType = Nothing,
      _mEnabledDate = Nothing
    }

-- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
mState :: Lens' Member (Maybe EntityState)
mState = lens _mState (\s a -> s {_mState = a})

-- | The date indicating when the member was disabled from Amazon WorkMail use.
mDisabledDate :: Lens' Member (Maybe UTCTime)
mDisabledDate = lens _mDisabledDate (\s a -> s {_mDisabledDate = a}) . mapping _Time

-- | The name of the member.
mName :: Lens' Member (Maybe Text)
mName = lens _mName (\s a -> s {_mName = a})

-- | The identifier of the member.
mId :: Lens' Member (Maybe Text)
mId = lens _mId (\s a -> s {_mId = a})

-- | A member can be a user or group.
mType :: Lens' Member (Maybe MemberType)
mType = lens _mType (\s a -> s {_mType = a})

-- | The date indicating when the member was enabled for Amazon WorkMail use.
mEnabledDate :: Lens' Member (Maybe UTCTime)
mEnabledDate = lens _mEnabledDate (\s a -> s {_mEnabledDate = a}) . mapping _Time

instance FromJSON Member where
  parseJSON =
    withObject
      "Member"
      ( \x ->
          Member'
            <$> (x .:? "State")
            <*> (x .:? "DisabledDate")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "EnabledDate")
      )

instance Hashable Member

instance NFData Member
