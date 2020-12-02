{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Group where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkMail.Types.EntityState

-- | The representation of an Amazon WorkMail group.
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
  { _gEmail :: !(Maybe Text),
    _gState :: !(Maybe EntityState),
    _gDisabledDate :: !(Maybe POSIX),
    _gName :: !(Maybe Text),
    _gId :: !(Maybe Text),
    _gEnabledDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gEmail' - The email of the group.
--
-- * 'gState' - The state of the group, which can be ENABLED, DISABLED, or DELETED.
--
-- * 'gDisabledDate' - The date indicating when the group was disabled from Amazon WorkMail use.
--
-- * 'gName' - The name of the group.
--
-- * 'gId' - The identifier of the group.
--
-- * 'gEnabledDate' - The date indicating when the group was enabled for Amazon WorkMail use.
group' ::
  Group
group' =
  Group'
    { _gEmail = Nothing,
      _gState = Nothing,
      _gDisabledDate = Nothing,
      _gName = Nothing,
      _gId = Nothing,
      _gEnabledDate = Nothing
    }

-- | The email of the group.
gEmail :: Lens' Group (Maybe Text)
gEmail = lens _gEmail (\s a -> s {_gEmail = a})

-- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
gState :: Lens' Group (Maybe EntityState)
gState = lens _gState (\s a -> s {_gState = a})

-- | The date indicating when the group was disabled from Amazon WorkMail use.
gDisabledDate :: Lens' Group (Maybe UTCTime)
gDisabledDate = lens _gDisabledDate (\s a -> s {_gDisabledDate = a}) . mapping _Time

-- | The name of the group.
gName :: Lens' Group (Maybe Text)
gName = lens _gName (\s a -> s {_gName = a})

-- | The identifier of the group.
gId :: Lens' Group (Maybe Text)
gId = lens _gId (\s a -> s {_gId = a})

-- | The date indicating when the group was enabled for Amazon WorkMail use.
gEnabledDate :: Lens' Group (Maybe UTCTime)
gEnabledDate = lens _gEnabledDate (\s a -> s {_gEnabledDate = a}) . mapping _Time

instance FromJSON Group where
  parseJSON =
    withObject
      "Group"
      ( \x ->
          Group'
            <$> (x .:? "Email")
            <*> (x .:? "State")
            <*> (x .:? "DisabledDate")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "EnabledDate")
      )

instance Hashable Group

instance NFData Group
