{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.UserPhoneConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserPhoneConfig where

import Network.AWS.Connect.Types.PhoneType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the phone configuration settings for a user.
--
--
--
-- /See:/ 'userPhoneConfig' smart constructor.
data UserPhoneConfig = UserPhoneConfig'
  { _upcAutoAccept ::
      !(Maybe Bool),
    _upcAfterContactWorkTimeLimit :: !(Maybe Nat),
    _upcDeskPhoneNumber :: !(Maybe Text),
    _upcPhoneType :: !PhoneType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserPhoneConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcAutoAccept' - The Auto accept setting.
--
-- * 'upcAfterContactWorkTimeLimit' - The After Call Work (ACW) timeout setting, in seconds.
--
-- * 'upcDeskPhoneNumber' - The phone number for the user's desk phone.
--
-- * 'upcPhoneType' - The phone type.
userPhoneConfig ::
  -- | 'upcPhoneType'
  PhoneType ->
  UserPhoneConfig
userPhoneConfig pPhoneType_ =
  UserPhoneConfig'
    { _upcAutoAccept = Nothing,
      _upcAfterContactWorkTimeLimit = Nothing,
      _upcDeskPhoneNumber = Nothing,
      _upcPhoneType = pPhoneType_
    }

-- | The Auto accept setting.
upcAutoAccept :: Lens' UserPhoneConfig (Maybe Bool)
upcAutoAccept = lens _upcAutoAccept (\s a -> s {_upcAutoAccept = a})

-- | The After Call Work (ACW) timeout setting, in seconds.
upcAfterContactWorkTimeLimit :: Lens' UserPhoneConfig (Maybe Natural)
upcAfterContactWorkTimeLimit = lens _upcAfterContactWorkTimeLimit (\s a -> s {_upcAfterContactWorkTimeLimit = a}) . mapping _Nat

-- | The phone number for the user's desk phone.
upcDeskPhoneNumber :: Lens' UserPhoneConfig (Maybe Text)
upcDeskPhoneNumber = lens _upcDeskPhoneNumber (\s a -> s {_upcDeskPhoneNumber = a})

-- | The phone type.
upcPhoneType :: Lens' UserPhoneConfig PhoneType
upcPhoneType = lens _upcPhoneType (\s a -> s {_upcPhoneType = a})

instance FromJSON UserPhoneConfig where
  parseJSON =
    withObject
      "UserPhoneConfig"
      ( \x ->
          UserPhoneConfig'
            <$> (x .:? "AutoAccept")
            <*> (x .:? "AfterContactWorkTimeLimit")
            <*> (x .:? "DeskPhoneNumber")
            <*> (x .: "PhoneType")
      )

instance Hashable UserPhoneConfig

instance NFData UserPhoneConfig

instance ToJSON UserPhoneConfig where
  toJSON UserPhoneConfig' {..} =
    object
      ( catMaybes
          [ ("AutoAccept" .=) <$> _upcAutoAccept,
            ("AfterContactWorkTimeLimit" .=) <$> _upcAfterContactWorkTimeLimit,
            ("DeskPhoneNumber" .=) <$> _upcDeskPhoneNumber,
            Just ("PhoneType" .= _upcPhoneType)
          ]
      )
