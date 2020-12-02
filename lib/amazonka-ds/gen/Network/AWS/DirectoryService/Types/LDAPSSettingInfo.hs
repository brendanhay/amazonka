{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.LDAPSSettingInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.LDAPSSettingInfo where

import Network.AWS.DirectoryService.Types.LDAPSStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains general information about the LDAPS settings.
--
--
--
-- /See:/ 'lDAPSSettingInfo' smart constructor.
data LDAPSSettingInfo = LDAPSSettingInfo'
  { _ldapssiLastUpdatedDateTime ::
      !(Maybe POSIX),
    _ldapssiLDAPSStatusReason :: !(Maybe Text),
    _ldapssiLDAPSStatus :: !(Maybe LDAPSStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LDAPSSettingInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldapssiLastUpdatedDateTime' - The date and time when the LDAPS settings were last updated.
--
-- * 'ldapssiLDAPSStatusReason' - Describes a state change for LDAPS.
--
-- * 'ldapssiLDAPSStatus' - The state of the LDAPS settings.
lDAPSSettingInfo ::
  LDAPSSettingInfo
lDAPSSettingInfo =
  LDAPSSettingInfo'
    { _ldapssiLastUpdatedDateTime = Nothing,
      _ldapssiLDAPSStatusReason = Nothing,
      _ldapssiLDAPSStatus = Nothing
    }

-- | The date and time when the LDAPS settings were last updated.
ldapssiLastUpdatedDateTime :: Lens' LDAPSSettingInfo (Maybe UTCTime)
ldapssiLastUpdatedDateTime = lens _ldapssiLastUpdatedDateTime (\s a -> s {_ldapssiLastUpdatedDateTime = a}) . mapping _Time

-- | Describes a state change for LDAPS.
ldapssiLDAPSStatusReason :: Lens' LDAPSSettingInfo (Maybe Text)
ldapssiLDAPSStatusReason = lens _ldapssiLDAPSStatusReason (\s a -> s {_ldapssiLDAPSStatusReason = a})

-- | The state of the LDAPS settings.
ldapssiLDAPSStatus :: Lens' LDAPSSettingInfo (Maybe LDAPSStatus)
ldapssiLDAPSStatus = lens _ldapssiLDAPSStatus (\s a -> s {_ldapssiLDAPSStatus = a})

instance FromJSON LDAPSSettingInfo where
  parseJSON =
    withObject
      "LDAPSSettingInfo"
      ( \x ->
          LDAPSSettingInfo'
            <$> (x .:? "LastUpdatedDateTime")
            <*> (x .:? "LDAPSStatusReason")
            <*> (x .:? "LDAPSStatus")
      )

instance Hashable LDAPSSettingInfo

instance NFData LDAPSSettingInfo
