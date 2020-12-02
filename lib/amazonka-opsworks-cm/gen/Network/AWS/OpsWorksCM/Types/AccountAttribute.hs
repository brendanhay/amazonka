{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.AccountAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.AccountAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Stores account attributes.
--
--
--
-- /See:/ 'accountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { _aaUsed :: !(Maybe Int),
    _aaMaximum :: !(Maybe Int),
    _aaName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaUsed' - The current usage, such as the current number of servers that are associated with the account.
--
-- * 'aaMaximum' - The maximum allowed value.
--
-- * 'aaName' - The attribute name. The following are supported attribute names.      * /ServerLimit:/ The number of current servers/maximum number of servers allowed. By default, you can have a maximum of 10 servers.      * /ManualBackupLimit:/ The number of current manual backups/maximum number of backups allowed. By default, you can have a maximum of 50 manual backups saved.
accountAttribute ::
  AccountAttribute
accountAttribute =
  AccountAttribute'
    { _aaUsed = Nothing,
      _aaMaximum = Nothing,
      _aaName = Nothing
    }

-- | The current usage, such as the current number of servers that are associated with the account.
aaUsed :: Lens' AccountAttribute (Maybe Int)
aaUsed = lens _aaUsed (\s a -> s {_aaUsed = a})

-- | The maximum allowed value.
aaMaximum :: Lens' AccountAttribute (Maybe Int)
aaMaximum = lens _aaMaximum (\s a -> s {_aaMaximum = a})

-- | The attribute name. The following are supported attribute names.      * /ServerLimit:/ The number of current servers/maximum number of servers allowed. By default, you can have a maximum of 10 servers.      * /ManualBackupLimit:/ The number of current manual backups/maximum number of backups allowed. By default, you can have a maximum of 50 manual backups saved.
aaName :: Lens' AccountAttribute (Maybe Text)
aaName = lens _aaName (\s a -> s {_aaName = a})

instance FromJSON AccountAttribute where
  parseJSON =
    withObject
      "AccountAttribute"
      ( \x ->
          AccountAttribute'
            <$> (x .:? "Used") <*> (x .:? "Maximum") <*> (x .:? "Name")
      )

instance Hashable AccountAttribute

instance NFData AccountAttribute
