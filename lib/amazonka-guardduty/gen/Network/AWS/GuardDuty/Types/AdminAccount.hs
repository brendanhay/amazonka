{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AdminAccount where

import Network.AWS.GuardDuty.Types.AdminStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The account within the organization specified as the GuardDuty delegated administrator.
--
--
--
-- /See:/ 'adminAccount' smart constructor.
data AdminAccount = AdminAccount'
  { _aaAdminAccountId ::
      !(Maybe Text),
    _aaAdminStatus :: !(Maybe AdminStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaAdminAccountId' - The AWS account ID for the account.
--
-- * 'aaAdminStatus' - Indicates whether the account is enabled as the delegated administrator.
adminAccount ::
  AdminAccount
adminAccount =
  AdminAccount'
    { _aaAdminAccountId = Nothing,
      _aaAdminStatus = Nothing
    }

-- | The AWS account ID for the account.
aaAdminAccountId :: Lens' AdminAccount (Maybe Text)
aaAdminAccountId = lens _aaAdminAccountId (\s a -> s {_aaAdminAccountId = a})

-- | Indicates whether the account is enabled as the delegated administrator.
aaAdminStatus :: Lens' AdminAccount (Maybe AdminStatus)
aaAdminStatus = lens _aaAdminStatus (\s a -> s {_aaAdminStatus = a})

instance FromJSON AdminAccount where
  parseJSON =
    withObject
      "AdminAccount"
      ( \x ->
          AdminAccount'
            <$> (x .:? "adminAccountId") <*> (x .:? "adminStatus")
      )

instance Hashable AdminAccount

instance NFData AdminAccount
