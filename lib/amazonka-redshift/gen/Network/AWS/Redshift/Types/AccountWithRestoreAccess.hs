{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AccountWithRestoreAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AccountWithRestoreAccess where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes an AWS customer account authorized to restore a snapshot.
--
--
--
-- /See:/ 'accountWithRestoreAccess' smart constructor.
data AccountWithRestoreAccess = AccountWithRestoreAccess'
  { _awraAccountAlias ::
      !(Maybe Text),
    _awraAccountId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountWithRestoreAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'awraAccountAlias' - The identifier of an AWS support account authorized to restore a snapshot. For AWS support, the identifier is @amazon-redshift-support@ .
--
-- * 'awraAccountId' - The identifier of an AWS customer account authorized to restore a snapshot.
accountWithRestoreAccess ::
  AccountWithRestoreAccess
accountWithRestoreAccess =
  AccountWithRestoreAccess'
    { _awraAccountAlias = Nothing,
      _awraAccountId = Nothing
    }

-- | The identifier of an AWS support account authorized to restore a snapshot. For AWS support, the identifier is @amazon-redshift-support@ .
awraAccountAlias :: Lens' AccountWithRestoreAccess (Maybe Text)
awraAccountAlias = lens _awraAccountAlias (\s a -> s {_awraAccountAlias = a})

-- | The identifier of an AWS customer account authorized to restore a snapshot.
awraAccountId :: Lens' AccountWithRestoreAccess (Maybe Text)
awraAccountId = lens _awraAccountId (\s a -> s {_awraAccountId = a})

instance FromXML AccountWithRestoreAccess where
  parseXML x =
    AccountWithRestoreAccess'
      <$> (x .@? "AccountAlias") <*> (x .@? "AccountId")

instance Hashable AccountWithRestoreAccess

instance NFData AccountWithRestoreAccess
