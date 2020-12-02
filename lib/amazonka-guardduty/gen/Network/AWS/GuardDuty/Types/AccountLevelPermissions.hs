{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccountLevelPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccountLevelPermissions where

import Network.AWS.GuardDuty.Types.BlockPublicAccess
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the account level permissions on the S3 bucket.
--
--
--
-- /See:/ 'accountLevelPermissions' smart constructor.
newtype AccountLevelPermissions = AccountLevelPermissions'
  { _alpBlockPublicAccess ::
      Maybe BlockPublicAccess
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountLevelPermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alpBlockPublicAccess' - Describes the S3 Block Public Access settings of the bucket's parent account.
accountLevelPermissions ::
  AccountLevelPermissions
accountLevelPermissions =
  AccountLevelPermissions' {_alpBlockPublicAccess = Nothing}

-- | Describes the S3 Block Public Access settings of the bucket's parent account.
alpBlockPublicAccess :: Lens' AccountLevelPermissions (Maybe BlockPublicAccess)
alpBlockPublicAccess = lens _alpBlockPublicAccess (\s a -> s {_alpBlockPublicAccess = a})

instance FromJSON AccountLevelPermissions where
  parseJSON =
    withObject
      "AccountLevelPermissions"
      (\x -> AccountLevelPermissions' <$> (x .:? "blockPublicAccess"))

instance Hashable AccountLevelPermissions

instance NFData AccountLevelPermissions
