{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccountDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccountDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the account.
--
--
--
-- /See:/ 'accountDetail' smart constructor.
data AccountDetail = AccountDetail'
  { _adAccountId :: !Text,
    _adEmail :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAccountId' - The member account ID.
--
-- * 'adEmail' - The email address of the member account.
accountDetail ::
  -- | 'adAccountId'
  Text ->
  -- | 'adEmail'
  Text ->
  AccountDetail
accountDetail pAccountId_ pEmail_ =
  AccountDetail' {_adAccountId = pAccountId_, _adEmail = pEmail_}

-- | The member account ID.
adAccountId :: Lens' AccountDetail Text
adAccountId = lens _adAccountId (\s a -> s {_adAccountId = a})

-- | The email address of the member account.
adEmail :: Lens' AccountDetail Text
adEmail = lens _adEmail (\s a -> s {_adEmail = a})

instance Hashable AccountDetail

instance NFData AccountDetail

instance ToJSON AccountDetail where
  toJSON AccountDetail' {..} =
    object
      ( catMaybes
          [Just ("accountId" .= _adAccountId), Just ("email" .= _adEmail)]
      )
