{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageAccountResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageAccountResult where

import Network.AWS.GuardDuty.Types.Total
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the total of usage based on account IDs.
--
--
--
-- /See:/ 'usageAccountResult' smart constructor.
data UsageAccountResult = UsageAccountResult'
  { _uarAccountId ::
      !(Maybe Text),
    _uarTotal :: !(Maybe Total)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsageAccountResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarAccountId' - The Account ID that generated usage.
--
-- * 'uarTotal' - Represents the total of usage for the Account ID.
usageAccountResult ::
  UsageAccountResult
usageAccountResult =
  UsageAccountResult' {_uarAccountId = Nothing, _uarTotal = Nothing}

-- | The Account ID that generated usage.
uarAccountId :: Lens' UsageAccountResult (Maybe Text)
uarAccountId = lens _uarAccountId (\s a -> s {_uarAccountId = a})

-- | Represents the total of usage for the Account ID.
uarTotal :: Lens' UsageAccountResult (Maybe Total)
uarTotal = lens _uarTotal (\s a -> s {_uarTotal = a})

instance FromJSON UsageAccountResult where
  parseJSON =
    withObject
      "UsageAccountResult"
      ( \x ->
          UsageAccountResult' <$> (x .:? "accountId") <*> (x .:? "total")
      )

instance Hashable UsageAccountResult

instance NFData UsageAccountResult
