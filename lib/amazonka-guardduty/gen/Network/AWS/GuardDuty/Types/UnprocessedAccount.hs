{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UnprocessedAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UnprocessedAccount where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the accounts that weren't processed.
--
--
--
-- /See:/ 'unprocessedAccount' smart constructor.
data UnprocessedAccount = UnprocessedAccount'
  { _uaAccountId ::
      !Text,
    _uaResult :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UnprocessedAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaAccountId' - The AWS account ID.
--
-- * 'uaResult' - A reason why the account hasn't been processed.
unprocessedAccount ::
  -- | 'uaAccountId'
  Text ->
  -- | 'uaResult'
  Text ->
  UnprocessedAccount
unprocessedAccount pAccountId_ pResult_ =
  UnprocessedAccount'
    { _uaAccountId = pAccountId_,
      _uaResult = pResult_
    }

-- | The AWS account ID.
uaAccountId :: Lens' UnprocessedAccount Text
uaAccountId = lens _uaAccountId (\s a -> s {_uaAccountId = a})

-- | A reason why the account hasn't been processed.
uaResult :: Lens' UnprocessedAccount Text
uaResult = lens _uaResult (\s a -> s {_uaResult = a})

instance FromJSON UnprocessedAccount where
  parseJSON =
    withObject
      "UnprocessedAccount"
      ( \x ->
          UnprocessedAccount' <$> (x .: "accountId") <*> (x .: "result")
      )

instance Hashable UnprocessedAccount

instance NFData UnprocessedAccount
