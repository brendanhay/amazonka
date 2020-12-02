{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AccountSharingInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AccountSharingInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information includes the AWS account ID where the current document is shared and the version shared with that account.
--
--
--
-- /See:/ 'accountSharingInfo' smart constructor.
data AccountSharingInfo = AccountSharingInfo'
  { _asiSharedDocumentVersion ::
      !(Maybe Text),
    _asiAccountId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountSharingInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asiSharedDocumentVersion' - The version of the current document shared with the account.
--
-- * 'asiAccountId' - The AWS account ID where the current document is shared.
accountSharingInfo ::
  AccountSharingInfo
accountSharingInfo =
  AccountSharingInfo'
    { _asiSharedDocumentVersion = Nothing,
      _asiAccountId = Nothing
    }

-- | The version of the current document shared with the account.
asiSharedDocumentVersion :: Lens' AccountSharingInfo (Maybe Text)
asiSharedDocumentVersion = lens _asiSharedDocumentVersion (\s a -> s {_asiSharedDocumentVersion = a})

-- | The AWS account ID where the current document is shared.
asiAccountId :: Lens' AccountSharingInfo (Maybe Text)
asiAccountId = lens _asiAccountId (\s a -> s {_asiAccountId = a})

instance FromJSON AccountSharingInfo where
  parseJSON =
    withObject
      "AccountSharingInfo"
      ( \x ->
          AccountSharingInfo'
            <$> (x .:? "SharedDocumentVersion") <*> (x .:? "AccountId")
      )

instance Hashable AccountSharingInfo

instance NFData AccountSharingInfo
