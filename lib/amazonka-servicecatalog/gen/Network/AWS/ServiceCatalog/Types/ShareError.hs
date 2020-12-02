{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ShareError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ShareError where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Errors that occurred during the portfolio share operation.
--
--
--
-- /See:/ 'shareError' smart constructor.
data ShareError = ShareError'
  { _seAccounts :: !(Maybe [Text]),
    _seError :: !(Maybe Text),
    _seMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShareError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seAccounts' - List of accounts impacted by the error.
--
-- * 'seError' - Error type that happened when processing the operation.
--
-- * 'seMessage' - Information about the error.
shareError ::
  ShareError
shareError =
  ShareError'
    { _seAccounts = Nothing,
      _seError = Nothing,
      _seMessage = Nothing
    }

-- | List of accounts impacted by the error.
seAccounts :: Lens' ShareError [Text]
seAccounts = lens _seAccounts (\s a -> s {_seAccounts = a}) . _Default . _Coerce

-- | Error type that happened when processing the operation.
seError :: Lens' ShareError (Maybe Text)
seError = lens _seError (\s a -> s {_seError = a})

-- | Information about the error.
seMessage :: Lens' ShareError (Maybe Text)
seMessage = lens _seMessage (\s a -> s {_seMessage = a})

instance FromJSON ShareError where
  parseJSON =
    withObject
      "ShareError"
      ( \x ->
          ShareError'
            <$> (x .:? "Accounts" .!= mempty)
            <*> (x .:? "Error")
            <*> (x .:? "Message")
      )

instance Hashable ShareError

instance NFData ShareError
