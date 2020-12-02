{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TransactGetItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TransactGetItem where

import Network.AWS.DynamoDB.Types.Get
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies an item to be retrieved as part of the transaction.
--
--
--
-- /See:/ 'transactGetItem' smart constructor.
newtype TransactGetItem = TransactGetItem' {_tgiGet :: Get}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransactGetItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgiGet' - Contains the primary key that identifies the item to get, together with the name of the table that contains the item, and optionally the specific attributes of the item to retrieve.
transactGetItem ::
  -- | 'tgiGet'
  Get ->
  TransactGetItem
transactGetItem pGet_ = TransactGetItem' {_tgiGet = pGet_}

-- | Contains the primary key that identifies the item to get, together with the name of the table that contains the item, and optionally the specific attributes of the item to retrieve.
tgiGet :: Lens' TransactGetItem Get
tgiGet = lens _tgiGet (\s a -> s {_tgiGet = a})

instance Hashable TransactGetItem

instance NFData TransactGetItem

instance ToJSON TransactGetItem where
  toJSON TransactGetItem' {..} =
    object (catMaybes [Just ("Get" .= _tgiGet)])
