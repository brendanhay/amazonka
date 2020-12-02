{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Order
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Order where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the sort order of a sorted column.
--
--
--
-- /See:/ 'order' smart constructor.
data Order = Order' {_oColumn :: !Text, _oSortOrder :: !Nat}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Order' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oColumn' - The name of the column.
--
-- * 'oSortOrder' - Indicates that the column is sorted in ascending order (@== 1@ ), or in descending order (@==0@ ).
order ::
  -- | 'oColumn'
  Text ->
  -- | 'oSortOrder'
  Natural ->
  Order
order pColumn_ pSortOrder_ =
  Order' {_oColumn = pColumn_, _oSortOrder = _Nat # pSortOrder_}

-- | The name of the column.
oColumn :: Lens' Order Text
oColumn = lens _oColumn (\s a -> s {_oColumn = a})

-- | Indicates that the column is sorted in ascending order (@== 1@ ), or in descending order (@==0@ ).
oSortOrder :: Lens' Order Natural
oSortOrder = lens _oSortOrder (\s a -> s {_oSortOrder = a}) . _Nat

instance FromJSON Order where
  parseJSON =
    withObject
      "Order"
      (\x -> Order' <$> (x .: "Column") <*> (x .: "SortOrder"))

instance Hashable Order

instance NFData Order

instance ToJSON Order where
  toJSON Order' {..} =
    object
      ( catMaybes
          [Just ("Column" .= _oColumn), Just ("SortOrder" .= _oSortOrder)]
      )
