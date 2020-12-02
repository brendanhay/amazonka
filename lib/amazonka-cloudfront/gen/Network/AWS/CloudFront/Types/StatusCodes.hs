{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StatusCodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StatusCodes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex data type for the status codes that you specify that, when returned by a primary origin, trigger CloudFront to failover to a second origin.
--
--
--
-- /See:/ 'statusCodes' smart constructor.
data StatusCodes = StatusCodes'
  { _scQuantity :: !Int,
    _scItems :: !(List1 Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StatusCodes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scQuantity' - The number of status codes.
--
-- * 'scItems' - The items (status codes) for an origin group.
statusCodes ::
  -- | 'scQuantity'
  Int ->
  -- | 'scItems'
  NonEmpty Int ->
  StatusCodes
statusCodes pQuantity_ pItems_ =
  StatusCodes'
    { _scQuantity = pQuantity_,
      _scItems = _List1 # pItems_
    }

-- | The number of status codes.
scQuantity :: Lens' StatusCodes Int
scQuantity = lens _scQuantity (\s a -> s {_scQuantity = a})

-- | The items (status codes) for an origin group.
scItems :: Lens' StatusCodes (NonEmpty Int)
scItems = lens _scItems (\s a -> s {_scItems = a}) . _List1

instance FromXML StatusCodes where
  parseXML x =
    StatusCodes'
      <$> (x .@ "Quantity")
      <*> (x .@? "Items" .!@ mempty >>= parseXMLList1 "StatusCode")

instance Hashable StatusCodes

instance NFData StatusCodes

instance ToXML StatusCodes where
  toXML StatusCodes' {..} =
    mconcat
      [ "Quantity" @= _scQuantity,
        "Items" @= toXMLList "StatusCode" _scItems
      ]
