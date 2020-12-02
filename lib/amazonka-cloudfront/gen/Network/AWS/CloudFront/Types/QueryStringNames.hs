{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.QueryStringNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.QueryStringNames where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a list of query string names.
--
--
--
-- /See:/ 'queryStringNames' smart constructor.
data QueryStringNames = QueryStringNames'
  { _qsnItems ::
      !(Maybe [Text]),
    _qsnQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryStringNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsnItems' - A list of query string names.
--
-- * 'qsnQuantity' - The number of query string names in the @Items@ list.
queryStringNames ::
  -- | 'qsnQuantity'
  Int ->
  QueryStringNames
queryStringNames pQuantity_ =
  QueryStringNames' {_qsnItems = Nothing, _qsnQuantity = pQuantity_}

-- | A list of query string names.
qsnItems :: Lens' QueryStringNames [Text]
qsnItems = lens _qsnItems (\s a -> s {_qsnItems = a}) . _Default . _Coerce

-- | The number of query string names in the @Items@ list.
qsnQuantity :: Lens' QueryStringNames Int
qsnQuantity = lens _qsnQuantity (\s a -> s {_qsnQuantity = a})

instance FromXML QueryStringNames where
  parseXML x =
    QueryStringNames'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "Name"))
      <*> (x .@ "Quantity")

instance Hashable QueryStringNames

instance NFData QueryStringNames

instance ToXML QueryStringNames where
  toXML QueryStringNames' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "Name" <$> _qsnItems),
        "Quantity" @= _qsnQuantity
      ]
