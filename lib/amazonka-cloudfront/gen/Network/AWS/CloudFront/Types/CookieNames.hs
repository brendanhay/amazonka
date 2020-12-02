{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CookieNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CookieNames where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a list of cookie names.
--
--
--
-- /See:/ 'cookieNames' smart constructor.
data CookieNames = CookieNames'
  { _cnItems :: !(Maybe [Text]),
    _cnQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CookieNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnItems' - A list of cookie names.
--
-- * 'cnQuantity' - The number of cookie names in the @Items@ list.
cookieNames ::
  -- | 'cnQuantity'
  Int ->
  CookieNames
cookieNames pQuantity_ =
  CookieNames' {_cnItems = Nothing, _cnQuantity = pQuantity_}

-- | A list of cookie names.
cnItems :: Lens' CookieNames [Text]
cnItems = lens _cnItems (\s a -> s {_cnItems = a}) . _Default . _Coerce

-- | The number of cookie names in the @Items@ list.
cnQuantity :: Lens' CookieNames Int
cnQuantity = lens _cnQuantity (\s a -> s {_cnQuantity = a})

instance FromXML CookieNames where
  parseXML x =
    CookieNames'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "Name"))
      <*> (x .@ "Quantity")

instance Hashable CookieNames

instance NFData CookieNames

instance ToXML CookieNames where
  toXML CookieNames' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "Name" <$> _cnItems),
        "Quantity" @= _cnQuantity
      ]
