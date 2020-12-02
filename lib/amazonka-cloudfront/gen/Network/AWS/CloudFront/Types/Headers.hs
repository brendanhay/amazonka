{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Headers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Headers where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a list of HTTP header names.
--
--
--
-- /See:/ 'headers' smart constructor.
data Headers = Headers'
  { _hItems :: !(Maybe [Text]),
    _hQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Headers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hItems' - A list of HTTP header names.
--
-- * 'hQuantity' - The number of header names in the @Items@ list.
headers ::
  -- | 'hQuantity'
  Int ->
  Headers
headers pQuantity_ =
  Headers' {_hItems = Nothing, _hQuantity = pQuantity_}

-- | A list of HTTP header names.
hItems :: Lens' Headers [Text]
hItems = lens _hItems (\s a -> s {_hItems = a}) . _Default . _Coerce

-- | The number of header names in the @Items@ list.
hQuantity :: Lens' Headers Int
hQuantity = lens _hQuantity (\s a -> s {_hQuantity = a})

instance FromXML Headers where
  parseXML x =
    Headers'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "Name"))
      <*> (x .@ "Quantity")

instance Hashable Headers

instance NFData Headers

instance ToXML Headers where
  toXML Headers' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "Name" <$> _hItems),
        "Quantity" @= _hQuantity
      ]
