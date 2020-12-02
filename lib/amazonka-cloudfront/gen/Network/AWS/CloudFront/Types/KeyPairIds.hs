{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyPairIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyPairIds where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of CloudFront key pair identifiers.
--
--
--
-- /See:/ 'keyPairIds' smart constructor.
data KeyPairIds = KeyPairIds'
  { _kpiItems :: !(Maybe [Text]),
    _kpiQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyPairIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kpiItems' - A list of CloudFront key pair identifiers.
--
-- * 'kpiQuantity' - The number of key pair identifiers in the list.
keyPairIds ::
  -- | 'kpiQuantity'
  Int ->
  KeyPairIds
keyPairIds pQuantity_ =
  KeyPairIds' {_kpiItems = Nothing, _kpiQuantity = pQuantity_}

-- | A list of CloudFront key pair identifiers.
kpiItems :: Lens' KeyPairIds [Text]
kpiItems = lens _kpiItems (\s a -> s {_kpiItems = a}) . _Default . _Coerce

-- | The number of key pair identifiers in the list.
kpiQuantity :: Lens' KeyPairIds Int
kpiQuantity = lens _kpiQuantity (\s a -> s {_kpiQuantity = a})

instance FromXML KeyPairIds where
  parseXML x =
    KeyPairIds'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "KeyPairId"))
      <*> (x .@ "Quantity")

instance Hashable KeyPairIds

instance NFData KeyPairIds
