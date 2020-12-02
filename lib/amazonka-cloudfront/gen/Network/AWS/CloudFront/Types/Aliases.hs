{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Aliases
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Aliases where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this distribution.
--
--
--
-- /See:/ 'aliases' smart constructor.
data Aliases = Aliases'
  { _aItems :: !(Maybe [Text]),
    _aQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Aliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aItems' - A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
--
-- * 'aQuantity' - The number of CNAME aliases, if any, that you want to associate with this distribution.
aliases ::
  -- | 'aQuantity'
  Int ->
  Aliases
aliases pQuantity_ =
  Aliases' {_aItems = Nothing, _aQuantity = pQuantity_}

-- | A complex type that contains the CNAME aliases, if any, that you want to associate with this distribution.
aItems :: Lens' Aliases [Text]
aItems = lens _aItems (\s a -> s {_aItems = a}) . _Default . _Coerce

-- | The number of CNAME aliases, if any, that you want to associate with this distribution.
aQuantity :: Lens' Aliases Int
aQuantity = lens _aQuantity (\s a -> s {_aQuantity = a})

instance FromXML Aliases where
  parseXML x =
    Aliases'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "CNAME"))
      <*> (x .@ "Quantity")

instance Hashable Aliases

instance NFData Aliases

instance ToXML Aliases where
  toXML Aliases' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "CNAME" <$> _aItems),
        "Quantity" @= _aQuantity
      ]
