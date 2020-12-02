{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CacheBehaviors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CacheBehaviors where

import Network.AWS.CloudFront.Types.CacheBehavior
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains zero or more @CacheBehavior@ elements.
--
--
--
-- /See:/ 'cacheBehaviors' smart constructor.
data CacheBehaviors = CacheBehaviors'
  { _cbItems ::
      !(Maybe [CacheBehavior]),
    _cbQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheBehaviors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbItems' - Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- * 'cbQuantity' - The number of cache behaviors for this distribution.
cacheBehaviors ::
  -- | 'cbQuantity'
  Int ->
  CacheBehaviors
cacheBehaviors pQuantity_ =
  CacheBehaviors' {_cbItems = Nothing, _cbQuantity = pQuantity_}

-- | Optional: A complex type that contains cache behaviors for this distribution. If @Quantity@ is @0@ , you can omit @Items@ .
cbItems :: Lens' CacheBehaviors [CacheBehavior]
cbItems = lens _cbItems (\s a -> s {_cbItems = a}) . _Default . _Coerce

-- | The number of cache behaviors for this distribution.
cbQuantity :: Lens' CacheBehaviors Int
cbQuantity = lens _cbQuantity (\s a -> s {_cbQuantity = a})

instance FromXML CacheBehaviors where
  parseXML x =
    CacheBehaviors'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "CacheBehavior"))
      <*> (x .@ "Quantity")

instance Hashable CacheBehaviors

instance NFData CacheBehaviors

instance ToXML CacheBehaviors where
  toXML CacheBehaviors' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "CacheBehavior" <$> _cbItems),
        "Quantity" @= _cbQuantity
      ]
