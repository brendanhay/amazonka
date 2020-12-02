{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Paths
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Paths where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains information about the objects that you want to invalidate. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
--
--
--
-- /See:/ 'paths' smart constructor.
data Paths = Paths' {_pItems :: !(Maybe [Text]), _pQuantity :: !Int}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Paths' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pItems' - A complex type that contains a list of the paths that you want to invalidate.
--
-- * 'pQuantity' - The number of invalidation paths specified for the objects that you want to invalidate.
paths ::
  -- | 'pQuantity'
  Int ->
  Paths
paths pQuantity_ =
  Paths' {_pItems = Nothing, _pQuantity = pQuantity_}

-- | A complex type that contains a list of the paths that you want to invalidate.
pItems :: Lens' Paths [Text]
pItems = lens _pItems (\s a -> s {_pItems = a}) . _Default . _Coerce

-- | The number of invalidation paths specified for the objects that you want to invalidate.
pQuantity :: Lens' Paths Int
pQuantity = lens _pQuantity (\s a -> s {_pQuantity = a})

instance FromXML Paths where
  parseXML x =
    Paths'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "Path"))
      <*> (x .@ "Quantity")

instance Hashable Paths

instance NFData Paths

instance ToXML Paths where
  toXML Paths' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "Path" <$> _pItems),
        "Quantity" @= _pQuantity
      ]
