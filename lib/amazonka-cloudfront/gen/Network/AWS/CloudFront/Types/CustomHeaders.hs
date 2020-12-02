{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomHeaders
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CustomHeaders where

import Network.AWS.CloudFront.Types.OriginCustomHeader
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains the list of Custom Headers for each origin.
--
--
--
-- /See:/ 'customHeaders' smart constructor.
data CustomHeaders = CustomHeaders'
  { _chItems ::
      !(Maybe [OriginCustomHeader]),
    _chQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomHeaders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chItems' - __Optional__ : A list that contains one @OriginCustomHeader@ element for each custom header that you want CloudFront to forward to the origin. If Quantity is @0@ , omit @Items@ .
--
-- * 'chQuantity' - The number of custom headers, if any, for this distribution.
customHeaders ::
  -- | 'chQuantity'
  Int ->
  CustomHeaders
customHeaders pQuantity_ =
  CustomHeaders' {_chItems = Nothing, _chQuantity = pQuantity_}

-- | __Optional__ : A list that contains one @OriginCustomHeader@ element for each custom header that you want CloudFront to forward to the origin. If Quantity is @0@ , omit @Items@ .
chItems :: Lens' CustomHeaders [OriginCustomHeader]
chItems = lens _chItems (\s a -> s {_chItems = a}) . _Default . _Coerce

-- | The number of custom headers, if any, for this distribution.
chQuantity :: Lens' CustomHeaders Int
chQuantity = lens _chQuantity (\s a -> s {_chQuantity = a})

instance FromXML CustomHeaders where
  parseXML x =
    CustomHeaders'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "OriginCustomHeader")
          )
      <*> (x .@ "Quantity")

instance Hashable CustomHeaders

instance NFData CustomHeaders

instance ToXML CustomHeaders where
  toXML CustomHeaders' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "OriginCustomHeader" <$> _chItems),
        "Quantity" @= _chQuantity
      ]
