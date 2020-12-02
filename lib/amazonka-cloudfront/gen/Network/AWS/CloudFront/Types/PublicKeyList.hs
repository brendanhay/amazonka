{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.PublicKeyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKeyList where

import Network.AWS.CloudFront.Types.PublicKeySummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of public keys that you can use with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html signed URLs and signed cookies> , or with <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/field-level-encryption.html field-level encryption> .
--
--
--
-- /See:/ 'publicKeyList' smart constructor.
data PublicKeyList = PublicKeyList'
  { _pklItems ::
      !(Maybe [PublicKeySummary]),
    _pklNextMarker :: !(Maybe Text),
    _pklMaxItems :: !Int,
    _pklQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicKeyList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pklItems' - A list of public keys.
--
-- * 'pklNextMarker' - If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your public keys where you left off.
--
-- * 'pklMaxItems' - The maximum number of public keys you want in the response.
--
-- * 'pklQuantity' - The number of public keys in the list.
publicKeyList ::
  -- | 'pklMaxItems'
  Int ->
  -- | 'pklQuantity'
  Int ->
  PublicKeyList
publicKeyList pMaxItems_ pQuantity_ =
  PublicKeyList'
    { _pklItems = Nothing,
      _pklNextMarker = Nothing,
      _pklMaxItems = pMaxItems_,
      _pklQuantity = pQuantity_
    }

-- | A list of public keys.
pklItems :: Lens' PublicKeyList [PublicKeySummary]
pklItems = lens _pklItems (\s a -> s {_pklItems = a}) . _Default . _Coerce

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your public keys where you left off.
pklNextMarker :: Lens' PublicKeyList (Maybe Text)
pklNextMarker = lens _pklNextMarker (\s a -> s {_pklNextMarker = a})

-- | The maximum number of public keys you want in the response.
pklMaxItems :: Lens' PublicKeyList Int
pklMaxItems = lens _pklMaxItems (\s a -> s {_pklMaxItems = a})

-- | The number of public keys in the list.
pklQuantity :: Lens' PublicKeyList Int
pklQuantity = lens _pklQuantity (\s a -> s {_pklQuantity = a})

instance FromXML PublicKeyList where
  parseXML x =
    PublicKeyList'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "PublicKeySummary")
          )
      <*> (x .@? "NextMarker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "Quantity")

instance Hashable PublicKeyList

instance NFData PublicKeyList
