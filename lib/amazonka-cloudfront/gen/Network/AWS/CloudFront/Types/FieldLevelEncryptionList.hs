{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionList where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | List of field-level encrpytion configurations.
--
--
--
-- /See:/ 'fieldLevelEncryptionList' smart constructor.
data FieldLevelEncryptionList = FieldLevelEncryptionList'
  { _flelItems ::
      !(Maybe [FieldLevelEncryptionSummary]),
    _flelNextMarker :: !(Maybe Text),
    _flelMaxItems :: !Int,
    _flelQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldLevelEncryptionList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flelItems' - An array of field-level encryption items.
--
-- * 'flelNextMarker' - If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your configurations where you left off.
--
-- * 'flelMaxItems' - The maximum number of elements you want in the response body.
--
-- * 'flelQuantity' - The number of field-level encryption items.
fieldLevelEncryptionList ::
  -- | 'flelMaxItems'
  Int ->
  -- | 'flelQuantity'
  Int ->
  FieldLevelEncryptionList
fieldLevelEncryptionList pMaxItems_ pQuantity_ =
  FieldLevelEncryptionList'
    { _flelItems = Nothing,
      _flelNextMarker = Nothing,
      _flelMaxItems = pMaxItems_,
      _flelQuantity = pQuantity_
    }

-- | An array of field-level encryption items.
flelItems :: Lens' FieldLevelEncryptionList [FieldLevelEncryptionSummary]
flelItems = lens _flelItems (\s a -> s {_flelItems = a}) . _Default . _Coerce

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your configurations where you left off.
flelNextMarker :: Lens' FieldLevelEncryptionList (Maybe Text)
flelNextMarker = lens _flelNextMarker (\s a -> s {_flelNextMarker = a})

-- | The maximum number of elements you want in the response body.
flelMaxItems :: Lens' FieldLevelEncryptionList Int
flelMaxItems = lens _flelMaxItems (\s a -> s {_flelMaxItems = a})

-- | The number of field-level encryption items.
flelQuantity :: Lens' FieldLevelEncryptionList Int
flelQuantity = lens _flelQuantity (\s a -> s {_flelQuantity = a})

instance FromXML FieldLevelEncryptionList where
  parseXML x =
    FieldLevelEncryptionList'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "FieldLevelEncryptionSummary")
          )
      <*> (x .@? "NextMarker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "Quantity")

instance Hashable FieldLevelEncryptionList

instance NFData FieldLevelEncryptionList
