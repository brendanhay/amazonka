{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList where

import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | List of field-level encryption profiles.
--
--
--
-- /See:/ 'fieldLevelEncryptionProfileList' smart constructor.
data FieldLevelEncryptionProfileList = FieldLevelEncryptionProfileList'
  { _fleplItems ::
      !( Maybe
           [FieldLevelEncryptionProfileSummary]
       ),
    _fleplNextMarker ::
      !(Maybe Text),
    _fleplMaxItems :: !Int,
    _fleplQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldLevelEncryptionProfileList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fleplItems' - The field-level encryption profile items.
--
-- * 'fleplNextMarker' - If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your profiles where you left off.
--
-- * 'fleplMaxItems' - The maximum number of field-level encryption profiles you want in the response body.
--
-- * 'fleplQuantity' - The number of field-level encryption profiles.
fieldLevelEncryptionProfileList ::
  -- | 'fleplMaxItems'
  Int ->
  -- | 'fleplQuantity'
  Int ->
  FieldLevelEncryptionProfileList
fieldLevelEncryptionProfileList pMaxItems_ pQuantity_ =
  FieldLevelEncryptionProfileList'
    { _fleplItems = Nothing,
      _fleplNextMarker = Nothing,
      _fleplMaxItems = pMaxItems_,
      _fleplQuantity = pQuantity_
    }

-- | The field-level encryption profile items.
fleplItems :: Lens' FieldLevelEncryptionProfileList [FieldLevelEncryptionProfileSummary]
fleplItems = lens _fleplItems (\s a -> s {_fleplItems = a}) . _Default . _Coerce

-- | If there are more elements to be listed, this element is present and contains the value that you can use for the @Marker@ request parameter to continue listing your profiles where you left off.
fleplNextMarker :: Lens' FieldLevelEncryptionProfileList (Maybe Text)
fleplNextMarker = lens _fleplNextMarker (\s a -> s {_fleplNextMarker = a})

-- | The maximum number of field-level encryption profiles you want in the response body.
fleplMaxItems :: Lens' FieldLevelEncryptionProfileList Int
fleplMaxItems = lens _fleplMaxItems (\s a -> s {_fleplMaxItems = a})

-- | The number of field-level encryption profiles.
fleplQuantity :: Lens' FieldLevelEncryptionProfileList Int
fleplQuantity = lens _fleplQuantity (\s a -> s {_fleplQuantity = a})

instance FromXML FieldLevelEncryptionProfileList where
  parseXML x =
    FieldLevelEncryptionProfileList'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "FieldLevelEncryptionProfileSummary")
          )
      <*> (x .@? "NextMarker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "Quantity")

instance Hashable FieldLevelEncryptionProfileList

instance NFData FieldLevelEncryptionProfileList
