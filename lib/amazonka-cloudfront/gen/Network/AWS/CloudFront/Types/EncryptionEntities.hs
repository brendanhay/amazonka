{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EncryptionEntities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EncryptionEntities where

import Network.AWS.CloudFront.Types.EncryptionEntity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Complex data type for field-level encryption profiles that includes all of the encryption entities.
--
--
--
-- /See:/ 'encryptionEntities' smart constructor.
data EncryptionEntities = EncryptionEntities'
  { _eeItems ::
      !(Maybe [EncryptionEntity]),
    _eeQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionEntities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eeItems' - An array of field patterns in a field-level encryption content type-profile mapping.
--
-- * 'eeQuantity' - Number of field pattern items in a field-level encryption content type-profile mapping.
encryptionEntities ::
  -- | 'eeQuantity'
  Int ->
  EncryptionEntities
encryptionEntities pQuantity_ =
  EncryptionEntities' {_eeItems = Nothing, _eeQuantity = pQuantity_}

-- | An array of field patterns in a field-level encryption content type-profile mapping.
eeItems :: Lens' EncryptionEntities [EncryptionEntity]
eeItems = lens _eeItems (\s a -> s {_eeItems = a}) . _Default . _Coerce

-- | Number of field pattern items in a field-level encryption content type-profile mapping.
eeQuantity :: Lens' EncryptionEntities Int
eeQuantity = lens _eeQuantity (\s a -> s {_eeQuantity = a})

instance FromXML EncryptionEntities where
  parseXML x =
    EncryptionEntities'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "EncryptionEntity")
          )
      <*> (x .@ "Quantity")

instance Hashable EncryptionEntities

instance NFData EncryptionEntities

instance ToXML EncryptionEntities where
  toXML EncryptionEntities' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "EncryptionEntity" <$> _eeItems),
        "Quantity" @= _eeQuantity
      ]
