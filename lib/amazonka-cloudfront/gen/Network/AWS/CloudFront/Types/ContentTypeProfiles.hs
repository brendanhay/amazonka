{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ContentTypeProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ContentTypeProfiles where

import Network.AWS.CloudFront.Types.ContentTypeProfile
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Field-level encryption content type-profile.
--
--
--
-- /See:/ 'contentTypeProfiles' smart constructor.
data ContentTypeProfiles = ContentTypeProfiles'
  { _ctpItems ::
      !(Maybe [ContentTypeProfile]),
    _ctpQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContentTypeProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpItems' - Items in a field-level encryption content type-profile mapping.
--
-- * 'ctpQuantity' - The number of field-level encryption content type-profile mappings.
contentTypeProfiles ::
  -- | 'ctpQuantity'
  Int ->
  ContentTypeProfiles
contentTypeProfiles pQuantity_ =
  ContentTypeProfiles'
    { _ctpItems = Nothing,
      _ctpQuantity = pQuantity_
    }

-- | Items in a field-level encryption content type-profile mapping.
ctpItems :: Lens' ContentTypeProfiles [ContentTypeProfile]
ctpItems = lens _ctpItems (\s a -> s {_ctpItems = a}) . _Default . _Coerce

-- | The number of field-level encryption content type-profile mappings.
ctpQuantity :: Lens' ContentTypeProfiles Int
ctpQuantity = lens _ctpQuantity (\s a -> s {_ctpQuantity = a})

instance FromXML ContentTypeProfiles where
  parseXML x =
    ContentTypeProfiles'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "ContentTypeProfile")
          )
      <*> (x .@ "Quantity")

instance Hashable ContentTypeProfiles

instance NFData ContentTypeProfiles

instance ToXML ContentTypeProfiles where
  toXML ContentTypeProfiles' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "ContentTypeProfile" <$> _ctpItems),
        "Quantity" @= _ctpQuantity
      ]
