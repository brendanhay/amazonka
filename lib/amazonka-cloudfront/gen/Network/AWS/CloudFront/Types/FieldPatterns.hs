{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.FieldPatterns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.FieldPatterns where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex data type that includes the field patterns to match for field-level encryption.
--
--
--
-- /See:/ 'fieldPatterns' smart constructor.
data FieldPatterns = FieldPatterns'
  { _fpItems :: !(Maybe [Text]),
    _fpQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FieldPatterns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fpItems' - An array of the field-level encryption field patterns.
--
-- * 'fpQuantity' - The number of field-level encryption field patterns.
fieldPatterns ::
  -- | 'fpQuantity'
  Int ->
  FieldPatterns
fieldPatterns pQuantity_ =
  FieldPatterns' {_fpItems = Nothing, _fpQuantity = pQuantity_}

-- | An array of the field-level encryption field patterns.
fpItems :: Lens' FieldPatterns [Text]
fpItems = lens _fpItems (\s a -> s {_fpItems = a}) . _Default . _Coerce

-- | The number of field-level encryption field patterns.
fpQuantity :: Lens' FieldPatterns Int
fpQuantity = lens _fpQuantity (\s a -> s {_fpQuantity = a})

instance FromXML FieldPatterns where
  parseXML x =
    FieldPatterns'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "FieldPattern"))
      <*> (x .@ "Quantity")

instance Hashable FieldPatterns

instance NFData FieldPatterns

instance ToXML FieldPatterns where
  toXML FieldPatterns' {..} =
    mconcat
      [ "Items" @= toXML (toXMLList "FieldPattern" <$> _fpItems),
        "Quantity" @= _fpQuantity
      ]
