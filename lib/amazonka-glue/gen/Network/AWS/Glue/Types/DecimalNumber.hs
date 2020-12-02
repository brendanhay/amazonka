{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DecimalNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DecimalNumber where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a numeric value in decimal format.
--
--
--
-- /See:/ 'decimalNumber' smart constructor.
data DecimalNumber = DecimalNumber'
  { _dnUnscaledValue :: !Base64,
    _dnScale :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DecimalNumber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnUnscaledValue' - The unscaled numeric value.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'dnScale' - The scale that determines where the decimal point falls in the unscaled value.
decimalNumber ::
  -- | 'dnUnscaledValue'
  ByteString ->
  -- | 'dnScale'
  Int ->
  DecimalNumber
decimalNumber pUnscaledValue_ pScale_ =
  DecimalNumber'
    { _dnUnscaledValue = _Base64 # pUnscaledValue_,
      _dnScale = pScale_
    }

-- | The unscaled numeric value.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
dnUnscaledValue :: Lens' DecimalNumber ByteString
dnUnscaledValue = lens _dnUnscaledValue (\s a -> s {_dnUnscaledValue = a}) . _Base64

-- | The scale that determines where the decimal point falls in the unscaled value.
dnScale :: Lens' DecimalNumber Int
dnScale = lens _dnScale (\s a -> s {_dnScale = a})

instance FromJSON DecimalNumber where
  parseJSON =
    withObject
      "DecimalNumber"
      ( \x ->
          DecimalNumber' <$> (x .: "UnscaledValue") <*> (x .: "Scale")
      )

instance Hashable DecimalNumber

instance NFData DecimalNumber

instance ToJSON DecimalNumber where
  toJSON DecimalNumber' {..} =
    object
      ( catMaybes
          [ Just ("UnscaledValue" .= _dnUnscaledValue),
            Just ("Scale" .= _dnScale)
          ]
      )
