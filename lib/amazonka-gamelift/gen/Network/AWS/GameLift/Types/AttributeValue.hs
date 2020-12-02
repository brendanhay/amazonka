{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.AttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.AttributeValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Values for use in 'Player' attribute key-value pairs. This object lets you specify an attribute value using any of the valid data types: string, number, string array, or data map. Each @AttributeValue@ object can use only one of the available properties.
--
--
--
-- /See:/ 'attributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { _avSL :: !(Maybe [Text]),
    _avSDM :: !(Maybe (Map Text (Double))),
    _avN :: !(Maybe Double),
    _avS :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avSL' - For a list of up to 10 strings. Maximum length for each string is 100 characters. Duplicate values are not recognized; all occurrences of the repeated value after the first of a repeated value are ignored.
--
-- * 'avSDM' - For a map of up to 10 data type:value pairs. Maximum length for each string value is 100 characters.
--
-- * 'avN' - For number values, expressed as double.
--
-- * 'avS' - For single string values. Maximum string length is 100 characters.
attributeValue ::
  AttributeValue
attributeValue =
  AttributeValue'
    { _avSL = Nothing,
      _avSDM = Nothing,
      _avN = Nothing,
      _avS = Nothing
    }

-- | For a list of up to 10 strings. Maximum length for each string is 100 characters. Duplicate values are not recognized; all occurrences of the repeated value after the first of a repeated value are ignored.
avSL :: Lens' AttributeValue [Text]
avSL = lens _avSL (\s a -> s {_avSL = a}) . _Default . _Coerce

-- | For a map of up to 10 data type:value pairs. Maximum length for each string value is 100 characters.
avSDM :: Lens' AttributeValue (HashMap Text (Double))
avSDM = lens _avSDM (\s a -> s {_avSDM = a}) . _Default . _Map

-- | For number values, expressed as double.
avN :: Lens' AttributeValue (Maybe Double)
avN = lens _avN (\s a -> s {_avN = a})

-- | For single string values. Maximum string length is 100 characters.
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\s a -> s {_avS = a})

instance FromJSON AttributeValue where
  parseJSON =
    withObject
      "AttributeValue"
      ( \x ->
          AttributeValue'
            <$> (x .:? "SL" .!= mempty)
            <*> (x .:? "SDM" .!= mempty)
            <*> (x .:? "N")
            <*> (x .:? "S")
      )

instance Hashable AttributeValue

instance NFData AttributeValue

instance ToJSON AttributeValue where
  toJSON AttributeValue' {..} =
    object
      ( catMaybes
          [ ("SL" .=) <$> _avSL,
            ("SDM" .=) <$> _avSDM,
            ("N" .=) <$> _avN,
            ("S" .=) <$> _avS
          ]
      )
