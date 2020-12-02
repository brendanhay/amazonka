{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The minimum and maximum value of an attribute that is of the number data type.
--
--
--
-- /See:/ 'numberAttributeConstraintsType' smart constructor.
data NumberAttributeConstraintsType = NumberAttributeConstraintsType'
  { _nactMaxValue ::
      !(Maybe Text),
    _nactMinValue ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NumberAttributeConstraintsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nactMaxValue' - The maximum value of an attribute that is of the number data type.
--
-- * 'nactMinValue' - The minimum value of an attribute that is of the number data type.
numberAttributeConstraintsType ::
  NumberAttributeConstraintsType
numberAttributeConstraintsType =
  NumberAttributeConstraintsType'
    { _nactMaxValue = Nothing,
      _nactMinValue = Nothing
    }

-- | The maximum value of an attribute that is of the number data type.
nactMaxValue :: Lens' NumberAttributeConstraintsType (Maybe Text)
nactMaxValue = lens _nactMaxValue (\s a -> s {_nactMaxValue = a})

-- | The minimum value of an attribute that is of the number data type.
nactMinValue :: Lens' NumberAttributeConstraintsType (Maybe Text)
nactMinValue = lens _nactMinValue (\s a -> s {_nactMinValue = a})

instance FromJSON NumberAttributeConstraintsType where
  parseJSON =
    withObject
      "NumberAttributeConstraintsType"
      ( \x ->
          NumberAttributeConstraintsType'
            <$> (x .:? "MaxValue") <*> (x .:? "MinValue")
      )

instance Hashable NumberAttributeConstraintsType

instance NFData NumberAttributeConstraintsType

instance ToJSON NumberAttributeConstraintsType where
  toJSON NumberAttributeConstraintsType' {..} =
    object
      ( catMaybes
          [ ("MaxValue" .=) <$> _nactMaxValue,
            ("MinValue" .=) <$> _nactMinValue
          ]
      )
