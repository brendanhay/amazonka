{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The constraints associated with a string attribute.
--
--
--
-- /See:/ 'stringAttributeConstraintsType' smart constructor.
data StringAttributeConstraintsType = StringAttributeConstraintsType'
  { _sactMaxLength ::
      !(Maybe Text),
    _sactMinLength ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StringAttributeConstraintsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sactMaxLength' - The maximum length.
--
-- * 'sactMinLength' - The minimum length.
stringAttributeConstraintsType ::
  StringAttributeConstraintsType
stringAttributeConstraintsType =
  StringAttributeConstraintsType'
    { _sactMaxLength = Nothing,
      _sactMinLength = Nothing
    }

-- | The maximum length.
sactMaxLength :: Lens' StringAttributeConstraintsType (Maybe Text)
sactMaxLength = lens _sactMaxLength (\s a -> s {_sactMaxLength = a})

-- | The minimum length.
sactMinLength :: Lens' StringAttributeConstraintsType (Maybe Text)
sactMinLength = lens _sactMinLength (\s a -> s {_sactMinLength = a})

instance FromJSON StringAttributeConstraintsType where
  parseJSON =
    withObject
      "StringAttributeConstraintsType"
      ( \x ->
          StringAttributeConstraintsType'
            <$> (x .:? "MaxLength") <*> (x .:? "MinLength")
      )

instance Hashable StringAttributeConstraintsType

instance NFData StringAttributeConstraintsType

instance ToJSON StringAttributeConstraintsType where
  toJSON StringAttributeConstraintsType' {..} =
    object
      ( catMaybes
          [ ("MaxLength" .=) <$> _sactMaxLength,
            ("MinLength" .=) <$> _sactMinLength
          ]
      )
