{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AttributeType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies whether the attribute is standard or custom.
--
--
--
-- /See:/ 'attributeType' smart constructor.
data AttributeType = AttributeType'
  { _atValue ::
      !(Maybe (Sensitive Text)),
    _atName :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atValue' - The value of the attribute.
--
-- * 'atName' - The name of the attribute.
attributeType ::
  -- | 'atName'
  Text ->
  AttributeType
attributeType pName_ =
  AttributeType' {_atValue = Nothing, _atName = pName_}

-- | The value of the attribute.
atValue :: Lens' AttributeType (Maybe Text)
atValue = lens _atValue (\s a -> s {_atValue = a}) . mapping _Sensitive

-- | The name of the attribute.
atName :: Lens' AttributeType Text
atName = lens _atName (\s a -> s {_atName = a})

instance FromJSON AttributeType where
  parseJSON =
    withObject
      "AttributeType"
      (\x -> AttributeType' <$> (x .:? "Value") <*> (x .: "Name"))

instance Hashable AttributeType

instance NFData AttributeType

instance ToJSON AttributeType where
  toJSON AttributeType' {..} =
    object
      (catMaybes [("Value" .=) <$> _atValue, Just ("Name" .= _atName)])
