{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Attribute where

import Network.AWS.Connect.Types.InstanceAttributeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A toggle for an individual feature at the instance level.
--
--
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
  { _aValue :: !(Maybe Text),
    _aAttributeType :: !(Maybe InstanceAttributeType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aValue' - The value of the attribute.
--
-- * 'aAttributeType' - The type of attribute.
attribute ::
  Attribute
attribute =
  Attribute' {_aValue = Nothing, _aAttributeType = Nothing}

-- | The value of the attribute.
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\s a -> s {_aValue = a})

-- | The type of attribute.
aAttributeType :: Lens' Attribute (Maybe InstanceAttributeType)
aAttributeType = lens _aAttributeType (\s a -> s {_aAttributeType = a})

instance FromJSON Attribute where
  parseJSON =
    withObject
      "Attribute"
      (\x -> Attribute' <$> (x .:? "Value") <*> (x .:? "AttributeType"))

instance Hashable Attribute

instance NFData Attribute
