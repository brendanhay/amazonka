{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Attribute where

import Network.AWS.ECS.Types.TargetType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'attribute' smart constructor.
data Attribute = Attribute'
  { _aTargetId :: !(Maybe Text),
    _aValue :: !(Maybe Text),
    _aTargetType :: !(Maybe TargetType),
    _aName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aTargetId' - The ID of the target. You can specify the short form ID for a resource or the full Amazon Resource Name (ARN).
--
-- * 'aValue' - The value of the attribute. The @value@ must contain between 1 and 128 characters and may contain letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, back slashes, colons, or spaces. The value cannot contain any leading or trailing whitespace.
--
-- * 'aTargetType' - The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full ARN.
--
-- * 'aName' - The name of the attribute. The @name@ must contain between 1 and 128 characters and name may contain letters (uppercase and lowercase), numbers, hyphens, underscores, forward slashes, back slashes, or periods.
attribute ::
  -- | 'aName'
  Text ->
  Attribute
attribute pName_ =
  Attribute'
    { _aTargetId = Nothing,
      _aValue = Nothing,
      _aTargetType = Nothing,
      _aName = pName_
    }

-- | The ID of the target. You can specify the short form ID for a resource or the full Amazon Resource Name (ARN).
aTargetId :: Lens' Attribute (Maybe Text)
aTargetId = lens _aTargetId (\s a -> s {_aTargetId = a})

-- | The value of the attribute. The @value@ must contain between 1 and 128 characters and may contain letters (uppercase and lowercase), numbers, hyphens, underscores, periods, at signs (@), forward slashes, back slashes, colons, or spaces. The value cannot contain any leading or trailing whitespace.
aValue :: Lens' Attribute (Maybe Text)
aValue = lens _aValue (\s a -> s {_aValue = a})

-- | The type of the target with which to attach the attribute. This parameter is required if you use the short form ID for a resource instead of the full ARN.
aTargetType :: Lens' Attribute (Maybe TargetType)
aTargetType = lens _aTargetType (\s a -> s {_aTargetType = a})

-- | The name of the attribute. The @name@ must contain between 1 and 128 characters and name may contain letters (uppercase and lowercase), numbers, hyphens, underscores, forward slashes, back slashes, or periods.
aName :: Lens' Attribute Text
aName = lens _aName (\s a -> s {_aName = a})

instance FromJSON Attribute where
  parseJSON =
    withObject
      "Attribute"
      ( \x ->
          Attribute'
            <$> (x .:? "targetId")
            <*> (x .:? "value")
            <*> (x .:? "targetType")
            <*> (x .: "name")
      )

instance Hashable Attribute

instance NFData Attribute

instance ToJSON Attribute where
  toJSON Attribute' {..} =
    object
      ( catMaybes
          [ ("targetId" .=) <$> _aTargetId,
            ("value" .=) <$> _aValue,
            ("targetType" .=) <$> _aTargetType,
            Just ("name" .= _aName)
          ]
      )
