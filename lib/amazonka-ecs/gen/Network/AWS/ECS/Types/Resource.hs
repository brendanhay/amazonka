{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Resource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the resources available for a container instance.
--
--
--
-- /See:/ 'resource' smart constructor.
data Resource = Resource'
  { _rStringSetValue :: !(Maybe [Text]),
    _rIntegerValue :: !(Maybe Int),
    _rDoubleValue :: !(Maybe Double),
    _rLongValue :: !(Maybe Integer),
    _rName :: !(Maybe Text),
    _rType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rStringSetValue' - When the @stringSetValue@ type is set, the value of the resource must be a string type.
--
-- * 'rIntegerValue' - When the @integerValue@ type is set, the value of the resource must be an integer.
--
-- * 'rDoubleValue' - When the @doubleValue@ type is set, the value of the resource must be a double precision floating-point type.
--
-- * 'rLongValue' - When the @longValue@ type is set, the value of the resource must be an extended precision floating-point type.
--
-- * 'rName' - The name of the resource, such as @CPU@ , @MEMORY@ , @PORTS@ , @PORTS_UDP@ , or a user-defined resource.
--
-- * 'rType' - The type of the resource, such as @INTEGER@ , @DOUBLE@ , @LONG@ , or @STRINGSET@ .
resource ::
  Resource
resource =
  Resource'
    { _rStringSetValue = Nothing,
      _rIntegerValue = Nothing,
      _rDoubleValue = Nothing,
      _rLongValue = Nothing,
      _rName = Nothing,
      _rType = Nothing
    }

-- | When the @stringSetValue@ type is set, the value of the resource must be a string type.
rStringSetValue :: Lens' Resource [Text]
rStringSetValue = lens _rStringSetValue (\s a -> s {_rStringSetValue = a}) . _Default . _Coerce

-- | When the @integerValue@ type is set, the value of the resource must be an integer.
rIntegerValue :: Lens' Resource (Maybe Int)
rIntegerValue = lens _rIntegerValue (\s a -> s {_rIntegerValue = a})

-- | When the @doubleValue@ type is set, the value of the resource must be a double precision floating-point type.
rDoubleValue :: Lens' Resource (Maybe Double)
rDoubleValue = lens _rDoubleValue (\s a -> s {_rDoubleValue = a})

-- | When the @longValue@ type is set, the value of the resource must be an extended precision floating-point type.
rLongValue :: Lens' Resource (Maybe Integer)
rLongValue = lens _rLongValue (\s a -> s {_rLongValue = a})

-- | The name of the resource, such as @CPU@ , @MEMORY@ , @PORTS@ , @PORTS_UDP@ , or a user-defined resource.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\s a -> s {_rName = a})

-- | The type of the resource, such as @INTEGER@ , @DOUBLE@ , @LONG@ , or @STRINGSET@ .
rType :: Lens' Resource (Maybe Text)
rType = lens _rType (\s a -> s {_rType = a})

instance FromJSON Resource where
  parseJSON =
    withObject
      "Resource"
      ( \x ->
          Resource'
            <$> (x .:? "stringSetValue" .!= mempty)
            <*> (x .:? "integerValue")
            <*> (x .:? "doubleValue")
            <*> (x .:? "longValue")
            <*> (x .:? "name")
            <*> (x .:? "type")
      )

instance Hashable Resource

instance NFData Resource

instance ToJSON Resource where
  toJSON Resource' {..} =
    object
      ( catMaybes
          [ ("stringSetValue" .=) <$> _rStringSetValue,
            ("integerValue" .=) <$> _rIntegerValue,
            ("doubleValue" .=) <$> _rDoubleValue,
            ("longValue" .=) <$> _rLongValue,
            ("name" .=) <$> _rName,
            ("type" .=) <$> _rType
          ]
      )
