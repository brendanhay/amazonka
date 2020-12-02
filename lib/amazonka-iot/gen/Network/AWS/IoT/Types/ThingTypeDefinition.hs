{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingTypeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeDefinition where

import Network.AWS.IoT.Types.ThingTypeMetadata
import Network.AWS.IoT.Types.ThingTypeProperties
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The definition of the thing type, including thing type name and description.
--
--
--
-- /See:/ 'thingTypeDefinition' smart constructor.
data ThingTypeDefinition = ThingTypeDefinition'
  { _ttdThingTypeProperties ::
      !(Maybe ThingTypeProperties),
    _ttdThingTypeName :: !(Maybe Text),
    _ttdThingTypeMetadata :: !(Maybe ThingTypeMetadata),
    _ttdThingTypeARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThingTypeDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttdThingTypeProperties' - The ThingTypeProperties for the thing type.
--
-- * 'ttdThingTypeName' - The name of the thing type.
--
-- * 'ttdThingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- * 'ttdThingTypeARN' - The thing type ARN.
thingTypeDefinition ::
  ThingTypeDefinition
thingTypeDefinition =
  ThingTypeDefinition'
    { _ttdThingTypeProperties = Nothing,
      _ttdThingTypeName = Nothing,
      _ttdThingTypeMetadata = Nothing,
      _ttdThingTypeARN = Nothing
    }

-- | The ThingTypeProperties for the thing type.
ttdThingTypeProperties :: Lens' ThingTypeDefinition (Maybe ThingTypeProperties)
ttdThingTypeProperties = lens _ttdThingTypeProperties (\s a -> s {_ttdThingTypeProperties = a})

-- | The name of the thing type.
ttdThingTypeName :: Lens' ThingTypeDefinition (Maybe Text)
ttdThingTypeName = lens _ttdThingTypeName (\s a -> s {_ttdThingTypeName = a})

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
ttdThingTypeMetadata :: Lens' ThingTypeDefinition (Maybe ThingTypeMetadata)
ttdThingTypeMetadata = lens _ttdThingTypeMetadata (\s a -> s {_ttdThingTypeMetadata = a})

-- | The thing type ARN.
ttdThingTypeARN :: Lens' ThingTypeDefinition (Maybe Text)
ttdThingTypeARN = lens _ttdThingTypeARN (\s a -> s {_ttdThingTypeARN = a})

instance FromJSON ThingTypeDefinition where
  parseJSON =
    withObject
      "ThingTypeDefinition"
      ( \x ->
          ThingTypeDefinition'
            <$> (x .:? "thingTypeProperties")
            <*> (x .:? "thingTypeName")
            <*> (x .:? "thingTypeMetadata")
            <*> (x .:? "thingTypeArn")
      )

instance Hashable ThingTypeDefinition

instance NFData ThingTypeDefinition
