{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingTypeProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.
--
--
--
-- /See:/ 'thingTypeProperties' smart constructor.
data ThingTypeProperties = ThingTypeProperties'
  { _ttpSearchableAttributes ::
      !(Maybe [Text]),
    _ttpThingTypeDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThingTypeProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttpSearchableAttributes' - A list of searchable thing attribute names.
--
-- * 'ttpThingTypeDescription' - The description of the thing type.
thingTypeProperties ::
  ThingTypeProperties
thingTypeProperties =
  ThingTypeProperties'
    { _ttpSearchableAttributes = Nothing,
      _ttpThingTypeDescription = Nothing
    }

-- | A list of searchable thing attribute names.
ttpSearchableAttributes :: Lens' ThingTypeProperties [Text]
ttpSearchableAttributes = lens _ttpSearchableAttributes (\s a -> s {_ttpSearchableAttributes = a}) . _Default . _Coerce

-- | The description of the thing type.
ttpThingTypeDescription :: Lens' ThingTypeProperties (Maybe Text)
ttpThingTypeDescription = lens _ttpThingTypeDescription (\s a -> s {_ttpThingTypeDescription = a})

instance FromJSON ThingTypeProperties where
  parseJSON =
    withObject
      "ThingTypeProperties"
      ( \x ->
          ThingTypeProperties'
            <$> (x .:? "searchableAttributes" .!= mempty)
            <*> (x .:? "thingTypeDescription")
      )

instance Hashable ThingTypeProperties

instance NFData ThingTypeProperties

instance ToJSON ThingTypeProperties where
  toJSON ThingTypeProperties' {..} =
    object
      ( catMaybes
          [ ("searchableAttributes" .=) <$> _ttpSearchableAttributes,
            ("thingTypeDescription" .=) <$> _ttpThingTypeDescription
          ]
      )
