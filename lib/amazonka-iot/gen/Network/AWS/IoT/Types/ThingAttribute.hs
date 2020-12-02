{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The properties of the thing, including thing name, thing type name, and a list of thing attributes.
--
--
--
-- /See:/ 'thingAttribute' smart constructor.
data ThingAttribute = ThingAttribute'
  { _taThingTypeName ::
      !(Maybe Text),
    _taThingARN :: !(Maybe Text),
    _taAttributes :: !(Maybe (Map Text (Text))),
    _taVersion :: !(Maybe Integer),
    _taThingName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThingAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'taThingTypeName' - The name of the thing type, if the thing has been associated with a type.
--
-- * 'taThingARN' - The thing ARN.
--
-- * 'taAttributes' - A list of thing attributes which are name-value pairs.
--
-- * 'taVersion' - The version of the thing record in the registry.
--
-- * 'taThingName' - The name of the thing.
thingAttribute ::
  ThingAttribute
thingAttribute =
  ThingAttribute'
    { _taThingTypeName = Nothing,
      _taThingARN = Nothing,
      _taAttributes = Nothing,
      _taVersion = Nothing,
      _taThingName = Nothing
    }

-- | The name of the thing type, if the thing has been associated with a type.
taThingTypeName :: Lens' ThingAttribute (Maybe Text)
taThingTypeName = lens _taThingTypeName (\s a -> s {_taThingTypeName = a})

-- | The thing ARN.
taThingARN :: Lens' ThingAttribute (Maybe Text)
taThingARN = lens _taThingARN (\s a -> s {_taThingARN = a})

-- | A list of thing attributes which are name-value pairs.
taAttributes :: Lens' ThingAttribute (HashMap Text (Text))
taAttributes = lens _taAttributes (\s a -> s {_taAttributes = a}) . _Default . _Map

-- | The version of the thing record in the registry.
taVersion :: Lens' ThingAttribute (Maybe Integer)
taVersion = lens _taVersion (\s a -> s {_taVersion = a})

-- | The name of the thing.
taThingName :: Lens' ThingAttribute (Maybe Text)
taThingName = lens _taThingName (\s a -> s {_taThingName = a})

instance FromJSON ThingAttribute where
  parseJSON =
    withObject
      "ThingAttribute"
      ( \x ->
          ThingAttribute'
            <$> (x .:? "thingTypeName")
            <*> (x .:? "thingArn")
            <*> (x .:? "attributes" .!= mempty)
            <*> (x .:? "version")
            <*> (x .:? "thingName")
      )

instance Hashable ThingAttribute

instance NFData ThingAttribute
