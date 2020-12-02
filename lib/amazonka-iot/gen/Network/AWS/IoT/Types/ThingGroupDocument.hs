{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupDocument where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The thing group search index document.
--
--
--
-- /See:/ 'thingGroupDocument' smart constructor.
data ThingGroupDocument = ThingGroupDocument'
  { _tgdParentGroupNames ::
      !(Maybe [Text]),
    _tgdThingGroupId :: !(Maybe Text),
    _tgdThingGroupName :: !(Maybe Text),
    _tgdAttributes :: !(Maybe (Map Text (Text))),
    _tgdThingGroupDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThingGroupDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgdParentGroupNames' - Parent group names.
--
-- * 'tgdThingGroupId' - The thing group ID.
--
-- * 'tgdThingGroupName' - The thing group name.
--
-- * 'tgdAttributes' - The thing group attributes.
--
-- * 'tgdThingGroupDescription' - The thing group description.
thingGroupDocument ::
  ThingGroupDocument
thingGroupDocument =
  ThingGroupDocument'
    { _tgdParentGroupNames = Nothing,
      _tgdThingGroupId = Nothing,
      _tgdThingGroupName = Nothing,
      _tgdAttributes = Nothing,
      _tgdThingGroupDescription = Nothing
    }

-- | Parent group names.
tgdParentGroupNames :: Lens' ThingGroupDocument [Text]
tgdParentGroupNames = lens _tgdParentGroupNames (\s a -> s {_tgdParentGroupNames = a}) . _Default . _Coerce

-- | The thing group ID.
tgdThingGroupId :: Lens' ThingGroupDocument (Maybe Text)
tgdThingGroupId = lens _tgdThingGroupId (\s a -> s {_tgdThingGroupId = a})

-- | The thing group name.
tgdThingGroupName :: Lens' ThingGroupDocument (Maybe Text)
tgdThingGroupName = lens _tgdThingGroupName (\s a -> s {_tgdThingGroupName = a})

-- | The thing group attributes.
tgdAttributes :: Lens' ThingGroupDocument (HashMap Text (Text))
tgdAttributes = lens _tgdAttributes (\s a -> s {_tgdAttributes = a}) . _Default . _Map

-- | The thing group description.
tgdThingGroupDescription :: Lens' ThingGroupDocument (Maybe Text)
tgdThingGroupDescription = lens _tgdThingGroupDescription (\s a -> s {_tgdThingGroupDescription = a})

instance FromJSON ThingGroupDocument where
  parseJSON =
    withObject
      "ThingGroupDocument"
      ( \x ->
          ThingGroupDocument'
            <$> (x .:? "parentGroupNames" .!= mempty)
            <*> (x .:? "thingGroupId")
            <*> (x .:? "thingGroupName")
            <*> (x .:? "attributes" .!= mempty)
            <*> (x .:? "thingGroupDescription")
      )

instance Hashable ThingGroupDocument

instance NFData ThingGroupDocument
