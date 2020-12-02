{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingDocument where

import Network.AWS.IoT.Types.ThingConnectivity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The thing search index document.
--
--
--
-- /See:/ 'thingDocument' smart constructor.
data ThingDocument = ThingDocument'
  { _tdThingGroupNames ::
      !(Maybe [Text]),
    _tdThingTypeName :: !(Maybe Text),
    _tdShadow :: !(Maybe Text),
    _tdAttributes :: !(Maybe (Map Text (Text))),
    _tdConnectivity :: !(Maybe ThingConnectivity),
    _tdThingName :: !(Maybe Text),
    _tdThingId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThingDocument' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdThingGroupNames' - Thing group names.
--
-- * 'tdThingTypeName' - The thing type name.
--
-- * 'tdShadow' - The shadow.
--
-- * 'tdAttributes' - The attributes.
--
-- * 'tdConnectivity' - Indicates whether the thing is connected to the AWS IoT service.
--
-- * 'tdThingName' - The thing name.
--
-- * 'tdThingId' - The thing ID.
thingDocument ::
  ThingDocument
thingDocument =
  ThingDocument'
    { _tdThingGroupNames = Nothing,
      _tdThingTypeName = Nothing,
      _tdShadow = Nothing,
      _tdAttributes = Nothing,
      _tdConnectivity = Nothing,
      _tdThingName = Nothing,
      _tdThingId = Nothing
    }

-- | Thing group names.
tdThingGroupNames :: Lens' ThingDocument [Text]
tdThingGroupNames = lens _tdThingGroupNames (\s a -> s {_tdThingGroupNames = a}) . _Default . _Coerce

-- | The thing type name.
tdThingTypeName :: Lens' ThingDocument (Maybe Text)
tdThingTypeName = lens _tdThingTypeName (\s a -> s {_tdThingTypeName = a})

-- | The shadow.
tdShadow :: Lens' ThingDocument (Maybe Text)
tdShadow = lens _tdShadow (\s a -> s {_tdShadow = a})

-- | The attributes.
tdAttributes :: Lens' ThingDocument (HashMap Text (Text))
tdAttributes = lens _tdAttributes (\s a -> s {_tdAttributes = a}) . _Default . _Map

-- | Indicates whether the thing is connected to the AWS IoT service.
tdConnectivity :: Lens' ThingDocument (Maybe ThingConnectivity)
tdConnectivity = lens _tdConnectivity (\s a -> s {_tdConnectivity = a})

-- | The thing name.
tdThingName :: Lens' ThingDocument (Maybe Text)
tdThingName = lens _tdThingName (\s a -> s {_tdThingName = a})

-- | The thing ID.
tdThingId :: Lens' ThingDocument (Maybe Text)
tdThingId = lens _tdThingId (\s a -> s {_tdThingId = a})

instance FromJSON ThingDocument where
  parseJSON =
    withObject
      "ThingDocument"
      ( \x ->
          ThingDocument'
            <$> (x .:? "thingGroupNames" .!= mempty)
            <*> (x .:? "thingTypeName")
            <*> (x .:? "shadow")
            <*> (x .:? "attributes" .!= mempty)
            <*> (x .:? "connectivity")
            <*> (x .:? "thingName")
            <*> (x .:? "thingId")
      )

instance Hashable ThingDocument

instance NFData ThingDocument
