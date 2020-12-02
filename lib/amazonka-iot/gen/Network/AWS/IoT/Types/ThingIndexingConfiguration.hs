{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingIndexingConfiguration where

import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.ThingConnectivityIndexingMode
import Network.AWS.IoT.Types.ThingIndexingMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The thing indexing configuration. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/managing-index.html Managing Thing Indexing> .
--
--
--
-- /See:/ 'thingIndexingConfiguration' smart constructor.
data ThingIndexingConfiguration = ThingIndexingConfiguration'
  { _ticManagedFields ::
      !(Maybe [Field]),
    _ticThingConnectivityIndexingMode ::
      !( Maybe
           ThingConnectivityIndexingMode
       ),
    _ticCustomFields :: !(Maybe [Field]),
    _ticThingIndexingMode ::
      !ThingIndexingMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThingIndexingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ticManagedFields' - Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
--
-- * 'ticThingConnectivityIndexingMode' - Thing connectivity indexing mode. Valid values are:      * STATUS – Your thing index contains connectivity status. To enable thing connectivity indexing, thingIndexMode must not be set to OFF.     * OFF - Thing connectivity status indexing is disabled.
--
-- * 'ticCustomFields' - Contains custom field names and their data type.
--
-- * 'ticThingIndexingMode' - Thing indexing mode. Valid values are:     * REGISTRY – Your thing index contains registry data only.     * REGISTRY_AND_SHADOW - Your thing index contains registry and shadow data.     * OFF - Thing indexing is disabled.
thingIndexingConfiguration ::
  -- | 'ticThingIndexingMode'
  ThingIndexingMode ->
  ThingIndexingConfiguration
thingIndexingConfiguration pThingIndexingMode_ =
  ThingIndexingConfiguration'
    { _ticManagedFields = Nothing,
      _ticThingConnectivityIndexingMode = Nothing,
      _ticCustomFields = Nothing,
      _ticThingIndexingMode = pThingIndexingMode_
    }

-- | Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
ticManagedFields :: Lens' ThingIndexingConfiguration [Field]
ticManagedFields = lens _ticManagedFields (\s a -> s {_ticManagedFields = a}) . _Default . _Coerce

-- | Thing connectivity indexing mode. Valid values are:      * STATUS – Your thing index contains connectivity status. To enable thing connectivity indexing, thingIndexMode must not be set to OFF.     * OFF - Thing connectivity status indexing is disabled.
ticThingConnectivityIndexingMode :: Lens' ThingIndexingConfiguration (Maybe ThingConnectivityIndexingMode)
ticThingConnectivityIndexingMode = lens _ticThingConnectivityIndexingMode (\s a -> s {_ticThingConnectivityIndexingMode = a})

-- | Contains custom field names and their data type.
ticCustomFields :: Lens' ThingIndexingConfiguration [Field]
ticCustomFields = lens _ticCustomFields (\s a -> s {_ticCustomFields = a}) . _Default . _Coerce

-- | Thing indexing mode. Valid values are:     * REGISTRY – Your thing index contains registry data only.     * REGISTRY_AND_SHADOW - Your thing index contains registry and shadow data.     * OFF - Thing indexing is disabled.
ticThingIndexingMode :: Lens' ThingIndexingConfiguration ThingIndexingMode
ticThingIndexingMode = lens _ticThingIndexingMode (\s a -> s {_ticThingIndexingMode = a})

instance FromJSON ThingIndexingConfiguration where
  parseJSON =
    withObject
      "ThingIndexingConfiguration"
      ( \x ->
          ThingIndexingConfiguration'
            <$> (x .:? "managedFields" .!= mempty)
            <*> (x .:? "thingConnectivityIndexingMode")
            <*> (x .:? "customFields" .!= mempty)
            <*> (x .: "thingIndexingMode")
      )

instance Hashable ThingIndexingConfiguration

instance NFData ThingIndexingConfiguration

instance ToJSON ThingIndexingConfiguration where
  toJSON ThingIndexingConfiguration' {..} =
    object
      ( catMaybes
          [ ("managedFields" .=) <$> _ticManagedFields,
            ("thingConnectivityIndexingMode" .=)
              <$> _ticThingConnectivityIndexingMode,
            ("customFields" .=) <$> _ticCustomFields,
            Just ("thingIndexingMode" .= _ticThingIndexingMode)
          ]
      )
