{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupIndexingConfiguration where

import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.ThingGroupIndexingMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Thing group indexing configuration.
--
--
--
-- /See:/ 'thingGroupIndexingConfiguration' smart constructor.
data ThingGroupIndexingConfiguration = ThingGroupIndexingConfiguration'
  { _tgicManagedFields ::
      !(Maybe [Field]),
    _tgicCustomFields ::
      !(Maybe [Field]),
    _tgicThingGroupIndexingMode ::
      !ThingGroupIndexingMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ThingGroupIndexingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgicManagedFields' - Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
--
-- * 'tgicCustomFields' - A list of thing group fields to index. This list cannot contain any managed fields. Use the GetIndexingConfiguration API to get a list of managed fields. Contains custom field names and their data type.
--
-- * 'tgicThingGroupIndexingMode' - Thing group indexing mode.
thingGroupIndexingConfiguration ::
  -- | 'tgicThingGroupIndexingMode'
  ThingGroupIndexingMode ->
  ThingGroupIndexingConfiguration
thingGroupIndexingConfiguration pThingGroupIndexingMode_ =
  ThingGroupIndexingConfiguration'
    { _tgicManagedFields = Nothing,
      _tgicCustomFields = Nothing,
      _tgicThingGroupIndexingMode = pThingGroupIndexingMode_
    }

-- | Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
tgicManagedFields :: Lens' ThingGroupIndexingConfiguration [Field]
tgicManagedFields = lens _tgicManagedFields (\s a -> s {_tgicManagedFields = a}) . _Default . _Coerce

-- | A list of thing group fields to index. This list cannot contain any managed fields. Use the GetIndexingConfiguration API to get a list of managed fields. Contains custom field names and their data type.
tgicCustomFields :: Lens' ThingGroupIndexingConfiguration [Field]
tgicCustomFields = lens _tgicCustomFields (\s a -> s {_tgicCustomFields = a}) . _Default . _Coerce

-- | Thing group indexing mode.
tgicThingGroupIndexingMode :: Lens' ThingGroupIndexingConfiguration ThingGroupIndexingMode
tgicThingGroupIndexingMode = lens _tgicThingGroupIndexingMode (\s a -> s {_tgicThingGroupIndexingMode = a})

instance FromJSON ThingGroupIndexingConfiguration where
  parseJSON =
    withObject
      "ThingGroupIndexingConfiguration"
      ( \x ->
          ThingGroupIndexingConfiguration'
            <$> (x .:? "managedFields" .!= mempty)
            <*> (x .:? "customFields" .!= mempty)
            <*> (x .: "thingGroupIndexingMode")
      )

instance Hashable ThingGroupIndexingConfiguration

instance NFData ThingGroupIndexingConfiguration

instance ToJSON ThingGroupIndexingConfiguration where
  toJSON ThingGroupIndexingConfiguration' {..} =
    object
      ( catMaybes
          [ ("managedFields" .=) <$> _tgicManagedFields,
            ("customFields" .=) <$> _tgicCustomFields,
            Just ("thingGroupIndexingMode" .= _tgicThingGroupIndexingMode)
          ]
      )
