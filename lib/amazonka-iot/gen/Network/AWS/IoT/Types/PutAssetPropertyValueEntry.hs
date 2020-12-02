{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PutAssetPropertyValueEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PutAssetPropertyValueEntry where

import Network.AWS.IoT.Types.AssetPropertyValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An asset property value entry containing the following information.
--
--
--
-- /See:/ 'putAssetPropertyValueEntry' smart constructor.
data PutAssetPropertyValueEntry = PutAssetPropertyValueEntry'
  { _papveEntryId ::
      !(Maybe Text),
    _papvePropertyAlias :: !(Maybe Text),
    _papvePropertyId :: !(Maybe Text),
    _papveAssetId :: !(Maybe Text),
    _papvePropertyValues ::
      !(List1 AssetPropertyValue)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutAssetPropertyValueEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'papveEntryId' - Optional. A unique identifier for this entry that you can define to better track which message caused an error in case of failure. Accepts substitution templates. Defaults to a new UUID.
--
-- * 'papvePropertyAlias' - The name of the property alias associated with your asset property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
--
-- * 'papvePropertyId' - The ID of the asset's property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
--
-- * 'papveAssetId' - The ID of the AWS IoT SiteWise asset. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
--
-- * 'papvePropertyValues' - A list of property values to insert that each contain timestamp, quality, and value (TQV) information.
putAssetPropertyValueEntry ::
  -- | 'papvePropertyValues'
  NonEmpty AssetPropertyValue ->
  PutAssetPropertyValueEntry
putAssetPropertyValueEntry pPropertyValues_ =
  PutAssetPropertyValueEntry'
    { _papveEntryId = Nothing,
      _papvePropertyAlias = Nothing,
      _papvePropertyId = Nothing,
      _papveAssetId = Nothing,
      _papvePropertyValues = _List1 # pPropertyValues_
    }

-- | Optional. A unique identifier for this entry that you can define to better track which message caused an error in case of failure. Accepts substitution templates. Defaults to a new UUID.
papveEntryId :: Lens' PutAssetPropertyValueEntry (Maybe Text)
papveEntryId = lens _papveEntryId (\s a -> s {_papveEntryId = a})

-- | The name of the property alias associated with your asset property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
papvePropertyAlias :: Lens' PutAssetPropertyValueEntry (Maybe Text)
papvePropertyAlias = lens _papvePropertyAlias (\s a -> s {_papvePropertyAlias = a})

-- | The ID of the asset's property. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
papvePropertyId :: Lens' PutAssetPropertyValueEntry (Maybe Text)
papvePropertyId = lens _papvePropertyId (\s a -> s {_papvePropertyId = a})

-- | The ID of the AWS IoT SiteWise asset. You must specify either a @propertyAlias@ or both an @aliasId@ and a @propertyId@ . Accepts substitution templates.
papveAssetId :: Lens' PutAssetPropertyValueEntry (Maybe Text)
papveAssetId = lens _papveAssetId (\s a -> s {_papveAssetId = a})

-- | A list of property values to insert that each contain timestamp, quality, and value (TQV) information.
papvePropertyValues :: Lens' PutAssetPropertyValueEntry (NonEmpty AssetPropertyValue)
papvePropertyValues = lens _papvePropertyValues (\s a -> s {_papvePropertyValues = a}) . _List1

instance FromJSON PutAssetPropertyValueEntry where
  parseJSON =
    withObject
      "PutAssetPropertyValueEntry"
      ( \x ->
          PutAssetPropertyValueEntry'
            <$> (x .:? "entryId")
            <*> (x .:? "propertyAlias")
            <*> (x .:? "propertyId")
            <*> (x .:? "assetId")
            <*> (x .: "propertyValues")
      )

instance Hashable PutAssetPropertyValueEntry

instance NFData PutAssetPropertyValueEntry

instance ToJSON PutAssetPropertyValueEntry where
  toJSON PutAssetPropertyValueEntry' {..} =
    object
      ( catMaybes
          [ ("entryId" .=) <$> _papveEntryId,
            ("propertyAlias" .=) <$> _papvePropertyAlias,
            ("propertyId" .=) <$> _papvePropertyId,
            ("assetId" .=) <$> _papveAssetId,
            Just ("propertyValues" .= _papvePropertyValues)
          ]
      )
