{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.DiscoveredResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.DiscoveredResource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Object representing the on-premises resource being migrated.
--
--
--
-- /See:/ 'discoveredResource' smart constructor.
data DiscoveredResource = DiscoveredResource'
  { _drDescription ::
      !(Maybe Text),
    _drConfigurationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DiscoveredResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drDescription' - A description that can be free-form text to record additional detail about the discovered resource for clarity or later reference.
--
-- * 'drConfigurationId' - The configurationId in Application Discovery Service that uniquely identifies the on-premise resource.
discoveredResource ::
  -- | 'drConfigurationId'
  Text ->
  DiscoveredResource
discoveredResource pConfigurationId_ =
  DiscoveredResource'
    { _drDescription = Nothing,
      _drConfigurationId = pConfigurationId_
    }

-- | A description that can be free-form text to record additional detail about the discovered resource for clarity or later reference.
drDescription :: Lens' DiscoveredResource (Maybe Text)
drDescription = lens _drDescription (\s a -> s {_drDescription = a})

-- | The configurationId in Application Discovery Service that uniquely identifies the on-premise resource.
drConfigurationId :: Lens' DiscoveredResource Text
drConfigurationId = lens _drConfigurationId (\s a -> s {_drConfigurationId = a})

instance FromJSON DiscoveredResource where
  parseJSON =
    withObject
      "DiscoveredResource"
      ( \x ->
          DiscoveredResource'
            <$> (x .:? "Description") <*> (x .: "ConfigurationId")
      )

instance Hashable DiscoveredResource

instance NFData DiscoveredResource

instance ToJSON DiscoveredResource where
  toJSON DiscoveredResource' {..} =
    object
      ( catMaybes
          [ ("Description" .=) <$> _drDescription,
            Just ("ConfigurationId" .= _drConfigurationId)
          ]
      )
