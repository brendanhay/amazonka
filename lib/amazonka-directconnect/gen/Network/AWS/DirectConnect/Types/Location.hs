{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Location where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an AWS Direct Connect location.
--
--
--
-- /See:/ 'location' smart constructor.
data Location = Location'
  { _lAvailablePortSpeeds :: !(Maybe [Text]),
    _lLocationName :: !(Maybe Text),
    _lLocationCode :: !(Maybe Text),
    _lRegion :: !(Maybe Text),
    _lAvailableProviders :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lAvailablePortSpeeds' - The available port speeds for the location.
--
-- * 'lLocationName' - The name of the location. This includes the name of the colocation partner and the physical site of the building.
--
-- * 'lLocationCode' - The code for the location.
--
-- * 'lRegion' - The AWS Region for the location.
--
-- * 'lAvailableProviders' - The name of the service provider for the location.
location ::
  Location
location =
  Location'
    { _lAvailablePortSpeeds = Nothing,
      _lLocationName = Nothing,
      _lLocationCode = Nothing,
      _lRegion = Nothing,
      _lAvailableProviders = Nothing
    }

-- | The available port speeds for the location.
lAvailablePortSpeeds :: Lens' Location [Text]
lAvailablePortSpeeds = lens _lAvailablePortSpeeds (\s a -> s {_lAvailablePortSpeeds = a}) . _Default . _Coerce

-- | The name of the location. This includes the name of the colocation partner and the physical site of the building.
lLocationName :: Lens' Location (Maybe Text)
lLocationName = lens _lLocationName (\s a -> s {_lLocationName = a})

-- | The code for the location.
lLocationCode :: Lens' Location (Maybe Text)
lLocationCode = lens _lLocationCode (\s a -> s {_lLocationCode = a})

-- | The AWS Region for the location.
lRegion :: Lens' Location (Maybe Text)
lRegion = lens _lRegion (\s a -> s {_lRegion = a})

-- | The name of the service provider for the location.
lAvailableProviders :: Lens' Location [Text]
lAvailableProviders = lens _lAvailableProviders (\s a -> s {_lAvailableProviders = a}) . _Default . _Coerce

instance FromJSON Location where
  parseJSON =
    withObject
      "Location"
      ( \x ->
          Location'
            <$> (x .:? "availablePortSpeeds" .!= mempty)
            <*> (x .:? "locationName")
            <*> (x .:? "locationCode")
            <*> (x .:? "region")
            <*> (x .:? "availableProviders" .!= mempty)
      )

instance Hashable Location

instance NFData Location
