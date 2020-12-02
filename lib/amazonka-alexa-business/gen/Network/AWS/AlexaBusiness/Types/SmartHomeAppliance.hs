{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SmartHomeAppliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SmartHomeAppliance where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A smart home appliance that can connect to a central system. Any domestic device can be a smart appliance.
--
--
--
-- /See:/ 'smartHomeAppliance' smart constructor.
data SmartHomeAppliance = SmartHomeAppliance'
  { _shaFriendlyName ::
      !(Maybe Text),
    _shaManufacturerName :: !(Maybe Text),
    _shaDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SmartHomeAppliance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'shaFriendlyName' - The friendly name of the smart home appliance.
--
-- * 'shaManufacturerName' - The name of the manufacturer of the smart home appliance.
--
-- * 'shaDescription' - The description of the smart home appliance.
smartHomeAppliance ::
  SmartHomeAppliance
smartHomeAppliance =
  SmartHomeAppliance'
    { _shaFriendlyName = Nothing,
      _shaManufacturerName = Nothing,
      _shaDescription = Nothing
    }

-- | The friendly name of the smart home appliance.
shaFriendlyName :: Lens' SmartHomeAppliance (Maybe Text)
shaFriendlyName = lens _shaFriendlyName (\s a -> s {_shaFriendlyName = a})

-- | The name of the manufacturer of the smart home appliance.
shaManufacturerName :: Lens' SmartHomeAppliance (Maybe Text)
shaManufacturerName = lens _shaManufacturerName (\s a -> s {_shaManufacturerName = a})

-- | The description of the smart home appliance.
shaDescription :: Lens' SmartHomeAppliance (Maybe Text)
shaDescription = lens _shaDescription (\s a -> s {_shaDescription = a})

instance FromJSON SmartHomeAppliance where
  parseJSON =
    withObject
      "SmartHomeAppliance"
      ( \x ->
          SmartHomeAppliance'
            <$> (x .:? "FriendlyName")
            <*> (x .:? "ManufacturerName")
            <*> (x .:? "Description")
      )

instance Hashable SmartHomeAppliance

instance NFData SmartHomeAppliance
