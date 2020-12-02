{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Offering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Offering where

import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.OfferingType
import Network.AWS.DeviceFarm.Types.RecurringCharge
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the metadata of a device offering.
--
--
--
-- /See:/ 'offering' smart constructor.
data Offering = Offering'
  { _oPlatform :: !(Maybe DevicePlatform),
    _oId :: !(Maybe Text),
    _oRecurringCharges :: !(Maybe [RecurringCharge]),
    _oType :: !(Maybe OfferingType),
    _oDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Offering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oPlatform' - The platform of the device (for example, @ANDROID@ or @IOS@ ).
--
-- * 'oId' - The ID that corresponds to a device offering.
--
-- * 'oRecurringCharges' - Specifies whether there are recurring charges for the offering.
--
-- * 'oType' - The type of offering (for example, @RECURRING@ ) for a device.
--
-- * 'oDescription' - A string that describes the offering.
offering ::
  Offering
offering =
  Offering'
    { _oPlatform = Nothing,
      _oId = Nothing,
      _oRecurringCharges = Nothing,
      _oType = Nothing,
      _oDescription = Nothing
    }

-- | The platform of the device (for example, @ANDROID@ or @IOS@ ).
oPlatform :: Lens' Offering (Maybe DevicePlatform)
oPlatform = lens _oPlatform (\s a -> s {_oPlatform = a})

-- | The ID that corresponds to a device offering.
oId :: Lens' Offering (Maybe Text)
oId = lens _oId (\s a -> s {_oId = a})

-- | Specifies whether there are recurring charges for the offering.
oRecurringCharges :: Lens' Offering [RecurringCharge]
oRecurringCharges = lens _oRecurringCharges (\s a -> s {_oRecurringCharges = a}) . _Default . _Coerce

-- | The type of offering (for example, @RECURRING@ ) for a device.
oType :: Lens' Offering (Maybe OfferingType)
oType = lens _oType (\s a -> s {_oType = a})

-- | A string that describes the offering.
oDescription :: Lens' Offering (Maybe Text)
oDescription = lens _oDescription (\s a -> s {_oDescription = a})

instance FromJSON Offering where
  parseJSON =
    withObject
      "Offering"
      ( \x ->
          Offering'
            <$> (x .:? "platform")
            <*> (x .:? "id")
            <*> (x .:? "recurringCharges" .!= mempty)
            <*> (x .:? "type")
            <*> (x .:? "description")
      )

instance Hashable Offering

instance NFData Offering
