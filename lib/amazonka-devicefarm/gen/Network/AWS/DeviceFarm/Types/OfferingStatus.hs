{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingStatus where

import Network.AWS.DeviceFarm.Types.Offering
import Network.AWS.DeviceFarm.Types.OfferingTransactionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of the offering.
--
--
--
-- /See:/ 'offeringStatus' smart constructor.
data OfferingStatus = OfferingStatus'
  { _osEffectiveOn ::
      !(Maybe POSIX),
    _osOffering :: !(Maybe Offering),
    _osQuantity :: !(Maybe Int),
    _osType :: !(Maybe OfferingTransactionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OfferingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osEffectiveOn' - The date on which the offering is effective.
--
-- * 'osOffering' - Represents the metadata of an offering status.
--
-- * 'osQuantity' - The number of available devices in the offering.
--
-- * 'osType' - The type specified for the offering status.
offeringStatus ::
  OfferingStatus
offeringStatus =
  OfferingStatus'
    { _osEffectiveOn = Nothing,
      _osOffering = Nothing,
      _osQuantity = Nothing,
      _osType = Nothing
    }

-- | The date on which the offering is effective.
osEffectiveOn :: Lens' OfferingStatus (Maybe UTCTime)
osEffectiveOn = lens _osEffectiveOn (\s a -> s {_osEffectiveOn = a}) . mapping _Time

-- | Represents the metadata of an offering status.
osOffering :: Lens' OfferingStatus (Maybe Offering)
osOffering = lens _osOffering (\s a -> s {_osOffering = a})

-- | The number of available devices in the offering.
osQuantity :: Lens' OfferingStatus (Maybe Int)
osQuantity = lens _osQuantity (\s a -> s {_osQuantity = a})

-- | The type specified for the offering status.
osType :: Lens' OfferingStatus (Maybe OfferingTransactionType)
osType = lens _osType (\s a -> s {_osType = a})

instance FromJSON OfferingStatus where
  parseJSON =
    withObject
      "OfferingStatus"
      ( \x ->
          OfferingStatus'
            <$> (x .:? "effectiveOn")
            <*> (x .:? "offering")
            <*> (x .:? "quantity")
            <*> (x .:? "type")
      )

instance Hashable OfferingStatus

instance NFData OfferingStatus
