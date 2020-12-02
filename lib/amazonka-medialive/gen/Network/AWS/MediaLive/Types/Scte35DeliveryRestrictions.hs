{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
import Network.AWS.MediaLive.Types.Scte35DeviceRestrictions
import Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
import Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
import Network.AWS.Prelude

-- | Corresponds to SCTE-35 delivery_not_restricted_flag parameter. To declare delivery restrictions, include this element and its four "restriction" flags. To declare that there are no restrictions, omit this element.
--
-- /See:/ 'scte35DeliveryRestrictions' smart constructor.
data Scte35DeliveryRestrictions = Scte35DeliveryRestrictions'
  { _sdrDeviceRestrictions ::
      !Scte35DeviceRestrictions,
    _sdrArchiveAllowedFlag ::
      !Scte35ArchiveAllowedFlag,
    _sdrWebDeliveryAllowedFlag ::
      !Scte35WebDeliveryAllowedFlag,
    _sdrNoRegionalBlackoutFlag ::
      !Scte35NoRegionalBlackoutFlag
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35DeliveryRestrictions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdrDeviceRestrictions' - Corresponds to SCTE-35 device_restrictions parameter.
--
-- * 'sdrArchiveAllowedFlag' - Corresponds to SCTE-35 archive_allowed_flag.
--
-- * 'sdrWebDeliveryAllowedFlag' - Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
--
-- * 'sdrNoRegionalBlackoutFlag' - Corresponds to SCTE-35 no_regional_blackout_flag parameter.
scte35DeliveryRestrictions ::
  -- | 'sdrDeviceRestrictions'
  Scte35DeviceRestrictions ->
  -- | 'sdrArchiveAllowedFlag'
  Scte35ArchiveAllowedFlag ->
  -- | 'sdrWebDeliveryAllowedFlag'
  Scte35WebDeliveryAllowedFlag ->
  -- | 'sdrNoRegionalBlackoutFlag'
  Scte35NoRegionalBlackoutFlag ->
  Scte35DeliveryRestrictions
scte35DeliveryRestrictions
  pDeviceRestrictions_
  pArchiveAllowedFlag_
  pWebDeliveryAllowedFlag_
  pNoRegionalBlackoutFlag_ =
    Scte35DeliveryRestrictions'
      { _sdrDeviceRestrictions =
          pDeviceRestrictions_,
        _sdrArchiveAllowedFlag = pArchiveAllowedFlag_,
        _sdrWebDeliveryAllowedFlag = pWebDeliveryAllowedFlag_,
        _sdrNoRegionalBlackoutFlag = pNoRegionalBlackoutFlag_
      }

-- | Corresponds to SCTE-35 device_restrictions parameter.
sdrDeviceRestrictions :: Lens' Scte35DeliveryRestrictions Scte35DeviceRestrictions
sdrDeviceRestrictions = lens _sdrDeviceRestrictions (\s a -> s {_sdrDeviceRestrictions = a})

-- | Corresponds to SCTE-35 archive_allowed_flag.
sdrArchiveAllowedFlag :: Lens' Scte35DeliveryRestrictions Scte35ArchiveAllowedFlag
sdrArchiveAllowedFlag = lens _sdrArchiveAllowedFlag (\s a -> s {_sdrArchiveAllowedFlag = a})

-- | Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
sdrWebDeliveryAllowedFlag :: Lens' Scte35DeliveryRestrictions Scte35WebDeliveryAllowedFlag
sdrWebDeliveryAllowedFlag = lens _sdrWebDeliveryAllowedFlag (\s a -> s {_sdrWebDeliveryAllowedFlag = a})

-- | Corresponds to SCTE-35 no_regional_blackout_flag parameter.
sdrNoRegionalBlackoutFlag :: Lens' Scte35DeliveryRestrictions Scte35NoRegionalBlackoutFlag
sdrNoRegionalBlackoutFlag = lens _sdrNoRegionalBlackoutFlag (\s a -> s {_sdrNoRegionalBlackoutFlag = a})

instance FromJSON Scte35DeliveryRestrictions where
  parseJSON =
    withObject
      "Scte35DeliveryRestrictions"
      ( \x ->
          Scte35DeliveryRestrictions'
            <$> (x .: "deviceRestrictions")
            <*> (x .: "archiveAllowedFlag")
            <*> (x .: "webDeliveryAllowedFlag")
            <*> (x .: "noRegionalBlackoutFlag")
      )

instance Hashable Scte35DeliveryRestrictions

instance NFData Scte35DeliveryRestrictions

instance ToJSON Scte35DeliveryRestrictions where
  toJSON Scte35DeliveryRestrictions' {..} =
    object
      ( catMaybes
          [ Just ("deviceRestrictions" .= _sdrDeviceRestrictions),
            Just ("archiveAllowedFlag" .= _sdrArchiveAllowedFlag),
            Just ("webDeliveryAllowedFlag" .= _sdrWebDeliveryAllowedFlag),
            Just ("noRegionalBlackoutFlag" .= _sdrNoRegionalBlackoutFlag)
          ]
      )
