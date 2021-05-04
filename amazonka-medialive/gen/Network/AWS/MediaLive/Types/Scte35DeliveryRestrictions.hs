{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35DeliveryRestrictions where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
import Network.AWS.MediaLive.Types.Scte35DeviceRestrictions
import Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
import Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
import qualified Network.AWS.Prelude as Prelude

-- | Corresponds to SCTE-35 delivery_not_restricted_flag parameter. To
-- declare delivery restrictions, include this element and its four
-- \"restriction\" flags. To declare that there are no restrictions, omit
-- this element.
--
-- /See:/ 'newScte35DeliveryRestrictions' smart constructor.
data Scte35DeliveryRestrictions = Scte35DeliveryRestrictions'
  { -- | Corresponds to SCTE-35 device_restrictions parameter.
    deviceRestrictions :: Scte35DeviceRestrictions,
    -- | Corresponds to SCTE-35 archive_allowed_flag.
    archiveAllowedFlag :: Scte35ArchiveAllowedFlag,
    -- | Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
    webDeliveryAllowedFlag :: Scte35WebDeliveryAllowedFlag,
    -- | Corresponds to SCTE-35 no_regional_blackout_flag parameter.
    noRegionalBlackoutFlag :: Scte35NoRegionalBlackoutFlag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Scte35DeliveryRestrictions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceRestrictions', 'scte35DeliveryRestrictions_deviceRestrictions' - Corresponds to SCTE-35 device_restrictions parameter.
--
-- 'archiveAllowedFlag', 'scte35DeliveryRestrictions_archiveAllowedFlag' - Corresponds to SCTE-35 archive_allowed_flag.
--
-- 'webDeliveryAllowedFlag', 'scte35DeliveryRestrictions_webDeliveryAllowedFlag' - Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
--
-- 'noRegionalBlackoutFlag', 'scte35DeliveryRestrictions_noRegionalBlackoutFlag' - Corresponds to SCTE-35 no_regional_blackout_flag parameter.
newScte35DeliveryRestrictions ::
  -- | 'deviceRestrictions'
  Scte35DeviceRestrictions ->
  -- | 'archiveAllowedFlag'
  Scte35ArchiveAllowedFlag ->
  -- | 'webDeliveryAllowedFlag'
  Scte35WebDeliveryAllowedFlag ->
  -- | 'noRegionalBlackoutFlag'
  Scte35NoRegionalBlackoutFlag ->
  Scte35DeliveryRestrictions
newScte35DeliveryRestrictions
  pDeviceRestrictions_
  pArchiveAllowedFlag_
  pWebDeliveryAllowedFlag_
  pNoRegionalBlackoutFlag_ =
    Scte35DeliveryRestrictions'
      { deviceRestrictions =
          pDeviceRestrictions_,
        archiveAllowedFlag = pArchiveAllowedFlag_,
        webDeliveryAllowedFlag =
          pWebDeliveryAllowedFlag_,
        noRegionalBlackoutFlag =
          pNoRegionalBlackoutFlag_
      }

-- | Corresponds to SCTE-35 device_restrictions parameter.
scte35DeliveryRestrictions_deviceRestrictions :: Lens.Lens' Scte35DeliveryRestrictions Scte35DeviceRestrictions
scte35DeliveryRestrictions_deviceRestrictions = Lens.lens (\Scte35DeliveryRestrictions' {deviceRestrictions} -> deviceRestrictions) (\s@Scte35DeliveryRestrictions' {} a -> s {deviceRestrictions = a} :: Scte35DeliveryRestrictions)

-- | Corresponds to SCTE-35 archive_allowed_flag.
scte35DeliveryRestrictions_archiveAllowedFlag :: Lens.Lens' Scte35DeliveryRestrictions Scte35ArchiveAllowedFlag
scte35DeliveryRestrictions_archiveAllowedFlag = Lens.lens (\Scte35DeliveryRestrictions' {archiveAllowedFlag} -> archiveAllowedFlag) (\s@Scte35DeliveryRestrictions' {} a -> s {archiveAllowedFlag = a} :: Scte35DeliveryRestrictions)

-- | Corresponds to SCTE-35 web_delivery_allowed_flag parameter.
scte35DeliveryRestrictions_webDeliveryAllowedFlag :: Lens.Lens' Scte35DeliveryRestrictions Scte35WebDeliveryAllowedFlag
scte35DeliveryRestrictions_webDeliveryAllowedFlag = Lens.lens (\Scte35DeliveryRestrictions' {webDeliveryAllowedFlag} -> webDeliveryAllowedFlag) (\s@Scte35DeliveryRestrictions' {} a -> s {webDeliveryAllowedFlag = a} :: Scte35DeliveryRestrictions)

-- | Corresponds to SCTE-35 no_regional_blackout_flag parameter.
scte35DeliveryRestrictions_noRegionalBlackoutFlag :: Lens.Lens' Scte35DeliveryRestrictions Scte35NoRegionalBlackoutFlag
scte35DeliveryRestrictions_noRegionalBlackoutFlag = Lens.lens (\Scte35DeliveryRestrictions' {noRegionalBlackoutFlag} -> noRegionalBlackoutFlag) (\s@Scte35DeliveryRestrictions' {} a -> s {noRegionalBlackoutFlag = a} :: Scte35DeliveryRestrictions)

instance Prelude.FromJSON Scte35DeliveryRestrictions where
  parseJSON =
    Prelude.withObject
      "Scte35DeliveryRestrictions"
      ( \x ->
          Scte35DeliveryRestrictions'
            Prelude.<$> (x Prelude..: "deviceRestrictions")
            Prelude.<*> (x Prelude..: "archiveAllowedFlag")
            Prelude.<*> (x Prelude..: "webDeliveryAllowedFlag")
            Prelude.<*> (x Prelude..: "noRegionalBlackoutFlag")
      )

instance Prelude.Hashable Scte35DeliveryRestrictions

instance Prelude.NFData Scte35DeliveryRestrictions

instance Prelude.ToJSON Scte35DeliveryRestrictions where
  toJSON Scte35DeliveryRestrictions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("deviceRestrictions" Prelude..= deviceRestrictions),
            Prelude.Just
              ("archiveAllowedFlag" Prelude..= archiveAllowedFlag),
            Prelude.Just
              ( "webDeliveryAllowedFlag"
                  Prelude..= webDeliveryAllowedFlag
              ),
            Prelude.Just
              ( "noRegionalBlackoutFlag"
                  Prelude..= noRegionalBlackoutFlag
              )
          ]
      )
