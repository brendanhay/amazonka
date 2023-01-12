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
-- Module      : Amazonka.MediaLive.Types.Scte35DeliveryRestrictions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35DeliveryRestrictions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.Scte35ArchiveAllowedFlag
import Amazonka.MediaLive.Types.Scte35DeviceRestrictions
import Amazonka.MediaLive.Types.Scte35NoRegionalBlackoutFlag
import Amazonka.MediaLive.Types.Scte35WebDeliveryAllowedFlag
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON Scte35DeliveryRestrictions where
  parseJSON =
    Data.withObject
      "Scte35DeliveryRestrictions"
      ( \x ->
          Scte35DeliveryRestrictions'
            Prelude.<$> (x Data..: "deviceRestrictions")
            Prelude.<*> (x Data..: "archiveAllowedFlag")
            Prelude.<*> (x Data..: "webDeliveryAllowedFlag")
            Prelude.<*> (x Data..: "noRegionalBlackoutFlag")
      )

instance Prelude.Hashable Scte35DeliveryRestrictions where
  hashWithSalt _salt Scte35DeliveryRestrictions' {..} =
    _salt `Prelude.hashWithSalt` deviceRestrictions
      `Prelude.hashWithSalt` archiveAllowedFlag
      `Prelude.hashWithSalt` webDeliveryAllowedFlag
      `Prelude.hashWithSalt` noRegionalBlackoutFlag

instance Prelude.NFData Scte35DeliveryRestrictions where
  rnf Scte35DeliveryRestrictions' {..} =
    Prelude.rnf deviceRestrictions
      `Prelude.seq` Prelude.rnf archiveAllowedFlag
      `Prelude.seq` Prelude.rnf webDeliveryAllowedFlag
      `Prelude.seq` Prelude.rnf noRegionalBlackoutFlag

instance Data.ToJSON Scte35DeliveryRestrictions where
  toJSON Scte35DeliveryRestrictions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("deviceRestrictions" Data..= deviceRestrictions),
            Prelude.Just
              ("archiveAllowedFlag" Data..= archiveAllowedFlag),
            Prelude.Just
              ( "webDeliveryAllowedFlag"
                  Data..= webDeliveryAllowedFlag
              ),
            Prelude.Just
              ( "noRegionalBlackoutFlag"
                  Data..= noRegionalBlackoutFlag
              )
          ]
      )
