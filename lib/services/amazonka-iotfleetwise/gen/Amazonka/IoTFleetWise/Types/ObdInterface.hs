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
-- Module      : Amazonka.IoTFleetWise.Types.ObdInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.ObdInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A network interface that specifies the On-board diagnostic (OBD) II
-- network protocol.
--
-- /See:/ 'newObdInterface' smart constructor.
data ObdInterface = ObdInterface'
  { -- | The maximum number message requests per second.
    pidRequestIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The standard OBD II PID.
    obdStandard :: Prelude.Maybe Prelude.Text,
    -- | Whether the vehicle has a transmission control module (TCM).
    hasTransmissionEcu :: Prelude.Maybe Prelude.Bool,
    -- | Whether to use extended IDs in the message.
    useExtendedIds :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number message requests per diagnostic trouble code per
    -- second.
    dtcRequestIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The name of the interface.
    name :: Prelude.Text,
    -- | The ID of the message requesting vehicle data.
    requestMessageId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObdInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pidRequestIntervalSeconds', 'obdInterface_pidRequestIntervalSeconds' - The maximum number message requests per second.
--
-- 'obdStandard', 'obdInterface_obdStandard' - The standard OBD II PID.
--
-- 'hasTransmissionEcu', 'obdInterface_hasTransmissionEcu' - Whether the vehicle has a transmission control module (TCM).
--
-- 'useExtendedIds', 'obdInterface_useExtendedIds' - Whether to use extended IDs in the message.
--
-- 'dtcRequestIntervalSeconds', 'obdInterface_dtcRequestIntervalSeconds' - The maximum number message requests per diagnostic trouble code per
-- second.
--
-- 'name', 'obdInterface_name' - The name of the interface.
--
-- 'requestMessageId', 'obdInterface_requestMessageId' - The ID of the message requesting vehicle data.
newObdInterface ::
  -- | 'name'
  Prelude.Text ->
  -- | 'requestMessageId'
  Prelude.Natural ->
  ObdInterface
newObdInterface pName_ pRequestMessageId_ =
  ObdInterface'
    { pidRequestIntervalSeconds =
        Prelude.Nothing,
      obdStandard = Prelude.Nothing,
      hasTransmissionEcu = Prelude.Nothing,
      useExtendedIds = Prelude.Nothing,
      dtcRequestIntervalSeconds = Prelude.Nothing,
      name = pName_,
      requestMessageId = pRequestMessageId_
    }

-- | The maximum number message requests per second.
obdInterface_pidRequestIntervalSeconds :: Lens.Lens' ObdInterface (Prelude.Maybe Prelude.Natural)
obdInterface_pidRequestIntervalSeconds = Lens.lens (\ObdInterface' {pidRequestIntervalSeconds} -> pidRequestIntervalSeconds) (\s@ObdInterface' {} a -> s {pidRequestIntervalSeconds = a} :: ObdInterface)

-- | The standard OBD II PID.
obdInterface_obdStandard :: Lens.Lens' ObdInterface (Prelude.Maybe Prelude.Text)
obdInterface_obdStandard = Lens.lens (\ObdInterface' {obdStandard} -> obdStandard) (\s@ObdInterface' {} a -> s {obdStandard = a} :: ObdInterface)

-- | Whether the vehicle has a transmission control module (TCM).
obdInterface_hasTransmissionEcu :: Lens.Lens' ObdInterface (Prelude.Maybe Prelude.Bool)
obdInterface_hasTransmissionEcu = Lens.lens (\ObdInterface' {hasTransmissionEcu} -> hasTransmissionEcu) (\s@ObdInterface' {} a -> s {hasTransmissionEcu = a} :: ObdInterface)

-- | Whether to use extended IDs in the message.
obdInterface_useExtendedIds :: Lens.Lens' ObdInterface (Prelude.Maybe Prelude.Bool)
obdInterface_useExtendedIds = Lens.lens (\ObdInterface' {useExtendedIds} -> useExtendedIds) (\s@ObdInterface' {} a -> s {useExtendedIds = a} :: ObdInterface)

-- | The maximum number message requests per diagnostic trouble code per
-- second.
obdInterface_dtcRequestIntervalSeconds :: Lens.Lens' ObdInterface (Prelude.Maybe Prelude.Natural)
obdInterface_dtcRequestIntervalSeconds = Lens.lens (\ObdInterface' {dtcRequestIntervalSeconds} -> dtcRequestIntervalSeconds) (\s@ObdInterface' {} a -> s {dtcRequestIntervalSeconds = a} :: ObdInterface)

-- | The name of the interface.
obdInterface_name :: Lens.Lens' ObdInterface Prelude.Text
obdInterface_name = Lens.lens (\ObdInterface' {name} -> name) (\s@ObdInterface' {} a -> s {name = a} :: ObdInterface)

-- | The ID of the message requesting vehicle data.
obdInterface_requestMessageId :: Lens.Lens' ObdInterface Prelude.Natural
obdInterface_requestMessageId = Lens.lens (\ObdInterface' {requestMessageId} -> requestMessageId) (\s@ObdInterface' {} a -> s {requestMessageId = a} :: ObdInterface)

instance Core.FromJSON ObdInterface where
  parseJSON =
    Core.withObject
      "ObdInterface"
      ( \x ->
          ObdInterface'
            Prelude.<$> (x Core..:? "pidRequestIntervalSeconds")
            Prelude.<*> (x Core..:? "obdStandard")
            Prelude.<*> (x Core..:? "hasTransmissionEcu")
            Prelude.<*> (x Core..:? "useExtendedIds")
            Prelude.<*> (x Core..:? "dtcRequestIntervalSeconds")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "requestMessageId")
      )

instance Prelude.Hashable ObdInterface where
  hashWithSalt _salt ObdInterface' {..} =
    _salt
      `Prelude.hashWithSalt` pidRequestIntervalSeconds
      `Prelude.hashWithSalt` obdStandard
      `Prelude.hashWithSalt` hasTransmissionEcu
      `Prelude.hashWithSalt` useExtendedIds
      `Prelude.hashWithSalt` dtcRequestIntervalSeconds
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` requestMessageId

instance Prelude.NFData ObdInterface where
  rnf ObdInterface' {..} =
    Prelude.rnf pidRequestIntervalSeconds
      `Prelude.seq` Prelude.rnf obdStandard
      `Prelude.seq` Prelude.rnf hasTransmissionEcu
      `Prelude.seq` Prelude.rnf useExtendedIds
      `Prelude.seq` Prelude.rnf dtcRequestIntervalSeconds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestMessageId

instance Core.ToJSON ObdInterface where
  toJSON ObdInterface' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("pidRequestIntervalSeconds" Core..=)
              Prelude.<$> pidRequestIntervalSeconds,
            ("obdStandard" Core..=) Prelude.<$> obdStandard,
            ("hasTransmissionEcu" Core..=)
              Prelude.<$> hasTransmissionEcu,
            ("useExtendedIds" Core..=)
              Prelude.<$> useExtendedIds,
            ("dtcRequestIntervalSeconds" Core..=)
              Prelude.<$> dtcRequestIntervalSeconds,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("requestMessageId" Core..= requestMessageId)
          ]
      )
