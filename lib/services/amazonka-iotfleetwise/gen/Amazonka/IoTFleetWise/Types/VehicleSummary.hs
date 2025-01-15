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
-- Module      : Amazonka.IoTFleetWise.Types.VehicleSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.VehicleSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a vehicle.
--
-- To return this information about vehicles in your account, you can use
-- the API operation.
--
-- /See:/ 'newVehicleSummary' smart constructor.
data VehicleSummary = VehicleSummary'
  { -- | The unique ID of the vehicle.
    vehicleName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the vehicle.
    arn :: Prelude.Text,
    -- | The ARN of a vehicle model (model manifest) associated with the vehicle.
    modelManifestArn :: Prelude.Text,
    -- | The ARN of a decoder manifest associated with the vehicle.
    decoderManifestArn :: Prelude.Text,
    -- | The time the vehicle was created in seconds since epoch (January 1, 1970
    -- at midnight UTC time).
    creationTime :: Data.POSIX,
    -- | The time the vehicle was last updated in seconds since epoch (January 1,
    -- 1970 at midnight UTC time).
    lastModificationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VehicleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vehicleName', 'vehicleSummary_vehicleName' - The unique ID of the vehicle.
--
-- 'arn', 'vehicleSummary_arn' - The Amazon Resource Name (ARN) of the vehicle.
--
-- 'modelManifestArn', 'vehicleSummary_modelManifestArn' - The ARN of a vehicle model (model manifest) associated with the vehicle.
--
-- 'decoderManifestArn', 'vehicleSummary_decoderManifestArn' - The ARN of a decoder manifest associated with the vehicle.
--
-- 'creationTime', 'vehicleSummary_creationTime' - The time the vehicle was created in seconds since epoch (January 1, 1970
-- at midnight UTC time).
--
-- 'lastModificationTime', 'vehicleSummary_lastModificationTime' - The time the vehicle was last updated in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
newVehicleSummary ::
  -- | 'vehicleName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'modelManifestArn'
  Prelude.Text ->
  -- | 'decoderManifestArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModificationTime'
  Prelude.UTCTime ->
  VehicleSummary
newVehicleSummary
  pVehicleName_
  pArn_
  pModelManifestArn_
  pDecoderManifestArn_
  pCreationTime_
  pLastModificationTime_ =
    VehicleSummary'
      { vehicleName = pVehicleName_,
        arn = pArn_,
        modelManifestArn = pModelManifestArn_,
        decoderManifestArn = pDecoderManifestArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModificationTime =
          Data._Time Lens.# pLastModificationTime_
      }

-- | The unique ID of the vehicle.
vehicleSummary_vehicleName :: Lens.Lens' VehicleSummary Prelude.Text
vehicleSummary_vehicleName = Lens.lens (\VehicleSummary' {vehicleName} -> vehicleName) (\s@VehicleSummary' {} a -> s {vehicleName = a} :: VehicleSummary)

-- | The Amazon Resource Name (ARN) of the vehicle.
vehicleSummary_arn :: Lens.Lens' VehicleSummary Prelude.Text
vehicleSummary_arn = Lens.lens (\VehicleSummary' {arn} -> arn) (\s@VehicleSummary' {} a -> s {arn = a} :: VehicleSummary)

-- | The ARN of a vehicle model (model manifest) associated with the vehicle.
vehicleSummary_modelManifestArn :: Lens.Lens' VehicleSummary Prelude.Text
vehicleSummary_modelManifestArn = Lens.lens (\VehicleSummary' {modelManifestArn} -> modelManifestArn) (\s@VehicleSummary' {} a -> s {modelManifestArn = a} :: VehicleSummary)

-- | The ARN of a decoder manifest associated with the vehicle.
vehicleSummary_decoderManifestArn :: Lens.Lens' VehicleSummary Prelude.Text
vehicleSummary_decoderManifestArn = Lens.lens (\VehicleSummary' {decoderManifestArn} -> decoderManifestArn) (\s@VehicleSummary' {} a -> s {decoderManifestArn = a} :: VehicleSummary)

-- | The time the vehicle was created in seconds since epoch (January 1, 1970
-- at midnight UTC time).
vehicleSummary_creationTime :: Lens.Lens' VehicleSummary Prelude.UTCTime
vehicleSummary_creationTime = Lens.lens (\VehicleSummary' {creationTime} -> creationTime) (\s@VehicleSummary' {} a -> s {creationTime = a} :: VehicleSummary) Prelude.. Data._Time

-- | The time the vehicle was last updated in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
vehicleSummary_lastModificationTime :: Lens.Lens' VehicleSummary Prelude.UTCTime
vehicleSummary_lastModificationTime = Lens.lens (\VehicleSummary' {lastModificationTime} -> lastModificationTime) (\s@VehicleSummary' {} a -> s {lastModificationTime = a} :: VehicleSummary) Prelude.. Data._Time

instance Data.FromJSON VehicleSummary where
  parseJSON =
    Data.withObject
      "VehicleSummary"
      ( \x ->
          VehicleSummary'
            Prelude.<$> (x Data..: "vehicleName")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "modelManifestArn")
            Prelude.<*> (x Data..: "decoderManifestArn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "lastModificationTime")
      )

instance Prelude.Hashable VehicleSummary where
  hashWithSalt _salt VehicleSummary' {..} =
    _salt
      `Prelude.hashWithSalt` vehicleName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` modelManifestArn
      `Prelude.hashWithSalt` decoderManifestArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModificationTime

instance Prelude.NFData VehicleSummary where
  rnf VehicleSummary' {..} =
    Prelude.rnf vehicleName `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf modelManifestArn `Prelude.seq`
          Prelude.rnf decoderManifestArn `Prelude.seq`
            Prelude.rnf creationTime `Prelude.seq`
              Prelude.rnf lastModificationTime
