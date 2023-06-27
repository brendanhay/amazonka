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
-- Module      : Amazonka.Snowball.Types.S3OnDeviceServiceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.S3OnDeviceServiceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.StorageUnit

-- | Amazon S3 compatible storage on Snow family devices configuration items.
--
-- /See:/ 'newS3OnDeviceServiceConfiguration' smart constructor.
data S3OnDeviceServiceConfiguration = S3OnDeviceServiceConfiguration'
  { -- | >Fault tolerance level of the cluster. This indicates the number of
    -- nodes that can go down without degrading the performance of the cluster.
    -- This additional input helps when the specified @StorageLimit@ matches
    -- more than one Amazon S3 compatible storage on Snow family devices
    -- service configuration.
    faultTolerance :: Prelude.Maybe Prelude.Natural,
    -- | Applicable when creating a cluster. Specifies how many nodes are needed
    -- for Amazon S3 compatible storage on Snow family devices. If specified,
    -- the other input can be omitted.
    serviceSize :: Prelude.Maybe Prelude.Natural,
    -- | If the specified storage limit value matches storage limit of one of the
    -- defined configurations, that configuration will be used. If the
    -- specified storage limit value does not match any defined configuration,
    -- the request will fail. If more than one configuration has the same
    -- storage limit as specified, the other input need to be provided.
    storageLimit :: Prelude.Maybe Prelude.Double,
    -- | Storage unit. Currently the only supported unit is TB.
    storageUnit :: Prelude.Maybe StorageUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3OnDeviceServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faultTolerance', 's3OnDeviceServiceConfiguration_faultTolerance' - >Fault tolerance level of the cluster. This indicates the number of
-- nodes that can go down without degrading the performance of the cluster.
-- This additional input helps when the specified @StorageLimit@ matches
-- more than one Amazon S3 compatible storage on Snow family devices
-- service configuration.
--
-- 'serviceSize', 's3OnDeviceServiceConfiguration_serviceSize' - Applicable when creating a cluster. Specifies how many nodes are needed
-- for Amazon S3 compatible storage on Snow family devices. If specified,
-- the other input can be omitted.
--
-- 'storageLimit', 's3OnDeviceServiceConfiguration_storageLimit' - If the specified storage limit value matches storage limit of one of the
-- defined configurations, that configuration will be used. If the
-- specified storage limit value does not match any defined configuration,
-- the request will fail. If more than one configuration has the same
-- storage limit as specified, the other input need to be provided.
--
-- 'storageUnit', 's3OnDeviceServiceConfiguration_storageUnit' - Storage unit. Currently the only supported unit is TB.
newS3OnDeviceServiceConfiguration ::
  S3OnDeviceServiceConfiguration
newS3OnDeviceServiceConfiguration =
  S3OnDeviceServiceConfiguration'
    { faultTolerance =
        Prelude.Nothing,
      serviceSize = Prelude.Nothing,
      storageLimit = Prelude.Nothing,
      storageUnit = Prelude.Nothing
    }

-- | >Fault tolerance level of the cluster. This indicates the number of
-- nodes that can go down without degrading the performance of the cluster.
-- This additional input helps when the specified @StorageLimit@ matches
-- more than one Amazon S3 compatible storage on Snow family devices
-- service configuration.
s3OnDeviceServiceConfiguration_faultTolerance :: Lens.Lens' S3OnDeviceServiceConfiguration (Prelude.Maybe Prelude.Natural)
s3OnDeviceServiceConfiguration_faultTolerance = Lens.lens (\S3OnDeviceServiceConfiguration' {faultTolerance} -> faultTolerance) (\s@S3OnDeviceServiceConfiguration' {} a -> s {faultTolerance = a} :: S3OnDeviceServiceConfiguration)

-- | Applicable when creating a cluster. Specifies how many nodes are needed
-- for Amazon S3 compatible storage on Snow family devices. If specified,
-- the other input can be omitted.
s3OnDeviceServiceConfiguration_serviceSize :: Lens.Lens' S3OnDeviceServiceConfiguration (Prelude.Maybe Prelude.Natural)
s3OnDeviceServiceConfiguration_serviceSize = Lens.lens (\S3OnDeviceServiceConfiguration' {serviceSize} -> serviceSize) (\s@S3OnDeviceServiceConfiguration' {} a -> s {serviceSize = a} :: S3OnDeviceServiceConfiguration)

-- | If the specified storage limit value matches storage limit of one of the
-- defined configurations, that configuration will be used. If the
-- specified storage limit value does not match any defined configuration,
-- the request will fail. If more than one configuration has the same
-- storage limit as specified, the other input need to be provided.
s3OnDeviceServiceConfiguration_storageLimit :: Lens.Lens' S3OnDeviceServiceConfiguration (Prelude.Maybe Prelude.Double)
s3OnDeviceServiceConfiguration_storageLimit = Lens.lens (\S3OnDeviceServiceConfiguration' {storageLimit} -> storageLimit) (\s@S3OnDeviceServiceConfiguration' {} a -> s {storageLimit = a} :: S3OnDeviceServiceConfiguration)

-- | Storage unit. Currently the only supported unit is TB.
s3OnDeviceServiceConfiguration_storageUnit :: Lens.Lens' S3OnDeviceServiceConfiguration (Prelude.Maybe StorageUnit)
s3OnDeviceServiceConfiguration_storageUnit = Lens.lens (\S3OnDeviceServiceConfiguration' {storageUnit} -> storageUnit) (\s@S3OnDeviceServiceConfiguration' {} a -> s {storageUnit = a} :: S3OnDeviceServiceConfiguration)

instance Data.FromJSON S3OnDeviceServiceConfiguration where
  parseJSON =
    Data.withObject
      "S3OnDeviceServiceConfiguration"
      ( \x ->
          S3OnDeviceServiceConfiguration'
            Prelude.<$> (x Data..:? "FaultTolerance")
            Prelude.<*> (x Data..:? "ServiceSize")
            Prelude.<*> (x Data..:? "StorageLimit")
            Prelude.<*> (x Data..:? "StorageUnit")
      )

instance
  Prelude.Hashable
    S3OnDeviceServiceConfiguration
  where
  hashWithSalt
    _salt
    S3OnDeviceServiceConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` faultTolerance
        `Prelude.hashWithSalt` serviceSize
        `Prelude.hashWithSalt` storageLimit
        `Prelude.hashWithSalt` storageUnit

instance
  Prelude.NFData
    S3OnDeviceServiceConfiguration
  where
  rnf S3OnDeviceServiceConfiguration' {..} =
    Prelude.rnf faultTolerance
      `Prelude.seq` Prelude.rnf serviceSize
      `Prelude.seq` Prelude.rnf storageLimit
      `Prelude.seq` Prelude.rnf storageUnit

instance Data.ToJSON S3OnDeviceServiceConfiguration where
  toJSON S3OnDeviceServiceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FaultTolerance" Data..=)
              Prelude.<$> faultTolerance,
            ("ServiceSize" Data..=) Prelude.<$> serviceSize,
            ("StorageLimit" Data..=) Prelude.<$> storageLimit,
            ("StorageUnit" Data..=) Prelude.<$> storageUnit
          ]
      )
