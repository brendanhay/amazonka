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
-- Module      : Amazonka.HealthLake.Types.DatastoreProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.DatastoreProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HealthLake.Types.DatastoreStatus
import Amazonka.HealthLake.Types.FHIRVersion
import Amazonka.HealthLake.Types.PreloadDataConfig
import Amazonka.HealthLake.Types.SseConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Displays the properties of the Data Store, including the ID, Arn, name,
-- and the status of the Data Store.
--
-- /See:/ 'newDatastoreProperties' smart constructor.
data DatastoreProperties = DatastoreProperties'
  { -- | The user-generated name for the Data Store.
    datastoreName :: Prelude.Maybe Prelude.Text,
    -- | The server-side encryption key configuration for a customer provided
    -- encryption key (CMK).
    sseConfiguration :: Prelude.Maybe SseConfiguration,
    -- | The preloaded data configuration for the Data Store. Only data preloaded
    -- from Synthea is supported.
    preloadDataConfig :: Prelude.Maybe PreloadDataConfig,
    -- | The time that a Data Store was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The AWS-generated ID number for the Data Store.
    datastoreId :: Prelude.Text,
    -- | The Amazon Resource Name used in the creation of the Data Store.
    datastoreArn :: Prelude.Text,
    -- | The status of the Data Store. Possible statuses are \'CREATING\',
    -- \'ACTIVE\', \'DELETING\', or \'DELETED\'.
    datastoreStatus :: DatastoreStatus,
    -- | The FHIR version. Only R4 version data is supported.
    datastoreTypeVersion :: FHIRVersion,
    -- | The AWS endpoint for the Data Store. Each Data Store will have it\'s own
    -- endpoint with Data Store ID in the endpoint URL.
    datastoreEndpoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastoreProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastoreName', 'datastoreProperties_datastoreName' - The user-generated name for the Data Store.
--
-- 'sseConfiguration', 'datastoreProperties_sseConfiguration' - The server-side encryption key configuration for a customer provided
-- encryption key (CMK).
--
-- 'preloadDataConfig', 'datastoreProperties_preloadDataConfig' - The preloaded data configuration for the Data Store. Only data preloaded
-- from Synthea is supported.
--
-- 'createdAt', 'datastoreProperties_createdAt' - The time that a Data Store was created.
--
-- 'datastoreId', 'datastoreProperties_datastoreId' - The AWS-generated ID number for the Data Store.
--
-- 'datastoreArn', 'datastoreProperties_datastoreArn' - The Amazon Resource Name used in the creation of the Data Store.
--
-- 'datastoreStatus', 'datastoreProperties_datastoreStatus' - The status of the Data Store. Possible statuses are \'CREATING\',
-- \'ACTIVE\', \'DELETING\', or \'DELETED\'.
--
-- 'datastoreTypeVersion', 'datastoreProperties_datastoreTypeVersion' - The FHIR version. Only R4 version data is supported.
--
-- 'datastoreEndpoint', 'datastoreProperties_datastoreEndpoint' - The AWS endpoint for the Data Store. Each Data Store will have it\'s own
-- endpoint with Data Store ID in the endpoint URL.
newDatastoreProperties ::
  -- | 'datastoreId'
  Prelude.Text ->
  -- | 'datastoreArn'
  Prelude.Text ->
  -- | 'datastoreStatus'
  DatastoreStatus ->
  -- | 'datastoreTypeVersion'
  FHIRVersion ->
  -- | 'datastoreEndpoint'
  Prelude.Text ->
  DatastoreProperties
newDatastoreProperties
  pDatastoreId_
  pDatastoreArn_
  pDatastoreStatus_
  pDatastoreTypeVersion_
  pDatastoreEndpoint_ =
    DatastoreProperties'
      { datastoreName =
          Prelude.Nothing,
        sseConfiguration = Prelude.Nothing,
        preloadDataConfig = Prelude.Nothing,
        createdAt = Prelude.Nothing,
        datastoreId = pDatastoreId_,
        datastoreArn = pDatastoreArn_,
        datastoreStatus = pDatastoreStatus_,
        datastoreTypeVersion = pDatastoreTypeVersion_,
        datastoreEndpoint = pDatastoreEndpoint_
      }

-- | The user-generated name for the Data Store.
datastoreProperties_datastoreName :: Lens.Lens' DatastoreProperties (Prelude.Maybe Prelude.Text)
datastoreProperties_datastoreName = Lens.lens (\DatastoreProperties' {datastoreName} -> datastoreName) (\s@DatastoreProperties' {} a -> s {datastoreName = a} :: DatastoreProperties)

-- | The server-side encryption key configuration for a customer provided
-- encryption key (CMK).
datastoreProperties_sseConfiguration :: Lens.Lens' DatastoreProperties (Prelude.Maybe SseConfiguration)
datastoreProperties_sseConfiguration = Lens.lens (\DatastoreProperties' {sseConfiguration} -> sseConfiguration) (\s@DatastoreProperties' {} a -> s {sseConfiguration = a} :: DatastoreProperties)

-- | The preloaded data configuration for the Data Store. Only data preloaded
-- from Synthea is supported.
datastoreProperties_preloadDataConfig :: Lens.Lens' DatastoreProperties (Prelude.Maybe PreloadDataConfig)
datastoreProperties_preloadDataConfig = Lens.lens (\DatastoreProperties' {preloadDataConfig} -> preloadDataConfig) (\s@DatastoreProperties' {} a -> s {preloadDataConfig = a} :: DatastoreProperties)

-- | The time that a Data Store was created.
datastoreProperties_createdAt :: Lens.Lens' DatastoreProperties (Prelude.Maybe Prelude.UTCTime)
datastoreProperties_createdAt = Lens.lens (\DatastoreProperties' {createdAt} -> createdAt) (\s@DatastoreProperties' {} a -> s {createdAt = a} :: DatastoreProperties) Prelude.. Lens.mapping Data._Time

-- | The AWS-generated ID number for the Data Store.
datastoreProperties_datastoreId :: Lens.Lens' DatastoreProperties Prelude.Text
datastoreProperties_datastoreId = Lens.lens (\DatastoreProperties' {datastoreId} -> datastoreId) (\s@DatastoreProperties' {} a -> s {datastoreId = a} :: DatastoreProperties)

-- | The Amazon Resource Name used in the creation of the Data Store.
datastoreProperties_datastoreArn :: Lens.Lens' DatastoreProperties Prelude.Text
datastoreProperties_datastoreArn = Lens.lens (\DatastoreProperties' {datastoreArn} -> datastoreArn) (\s@DatastoreProperties' {} a -> s {datastoreArn = a} :: DatastoreProperties)

-- | The status of the Data Store. Possible statuses are \'CREATING\',
-- \'ACTIVE\', \'DELETING\', or \'DELETED\'.
datastoreProperties_datastoreStatus :: Lens.Lens' DatastoreProperties DatastoreStatus
datastoreProperties_datastoreStatus = Lens.lens (\DatastoreProperties' {datastoreStatus} -> datastoreStatus) (\s@DatastoreProperties' {} a -> s {datastoreStatus = a} :: DatastoreProperties)

-- | The FHIR version. Only R4 version data is supported.
datastoreProperties_datastoreTypeVersion :: Lens.Lens' DatastoreProperties FHIRVersion
datastoreProperties_datastoreTypeVersion = Lens.lens (\DatastoreProperties' {datastoreTypeVersion} -> datastoreTypeVersion) (\s@DatastoreProperties' {} a -> s {datastoreTypeVersion = a} :: DatastoreProperties)

-- | The AWS endpoint for the Data Store. Each Data Store will have it\'s own
-- endpoint with Data Store ID in the endpoint URL.
datastoreProperties_datastoreEndpoint :: Lens.Lens' DatastoreProperties Prelude.Text
datastoreProperties_datastoreEndpoint = Lens.lens (\DatastoreProperties' {datastoreEndpoint} -> datastoreEndpoint) (\s@DatastoreProperties' {} a -> s {datastoreEndpoint = a} :: DatastoreProperties)

instance Data.FromJSON DatastoreProperties where
  parseJSON =
    Data.withObject
      "DatastoreProperties"
      ( \x ->
          DatastoreProperties'
            Prelude.<$> (x Data..:? "DatastoreName")
            Prelude.<*> (x Data..:? "SseConfiguration")
            Prelude.<*> (x Data..:? "PreloadDataConfig")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..: "DatastoreId")
            Prelude.<*> (x Data..: "DatastoreArn")
            Prelude.<*> (x Data..: "DatastoreStatus")
            Prelude.<*> (x Data..: "DatastoreTypeVersion")
            Prelude.<*> (x Data..: "DatastoreEndpoint")
      )

instance Prelude.Hashable DatastoreProperties where
  hashWithSalt _salt DatastoreProperties' {..} =
    _salt `Prelude.hashWithSalt` datastoreName
      `Prelude.hashWithSalt` sseConfiguration
      `Prelude.hashWithSalt` preloadDataConfig
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` datastoreId
      `Prelude.hashWithSalt` datastoreArn
      `Prelude.hashWithSalt` datastoreStatus
      `Prelude.hashWithSalt` datastoreTypeVersion
      `Prelude.hashWithSalt` datastoreEndpoint

instance Prelude.NFData DatastoreProperties where
  rnf DatastoreProperties' {..} =
    Prelude.rnf datastoreName
      `Prelude.seq` Prelude.rnf sseConfiguration
      `Prelude.seq` Prelude.rnf preloadDataConfig
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf datastoreArn
      `Prelude.seq` Prelude.rnf datastoreStatus
      `Prelude.seq` Prelude.rnf datastoreTypeVersion
      `Prelude.seq` Prelude.rnf datastoreEndpoint
