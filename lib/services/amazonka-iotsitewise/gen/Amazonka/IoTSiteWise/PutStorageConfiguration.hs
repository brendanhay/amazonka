{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTSiteWise.PutStorageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures storage settings for IoT SiteWise.
module Amazonka.IoTSiteWise.PutStorageConfiguration
  ( -- * Creating a Request
    PutStorageConfiguration (..),
    newPutStorageConfiguration,

    -- * Request Lenses
    putStorageConfiguration_disassociatedDataStorage,
    putStorageConfiguration_multiLayerStorage,
    putStorageConfiguration_retentionPeriod,
    putStorageConfiguration_storageType,

    -- * Destructuring the Response
    PutStorageConfigurationResponse (..),
    newPutStorageConfigurationResponse,

    -- * Response Lenses
    putStorageConfigurationResponse_disassociatedDataStorage,
    putStorageConfigurationResponse_multiLayerStorage,
    putStorageConfigurationResponse_retentionPeriod,
    putStorageConfigurationResponse_httpStatus,
    putStorageConfigurationResponse_storageType,
    putStorageConfigurationResponse_configurationStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutStorageConfiguration' smart constructor.
data PutStorageConfiguration = PutStorageConfiguration'
  { -- | Contains the storage configuration for time series (data streams) that
    -- aren\'t associated with asset properties. The @disassociatedDataStorage@
    -- can be one of the following values:
    --
    -- -   @ENABLED@ – IoT SiteWise accepts time series that aren\'t associated
    --     with asset properties.
    --
    --     After the @disassociatedDataStorage@ is enabled, you can\'t disable
    --     it.
    --
    -- -   @DISABLED@ – IoT SiteWise doesn\'t accept time series (data streams)
    --     that aren\'t associated with asset properties.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/data-streams.html Data streams>
    -- in the /IoT SiteWise User Guide/.
    disassociatedDataStorage :: Prelude.Maybe DisassociatedDataStorageState,
    -- | Identifies a storage destination. If you specified @MULTI_LAYER_STORAGE@
    -- for the storage type, you must specify a @MultiLayerStorage@ object.
    multiLayerStorage :: Prelude.Maybe MultiLayerStorage,
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The storage tier that you specified for your data. The @storageType@
    -- parameter can be one of the following values:
    --
    -- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise saves your data into the
    --     hot tier. The hot tier is a service-managed database.
    --
    -- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise saves your data in both the
    --     cold tier and the hot tier. The cold tier is a customer-managed
    --     Amazon S3 bucket.
    storageType :: StorageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutStorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disassociatedDataStorage', 'putStorageConfiguration_disassociatedDataStorage' - Contains the storage configuration for time series (data streams) that
-- aren\'t associated with asset properties. The @disassociatedDataStorage@
-- can be one of the following values:
--
-- -   @ENABLED@ – IoT SiteWise accepts time series that aren\'t associated
--     with asset properties.
--
--     After the @disassociatedDataStorage@ is enabled, you can\'t disable
--     it.
--
-- -   @DISABLED@ – IoT SiteWise doesn\'t accept time series (data streams)
--     that aren\'t associated with asset properties.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/data-streams.html Data streams>
-- in the /IoT SiteWise User Guide/.
--
-- 'multiLayerStorage', 'putStorageConfiguration_multiLayerStorage' - Identifies a storage destination. If you specified @MULTI_LAYER_STORAGE@
-- for the storage type, you must specify a @MultiLayerStorage@ object.
--
-- 'retentionPeriod', 'putStorageConfiguration_retentionPeriod' - Undocumented member.
--
-- 'storageType', 'putStorageConfiguration_storageType' - The storage tier that you specified for your data. The @storageType@
-- parameter can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise saves your data into the
--     hot tier. The hot tier is a service-managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise saves your data in both the
--     cold tier and the hot tier. The cold tier is a customer-managed
--     Amazon S3 bucket.
newPutStorageConfiguration ::
  -- | 'storageType'
  StorageType ->
  PutStorageConfiguration
newPutStorageConfiguration pStorageType_ =
  PutStorageConfiguration'
    { disassociatedDataStorage =
        Prelude.Nothing,
      multiLayerStorage = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      storageType = pStorageType_
    }

-- | Contains the storage configuration for time series (data streams) that
-- aren\'t associated with asset properties. The @disassociatedDataStorage@
-- can be one of the following values:
--
-- -   @ENABLED@ – IoT SiteWise accepts time series that aren\'t associated
--     with asset properties.
--
--     After the @disassociatedDataStorage@ is enabled, you can\'t disable
--     it.
--
-- -   @DISABLED@ – IoT SiteWise doesn\'t accept time series (data streams)
--     that aren\'t associated with asset properties.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/data-streams.html Data streams>
-- in the /IoT SiteWise User Guide/.
putStorageConfiguration_disassociatedDataStorage :: Lens.Lens' PutStorageConfiguration (Prelude.Maybe DisassociatedDataStorageState)
putStorageConfiguration_disassociatedDataStorage = Lens.lens (\PutStorageConfiguration' {disassociatedDataStorage} -> disassociatedDataStorage) (\s@PutStorageConfiguration' {} a -> s {disassociatedDataStorage = a} :: PutStorageConfiguration)

-- | Identifies a storage destination. If you specified @MULTI_LAYER_STORAGE@
-- for the storage type, you must specify a @MultiLayerStorage@ object.
putStorageConfiguration_multiLayerStorage :: Lens.Lens' PutStorageConfiguration (Prelude.Maybe MultiLayerStorage)
putStorageConfiguration_multiLayerStorage = Lens.lens (\PutStorageConfiguration' {multiLayerStorage} -> multiLayerStorage) (\s@PutStorageConfiguration' {} a -> s {multiLayerStorage = a} :: PutStorageConfiguration)

-- | Undocumented member.
putStorageConfiguration_retentionPeriod :: Lens.Lens' PutStorageConfiguration (Prelude.Maybe RetentionPeriod)
putStorageConfiguration_retentionPeriod = Lens.lens (\PutStorageConfiguration' {retentionPeriod} -> retentionPeriod) (\s@PutStorageConfiguration' {} a -> s {retentionPeriod = a} :: PutStorageConfiguration)

-- | The storage tier that you specified for your data. The @storageType@
-- parameter can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise saves your data into the
--     hot tier. The hot tier is a service-managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise saves your data in both the
--     cold tier and the hot tier. The cold tier is a customer-managed
--     Amazon S3 bucket.
putStorageConfiguration_storageType :: Lens.Lens' PutStorageConfiguration StorageType
putStorageConfiguration_storageType = Lens.lens (\PutStorageConfiguration' {storageType} -> storageType) (\s@PutStorageConfiguration' {} a -> s {storageType = a} :: PutStorageConfiguration)

instance Core.AWSRequest PutStorageConfiguration where
  type
    AWSResponse PutStorageConfiguration =
      PutStorageConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutStorageConfigurationResponse'
            Prelude.<$> (x Data..?> "disassociatedDataStorage")
            Prelude.<*> (x Data..?> "multiLayerStorage")
            Prelude.<*> (x Data..?> "retentionPeriod")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "storageType")
            Prelude.<*> (x Data..:> "configurationStatus")
      )

instance Prelude.Hashable PutStorageConfiguration where
  hashWithSalt _salt PutStorageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` disassociatedDataStorage
      `Prelude.hashWithSalt` multiLayerStorage
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` storageType

instance Prelude.NFData PutStorageConfiguration where
  rnf PutStorageConfiguration' {..} =
    Prelude.rnf disassociatedDataStorage
      `Prelude.seq` Prelude.rnf multiLayerStorage
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf storageType

instance Data.ToHeaders PutStorageConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutStorageConfiguration where
  toJSON PutStorageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("disassociatedDataStorage" Data..=)
              Prelude.<$> disassociatedDataStorage,
            ("multiLayerStorage" Data..=)
              Prelude.<$> multiLayerStorage,
            ("retentionPeriod" Data..=)
              Prelude.<$> retentionPeriod,
            Prelude.Just ("storageType" Data..= storageType)
          ]
      )

instance Data.ToPath PutStorageConfiguration where
  toPath =
    Prelude.const "/configuration/account/storage"

instance Data.ToQuery PutStorageConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutStorageConfigurationResponse' smart constructor.
data PutStorageConfigurationResponse = PutStorageConfigurationResponse'
  { -- | Contains the storage configuration for time series (data streams) that
    -- aren\'t associated with asset properties. The @disassociatedDataStorage@
    -- can be one of the following values:
    --
    -- -   @ENABLED@ – IoT SiteWise accepts time series that aren\'t associated
    --     with asset properties.
    --
    --     After the @disassociatedDataStorage@ is enabled, you can\'t disable
    --     it.
    --
    -- -   @DISABLED@ – IoT SiteWise doesn\'t accept time series (data streams)
    --     that aren\'t associated with asset properties.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/data-streams.html Data streams>
    -- in the /IoT SiteWise User Guide/.
    disassociatedDataStorage :: Prelude.Maybe DisassociatedDataStorageState,
    -- | Contains information about the storage destination.
    multiLayerStorage :: Prelude.Maybe MultiLayerStorage,
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The storage tier that you specified for your data. The @storageType@
    -- parameter can be one of the following values:
    --
    -- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise saves your data into the
    --     hot tier. The hot tier is a service-managed database.
    --
    -- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise saves your data in both the
    --     cold tier and the hot tier. The cold tier is a customer-managed
    --     Amazon S3 bucket.
    storageType :: StorageType,
    configurationStatus :: ConfigurationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutStorageConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disassociatedDataStorage', 'putStorageConfigurationResponse_disassociatedDataStorage' - Contains the storage configuration for time series (data streams) that
-- aren\'t associated with asset properties. The @disassociatedDataStorage@
-- can be one of the following values:
--
-- -   @ENABLED@ – IoT SiteWise accepts time series that aren\'t associated
--     with asset properties.
--
--     After the @disassociatedDataStorage@ is enabled, you can\'t disable
--     it.
--
-- -   @DISABLED@ – IoT SiteWise doesn\'t accept time series (data streams)
--     that aren\'t associated with asset properties.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/data-streams.html Data streams>
-- in the /IoT SiteWise User Guide/.
--
-- 'multiLayerStorage', 'putStorageConfigurationResponse_multiLayerStorage' - Contains information about the storage destination.
--
-- 'retentionPeriod', 'putStorageConfigurationResponse_retentionPeriod' - Undocumented member.
--
-- 'httpStatus', 'putStorageConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'storageType', 'putStorageConfigurationResponse_storageType' - The storage tier that you specified for your data. The @storageType@
-- parameter can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise saves your data into the
--     hot tier. The hot tier is a service-managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise saves your data in both the
--     cold tier and the hot tier. The cold tier is a customer-managed
--     Amazon S3 bucket.
--
-- 'configurationStatus', 'putStorageConfigurationResponse_configurationStatus' - Undocumented member.
newPutStorageConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'storageType'
  StorageType ->
  -- | 'configurationStatus'
  ConfigurationStatus ->
  PutStorageConfigurationResponse
newPutStorageConfigurationResponse
  pHttpStatus_
  pStorageType_
  pConfigurationStatus_ =
    PutStorageConfigurationResponse'
      { disassociatedDataStorage =
          Prelude.Nothing,
        multiLayerStorage = Prelude.Nothing,
        retentionPeriod = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        storageType = pStorageType_,
        configurationStatus =
          pConfigurationStatus_
      }

-- | Contains the storage configuration for time series (data streams) that
-- aren\'t associated with asset properties. The @disassociatedDataStorage@
-- can be one of the following values:
--
-- -   @ENABLED@ – IoT SiteWise accepts time series that aren\'t associated
--     with asset properties.
--
--     After the @disassociatedDataStorage@ is enabled, you can\'t disable
--     it.
--
-- -   @DISABLED@ – IoT SiteWise doesn\'t accept time series (data streams)
--     that aren\'t associated with asset properties.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/data-streams.html Data streams>
-- in the /IoT SiteWise User Guide/.
putStorageConfigurationResponse_disassociatedDataStorage :: Lens.Lens' PutStorageConfigurationResponse (Prelude.Maybe DisassociatedDataStorageState)
putStorageConfigurationResponse_disassociatedDataStorage = Lens.lens (\PutStorageConfigurationResponse' {disassociatedDataStorage} -> disassociatedDataStorage) (\s@PutStorageConfigurationResponse' {} a -> s {disassociatedDataStorage = a} :: PutStorageConfigurationResponse)

-- | Contains information about the storage destination.
putStorageConfigurationResponse_multiLayerStorage :: Lens.Lens' PutStorageConfigurationResponse (Prelude.Maybe MultiLayerStorage)
putStorageConfigurationResponse_multiLayerStorage = Lens.lens (\PutStorageConfigurationResponse' {multiLayerStorage} -> multiLayerStorage) (\s@PutStorageConfigurationResponse' {} a -> s {multiLayerStorage = a} :: PutStorageConfigurationResponse)

-- | Undocumented member.
putStorageConfigurationResponse_retentionPeriod :: Lens.Lens' PutStorageConfigurationResponse (Prelude.Maybe RetentionPeriod)
putStorageConfigurationResponse_retentionPeriod = Lens.lens (\PutStorageConfigurationResponse' {retentionPeriod} -> retentionPeriod) (\s@PutStorageConfigurationResponse' {} a -> s {retentionPeriod = a} :: PutStorageConfigurationResponse)

-- | The response's http status code.
putStorageConfigurationResponse_httpStatus :: Lens.Lens' PutStorageConfigurationResponse Prelude.Int
putStorageConfigurationResponse_httpStatus = Lens.lens (\PutStorageConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutStorageConfigurationResponse' {} a -> s {httpStatus = a} :: PutStorageConfigurationResponse)

-- | The storage tier that you specified for your data. The @storageType@
-- parameter can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise saves your data into the
--     hot tier. The hot tier is a service-managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise saves your data in both the
--     cold tier and the hot tier. The cold tier is a customer-managed
--     Amazon S3 bucket.
putStorageConfigurationResponse_storageType :: Lens.Lens' PutStorageConfigurationResponse StorageType
putStorageConfigurationResponse_storageType = Lens.lens (\PutStorageConfigurationResponse' {storageType} -> storageType) (\s@PutStorageConfigurationResponse' {} a -> s {storageType = a} :: PutStorageConfigurationResponse)

-- | Undocumented member.
putStorageConfigurationResponse_configurationStatus :: Lens.Lens' PutStorageConfigurationResponse ConfigurationStatus
putStorageConfigurationResponse_configurationStatus = Lens.lens (\PutStorageConfigurationResponse' {configurationStatus} -> configurationStatus) (\s@PutStorageConfigurationResponse' {} a -> s {configurationStatus = a} :: PutStorageConfigurationResponse)

instance
  Prelude.NFData
    PutStorageConfigurationResponse
  where
  rnf PutStorageConfigurationResponse' {..} =
    Prelude.rnf disassociatedDataStorage
      `Prelude.seq` Prelude.rnf multiLayerStorage
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf configurationStatus
