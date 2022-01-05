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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures storage settings for IoT SiteWise.
module Amazonka.IoTSiteWise.PutStorageConfiguration
  ( -- * Creating a Request
    PutStorageConfiguration (..),
    newPutStorageConfiguration,

    -- * Request Lenses
    putStorageConfiguration_multiLayerStorage,
    putStorageConfiguration_storageType,

    -- * Destructuring the Response
    PutStorageConfigurationResponse (..),
    newPutStorageConfigurationResponse,

    -- * Response Lenses
    putStorageConfigurationResponse_multiLayerStorage,
    putStorageConfigurationResponse_httpStatus,
    putStorageConfigurationResponse_storageType,
    putStorageConfigurationResponse_configurationStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutStorageConfiguration' smart constructor.
data PutStorageConfiguration = PutStorageConfiguration'
  { -- | Identifies a storage destination. If you specified @MULTI_LAYER_STORAGE@
    -- for the storage type, you must specify a @MultiLayerStorage@ object.
    multiLayerStorage :: Prelude.Maybe MultiLayerStorage,
    -- | The type of storage that you specified for your data. The storage type
    -- can be one of the following values:
    --
    -- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise replicates your data into
    --     a service managed database.
    --
    -- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise replicates your data into a
    --     service managed database and saves a copy of your raw data and
    --     metadata in an Amazon S3 object that you specified.
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
-- 'multiLayerStorage', 'putStorageConfiguration_multiLayerStorage' - Identifies a storage destination. If you specified @MULTI_LAYER_STORAGE@
-- for the storage type, you must specify a @MultiLayerStorage@ object.
--
-- 'storageType', 'putStorageConfiguration_storageType' - The type of storage that you specified for your data. The storage type
-- can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise replicates your data into
--     a service managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise replicates your data into a
--     service managed database and saves a copy of your raw data and
--     metadata in an Amazon S3 object that you specified.
newPutStorageConfiguration ::
  -- | 'storageType'
  StorageType ->
  PutStorageConfiguration
newPutStorageConfiguration pStorageType_ =
  PutStorageConfiguration'
    { multiLayerStorage =
        Prelude.Nothing,
      storageType = pStorageType_
    }

-- | Identifies a storage destination. If you specified @MULTI_LAYER_STORAGE@
-- for the storage type, you must specify a @MultiLayerStorage@ object.
putStorageConfiguration_multiLayerStorage :: Lens.Lens' PutStorageConfiguration (Prelude.Maybe MultiLayerStorage)
putStorageConfiguration_multiLayerStorage = Lens.lens (\PutStorageConfiguration' {multiLayerStorage} -> multiLayerStorage) (\s@PutStorageConfiguration' {} a -> s {multiLayerStorage = a} :: PutStorageConfiguration)

-- | The type of storage that you specified for your data. The storage type
-- can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise replicates your data into
--     a service managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise replicates your data into a
--     service managed database and saves a copy of your raw data and
--     metadata in an Amazon S3 object that you specified.
putStorageConfiguration_storageType :: Lens.Lens' PutStorageConfiguration StorageType
putStorageConfiguration_storageType = Lens.lens (\PutStorageConfiguration' {storageType} -> storageType) (\s@PutStorageConfiguration' {} a -> s {storageType = a} :: PutStorageConfiguration)

instance Core.AWSRequest PutStorageConfiguration where
  type
    AWSResponse PutStorageConfiguration =
      PutStorageConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutStorageConfigurationResponse'
            Prelude.<$> (x Core..?> "multiLayerStorage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "storageType")
            Prelude.<*> (x Core..:> "configurationStatus")
      )

instance Prelude.Hashable PutStorageConfiguration where
  hashWithSalt _salt PutStorageConfiguration' {..} =
    _salt `Prelude.hashWithSalt` multiLayerStorage
      `Prelude.hashWithSalt` storageType

instance Prelude.NFData PutStorageConfiguration where
  rnf PutStorageConfiguration' {..} =
    Prelude.rnf multiLayerStorage
      `Prelude.seq` Prelude.rnf storageType

instance Core.ToHeaders PutStorageConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutStorageConfiguration where
  toJSON PutStorageConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("multiLayerStorage" Core..=)
              Prelude.<$> multiLayerStorage,
            Prelude.Just ("storageType" Core..= storageType)
          ]
      )

instance Core.ToPath PutStorageConfiguration where
  toPath =
    Prelude.const "/configuration/account/storage"

instance Core.ToQuery PutStorageConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutStorageConfigurationResponse' smart constructor.
data PutStorageConfigurationResponse = PutStorageConfigurationResponse'
  { -- | Contains information about the storage destination.
    multiLayerStorage :: Prelude.Maybe MultiLayerStorage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The type of storage that you specified for your data. The storage type
    -- can be one of the following values:
    --
    -- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise replicates your data into
    --     a service managed database.
    --
    -- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise replicates your data into a
    --     service managed database and saves a copy of your raw data and
    --     metadata in an Amazon S3 object that you specified.
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
-- 'multiLayerStorage', 'putStorageConfigurationResponse_multiLayerStorage' - Contains information about the storage destination.
--
-- 'httpStatus', 'putStorageConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'storageType', 'putStorageConfigurationResponse_storageType' - The type of storage that you specified for your data. The storage type
-- can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise replicates your data into
--     a service managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise replicates your data into a
--     service managed database and saves a copy of your raw data and
--     metadata in an Amazon S3 object that you specified.
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
      { multiLayerStorage =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        storageType = pStorageType_,
        configurationStatus =
          pConfigurationStatus_
      }

-- | Contains information about the storage destination.
putStorageConfigurationResponse_multiLayerStorage :: Lens.Lens' PutStorageConfigurationResponse (Prelude.Maybe MultiLayerStorage)
putStorageConfigurationResponse_multiLayerStorage = Lens.lens (\PutStorageConfigurationResponse' {multiLayerStorage} -> multiLayerStorage) (\s@PutStorageConfigurationResponse' {} a -> s {multiLayerStorage = a} :: PutStorageConfigurationResponse)

-- | The response's http status code.
putStorageConfigurationResponse_httpStatus :: Lens.Lens' PutStorageConfigurationResponse Prelude.Int
putStorageConfigurationResponse_httpStatus = Lens.lens (\PutStorageConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutStorageConfigurationResponse' {} a -> s {httpStatus = a} :: PutStorageConfigurationResponse)

-- | The type of storage that you specified for your data. The storage type
-- can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise replicates your data into
--     a service managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise replicates your data into a
--     service managed database and saves a copy of your raw data and
--     metadata in an Amazon S3 object that you specified.
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
    Prelude.rnf multiLayerStorage
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf configurationStatus
