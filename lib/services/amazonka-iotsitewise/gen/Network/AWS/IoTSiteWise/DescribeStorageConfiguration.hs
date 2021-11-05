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
-- Module      : Network.AWS.IoTSiteWise.DescribeStorageConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the storage configuration for IoT SiteWise.
module Network.AWS.IoTSiteWise.DescribeStorageConfiguration
  ( -- * Creating a Request
    DescribeStorageConfiguration (..),
    newDescribeStorageConfiguration,

    -- * Destructuring the Response
    DescribeStorageConfigurationResponse (..),
    newDescribeStorageConfigurationResponse,

    -- * Response Lenses
    describeStorageConfigurationResponse_multiLayerStorage,
    describeStorageConfigurationResponse_lastUpdateDate,
    describeStorageConfigurationResponse_httpStatus,
    describeStorageConfigurationResponse_storageType,
    describeStorageConfigurationResponse_configurationStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSiteWise.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStorageConfiguration' smart constructor.
data DescribeStorageConfiguration = DescribeStorageConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeStorageConfiguration ::
  DescribeStorageConfiguration
newDescribeStorageConfiguration =
  DescribeStorageConfiguration'

instance Core.AWSRequest DescribeStorageConfiguration where
  type
    AWSResponse DescribeStorageConfiguration =
      DescribeStorageConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStorageConfigurationResponse'
            Prelude.<$> (x Core..?> "multiLayerStorage")
            Prelude.<*> (x Core..?> "lastUpdateDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "storageType")
            Prelude.<*> (x Core..:> "configurationStatus")
      )

instance
  Prelude.Hashable
    DescribeStorageConfiguration

instance Prelude.NFData DescribeStorageConfiguration

instance Core.ToHeaders DescribeStorageConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeStorageConfiguration where
  toPath =
    Prelude.const "/configuration/account/storage"

instance Core.ToQuery DescribeStorageConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStorageConfigurationResponse' smart constructor.
data DescribeStorageConfigurationResponse = DescribeStorageConfigurationResponse'
  { -- | Contains information about the storage destination.
    multiLayerStorage :: Prelude.Maybe MultiLayerStorage,
    -- | The date the storage configuration was last updated, in Unix epoch time.
    lastUpdateDate :: Prelude.Maybe Core.POSIX,
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
-- Create a value of 'DescribeStorageConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiLayerStorage', 'describeStorageConfigurationResponse_multiLayerStorage' - Contains information about the storage destination.
--
-- 'lastUpdateDate', 'describeStorageConfigurationResponse_lastUpdateDate' - The date the storage configuration was last updated, in Unix epoch time.
--
-- 'httpStatus', 'describeStorageConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'storageType', 'describeStorageConfigurationResponse_storageType' - The type of storage that you specified for your data. The storage type
-- can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise replicates your data into
--     a service managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise replicates your data into a
--     service managed database and saves a copy of your raw data and
--     metadata in an Amazon S3 object that you specified.
--
-- 'configurationStatus', 'describeStorageConfigurationResponse_configurationStatus' - Undocumented member.
newDescribeStorageConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'storageType'
  StorageType ->
  -- | 'configurationStatus'
  ConfigurationStatus ->
  DescribeStorageConfigurationResponse
newDescribeStorageConfigurationResponse
  pHttpStatus_
  pStorageType_
  pConfigurationStatus_ =
    DescribeStorageConfigurationResponse'
      { multiLayerStorage =
          Prelude.Nothing,
        lastUpdateDate = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        storageType = pStorageType_,
        configurationStatus =
          pConfigurationStatus_
      }

-- | Contains information about the storage destination.
describeStorageConfigurationResponse_multiLayerStorage :: Lens.Lens' DescribeStorageConfigurationResponse (Prelude.Maybe MultiLayerStorage)
describeStorageConfigurationResponse_multiLayerStorage = Lens.lens (\DescribeStorageConfigurationResponse' {multiLayerStorage} -> multiLayerStorage) (\s@DescribeStorageConfigurationResponse' {} a -> s {multiLayerStorage = a} :: DescribeStorageConfigurationResponse)

-- | The date the storage configuration was last updated, in Unix epoch time.
describeStorageConfigurationResponse_lastUpdateDate :: Lens.Lens' DescribeStorageConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
describeStorageConfigurationResponse_lastUpdateDate = Lens.lens (\DescribeStorageConfigurationResponse' {lastUpdateDate} -> lastUpdateDate) (\s@DescribeStorageConfigurationResponse' {} a -> s {lastUpdateDate = a} :: DescribeStorageConfigurationResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeStorageConfigurationResponse_httpStatus :: Lens.Lens' DescribeStorageConfigurationResponse Prelude.Int
describeStorageConfigurationResponse_httpStatus = Lens.lens (\DescribeStorageConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeStorageConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeStorageConfigurationResponse)

-- | The type of storage that you specified for your data. The storage type
-- can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise replicates your data into
--     a service managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise replicates your data into a
--     service managed database and saves a copy of your raw data and
--     metadata in an Amazon S3 object that you specified.
describeStorageConfigurationResponse_storageType :: Lens.Lens' DescribeStorageConfigurationResponse StorageType
describeStorageConfigurationResponse_storageType = Lens.lens (\DescribeStorageConfigurationResponse' {storageType} -> storageType) (\s@DescribeStorageConfigurationResponse' {} a -> s {storageType = a} :: DescribeStorageConfigurationResponse)

-- | Undocumented member.
describeStorageConfigurationResponse_configurationStatus :: Lens.Lens' DescribeStorageConfigurationResponse ConfigurationStatus
describeStorageConfigurationResponse_configurationStatus = Lens.lens (\DescribeStorageConfigurationResponse' {configurationStatus} -> configurationStatus) (\s@DescribeStorageConfigurationResponse' {} a -> s {configurationStatus = a} :: DescribeStorageConfigurationResponse)

instance
  Prelude.NFData
    DescribeStorageConfigurationResponse
