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
-- Module      : Amazonka.IoTSiteWise.DescribeStorageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the storage configuration for IoT SiteWise.
module Amazonka.IoTSiteWise.DescribeStorageConfiguration
  ( -- * Creating a Request
    DescribeStorageConfiguration (..),
    newDescribeStorageConfiguration,

    -- * Destructuring the Response
    DescribeStorageConfigurationResponse (..),
    newDescribeStorageConfigurationResponse,

    -- * Response Lenses
    describeStorageConfigurationResponse_disassociatedDataStorage,
    describeStorageConfigurationResponse_lastUpdateDate,
    describeStorageConfigurationResponse_multiLayerStorage,
    describeStorageConfigurationResponse_retentionPeriod,
    describeStorageConfigurationResponse_httpStatus,
    describeStorageConfigurationResponse_storageType,
    describeStorageConfigurationResponse_configurationStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStorageConfigurationResponse'
            Prelude.<$> (x Data..?> "disassociatedDataStorage")
            Prelude.<*> (x Data..?> "lastUpdateDate")
            Prelude.<*> (x Data..?> "multiLayerStorage")
            Prelude.<*> (x Data..?> "retentionPeriod")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "storageType")
            Prelude.<*> (x Data..:> "configurationStatus")
      )

instance
  Prelude.Hashable
    DescribeStorageConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeStorageConfiguration where
  rnf _ = ()

instance Data.ToHeaders DescribeStorageConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeStorageConfiguration where
  toPath =
    Prelude.const "/configuration/account/storage"

instance Data.ToQuery DescribeStorageConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStorageConfigurationResponse' smart constructor.
data DescribeStorageConfigurationResponse = DescribeStorageConfigurationResponse'
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
    -- | The date the storage configuration was last updated, in Unix epoch time.
    lastUpdateDate :: Prelude.Maybe Data.POSIX,
    -- | Contains information about the storage destination.
    multiLayerStorage :: Prelude.Maybe MultiLayerStorage,
    -- | How many days your data is kept in the hot tier. By default, your data
    -- is kept indefinitely in the hot tier.
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
-- Create a value of 'DescribeStorageConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disassociatedDataStorage', 'describeStorageConfigurationResponse_disassociatedDataStorage' - Contains the storage configuration for time series (data streams) that
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
-- 'lastUpdateDate', 'describeStorageConfigurationResponse_lastUpdateDate' - The date the storage configuration was last updated, in Unix epoch time.
--
-- 'multiLayerStorage', 'describeStorageConfigurationResponse_multiLayerStorage' - Contains information about the storage destination.
--
-- 'retentionPeriod', 'describeStorageConfigurationResponse_retentionPeriod' - How many days your data is kept in the hot tier. By default, your data
-- is kept indefinitely in the hot tier.
--
-- 'httpStatus', 'describeStorageConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'storageType', 'describeStorageConfigurationResponse_storageType' - The storage tier that you specified for your data. The @storageType@
-- parameter can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise saves your data into the
--     hot tier. The hot tier is a service-managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise saves your data in both the
--     cold tier and the hot tier. The cold tier is a customer-managed
--     Amazon S3 bucket.
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
      { disassociatedDataStorage =
          Prelude.Nothing,
        lastUpdateDate = Prelude.Nothing,
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
describeStorageConfigurationResponse_disassociatedDataStorage :: Lens.Lens' DescribeStorageConfigurationResponse (Prelude.Maybe DisassociatedDataStorageState)
describeStorageConfigurationResponse_disassociatedDataStorage = Lens.lens (\DescribeStorageConfigurationResponse' {disassociatedDataStorage} -> disassociatedDataStorage) (\s@DescribeStorageConfigurationResponse' {} a -> s {disassociatedDataStorage = a} :: DescribeStorageConfigurationResponse)

-- | The date the storage configuration was last updated, in Unix epoch time.
describeStorageConfigurationResponse_lastUpdateDate :: Lens.Lens' DescribeStorageConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
describeStorageConfigurationResponse_lastUpdateDate = Lens.lens (\DescribeStorageConfigurationResponse' {lastUpdateDate} -> lastUpdateDate) (\s@DescribeStorageConfigurationResponse' {} a -> s {lastUpdateDate = a} :: DescribeStorageConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | Contains information about the storage destination.
describeStorageConfigurationResponse_multiLayerStorage :: Lens.Lens' DescribeStorageConfigurationResponse (Prelude.Maybe MultiLayerStorage)
describeStorageConfigurationResponse_multiLayerStorage = Lens.lens (\DescribeStorageConfigurationResponse' {multiLayerStorage} -> multiLayerStorage) (\s@DescribeStorageConfigurationResponse' {} a -> s {multiLayerStorage = a} :: DescribeStorageConfigurationResponse)

-- | How many days your data is kept in the hot tier. By default, your data
-- is kept indefinitely in the hot tier.
describeStorageConfigurationResponse_retentionPeriod :: Lens.Lens' DescribeStorageConfigurationResponse (Prelude.Maybe RetentionPeriod)
describeStorageConfigurationResponse_retentionPeriod = Lens.lens (\DescribeStorageConfigurationResponse' {retentionPeriod} -> retentionPeriod) (\s@DescribeStorageConfigurationResponse' {} a -> s {retentionPeriod = a} :: DescribeStorageConfigurationResponse)

-- | The response's http status code.
describeStorageConfigurationResponse_httpStatus :: Lens.Lens' DescribeStorageConfigurationResponse Prelude.Int
describeStorageConfigurationResponse_httpStatus = Lens.lens (\DescribeStorageConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeStorageConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeStorageConfigurationResponse)

-- | The storage tier that you specified for your data. The @storageType@
-- parameter can be one of the following values:
--
-- -   @SITEWISE_DEFAULT_STORAGE@ – IoT SiteWise saves your data into the
--     hot tier. The hot tier is a service-managed database.
--
-- -   @MULTI_LAYER_STORAGE@ – IoT SiteWise saves your data in both the
--     cold tier and the hot tier. The cold tier is a customer-managed
--     Amazon S3 bucket.
describeStorageConfigurationResponse_storageType :: Lens.Lens' DescribeStorageConfigurationResponse StorageType
describeStorageConfigurationResponse_storageType = Lens.lens (\DescribeStorageConfigurationResponse' {storageType} -> storageType) (\s@DescribeStorageConfigurationResponse' {} a -> s {storageType = a} :: DescribeStorageConfigurationResponse)

-- | Undocumented member.
describeStorageConfigurationResponse_configurationStatus :: Lens.Lens' DescribeStorageConfigurationResponse ConfigurationStatus
describeStorageConfigurationResponse_configurationStatus = Lens.lens (\DescribeStorageConfigurationResponse' {configurationStatus} -> configurationStatus) (\s@DescribeStorageConfigurationResponse' {} a -> s {configurationStatus = a} :: DescribeStorageConfigurationResponse)

instance
  Prelude.NFData
    DescribeStorageConfigurationResponse
  where
  rnf DescribeStorageConfigurationResponse' {..} =
    Prelude.rnf disassociatedDataStorage
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf multiLayerStorage
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf configurationStatus
