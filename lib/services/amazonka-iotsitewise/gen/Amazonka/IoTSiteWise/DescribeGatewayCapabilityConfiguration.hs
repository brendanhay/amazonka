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
-- Module      : Amazonka.IoTSiteWise.DescribeGatewayCapabilityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a gateway capability configuration. Each
-- gateway capability defines data sources for a gateway. A capability
-- configuration can contain multiple data source configurations. If you
-- define OPC-UA sources for a gateway in the IoT SiteWise console, all of
-- your OPC-UA sources are stored in one capability configuration. To list
-- all capability configurations for a gateway, use
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeGateway.html DescribeGateway>.
module Amazonka.IoTSiteWise.DescribeGatewayCapabilityConfiguration
  ( -- * Creating a Request
    DescribeGatewayCapabilityConfiguration (..),
    newDescribeGatewayCapabilityConfiguration,

    -- * Request Lenses
    describeGatewayCapabilityConfiguration_gatewayId,
    describeGatewayCapabilityConfiguration_capabilityNamespace,

    -- * Destructuring the Response
    DescribeGatewayCapabilityConfigurationResponse (..),
    newDescribeGatewayCapabilityConfigurationResponse,

    -- * Response Lenses
    describeGatewayCapabilityConfigurationResponse_httpStatus,
    describeGatewayCapabilityConfigurationResponse_gatewayId,
    describeGatewayCapabilityConfigurationResponse_capabilityNamespace,
    describeGatewayCapabilityConfigurationResponse_capabilityConfiguration,
    describeGatewayCapabilityConfigurationResponse_capabilitySyncStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGatewayCapabilityConfiguration' smart constructor.
data DescribeGatewayCapabilityConfiguration = DescribeGatewayCapabilityConfiguration'
  { -- | The ID of the gateway that defines the capability configuration.
    gatewayId :: Prelude.Text,
    -- | The namespace of the capability configuration. For example, if you
    -- configure OPC-UA sources from the IoT SiteWise console, your OPC-UA
    -- capability configuration has the namespace
    -- @iotsitewise:opcuacollector:version@, where @version@ is a number such
    -- as @1@.
    capabilityNamespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGatewayCapabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayId', 'describeGatewayCapabilityConfiguration_gatewayId' - The ID of the gateway that defines the capability configuration.
--
-- 'capabilityNamespace', 'describeGatewayCapabilityConfiguration_capabilityNamespace' - The namespace of the capability configuration. For example, if you
-- configure OPC-UA sources from the IoT SiteWise console, your OPC-UA
-- capability configuration has the namespace
-- @iotsitewise:opcuacollector:version@, where @version@ is a number such
-- as @1@.
newDescribeGatewayCapabilityConfiguration ::
  -- | 'gatewayId'
  Prelude.Text ->
  -- | 'capabilityNamespace'
  Prelude.Text ->
  DescribeGatewayCapabilityConfiguration
newDescribeGatewayCapabilityConfiguration
  pGatewayId_
  pCapabilityNamespace_ =
    DescribeGatewayCapabilityConfiguration'
      { gatewayId =
          pGatewayId_,
        capabilityNamespace =
          pCapabilityNamespace_
      }

-- | The ID of the gateway that defines the capability configuration.
describeGatewayCapabilityConfiguration_gatewayId :: Lens.Lens' DescribeGatewayCapabilityConfiguration Prelude.Text
describeGatewayCapabilityConfiguration_gatewayId = Lens.lens (\DescribeGatewayCapabilityConfiguration' {gatewayId} -> gatewayId) (\s@DescribeGatewayCapabilityConfiguration' {} a -> s {gatewayId = a} :: DescribeGatewayCapabilityConfiguration)

-- | The namespace of the capability configuration. For example, if you
-- configure OPC-UA sources from the IoT SiteWise console, your OPC-UA
-- capability configuration has the namespace
-- @iotsitewise:opcuacollector:version@, where @version@ is a number such
-- as @1@.
describeGatewayCapabilityConfiguration_capabilityNamespace :: Lens.Lens' DescribeGatewayCapabilityConfiguration Prelude.Text
describeGatewayCapabilityConfiguration_capabilityNamespace = Lens.lens (\DescribeGatewayCapabilityConfiguration' {capabilityNamespace} -> capabilityNamespace) (\s@DescribeGatewayCapabilityConfiguration' {} a -> s {capabilityNamespace = a} :: DescribeGatewayCapabilityConfiguration)

instance
  Core.AWSRequest
    DescribeGatewayCapabilityConfiguration
  where
  type
    AWSResponse
      DescribeGatewayCapabilityConfiguration =
      DescribeGatewayCapabilityConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGatewayCapabilityConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "gatewayId")
            Prelude.<*> (x Data..:> "capabilityNamespace")
            Prelude.<*> (x Data..:> "capabilityConfiguration")
            Prelude.<*> (x Data..:> "capabilitySyncStatus")
      )

instance
  Prelude.Hashable
    DescribeGatewayCapabilityConfiguration
  where
  hashWithSalt
    _salt
    DescribeGatewayCapabilityConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` gatewayId
        `Prelude.hashWithSalt` capabilityNamespace

instance
  Prelude.NFData
    DescribeGatewayCapabilityConfiguration
  where
  rnf DescribeGatewayCapabilityConfiguration' {..} =
    Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf capabilityNamespace

instance
  Data.ToHeaders
    DescribeGatewayCapabilityConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DescribeGatewayCapabilityConfiguration
  where
  toPath DescribeGatewayCapabilityConfiguration' {..} =
    Prelude.mconcat
      [ "/20200301/gateways/",
        Data.toBS gatewayId,
        "/capability/",
        Data.toBS capabilityNamespace
      ]

instance
  Data.ToQuery
    DescribeGatewayCapabilityConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGatewayCapabilityConfigurationResponse' smart constructor.
data DescribeGatewayCapabilityConfigurationResponse = DescribeGatewayCapabilityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the gateway that defines the capability configuration.
    gatewayId :: Prelude.Text,
    -- | The namespace of the gateway capability.
    capabilityNamespace :: Prelude.Text,
    -- | The JSON document that defines the gateway capability\'s configuration.
    -- For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/configure-sources.html#configure-source-cli Configuring data sources (CLI)>
    -- in the /IoT SiteWise User Guide/.
    capabilityConfiguration :: Prelude.Text,
    -- | The synchronization status of the capability configuration. The sync
    -- status can be one of the following:
    --
    -- -   @IN_SYNC@ – The gateway is running the capability configuration.
    --
    -- -   @OUT_OF_SYNC@ – The gateway hasn\'t received the capability
    --     configuration.
    --
    -- -   @SYNC_FAILED@ – The gateway rejected the capability configuration.
    capabilitySyncStatus :: CapabilitySyncStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGatewayCapabilityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeGatewayCapabilityConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'gatewayId', 'describeGatewayCapabilityConfigurationResponse_gatewayId' - The ID of the gateway that defines the capability configuration.
--
-- 'capabilityNamespace', 'describeGatewayCapabilityConfigurationResponse_capabilityNamespace' - The namespace of the gateway capability.
--
-- 'capabilityConfiguration', 'describeGatewayCapabilityConfigurationResponse_capabilityConfiguration' - The JSON document that defines the gateway capability\'s configuration.
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/configure-sources.html#configure-source-cli Configuring data sources (CLI)>
-- in the /IoT SiteWise User Guide/.
--
-- 'capabilitySyncStatus', 'describeGatewayCapabilityConfigurationResponse_capabilitySyncStatus' - The synchronization status of the capability configuration. The sync
-- status can be one of the following:
--
-- -   @IN_SYNC@ – The gateway is running the capability configuration.
--
-- -   @OUT_OF_SYNC@ – The gateway hasn\'t received the capability
--     configuration.
--
-- -   @SYNC_FAILED@ – The gateway rejected the capability configuration.
newDescribeGatewayCapabilityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'gatewayId'
  Prelude.Text ->
  -- | 'capabilityNamespace'
  Prelude.Text ->
  -- | 'capabilityConfiguration'
  Prelude.Text ->
  -- | 'capabilitySyncStatus'
  CapabilitySyncStatus ->
  DescribeGatewayCapabilityConfigurationResponse
newDescribeGatewayCapabilityConfigurationResponse
  pHttpStatus_
  pGatewayId_
  pCapabilityNamespace_
  pCapabilityConfiguration_
  pCapabilitySyncStatus_ =
    DescribeGatewayCapabilityConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        gatewayId = pGatewayId_,
        capabilityNamespace =
          pCapabilityNamespace_,
        capabilityConfiguration =
          pCapabilityConfiguration_,
        capabilitySyncStatus =
          pCapabilitySyncStatus_
      }

-- | The response's http status code.
describeGatewayCapabilityConfigurationResponse_httpStatus :: Lens.Lens' DescribeGatewayCapabilityConfigurationResponse Prelude.Int
describeGatewayCapabilityConfigurationResponse_httpStatus = Lens.lens (\DescribeGatewayCapabilityConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeGatewayCapabilityConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeGatewayCapabilityConfigurationResponse)

-- | The ID of the gateway that defines the capability configuration.
describeGatewayCapabilityConfigurationResponse_gatewayId :: Lens.Lens' DescribeGatewayCapabilityConfigurationResponse Prelude.Text
describeGatewayCapabilityConfigurationResponse_gatewayId = Lens.lens (\DescribeGatewayCapabilityConfigurationResponse' {gatewayId} -> gatewayId) (\s@DescribeGatewayCapabilityConfigurationResponse' {} a -> s {gatewayId = a} :: DescribeGatewayCapabilityConfigurationResponse)

-- | The namespace of the gateway capability.
describeGatewayCapabilityConfigurationResponse_capabilityNamespace :: Lens.Lens' DescribeGatewayCapabilityConfigurationResponse Prelude.Text
describeGatewayCapabilityConfigurationResponse_capabilityNamespace = Lens.lens (\DescribeGatewayCapabilityConfigurationResponse' {capabilityNamespace} -> capabilityNamespace) (\s@DescribeGatewayCapabilityConfigurationResponse' {} a -> s {capabilityNamespace = a} :: DescribeGatewayCapabilityConfigurationResponse)

-- | The JSON document that defines the gateway capability\'s configuration.
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/configure-sources.html#configure-source-cli Configuring data sources (CLI)>
-- in the /IoT SiteWise User Guide/.
describeGatewayCapabilityConfigurationResponse_capabilityConfiguration :: Lens.Lens' DescribeGatewayCapabilityConfigurationResponse Prelude.Text
describeGatewayCapabilityConfigurationResponse_capabilityConfiguration = Lens.lens (\DescribeGatewayCapabilityConfigurationResponse' {capabilityConfiguration} -> capabilityConfiguration) (\s@DescribeGatewayCapabilityConfigurationResponse' {} a -> s {capabilityConfiguration = a} :: DescribeGatewayCapabilityConfigurationResponse)

-- | The synchronization status of the capability configuration. The sync
-- status can be one of the following:
--
-- -   @IN_SYNC@ – The gateway is running the capability configuration.
--
-- -   @OUT_OF_SYNC@ – The gateway hasn\'t received the capability
--     configuration.
--
-- -   @SYNC_FAILED@ – The gateway rejected the capability configuration.
describeGatewayCapabilityConfigurationResponse_capabilitySyncStatus :: Lens.Lens' DescribeGatewayCapabilityConfigurationResponse CapabilitySyncStatus
describeGatewayCapabilityConfigurationResponse_capabilitySyncStatus = Lens.lens (\DescribeGatewayCapabilityConfigurationResponse' {capabilitySyncStatus} -> capabilitySyncStatus) (\s@DescribeGatewayCapabilityConfigurationResponse' {} a -> s {capabilitySyncStatus = a} :: DescribeGatewayCapabilityConfigurationResponse)

instance
  Prelude.NFData
    DescribeGatewayCapabilityConfigurationResponse
  where
  rnf
    DescribeGatewayCapabilityConfigurationResponse' {..} =
      Prelude.rnf httpStatus
        `Prelude.seq` Prelude.rnf gatewayId
        `Prelude.seq` Prelude.rnf capabilityNamespace
        `Prelude.seq` Prelude.rnf capabilityConfiguration
        `Prelude.seq` Prelude.rnf capabilitySyncStatus
