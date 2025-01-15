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
-- Module      : Amazonka.IoTSiteWise.DescribeGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a gateway.
module Amazonka.IoTSiteWise.DescribeGateway
  ( -- * Creating a Request
    DescribeGateway (..),
    newDescribeGateway,

    -- * Request Lenses
    describeGateway_gatewayId,

    -- * Destructuring the Response
    DescribeGatewayResponse (..),
    newDescribeGatewayResponse,

    -- * Response Lenses
    describeGatewayResponse_gatewayPlatform,
    describeGatewayResponse_httpStatus,
    describeGatewayResponse_gatewayId,
    describeGatewayResponse_gatewayName,
    describeGatewayResponse_gatewayArn,
    describeGatewayResponse_gatewayCapabilitySummaries,
    describeGatewayResponse_creationDate,
    describeGatewayResponse_lastUpdateDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGateway' smart constructor.
data DescribeGateway = DescribeGateway'
  { -- | The ID of the gateway device.
    gatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayId', 'describeGateway_gatewayId' - The ID of the gateway device.
newDescribeGateway ::
  -- | 'gatewayId'
  Prelude.Text ->
  DescribeGateway
newDescribeGateway pGatewayId_ =
  DescribeGateway' {gatewayId = pGatewayId_}

-- | The ID of the gateway device.
describeGateway_gatewayId :: Lens.Lens' DescribeGateway Prelude.Text
describeGateway_gatewayId = Lens.lens (\DescribeGateway' {gatewayId} -> gatewayId) (\s@DescribeGateway' {} a -> s {gatewayId = a} :: DescribeGateway)

instance Core.AWSRequest DescribeGateway where
  type
    AWSResponse DescribeGateway =
      DescribeGatewayResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGatewayResponse'
            Prelude.<$> (x Data..?> "gatewayPlatform")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "gatewayId")
            Prelude.<*> (x Data..:> "gatewayName")
            Prelude.<*> (x Data..:> "gatewayArn")
            Prelude.<*> ( x
                            Data..?> "gatewayCapabilitySummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "creationDate")
            Prelude.<*> (x Data..:> "lastUpdateDate")
      )

instance Prelude.Hashable DescribeGateway where
  hashWithSalt _salt DescribeGateway' {..} =
    _salt `Prelude.hashWithSalt` gatewayId

instance Prelude.NFData DescribeGateway where
  rnf DescribeGateway' {..} = Prelude.rnf gatewayId

instance Data.ToHeaders DescribeGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeGateway where
  toPath DescribeGateway' {..} =
    Prelude.mconcat
      ["/20200301/gateways/", Data.toBS gatewayId]

instance Data.ToQuery DescribeGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGatewayResponse' smart constructor.
data DescribeGatewayResponse = DescribeGatewayResponse'
  { -- | The gateway\'s platform.
    gatewayPlatform :: Prelude.Maybe GatewayPlatform,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the gateway device.
    gatewayId :: Prelude.Text,
    -- | The name of the gateway.
    gatewayName :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the gateway, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:gateway\/${GatewayId}@
    gatewayArn :: Prelude.Text,
    -- | A list of gateway capability summaries that each contain a namespace and
    -- status. Each gateway capability defines data sources for the gateway. To
    -- retrieve a capability configuration\'s definition, use
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeGatewayCapabilityConfiguration.html DescribeGatewayCapabilityConfiguration>.
    gatewayCapabilitySummaries :: [GatewayCapabilitySummary],
    -- | The date the gateway was created, in Unix epoch time.
    creationDate :: Data.POSIX,
    -- | The date the gateway was last updated, in Unix epoch time.
    lastUpdateDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayPlatform', 'describeGatewayResponse_gatewayPlatform' - The gateway\'s platform.
--
-- 'httpStatus', 'describeGatewayResponse_httpStatus' - The response's http status code.
--
-- 'gatewayId', 'describeGatewayResponse_gatewayId' - The ID of the gateway device.
--
-- 'gatewayName', 'describeGatewayResponse_gatewayName' - The name of the gateway.
--
-- 'gatewayArn', 'describeGatewayResponse_gatewayArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the gateway, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:gateway\/${GatewayId}@
--
-- 'gatewayCapabilitySummaries', 'describeGatewayResponse_gatewayCapabilitySummaries' - A list of gateway capability summaries that each contain a namespace and
-- status. Each gateway capability defines data sources for the gateway. To
-- retrieve a capability configuration\'s definition, use
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeGatewayCapabilityConfiguration.html DescribeGatewayCapabilityConfiguration>.
--
-- 'creationDate', 'describeGatewayResponse_creationDate' - The date the gateway was created, in Unix epoch time.
--
-- 'lastUpdateDate', 'describeGatewayResponse_lastUpdateDate' - The date the gateway was last updated, in Unix epoch time.
newDescribeGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'gatewayId'
  Prelude.Text ->
  -- | 'gatewayName'
  Prelude.Text ->
  -- | 'gatewayArn'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'lastUpdateDate'
  Prelude.UTCTime ->
  DescribeGatewayResponse
newDescribeGatewayResponse
  pHttpStatus_
  pGatewayId_
  pGatewayName_
  pGatewayArn_
  pCreationDate_
  pLastUpdateDate_ =
    DescribeGatewayResponse'
      { gatewayPlatform =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        gatewayId = pGatewayId_,
        gatewayName = pGatewayName_,
        gatewayArn = pGatewayArn_,
        gatewayCapabilitySummaries = Prelude.mempty,
        creationDate = Data._Time Lens.# pCreationDate_,
        lastUpdateDate =
          Data._Time Lens.# pLastUpdateDate_
      }

-- | The gateway\'s platform.
describeGatewayResponse_gatewayPlatform :: Lens.Lens' DescribeGatewayResponse (Prelude.Maybe GatewayPlatform)
describeGatewayResponse_gatewayPlatform = Lens.lens (\DescribeGatewayResponse' {gatewayPlatform} -> gatewayPlatform) (\s@DescribeGatewayResponse' {} a -> s {gatewayPlatform = a} :: DescribeGatewayResponse)

-- | The response's http status code.
describeGatewayResponse_httpStatus :: Lens.Lens' DescribeGatewayResponse Prelude.Int
describeGatewayResponse_httpStatus = Lens.lens (\DescribeGatewayResponse' {httpStatus} -> httpStatus) (\s@DescribeGatewayResponse' {} a -> s {httpStatus = a} :: DescribeGatewayResponse)

-- | The ID of the gateway device.
describeGatewayResponse_gatewayId :: Lens.Lens' DescribeGatewayResponse Prelude.Text
describeGatewayResponse_gatewayId = Lens.lens (\DescribeGatewayResponse' {gatewayId} -> gatewayId) (\s@DescribeGatewayResponse' {} a -> s {gatewayId = a} :: DescribeGatewayResponse)

-- | The name of the gateway.
describeGatewayResponse_gatewayName :: Lens.Lens' DescribeGatewayResponse Prelude.Text
describeGatewayResponse_gatewayName = Lens.lens (\DescribeGatewayResponse' {gatewayName} -> gatewayName) (\s@DescribeGatewayResponse' {} a -> s {gatewayName = a} :: DescribeGatewayResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the gateway, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:gateway\/${GatewayId}@
describeGatewayResponse_gatewayArn :: Lens.Lens' DescribeGatewayResponse Prelude.Text
describeGatewayResponse_gatewayArn = Lens.lens (\DescribeGatewayResponse' {gatewayArn} -> gatewayArn) (\s@DescribeGatewayResponse' {} a -> s {gatewayArn = a} :: DescribeGatewayResponse)

-- | A list of gateway capability summaries that each contain a namespace and
-- status. Each gateway capability defines data sources for the gateway. To
-- retrieve a capability configuration\'s definition, use
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeGatewayCapabilityConfiguration.html DescribeGatewayCapabilityConfiguration>.
describeGatewayResponse_gatewayCapabilitySummaries :: Lens.Lens' DescribeGatewayResponse [GatewayCapabilitySummary]
describeGatewayResponse_gatewayCapabilitySummaries = Lens.lens (\DescribeGatewayResponse' {gatewayCapabilitySummaries} -> gatewayCapabilitySummaries) (\s@DescribeGatewayResponse' {} a -> s {gatewayCapabilitySummaries = a} :: DescribeGatewayResponse) Prelude.. Lens.coerced

-- | The date the gateway was created, in Unix epoch time.
describeGatewayResponse_creationDate :: Lens.Lens' DescribeGatewayResponse Prelude.UTCTime
describeGatewayResponse_creationDate = Lens.lens (\DescribeGatewayResponse' {creationDate} -> creationDate) (\s@DescribeGatewayResponse' {} a -> s {creationDate = a} :: DescribeGatewayResponse) Prelude.. Data._Time

-- | The date the gateway was last updated, in Unix epoch time.
describeGatewayResponse_lastUpdateDate :: Lens.Lens' DescribeGatewayResponse Prelude.UTCTime
describeGatewayResponse_lastUpdateDate = Lens.lens (\DescribeGatewayResponse' {lastUpdateDate} -> lastUpdateDate) (\s@DescribeGatewayResponse' {} a -> s {lastUpdateDate = a} :: DescribeGatewayResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeGatewayResponse where
  rnf DescribeGatewayResponse' {..} =
    Prelude.rnf gatewayPlatform `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf gatewayId `Prelude.seq`
          Prelude.rnf gatewayName `Prelude.seq`
            Prelude.rnf gatewayArn `Prelude.seq`
              Prelude.rnf gatewayCapabilitySummaries `Prelude.seq`
                Prelude.rnf creationDate `Prelude.seq`
                  Prelude.rnf lastUpdateDate
