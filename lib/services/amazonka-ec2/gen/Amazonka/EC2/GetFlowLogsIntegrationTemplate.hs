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
-- Module      : Amazonka.EC2.GetFlowLogsIntegrationTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a CloudFormation template that streamlines and automates the
-- integration of VPC flow logs with Amazon Athena. This make it easier for
-- you to query and gain insights from VPC flow logs data. Based on the
-- information that you provide, we configure resources in the template to
-- do the following:
--
-- -   Create a table in Athena that maps fields to a custom log format
--
-- -   Create a Lambda function that updates the table with new partitions
--     on a daily, weekly, or monthly basis
--
-- -   Create a table partitioned between two timestamps in the past
--
-- -   Create a set of named queries in Athena that you can use to get
--     started quickly
module Amazonka.EC2.GetFlowLogsIntegrationTemplate
  ( -- * Creating a Request
    GetFlowLogsIntegrationTemplate (..),
    newGetFlowLogsIntegrationTemplate,

    -- * Request Lenses
    getFlowLogsIntegrationTemplate_dryRun,
    getFlowLogsIntegrationTemplate_flowLogId,
    getFlowLogsIntegrationTemplate_configDeliveryS3DestinationArn,
    getFlowLogsIntegrationTemplate_integrateServices,

    -- * Destructuring the Response
    GetFlowLogsIntegrationTemplateResponse (..),
    newGetFlowLogsIntegrationTemplateResponse,

    -- * Response Lenses
    getFlowLogsIntegrationTemplateResponse_result,
    getFlowLogsIntegrationTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFlowLogsIntegrationTemplate' smart constructor.
data GetFlowLogsIntegrationTemplate = GetFlowLogsIntegrationTemplate'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the flow log.
    flowLogId :: Prelude.Text,
    -- | To store the CloudFormation template in Amazon S3, specify the location
    -- in Amazon S3.
    configDeliveryS3DestinationArn :: Prelude.Text,
    -- | Information about the service integration.
    integrateServices :: IntegrateServices
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFlowLogsIntegrationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getFlowLogsIntegrationTemplate_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'flowLogId', 'getFlowLogsIntegrationTemplate_flowLogId' - The ID of the flow log.
--
-- 'configDeliveryS3DestinationArn', 'getFlowLogsIntegrationTemplate_configDeliveryS3DestinationArn' - To store the CloudFormation template in Amazon S3, specify the location
-- in Amazon S3.
--
-- 'integrateServices', 'getFlowLogsIntegrationTemplate_integrateServices' - Information about the service integration.
newGetFlowLogsIntegrationTemplate ::
  -- | 'flowLogId'
  Prelude.Text ->
  -- | 'configDeliveryS3DestinationArn'
  Prelude.Text ->
  -- | 'integrateServices'
  IntegrateServices ->
  GetFlowLogsIntegrationTemplate
newGetFlowLogsIntegrationTemplate
  pFlowLogId_
  pConfigDeliveryS3DestinationArn_
  pIntegrateServices_ =
    GetFlowLogsIntegrationTemplate'
      { dryRun =
          Prelude.Nothing,
        flowLogId = pFlowLogId_,
        configDeliveryS3DestinationArn =
          pConfigDeliveryS3DestinationArn_,
        integrateServices = pIntegrateServices_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getFlowLogsIntegrationTemplate_dryRun :: Lens.Lens' GetFlowLogsIntegrationTemplate (Prelude.Maybe Prelude.Bool)
getFlowLogsIntegrationTemplate_dryRun = Lens.lens (\GetFlowLogsIntegrationTemplate' {dryRun} -> dryRun) (\s@GetFlowLogsIntegrationTemplate' {} a -> s {dryRun = a} :: GetFlowLogsIntegrationTemplate)

-- | The ID of the flow log.
getFlowLogsIntegrationTemplate_flowLogId :: Lens.Lens' GetFlowLogsIntegrationTemplate Prelude.Text
getFlowLogsIntegrationTemplate_flowLogId = Lens.lens (\GetFlowLogsIntegrationTemplate' {flowLogId} -> flowLogId) (\s@GetFlowLogsIntegrationTemplate' {} a -> s {flowLogId = a} :: GetFlowLogsIntegrationTemplate)

-- | To store the CloudFormation template in Amazon S3, specify the location
-- in Amazon S3.
getFlowLogsIntegrationTemplate_configDeliveryS3DestinationArn :: Lens.Lens' GetFlowLogsIntegrationTemplate Prelude.Text
getFlowLogsIntegrationTemplate_configDeliveryS3DestinationArn = Lens.lens (\GetFlowLogsIntegrationTemplate' {configDeliveryS3DestinationArn} -> configDeliveryS3DestinationArn) (\s@GetFlowLogsIntegrationTemplate' {} a -> s {configDeliveryS3DestinationArn = a} :: GetFlowLogsIntegrationTemplate)

-- | Information about the service integration.
getFlowLogsIntegrationTemplate_integrateServices :: Lens.Lens' GetFlowLogsIntegrationTemplate IntegrateServices
getFlowLogsIntegrationTemplate_integrateServices = Lens.lens (\GetFlowLogsIntegrationTemplate' {integrateServices} -> integrateServices) (\s@GetFlowLogsIntegrationTemplate' {} a -> s {integrateServices = a} :: GetFlowLogsIntegrationTemplate)

instance
  Core.AWSRequest
    GetFlowLogsIntegrationTemplate
  where
  type
    AWSResponse GetFlowLogsIntegrationTemplate =
      GetFlowLogsIntegrationTemplateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetFlowLogsIntegrationTemplateResponse'
            Prelude.<$> (x Core..@? "result")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetFlowLogsIntegrationTemplate
  where
  hashWithSalt
    _salt
    GetFlowLogsIntegrationTemplate' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` flowLogId
        `Prelude.hashWithSalt` configDeliveryS3DestinationArn
        `Prelude.hashWithSalt` integrateServices

instance
  Prelude.NFData
    GetFlowLogsIntegrationTemplate
  where
  rnf GetFlowLogsIntegrationTemplate' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf flowLogId
      `Prelude.seq` Prelude.rnf configDeliveryS3DestinationArn
      `Prelude.seq` Prelude.rnf integrateServices

instance
  Core.ToHeaders
    GetFlowLogsIntegrationTemplate
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetFlowLogsIntegrationTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery GetFlowLogsIntegrationTemplate where
  toQuery GetFlowLogsIntegrationTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetFlowLogsIntegrationTemplate" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "FlowLogId" Core.=: flowLogId,
        "ConfigDeliveryS3DestinationArn"
          Core.=: configDeliveryS3DestinationArn,
        "IntegrateService" Core.=: integrateServices
      ]

-- | /See:/ 'newGetFlowLogsIntegrationTemplateResponse' smart constructor.
data GetFlowLogsIntegrationTemplateResponse = GetFlowLogsIntegrationTemplateResponse'
  { -- | The generated CloudFormation template.
    result :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFlowLogsIntegrationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'getFlowLogsIntegrationTemplateResponse_result' - The generated CloudFormation template.
--
-- 'httpStatus', 'getFlowLogsIntegrationTemplateResponse_httpStatus' - The response's http status code.
newGetFlowLogsIntegrationTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFlowLogsIntegrationTemplateResponse
newGetFlowLogsIntegrationTemplateResponse
  pHttpStatus_ =
    GetFlowLogsIntegrationTemplateResponse'
      { result =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The generated CloudFormation template.
getFlowLogsIntegrationTemplateResponse_result :: Lens.Lens' GetFlowLogsIntegrationTemplateResponse (Prelude.Maybe Prelude.Text)
getFlowLogsIntegrationTemplateResponse_result = Lens.lens (\GetFlowLogsIntegrationTemplateResponse' {result} -> result) (\s@GetFlowLogsIntegrationTemplateResponse' {} a -> s {result = a} :: GetFlowLogsIntegrationTemplateResponse)

-- | The response's http status code.
getFlowLogsIntegrationTemplateResponse_httpStatus :: Lens.Lens' GetFlowLogsIntegrationTemplateResponse Prelude.Int
getFlowLogsIntegrationTemplateResponse_httpStatus = Lens.lens (\GetFlowLogsIntegrationTemplateResponse' {httpStatus} -> httpStatus) (\s@GetFlowLogsIntegrationTemplateResponse' {} a -> s {httpStatus = a} :: GetFlowLogsIntegrationTemplateResponse)

instance
  Prelude.NFData
    GetFlowLogsIntegrationTemplateResponse
  where
  rnf GetFlowLogsIntegrationTemplateResponse' {..} =
    Prelude.rnf result
      `Prelude.seq` Prelude.rnf httpStatus
